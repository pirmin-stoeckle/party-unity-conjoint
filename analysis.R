library(data.table)
library(ggplot2)
library(survival)
library(haven)
library(MASS)
library(forcats) # to re-order factors (within ggplot)

# cleaning directory
rm(list=ls())

# load data from Stata where reshaping and some recoding is done (in data_preparation_for_R.do)
dat <- as.data.table(read_dta(file="//nas.uni-mannheim.de/sfb884/sfb884c/C2/research/GIP_vote_experiment/data/reshaped_experiment_data.dta"))
#dat <- as.data.table(read_dta(file="/Volumes/sfb884c/C2/research/GIP_vote_experiment/data/reshaped_experiment_data.dta"))

# label factors correctly
dat$role <- factor(dat$role, levels=2:4,
                    labels=c("opposition party",
                             "PM party", "government party (not PM party)"))
dat$conference <- factor(dat$conference, levels=1:3,
                    labels=c("united", "neither united nor divided", "divided"))
dat$parliament <- factor(dat$parliament, levels=2:3,
                    labels=c("united", "divided"))
dat$critique <- factor(dat$critique, levels=1:4,
                    labels=c("grass-root members", "former party leader", "party faction", "none"))
dat$reform <- factor(dat$reform, levels=1:2,
                     labels=c("high", "low"))
dat$gender <- factor(dat$gender, levels=1:2,
                     labels=c("male", "female"))
dat$age <- factor(dat$age, levels=1:3,
                     labels=c("38y", "56y", "74y"))
dat$job <- factor(dat$job, levels=1:6,
                  labels=c("activist", "employee", "lawyer", "politician",
                           "entrepreneur", "employee (retired)"))
dat$dist <- factor(dat$dist, levels=0:4)

#set baseline scenario (corresponding to the highest preference, as identified in the model)
       # personal characteristics
       dat$gender <- relevel(dat$gender, ref="female")
       dat$age <- relevel(dat$age, ref="38y")
       dat$job <- relevel(dat$job, ref="employee")
       # party status
       dat$role <- relevel(dat$role, ref="government party (not PM party)")
       # party unity
       dat$critique <- relevel(dat$critique, ref="grass-root members")
       dat$parliament <- relevel(dat$parliament, ref="united")
       dat$conference <- relevel(dat$conference, ref="united")
       # reform clarity
       dat$reform <- relevel(dat$reform, ref="high")
       # party position
       dat$dist <- relevel(dat$dist, ref="0")

# choice data descriptives
head(dat)
summary(dat)

# data check: do party ratings correspond to chosen parties?
dat %>% 
    filter(!is.na(rating)) %>%
    pivot_wider(
        id_cols = id_screen,
        names_from = chosen,
        names_prefix = "rating_",
        values_from = rating,
        values_fn = mean
    ) %>% 
    mutate(correct_order = (rating_1 >= rating_0)) %>% 
    count(correct_order) %>% 
    mutate(freq = n/sum(n))

# what is the sample size after removal of missings?
dat[complete.cases(chosen, dist), #other variables relevant for model are non-missing by design
    .(n_respondents = length(unique(id_g)),
      n_observations = length(unique(id_screen)))]

# compute conditional logit model
# strata distinguished the different choice situations (each respondent-screen combination)
# cluster allows for clustered SEs by respondent

model <- clogit(chosen~
                 gender
                +age
                +job
                +role
                +critique
                +parliament
                +conference
                +reform
                +dist
                +strata(id_screen)
                +cluster(id_g)
                ,data=dat, method="efron", robust=TRUE)


compute_pdata <- function(model) {
    #(get data for) AMCEs plot
    coefs <- coef(model) #get cofficients
    ses <- summary(model)$coefficients[,4] #get ROBUST res
    names <- rownames(summary(model)$coefficients) #get names of coefs
    pdata <- data.table(mean=(1/(1+exp(-coefs)))-.5, #compute probability difference to all 0's scenario
                        lower=(1/(1+exp(-(coefs-(1.96*ses)))))-.5, #compute corresponding CI
                        upper=(1/(1+exp(-(coefs+(1.96*ses)))))-.5, #compute corresponding CI
                        names=names
                        )
    return(pdata)
}

pdata <- compute_pdata(model = model)

# pdata can be used for plotting

# alternative AMCE estimation following Hainmueller, Hopkins, Yamamoto, T. (2014)
model_cjoint <- cjoint::amce(chosen~
                       gender
                     +age
                     +job
                     +role
                     +critique
                     +parliament
                     +conference
                     +reform
                     +dist, 
                     data = dat[complete.cases(dist, chosen),], design = "uniform",
                     respondent.varying = NULL, subset = NULL,
                     respondent.id = "id_g", cluster = TRUE, na.ignore=T,
                     weights = NULL, baselines = NULL)

####################################################
# simulate probabilities for a given scenario      #
# compared to baseline scenario as specified above #
####################################################

# define scenario (here all possible scenarios)
compute_scenarios <- function(model) {
    scenarios=expand.grid(role=c("opposition party", "PM party", "government party (not PM party)"),
                          conference= c("united", "neither united nor divided", "divided"),
                          parliament=c("united", "divided"),
                          critique= c("grass-root members", "former party leader", "party faction", "none"),
                          reform=c("high", "low"),
                          gender=c("male", "female"),
                          age=c("38y", "56y", "74y"),
                          job=c("activist", "employee", "lawyer", "politician",
                                "entrepreneur", "employee (retired)"),
                          dist=c("0", "1", "2", "3", "4"),
                          id_screen=1)
    scenarios <- as.data.table(scenarios)
    
    # drop scenarios that are not in experiment (young retiree, or old employee)
    scenarios$out <- ifelse(scenarios$job=="employee"&scenarios$age=="74y", 1, 0)
    scenarios$out <- ifelse(scenarios$job=="employee (retired)"&scenarios$age%in%c("38y","56y"), 1, scenarios$out)
    scenarios <- scenarios[out==0] # keep if out==0
    scenarios$out <- NULL # drop variable out
    
    # create design matrix that has indicator vars instead of factors
    scenarios.expanded <- model.matrix(model, data=scenarios)
    
    # draw from sampling distribution
    betas <- mvrnorm(1000, coef(model), vcov(model)) #the vcov is already robust
    
    # compute probabilities
    linpred <- scenarios.expanded%*%t(betas)
    probs <- exp(linpred)/(1+exp(linpred))
    
    
    
    # find mean probability and CIs for plotting
    scenarios$prob <- apply(probs, 1, mean)*100
    scenarios$upper <- apply(probs, 1, quantile, p=.975)*100
    scenarios$lower <- apply(probs, 1, quantile, p=.025)*100
    
    # return
    return(scenarios)
    
}

scenarios <- compute_scenarios(model = model)

#scenarios can be used for plotting now

#############################################################################
# Code for getting probabilities from one scenario against another scenario #
#############################################################################

# define scenario (here all possible scenarios)
# output are linear predictors, not probabilities
compute_scenarios_linpred <- function(model){
    scenarios=expand.grid(role=c("opposition party", "PM party", "government party (not PM party)"),
                          conference= c("united", "neither united nor divided", "divided"),
                          parliament=c("united", "divided"),
                          critique= c("grass-root members", "former party leader", "party faction", "none"),
                          reform=c("high", "low"),
                          gender=c("male", "female"),
                          age=c("38y", "56y", "74y"),
                          job=c("activist", "employee", "lawyer", "politician",
                                "entrepreneur", "employee (retired)"),
                          dist=c("0", "1", "2", "3", "4"),
                          id_screen=1)
    scenarios <- as.data.table(scenarios)
    
    # drop scenarios that are not in experiment (young retiree, or old employee)
    scenarios$out <- ifelse(scenarios$job=="employee"&scenarios$age=="74y", 1, 0)
    scenarios$out <- ifelse(scenarios$job=="employee (retired)"&scenarios$age%in%c("38y","56y"), 1, scenarios$out)
    scenarios <- scenarios[out==0] # keep if out==0
    scenarios$out <- NULL # drop variable out
    
    # create design matrix that has indicator vars instead of factors
    scenarios.expanded <- model.matrix(model, data=scenarios)
    
    # draw from sampling distribution
    betas <- mvrnorm(1000, coef(model), vcov(model)) #the vcov is already robust
    
    # compute linear predictor
    linpred <- scenarios.expanded%*%t(betas)
    
    # summarize
    scenarios$mean <- apply(linpred, 1, mean)
    scenarios$upper <- apply(linpred, 1, quantile, p=.975)
    scenarios$lower <- apply(linpred, 1, quantile, p=.025)
    
    # return
    return(scenarios)
}

# get linear predictors
linpreds <- compute_scenarios_linpred(model)

# save output
#save(linpreds, file="./data/linpreds.RData")
