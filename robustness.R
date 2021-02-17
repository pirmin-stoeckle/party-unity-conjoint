library(data.table)
library(tidyverse)

######################################################################
# data check: do party ratings correspond to chosen parties?
######################################################################
# load data:
source("analysis.R")


dat %>% 
  filter(complete.cases(rating, chosen)) %>% 
  pivot_wider(
    id_cols = c(id_g, id_screen, screen),
    names_from = chosen,
    names_prefix = "rating_",
    values_from = rating
  ) %>% 
  mutate(incorrect_order = (rating_1 < rating_0)) %>% 
  count(incorrect_order) %>% 
  mutate(freq = n/sum(n))

# -> within the whole sample, 6% of ratings show a correct ordering (chosen worse than not chosen, 
# conditional on having rated and having chosen, NA's here represent cases where only one party was rated)

dat %>% 
  filter(complete.cases(rating, chosen, dist)) %>% # restrict to our used sample for estimation including dist variable
  pivot_wider(
    id_cols = c(id_g, id_screen, screen),
    names_from = chosen,
    names_prefix = "rating_",
    values_from = rating
  ) %>% 
  mutate(incorrect_order = (rating_1 < rating_0)) %>%
  count(incorrect_order) %>% 
  mutate(freq = n/sum(n))

# -> same for our analysis sample

# are there more incorrect orderings on later screens?
dat %>% 
  filter(complete.cases(rating, chosen)) %>%
  pivot_wider(
    id_cols = c(id_g, id_screen, screen),
    names_from = chosen,
    names_prefix = "rating_",
    values_from = rating
  ) %>% 
  mutate(incorrect_order = (rating_1 < rating_0)) %>%
  group_by(screen) %>% 
  count(incorrect_order) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(incorrect_order != "FALSE") %>% 
    ggplot(aes(x = screen, y = freq, group = incorrect_order)) +
      geom_col()+
      ylab("Share of party ratings not matching the choice") +
      scale_x_discrete(limits = c(1:10)) 
# -> yes. how bad is this?

# we could re-run everything excluding respondents with any incorrect ordering (same did multiple orderings)
# extract id of incorrect orderings
id_g_incorrect_ordering <- dat %>% 
  filter(complete.cases(rating, chosen)) %>% 
  pivot_wider(
    id_cols = c(id_g, id_screen, screen),
    names_from = chosen,
    names_prefix = "rating_",
    values_from = rating,
    values_fn = mean
  ) %>% 
  mutate(incorrect_order = (rating_1 < rating_0)) %>% 
  filter(incorrect_order== TRUE) %>% 
  pull(id_g)

dat_correct_ratings <- dat %>% 
  filter(!id_g %in% id_g_incorrect_ordering) 

# exclude missings
dat_correct_ratings.cbc <- dat_correct_ratings[complete.cases(chosen, dist), ]
length(unique(dat_correct_ratings.cbc$id_g))
# sample size (respondents) of 3395

model_clogit_correct_ratings <- clogit(chosen~
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
                       ,data=dat_correct_ratings.cbc, method="efron", robust=TRUE)

summary(model_clogit_correct_ratings)

# custom function to get data for AMCE plot in consistent format
compute_amce_clogit_correct_ratings <- function(model) {
  coefs <- coef(model) #get coefficients 
  ses <- summary(model)$coefficients[,4] #get ROBUST ses
  names <- rownames(summary(model)$coefficients) #get names of coefs
  pdata <- data.table(amce=(1/(1+exp(-coefs)))-.5, #compute probability difference to all 0's scenario
                      lower=(1/(1+exp(-(coefs-(1.96*ses)))))-.5, #compute corresponding CI
                      upper=(1/(1+exp(-(coefs+(1.96*ses)))))-.5, #compute corresponding CI
                      names=names,
                      coefs = coefs, 
                      ses = ses,
                      specification = "clogit_correct_ratings"
  )
  return(pdata)
}

pdata_clogit_correct_ratings <- compute_amce_clogit_correct_ratings(model = model_clogit_correct_ratings)

######################################################################
# comparison of model specifications
######################################################################

# load data:
#source(analysis.R)

# exclude missings
dat.cbc <- dat[complete.cases(chosen, dist), ]

# conditional logit (our approach)
library(survival)
library(data.table)

model_clogit <- clogit(chosen~
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
                       ,data=dat.cbc, method="efron", robust=TRUE)

summary(model_clogit)

# custom function to get data for AMCE plot in consistent format
compute_amce_clogit <- function(model) {
  coefs <- coef(model) #get coefficients 
  ses <- summary(model)$coefficients[,4] #get ROBUST ses
  names <- rownames(summary(model)$coefficients) #get names of coefs
  pdata <- data.table(amce=(1/(1+exp(-coefs)))-.5, #compute probability difference to all 0's scenario
                      lower=(1/(1+exp(-(coefs-(1.96*ses)))))-.5, #compute corresponding CI
                      upper=(1/(1+exp(-(coefs+(1.96*ses)))))-.5, #compute corresponding CI
                      names=names,
                      coefs = coefs, 
                      ses = ses,
                      specification = "clogit"
  )
  return(pdata)
}

pdata_clogit <- compute_amce_clogit(model = model_clogit)

# approach by Hainmueller et al 2014, implemented in the "cjoint" package
library(cjoint)
model_cjoint <- amce(chosen~
                       gender
                     +age
                     +job
                     +role
                     +critique
                     +parliament
                     +conference
                     +reform
                     +dist, 
                     data = dat.cbc, design = "uniform",
                     respondent.varying = NULL, subset = NULL,
                     respondent.id = "id_g", cluster = TRUE, na.ignore=T,
                     weights = NULL, baselines = NULL)

summary(model_cjoint)

# custom function to get data for AMCE plot in consistent format
compute_amce_cjoint <- function(model) {
  coefs <- summary(model_cjoint)$amce[,3] #get coefficients
  ses <- summary(model_cjoint)$amce[,4] #get ses (not robust here)
  names <- paste0(summary(model_cjoint)$amce$Attribute,summary(model_cjoint)$amce$Level)  #get names of coefs
  pdata <- data.table(amce=coefs, #here, AMCE is just the coefficients
                      lower=coefs-(1.96*ses), #compute corresponding CI
                      upper=coefs+(1.96*ses), #compute corresponding CI
                      names=names,
                      coefs = coefs, 
                      ses = ses,
                      specification = "cjoint"
  )
  return(pdata)
}

pdata_cjoint <- compute_amce_cjoint(model = model_cjoint)

# linear regression without any clustering
model_lm <- lm(chosen~  
               gender
               +age
               +job
               +role
               +critique
               +parliament
               +conference
               +reform
               +dist
               ,data=dat.cbc)

summary(model_lm)
# coefficients are the same as Hainmuellers approach, only small differenes in ses

# custom function to get data for AMCE plot in consistent format
compute_amce_lm <- function(model) {
  coefs <- coef(model)[-1] #get coefficients but leave out intercept
  ses <- summary(model)$coefficients[-1,2] #get ses (not robust here)
  names <- rownames(summary(model)$coefficients[-1,]) #get names of coefs
  pdata <- data.table(amce=coefs, #here, AMCE is just the coefficients
                      lower=coefs-(1.96*ses), #compute corresponding CI
                      upper=coefs+(1.96*ses), #compute corresponding CI
                      names=names,
                      coefs = coefs, 
                      ses = ses,
                      specification = "lm"
  )
  return(pdata)
}

pdata_lm <- compute_amce_lm(model = model_lm)


# join in one dataframe

pdata_robustness1 <- rbind(pdata_clogit, pdata_cjoint)
pdata_robustness2 <- rbind(pdata_clogit, pdata_clogit_correct_ratings)

#plot 1
amceplot_robustness1 <- ggplot(pdata_robustness1, aes(x = amce, y = names, shape = specification)) + 
  geom_pointrange(aes(xmin = lower, xmax = upper), 
                  size = 0.25,
                  position = position_dodge(width = 0.5))+
  geom_vline(xintercept = 0, linetype = 3) +
  xlab("Change: Pr(Vote for the respective candidate)") +
  ylab("")

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/amceplot_robustness1.pdf"))
amceplot_robustness1
dev.off()

#plot 2
amceplot_robustness2 <- ggplot(pdata_robustness2, aes(x = amce, y = names, shape = specification)) + 
  geom_pointrange(aes(xmin = lower, xmax = upper), 
                  size = 0.25,
                  position = position_dodge(width = 0.5))+
  geom_vline(xintercept = 0, linetype = 3) +
  xlab("Change: Pr(Vote for the respective candidate)") +
  ylab("")

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/amceplot_robustness2.pdf"))
amceplot_robustness2
dev.off()

# textbook approach for choice-based conjoint based on mlogit package
# requires data in mlogit.data format, for which an id for the alternative
# (party 1 vs party 2 per screen) is needed.

dat.cbc[, alt_id := seq_len(.N), by = id_screen]

# create mlogit data with special function
dat.mlogit <- mlogit.data(dat.cbc, 
                          choice="chosen",
                          shape="long",
                          alt.var = "alt_id",  
                          id.var="id_g")
dat.mlogit
dat.mlogit$idx

# pure conditional model
mlogit1 <- mlogit(chosen ~ 
               0 + gender
             +age
             +job
             +role
             +critique
             +parliament
             +conference
             +reform
             +dist, dat.mlogit)

# --> problem here is that respondents saw different numbers of screens 
# (presumably because the skipped some).
# This does not seem to work with mlogit...

dat.cbc %>% 
  group_by(id_g) %>% 
  mutate(obs_per_respondent = n()) %>% 
  ungroup() %>% 
  group_by(obs_per_respondent) %>% 
  summarize(n_respondents = n())

# Seems like there has to be the same number of screens seen per respondents for mlogit...

dat.cbc.subset <- dat.cbc %>% 
  group_by(id_g) %>% 
  mutate(n = n()) %>% 
  filter(n == 20) %>% 
  ungroup()

# create mlogit data with special function
dat.mlogit <- mlogit.data(dat.cbc.subset, 
                          choice="chosen",
                          shape="long",
                          alt.var = "alt_id",  
                          id.var="id_g")
dat.mlogit
dat.mlogit$idx

# pure conditional model
mlogit1 <- mlogit(chosen ~ 
               0 + gender
             +age
             +job
             +role
             +critique
             +parliament
             +conference
             +reform
             +dist, dat.mlogit)
# now it works...
summary(mlogit1)

var_cov <- solve(-m1$hessian)
se <- std_err <- sqrt(diag(var_cov))

summary(m1)$CoefTable

# compare this to clogit (our approach) with same data subset
model_clogit_subset <- clogit(chosen~
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
                       ,data=dat.cbc.subset, method="efron", robust=TRUE)

summary(model_clogit_subset)

data.frame(clogit_coef = summary(model_clogit_subset)$coef[,1],
           mlogit_coef = summary(mlogit1)$CoefTable[,1],
           diff_coef = round(summary(model)$coef[,1] - summary(m1)$CoefTable[,1], 5),
           clogit_se = summary(model_clogit_subset)$coef[,3],
           mlogit_se = summary(mlogit1)$CoefTable[,2],
           diff_se = round(summary(model)$coef[,3] - summary(m1)$CoefTable[,2], 5),
           clogit_rse = summary(model_clogit_subset)$coef[,4],
           diff_rse = round(summary(model)$coef[,4] - summary(m1)$CoefTable[,2], 5)
           )

# --> coefficients and se are the same, but robust se from clogit are different

# some additional tests
# should we include an intercept in the conditional mlogit model?
# if yes, then one of the alternatives is more preffered irrespective of content
mlogit2 <- mlogit(chosen ~ 
               gender
             +age
             +job
             +role
             +critique
             +parliament
             +conference
             +reform
             +dist, dat.mlogit)
summary(mlogit2)

lrtest(mlogit1, mlogit2) # interpcet makes no differnece --> good

# pure multinomial model
mlogit3 <- mlogit(chosen ~ 0 |
               0 + gender
             +age
             +job
             +role
             +critique
             +parliament
             +conference
             +reform
             + dist, dat.mlogit)
summary(mlogit3)

lrtest(mlogit1, mlogit3) # conditinal model is better




