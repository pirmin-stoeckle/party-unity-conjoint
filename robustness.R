

######################################################################
# comparison of model specifications
######################################################################

# choice data descriptives
head(dat)
summary(dat)

# our approach

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
                       ,data=dat, method="efron", robust=TRUE)


compute_amce_clogit <- function(model) {
  #(get data for) AMCEs plot
  coefs <- coef(model) #get cofficients 
  ses <- summary(model)$coefficients[,4] #get ROBUST res
  names <- rownames(summary(model)$coefficients) #get names of coefs
  pdata <- data.table(amce=(1/(1+exp(-coefs)))-.5, #compute probability difference to all 0's scenario
                      lower=(1/(1+exp(-(coefs-(1.96*ses)))))-.5, #compute corresponding CI
                      upper=(1/(1+exp(-(coefs+(1.96*ses)))))-.5, #compute corresponding CI
                      names=names,
                      specification = "clogit"
  )
  return(pdata)
}

pdata_clogit <- compute_amce_clogit(model = model_clogit)

# linear regression

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
               ,data=dat)
summary(model_lm)

compute_amce_lm <- function(model) {
  #(get data for) AMCEs plot
  coefs <- coef(model)[-1] #get cofficients but leave out intercept
  ses <- summary(model)$coefficients[-1,2] #get ROBUST res
  names <- rownames(summary(model)$coefficients[-1,]) #get names of coefs
  pdata <- data.table(amce=coefs, #compute probability difference to all 0's scenario
                      lower=coefs-(1.96*ses), #compute corresponding CI
                      upper=coefs+(1.96*ses), #compute corresponding CI
                      names=names,
                      specification = "lm"
  )
  return(pdata)
}

pdata_lm <- compute_amce_lm(model = model_lm)

# hainmueller

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
                     data = dat, design = "uniform",
                     respondent.varying = NULL, subset = NULL,
                     respondent.id = "id_g", cluster = TRUE, na.ignore=T,
                     weights = NULL, baselines = NULL)


compute_amce_cjoint <- function(model) {
  #(get data for) AMCEs plot
  coefs <- summary(model_cjoint)$amce[,3] #get cofficients
  ses <- summary(model_cjoint)$amce[,4] #get ROBUST res
  names <- paste0(summary(model_cjoint)$amce$Attribute,summary(model_cjoint)$amce$Level)  #get names of coefs
  pdata <- data.table(amce=coefs, #compute probability difference to all 0's scenario
                      lower=coefs-(1.96*ses), #compute corresponding CI
                      upper=coefs+(1.96*ses), #compute corresponding CI
                      names=names,
                      specification = "cjoint"
  )
  return(pdata)
}

pdata_cjoint <- compute_amce_cjoint(model = model_cjoint)


# logit

model_logit <- glm(chosen ~  
                     gender
                   +age
                   +job
                   +role
                   +critique
                   +parliament
                   +conference
                   +reform
                   +dist
                   #+strata(id_screen)
                   #+cluster(id_g)
                   ,data=dat, family="binomial")
summary(model_logit)$coefficients

compute_amce_logit <- function(model) {
  #(get data for) AMCEs plot
  coefs <- coef(model)[-1] #get cofficients  but without intercept
  ses <- summary(model)$coefficients[-1,2] #get  res
  names <- rownames(summary(model)$coefficients[-1,]) #get names of coefs
  pdata <- data.table(amce=(1/(1+exp(-coefs)))-.5, #compute probability difference to all 0's scenario
                      lower=(1/(1+exp(-(coefs-(1.96*ses)))))-.5, #compute corresponding CI
                      upper=(1/(1+exp(-(coefs+(1.96*ses)))))-.5, #compute corresponding CI
                      names=names,
                      specification = "logit"
  )
  return(pdata)
}

pdata_logit <- compute_amce_logit(model = model_logit)

# join in one dataframe

pdata_comp <- rbind(pdata_clogit, pdata_lm, pdata_cjoint, pdata_logit)


amceplot_robustness <- ggplot(pdata_comp, aes(x = amce, y = names, shape = specification)) + 
  geom_pointrange(aes(xmin = lower, xmax = upper), 
                  size = 0.25,
                  position = position_dodge(width = 0.5))+
  geom_vline(xintercept = 0, linetype = 3) +
  xlab("Change: Pr(Vote for the respective candidate)") +
  ylab("")

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/amceplot_robustness.pdf"))
amceplot_robustness
dev.off()


