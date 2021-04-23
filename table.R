library(knitr)
library(kableExtra)
library(stargazer)

# loading analysis script
source("analysis.R")

model <- clogit(chosen~
                  dist +
                  critique +
                  parliament +
                  conference +
                  reform +
                  role +
                  gender +
                  age +
                  job +
                  strata(id_screen) +
                  cluster(id_g),
                data=dat, method="efron", robust=TRUE)

coefs <- coef(model)
rses <- as.character(round(summary(model)$coefficients[,4], 3))
zs <- coef(model)/summary(model)$coefficients[,4]
ps <- 2*pnorm(-abs(zs))
names <- rownames(summary(model)$coefficients)


reg_table <- data.frame(names, coefs, rses, zs, ps)

reg_table$coefs_stars <- ifelse(reg_table$ps<0.05,
                                ifelse(reg_table$ps<0.01,
                                       ifelse(reg_table$ps<0.001, paste0(sprintf("%.3f", reg_table$coefs), "***"),
                                              paste0(sprintf("%.3f", reg_table$coefs), "**")),
                                       paste0(sprintf("%.3f", reg_table$coefs), "*")),
                                sprintf("%.3f", reg_table$coefs))

out <- reg_table %>% select(names, coefs_stars, rses)

# add empty rows to include the baseline categories
out <- out %>% 
  add_row(names = "NA",coefs_stars = "", rses = "", .before = 1) %>% 
  add_row(names = "NA",coefs_stars = "", rses = "", .before = 6) %>% 
  add_row(names = "NA",coefs_stars = "", rses = "", .before = 10) %>% 
  add_row(names = "NA",coefs_stars = "", rses = "", .before = 12) %>% 
  add_row(names = "NA",coefs_stars = "", rses = "", .before = 15) %>% 
  add_row(names = "NA", coefs_stars = "", rses = "",.before = 17) %>% 
  add_row(names = "NA",coefs_stars = "", rses = "", .before = 20) %>% 
  add_row(names = "NA", coefs_stars = "", rses = "",.before = 22) %>% 
  add_row(names = "NA",coefs_stars = "", rses = "", .before = 25) %>% 
  add_row(names = "NA",coefs_stars = "", rses = "") %>% 
  add_row(names = "Log Likelihood", 
          coefs_stars = as.character(round(summary(model)$loglik[2], 3)), 
          rses = "") %>% 
  add_row(names = "N (observations)", 
          coefs_stars = as.character(dat[complete.cases(chosen, dist), .(n_observations = length(id_screen))]), 
          rses = "") %>% 
  add_row(names = "N (choices)", 
          coefs_stars = as.character(dat[complete.cases(chosen, dist), .(n_tasks = length(unique(id_screen)))]), 
          rses = "") %>% 
  add_row(names = "N (respondents)", 
          coefs_stars = as.character(dat[complete.cases(chosen, dist), .(n_respondents = length(unique(id_g)))]), 
          rses = "")

# names vector for output
names_out <-  c("Ideological distance (reference: 0)", "1", "2", "3", "4",
            "Critique (reference: Rank-and-file members)","Former party leader", "Party faction", "None", 
            "Parliamentary voting (reference: United)", "Divided",
            "Behavior at congress (reference: United)", "Neither united nor divided", "Divided",
            "Reform clarity (reference: High)", "Low",
            "Party role (reference: Junior coalition partner)", "Opposition party", "PM party",
            "Candidate's gender (reference: Female)", "Male",
            "Candidate's age (reference: 38 years)", "56 years", "74 years",
            "Candidate's occupation (reference: Employee)", "Activist", "Lawyer", "Politician", "Entrepreneur", "Employee (retired)",
            "",
            "Log Likelihood",
            "N (observations)", "N (choices)", "N (respondents)")

out$names <- names_out

# check correct names by row.names
out 
# then remove row-names
row.names(out) <- NULL


kable(out, format = "latex", 
      col.names = c("", "coefficient", "robust s.e."), 
      caption = "Estimated coefficients from the conditional logit model",
      label = "reg-output",
      booktabs = T, linesep = "") %>% 
  add_footnote("***p<0.001; **p<0.01; *p<0.05", notation = "none") %>% 
  kable_styling(font_size = 11,
                latex_options = c("hold_position"),
                position  = "center")
