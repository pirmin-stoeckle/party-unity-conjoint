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




##############
# PLOT
# Robustness 1
##############
# insert nice names for graph and extra rows for baseline categories and labels
pdata_tidy <- pdata %>%
  mutate(nice_names = c("Male",
                        "56 years",
                        "74 years",
                        "Activist",
                        "Lawyer",
                        "Politician",
                        "Entrepreneur",
                        "Employee (retired)",
                        "Opposition party",
                        "PM party",
                        "Former party leader",
                        "Party faction",
                        "None",
                        "Divided Voting",
                        "Neither united nor divided",
                        "Divided",
                        "Low",
                        "1",
                        "2",
                        "3",
                        "4")) %>% 
  add_row(nice_names = "GENDER:", .before = 1) %>% 
  add_row(nice_names = "Female", mean = 0, .before = 2) %>% 
  add_row(nice_names = "AGE:", .before = 4) %>% 
  add_row(nice_names = "38 years", mean = 0, .before = 5) %>% 
  add_row(nice_names = "OCCUPATION:", .before = 8) %>% 
  add_row(nice_names = "Employee", mean = 0, .before = 9) %>% 
  add_row(nice_names = "PARTY ROLE:", .before = 15) %>% 
  add_row(nice_names = "Governing party (not PM party)", mean = 0, .before = 16) %>% 
  add_row(nice_names = "INTRA-PARTY CRITIQUE:", .before = 19) %>% 
  add_row(nice_names = "Rank-and-file Members", mean = 0, .before = 20) %>% 
  add_row(nice_names = "VOTING BEHAVIOR IN PARLIAMENT:", .before = 24) %>% 
  add_row(nice_names = "United Voting", mean = 0, .before = 25) %>% 
  add_row(nice_names = "BEHAVIOR AT PARTY CONFERENCE:", .before = 27) %>% 
  add_row(nice_names = "United", mean = 0, .before = 28) %>% 
  add_row(nice_names = "CLARITY OF REFORM PROPOSALS:", .before = 31) %>% 
  add_row(nice_names = "High", mean = 0, .before = 32) %>% 
  add_row(nice_names = "IDEOLOGICAL DISTANCE:", .before = 34) %>% 
  add_row(nice_names = "0", mean = 0, .before = 35)

# merge nice names
unique(pdata_clogit$names)
all(pdata_clogit$names %in% pdata_tidy$names)
pdata_clogit <- merge(x=pdata_clogit, y=pdata_tidy[,c("names","nice_names")]
                           ,by="names",all.x=T,all.y=F)
pdata_cjoint <- merge(x=pdata_cjoint, y=pdata_tidy[,c("names","nice_names")]
                      ,by="names",all.x=T,all.y=F)

# define groups (clogit)
gender_clogit <- pdata_clogit[pdata_clogit$nice_names %in% c("Female","Male"),]
age_clogit <- pdata_clogit[pdata_clogit$nice_names %in% c("38 years","56 years", "74 years"),]
occup_clogit <- pdata_clogit[pdata_clogit$nice_names %in% c("Employee","Activist", "Lawyer","Politician"
                                                 ,"Entrepreneur", "Employee (retired)"),]
role_clogit <- pdata_clogit[pdata_clogit$nice_names %in% c("Opposition party","PM party"
                                                ,"Governing party (not PM party)"),]
critique_clogit <- pdata_clogit[pdata_clogit$nice_names %in% c("Rank-and-file Members","Former party leader"
                                                    ,"Party faction","None"),]
voting_clogit <- pdata_clogit[pdata_clogit$nice_names %in% c("United Voting","Divided Voting"),]
congress_clogit <- pdata_clogit[pdata_clogit$nice_names %in% c("United","Neither united nor divided"
                                                    ,"Divided"),]
clarity_clogit <- pdata_clogit[pdata_clogit$nice_names %in% c("High","Low"),]
ideology_clogit <- pdata_clogit[pdata_clogit$nice_names %in% c("0","1","2","3","4"),]

# define groups (cjoint)
gender_cjoint <- pdata_cjoint[pdata_cjoint$nice_names %in% c("Female","Male"),]
age_cjoint <- pdata_cjoint[pdata_cjoint$nice_names %in% c("38 years","56 years", "74 years"),]
occup_cjoint <- pdata_cjoint[pdata_cjoint$nice_names %in% c("Employee","Activist", "Lawyer","Politician"
                                                    ,"Entrepreneur", "Employee (retired)"),]
role_cjoint <- pdata_cjoint[pdata_cjoint$nice_names %in% c("Opposition party","PM party"
                                                   ,"Governing party (not PM party)"),]
critique_cjoint <- pdata_cjoint[pdata_cjoint$nice_names %in% c("Rank-and-file Members","Former party leader"
                                                       ,"Party faction","None"),]
voting_cjoint <- pdata_cjoint[pdata_cjoint$nice_names %in% c("United Voting","Divided Voting"),]
congress_cjoint <- pdata_cjoint[pdata_cjoint$nice_names %in% c("United","Neither united nor divided"
                                                       ,"Divided"),]
clarity_cjoint <- pdata_cjoint[pdata_cjoint$nice_names %in% c("High","Low"),]
ideology_cjoint <- pdata_cjoint[pdata_cjoint$nice_names %in% c("0","1","2","3","4"),]

# order attribute levels (based on clogit!)
gender_clogit <- gender_clogit[order(abs(gender_clogit$amce)),]
gender_cjoint <- gender_cjoint[match(gender_clogit$names,gender_cjoint$names),]
age_clogit <- age_clogit[order(abs(age_clogit$amce)),]
age_cjoint <- age_cjoint[match(age_clogit$names,age_cjoint$names),]
occup_clogit <- occup_clogit[order(abs(occup_clogit$amce)),]
occup_cjoint <- occup_cjoint[match(occup_clogit$names,occup_cjoint$names),]
role_clogit <- role_clogit[order(abs(role_cjoint$amce)),]
role_cjoint <- role_cjoint[match(role_clogit$names,role_cjoint$names),]
critique_clogit <- critique_clogit[order(abs(critique_clogit$amce)),]
critique_cjoint <- critique_cjoint[match(critique_clogit$names,critique_cjoint$names),]
voting_clogit <- voting_clogit[order(abs(voting_clogit$amce)),]
voting_cjoint <- voting_cjoint[match(voting_clogit$names,voting_cjoint$names),]
congress_clogit <- congress_clogit[order(abs(congress_clogit$amce)),]
congress_cjoint <- congress_cjoint[match(congress_clogit$names,congress_cjoint$names),]
clarity_clogit <- clarity_clogit[order(abs(clarity_clogit$amce)),]
clarity_cjoint <- clarity_cjoint[match(clarity_clogit$names,clarity_cjoint$names),]
ideology_clogit <- ideology_clogit[order(abs(ideology_clogit$amce)),]
ideology_cjoint <- ideology_cjoint[match(ideology_clogit$names,ideology_cjoint$names),]

# create data frame in correct attribute order
plotdata_clogit <- rbind(NA,ideology_clogit,NA,critique_clogit,NA,voting_clogit
                         ,NA,congress_clogit,NA,clarity_clogit,NA,role_clogit,NA
                         ,gender_clogit,NA,age_clogit,NA,occup_clogit
                  ,fill=TRUE)
plotdata_cjoint <- rbind(NA,ideology_cjoint,NA,critique_cjoint,NA,voting_cjoint
                         ,NA,congress_cjoint,NA,clarity_cjoint,NA,role_cjoint,NA
                         ,gender_cjoint,NA,age_cjoint,NA,occup_cjoint
                         ,fill=TRUE)
napos <- which(is.na(plotdata_clogit$nice_names)) # position of NAs

all(plotdata_clogit$names==plotdata_cjoint$names,na.rm=T)


# PLOT
pcex <- .5 # point size
lcex <- .5 # line size
pdf(file=paste0(getwd(),"/figures/amceplot_robustness1.pdf"),width=6, height=7)
par(oma=c(0,0,0,0),mar=c(3,0,0,7.5))
plot(0,xlim=c(-.51,0),ylim=c(1,nrow(plotdata_cjoint))
     ,type="n",axes=F,ann=F)
# gray polygon
polygon(x=c(-1,0,0,-1),y=c(0,0,50,50)
        ,border=F,col=alpha("gray",alpha=.15))
# add white lines
abline(v=c(-.5,-.4,-.3,-.2,-.1),col="white")
abline(h=nrow(plotdata_cjoint)+1-napos,col="white",lwd=8)
# points and lines (clogit)
points(x=plotdata_clogit$amce
       ,y=(nrow(plotdata_clogit)+.1):1.1,pch=20,cex=pcex)
for(i in 1:nrow(plotdata_clogit)){
  lines(x=c(plotdata_clogit$lower[i],plotdata_clogit$upper[i])
        ,y=c(nrow(plotdata_clogit)+1.1-i,nrow(plotdata_clogit)+1.1-i),lty=1,cex=lcex)
}
# points and lines (cjoint)
points(x=plotdata_cjoint$amce
       ,y=(nrow(plotdata_cjoint)-.1):.9,pch=4,cex=pcex)
for(i in 1:nrow(plotdata_cjoint)){
  lines(x=c(plotdata_cjoint$lower[i],plotdata_cjoint$upper[i])
        ,y=c(nrow(plotdata_cjoint)+.9-i,nrow(plotdata_cjoint)+.9-i),lty=1,cex=lcex)
}
# category text
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[1],"Ideological Distance",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[2],"Intra-Party Critique",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[3],"Parliamentary Voting Behavior",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[4],"Behavior at Congress",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[5],"Reform Clarity",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[6],"Party Role",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[7],"Gender of Candidate",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[8],"Age of Candidate",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[9],"Candidate's Occupation",font=2,cex=.6)
# line
abline(v=0,lty=2)
# axes
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),labels=NA,cex.axis=.8)
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),line=-.5,lwd=0,cex.axis=.8)
axis(4,at=c(50,seq(from=nrow(plotdata_cjoint),to=1)[-napos],-5)
     ,labels=NA,cex.axis=.7)
axis(4,at=c(50,seq(from=nrow(plotdata_cjoint),to=1)[-napos])
     ,labels=c("NA",plotdata_cjoint$nice_names[-napos])
     ,las=1,lwd=0,line=-.3,cex.axis=.7)
# label
mtext(side=1,"Average Marginal Component Effect",outer=F,line=1.7)
# legend
legend("bottomleft",legend=c("clogit","cjoint"),pch=c(20,4),lty=1,cex=.9,bg="white")
dev.off()











##############
# PLOT
# Robustness 2
##############
# merge nice names
unique(pdata_clogit_correct_ratings$names)
all(pdata_clogit_correct_ratings$names %in% pdata_clogit$names)
pdata_clogit_correct_ratings <- merge(x=pdata_clogit_correct_ratings
                                      ,y=pdata_clogit[,c("names","nice_names")]
                                      ,by="names",all.x=T,all.y=F)

# define groups (pdata_clogit_correct_ratings)
gender_clogit_c <- pdata_clogit_correct_ratings[pdata_clogit_correct_ratings$nice_names %in% c("Female","Male"),]
age_clogit_c <- pdata_clogit_correct_ratings[pdata_clogit_correct_ratings$nice_names %in% c("38 years","56 years", "74 years"),]
occup_clogit_c <- pdata_clogit_correct_ratings[pdata_clogit_correct_ratings$nice_names %in% c("Employee","Activist", "Lawyer","Politician"
                                                            ,"Entrepreneur", "Employee (retired)"),]
role_clogit_c <- pdata_clogit_correct_ratings[pdata_clogit_correct_ratings$nice_names %in% c("Opposition party","PM party"
                                                           ,"Governing party (not PM party)"),]
critique_clogit_c <- pdata_clogit_correct_ratings[pdata_clogit_correct_ratings$nice_names %in% c("Rank-and-file Members","Former party leader"
                                                               ,"Party faction","None"),]
voting_clogit_c <- pdata_clogit_correct_ratings[pdata_clogit_correct_ratings$nice_names %in% c("United Voting","Divided Voting"),]
congress_clogit_c <- pdata_clogit_correct_ratings[pdata_clogit_correct_ratings$nice_names %in% c("United","Neither united nor divided"
                                                               ,"Divided"),]
clarity_clogit_c <- pdata_clogit_correct_ratings[pdata_clogit_correct_ratings$nice_names %in% c("High","Low"),]
ideology_clogit_c <- pdata_clogit_correct_ratings[pdata_clogit_correct_ratings$nice_names %in% c("0","1","2","3","4"),]

# order attribute levels (based on clogit!)
gender_clogit_c <- gender_cjoint[match(gender_clogit$names,gender_cjoint$names),]
age_clogit_c <- age_clogit_c[match(age_clogit$names,age_clogit_c$names),]
occup_clogit_c <- occup_clogit_c[match(occup_clogit$names,occup_clogit_c$names),]
role_clogit_c <- role_clogit_c[match(role_clogit$names,role_clogit_c$names),]
critique_clogit_c <- critique_clogit_c[match(critique_clogit$names,critique_clogit_c$names),]
voting_clogit_c <- voting_clogit_c[match(voting_clogit$names,voting_clogit_c$names),]
congress_clogit_c <- congress_clogit_c[match(congress_clogit$names,congress_clogit_c$names),]
clarity_clogit_c <- clarity_clogit_c[match(clarity_clogit$names,clarity_clogit_c$names),]
ideology_clogit_c <- ideology_clogit_c[match(ideology_clogit$names,ideology_clogit_c$names),]

# create data frame in correct attribute order
plotdata_clogit_c <- rbind(NA,ideology_clogit_c,NA,critique_clogit_c,NA,voting_clogit_c
                           ,NA,congress_clogit_c,NA,clarity_clogit_c,NA,role_clogit_c,NA
                           ,gender_clogit_c,NA,age_clogit_c,NA,occup_clogit_c
                           ,fill=TRUE)
all(plotdata_clogit$names==plotdata_clogit_c$names,na.rm=T)


# PLOT
pcex <- .5 # point size
lcex <- .5 # line size
pdf(file=paste0(getwd(),"/figures/amceplot_robustness2.pdf"),width=6, height=7)
par(oma=c(0,0,0,0),mar=c(3,0,0,7.5))
plot(0,xlim=c(-.51,0),ylim=c(1,nrow(plotdata_clogit_c))
     ,type="n",axes=F,ann=F)
# gray polygon
polygon(x=c(-1,0,0,-1),y=c(0,0,50,50)
        ,border=F,col=alpha("gray",alpha=.15))
# add white lines
abline(v=c(-.5,-.4,-.3,-.2,-.1),col="white")
abline(h=nrow(plotdata_clogit_c)+1-napos,col="white",lwd=8)
# points and lines (clogit)
points(x=plotdata_clogit$amce
       ,y=(nrow(plotdata_clogit)+.1):1.1,pch=20,cex=pcex)
for(i in 1:nrow(plotdata_clogit)){
  lines(x=c(plotdata_clogit$lower[i],plotdata_clogit$upper[i])
        ,y=c(nrow(plotdata_clogit)+1.1-i,nrow(plotdata_clogit)+1.1-i),lty=1,cex=lcex)
}
# points and lines (clogit correct ratings)
points(x=plotdata_clogit_c$amce
       ,y=(nrow(plotdata_clogit_c)-.1):.9,pch=4,cex=pcex)
for(i in 1:nrow(plotdata_clogit_c)){
  lines(x=c(plotdata_clogit_c$lower[i],plotdata_clogit_c$upper[i])
        ,y=c(nrow(plotdata_clogit_c)+.9-i,nrow(plotdata_clogit_c)+.9-i),lty=1,cex=lcex)
}
# category text
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[1],"Ideological Distance",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[2],"Intra-Party Critique",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[3],"Parliamentary Voting Behavior",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[4],"Behavior at Congress",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[5],"Reform Clarity",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[6],"Party Role",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[7],"Gender of Candidate",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[8],"Age of Candidate",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[9],"Candidate's Occupation",font=2,cex=.6)
# line
abline(v=0,lty=2)
# axes
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),labels=NA,cex.axis=.8)
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),line=-.5,lwd=0,cex.axis=.8)
axis(4,at=c(50,seq(from=nrow(plotdata_clogit_c),to=1)[-napos],-5)
     ,labels=NA,cex.axis=.7)
axis(4,at=c(50,seq(from=nrow(plotdata_clogit_c),to=1)[-napos])
     ,labels=c("NA",plotdata_clogit_c$nice_names[-napos])
     ,las=1,lwd=0,line=-.3,cex.axis=.7)
# label
mtext(side=1,"Average Marginal Component Effect",outer=F,line=1.7)
# legend
legend("bottomleft",legend=c("All","Corrected"),pch=c(20,4),lty=1,cex=.9,bg="white")
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
# if yes, then one of the alternatives is more preferred irrespective of content
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

lrtest(mlogit1, mlogit2) # intercept makes no difference --> good

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

lrtest(mlogit1, mlogit3) # conditional model is better




