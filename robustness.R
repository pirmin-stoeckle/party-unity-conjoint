library(data.table)
library(tidyverse)
library(survival)
library(cjoint)

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

# -> within the whole sample, 6% of ratings show an incorrect ordering (chosen worse than not chosen, 
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
#amceplot_robustness1 <- ggplot(pdata_robustness1, aes(x = amce, y = names, shape = specification)) + 
#  geom_pointrange(aes(xmin = lower, xmax = upper), 
#                  size = 0.25,
#                  position = position_dodge(width = 0.5))+
#  geom_vline(xintercept = 0, linetype = 3) +
#  xlab("Change: Pr(Vote for the respective candidate)") +
#  ylab("")

#save plot to pdf
#pdf(file=paste0(getwd(),"/figures/amceplot_robustness1.pdf"))
#amceplot_robustness1
#dev.off()




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
pdf(file=paste0(getwd(),"/figures/amceplot_robustness1.pdf"),width=6.5, height=7)
par(oma=c(0,0,0,0),mar=c(3,0,0,9.5))
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
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[1],"Ideological Distance",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[2],"Intra-Party Critique",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[3],"Parliamentary Voting Behavior",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[4],"Behavior at Congress",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[5],"Reform Clarity",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[6],"Party Role",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[7],"Gender of Candidate",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[8],"Age of Candidate",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[9],"Candidate's Occupation",font=2,cex=.7)
# line
abline(v=0,lty=2)
# axes
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),labels=NA,cex.axis=.9)
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),line=-.5,lwd=0,cex.axis=.9)
axis(4,at=c(50,seq(from=nrow(plotdata_cjoint),to=1)[-napos],-5)
     ,labels=NA,cex.axis=.7)
axis(4,at=c(50,seq(from=nrow(plotdata_cjoint),to=1)[-napos])
     ,labels=c("NA",plotdata_cjoint$nice_names[-napos])
     ,las=1,lwd=0,line=-.3,cex.axis=.9)
# label
mtext(side=1,"Average Marginal Component Effect",outer=F,line=1.7)
# legend
legend("bottomleft",legend=c("clogit","cjoint"),pch=c(20,4),lty=1,cex=1,bg="white")
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
pdf(file=paste0(getwd(),"/figures/amceplot_robustness2.pdf"),width=6.5, height=7)
par(oma=c(0,0,0,0),mar=c(3,0,0,9.5))
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
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[1],"Ideological Distance",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[2],"Intra-Party Critique",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[3],"Parliamentary Voting Behavior",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[4],"Behavior at Congress",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[5],"Reform Clarity",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[6],"Party Role",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[7],"Gender of Candidate",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[8],"Age of Candidate",font=2,cex=.7)
text(x=-.25,y=nrow(plotdata_clogit)+1-napos[9],"Candidate's Occupation",font=2,cex=.7)
# line
abline(v=0,lty=2)
# axes
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),labels=NA,cex.axis=.9)
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),line=-.5,lwd=0,cex.axis=.9)
axis(4,at=c(50,seq(from=nrow(plotdata_clogit_c),to=1)[-napos],-5)
     ,labels=NA,cex.axis=.7)
axis(4,at=c(50,seq(from=nrow(plotdata_clogit_c),to=1)[-napos])
     ,labels=c("NA",plotdata_clogit_c$nice_names[-napos])
     ,las=1,lwd=0,line=-.3,cex.axis=.9)
# label
mtext(side=1,"Average Marginal Component Effect",outer=F,line=1.7)
# legend
legend("bottomleft",legend=c("full sample","consistent ratings"),pch=c(20,4),lty=1,cex=1,bg="white")
dev.off()



##############
# Table Output
##############

library(stargazer)

stargazer(model, model_clogit_correct_ratings,
          order = c(18,19,20,21,13,11,12,14,15,16,17,10,9,1,2,3,7,5,6,8,4), # better change in model formula
          covariate.labels = c("Dist: 1",
                               "Dist: 2",
                               "Dist: 3",
                               "Dist: 4",
                               "Critique: None",
                               "Critique: Former party leader",
                               "Critique: Party faction",
                               "Parliamentary voting: Divided",
                               "Behavior at congress: Neither nor",
                               "Behavior at congress: Divided",
                               "Reform clarity: Low",
                               "Party role: PM party",
                               "Party role: Opposition party",
                               "Candidate gender: Male",
                               "Candidate age: 56 years",
                               "Candidate age: 74 years",
                               "Candidate occupation: Entrepreneur",
                               "Candidate occupation: Lawyer",
                               "Candidate occupation: Politician",
                               "Candidate occupation: Employee (retired)",
                               "Candidate occupation: Activist"),
          style = "ajps",
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "small")


######################################################################
# no carry-over effects (i.e. results are the same when only looking at the first choice screen)
######################################################################

dat_first_screen <- dat[screen == 1 ,]
length(dat_first_screen$chosen)
# sample size (observations) of 6768
length(unique(dat_first_screen$id_g))
# sample size (respondents) of 3394

model_clogit_first_screen <- clogit(chosen~
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
                                    ,data=dat_first_screen, method="efron", robust=TRUE)

summary(model_clogit_first_screen)

# custom function to get data for AMCE plot in consistent format
compute_amce_clogit_first_screen <- function(model) {
  coefs <- coef(model) #get coefficients 
  ses <- summary(model)$coefficients[,4] #get ROBUST ses
  names <- rownames(summary(model)$coefficients) #get names of coefs
  pdata <- data.table(amce=(1/(1+exp(-coefs)))-.5, #compute probability difference to all 0's scenario
                      lower=(1/(1+exp(-(coefs-(1.96*ses)))))-.5, #compute corresponding CI
                      upper=(1/(1+exp(-(coefs+(1.96*ses)))))-.5, #compute corresponding CI
                      names=names,
                      coefs = coefs, 
                      ses = ses,
                      specification = "clogit_first_screen"
  )
  return(pdata)
}

pdata_clogit_first_screen <- compute_amce_clogit_first_screen(model = model_clogit_first_screen)

pdata$specification <- "clogit_all"
names(pdata)[1] <- "amce"
pdata_robustness_first_screen <- rbind(pdata, pdata_clogit_first_screen, fill = TRUE)


# plot
amceplot_robustness_first_screen <- ggplot(pdata_robustness_first_screen, aes(x = amce, y = names, shape = specification)) + 
  geom_pointrange(aes(xmin = lower, xmax = upper), 
                  size = 0.25,
                  position = position_dodge(width = 0.5))+
  geom_vline(xintercept = 0, linetype = 3) +
  xlab("Change: Pr(Vote for the respective candidate)") +
  ylab("")

amceplot_robustness_first_screen
