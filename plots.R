library(tidyverse)
library(data.table)
library(dplyr)

# cleaning directory
rm(list=ls())

# loading analysis script
source("./analysis.R")

# pdata from analysis.R can be used for plotting AMCEs

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

# check new names versus those from the analysis
pdata_tidy %>% select(names, nice_names)



### AMCE base plot
# define group names
unique(pdata_tidy$nice_names)
gender <- pdata_tidy[pdata_tidy$nice_names %in% c("Female","Male"),]
age <- pdata_tidy[pdata_tidy$nice_names %in% c("38 years","56 years", "74 years"),]
occup <- pdata_tidy[pdata_tidy$nice_names %in% c("Employee","Activist", "Lawyer","Politician"
                                   ,"Entrepreneur", "Employee (retired)"),]
role <- pdata_tidy[pdata_tidy$nice_names %in% c("Opposition party","PM party"
                                  ,"Governing party (not PM party)"),]
critique <- pdata_tidy[pdata_tidy$nice_names %in% c("Rank-and-file Members","Former party leader"
                                      ,"Party faction","None"),]
voting <- pdata_tidy[pdata_tidy$nice_names %in% c("United Voting","Divided Voting"),]
congress <- pdata_tidy[pdata_tidy$nice_names %in% c("United","Neither united nor divided"
                                      ,"Divided"),]
clarity <- pdata_tidy[pdata_tidy$nice_names %in% c("High","Low"),]
ideology <- pdata_tidy[pdata_tidy$nice_names %in% c("0","1","2","3","4"),]

# order attribute levels
gender <- gender[order(abs(gender$mean)),]
age <- age[order(abs(age$mean)),]
occup <- occup[order(abs(occup$mean)),]
role <- role[order(abs(role$mean)),]
critique <- critique[order(abs(critique$mean)),]
voting <- voting[order(abs(voting$mean)),]
congress <- congress[order(abs(congress$mean)),]
clarity <- clarity[order(abs(clarity$mean)),]
ideology <- ideology[order(abs(ideology$mean)),]

# adjust labels
role$nice_names[role$nice_names=="Governing party (not PM party)"] <- "Junior Coalition Partner"

# create data frame in correct attribute order
plotdata <- rbind(NA,ideology,NA,critique,NA,voting,NA,congress
                  ,NA,clarity,NA,role,NA,gender,NA,age,NA,occup
                  ,fill=TRUE)
napos <- which(is.na(plotdata$nice_names)) # position of NAs


# PLOT
ptype <- 20 # point type
ltype <- 1 # line type
pcex <- 1 # point size
lcex <- 1 # line size
pdf(file=paste0(getwd(),"/figures/amceplot2.pdf"),width=6, height=7)
par(oma=c(0,0,0,0),mar=c(3,0,0,7.5))
plot(0,xlim=c(-.5,0),ylim=c(1,nrow(plotdata))
     ,type="n",axes=F,ann=F)
# gray polygon
polygon(x=c(-1,1,1,-1),y=c(-20,-20,50,50),border=F,col=alpha("gray",alpha=.15))
# add white lines
abline(v=c(-.5,-.4,-.3,-.2,-.1),col="white")
abline(h=nrow(plotdata)+1-napos,col="white",lwd=8)
# points and lines
points(x=plotdata$mean
       ,y=nrow(plotdata):1,pch=ptype,cex=pcex)
for(i in 1:nrow(plotdata)){
  lines(x=c(plotdata$lower[i],plotdata$upper[i])
        ,y=c(nrow(plotdata)+1-i,nrow(plotdata)+1-i),lty=ltype,cex=lcex)
}
# category text
text(x=-.25,y=nrow(plotdata)+1-napos[1],"Ideological Distance",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata)+1-napos[2],"Intra-Party Critique",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata)+1-napos[3],"Parliamentary Voting Behavior",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata)+1-napos[4],"Behavior at Congress",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata)+1-napos[5],"Reform Clarity",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata)+1-napos[6],"Party Role",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata)+1-napos[7],"Gender of Candidate",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata)+1-napos[8],"Age of Candidate",font=2,cex=.6)
text(x=-.25,y=nrow(plotdata)+1-napos[9],"Candidate's Occupation",font=2,cex=.6)
# line
abline(v=0,lty=2)
# axes
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),labels=NA,cex.axis=.8)
axis(1,at=c(-1,-.5,-.4,-.3,-.2,-.1,0),line=-.5,lwd=0,cex.axis=.8)
axis(4,at=c(50,seq(from=nrow(plotdata),to=1)[-napos],-5)
     ,labels=NA,cex.axis=.7)
axis(4,at=c(50,seq(from=nrow(plotdata),to=1)[-napos])
     ,labels=c("NA",plotdata$nice_names[-napos])
     ,las=1,lwd=0,line=-.3,cex.axis=.7)
# label
mtext(side=1,"Average Marginal Component Effect",outer=F,line=1.7)
dev.off()




# alternative AMCE estimation following Hainmueller, Hopkins, Yamamoto, T. (2014)
# cjoint::plot.amce(model_cjoint, group.order = c("gender",
#                                                 "age",
#                                                 "job",
#                                                 "role",
#                                                 "critique",
#                                                 "parliament",
#                                                 "conference",
#                                                 "reform",
#                                                 "dist"))
# -> similar (but not identical)


# scenarios from analysis.R can be used to get probabilities for scenarios compared to baseline

# Effect of ideological distance by unity in party conference behavior
dist_by_conference <- scenarios[role == "Junior coalition partner" &
                         conference %in% c("United", "Neither united nor divided", "Divided") &
                         parliament == "United voting" &
                         critique == "Rank-and-file members" &
                         reform == "High" &
                         gender == "Female" &
                         age == "38 years" &
                         job == "Employee" &
                         dist %in% c(0:4),
                         .(conference, dist, lower, prob, upper)]


# PLOT
pdf(file=paste0(getwd(),"/figures/distplot_conference2.pdf"),width=7, height=3)
par(oma=c(0,0,1,0),mar=c(3,3,0,0))
plot(0,xlim=c(.5,5.5),ylim=c(0,50)
     ,type="n",axes=F,ann=F)
# gray polygon
polygon(x=c(.5,10,10,.5),y=c(-20,-20,70,70),border=F,col=alpha("gray",alpha=.15))
# add white lines
abline(v=c(0.5,1.5,2.5,3.5,4.5),col="white",lwd=13)
# points & lines
for(i in as.numeric(unique(dist_by_conference$dist))){
  points(x=i-.1
         ,y=dist_by_conference$prob[dist_by_conference$conference=="United"
                                    & as.numeric(dist_by_conference$dist)==i]
         ,pch=20
         ,cex=.8)
  lines(x=c(i-.1,i-.1)
        ,y=c(dist_by_conference$lower[dist_by_conference$conference=="United"
                                      & as.numeric(dist_by_conference$dist)==i]
             ,dist_by_conference$upper[dist_by_conference$conference=="United"
                                      & as.numeric(dist_by_conference$dist)==i]))
  points(x=i
         ,y=dist_by_conference$prob[dist_by_conference$conference=="Neither united nor divided"
                                    & as.numeric(dist_by_conference$dist)==i]
         ,pch=4
         ,cex=.8)
  lines(x=c(i,i)
        ,y=c(dist_by_conference$lower[dist_by_conference$conference=="Neither united nor divided"
                                      & as.numeric(dist_by_conference$dist)==i]
             ,dist_by_conference$upper[dist_by_conference$conference=="Neither united nor divided"
                                       & as.numeric(dist_by_conference$dist)==i]))
  points(x=i+.1
         ,y=dist_by_conference$prob[dist_by_conference$conference=="Divided"
                                    & as.numeric(dist_by_conference$dist)==i]
         ,pch=17
         ,cex=.8)
  lines(x=c(i+.1,i+.1)
        ,y=c(dist_by_conference$lower[dist_by_conference$conference=="Divided"
                                      & as.numeric(dist_by_conference$dist)==i]
             ,dist_by_conference$upper[dist_by_conference$conference=="Divided"
                                       & as.numeric(dist_by_conference$dist)==i]))
}
# axes
axis(1,at=c(-1,1:6),labels=c("",0:4,""))
axis(2,las=1)
mtext("Predicted Vote Share vs. Baseline",side=2,line=2)
mtext("Ideological Distance",side=1,line=2)
# legend
legend("topright",pch=c(20,4,17),legend=c("United","Neither","Divided"),cex=.8,bg="white")
dev.off()



# Effect of ideological distance by unity in parliamentary voting
dist_by_parliament <- scenarios[role == "government party (not PM party)" &
                                  conference == "united" &
                                  parliament %in% c("united", "divided") &
                                  critique == "rank-and-file members" &
                                  reform == "high" &
                                  gender == "female" &
                                  age == "38y" &
                                  job == "employee" &
                                  dist %in% c(0:4),]

# #plot
# distplot_parliament <- ggplot(dist_by_parliament, aes(x = prob, y = dist)) +
#   geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.25) +
#   facet_wrap(vars(parliament)) +
#   ggtitle("Voting behavior in parliament") +
#   ylab("Ideological distance") +
#   xlab("Predicted vote share versus baseline") +
#   coord_flip() +
#   theme_bw()
# 
# #save plot to pdf
# pdf(file=paste0(getwd(),"/figures/distplot_parliament.pdf"), width = 7, height = 3)
# distplot_parliament
# dev.off()


# ALTERNATIVE PLOT
pdf(file=paste0(getwd(),"/figures/distplot_parliament2.pdf"),width=7, height=3)
par(oma=c(0,0,1,0),mar=c(3,3,0,0))
plot(0,xlim=c(.5,5.5),ylim=c(0,50)
     ,type="n",axes=F,ann=F)
# gray polygon
polygon(x=c(.5,10,10,.5),y=c(-20,-20,70,70),border=F,col=alpha("gray",alpha=.15))
# add white lines
abline(v=c(0.5,1.5,2.5,3.5,4.5),col="white",lwd=13)
# points & lines
for(i in as.numeric(unique(dist_by_parliament$dist))){
  points(x=i-.1
         ,y=dist_by_parliament$prob[dist_by_parliament$parliament=="united"
                                    & as.numeric(dist_by_parliament$dist)==i]
         ,pch=20
         ,cex=.8)
  lines(x=c(i-.1,i-.1)
        ,y=c(dist_by_parliament$lower[dist_by_parliament$parliament=="united"
                                      & as.numeric(dist_by_parliament$dist)==i]
             ,dist_by_parliament$upper[dist_by_parliament$parliament=="united"
                                       & as.numeric(dist_by_parliament$dist)==i]))
  points(x=i+.1
         ,y=dist_by_parliament$prob[dist_by_parliament$parliament=="divided"
                                    & as.numeric(dist_by_parliament$dist)==i]
         ,pch=4
         ,cex=.8)
  lines(x=c(i+.1,i+.1)
        ,y=c(dist_by_parliament$lower[dist_by_parliament$parliament=="divided"
                                      & as.numeric(dist_by_parliament$dist)==i]
             ,dist_by_parliament$upper[dist_by_parliament$parliament=="divided"
                                       & as.numeric(dist_by_parliament$dist)==i]))
}
# axes
axis(1,at=c(-1,1:6),labels=c("",0:4,""))
axis(2,las=1)
mtext("Predicted Vote Share vs. Baseline",side=2,line=2)
mtext("Ideological Distance",side=1,line=2)
# legend
legend("topright",pch=c(20,4),legend=c("United","Divided"),cex=.8,bg="white")
dev.off()






# Effect of ideological distance by reform clarity
dist_by_reform <- scenarios[role == "government party (not PM party)" &
                                  conference == "united" &
                                  parliament == "united" &
                                  critique == "rank-and-file members" &
                                  reform %in% c("low", "high") &
                                  gender == "female" &
                                  age == "38y" &
                                  job == "employee" &
                                  dist %in% c(0:4),]
  

# #plot
# distplot_reform <- ggplot(dist_by_reform, aes(x = prob, y = dist)) +
#   geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.25) +
#   facet_wrap(vars(reform)) +
#   ggtitle("Clarity of reform proposals") +
#   ylab("Ideological distance") +
#   xlab("Predicted vote share versus baseline") +
#   coord_flip() +
#   theme_bw()
# 
# #save plot to pdf
# pdf(file=paste0(getwd(),"/figures/distplot_reform.pdf"), width = 7, height = 3)
# distplot_reform
# dev.off()

# Effect of ideological distance by internal critique
dist_by_critique <- scenarios[role == "government party (not PM party)" &
                              conference == "united" &
                              parliament == "united" &
                              critique %in% c("rank-and-file members"
                                              ,"former party leader"
                                              ,"party faction"
                                              ,"none") &
                              reform == "high" &
                              gender == "female" &
                              age == "38y" &
                              job == "employee" &
                              dist %in% c(0:4),]


# #plot
# distplot_critique <- ggplot(dist_by_critique, aes(x = prob, y = dist)) +
#   geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.25) +
#   facet_grid(vars(), vars(critique)) +
#   ggtitle("Intra-party critique") +
#   ylab("Ideological distance") +
#   xlab("Predicted vote share versus baseline") +
#   coord_flip() +
#   theme_bw()
# 
# #save plot to pdf
# pdf(file=paste0(getwd(),"/figures/distplot_critique.pdf"), width = 7, height = 3)
# distplot_critique
# dev.off()



# ALTERNATIVE PLOT
pdf(file=paste0(getwd(),"/figures/distplot_critique2.pdf"),width=7, height=3)
par(oma=c(0,0,1,0),mar=c(3,3,0,0))
plot(0,xlim=c(.5,5.5),ylim=c(0,50)
     ,type="n",axes=F,ann=F)
# gray polygon
polygon(x=c(.5,10,10,.5),y=c(-20,-20,70,70),border=F,col=alpha("gray",alpha=.15))
# add white lines
abline(v=c(0.5,1.5,2.5,3.5,4.5),col="white",lwd=13)
# points & lines
for(i in as.numeric(unique(dist_by_critique$dist))){
  # rank and file
  points(x=i-.15
         ,y=dist_by_critique$prob[dist_by_critique$critique=="rank-and-file members"
                                    & as.numeric(dist_by_critique$dist)==i]
         ,pch=20
         ,cex=.8)
  lines(x=c(i-.15,i-.15)
        ,y=c(dist_by_critique$lower[dist_by_critique$critique=="rank-and-file members"
                                      & as.numeric(dist_by_critique$dist)==i]
             ,dist_by_critique$upper[dist_by_critique$critique=="rank-and-file members"
                                       & as.numeric(dist_by_critique$dist)==i]))
  # none
  points(x=i-.05
         ,y=dist_by_critique$prob[dist_by_critique$critique=="none"
                                  & as.numeric(dist_by_critique$dist)==i]
         ,pch=4
         ,cex=.8)
  lines(x=c(i-.05,i-.05)
        ,y=c(dist_by_critique$lower[dist_by_critique$critique=="none"
                                    & as.numeric(dist_by_critique$dist)==i]
             ,dist_by_critique$upper[dist_by_critique$critique=="none"
                                     & as.numeric(dist_by_critique$dist)==i]))
  # former leader
  points(x=i+.05
         ,y=dist_by_critique$prob[dist_by_critique$critique=="former party leader"
                                    & as.numeric(dist_by_critique$dist)==i]
         ,pch=17
         ,cex=.8)
  lines(x=c(i+.05,i+.05)
        ,y=c(dist_by_critique$lower[dist_by_critique$critique=="former party leader"
                                      & as.numeric(dist_by_critique$dist)==i]
             ,dist_by_critique$upper[dist_by_critique$critique=="former party leader"
                                       & as.numeric(dist_by_critique$dist)==i]))
  # faction
  points(x=i+.15
         ,y=dist_by_critique$prob[dist_by_critique$critique=="party faction"
                                  & as.numeric(dist_by_critique$dist)==i]
         ,pch=18
         ,cex=.8)
  lines(x=c(i+.15,i+.15)
        ,y=c(dist_by_critique$lower[dist_by_critique$critique=="party faction"
                                    & as.numeric(dist_by_critique$dist)==i]
             ,dist_by_critique$upper[dist_by_critique$critique=="party faction"
                                     & as.numeric(dist_by_critique$dist)==i]))
  
}
# axes
axis(1,at=c(-1,1:6),labels=c("",0:4,""))
axis(2,las=1)
mtext("Predicted Vote Share vs. Baseline",side=2,line=2)
mtext("Ideological Distance",side=1,line=2)
# legend
legend("topright",pch=c(20,4,17,18),legend=c("Rank-and-File","None","Former Leader","Faction")
       ,cex=.8,bg="white")
dev.off()



###############################################
# simulating specific competitions
###############################################

# function to convert linear predictors to predicted probabilities
compute_probs_one_vs_second <- function(x) {
  prob.first <- exp(x[1])/(sum(exp(x)))
  return(c(prob.first, 1-prob.first))
}

cases1_2a <- rbind(linpreds[role == "opposition party" &
                          conference == "divided" &
                          parliament == "divided" &
                          critique == "former party leader" &
                          reform == "low" &
                          gender == "female" &
                          age == "38y" &
                          job == "employee" &
                          dist == 0
                        ,c("median", "upper", "lower")],
               linpreds[role == "opposition party" &
                          conference == "divided" &
                          parliament == "divided" &
                          critique == "former party leader" &
                          reform == "low" &
                          gender == "female" &
                          age == "38y" &
                          job == "employee" &
                          dist == 1
                        ,c("median", "upper", "lower")]
)

cases1_2b <- rbind(linpreds[role == "opposition party" &
                              conference == "divided" &
                              parliament == "divided" &
                              critique == "former party leader" &
                              reform == "low" &
                              gender == "female" &
                              age == "38y" &
                              job == "employee" &
                              dist == 0
                            ,c("median", "upper", "lower")],
                   linpreds[role == "opposition party" &
                              conference == "united" &
                              parliament == "united" &
                              critique == "none" &
                              reform == "low" &
                              gender == "female" &
                              age == "38y" &
                              job == "employee" &
                              dist == 1
                            ,c("median", "upper", "lower")]
)

predprob1_2a <- as.data.frame(apply(cases1_2a,2,compute_probs_one_vs_second))
colnames(predprob1_2a) <- c("prob","CI1","CI2")

predprob1_2b <- as.data.frame(apply(cases1_2b,2,compute_probs_one_vs_second))
colnames(predprob1_2b) <- c("prob","CI1","CI2")

predprob <- rbind(predprob1_2a[1,], predprob1_2b[1,])

abs(predprob$prob[1]-predprob[1,2:3])
abs(predprob$prob[2]-predprob[2,2:3])


# # plot
# competitionplot <- ggplot(predprob, aes(x = fct_rev(fct_inorder(c("Party 1 vs Party 2a",
#                                   "Party 1 vs Party 2b"))), y = prob)) +
#   geom_pointrange(aes(ymin = CI1, ymax = CI2), size = 0.25) +
#   ylim(c(0, 1)) +
#   ylab("Expected Probability to Vote for Party 1") +
#   xlab("") +
#   geom_hline(yintercept = 0.5, linetype = 3, col = "blue") +
#   coord_flip() +
#   theme_bw()
# 
# #save plot to pdf
# pdf(file=paste0(getwd(),"/figures/competitionplot.pdf"), width = 7, height = 3)
# competitionplot
# dev.off()



# ALTERNATIVE PLOT
pdf(file=paste0(getwd(),"/figures/competitionplot2.pdf"), width=7, height=2)
par(mar=c(0,0,0,0),mar=c(0,.5,0,.5))
plot(0,xlim=c(0,1),ylim=c(-4.5,1.2),type="n",axes=F,ann=F)
# 1 vs. 2a
polygon(x=c(-.002,1.002,1.002,-.002),y=c(0,0,.5,.5),border=F
        ,col=alpha("gray",alpha=.3))
text(x=.05,y=1.1,"Party 1",cex=.9,font=2)
text(x=.95,y=1.1,"Party 2a",cex=.9,font=2)
for(i in c(0,.25,.5,.75,1)){
  lines(x=c(i,i),y=c(.5,0),lwd=.7,lty=3)
  text(x=i,y=-.4,paste0(100-i*100,":",i*100),cex=.7)
}
polygon(x=c(1-predprob$CI1[1],1-predprob$CI2[1]
            ,1-predprob$CI2[1],1-predprob$CI1[1])
        ,y=c(-.05,-.05,.55,.55),col=alpha("gray",alpha=1),border=F)
lines(x=c(1-predprob$prob[1],1-predprob$prob[1]),y=c(.6,-.1),lwd=1)
text(x=1-predprob$prob[1],y=.9,paste(round(predprob$prob[1],2)*100
                                      ,":",round(1-predprob$prob[1],2)*100),cex=.8)
# 1 vs. 2b
polygon(x=c(-.002,1.002,1.002,-.002),y=c(-4,-4,-3.5,-3.5),border=F
        ,col=alpha("gray",alpha=.3))
text(x=.05,y=-2.9,"Party 1",cex=.9,font=2)
text(x=.95,y=-2.9,"Party 2b",cex=.9,font=2)
for(i in c(0,.25,.5,.75,1)){
  lines(x=c(i,i),y=c(-4,-3.5),lwd=.7,lty=3)
  text(x=i,y=-4.4,paste0(100-i*100,":",i*100),cex=.7)
}
polygon(x=c(1-predprob$CI1[2],1-predprob$CI2[2]
            ,1-predprob$CI2[2],1-predprob$CI1[2])
        ,y=c(-4.05,-4.05,-3.45,-3.45),col=alpha("gray",alpha=1),border=F)
lines(x=c(1-predprob$prob[2],1-predprob$prob[2]),y=c(-4.1,-3.4),lwd=1)
text(x=1-predprob$prob[2],y=-3.1,paste(round(predprob$prob[2],2)*100
                                       ,":",round(1-predprob$prob[2],2)*100),cex=.8)
dev.off()

