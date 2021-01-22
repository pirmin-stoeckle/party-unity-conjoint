library(tidyverse)
library(data.table)

# cleaning directory
rm(list=ls())

# loading analysis script
source("analysis.R")

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
  add_row(nice_names = "PM Party (not in Government)", mean = 0, .before = 16) %>% 
  add_row(nice_names = "INTRA-PARTY CRITIQUE:", .before = 19) %>% 
  add_row(nice_names = "Grass-root Members", mean = 0, .before = 20) %>% 
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

amceplot <- ggplot(pdata_tidy, aes(x = mean, y = fct_rev(fct_inorder(nice_names)))) + #to keep the factor levels as defined in pdata
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.25)+
  geom_vline(xintercept = 0, linetype = 3) +
  xlab("Change: Pr(Vote for the respective candidate)") +
  ylab("")

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/amceplot.pdf"))
amceplot
dev.off()

# alternative AMCE estimation following Hainmueller, Hopkins, Yamamoto, T. (2014)
cjoint::plot.amce(model_cjoint)
# -> looks pretty similar

# scenarios from analysis.R can be used to get probabilities for scenarios compared to baseline

# Effect of ideological distance by unity in party conference behavior
dist_by_conference <- scenarios[role == "government party (not PM party)" &
                         conference %in% c("united", "neither united nor divided", "divided") &
                         parliament == "united" &
                         critique == "grass-root members" &
                         reform == "high" &
                         gender == "female" &
                         age == "38y" &
                         job == "employee" &
                         dist %in% c(0:4),
                         .(conference, dist, lower, prob, upper)]

#plot
distplot_conference <- ggplot(dist_by_conference, aes(x = prob, y = dist)) +
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.25) +
  facet_wrap(vars(conference)) +
  ggtitle("Behavior at party conference") +
  ylab("Ideological distance") +
  xlab("Predicted vote share versus baseline") +
  coord_flip() +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/distplot_conference.pdf"), width = 7, height = 3)
distplot_conference
dev.off()


# Effect of ideological distance by unity in parliamentary voting
dist_by_parliament <- scenarios[role == "government party (not PM party)" &
                                  conference == "united" &
                                  parliament %in% c("united", "divided") &
                                  critique == "grass-root members" &
                                  reform == "high" &
                                  gender == "female" &
                                  age == "38y" &
                                  job == "employee" &
                                  dist %in% c(0:4),]

#plot
distplot_parliament <- ggplot(dist_by_parliament, aes(x = prob, y = dist)) +
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.25) +
  facet_wrap(vars(parliament)) +
  ggtitle("Voting behavior in parliament") +
  ylab("Ideological distance") +
  xlab("Predicted vote share versus baseline") +
  coord_flip() +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/distplot_parliament.pdf"), width = 7, height = 3)
distplot_parliament
dev.off()

# Effect of ideological distance by reform clarity
dist_by_reform <- scenarios[role == "government party (not PM party)" &
                                  conference == "united" &
                                  parliament == "united" &
                                  critique == "grass-root members" &
                                  reform %in% c("low", "high") &
                                  gender == "female" &
                                  age == "38y" &
                                  job == "employee" &
                                  dist %in% c(0:4),]
  

#plot
distplot_reform <- ggplot(dist_by_reform, aes(x = prob, y = dist)) +
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.25) +
  facet_wrap(vars(reform)) +
  ggtitle("Clarity of reform proposals") +
  ylab("Ideological distance") +
  xlab("Predicted vote share versus baseline") +
  coord_flip() +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/distplot_reform.pdf"), width = 7, height = 3)
distplot_reform
dev.off()

# Effect of ideological distance by internal critique
dist_by_critique <- scenarios[role == "government party (not PM party)" &
                              conference == "united" &
                              parliament == "united" &
                              critique %in% c("grass-root members", "former party leader", "party faction", "none") &
                              reform == "high" &
                              gender == "female" &
                              age == "38y" &
                              job == "employee" &
                              dist %in% c(0:4),]


#plot
distplot_critique <- ggplot(dist_by_critique, aes(x = prob, y = dist)) +
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.25) +
  facet_grid(vars(), vars(critique)) +
  ggtitle("Intra-party critique") +
  ylab("Ideological distance") +
  xlab("Predicted vote share versus baseline") +
  coord_flip() +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/distplot_critique.pdf"), width = 7, height = 3)
distplot_critique
dev.off()

###############################################
# simulating specific competitions
###############################################

# function to convert linear predictors to predicted probabilities
compute_probs_one_vs_second <- function(x) {
  prob.first <- exp(x[1])/(sum(exp(x)))
  return(c(prob.first, 1-prob.first))
}

cases1_2a <- rbind(linpreds[role == "government party (not PM party)" &
                          conference == "divided" &
                          parliament == "divided" &
                          critique == "former party leader" &
                          reform == "low" &
                          gender == "male" &
                          age == "56y" &
                          job == "lawyer" &
                          dist == 0
                        ,c("mean", "upper", "lower")],
               linpreds[role == "opposition party" &
                          conference == "divided" &
                          parliament == "divided" &
                          critique == "former party leader" &
                          reform == "low" &
                          gender == "female" &
                          age == "38y" &
                          job == "employee" &
                          dist == 2
                        ,c("mean", "upper", "lower")]
)

cases1_2b <- rbind(linpreds[role == "government party (not PM party)" &
                              conference == "divided" &
                              parliament == "divided" &
                              critique == "former party leader" &
                              reform == "low" &
                              gender == "male" &
                              age == "56y" &
                              job == "lawyer" &
                              dist == 0
                            ,c("mean", "upper", "lower")],
                   linpreds[role == "opposition party" &
                              conference == "united" &
                              parliament == "united" &
                              critique == "grass-root members" &
                              reform == "high" &
                              gender == "female" &
                              age == "38y" &
                              job == "employee" &
                              dist == 2
                            ,c("mean", "upper", "lower")]
)

predprob1_2a <- as.data.frame(apply(cases1_2a,2,compute_probs_one_vs_second))
colnames(predprob1_2a) <- c("prob","CI1","CI2")

predprob1_2b <- as.data.frame(apply(cases1_2b,2,compute_probs_one_vs_second))
colnames(predprob1_2b) <- c("prob","CI1","CI2")

predprob <- rbind(predprob1_2a[1,], predprob1_2b[1,])


# plot
competitionplot <- ggplot(predprob, aes(x = fct_rev(fct_inorder(c("Party 1 vs Party 2a",
                                  "Party 1 vs Party 2b"))), y = prob)) +
  geom_pointrange(aes(ymin = CI1, ymax = CI2), size = 0.25) +
  ylim(c(0, 1)) +
  ylab("Expected Probability to Vote for Party 1") +
  xlab("") +
  geom_hline(yintercept = 0.5, linetype = 3, col = "blue") +
  coord_flip() +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/competitionplot.pdf"), width = 7, height = 3)
competitionplot
dev.off()