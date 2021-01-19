library(tidyverse)
library(data.table)

#pdata from analysis.R can be used for plotting

#insert nice names for graph and extra rows for baseline categories and labels
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
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.15)+
  geom_vline(xintercept = 0, linetype = 3) +
  xlab("AMCE") +
  ylab("") +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/amceplot.pdf"))
amceplot
dev.off()


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
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.1) +
  facet_wrap(vars(conference)) +
  ggtitle("Behavior at party conference") +
  ylab("Ideological distance") +
  xlab("Predicted vote share versus baseline") +
  coord_flip() +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/distplot_conference.pdf"), width = 12, height = 5)
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
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.1) +
  facet_wrap(vars(parliament)) +
  ggtitle("Voting behavior in parliament") +
  ylab("Ideological distance") +
  xlab("Predicted vote share versus baseline") +
  coord_flip() +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/distplot_parliament.pdf"), width = 8, height = 5)
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
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.1) +
  facet_wrap(vars(reform)) +
  ggtitle("Clarity of reform proposals") +
  ylab("Ideological distance") +
  xlab("Predicted vote share versus baseline") +
  coord_flip() +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/distplot_reform.pdf"), width = 8, height = 5)
distplot_reform
dev.off()
