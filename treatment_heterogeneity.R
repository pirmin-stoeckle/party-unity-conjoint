# load data from main analysis:
source("analysis.R")

# get data from core questionnaire w43
load("//sfb884-share.ad.uni-mannheim.de/data$/2_Data/data_for_R_users/GIP_W43_V4.Rdata")
names(GIP_W43_V4)

# party identification
table(GIP_W43_V4$AA43042, useNA = "always")
table(GIP_W43_V4$AA43043, useNA = "always")

GIP_W43_V4 <- GIP_W43_V4 %>% 
  mutate(pid = case_when(
    AA43042 == "2. Nein" ~ "None", 
    AA43043 == "1. CDU / CSU" ~ "CDU/CSU",
    AA43043 == "2. SPD" ~ "SPD",
    AA43043 == "3. FDP" ~ "FDP",
    AA43043 == "4. BÃ¼ndnis 90 / Die GrÃ¼nen" ~ "Gruene",
    AA43043 == "5. Die Linke" ~ "Linke",
    AA43043 == "8. AfD" ~ "AfD",
    AA43043 %in% c("6. NPD", "7. Andere Partei") ~ "Other"
  ) %>% 
    fct_relevel("CDU/CSU",
                "SPD",
                "Gruene",
                "FDP",
                "Linke",
                "AfD",
                "Other",
                "None")
  )

table(GIP_W43_V4$pid, useNA = "always")

# vote at last election
table(GIP_W43_V4$AA43038, useNA = "always")

GIP_W43_V4 <- GIP_W43_V4 %>% 
  mutate(last_vote = fct_collapse(AA43038,
                                  "CDU/CSU" = "3. CDU / CSU",
                                  "SPD" = "4. SPD",
                                  "FDP" = "5. FDP",
                                  "Gruene" = "6. BÃ¼ndnis 90 / Die GrÃ¼nen",
                                  "Linke" = "7. Die Linke",
                                  "AfD" = "11. AfD",
                                  "Other" = c("8. NPD", 
                                              "9. Piratenpartei", 
                                              "10. Andere Partei, und zwar (bitte eintragen):"),
                                  "Did not vote" = c("1. Habe nicht gewÃ¤hlt",
                                                     "2. Nicht wahlberechtigt (nicht volljÃ¤hrig/keine deutsche StaatsbÃ¼rgerschaft)"),
                                  other_level = "Don't know / refuse to say"
                                  ) %>% 
           fct_relevel("CDU/CSU",
                       "SPD",
                       "Gruene",
                       "FDP",
                       "Linke",
                       "AfD",
                       "Other",
                       "Did not vote",
                       "Don't know / refuse to say")
  )
           


table(GIP_W43_V4$last_vote, useNA = "always")

core <- GIP_W43_V4 %>% 
  select(id_g, pid, last_vote)

dat_join <- dat %>% left_join(core, by = "id_g")

table(dat_join$pid,dat_join$last_vote, useNA = "always")
# compare to original data
table(GIP_W43_V4$AA43043, GIP_W43_V4$AA43038, useNA = "always")

# model by partisanship
# create function for model estimation on data subset

estimate_model <- function(data) {
  model <- clogit(chosen ~ dist +
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
                  data=data, method="efron", robust=TRUE)
}

compute_pdata <- function(model, label = NA) {
  #(get data for) AMCEs plot
  coefs <- coef(model) #get coefficients
  ses <- summary(model)$coefficients[,4] #get ROBUST res
  names <- rownames(summary(model)$coefficients) #get names of coefs
  pdata <- data.table(mean=(1/(1+exp(-coefs)))-.5, #compute probability difference to all 0's scenario
                      lower=(1/(1+exp(-(coefs-(1.96*ses)))))-.5, #compute corresponding CI
                      upper=(1/(1+exp(-(coefs+(1.96*ses)))))-.5, #compute corresponding CI
                      names=names,
                      label = label
  )
  return(pdata)
}



# party identification

dat_join %>% group_by(pid) %>% summarize(n_respondents = length(unique(id_g)))

pdata_pid <- rbind(
  compute_pdata(model = estimate_model(dat_join[pid == "CDU/CSU"]),
                label = "CDU/CSU"),
  compute_pdata(model = estimate_model(dat_join[pid == "SPD"]),
                label = "SPD"),
  compute_pdata(model = estimate_model(dat_join[pid == "Gruene"]),
                label = "Gruene"),
  compute_pdata(model = estimate_model(dat_join[pid == "FDP"]),
                label = "FDP"),
  compute_pdata(model = estimate_model(dat_join[pid == "Linke"]),
                label = "Linke"),
  compute_pdata(model = estimate_model(dat_join[pid == "AfD"]),
                label = "AfD"),
  compute_pdata(model = estimate_model(dat_join[pid == "Other"]),
                label = "Other"),
  compute_pdata(model = estimate_model(dat_join[pid == "None"]),
                label = "None"))

ggplot(pdata_pid, 
       aes(x = mean, 
           y = fct_rev(fct_inorder(names)), 
           color = fct_rev(fct_inorder(label)))) +
  geom_pointrange(aes(xmin = lower, xmax = upper), 
                  size = 0.25,
                  alpha = 0.5,
                  position = position_dodge(width = 0.75))+
  geom_vline(xintercept = 0, linetype = 3) +
  labs(
    title = "AMCE by party identification",
    x = "Change: Pr(Vote for the respective candidate)",
    y = ""
  ) +
  scale_color_manual(breaks = c("CDU/CSU", 
                                "SPD", 
                                "Gruene",
                                "FDP",
                                "Linke",
                                "AfD",
                                "Other",
                                "None"),
                     values=c("black", 
                              "red", 
                              "green3",
                              "yellow2",
                              "magenta4",
                              "darkblue",
                              "grey50",
                              "grey70")) +
  theme_minimal() +
  theme(legend.title = element_blank())


# last vote choice
dat_join %>% 
  group_by(last_vote) %>% 
  summarize(n_respondents = length(unique(id_g)))

pdata_last_vote <- rbind(
  compute_pdata(model = estimate_model(dat_join[last_vote == "CDU/CSU"]),
                label = "CDU/CSU"),
  compute_pdata(model = estimate_model(dat_join[last_vote == "SPD"]),
                label = "SPD"),
  compute_pdata(model = estimate_model(dat_join[last_vote == "Gruene"]),
                label = "Gruene"),
  compute_pdata(model = estimate_model(dat_join[last_vote == "FDP"]),
                label = "FDP"),
  compute_pdata(model = estimate_model(dat_join[last_vote == "Linke"]),
                label = "Linke"),
  compute_pdata(model = estimate_model(dat_join[last_vote == "AfD"]),
                label = "AfD"),
  compute_pdata(model = estimate_model(dat_join[last_vote == "Other"]),
                label = "Other"),
  compute_pdata(model = estimate_model(dat_join[last_vote == "Did not vote"]),
                label = "Did not vote"),
  compute_pdata(model = estimate_model(dat_join[last_vote == "Don't know / refuse to say"]),
                label = "Don't know / refuse to say"))

amceplot_last_vote <- ggplot(pdata_last_vote, 
       aes(x = mean, 
           y = fct_rev(fct_inorder(names)), 
           color = fct_rev(fct_inorder(label)))) +
  geom_pointrange(aes(xmin = lower, xmax = upper), 
                  size = 0.25,
                  alpha = 0.5,
                  position = position_dodge(width = 0.75))+
  geom_vline(xintercept = 0, linetype = 3) +
  labs(
    title = "AMCE by last vote choice",
    x = "Change: Pr(Vote for the respective candidate)",
    y = ""
  ) +
  scale_color_manual(breaks = c("CDU/CSU", 
                                "SPD", 
                                "Gruene",
                                "FDP",
                                "Linke",
                                "AfD",
                                "Other",
                                "Did not vote",
                                "Don't know / refuse to say"),
                     values=c("black", 
                              "red", 
                              "green3",
                              "yellow2",
                              "magenta4",
                              "darkblue",
                              "grey50",
                              "grey70",
                              "grey90")) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave("./figures/amceplot_by_last_vote.pdf")

pdf(file="./figures/amceplot_by_last_vote.pdf")
amceplot_last_vote
dev.off()

# by gender
pdata_gender <- rbind(
  compute_pdata(model = estimate_model(dat_join[respondent_gender == 1]),
                label = "male"),
  compute_pdata(model = estimate_model(dat_join[respondent_gender == 2]),
                label = "female"))

amceplot_gender <- ggplot(pdata_gender, 
                             aes(x = mean, 
                                 y = fct_rev(fct_inorder(names)), 
                                 color = label)) +
  geom_pointrange(aes(xmin = lower, xmax = upper), 
                  size = 0.25,
                  alpha = 0.5,
                  position = position_dodge(width = 0.75))+
  geom_vline(xintercept = 0, linetype = 3) +
  labs(
    title = "AMCE by gender",
    x = "Change: Pr(Vote for the respective candidate)",
    y = ""
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

pdf(file="./figures/amceplot_by_gender.pdf")
amceplot_gender
dev.off()
