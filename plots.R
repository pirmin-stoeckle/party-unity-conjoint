#pdata can be used for plotting
amceplot <- ggplot(pdata, aes(x = mean, y = fct_rev(fct_inorder(names)))) + #to keep the factor levels as defined in pdata
  geom_pointrange(aes(xmin = lower, xmax = upper))+
  geom_vline(xintercept = 0, linetype = 3) +
  xlab("AMCE") +
  ylab("") +
  theme_bw()

#save plot to pdf
pdf(file=paste0(getwd(),"/figures/amceplot.pdf"))
amceplot
dev.off()


# scenarios can be used to get probabilities for scenarios compared to baseline
strategy1 <- scenarios[role == "government party (not PM party)" &
                         conference == "united" &
                         parliament == "united" &
                         critique == "grass-root members" &
                         reform == "low" &
                         gender == "female" &
                         age == "38y" &
                         job == "employee" &
                         respondent_educ == "no higher educ" &
                         dist == "0",
                       .(lower, prob, upper)]

strategy2 <- scenarios[role == "government party (not PM party)" &
                         conference == "united" &
                         parliament == "united" &
                         critique == "grass-root members" &
                         reform == "low" &
                         gender == "female" &
                         age == "38y" &
                         job == "employee" &
                         respondent_educ == "no higher educ" &
                         dist == "1",
                       .(lower, prob, upper)]

strategy3 <- scenarios[role == "government party (not PM party)" &
                         conference == "united" &
                         parliament == "united" &
                         critique == "grass-root members" &
                         reform == "low" &
                         gender == "female" &
                         age == "38y" &
                         job == "employee" &
                         respondent_educ == "no higher educ" &
                         dist == "2",
                       .(lower, prob, upper)]

strategy4 <- scenarios[role == "government party (not PM party)" &
                         conference == "united" &
                         parliament == "united" &
                         critique == "grass-root members" &
                         reform == "low" &
                         gender == "female" &
                         age == "38y" &
                         job == "employee" &
                         respondent_educ == "no higher educ" &
                         dist == "3",
                       .(lower, prob, upper)]

strategy5 <- scenarios[role == "government party (not PM party)" &
                         conference == "united" &
                         parliament == "united" &
                         critique == "grass-root members" &
                         reform == "low" &
                         gender == "female" &
                         age == "38y" &
                         job == "employee" &
                         respondent_educ == "no higher educ" &
                         dist == "4",
                       .(lower, prob, upper)]

#put in data table for plotting
pstrategies <- as.data.table(rbind(strategy1, strategy2, strategy3, strategy4, strategy5))

#plot
ggplot(pstrategies, aes(x = prob, y = rownames(pstrategies))) +
  geom_pointrange(aes(xmin = lower, xmax = upper)) +
  geom_vline(xintercept = 50, color = "red", linetype = 3) +
  ylab("Strategy") +
  xlab("Predicted vote share versus baseline") +
  coord_flip() +
  theme_bw()
