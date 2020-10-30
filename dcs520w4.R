four_sessions <- read_csv("four_sessions.csv")

four_sessions %>% ggplot(aes(x=Page, y=Time)) +
  geom_boxplot() +
  labs(y='Time(in second)') 
choose(20,5)
aovp_output <- aovp(Time ~ Page, data=four_sessions)
summary(aovp_output)

aov_output <- aov(Time ~ Page, data = four_sessions)
summary(aov_output)

click_rate <- read_csv("click_rates.csv")

clicks <- matrix(click_rate$Rate, nrow = 3, ncol = 2, byrow=TRUE)
dimnames(clicks) <- list(unique(click_rate$Headline),
                         unique(click_rate$Click))
clicks

chisq.test(clicks, simulate.p.value = TRUE)

chisq.test(clicks, simulate.p.value = FALSE)

fisher.test(clicks)
