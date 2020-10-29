x_2 <- c(58899, 58523, 69163, 51585, 65461, 49414, 
         54244, 64756, 37196, 64313, 61720, 52986, 
         48764, 55008, 41069, 49605, 48639, 51568,
         49855, 64049, 57960, 57046, 45702, 68603,
         55967, 59273, 65077, 55281, 39603, 67960)
(our_s_m<- mean(x_2))

nul_m <- 52000

bootstrap_samples <- rep(0,10000)
for(i in 1:10000){
  bootstrap_samples[i] <- mean(sample(x_2 - our_s_m, length(x_2), replace=TRUE))
}

hist(bootstrap_samples + nul_m, main="Sampling Dist by BootStrappin")
abline(v=our_s_m, col="red", lty=2, lwd=3)

(p_value <- mean(bootstrap_samples + nul_m > our_s_m))

t.test(x_2, alternative = "greater", mu=52000)

session_time<- read.csv("session_times.csv")
session_time%>% ggplot(aes(x=Time, y=Page)) +
  geom_boxplot()

(avg_T_AB <- session_time %>% group_by(Page) %>%
    summarize(avg_T =mean(Time)))

avg_T_AB$avg_T[avg_T_AB$Page=="Page B"] - avg_T_AB$avg_T[avg_T_AB$Page=="Page A"]

t.test(session_time$Time[session_time$Page=="Page B"],
       session_time$Time[session_time$Page=="Page A"],
       alternative = "greater")
choose(36,21)
choose(25,10)
choose(10,6)
(mean_b <- avg_T_AB$avg_T[avg_T_AB$Page=="Page B"])
(mean_a <- avg_T_AB$avg_T[avg_T_AB$Page=="Page A"])
mean_diff_per_permute <- rep(0, 10000)

perm_fun <- function(x, nA, nB)
{
  n <- nA +nB
  idx_b <- sample(1:n, nB, replace = FALSE)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff_per_permute <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff_per_permute)
}

for(i in 1:10000) {
  mean_diff_per_permute[i] = perm_fun(session_time$Time, 21,15)
}
hist(mean_diff_per_permute, xlab = 'Session time differeces (in  seconds)' ,
    main="permutation Dists. of difference of Means between Page B & A")
abline(v=mean_b - mean_a, col="red", lty=2, lwd=3)

(p_value <- mean(mean_diff_per_permute >= mean_b - mean_a))

obs_pct_diff <- 100 * (200/23739 - 182/22588)
conversion <- c(rep(0,45945), rep(1 , 382))


perm_diffs <- rep(0, 1000)
for(i in 1:1000) {
  perm_diffs[i] = 100 * perm_fun(conversion, 23739, 22588)
}

hist(perm_diffs, xlab='Conversion rate (percent)', 
     main='Permutation Dist. of Difference of Mean Conversion Rates')
abline(v=obs_pct_diff, col="red", lty=2, lwd=3)

(p_value <- mean(perm_diffs > obs_pct_diff))

prop.test(x=c(200,182), n=c(23739, 22588), alternative = "greater")


t.test(session_time$Time[session_time$Page =="Page B"],
       session_time$Time[session_time$Page == "Page A"],
       conf.level = 0.95)
