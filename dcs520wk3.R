(dist_data <- flights %>% select(distance) %>%
  filter(!is.na(distance)))

set.seed(25); samp_mean_25 <- rep(0,10000)
for(i in 1:10000) {
  samp_mean_25[i] <- mean(sample(dist_data$distance, 25, replace = TRUE))
}; hist(samp_mean_25, breaks=15); sd(samp_mean_25)

set.seed(250); one_sample <- sample(dist_data$distance, 25, replace = TRUE)
samp_mean_250 <- rep(0,10000)
for(i in 1:10000)  {
  samp_mean_250[i]<- mean(sample(one_sample,25,replace = TRUE))
}; hist(samp_mean_250, breaks = 15); sd(samp_mean_250)

samplemean <- function(x,d) {
  return(mean(x[d]))
}
(boot_obj <- boot(one_sample, samplemean, R = 10000))

mean(one_sample); sd(one_sample)

(CI_1 <- c(mean(one_sample) -2* sd(one_sample)/sqrt(25) ,
           mean(one_sample) +2* sd(one_sample/sqrt(25))))

(CI_2 <- c(quantile(samp_mean_25, 0.025), quantile(samp_mean_25, 0.975)))  

(CI_2 <- c(quantile(samp_mean_250, 0.025), quantile(samp_mean_250,0.975)))  

x<- (-400):400
x<- x/100
plot(x, dt(x,df=5))

pt(2,df=5) - pt(-2, df=5)
qt(.999, df=5)
rt(10, df=5)

x <- ((-400):400)/100
data_1 <- tibble(x, y=dt(x, df=5))
ggplot(data=data_1, aes(x,y)) +
  geom_line(color="red", size=2)

pt(0, df=5)
ggplot(data=data_1, aes(x,y)) + geom_line(color="blue", size=2) +
  geom_area(aes(y=ifelse(x>=-4 & x<=-1,y,0)), fill = "red")

qt(.1816, df=5)
ggplot(data = data_1, aes(x,y) ) + geom_line(color="blue", size=2) +
  geom_area(aes(y=ifelse(x>=-4 & x<=-1, y, 0)), fill="red")

rt(10, df=5)
data_2 <- tibble(x=rt(1000,df=5))
qqnorm(data_2$x)
qqline(data_2$x, col="blue", lwd=3)

data_2 %>% ggplot(aes(sample=x)) +
  geom_qq() +
  geom_qq_line(color = "blue", size = 1)

params <- list(df=5)
data_2 %>%ggplot(aes(sample=x)) + 
  geom_qq(distribution=qt, dparams = params$df) +
  geom_qq_line(distribution = qt, dparams=params$df, color="blue", size =1)

(params <- as.list(fitdistr(data_2$x, "t")$estimate))

data_2 %>%ggplot(aes(sample=x)) + 
  geom_qq(distribution=qt, dparams = params$df) +
  geom_qq_line(distribution = qt, dparams=params$df, color="blue", size =1)

dbinom(x=2, size = 10, prob=1/6)

pbinom(q=2, size=10, prob = 1/6)
dbinom(0,10,1/6) + dbinom(1,10,1/6) + dbinom(2,10,1/6)

rbinom(n=4, size = 10, prob = 1/6)

x<- (0:1000)/100
data_3 <- tibble(x,y=dchisq(x,df=3))
ggplot(data=data_3, aes(x,y)) +
  geom_line(color="red", size=2)

pchisq(3, df=3)
ggplot(data=data_3, aes(x,y)) +
  geom_line(color="blue", size=2) +
  geom_area(aes(y=ifelse(x>=0 & x<=3, y, 0)), fill = "red")

qchisq(0.6083748, df=3)
rchisq(10, df=5)

set.seed(27101)
rexp(3, rate = 0.5)

set.seed(20201022)
rweibull(3, shape = 4, scale = 50)

x <- c(58301, 66630, 54219, 49711, 52027, 60799,
       59676, 72632, 72386, 53650, 73191, 56491,
       63484, 77683, 75872, 75266, 56473, 66917,
       68684, 44387, 59730, 54438, 65575, 49751,
       41926, 72681, 64154, 68418, 48846, 67912)

t.test( x, alternative="greater", mu=52000 )
t.test( x, alternative="two.sided", mu=52000 )
