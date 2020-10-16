cov(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Width[iris$Species=="setosa"])

cor(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Width[iris$Species=="setosa"])

iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

mtcars %>% ggpairs()
corr_tmp <- cor(mtcars)
corr_tmp %>% ggcorrplot(method = "circle", hc.order = TRUE, type="lower", outline.col="white")

kc_tax <- read_csv("kc_tax.csv")
knitr::kable(kc_tax[1:5,])
kc_tax0 <- subset(kc_tax, TaxAssessedValue <750000 & SqFtTotLiving > 100 & SqFtTotLiving < 3500)
nrow(kc_tax0)

ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() +
  geom_point(alpha=0.1) +
  geom_density2d(color='white') +
  labs(x='Finished Square Feet', y='Tax-Assessed Value')

ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) +
  stat_binhex(color='white') + theme_bw() +
  scale_fill_gradient(low='white', high='black') +
  labs(x='Finished Square Feet', y='Tax-Assessed Value')

lc_loans <- read_csv("lc_loans.csv")
head(lc_loans)

unique(lc_loans$grade)

CrossTable(lc_loans$grade, lc_loans$status, prop.c = FALSE, prop.chisq=FALSE, prop.t=FALSE)

diamonds %>% ggplot(aes(x=cut, y=price)) +
  geom_boxplot()
diamonds %>% ggplot(aes(x=cut, y=price)) +
  geom_violin()

ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x=SqFtTotLiving, y=TaxAssessedValue)) +
  stat_binhex(color='white') +
  theme_bw() +
  scale_fill_gradient(low='white', high='blue') +
  labs(x='Finished Square Feet', y='Tax-Assessed Value') +
  facet_wrap('ZipCode')


(dist_data <- flights %>% select (distance) %>% filter(!is.na(distance))) 

samp_mean_05 <- rep(0, 1000)
for( i in 1:1000 ){
  samp_mean_05[i] <- mean( sample( dist_data$distance, 5 ) ) }

tmp <- tibble(samp_mean_05)

ggplot(tmp, aes(samp_mean_05)) +
  geom_histogram(bins = 15)

hist(samp_mean_05, breaks = 15)

samp_mean_25 <- rep(0, 1000)
for( i in 1:1000 ){
  samp_mean_25[i] <- mean( sample( dist_data$distance, 25 ) )
}

hist( samp_mean_25, breaks=15 )


hist(samp_mean_25, breaks = 15)

samp_mean_64 <- rep(0, 1000)
for( i in 1:1000 ){
  samp_mean_64[i] <- mean( sample( dist_data$distance, 64 ) )
}

tmp <- tibble(samp_mean_64)

ggplot(tmp, aes(samp_mean_64)) +
  geom_histogram(bins = 15)

hist(samp_mean_64, breaks = 15)

single_sample_64 <- sample(dist_data$distance, 64)
(SE <- sd(single_sample_64) / sqrt(length(single_sample_64)))

binom_sample <- rbinom(100,2500,1/6)
qqnorm(binom_sample)
