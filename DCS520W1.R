(state <- read_csv("state.csv", col_names = TRUE))
kable( state[1:5,], cpation = "murder Rate by States (per 100,000)")

mean(state$Population)         
mean(state[['Population']])  
  
mean(state$Murder.Rate)
weighted.mean(state$Murder.Rate, w=state$Population)

median(state$Murder.Rate)
weightedMedian(state$Murder.Rate, w=state$Population)

state %>% ggplot() +
  geom_histogram(aes(Murder.Rate), bins = 15)

state %>%
  arrange(desc(Murder.Rate))

quantile(state$Murder.Rate, c(0.1, 0.25, 0.5, 0.98))

mean(state$Murder.Rate, trim=2/50)

head (iris)

iris %>% ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 15, fill="red", alpha=0.3)

iris %>% ggplot(aes(x=Sepal.Length)) +
  geom_histogram(bins = 15, fill="red", alpha = 0.3) +
  facet_wrap(~ Species)


(m_s.l <- mean(iris$Sepal.Length))

iris %>% select (Sepal.Length) %>%
  mutate(deviation = Sepal.Length - m_s.l) %>%
  summarise(sum = sum(deviation))

iris %>% select (Sepal.Length) %>%
  mutate(deviation = (Sepal.Length - m_s.l) ^2) %>%
  summarise(sumd = sum(deviation), varsl = sumd/(n()-1), stdsl = varsl^(1/2))  

var( iris$Sepal.Length)  

var_pop(iris$Sepal.Length)

sd(iris$Sepal.Length)

sd_pop(iris$Sepal.Length)

mad(iris$Sepal.Length)

IQR(iris$Sepal.Length)

fivenum(iris$Sepal.Length)

boxplot(iris$Sepal.Length, horizontal = TRUE)

iris%>% ggplot() +
  geom_boxplot(aes(x=Sepal.Length, y=Species))

state <- read_csv("state.csv", col_names=TRUE)
boxplot(state$Population, horizontal = TRUE)

state %>% ggplot() +
  geom_boxplot(aes(x=Population))

breaks <- seq(from=min(state$Population),
              to=max(state$Population), length=11)
pop_freq <- cut(state$Population, breaks = breaks,
                right=TRUE, include.lowest = TRUE)
table(pop_freq)

hist(state$Population, breaks=breaks)

state %>% ggplot(aes(x=Population)) +
  geom_histogram(fill="red", alpha=0.3, breaks=breaks)

state %>% ggplot(aes(x=Population)) +
  geom_histogram(aes(y=..density..), fill="red", alpha=0.3, bins = 15) +
  geom_density(color="blue", size=2)

ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut))

tmp <- diamonds %>% group_by(cut) %>% summarize(n=n())
pie(tmp$n)

ggplot(data=diamonds) +
  geom_bar(aes(x=cut, fill=cut)) +
  coord_polar()
