effect_size = ES.h(p1=0.0121, p2=0.011)
pwr.2p.test(h=effect_size, sig.level = 0.05, power = 0.8, alternative = "greater")

pwr.t.test(d=0.2, n=60, sig.level = 0.10, type = "one.sample", alternative = "two.sided")
pwr.t.test(d=0.2, sig.level = 0.10, power = 0.8, type = "one.sample", alternative = "two.sided")
d<-2/2.8
pwr.t.test(d=d, n=30, sig.level=0.05,
           type="two.sample", alternative="two.sided")

pwr.t.test(d=d, sig.level = 0.05, power = 0.85,
           type = "two.sample", alternative = "two.sided")
pwr.t2n.test(d=0.6, n1=90,n2=60, alternative = "greater")


lung <- read_csv("LungDisease.csv")
max(lung$Exposure)
min(lung$Exposure)
lung %>% ggplot(aes(Exposure, PEFR)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se=F)
model1 <- lm(PEFR~Exposure, data = lung)
summary(model1)
(ml_fitted <- predict(model1))
(ml_resid <- round(residuals(model1), digits = 2))
sum(ml_resid)

house <- read_delim('house_sales.csv',delim = '\t')
head(house     )

house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + 
                 BldgGrade, data=house, na.action = na.omit)
summary(house_lm)
min(house$SalePrice)
min(house$SqFtTotLiving)
max(house$SqFtTotLiving)

house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                   Bedrooms + BldgGrade + PropertyType + NbrLivingUnits +
                   SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                 data=house, na.action=na.omit)
summary(house_full)

step_output <- stepAIC(house_full, direction = "both"); step_output

new_data <- tibble(Exposure=18)
predict(model1, newdata = new_data, interval = "confidence", level = 0.93)
predict(model1, newdata = new_data, interval = "prediction", level = 0.93)
