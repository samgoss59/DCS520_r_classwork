house1 <- read.csv('house_sales.csv', sep='\t')
house1 <- tibble(house1)
house1 <- house1 %>% mutate(Date = as.Date(Date), ym = as.Date(ym),
                                           PropertyType = as.factor(PropertyType),
                                           ZipCode = as.factor(ZipCode))
house1_lm <- lm(AdjSalePrice ~SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms +
                  BldgGrade + PropertyType, data = house1)                            
summary(house1_lm)
unique(house1$PropertyType)
house1_lm$coefficients
min(house1$AdjSalePrice)
max(house1$AdjSalePrice)
prop_type_dummies1 <- model.matrix(~PropertyType, data = house1)
prop_type_dummies2 <- model.matrix(~PropertyType -1, data = house1)
unique(house1$ZipCode)
table(house1$ZipCode)
zip_groups <- house1 %>% mutate(resid = residuals(house1_lm)) %>%
  group_by(ZipCode) %>% summarize(med_resid = median(resid), cnt= n()) %>%
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt), ZipGroup = ntile(cum_cnt, 5))
house2 <- house1 %>%
  left_join(select(zip_groups, ZipCode, ZipGroup), by='ZipCode')
head(house2)

house1_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                    Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                    SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                  data=house1, na.action=na.omit)
step_lm <- stepAIC(house1_full, direction = "both")
summary(step_lm)

update(step_lm, .~. - SqFtTotLiving - SqFtFinBasement - Bathrooms)

house2_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms +
                  BldgGrade + PropertyType + ZipGroup, data=house2, na.action=na.omit)
summary(house2_lm)

house2_lm_interac <- lm(AdjSalePrice ~ SqFtTotLiving * ZipGroup + SqFtLot + Bathrooms
                        + Bedrooms + BldgGrade + PropertyType, data = house2,
                        na.action = na.omit)
summary(house2_lm_interac)

power.t.test(delta = 3, sd=6, sig.level = .05, power=.9, type = c("one.sample"),
             alternative = "one.sided")

house_98105 <- house1[house1$ZipCode == "98105",]
lm_98105 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade,
               data=house_98105)
summary(lm_98105)

sresid <- rstandard(lm_98105)
plot(sresid)
sresid[abs(sresid)>3]
plot(lm_98105, 1)
plot(lm_98105, 2)
plot(lm_98105, 3)
plot(lm_98105, 4)
plot(lm_98105, 5)
plot(lm_98105,6)

sresid <- rstandard(lm_98105)
cooks_D <- cooks.distance(lm_98105)
ht_v <- hatvalues(lm_98105)

tibble(fitted=lm_98105$fitted.values, sres=sresid) %>%
  ggplot(aes(fitted,sres)) + geom_point(color = "blue") +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")

lm_98105_sq <- lm(AdjSalePrice ~ poly(SqFtTotLiving,2)+ SqFtLot +
                    Bathrooms + Bedrooms + BldgGrade, data = house_98105)
summary(lm_98105_sq)
AIC(lm_98105, lm_98105_sq, lm_98105_spline, lm_98015_gam)
BIC(lm_98105, lm_98105_sq, lm_98105_spline, lm_98015_gam)

knots <- quantile(house_98105$SqFtTotLiving, p=c(.25,.5,.75))
lm_98105_spline <- lm(AdjSalePrice ~ bs(SqFtTotLiving, knots = knots, degree = 3)
                      +SqFtLot + Bathrooms + Bedrooms + BldgGrade, data = house_98105)
summary(lm_98105_spline)

lm_98015_gam <- gam(AdjSalePrice ~s(SqFtTotLiving) + SqFtLot + Bathrooms + Bedrooms
                    +BldgGrade, data= house_98105)
summary(lm_98015_gam)

loan_data <- read.csv('loan_data.csv')
loan_data <- tibble(loan_data)
loan_data <- loan_data %>% mutate(outcome = as.factor(outcome), 
                                  purpose_ = as.factor(purpose_),
                                  home_ = as.factor(home_),
                                  emp_len_ = as.factor(emp_len_))
naive_model <- NaiveBayes(outcome ~ purpose_ +home_ + emp_len_,
                          data=na.omit(loan_data))
naive_model

new_loan <- loan_data[147, c('purpose_', 'home_', 'emp_len_')]
row.names(new_loan) <- NULL
predict(naive_model, new_loan)
