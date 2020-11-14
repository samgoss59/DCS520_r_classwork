mpg$class <- factor(mpg$class, levels = c("2seater", "subcompact", "compact",
                                          "midsize","minivan", "suv", "pickup"))
mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(method = "lm", color = "red", se=FALSE) +
  labs(x="Engine Displacement in Litres", y="Highway Miles per Gallon",
       title = "Engine Displacement vs Highway MPG")
View(mpg     )

model.mpg <- lm(hwy ~ displ, data = mpg)
summary(model.mpg)

35.6977 +(-3.5307*2.4)
resid_mpg <- resid(model.mpg)
sum(resid_mpg^2)

new_data <- tibble(displ = 2.4)
predict(model.mpg, newdata = new_data, interval = "confidence", level = 0.93)
predict(model.mpg, newdata = new_data, interval = "prediction", level = 0.93)

write.csv(mpg, "mpg.csv")


model_mpg2 <- lm(hwy ~ displ + drv + cyl + fl + class + manufacturer,
                 data=mpg, na.action = na.omit)
summary(model_mpg2)

step_output_mpg <- stepAIC(model_mpg2, direction = "both"); step_output_mpg

new_data2 <- tibble( manufacturer="audi", displ=3.5, cyl=6, drv="4", fl="p", class="midsize" )
predict(model_mpg2, newdata = new_data2, interval = "confidence", level = 0.95)
predict(model_mpg2, newdata = new_data2, interval = "prediction", level = 0.95)

