wages <- read.csv("wages.csv")
wages <- wages[!is.na(wages$hgc) & !is.na(wages$tenure), ]

model1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
             data = wages, na.action = na.omit)

wages$logwage_mean <- wages$logwage
wages$logwage_mean[is.na(wages$logwage)] <- mean(wages$logwage, na.rm = TRUE)
model2 <- lm(logwage_mean ~ hgc + college + tenure + I(tenure^2) + age + married, 
             data = wages)

wages$logwage_pred <- wages$logwage
predicted <- predict(model1, newdata = wages[is.na(wages$logwage), ])
wages$logwage_pred[is.na(wages$logwage)] <- predicted
model3 <- lm(logwage_pred ~ hgc + college + tenure + I(tenure^2) + age + married, 
             data = wages)

library(stargazer)
stargazer(wages, type="latex", summary=TRUE)
stargazer(model1, model2, model3, type="latex")
