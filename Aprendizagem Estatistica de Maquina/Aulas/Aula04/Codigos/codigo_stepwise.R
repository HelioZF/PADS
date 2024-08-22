library(ISLR)
library(MASS)

data(Credit)

fit <- lm(Balance ~ ., data = Credit[,-1])

summary(fit)$coefficients

## forward

stepAIC(fit, direction = "forward", 
        scope = list(lower = ~ 1, 
                     upper = ~ Income + Limit + Rating + Cards + Age + Education + 
                               Gender + Student + Married + Ethnicity))


## backward 

fit <- lm(Balance ~ ., data = Credit[,-1])

stepAIC(fit, direction = "backward")

## both

stepAIC(fit, direction = "both")