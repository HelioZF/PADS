library(MASS)
library(tidyverse)
library(rsample)
library(rpart)
library(rpart.plot)
library(partykit)
library(skimr)


skim(Boston)


# treinamento e teste -----------------------------------------------------

set.seed(123)

splits <- initial_split(Boston, prop = 0.8)

tr <- training(splits)
test <- testing(splits)


# modelo linear -----------------------------------------------------------

ml <- lm(medv ~ ., data = tr)

summary(ml)

predito_lm <- predict(ml, test)


# árvore ------------------------------------------------------------------

arvore_1 <- rpart(medv ~ ., tr, control = rpart.control(cp = 0.1))

rpart.plot(arvore_1, roundint = FALSE)

predito_ar_1 <- predict(arvore_1, test)





# árvore com poda ---------------------------------------------------------

arvore_2 <- rpart(medv ~ ., tr, control = rpart.control(cp = 0.001))

arvore_2 <- prune(arvore_2, cp = 0.001)

rpart.plot(arvore_2)

predito_ar_2 <- predict(arvore_2, test)


# resultados --------------------------------------------------------------

resultados <- tibble(predito = c(predito_lm, predito_ar_1, predito_ar_2), 
                     observado = rep(test$medv, 3), 
                     metodo = c(rep("modelo linear", nrow(test)), 
                                rep("árvore 1", nrow(test)), 
                                rep("árvore 2", nrow(test)))) 


resultados %>% 
  ggplot(aes(observado, predito)) + 
    geom_abline(intercept = 0, slope = 1, color = "red", lty = 2, size = 1) + 
    geom_point() +
    facet_wrap(~ metodo)

mean((test$medv - predito_ar_1)^2)
mean((test$medv - predito_ar_2)^2)
mean((test$medv - predito_lm)^2)

# ou pode ser feito diretamente como 

resultados %>% 
  group_by(metodo) %>% 
  summarise(eqm = mean((observado - predito)^2))


# Para considerar a raiz quadrada do erro quadrático médio - RMSE

resultados %>% 
  group_by(metodo) %>% 
  rmse(observado, predito)
  

##
## INCLUIR LASSO e RIDGE na comparação
##

