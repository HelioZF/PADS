library(MASS); library(ranger); data(Boston); library(tidyverse)
set.seed(123)
(rf <- ranger(medv ~ ., data = Boston))
(rf$prediction.error)
Boston <- Boston
resultados <- tibble(n_arvores = 1:500, mse = NA)

for (i in 1:nrow(resultados)) {
  rf <- ranger(medv ~ ., num.trees = resultados$n_arvores[i], data = Boston)
  resultados$mse[i] <- rf$prediction.error
}

resultados %>%
  ggplot(aes(n_arvores, mse)) +
  windows() +
  geom_line(color = "#5B5FFF", size = 1.2) +
  labs(x = "Número de Árvores", y = "MSE (OOB)") +
  theme_bw()

resultados <- crossing(mtry = c(2, 4, 8, 13), 
                       n_arvores = c(1:10, seq(10, 500, 10)))

ajusta <- function(mtry, n_arvores) {
  rf <- ranger(medv ~ ., num.trees = n_arvores, mtry = mtry, data = Boston)
  return(rf$prediction.error)
}
resultados <- resultados %>%
  mutate(mse = map2_dbl(mtry, n_arvores, ajusta))
head(resultados)

resultados %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(n_arvores, mse, group = mtry, color = mtry)) +
  windows() +
  geom_line( size = 1.2) +
  labs(x = "Número de Árvores", y = "MSE (OOB)") +
  theme_bw()

library(vip)
rf1 <- ranger(medv ~ ., importance = "impurity", data = Boston)
vip::vip(rf1, aesthetics = list(fill = "#FF5757"))
rf2 <- ranger(medv ~ ., importance = "permutation", data = Boston)
vip::vip(rf2, aesthetics = list(fill = "#FF5757"))

#--------------------------------------------------------------------
library(modeldata)
library(ranger)
library(rsample)

data(mlc_churn)
mlc_churn$churn <- factor(mlc_churn$churn, levels = c("no", "yes"))

set.seed(15)
splits <- initial_split(mlc_churn, prop = .9, strata = "churn")
treino <- training(splits)
teste <- testing(splits)
(rf <- ranger(churn ~ ., data = treino))

rf$confusion.matrix

resultados <- tibble(n_arvores = c(1:15, seq(25, 500, 25)), erro = NA)
resultados <- resultados %>%
  mutate(erro = map_dbl(n_arvores, ~ranger(churn ~ ., num.trees = .x,
                                           data = treino)$prediction.error))
resultados %>%
  ggplot(aes(n_arvores, erro)) +
  geom_line(color = "#5B5FFF", size = 1.2) +
  labs(x = "Número de Árvores", y = "Erro de Classificação (OOB)") +
  theme_bw()

resultados <- crossing(mtry = c(4, 8, 15, 19),
                       n_arvores = c(1, 5, 10, seq(25, 500, 25)))
ajusta <- function(mtry, n_arvores) {
  rf <- ranger(churn ~ ., num.trees = n_arvores, mtry = mtry, data = treino)
  return(rf$prediction.error)
}
resultados <- resultados %>%
  mutate(erro = map2_dbl(mtry, n_arvores, ajusta))
head(resultados)

resultados %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(n_arvores, erro, group = mtry, color = mtry)) +
  geom_line( size = 1.2) +
  labs(x = "Número de Árvores", y = "Erro de Classificação (OOB)") +
  theme_bw()

rf1 <- ranger(churn ~ ., importance = "impurity", data = treino)
vip::vip(rf1, aesthetics = list(fill = "#FF5757"))
rf2 <- ranger(churn ~ ., importance = "permutation", data = treino)
vip::vip(rf2, aesthetics = list(fill = "#FF5757"))
