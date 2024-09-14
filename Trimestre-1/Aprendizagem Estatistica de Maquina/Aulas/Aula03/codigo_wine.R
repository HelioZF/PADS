library(tidyverse)
library(rsample)
library(yardstick)

# leitura -----------------------------------------------------------------

dados <- read.csv2("../dados/winequality-white.csv") %>% 
  mutate(quality = case_when(quality  > 6 ~ "alta",
                             quality <= 6 ~ "baixa")) %>% 
  mutate(quality = factor(quality, levels = c("baixa", "alta")))



# treinamento x teste -----------------------------------------------------

set.seed(321)

splits <- initial_split(dados, prop = .8, strata = "quality")

treinamento <- training(splits)
teste <- testing(splits)


# logística ---------------------------------------------------------------

fit <- glm(quality ~ ., family = "binomial", data = treinamento)

summary(fit)

tibble(marcador = predict(fit, teste, type = "response"), 
       resposta = teste$quality, 
       modelo = "logística") %>% 
  roc_auc(resposta, marcador, event_level = "second")


# KNN ---------------------------------------------------------------------

knn_prob <- class::knn(treinamento[,-ncol(treinamento)], 
                       teste[,-ncol(treinamento)], 
                       treinamento$quality, 
                       k = 5, 
                       prob = TRUE
                       )

attr(knn_prob, "prob")     # probabilidade de qualidade baixa
1 - attr(knn_prob, "prob") # probabilidade de qualidade alta


# qual o valor ótimo de K??


# avaliação ---------------------------------------------------------------

tibble(marcador = predict(fit, teste, type = "response"), 
       resposta = teste$quality, 
       modelo = "logística") %>% 
  bind_rows(tibble(marcador = 1 - attr(knn_prob, "prob"), 
                   resposta = teste$quality, 
                   modelo = "5-NN")) %>% 
  group_by(modelo) %>% 
  # roc_auc(resposta, marcador, event_level = "second")
  roc_curve(resposta, marcador, event_level = "second") %>% 
  autoplot()

