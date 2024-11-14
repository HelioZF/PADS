# Nessa tarefa você deverá executar o código completo uma vez e se certificar 
# de que entendeu todos os passos. Após essa etapa, pense em como trabalhar com 
# engenharia de features para ter melhores variáveis de entrada na rede.

library(tidyverse)
library(tidymodels)
library(keras)


# leitura dos dados  ------------------------------------------------------
path <-"C:/Users/helio/OneDrive/Documentos/Insper/PADS/PADS/Trimestre-2/Aprendizagem Estatistica de Maquina II/Aula05"
dados <- read_csv("C:/Users/helio/OneDrive/Documentos/Insper/PADS/PADS/Trimestre-2/Aprendizagem Estatistica de Maquina II/Aula05/circulo.csv") %>% 
  mutate(z= x^2 + y^2)
dados %>% 
  ggplot(aes(x, y, color = factor(classe))) + 
  geom_point(show.legend = FALSE) + 
  theme_bw()


# separação em treinamento, validação e teste -----------------------------

set.seed(123)

splits1 <- initial_split(dados, prop = 1/3, strata = "y")
splits2 <- initial_split(testing(splits1), prop = .5, strata = "y")

tr  <- training(splits1)
val <- training(splits2)
tst <- testing(splits2)

X_tr <- tr %>% 
  select(-classe) %>% 
  as.matrix()

y_tr <- as.numeric(tr$classe)


X_val <- val %>% 
  select(-classe) %>% 
  as.matrix()

y_val <- as.numeric(val$classe)


X_tst <- tst %>% 
  select(-classe) %>% 
  as.matrix()

y_tst <- as.numeric(tst$classe)


# visualização dos splits -------------------------------------------------

data.frame(X_tr, classe = y_tr, grupo = "tr") %>% 
  bind_rows(data.frame(X_val, classe =  y_val, grupo = "val")) %>% 
  bind_rows(data.frame(X_tst, classe =  y_tst, grupo = "tst")) %>% 
  ggplot(aes(x, y, color = factor(classe))) + 
    geom_point(show.legend = FALSE) + 
    facet_wrap(~ grupo)


# cria modelo -------------------------------------------------------------

rm(network)

network <- keras_model_sequential() %>% 
  layer_dense(units = 3, activation = "relu", input_shape = ncol(X_tr)) %>%
  layer_dense(units = 2, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")


# quantos parâmetros, contando peso e viés, deve ter em cada camada 
# dessa rede?

network

network %>% 
  compile(optimizer = "rmsprop", 
          loss = "binary_crossentropy", 
          metrics = c("accuracy"))

# otimizadores
# - SGD
# - rmsprop
# - Adam
# - Adadelta
# - Nadam
# - Ftrl


# note que não ajustamos o modelo

network %>% 
  fit(X_tr, y_tr, epochs = 80, batch_size = 16, 
      validation_data = list(X_val, y_val))


keras::get_weights(network)


x_grid <- crossing(x = seq(-1, 1, .01), 
                   y = seq(-1, 1, .01)) %>%
                    mutate(z= x^2 + y^2)

x_grid %>% 
  bind_cols(prob = predict(network, as.matrix(x_grid))[,1]) %>% 
  ggplot(aes(x, y)) + 
  geom_contour(aes(z = prob), breaks = seq(.5, .9, .1), col = "black", linewidth = 1) + 
  geom_point(data = dados, aes(x, y, color = factor(classe)), alpha = 0.3, show.legend = FALSE)

x_grid %>% 
  bind_cols(prob = predict(network, as.matrix(x_grid))[,1]) %>% 
  ggplot(aes(prob, ..density..)) + 
    geom_histogram()

tibble(observado = factor(y_tst)) %>% 
  bind_cols(data.frame(prob = predict(network, as.matrix(X_tst)))) %>% 
  roc_auc(observado, prob, event_level = "second") 

tibble(observado = factor(y_tst)) %>% 
  bind_cols(data.frame(prob = predict(network, as.matrix(X_tst)))) %>% 
  roc_curve(observado, prob, event_level = "second") %>% 
  autoplot()

