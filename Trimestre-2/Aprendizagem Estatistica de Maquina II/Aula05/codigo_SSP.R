library(viridis)
library(tidyverse)
library(tidymodels)
library(keras)


# indicadores -------------------------------------------------------------

dados <- read.csv2("../dados/indicadores_completos_processado.csv", 
                   fileEncoding = "UTF-8")

head(dados)


# processamento dos dados -------------------------------------------------

receita_prep <- dados %>% 
  select(-(municipio_cod:pop_19)) %>% 
  recipe(~ .) %>% 
  step_normalize(all_numeric()) %>% 
  prep

dados_proc <- bake(receita_prep, new_data = NULL)


# treinamento e teste -----------------------------------------------------

set.seed(314)

splits <- initial_split(dados_proc, prop = 0.8)

treinamento <- training(splits) %>% as.matrix()

teste <- testing(splits) %>% as.matrix()

rm(network)

network <- keras_model_sequential() %>% 
  layer_dense(units = 10, activation = "relu", input_shape = 23) %>%
  #layer_dropout(rate = .5) %>% 
  layer_dense(units = 3, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  #layer_dropout(rate = .5) %>% 
  layer_dense(units = 23)

network

network %>% 
  compile(optimizer = "adam", 
          loss = "mean_squared_error")

network %>% 
  fit(treinamento, treinamento, 
      epochs = 100, 
      batch_size = 16, 
      validation_split = 0.2)


keras::get_weights(network)

# 23 x 10 (pesos) e 10 vieses
# 10 x 3 (pesos) e 3 vieses
#  3 x 10 (pesos) e 10 vieses
# 10 x 23 (pesos) e 23 vieses


diferenca <- data.frame(abs(teste - predict(network, teste)))

colnames(diferenca) <- colnames(dados)[-(1:3)]

tibble(id = 1:nrow(diferenca)) %>% 
  bind_cols(diferenca) %>% 
  pivot_longer(-id, names_to = "indicador", values_to = "diferenca") %>% 
  ggplot(aes(id, indicador, fill = diferenca)) + 
    geom_tile() +   
    labs(y = "") + 
    scale_fill_viridis(direction = 1) + 
    theme_bw()

