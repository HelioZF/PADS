library(tidyverse)
library(FNN)

# amostra gerada ----------------------------------------------------------

n_obs <- 100

set.seed(123)

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

dados %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  theme_bw(base_size = 20)


# KNN ---------------------------------------------------------------------

knn.reg(train = dados$x, 
        test = matrix(dados$x[1:10]), 
        y =  dados$y, 
        k = 10)$pred


n_obs <- 100

set.seed(123)

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4)) # nolint

x_pred <- seq(8, 18, 0.1)

tibble(ajuste = x_pred,
       y_fit = knn.reg(train = dados$x, 
                       test = matrix(x_pred), 
                       y =  dados$y, 
                       k = 15)$pred) %>% 
  ggplot(aes(ajuste, y_fit)) + 
    geom_point(data = dados, aes(x, y)) +
    geom_step(color = "red") +
    theme_grey(base_size = 20)
# EQM ---------------------------------------------------------------------
resultados <- tibble(knn = 1:90,
                     eqm = NA_real_)

set.seed(123)
lotes <- sample(rep(1:10, each = 10))
lotes

eqm_aux <- vector("numeric", 10)

for (i in 1:nrow(resultados)){

  for (j in 1:10){
    dados_tr <- dados[lotes != j,]
    dados_tst <- dados[lotes == j,]

    y_hat <- knn.reg(train = dados_tr$x,
                     test = matrix(dados_tst$x),
                     y =  dados_tr$y,
                     k = resultados$knn[i])$pred

    eqm_aux[j] <- mean((dados_tst$y - y_hat)^2)
  }
  resultados$eqm[i] <- mean(eqm_aux)
}

resultados |> arrange(eqm)
resultados |> filter(eqm == min(eqm))
