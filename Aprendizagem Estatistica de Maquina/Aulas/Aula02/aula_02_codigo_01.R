library(tidyverse)
library(FNN)
# Definir as opções do dispositivo gráfico
options(jupyter.plot_mimetypes = 'image/png')

# Ajustar as opções para gráficos
options(repr.plot.width = 8, repr.plot.height = 6, repr.plot.res = 120)

#ctrl+shift+r Para criar seções
# psi(x) - modelo base ----------------------------------------------------

ggplot(data = data.frame(x = 0), aes(x = x)) +
  stat_function(fun = function(x) 45 * tanh(x / 1.9 - 7) + 57) +
  xlim(8, 18)

# amostra gerada ----------------------------------------------------------
n_obs <- 100

set.seed(0508)#123

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)),
                y = 45 * tanh(x / 1.9 - 7) + 57 +
                  rnorm(n = n_obs, mean = 0, sd = 4))

head(dados)
dados |>
  ggplot(aes(x, y)) +
  geom_point()

dados[dados$x > 15, ]
# spline ------------------------------------------------------------------

fit <- smooth.spline(dados$x, dados$y, df = 10, all.knots = TRUE)

fit

predict(fit, dados$x)$y # x e a variavel preditora e y as previsões


# avaliar o EQM considerando de 2 a 100 graus de liberdade
graus_liberdade <- seq(from = 2, to = 100)
resultados <- tibble(gl = graus_liberdade, eqm = NA_real_)
head(resultados)

for (i in 1:nrow(resultados)) {
  fit <- smooth.spline(dados$x, dados$y, df = resultados$gl[i],
                       all.knots = TRUE)
  predictions <- predict(fit, dados$x)$y
  resultados$eqm[i] <- mean((dados$y - predictions)^2)
}

head(resultados)
resultados |>
  ggplot(aes(gl, eqm)) +
  geom_point() +
  geom_line()
# Criando um dataframe diferenet para testar o modelo
# uma vez que o modelo foi treinado e testado no mesmo DF

dados_teste <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)),
                            y = 45 * tanh(x / 1.9 - 7) + 57 +
                              rnorm(n = n_obs, mean = 0, sd = 4))
head(dados_teste)

for (i in 1:nrow(resultados)) {
  fit <- smooth.spline(dados$x, dados$y, df = resultados$gl[i],
                       all.knots = TRUE)
  predictions <- predict(fit, dados_teste$x)$y
  resultados$eqm[i] <- mean((dados$y - predictions)^2)
}
head(resultados)
resultados |>
  ggplot(aes(gl, eqm)) +
  geom_point() +
  geom_line() +
  theme_minimal(base_size = 20)
