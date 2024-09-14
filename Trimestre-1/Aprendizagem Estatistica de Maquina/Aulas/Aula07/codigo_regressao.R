# install.packages("gbm")
library(MASS)        # banco Boston
library(skimr)       # descritiva dos dados
library(patchwork)   # juntar gráficos
library(tidyverse)
library(gbm)         # boosting
library(xgboost)     # boosting
library(rsample)     # reamostragem
library(yardstick)   # avaliação de desempenho
library(pdp)         # dependência parcial

data(Boston)


# patchwork ---------------------------------------------------------------
# rm: average number of rooms per dwelling.
# lstat: “Proportion of population that is 
#         lower status = 0.5 * (proportion of adults without some high school education 
#                               and proportion of male workers classified as laborers).
# dis: weighted mean of distances to five Boston employment centres.

g1 <- ggplot(Boston, aes(lstat, medv)) + 
  geom_point(color = "blue", size = 1.2) + 
  geom_smooth(color = "red", se = FALSE) + 
  theme_bw()

g2 <- ggplot(Boston, aes(rm, medv)) + 
  geom_point(color = "blue", size = 1.2) + 
  geom_smooth(color = "red", se = FALSE) + 
  theme_bw()

g3 <- ggplot(Boston, aes(dis, medv)) + 
  geom_point(color = "blue", size = 1.2) + 
  geom_smooth(color = "red", se = FALSE) + 
  theme_bw()

(g1 + g2) / g3 +
  windows()


# treino x teste ----------------------------------------------------------

set.seed(123)
# separando o database em treino e teste
split <- initial_split(Boston, prop = .8)

treinamento <- training(split) 
teste <- testing(split)

(fit_bst <- gbm(medv ~ ., distribution = "gaussian", # perda quadratica
                n.trees = 5000,
                interaction.depth = 1, shrinkage = 0.1, data = treinamento))
windows() +
  summary(fit_bst) # importancia das variaveis

# partial dependence plot -------------------------------------------------

plot(fit_bst, i = "rm", lwd = 2)
plot(fit_bst, i = "dis", lwd = 2)
plot(fit_bst, i = "lstat", lwd = 2)
plot(fit_bst, i = "crim", lwd = 2)

summary(treinamento$lstat)

inter_lstat  <- seq(1, 40, 0.1)

efeito_lstat <- vector("numeric", length(inter_lstat))

treinamento_pdp <- treinamento


# barra de progresso

pb <- txtProgressBar(min = 1, max = length(inter_lstat), style = 3, width = 50)

for(i in 1:length(inter_lstat)){
  
  treinamento_pdp$lstat <- inter_lstat[i]
  efeito_lstat[i] <- mean(predict(fit_bst, treinamento_pdp, n.trees = 5000))
  setTxtProgressBar(pb, i)
  
}

close(pb)

plot(inter_lstat, efeito_lstat, type = "l", col = "blue", lwd = 2)



# Partial dependence plot para duas variáveis -----------------------------

plot(fit_bst, i = c("lstat", "rm"))



# pdp ---------------------------------------------------------------------

pdp::partial(fit_bst, pred.var = "lstat", n.trees = 5000) %>% 
  ggplot(aes(lstat, yhat)) + 
  geom_line(color = "blue", size = 1) + 
  geom_smooth(color = "red", size = 1.2, se = FALSE) +
  theme_bw() +
  windows()


pdp::partial(fit_bst, pred.var = c("lstat", "rm"), n.trees = 5000, plot = TRUE)

  
pdp::partial(fit_bst, pred.var = c("lstat", "rm"), n.trees = 5000) %>% 
  windows() +
  plotPartial(levelplot = FALSE, zlab = "medv", drape = TRUE,
              colorkey = FALSE, screen = list(z = -20, x = -60))

partial_plot <- pdp::partial(fit_bst, pred.var = c("lstat", "rm"), n.trees = 5000)

# Abrindo uma nova janela de gráficos (se necessário, no Windows)
windows() 

# Plotando o gráfico parcial
plotPartial(
  partial_plot, 
  levelplot = FALSE, 
  zlab = "medv", 
  drape = TRUE, 
  colorkey = FALSE, 
  screen = list(z = -20, x = -60)
)

# iterations --------------------------------------------------------------

boston_cv <- gbm(medv ~ ., data = treinamento, cv.folds = 5, 
                 n.trees = 5000, 
                 interaction.depth = 2, shrinkage = 0.1)

windows()+
gbm.perf(boston_cv, method = "cv")

(n_arvores <- which.min(boston_cv$cv.error)) # definindo menor n arvores para o modelo #nolint
                                             # por meio da validação cruzada #nolint

# avaliação preditiva -----------------------------------------------------

predito_bst <- predict(fit_bst, newdata = teste, n.trees = n_arvores)

(erro_bst <- mean((teste$medv - predito_bst)^2))

resultados <- tibble(observado = teste$medv, 
                     predito = predito_bst)

ggplot(resultados, aes(observado, predito)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = .5, linetype = "dashed") + 
  geom_point(color = "blue", size = 2) +
  theme_bw()


# ajuste modelo linear ----------------------------------------------------

fit_lm <- lm(medv ~ ., data = treinamento)

summary(fit_lm)

predito_lm <- predict(fit_lm, teste)

erro_lm <- mean((teste$medv - predito_lm)^2)

erro_lm




# XGBoost -----------------------------------------------------------------

library(conflicted)

conflict_prefer("select", "dplyr")   # declara o pacote da preferência de 
                                     # acordo com a função

d_tr <- xgb.DMatrix(label = treinamento$medv, 
                    data = as.matrix(dplyr::select(treinamento, -medv)))

boston_xgb <- xgboost(data = d_tr, nrounds = 500, max_depth = 4, 
                      eta = 0.1, nthread = 3, verbose = FALSE,
                      objective = "reg:squarederror")

importancia <- xgb.importance(model = boston_xgb)

xgb.plot.importance(importancia, rel_to_first = TRUE, xlab = "Relative Importance")


# erro de teste -----------------------------------------------------------

d_test <- xgb.DMatrix(label = teste$medv, data = as.matrix(dplyr::select(teste, -medv)))

pred_xgb <- predict(boston_xgb, d_test)

mean((pred_xgb - teste$medv)^2)




# ajuste de hiperparâmetro ------------------------------------------------

set.seed(123)

hiperparametros <- crossing(eta = c(.01, .1), 
                            nrounds = c(250, 750), 
                            max_depth = c(1, 4))




ajusta_bst <- function(splits, eta, nrounds, max_depth) {
  
  tr <- training(splits)
  teste <- testing(splits)
  
  d_treino <- xgb.DMatrix(label = tr$medv, 
                          data = as.matrix(select(tr, -medv)))
  
  d_teste  <- xgb.DMatrix(label = teste$medv, 
                          data = as.matrix(select(teste, -medv)))
  
  fit <- xgb.train(data = d_treino, nrounds = nrounds, max_depth = max_depth, eta = eta, 
                   nthread = 10, verbose = FALSE, objective = "reg:squarederror")
  
  eqm <- mean((teste$medv - predict(fit, as.matrix(select(teste, -medv))))^2)
  
  return(sqrt(eqm))
  
}




resultados <- rsample::vfold_cv(treinamento, 5) %>% 
  crossing(hiperparametros) %>% 
  mutate(reqm = pmap_dbl(list(splits, eta, nrounds, max_depth), ajusta_bst))


resultados %>% 
  group_by(eta, nrounds, max_depth) %>% 
  summarise(reqm = mean(reqm)) %>% 
  arrange(reqm)

