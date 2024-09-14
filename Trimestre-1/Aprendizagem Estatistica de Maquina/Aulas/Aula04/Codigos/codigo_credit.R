library(ISLR)      # banco de dados
install.packages("ISLR")
library(vip)       # variable importance 
library(glmnet)    # LASSO, ridge e elasticnet
library(plotmo)    # gráficos
library(patchwork) # agrupar gráficos
library(tidyverse)


# entradas para glmnet ----------------------------------------------------

X <- model.matrix(Balance ~ ., data = Credit[,-1])[,-1] # X deve ser uma matrix

class(X)

y <- Credit$Balance


# resultados dos modelos para comparação ----------------------------------

tab <- tibble(metodo = c("lm", "ridge", "lasso", "elastic-net"), 
              mse = NA)


# treinamento x teste -----------------------------------------------------

set.seed(321)

idx <- sample(nrow(Credit), size = .75 * nrow(Credit)) # indice treinamento


# modelo linear -----------------------------------------------------------

fit_lm <- lm(Balance ~ ., data = Credit[idx, -1])

summary(fit_lm)

y_lm <- predict(fit_lm, Credit[-idx,])

tab$mse[tab$metodo == "lm"] <- mean((y[-idx] - y_lm)^2)

tab


# ridge -------------------------------------------------------------------

ridge <- glmnet(X[idx,], y[idx], alpha = 0, nlambda = 500)

plot_glmnet(ridge, lwd = 2, cex.lab = 1.3)

ridge$a0

ridge$beta

ridge$lambda


# ridge - validação cruzada -----------------------------------------------

cv_ridge <- cv.glmnet(X[idx,], y[idx], alpha = 0)

plot(cv_ridge, cex.lab = 1.3)

y_ridge <- predict(ridge, newx = X[-idx,], s = cv_ridge$lambda.1se) # valor predito

tab$mse[tab$metodo == "ridge"] <- mean((y[-idx] - y_ridge)^2)


fit_ridge <- glmnet(X[idx,], y[idx], alpha = 0, lambda = cv_ridge$lambda.1se)


# LASSO -------------------------------------------------------------------

lasso <- glmnet(X[idx,], y[idx], alpha = 1, nlambda = 1000)

plot_glmnet(lasso, lwd = 2, cex.lab = 1.3, xvar = "lambda")

cv_lasso <- cv.glmnet(X[idx,], y[idx], alpha = 1, lambda = lasso$lambda)

plot(cv_lasso, cex.lab = 1.3)

y_lasso <- predict(lasso, newx = X[-idx,], s = cv_lasso$lambda.min)

tab$mse[tab$metodo == "lasso"] <- mean((y[-idx] - y_lasso)^2)

tab


fit_lasso <- glmnet(X[idx,], y[idx], alpha = 1, lambda = cv_lasso$lambda.min)

# elastic-net -------------------------------------------------------------

elastic <- glmnet(X[idx,], y[idx], alpha = 0.75, nlambda = 1000)

plot_glmnet(elastic, lwd = 2, cex.lab = 1.3, xvar = "lambda")

cv_elastic <- cv.glmnet(X[idx,], y[idx], alpha = 0.5, lambda = elastic$lambda)

plot(cv_elastic, cex.lab = 1.3)

y_elastic <- predict(elastic, newx = X[-idx,], s = cv_elastic$lambda.min)

tab$mse[tab$metodo == "elastic-net"] <- mean((y[-idx] - y_elastic)^2)

fit_elastic <- glmnet(X[idx,], y[idx], alpha = 0.5, lambda = cv_elastic$lambda.min)



# avaliação ---------------------------------------------------------------

tab %>% 
  arrange(mse)


# variable importance -----------------------------------------------------

g1 <- vip(fit_lm, mapping = aes(fill = Sign)) + 
  labs(subtitle = "LM")

g2 <- vip(fit_ridge, mapping = aes(fill = Sign)) + 
  labs(subtitle = "Ridge")

g3 <- vip(fit_lasso, mapping = aes(fill = Sign)) + 
  labs(subtitle = "LASSO")

g4 <- vip(fit_elastic, mapping = aes(fill = Sign)) + 
  labs(subtitle = "Elastic Net")

(g1 + g2) / (g3 + g4) + plot_layout(guides = "collect")
