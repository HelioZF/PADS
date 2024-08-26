library(tidymodels)
library(skimr)
library(modeldata)
library(glmnet)
library(plotmo)
library(pROC)

data(mlc_churn)

# Customer churn data
# Description
# A data set from the MLC++ machine learning software for modeling customer churn. There 
# are 19 predictors, mostly numeric: state (categorical), account_length area_code 
# international_plan (yes/no), voice_mail_plan (yes/no), number_vmail_messages 
# total_day_minutes total_day_calls total_day_charge total_eve_minutes total_eve_calls 
# total_eve_charge total_night_minutes total_night_calls total_night_charge 
# total_intl_minutes total_intl_calls total_intl_charge, and number_customer_service_calls.
# 
# Details
# The outcome is contained in a column called churn (also yes/no). A note in one of the 
# source files states that the data are "artificial based on claims similar to real world".


#?mlc_churn 

mlc_churn %>% 
  group_by(churn) %>% 
  skim()
  
mlc_churn$state <- NULL

mlc_churn <- mlc_churn %>% 
             mutate(churn = factor(churn, levels = c("no", "yes")))

corte <- 0.50

# comparação dos modelos --------------------------------------------------

resultados <- tibble(modelo = c("logistica", "ridge", "lasso", "elastic-net"), 
                     acuracia = NA, 
                     auc = NA)

# treinamento x teste -----------------------------------------------------

set.seed(313)

idx <- sample(nrow(mlc_churn), size = .90 * nrow(mlc_churn), replace = FALSE)


# logística ---------------------------------------------------------------

fit <- glm(churn ~ ., mlc_churn[idx,], family = "binomial")

summary(fit)

prob_logistica <- predict(fit, mlc_churn[-idx,], type = "response")

resultados$acuracia[resultados$modelo == "logistica"] <- mean(mlc_churn$churn[-idx] == ifelse(prob_logistica >= corte, "yes", "no"))

resultados$auc[resultados$modelo == "logistica"] <- roc(mlc_churn$churn[-idx], prob_logistica)$auc



# prepara para glmnet -----------------------------------------------------

X_tr <- model.matrix(churn ~ ., mlc_churn)[idx, -1]
y_tr <- mlc_churn$churn[idx]


X_test <- model.matrix(churn ~ ., mlc_churn)[-idx,-1]
y_test <- mlc_churn$churn[-idx]


# ridge -------------------------------------------------------------------

ridge <- glmnet(X_tr, y_tr, alpha = 0, family = "binomial")

plot_glmnet(ridge)

cv_ridge <- cv.glmnet(X_tr, y_tr, alpha = 0, family = "binomial")

plot(cv_ridge)

lambda_ridge <- cv_ridge$lambda.1se

predict(ridge, newx = X_test, type = "response", s = lambda_ridge)

predict(ridge, newx = X_test, type = "class", s = lambda_ridge)

prob_ridge <- as.numeric(predict(ridge, newx = X_test, type = "response", s = lambda_ridge))

resultados$acuracia[resultados$modelo == "ridge"] <- mean(y_test == ifelse(prob_ridge >= corte, "yes", "no"))


# Forma 1

resultados$auc[resultados$modelo == "ridge"] <- roc(mlc_churn$churn[-idx], prob_ridge)$auc


# Forma 2

tibble(modelo = "logística", 
       probabilidade = prob_logistica, 
       classe = mlc_churn$churn[-idx]) %>% 
  bind_rows(tibble(modelo = "ridge", 
                   probabilidade = prob_ridge, 
                   classe = mlc_churn$churn[-idx])) %>% 
  group_by(modelo) %>% 
  # roc_auc(classe, probabilidade, event_level = "second") 

  roc_curve(classe, probabilidade, event_level = "second") %>% 
  autoplot()





# lasso -------------------------------------------------------------------
# Convertendo a variável alvo para numérica
y_tr <- ifelse(mlc_churn$churn[idx] == "yes", 1, 0)
y_test <- ifelse(mlc_churn$churn[-idx] == "yes", 1, 0)

lasso <- glmnet(X_tr, y_tr, alpha = 1, nlambda = 1000)

plot_glmnet(lasso, lwd = 2, cex.lab = 1.3, xvar = "lambda")

cv_lasso <- cv.glmnet(X_tr, y_tr, alpha = 1, family = "binomial")

lambda_lasso <- cv_lasso$lambda.1se

prob_lasso <- as.numeric(predict(lasso, newx = X_test, type = "response", s = lambda_lasso))

resultados$acuracia[resultados$modelo == "lasso"] <- mean(y_test == ifelse(prob_lasso >= corte, 1, 0))

resultados$auc[resultados$modelo == "lasso"] <- roc(y_test, prob_lasso)$auc

# Calculando a curva ROC
roc_lasso <- roc(y_test, prob_lasso)
# Plotando a curva ROC
plot(roc_lasso, main = "Curva ROC - Modelo Lasso", col = "blue", lwd = 2)
plot(cv_lasso)

# elastic-net -------------------------------------------------------------
# Ajuste do modelo Elastic Net
elastic_net <- glmnet(X_tr, y_tr, alpha = 0.5, nlambda = 1000)

# Validação cruzada para escolher o melhor lambda
cv_elastic_net <- cv.glmnet(X_tr, y_tr, alpha = 0.5, family = "binomial")
lambda_elastic_net <- cv_elastic_net$lambda.1se

# Gerando as probabilidades preditivas para o conjunto de teste
prob_elastic_net <- as.numeric(predict(elastic_net, newx = X_test, type = "response", s = lambda_elastic_net))

# Calculando a curva ROC
roc_elastic_net <- roc(y_test, prob_elastic_net)
#Calculando a acurácia
resultados$acuracia[resultados$modelo == "elastic-net"] <- mean(y_test == ifelse(prob_elastic_net >= corte, 1, 0))

# Calculando a AUC
resultados$auc[resultados$modelo == "elastic-net"] <- roc(y_test, prob_elastic_net)$auc


# Plotando a curva ROC
plot(roc_elastic_net, main = "Curva ROC - Modelo Elastic Net", col = "red", lwd = 2)


resultados











