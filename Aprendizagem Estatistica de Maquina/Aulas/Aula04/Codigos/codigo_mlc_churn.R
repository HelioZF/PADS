library(tidymodels)  # Carrega a coleção de pacotes para modelagem
library(skimr)       # Carrega o pacote para exploração dos dados
library(modeldata)   # Carrega o pacote que contém datasets de exemplo
library(glmnet)      # Carrega o pacote para regressão penalizada (Lasso, Ridge, Elastic Net)
library(plotmo)      # Carrega o pacote para visualização de modelos de regressão
library(pROC)        # Carrega o pacote para análise da curva ROC

data(mlc_churn)  # Carrega o conjunto de dados "mlc_churn" para modelagem

# Resumo do conjunto de dados dividido pela variável de resposta "churn"
mlc_churn %>% 
  group_by(churn) %>% 
  skim()  # Aplica a função skim para obter um resumo estatístico dos dados

mlc_churn$state <- NULL  # Remove a coluna "state" do conjunto de dados

# Converte a variável "churn" em um fator com níveis "no" e "yes"
mlc_churn <- mlc_churn %>% 
             mutate(churn = factor(churn, levels = c("no", "yes")))

corte <- 0.50  # Define o corte para classificar as probabilidades em "yes" ou "no"

# Cria um tibble para armazenar os resultados dos modelos
resultados <- tibble(modelo = c("logistica", "ridge", "lasso", "elastic-net"), 
                     acuracia = NA,  # Coluna para armazenar a acurácia de cada modelo
                     auc = NA)       # Coluna para armazenar a AUC de cada modelo

# Divisão dos dados em treinamento (90%) e teste (10%)
set.seed(313)  # Define a semente para reprodução dos resultados
idx <- sample(nrow(mlc_churn), size = .90 * nrow(mlc_churn), replace = FALSE)

# Modelo de Regressão Logística -------------------------------------------
fit <- glm(churn ~ ., mlc_churn[idx,], family = "binomial")  
# Ajusta o modelo de regressão logística, onde a variável resposta é "churn" 
# e todas as outras colunas são preditores

summary(fit)  # Exibe o resumo do modelo ajustado, incluindo coeficientes e estatísticas

# Gera as probabilidades preditivas para o conjunto de teste
prob_logistica <- predict(fit, mlc_churn[-idx,], type = "response")

# Calcula a acurácia do modelo de regressão logística
resultados$acuracia[resultados$modelo == "logistica"] <- mean(mlc_churn$churn[-idx] == ifelse(prob_logistica >= corte, "yes", "no"))

# Calcula a AUC para o modelo de regressão logística
resultados$auc[resultados$modelo == "logistica"] <- roc(mlc_churn$churn[-idx], prob_logistica)$auc

# Preparação dos dados para o glmnet --------------------------------------

# Cria matrizes de preditores para o glmnet; a função model.matrix converte 
# as variáveis categóricas em variáveis dummy
X_tr <- model.matrix(churn ~ ., mlc_churn)[idx, -1]  # Matriz de treinamento
y_tr <- mlc_churn$churn[idx]  # Variável resposta de treinamento

X_test <- model.matrix(churn ~ ., mlc_churn)[-idx,-1]  # Matriz de teste
y_test <- mlc_churn$churn[-idx]  # Variável resposta de teste

# Ridge Regression --------------------------------------------------------

ridge <- glmnet(X_tr, y_tr, alpha = 0, family = "binomial")
# Ajusta o modelo de regressão Ridge (alpha = 0) com família binomial (para classificação)

plot_glmnet(ridge)  # Plota os coeficientes das variáveis em função do lambda

cv_ridge <- cv.glmnet(X_tr, y_tr, alpha = 0, family = "binomial")
# Realiza validação cruzada para escolher o melhor valor de lambda

plot(cv_ridge)  # Plota os erros de validação cruzada em função de lambda

lambda_ridge <- cv_ridge$lambda.1se  # Seleciona o lambda com 1 erro-padrão acima do mínimo

# Gera as probabilidades preditivas para o conjunto de teste
prob_ridge <- as.numeric(predict(ridge, newx = X_test, type = "response", s = lambda_ridge))

# Calcula a acurácia do modelo Ridge
resultados$acuracia[resultados$modelo == "ridge"] <- mean(y_test == ifelse(prob_ridge >= corte, "yes", "no"))

# Calcula a AUC para o modelo Ridge
resultados$auc[resultados$modelo == "ridge"] <- roc(mlc_churn$churn[-idx], prob_ridge)$auc

# Alternativamente, cria uma tabela com as probabilidades preditivas e classes verdadeiras
tibble(modelo = "logística", 
       probabilidade = prob_logistica, 
       classe = mlc_churn$churn[-idx]) %>% 
  bind_rows(tibble(modelo = "ridge", 
                   probabilidade = prob_ridge, 
                   classe = mlc_churn$churn[-idx])) %>% 
  group_by(modelo) %>% 
  roc_curve(classe, probabilidade, event_level = "second") %>% 
  autoplot()  # Plota a curva ROC para os modelos logística e Ridge

# Lasso Regression --------------------------------------------------------
# Convertendo a variável alvo para numérica
y_tr <- ifelse(mlc_churn$churn[idx] == "yes", 1, 0)  # Converte "yes" para 1 e "no" para 0
y_test <- ifelse(mlc_churn$churn[-idx] == "yes", 1, 0)

lasso <- glmnet(X_tr, y_tr, alpha = 1, nlambda = 1000)
# Ajusta o modelo de regressão Lasso (alpha = 1) com 1000 valores de lambda

plot_glmnet(lasso, lwd = 2, cex.lab = 1.3, xvar = "lambda")
# Plota os coeficientes das variáveis em função de lambda

cv_lasso <- cv.glmnet(X_tr, y_tr, alpha = 1, family = "binomial")
# Realiza validação cruzada para escolher o melhor valor de lambda

lambda_lasso <- cv_lasso$lambda.1se  # Seleciona o lambda com 1 erro-padrão acima do mínimo

# Gera as probabilidades preditivas para o conjunto de teste
prob_lasso <- as.numeric(predict(lasso, newx = X_test, type = "response", s = lambda_lasso))

# Calcula a acurácia do modelo Lasso
resultados$acuracia[resultados$modelo == "lasso"] <- mean(y_test == ifelse(prob_lasso >= corte, 1, 0))

# Calcula a AUC para o modelo Lasso
resultados$auc[resultados$modelo == "lasso"] <- roc(y_test, prob_lasso)$auc

# Calcula e plota a curva ROC para o modelo Lasso
roc_lasso <- roc(y_test, prob_lasso)  # Calcula a curva ROC
plot(roc_lasso, main = "Curva ROC - Modelo Lasso", col = "blue", lwd = 2)  # Plota a curva ROC
plot(cv_lasso)  # Plota os erros de validação cruzada em função de lambda

# Elastic Net -------------------------------------------------------------
# Ajuste do modelo Elastic Net
elastic_net <- glmnet(X_tr, y_tr, alpha = 0.5, nlambda = 1000)
# Ajusta o modelo de regressão Elastic Net (alpha = 0.5, combinação de Lasso e Ridge)

# Validação cruzada para escolher o melhor lambda
cv_elastic_net <- cv.glmnet(X_tr, y_tr, alpha = 0.5, family = "binomial")
lambda_elastic_net <- cv_elastic_net$lambda.1se  # Seleciona o lambda com 1 erro-padrão acima do mínimo

# Gera as probabilidades preditivas para o conjunto de teste
prob_elastic_net <- as.numeric(predict(elastic_net, newx = X_test, type = "response", s = lambda_elastic_net))

# Calcula a acurácia do modelo Elastic Net
resultados$acuracia[resultados$modelo == "elastic-net"] <- mean(y_test == ifelse(prob_elastic_net >= corte, 1, 0))

# Calcula a AUC para o modelo Elastic Net
resultados$auc[resultados$modelo == "elastic-net"] <- roc(y_test, prob_elastic_net)$auc

# Calcula e plota a curva ROC para o modelo Elastic Net
roc_elastic_net <- roc(y_test, prob_elastic_net)
plot(roc_elastic_net, main = "Curva ROC - Modelo Elastic Net", col = "red", lwd = 2)

# Exibe a tabela de resultados atualizada com acurácia e AUC para todos os modelos
resultados