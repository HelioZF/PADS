#Carregando os pacotes que serão utilizados
library(tidymodels)
library(skimr)
library(modeldata)
library(glmnet)
#install.packages()
library(plotmo)
library(pROC)
library(DescTools)

#base de dados do pacote modeldata
data(credit_data)
str(credit_data)

credit_data %>%  
  skim()

# analise dos dados --------------------------------------------------
# Qual a variável resposta?
# R: A variavel que estamos tentando prever é se a divida será paga ou não
#    assim sendo a resposta é a variável: ---Status---
# Como ela se relaciona com as demais variáveis?
credit_data |>
  ggplot(aes(x = Job, y = Expenses, color = Status)) +
  geom_point()

# tratamento de missings --------------------------------------------------

#Opção 1: remover
df <- na.omit(credit_data)
df |> 
  skim()

#Opção 2: inputar com média, mediana ou moda
#credit_data$column_with_missing_values <- ifelse(is.na(credit_data$column_with_missing_values), mean(credit_data$column_with_missing_values, na.rm = TRUE), credit_data$column_with_missing_values)
#credit_data$column_with_missing_values <- ifelse(is.na(credit_data$column_with_missing_values), Mode(credit_data$column_with_missing_values), credit_data$column_with_missing_values)


# treinamento x teste -----------------------------------------------------

set.seed(42)

idx <- sample(nrow(credit_data), size = .70 * nrow(credit_data), replace = FALSE)
train_data <- credit_data[idx, ]  # Conjunto de dados de treinamento
test_data <- credit_data[-idx, ]  # Conjunto de dados de teste
# logística ---------------------------------------------------------------

# Ajuste da regressão com o glm()
fit <- glm(Status ~ ., data = train_data, family = "binomial")
# Status-> Variavel a ser predita -- ~ . todas as outras variaveis serão utilizadas para prever Status -- data-> banc de dados utilizado -- family-> tipo de classificação
# Exibir o resumo do modelo para ver os coeficientes ajustados e outras estatísticas
summary(fit)

# Considerar um ponto de corte para a análise de acurácia
# Fazer previsões no conjunto de teste
pred_prob <- predict(fit, newdata = test_data, type = "response")

# Considerar um ponto de corte (threshold) para classificar as previsões como "paga" ou "não paga"
threshold <- 0.5
pred_class <- ifelse(pred_prob > threshold, "paga", "não paga")

# Criar uma matriz de confusão para avaliar a acurácia do modelo
conf_matrix <- table(test_data$Status, pred_class)

# Exibir a matriz de confusão
print(conf_matrix)

# Calcular a acurácia
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Acurácia:", round(accuracy, 2)))

# Você também pode usar o pacote pROC para calcular a curva ROC e a área sob a curva (AUC)
roc_curve <- roc(test_data$Status, pred_prob)
plot(roc_curve)
auc(roc_curve)

# Explorar métricas que o Tiago dividiu na aula 3

