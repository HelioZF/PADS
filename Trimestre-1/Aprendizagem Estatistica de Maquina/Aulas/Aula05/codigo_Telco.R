library(rpart)      # árvore
library(rpart.plot) # árvore
library(partykit)   # árvore
library(skimr)      # descritiva dos dados
library(glmnet)     # LASSO e ridge
library(plotmo)     # gráficos para LASSO e Ridge
library(naniar)     # biblioteca para visualizar missing
library(rsample)    # biblioteca para separar em treinamento e teste
library(pROC)       # curva ROC
library(yardstick)  # curva ROC
library(tidyverse)


dados <- read_csv("../dados/WA_Fn-UseC_-Telco-Customer-Churn.csv")

skim(dados) # descritiva dos dados

vis_miss(dados) # apresenta os dados faltantes

dados <- dados %>%
  na.omit(df) %>%            # retira toda linha que tenha dado faltante
  dplyr::select(-customerID) # exclui a medidas customerID


# treino e teste ----------------------------------------------------------


# árvore ------------------------------------------------------------------


# logística ---------------------------------------------------------------


# ridge -------------------------------------------------------------------


# LASSO -------------------------------------------------------------------


# curva ROC ---------------------------------------------------------------


## Comparação dos modelos
