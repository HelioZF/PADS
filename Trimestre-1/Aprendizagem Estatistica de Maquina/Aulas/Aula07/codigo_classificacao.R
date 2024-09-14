required_packages <- c(
  "skimr", "patchwork", "tidyverse", "gbm", "xgboost", 
  "ranger", "rsample", "yardstick", "pdp"
)

# Função para verificar e instalar pacotes ausentes
install_if_missing <- function(packages) {
  installed_packages <- rownames(installed.packages())
  for (pkg in packages) {
    if (!pkg %in% installed_packages) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
}

# Instalando pacotes necessários
install_if_missing(required_packages)
library(skimr)
library(patchwork)
library(tidyverse)
library(gbm)
library(xgboost)
library(ranger)
library(rsample)
library(yardstick)
library(pdp)


# telco -------------------------------------------------------------------

dados <- read.csv("../dados/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
  dplyr::select(-customerID)

dim(dados)

dados <- na.omit(dados)

dim(dados)

dados <- dados %>% 
  mutate_if(is.character, ~as.factor(.x))

dados %>% 
  group_by(Churn) %>% 
  skim()


dados %>% 
  count(gender) %>% 
  mutate(porcentagem = n / sum(n))



g1 <- dados %>% 
  group_by(Churn) %>% 
  count(Dependents) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(Churn, y = prop, fill = Dependents)) +
  geom_col() +
  theme_bw()


g2 <- dados %>% 
  ggplot(aes(Churn, TotalCharges, fill = Churn)) +
  geom_boxplot() + 
  theme_bw()


g3 <- dados %>% 
  ggplot(aes(Churn, tenure, fill = Churn)) +
  geom_boxplot() + 
  theme_bw()

g1 + g2 / g3 


# treinamento e teste -----------------------------------------------------

set.seed(123)

splits <- initial_split(dados, prop = .8, strata = Churn)

dados_tr   <- training(splits)
dados_test <- testing(splits)


# floresta aleatória ------------------------------------------------------

rf <- ranger(Churn ~ ., probability = TRUE, dados_tr)

predito_rf <- predict(rf, dados_test)$predictions[,"Yes"]

desempenho <- tibble(prob = predito_rf, 
                     classes = dados_test$Churn, 
                     metodo = "floresta aleatória")



# boosting ----------------------------------------------------------------

dados_tr$Churn <- ifelse(dados_tr$Churn == "Yes", 1, 0)
dados_test$Churn <- ifelse(dados_test$Churn == "Yes", 1, 0)

(fit_bst <- gbm(Churn ~ ., distribution = "bernoulli", n.trees = 1000, 
                interaction.depth = 4, shrinkage = 0.05, data = dados_tr))

summary(fit_bst)

fit_cv <- gbm(Churn ~ ., data = dados_tr, cv.folds = 5, n.trees = 1000, 
              interaction.depth = 4, distribution = "bernoulli", shrinkage = 0.05)

gbm.perf(fit_cv, method = "cv")

prob_gbm <- predict(fit_cv, dados_test, n.trees = 114, type = "response") # revisar o n.trees

# predito <- ifelse(prob_gbm >= .75, 1, 0)
# mean(dados_test$Churn != predito)

desempenho <- desempenho %>% 
  bind_rows(tibble(prob = prob_gbm, 
                   classes = ifelse(dados_test$Churn == 1, "Yes", "No"), 
                   metodo = "gbm"))


# logística ---------------------------------------------------------------

fit_log <- glm(Churn ~ ., data = dados_tr, family = "binomial")

prob_log <- predict(fit_log, newdata = dados_test, type = "response")


desempenho <- desempenho %>% 
  bind_rows(tibble(prob = prob_log, 
                   classes = ifelse(dados_test$Churn == 1, "Yes", "No"), 
                   metodo = "logística"))



# curva ROC ---------------------------------------------------------------

desempenho %>% 
  mutate(classes = factor(classes)) %>% 
  group_by(metodo) %>% 
  roc_auc(classes, prob, event_level = "second") %>% 
  arrange(desc(.estimate))


desempenho %>% 
  mutate(classes = factor(classes)) %>% 
  group_by(metodo) %>% 
  roc_curve(classes, prob, event_level = "second") %>% 
  autoplot()


