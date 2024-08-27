library(tidyverse)
library(rpart)
library(partykit)
library(rpart.plot)

set.seed(123)
beta <- rep(c(2, 55, 24), each = 100)
dados <- tibble(x = rep(1:30, each = 10),
y = beta + rnorm(length(x), 0, 3))
dados %>%
ggplot(aes(x, y)) +
geom_point(color = "blue", size = 2)


(RSS_nulo <- sum((dados$y - mean(dados$y))^2))
 
cortes <- unique(dados$x)
 
cortes <- (cortes[-1] + cortes[-length(cortes)])/2
 
resultados <- tibble(c = cortes, RSS = NA)
 
for (i in 1:nrow(resultados)) { # nolint
  RSS1 <- sum((dados$y[dados$x < resultados$c[i]] - mean(dados$y[dados$x < resultados$c[i]]))^2) # nolint: line_length_linter.
  RSS2 <- sum((dados$y[dados$x > resultados$c[i]] - mean(dados$y[dados$x > resultados$c[i]]))^2)
  resultados$RSS[i] <- RSS1 + RSS2
 
}
resultados|>
    ggplot(aes(c,RSS))+
    geom_hline(yintercept = RSS_nulo, color = "red", linetype=2)+
    geom_line()+
    geom_point()

arvore <- rpart(y ~ x, dados)
plot_arvore <- as.party(arvore)
windows()+plot(plot_arvore)

arvore <- rpart(y ~ x, dados, control = rpart.control(minsplit = 40, minbucket = 20, cp = 0))
arvore <- as.party(arvore)
plot(arvore)

arvore <- rpart(y ~ x, dados, control = rpart.control(minsplit = 40, minbucket = 20, cp = 0))
windows()+rpart.plot(arvore)

arvore <- rpart(y ~ x, dados, control = rpart.control(cp = 0.0001))
arvore$cptable
windows()+plotcp(arvore)

tabela_cp <- arvore$cptable
cp_otimo <- tabela_cp[,"xerror"] == min(tabela_cp[, "xerror"], "CP")
#-----------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(rsample)    # divisão em treinamento e teste
library(rpart)      # árvore
library(rpart.plot) # diagrama de árvore
library(ISLR)       # banco de dados
library(yardstick)  # métricas de desempenho
set.seed(21)
 
splits <- initial_split(Credit, prop = .8)
 
tr   <- training(splits)
test <- testing(splits)
 
arvore <- rpart(Balance ~ . -ID, 
                control = rpart.control(cp = 0), 
                data = tr)
 
rpart.plot(arvore)
 
# Poda
 
tabela_cp <- arvore$cptable
 
cp_otimo <- tabela_cp %>% 
  as_tibble() %>% 
  filter(xerror == min(xerror))
 
arvore_podada <- prune(arvore, cp = cp_otimo$CP[1])
 
rpart.plot(arvore_podada)
 
 
# Modelo linear -----------------------------------------------------------
 
modelo_linear <- lm(Balance ~ . - ID, data = tr)
 
 
# Previsões ---------------------------------------------------------------
 
tibble(modelo = "árvore podada", 
       y_test = test$Balance, 
       y_predito = predict(arvore_podada, newdata = test)) %>% 
  bind_rows(tibble(modelo = "modelo linear", 
                   y_test = test$Balance, 
                   y_predito = predict(modelo_linear, newdata = test))) %>% 
  ggplot(aes(y_predito, y_test)) + 
    geom_abline(slope = 1, intercept = 0, color = "red", size = 2) + 
    geom_point() +
    facet_wrap(~ modelo)
 
 
 
tibble(modelo = "árvore podada", 
       y_test = test$Balance, 
       y_predito = predict(arvore_podada, newdata = test)) %>% 
  bind_rows(tibble(modelo = "modelo linear", 
                   y_test = test$Balance, 
                   y_predito = predict(modelo_linear, newdata = test))) %>% 
  group_by(modelo) %>% 
  # summarise(eqm = sqrt(mean((y_test - y_predito)^2)))
  rmse(y_test, y_predito)
 