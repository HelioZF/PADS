library(tidyverse)
library(patchwork)
library(pROC)
library(yardstick)
library(ISLR)
library(skimr)



# leitura dos dados -------------------------------------------------------

Default %>%
  group_by(default) %>%
  skim()


(g1 <- Default %>% 
  ggplot(aes(default, balance, fill = default)) +
  geom_boxplot())


(g2 <- Default %>%
  ggplot(aes(default, income, fill = default)) +
  geom_boxplot())

g1 + g2 + plot_layout(guides = "collect")


Default %>%
  ggplot(aes(balance, income, color = default)) +
  geom_point(alpha = .4)


Default %>%
  ggplot(aes(balance, income, color = default)) +
  geom_point(alpha = .4) +
  facet_grid(~ student)



# regressão logística -----------------------------------------------------

fit <- glm(default ~ balance + student, data = Default, family = "binomial")

summary(fit)
# -------------------------------------------------------------------------

library(rsample)
set.seed(123)
splits <- initial_split(Default, prop = .7, strata = default)
tr <- training(splits)
test <- testing(splits)

tr |> count(default)
test |> count(default)

fit_tr <- glm(default ~ balance + student, data = tr, family = "binomial")
summary(fit_tr)

prob <- predict(fit, test, type = "response")
summary(prob)

(roc_log <- roc(test$default, prob))
coords(roc_log, c(.2, .5, .8),
       ret = c("threshold", "accuracy", "specificity",
               "sensitivity", "npv", "ppv"))

# curva ROC
coords(roc_log, seq(0, 1, 0.0005), ret = c("threshold", "specificity", "sensitivity")) %>%
as_tibble() %>%
ggplot(aes(1 - specificity, sensitivity)) +
geom_step(direction = "vh")

tibble(classe = test$default, marcador = prob) %>%
roc_curve(classe, marcador, event_level = "second") %>%
autoplot()
