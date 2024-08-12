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
