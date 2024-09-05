library(survival)
library(survminer)

dados <- read_csv("Aprendizagem Estatistica de Maquina/Aulas/Aula08/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>%
    mutate(Churn = case_when(Churn == "Yes" ~ 1, Churn == "No" ~ 0)) %>%
        mutate_if(is.character, factor)

(fit <- survfit(Surv(tenure, Churn) ~ 1, dados))
summary(fit)


ggsurvplot(fit, risk.table = TRUE) + windows()

fit <- survfit(Surv(tenure, Churn) ~ gender, dados)
ggsurvplot(fit, risk.table = TRUE, conf.int = TRUE, pval = TRUE,
censor = FALSE) + windows() # fun = "event"
