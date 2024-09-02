# Lista dos pacotes necessários
required_packages <- c("tidyverse", "pROC", "ggplot2", "dplyr", "gridExtra")

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

# Carregando as bibliotecas
library(tidyverse)
library(pROC)
library(ggplot2)
library(dplyr)
library(gridExtra)

dados <- read.csv("Aprendizagem Estatistica de Maquina/Analise de Dados/Analise01/sao-paulo-properties-april-2019.csv", sep = ",") # nolint
head(dados)

# a) Calcule a frequência absoluta e relativa de cada categoria da variável “Negotiation.Type”.  # nolint
(freq_abs <- table(dados$Negotiation.Type))
(freq_rlt <- freq_abs / length(dados$Negotiation.Type))

# b) Faça  um  gráfico  de  dispersão  do  preço  do  condomínio  e  do  preço  anunciado. # nolint
#    Como  você descreveria esse tipo de relação?

dados %>%
  ggplot(aes(x = Condo, y = Price, color = Negotiation.Type)) +
  windows() +
  geom_point()

# Fica dificil notar com essa visualização a diferença entre os gráficos pois estamos # nolint
# comparando valores de aluguel com valores de venda de imóveis, assim sendo os valores # nolint
# de aluguel ficam sem nos informar nada.

# c) Faça o gráfico do item anterior considerando facetas de acordo com “Negotiation.Type” (rent ou sale).  # nolint
dados |>
  ggplot(aes(x = Condo, y = Price, color = Negotiation.Type)) +
  windows() +
  geom_point() +
  scale_color_manual(values = c("rent" = "red", "sale" = "blue")) +
  facet_wrap(~ Negotiation.Type, scales = "free") +
  theme_minimal() +
  labs(title = "Preço vs. Condomínio por Tipo de Negociação",
       x = "Condomínio",
       y = "Preço")

# Exibindo o gráfico
plot

# d) Crie uma tabela para identificar quais são os dez distritos com maior frequência nesse banco de  dados. # nolint
#    Utilize essa tabela para criar um gráfico de frequências ordenado de forma decrescente.  # nolint

# Criando tabela e encontrando os distritos
distritos <- dados |>
  count(District, sort = TRUE) |>
  head(10)
distritos

# Adicionando os Distritos na tabela
(top_districts$Distrito <- distritos)

# Criar o gráfico de barras com as frequências ordenadas de forma decrescente
ggplot(distritos, aes(x = reorder(District, -n), y = n)) +
  windows() +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Distritos por Frequência",
       x = "Distrito",
       y = "Frequência") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # Ajusta o ângulo e posicionamento dos rótulos #nolint
    plot.margin = margin(t = 10, r = 10, b = 50, l = 10) # Aumenta a margem inferior para evitar cortes nos textos #nolint
  )
  # Confesso que utilizei o chatGPT para fazer a legenda. #nolint

# e) Faça um filtro nos dados considerando apenas os dados de aluguel (Negotiation.Type == “rent”), #nolint
#    separe  os  dados  em  dois  conjuntos  (treinamento  e  teste)  e  avalie
#    o  erro  de  previsão  para  os seguintes modelos:

# i) regressão linear

# ii) regressão ridge

# iii) regressão LASSO

# iv) árvore de decisão

# v) floresta aleatória

# obs: A definição das preditoras utilizadas no modelo é de livre escolha






