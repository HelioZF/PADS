library(arrow)
library(tidyverse)
library(ggplot2)
install.packages("ggthemr")  # Se ainda não tiver instalado
library(ggthemr)
library(dplyr)


# Importar o dataset correto
olist <- arrow::read_parquet(
  "https://github.com/padsInsper/202433-padsv/releases/download/dados/olist_items.parquet"
)

#### Relação entre Preço e Frete
## filtrando para price <= 5000 e frete abaixo de 350
olist_filtered <- olist %>% filter(price <= 5000 , freight_value <= 350)

ggplot(olist_filtered, aes(x = price, y = freight_value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relação entre Preço e Frete", x = "Preço do Produto", y = "Valor do Frete") +
  theme_minimal()


#### Impacto do Peso do Produto no Frete filtrando para casos < 28000 g
# Remover pesos acima de 28.000g e frete abaixo de 301
olist_filtered <- olist %>% filter(product_weight_g <= 28000 , freight_value <= 301)

# Replotar gráfico de dispersão
ggplot(olist_filtered, aes(x = product_weight_g, y = freight_value)) +
  geom_point(alpha = 0.5, color = "black") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relação entre Peso do Produto e Frete (Sem Outliers)",
       x = "Peso do Produto (g)", 
       y = "Valor do Frete") +
  theme_minimal()