library(tidyverse)
library(readxl)
library(stringi)
library(factoextra)
library(ggrepel)

dados <- read_xlsx("produtividade_policial.xlsx")

dados %>% 
  sample_n(10)

dados <- dados %>% 
  rename_with(~ stri_trans_general(.x, "Latin-ASCII") %>% 
                tolower())

# #1 ----------------------------------------------------------------------

dados <- dados %>%
  pivot_wider(names_from = ocorrencia, values_from = total, values_fill = 0) %>%
  group_by(regiao) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

dados <- dados %>% 
  select(-jan, -fev, -mar, -abr, -mai, -jun, -jul, -ago, -set, -out, -nov, -dez)
print(dados)


# #2 ----------------------------------------------------------------------


dados_num <- dados %>% select(where(is.numeric))

dados_pca <- prcomp(dados_num, scale. = TRUE)
fviz_eig(dados_pca, addlabels = TRUE, ncp = ncol(dados_num)) +
  labs(x = "Componente Principal", y = "Percentual de Variância Explicada")

variancia_acumulada <- cumsum(dados_pca$sdev^2 / sum(dados_pca$sdev^2))
variancia_acumulada
#como podemos notar teremos mais de 80% da variancia dos dados a partir de duas colunes do dataframe reduzido


# #3 ----------------------------------------------------------------------
cargas <- dados_pca$rotation[, 1]
cargas
contribuicoes <- 100 * (cargas^2) / sum(cargas^2)
contribuicoes_df <- tibble(
  Variavel = names(contribuicoes),
  Contribuicao = contribuicoes
) %>%
  arrange(desc(Contribuicao))

print(contribuicoes_df)

fviz_contrib(dados_pca, choice = "var", axes = 1, top = 10) +
  labs(title = "Primeira Componente Principal")

# #4 ----------------------------------------------------------------------

cargas_segunda <- dados_pca$rotation[,2]
contribuicoes_segunda <- 100 * (cargas_segunda^2) / sum(cargas_segunda^2)

contribuicoes_segunda_df <- tibble(
  Variavel = names(contribuicoes_segunda),
  Contribuicao = contribuicoes_segunda
) %>%
  arrange(desc(Contribuicao))

print(contribuicoes_segunda_df)

fviz_contrib(dados_pca, choice = "var", axes = 2, top = 10) +
  labs(title = "Segunda Componente Principal")
#5 
colnames(dados_pca$x) <- c("Repressão Policial", "Crimes com armas e drogas")

scores <- as_tibble(dados_pca$x) %>%
  select(PC1, PC2) %>%
  mutate(Regiao = dados$regiao)
colnames(scores) <- c("Repressão Policial", "Crimes com armas e drogas", "Regiao")

ggplot(scores, aes(x = `Repressão Policial`, y = `Crimes com armas e drogas`, label = Regiao)) +
  geom_point(aes(color = Regiao), size = 3) +
  geom_text_repel() +
  labs(title = "Distribuição das Regiões nas Duas Primeiras Componentes Principais",
       x = "Repressão Policial",
       y = "Crimes com armas e drogas") +
  theme_minimal()
 
#Pelo grafico podemos notar que na capital temos alta dominancia de crimes envolvendo armas e drogas e quase sem repressão policial
# Em ribeirão Pretro podemos notar que nao ha criminalidade (negativa) e igualmente a repressao policial e negativa, quase zero
# o que provavelmente quer dizer que e uma cidade com baixa criminalidade e baixa repressao policial
# Em Sorocaba podemos ver que ha uma quantidade significativa de criminalidade mas tambem há uma repressao policial significativa


# #6 ----------------------------------------------------------------------
#6.1
set.seed(123)
dados_kmeans <- scores %>% select(`Repressão Policial`, `Crimes com armas e drogas`)
num_clusters <- 1:10
fviz_nbclust(dados_kmeans, kmeans, method = "wss") +
  labs(title = "Método do Cotovelo para ver numero otimo Clusters",
       x = "Número de Clusters",
       y = "Soma das Distâncias Quadradas")
# pelo visto temos que o numero otimo e de aproximadamente 7 (ha mum decrescimo significativo entre 6 e 7)

#6.2
k<-4
# notamos com o grafico que 7 foi um valor muito alto, entao reduzimos para 4
kmeans_result <- kmeans(dados_kmeans, centers = k, nstart = 25)
scores <- scores %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))  

ggplot(scores, aes(x = `Repressão Policial`, y = `Crimes com armas e drogas`, color = Cluster, label = Regiao)) +
  geom_point(size = 3) +
  geom_text_repel() +
  labs(title = "Distribuição das Regiões nas Duas Primeiras Componentes Principais com Clusters",
       x = "Repressão Policial",
       y = "Crimes com armas e drogas") +
  theme_minimal() +
  scale_color_manual(values = rainbow(k)) 

#6.3
# Podemos notar que os 4 grupos gerados, se diferenciam principalmente entre os com alta repressao e alta cirminalidade (2),
# baixa criminalidade e baixa repressão(1), e os que possuem um valor intermediario de ambas (3,4)

#6.4
#Podemos realizar uma realocação das forças de segurança publica, dads cidades com o cluster em vermelho para as com o cluster verde. 
# Além disso podemos notar que as cidades com maior indice de criminalidade também são as com menor repressão policial, podendo assim
# indicar a presença de mílicias, o que dificuta o trabalho policial por ter corrupção interna. Assim sendo necessita-se de uma maior 
# fiscalização tanto nas ruas quanto internamente na policia para garantri um melhor funcionamento das politicas de segurança publica

