#Carregando bibliotecas
library(tidyverse)
library(gapminder)

# Exercicio 1 -----------------------------------
vetor <- c(15, 10, 21, 30, 52, 60)
# A
media_vetor <- mean(vetor)
print(media_vetor)
# B
std_vetor <- sd(vetor)
print(std_vetor)
# C
val_ter_pos <- vetor[3]
print(val_ter_pos)
# D
comprimento_vetor <- length(vetor)
print(comprimento_vetor)

# Exercicio 2 -----------------------------------
fibonacci <- function(n) {
  if (n == 1) {
    return(0)
  } else if (n == 2) {
    return(c(0, 1))
  }
  fib_vec <- numeric(n)
  fib_vec[1] <- 0
  fib_vec[2] <- 1

  for (i in 3:n) {
    fib_vec[i] <- fib_vec[i-1] + fib_vec[i-2]
  }
  return(fib_vec)
}

fibonacci(n = 10)
fibonacci(n = 5)
fibonacci(n = 20)

# Exercicio 3 -----------------------------------
id <- c(1:10)
idade <- c(35, 24, 31, 29, 20, 19, 42, 54, 49, 60)
categoria <- c("tecnologia", "romance", "ficção", "tecnologia", "tecnologia",
               "tecnologia", "ficção", "romance", "tecnologia", "ficção")
satisfacao <- c(70, 80, 93, 79, 95, 88, 85, 91, 100, 79)
data_frame <- tibble(id, idade, categoria, satisfacao)
print(data_frame)

# A
num_line <- nrow(data_frame)
num_line

num_col <- ncol(data_frame)
num_col

# B
vec_idade <- data_frame$idade
vec_idade

# C
new_obj <- data_frame[1:2, ]
new_obj

# D
idade_media <- mean(vec_idade)
idade_media

# E
vec_satisfacao <- data_frame$satisfacao
mediana_satisfacao <- median(vec_satisfacao)
mediana_satisfacao

# F
contagem_categoria <- table(data_frame$categoria)
contagem_categoria

# G
idade_media_por_categoria <- data_frame |>
  group_by(categoria) |>
  summarise(media_idade = mean(idade))
idade_media_por_categoria

# H
data_frame |>
  ggplot(aes(idade, satisfacao)) +
  geom_point()

# I
data_frame |>
  ggplot(aes(x = idade, y = satisfacao, color = categoria)) +
  geom_point()

# J
data_frame |>
  ggplot(aes(x = idade, y = satisfacao, color = categoria)) +
  geom_point() +
  geom_smooth(method = "lm")

data_frame |>
  ggplot(aes(x = idade, y = satisfacao, color = categoria)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Exercicio 4 -----------------------------------
diamonds_data_frame <- diamonds
diamonds_data_frame

# A
freq_color <- diamonds_data_frame |>
  count(color, sort = TRUE)
freq_color

# B
freq_cut_color <- diamonds_data_frame |>
  count(cut, color)
freq_cut_color

# C
diamonds_ratio_df <- diamonds_data_frame |>
  mutate(ratio = price / table)
diamonds_ratio_df

# D
diamonds_data_frame |>
  group_by(cut) |>
  summarise(mean = mean(price), std = sd(price))

# E
diamonds_data_frame |>
  filter(table > 50) |>
  group_by(cut) |>
  summarise(mean = mean(price), std = sd(price))

# F
diamonds_data_frame |>
  group_by(cut) |>
  summarise(mean_price = mean(price)) |>
  ggplot(aes(x = cut, y = mean_price, fill = cut)) +
  geom_col()

# G
diamonds_data_frame |>
  ggplot(aes(x = price, y = depth)) +
  geom_point()

# H
diamonds_data_frame |>
  ggplot(aes(x = price, y = depth)) +
  geom_point() +
  facet_grid(~ cut)

# Exercicio 5 -----------------------------------
# biblioteca carregada no inicio do codigo
head(gapminder)
# A
nrow(gapminder)

# B
gapminder |>
  distinct(year)
unique(gapminder$year)

# C
gapminder |>
  group_by(continent) |>
  summarise(n_obs = n())

# D
gapminder |>
  group_by(continent, year) |>
  summarise(n_obs = n())

# E
gapminder |>
  filter(year >= 2000) |>
  group_by(continent, year) |>
  summarise(n_obs = n())

# F
gapminder |>
  filter(year >= 2000) |>
  group_by(continent, year) |>
  summarise(mean_gdp = mean(gdpPercap), sd_gdpPercap = sd(gdpPercap))

# G
gapminder |>
  ggplot(aes(x = lifeExp, y = year, group = country, color = country)) +
  geom_line() +
  # Remove a legenda para facilitar a visualização
  theme(legend.position = "none")

# H
gapminder |>
  group_by(continent, year) |>
  summarise(mean_life_exp = mean(lifeExp))

# I
gapminder |>
  group_by(continent, year) |>
  summarise(mean_life_exp = mean(lifeExp)) |>
  ggplot(aes(x = mean_life_exp, y = year, 
             group = continent, color = continent)) +
  geom_line()
