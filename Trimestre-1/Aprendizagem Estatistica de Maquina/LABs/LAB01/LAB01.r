library(ggplot2)
library(tidyverse)
library(magrittr)
library(gapminder)

#Teste de Calculadora
1 + 1
2 * 7
1 / 2
2 ^ 2
10 - 1

7 == 6 + 1
7 == 6

#Variaveis
a <- 2
a <- 10
b <- 5 * 1
c <- "Bom Dia!"
d <- FALSE

(a <- 10) #Atribui o valor e printa no console

#Numeros Especiais
Inf + 1
Inf - 1
Inf - Inf #NaN - not a number
NA  #Missing Value

x <- c(1, 2, 3) #Criando um vetor (C=concatenate)w [lista]
x <- c(1.1, "casa", 3) #Neste caso todo o vetor é gerado como string
y <- 10:100
z <- seq(10, 100, by = 0.1)#Criando vetor de 10 a 100 com step de 0.1

class(a)
class(d)

#Representar VAriaveis categoricas/qualitativas
genero <- factor(c("Feminino", "Masculino"))
class(genero)

#Numeros Aleatorios
set.seed(42)

amostra <- sample(1:100, 10)

amostra[2]
amostra[2:3] <- c(4, 10)
amostra[c(2, 4)] <- c(4, 10)

amostra[c(9:length(amostra))] <- c(9, 10)

amostra[amostra > 5]

amostra[amostra > 5 | amostra < 20]

set.seed(42)
amostra1 <- sample(1:100, 10)
amostra2 <- sample(1:100, 10)

(amostra3 <- c(1, 2, 3, 4, 5))
(amostra3[6:length(amostra1)] <- c(0, 0, 0, 0, 0))

amostra1[length(amostra1)] <- 11
amostra1
amostra3
amostra1 + amostra3

#TidyVerse

amostra1 |> length()

#matrix
m1 <- matrix(1:20, nrow = 5, byrow = TRUE)
m1[1, 1]
m1[1, ]
m1[, 1]
m1[, c(1, 3)]

#data.frame
df <- data.frame(x = c("a", "b", "c", "d")) #Coluna de caracteres
num_aleat <- rnorm(4) #Coluna de numeros com dist normal
z <- runif(4) > 0.5 #logico | gera valores aleatorios se >0.5 True else False
n <- 1 #numeros

class(df)
names(df)

df[, "x"]
df$x

class(df$num_aleat)
df[c(1, 2), ]

dim(df)
nrow(df)
lenght(df)

#salvo como csv
write_csv(df, file = "df_exemplo.csv")

#fazer seleção de 2 linhas aleat do df
set.seed(42)
amostra <- sample(1:row(df), 2)
amostra

#ggplot
#grafico de linha
df |>
  ggplot(aes(x = n, y = num_aleat)) +
  geom_line() +
  geom_hline(yintercept = mean(df$num_aleat, color = "red")) +
  labs(title = "Grafico de linha", x = "coliuna n", y = "var num aleat")

#gapminder
dados <- gapminder

#filtrando com dplyr
dados |>
  filter(country == "Brazil") |>
  summary()
#anos x life exp brasil x noruega
dados |>
  filter(country == "Brazil" | country == "Norway") |>
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_point()

#expec media por pais
dados |>
  filter(country == "Brazil" | country == "Norway") |>
  group_by(country) |>
  summarise(media_lifeexp = mean(lifeExp))

#desafio 2
dados |>
  filter(lifeExp > 70 & continent == "Americas") |>
  summarise(meanGDP70 = mean(gdpPercap))

dados |>
  group_by(continent) |>
  ggplot(aes(x = year, y = lifeExp, color = continent)) +
  geom_point()
