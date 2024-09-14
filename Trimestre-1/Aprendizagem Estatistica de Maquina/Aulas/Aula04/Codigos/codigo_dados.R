library(tidyverse)
library(readxl)

theme_set(theme_bw())

# leitura dos dados

dados <- read_xlsx("dados.xlsx")

dados


dados %>% 
  ggplot(aes(x1, y1)) + 
    geom_point() + 
    geom_smooth(method = "lm")


dados %>% 
  ggplot(aes(x1, y1, color = x2)) + 
    geom_point() + 
    geom_smooth(method = "lm")


dados %>% 
  ggplot(aes(x1, y2, color = x2)) + 
    geom_point() + 
    geom_smooth(method = "lm")

