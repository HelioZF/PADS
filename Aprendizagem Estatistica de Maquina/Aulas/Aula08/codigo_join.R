library(tidyverse)
library(janitor)
library(plotly)
library(geobr)

idh <- read.csv2("Aprendizagem Estatistica de Maquina/Aulas/Aula08/idh.csv")

skimr::skim(idh)

idh <- read.csv2("../dados/idh.csv") %>% 
  janitor::clean_names()

skimr::skim(idh)

renda <- read.csv2("../dados/renda.csv") %>% 
  janitor::clean_names()

saude <- read.csv2("../dados/saude_mun.csv") %>% 
  janitor::clean_names()




# Agrupe esses bancos de dados considerando apenas dados de 2010 e --------

dados <- idh %>% 
  filter(periodos == 2010) %>% 
  left_join(renda %>% 
              filter(periodos == 2010), by = c("cod_ibge", "periodos", "localidades")) %>% 
  left_join(saude %>% 
              filter(periodos == 2010), by = c("cod_ibge", "periodos", "localidades"))

dados %>% 
  slice_max(renda_per_capita_censo_demografico_em_reais_correntes, n = 10) %>% 
  ggplot(aes(renda_per_capita_censo_demografico_em_reais_correntes, 
             reorder(localidades, renda_per_capita_censo_demografico_em_reais_correntes))) + 
    geom_col(fill = "purple") + 
    labs(x = "renda per capta (reais correntes)", y = NULL) +
    scale_x_continuous(labels = scales::comma_format(big.mark = ".")) +
    theme_bw() + 
    theme(text = element_text(size = 14))



# mapas -------------------------------------------------------------------

# https://github.com/ipeaGIT/geobr

(mapa <- geobr::read_municipality(code_muni = "SP", showProgress = FALSE))

tibble(mapa)

(fig <- mapa %>% 
    left_join(dados, by = c("code_muni" = "cod_ibge")) %>% 
    ggplot(aes(fill = renda_per_capita_censo_demografico_em_reais_correntes)) + 
      geom_sf(color = "black", linewidth = .1) + 
      scale_fill_viridis_c() +
      labs(fill = "Renda per capta") +
      theme_void())

plotly::ggplotly(fig)
