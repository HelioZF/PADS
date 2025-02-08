# Instalar e carregar pacotes necessários
#install.packages(c("arrow", "tidyverse", "ggplot2"))

library(arrow)
library(tidyverse)
library(ggplot2)

# Importar o dataset correto
olist <- arrow::read_parquet(
  "https://github.com/padsInsper/202433-padsv/releases/download/dados/olist_items.parquet"
)

# Processar os dados usando a coluna 'types'
formas_pagamento <- olist %>%
  count(types) %>%               # Contar a frequência de cada tipo de pagamento
  filter(n > 100) %>%            # Manter apenas tipos com mais de 100 observações
  arrange(desc(n)) %>%           # Ordenar pela quantidade
  mutate(n_milhares = n / 1000)  # Converter para milhares

# Criar o gráfico ajustado

ggplot(formas_pagamento, aes(x = n_milhares, y = reorder(types, n_milhares))) +
  geom_col(fill = "#76EEC6") +  # Cor extraída com color picker
  geom_label(
    aes(x = n_milhares / 2, label = round(n_milhares, 2)),  # Posiciona o texto no centro da barra
    fill = "white",            # Caixa branca para os rótulos
    color = "black",           # Texto preto
    size = 5,                  # Tamanho do texto
    label.size = 0.5,          # Adiciona a borda preta na caixa
    label.r = unit(0.15, "lines") # Arredondar levemente as bordas
  ) +
  labs(
    title = "Formas de pagamento mais comuns",
    subtitle = "Considerando tipos com mais de 100 observações",
    x = "Quantidade\n(milhares)",
    y = "Forma de pagamento",
    caption = "Fonte: Olist"
  ) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),   # Fundo black extraído da imagem original
    panel.background = element_rect(fill = "#2E2E2E", color = NA),  # Painel cinza claro
    panel.grid.major = element_line(color = "gray70", size = 0.5), # Adiciona grid maior mais suave
    panel.grid.minor = element_line(color = "gray70", size = 0.25),# Adiciona grid menor mais suave
    text = element_text(color = "white", family = "serif"),         # Texto branco com fonte serifada
    axis.text = element_text(color = "white", size = 12),           # Texto dos eixos em branco e ajustado
    axis.title = element_text(color = "white", size = 14),
    plot.title = element_text(size = 24, face = "bold"),            # Título maior e em negrito
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(hjust = 1, size = 10),
    axis.ticks = element_blank()                                    # Remover ticks dos eixos
  ) +
  xlim(0, max(formas_pagamento$n_milhares) * 1.1)  # Espaço adicional para os rótulos


items_agg <- olist |>
  group_by(product_category_name) |>
  filter(n() > 4000) |>
  ungroup() |>
  filter(product_category_name != "ferramentas_jardim") |>
  mutate(
    product_category_name = stringr::str_replace_all(
      product_category_name, "_", " "
    ),
    product_category_name = stringr::str_to_title(product_category_name),
    product_category_name = fct_reorder(
      product_category_name, price, median
    ),
    relogios = ifelse(
      product_category_name == "Relogios Presentes",
      "destacar", "não destacar"
    )
  )

mediana <- items_agg |>
  summarise(mediana = median(price))

items_agg |>
  ggplot() +
  aes(x = price, y = product_category_name, fill = relogios) +
  ggridges::geom_density_ridges(
    quantile_lines = TRUE,
    quantiles = 2,
    na.rm = FALSE,
    n = 2048,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    labels = scales::dollar_format(prefix = "R$")
  ) +
  coord_cartesian(xlim = c(0, 300)) +
  geom_vline(
    aes(xintercept = mediana),
    data = mediana,
    linetype = 2,
    colour = "red"
  ) +
  scale_fill_manual(
    values = c("#6686e6", "#eaeaea")
  ) +
  theme_minimal() +
  annotate(
    "text",
    x = mediana$mediana,
    y = 0.8,
    hjust = -0.1,
    label = "Mediana",
    colour = "red",
    size = 3
  ) +
  labs(
    x = "Preço",
    y = "Categoria",
    title = "Relógios são caros!!"
  ) +
  theme(
    plot.title.position = "plot"
  )