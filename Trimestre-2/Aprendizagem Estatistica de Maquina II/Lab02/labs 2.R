load("amostra_drugs.RData")

library(tidyverse)
library(ggplot2)
library(kableExtra)
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)

## ----glimpse------------------------------------------------------------------
glimpse(data)


## ----top_conditions-----------------------------------------------------------
data %>% 
  group_by(condition) %>% 
  tally() %>% 
  top_n(10) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(condition,n), y=n))+
  geom_col()+
  coord_flip()+
  labs(x="Condition", y="")


## ----top_drug-----------------------------------------------------------------
data %>% 
  group_by(drugName) %>% 
  tally() %>% 
  top_n(10) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(drugName,n), y=n))+
  geom_col()+
  coord_flip()+
  labs(x="Drug name", y="")


## ----review-------------------------------------------------------------------
data %>% 
  select(condition, review) %>% 
  top_n(100) %>% 
  kable(format = 'html') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = TRUE, position = "center") %>%
  scroll_box(height = "250px")


# ----Remover tatgs html no texto ------------------------------------------

data <- data %>% 
  mutate(review = map_chr(review, 
                          ~xml2::xml_text(xml2::read_html(paste0("<x>", .x, "</x>")))))

data %>% 
  select(condition, review) %>% 
  top_n(100) %>% 
  kable(format = 'html') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = TRUE, position = "center") %>%
  scroll_box(height = "250px")


## ----corpus-------------------------------------------------------------------
corpus <- VCorpus(VectorSource(data$review), 
              readerControl = list(language = "english"))


## ----tm-----------------------------------------------------------------------
corpus <- corpus %>% 
  tm_map(content_transformer(tolower)) %>%  # minusculas
  tm_map(stripWhitespace) %>%         # remove espaços em branco
  tm_map(removePunctuation) %>%       # remove pontuacao
  tm_map(removeWords, stopwords("english"))   # remove stopwords
  # tm_map(stemDocument)                # stemiza documento (nao vou stemizar para fazer o match de palavras)



## ----nuvem, fig.width=7-------------------------------------------------------
library(wordcloud)
wordcloud(corpus, max.words = 60, random.order = FALSE,
          rot.per = 0, colors = brewer.pal(8, "Blues"))


## ----dtm, message=FALSE-------------------------------------------------------
dtm <- DocumentTermMatrix(corpus)

dtm

inspect(dtm)

reviews <- tidy(dtm)


## ----merge--------------------------------------------------------------------
# qual o objetivo do merge abaixo?
data_merge <- data %>% 
  mutate(document = as.character(1:nrow(data))) %>% 
  left_join(reviews) %>% 
  select(document, term, count, condition, drugName, rating, usefulCount)


par(mar=rep(0,4))

data_merge %>% 
  filter(condition=="Birth Control") %>% 
  group_by(term) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  top_n(100, wt = n) %>% 
  with(wordcloud(term, n, random.order = FALSE, rot.per = 0, 
       colors = brewer.pal(8, "Blues")))

data_merge %>% 
  filter(condition=="Depression") %>% 
  group_by(term) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  top_n(100, wt = n) %>% 
  with(wordcloud(term, n, random.order = FALSE, rot.per = 0, 
       colors = brewer.pal(8, "Blues")))

data_merge %>% 
  filter(condition=="Anxiety") %>% 
  group_by(term) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  top_n(100, wt = n) %>% 
  with(wordcloud(term, n, random.order = FALSE, rot.per = 0, 
       colors = brewer.pal(8, "Blues")))

data_merge %>% 
  filter(condition=="Pain") %>% 
  group_by(term) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  top_n(100) %>% 
  with(wordcloud(term, n, random.order = FALSE, rot.per = 0, 
       colors = brewer.pal(8, "Blues")))

reviews %>% 
  group_by(term) %>% 
  summarise(count = sum(count)) %>% 
  top_n(100, wt = count) %>% 
  with(wordcloud(term, count, random.order = FALSE, rot.per = 0, 
       colors = brewer.pal(8, "Blues")))


## ----Sentimentos-----------------------------------------------------------------
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


## ----obtendo sentimendo de nrc----------------------------------------------------------------------
get_sentiments("nrc") %>% 
  select(sentiment) %>% 
  unique()


## ----avaliacao segundo nrc-----------------------------------------------

top_conditions <- data %>% 
  group_by(condition) %>% 
  tally() %>%
  top_n(6) %>% 
  select(condition) %>% 
  pull()

sentimentos_bing <- data_merge %>% 
  select(document, word=term, count, condition) %>% 
  filter(condition %in% top_conditions) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(document, sentiment, condition) %>% 
  pivot_wider(names_from=sentiment, values_from=n, values_fill=0) %>% 
  mutate(sentiment = positive-negative)


## ----historico_sentimento-----------------------------------------------------
# quando este grafico faz sentido?
sentimentos_bing %>% 
  ggplot(aes(document, sentiment, fill=condition))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~condition, ncol=2, scales="free_x")


## ----boxplot----------------------------------------------------------------
sentimentos_bing %>% 
  ggplot(aes(x=sentiment, y=condition, fill=condition))+
  geom_vline(xintercept = 0, linetype="dashed", col="blue")+
  scale_fill_brewer(palette = "Dark2")+
  geom_boxplot(show.legend = FALSE)+
  coord_flip()

sentimentos_bing %>% 
  ggplot(aes(x=sentiment, fill=condition))+
  geom_density(show.legend = FALSE, alpha=0.7)+
  geom_vline(xintercept = 0, color="gray", linetype="dashed")+
  scale_fill_brewer(palette = "Dark2")+
  facet_wrap(~condition)+
  labs(x="Sentimento", y="Densidade")


## ----afinn--------------------------------------------------------------------
sentimentos_afinn <- data_merge %>% 
  select(document, word=term, count, condition) %>% 
  filter(condition %in% top_conditions) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(document, condition) %>% 
  summarise(sentiment = sum(count*value))

sentimentos_afinn %>% 
  ggplot(aes(document, sentiment, fill=condition))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~condition, ncol=2, scales="free_x")
  
sentimentos_afinn %>% 
  ggplot(aes(x=sentiment, y=condition, fill=condition))+
  geom_vline(xintercept = 0, linetype="dashed", col="blue")+
  scale_fill_brewer(palette = "Dark2")+
  geom_boxplot(show.legend = FALSE)+
  coord_flip()

sentimentos_afinn %>% 
  ggplot(aes(x=sentiment, fill=condition))+
  geom_density(show.legend = FALSE, alpha=0.7)+
  geom_vline(xintercept = 0, color="gray", linetype="dashed")+
  scale_fill_brewer(palette = "Dark2")+
  facet_wrap(~condition)+
  labs(x="Sentimento", y="Densidade")
