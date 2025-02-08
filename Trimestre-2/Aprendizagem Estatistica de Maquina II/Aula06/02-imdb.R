# Aprendizagem Estatistica de Maquina II 
# Aula 7
# Rede neural convolucional para classificacao
# Dados: review de filmes no imdb

# Pacotes utilizados ------------------------------------------------------
library(keras) # rede neural
library(Matrix) # representacao de matriz esparca
library(tidyverse)
library(glmnet)

# Dados IMDB --------------------------------------------------------------
max_features <- 10000
imdb <- dataset_imdb(num_words = max_features)

# c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb

x_train <- imdb$train$x
y_train <- imdb$train$y

x_test <- imdb$test$x
y_test <- imdb$test$y

x_train[[1]][1:12]

word_index <- dataset_imdb_word_index()

# funcao que decodifica uma lista de indices em palavras
decode_review <- function(text, word_index) {
  word <- names(word_index)
  idx <- unlist(word_index, use.names = FALSE)
  word <- c("<PAD>", "<START>", "<UNK>", "<UNUSED>", word)
  idx <- c(0:3, idx + 3)
  words <- word[match(text, idx, 2)]
  paste(words, collapse = " ")
}

decode_review(x_train[[1]], word_index)



one_hot <- function(sequences, dimension) {
  seqlen <- sapply(sequences, length)
  n <- length(seqlen)
  rowind <- rep(1:n, seqlen)
  colind <- unlist(sequences)
  sparseMatrix(i = rowind, j = colind,
               dims = c(n, dimension))
}

# representa os dados de treino e teste com one-hot-enconding
x_train_1h <- one_hot(x_train, 10000)
x_test_1h <- one_hot(x_test, 10000)

dim(x_train_1h)

nnzero(x_train_1h) / (25000 * 10000)

set.seed(3)
id_validacao <- sample(seq(along = y_train), 2000)


# Regressao Logistica -----------------------------------------------------

fit_lasso <- glmnet(x_train_1h[-id_validacao, ],
                    y_train[-id_validacao],
                family = "binomial", standardize = FALSE)

class_lasso <- predict(fit_lasso, x_train_1h[id_validacao, ]) > 0

acuracia_lasso <- apply(class_lasso, 2, Metrics::accuracy,  
                        y_train[id_validacao] > 0)

par(mar = c(4, 4, 4, 4), mfrow = c(1, 1))
plot(-log(fit_lasso$lambda), acuracia_lasso) 

tibble(penalidade = -log(fit_lasso$lambda),
       acuracia = acuracia_lasso) %>% 
  ggplot(aes(penalidade, acuracia)) +
  geom_line() +
  geom_point() +
  labs(x = "Penalidade (-log(lambda))", y = "Acuracia") +
  theme_bw()

# Rede Neural LSTM --------------------------------------------------------

wc <- sapply(x_train, length)
median(wc)
sum(wc <= 500) / length(wc)

maxlen <- 500
x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test <- pad_sequences(x_test, maxlen = maxlen)

dim(x_train)
dim(x_test)
x_train[1, 490:500]

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy", metrics = c("acc"))

summary(model)

history <- model %>% 
  fit(x_train, y_train, epochs = 10,
      batch_size = 128, validation_data = list(x_test, y_test))

plot(history)

predy_lstm <- predict(model, x_test) > 0.5

(accuracia_lstm <- mean(abs(y_test == as.numeric(predy_lstm))))

table(predy_lstm, y_test)

# Comparativo
max(acuracia_lasso)
accuracia_lstm
