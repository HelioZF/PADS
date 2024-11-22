# O exemplo que será apresentado a seguir faz parte do livro Elements of 
# Statistical Learning.
# 
# Normalized handwritten digits, automatically scanned from envelopes by 
# the U.S. Postal Service. The original scanned digits are binary and of 
# different sizes and orientations; the images here have been deslanted 
# and size normalized, resulting in 16 x 16 grayscale images (Le Cun et al., 1990).
# 
# The data are in two gzipped files, and each line consists of the 
# digit id (0-9) followed by the 256 grayscale values.
# 
# There are 7291 training observations and 2007 test observations, distributed as 
# follows: 0 1 2 3 4 5 6 7 8 9 
# Total Train 1194 1005 731 658 652 556 664 645 542 644 7291 
# Test 359 264 198 166 200 160 170 147 166 177 2007
# 
# or as proportions: 0 1 2 3 4 5 6 7 8 9 
# Train 0.16 0.14 0.1 0.09 0.09 0.08 0.09 0.09 0.07 0.09 
# Test 0.18 0.13 0.1 0.08 0.10 0.08 0.08 0.07 0.08 0.09
# 
# Alternatively, the training data are available as separate files per 
# digit (and hence without the digit identifier in each row)
# 
# The test set is notoriously “difficult”, and a 2.5% error rate is 
# excellent. These data were kindly made available by the neural network 
# group at AT&T research labs (thanks to Yann Le Cunn).

library(tidyverse)
library(tidymodels)
library(keras)



# leitura dos dados -------------------------------------------------------

tr    <- read.table("../dados/zip.train", header = FALSE) 
teste <- read.table("../dados/zip.test", header = FALSE) 


x_tr <- tr[,-1] %>% 
  as.matrix()

y_tr <- to_categorical(tr$V1)


x_test <- teste[, -1] %>% 
  as.matrix()

y_test <- to_categorical(teste$V1)



# rede neural -------------------------------------------------------------

rm(network)

network <- keras_model_sequential() %>% 
  layer_dense(units = 100, activation = "tanh", input_shape = ncol(x_tr)) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

network

network %>% 
  compile(optimizer = "rmsprop", 
          loss = "categorical_crossentropy", 
          metrics = c("accuracy"))

network %>% 
  fit(x_tr, y_tr, epochs = 80, batch_size = 16)



# avaliação ---------------------------------------------------------------

network %>% 
  evaluate(x_test, y_test)

(y_hat <- network %>% 
  predict(x_test) %>% 
  k_argmax() %>% 
  as.vector())

# matriz de previsão
predict(network, x_test)


table(y_hat, teste$V1)


