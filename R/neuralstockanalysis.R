##############################################################
# Rede Neural para Previsoes de Stock Market                 #
# (c) Rodrigo Almeida - rodrigo.almeida@gmail.com            #
#                                                            #
# Esse codigo tem por objetivo experimentar aplicação de     #
# redes neurais para  a previsao de compra e venda           #
# de ativos.                                                 #
##############################################################

library(keras)
library(tensorflow)
library(dplyr)
library(quantmod)



##### VARIAVEIS IMPORTANTES ###
acoes <- c("BBDC4.SA","RADL3.SA","LAME4.SA","BRKM5.SA","OIBR4.SA",
           "PETR4.SA","TIMP3.SA","BBAS3.SA","USIM5.SA","VALE5.SA")

acao <- acoes[8]
inicio <- "2017-01-12"
fim <- "2018-01-11"

################################
# Recuperando Dados da Bovespa #
################################

# RECUPERANDO DADOS DO ATIVO

stockData <- na.omit(getSymbols(acao, src = "yahoo", from = inicio, to = fim, auto.assign = FALSE))

df_stock <- as.data.frame(stockData)

# Usando valor de Fechamento como Referencia
s_futuro <- as.data.frame(df_stock[2:nrow(df_stock),4])
s_passado <- as.data.frame(df_stock[1:nrow(df_stock)-1,4])

df_combinado <- cbind(s_futuro, s_passado)
colnames(df_combinado) <- c("futuro","passado")
df_combinado <- mutate(df_combinado, pred = as.numeric(s_futuro > s_passado))

# Trabalhando com a coluna Close
# df <- as.data.frame(df_stock[,4])

# removendo ultima linha do df (para merge com pred)
df <- df_stock[1:nrow(df_stock)-1,]

# adicionando coluna pred ao df
df$pred <- df_combinado$pred

# Trabalhando com a coluna Close
features <- df[,4]
y <- df$pred
y <- as.data.frame(y)

n = nrow(df)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)

train_x = features[trainIndex ,]
train_y = y[trainIndex ,]
test_x = features[-trainIndex ,]
test_y = y[-trainIndex ,]

train_y <- to_categorical(train_y, 2)
test_y <- to_categorical(test_y, 2)

train_x <- as.matrix(train_x)
train_y <- as.matrix(train_y)
test_x <- as.matrix(test_x)
test_y <- as.matrix(test_y)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 32, activation = 'relu', input_shape = c(4)) %>%
  layer_dropout(rate = 0.9) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.7) %>%
  layer_dense(units = 2, activation = 'softmax')



model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  #optimizer = "adadelta",
  metrics = c('accuracy')
)


history <- model %>% fit(
  train_x, train_y,
  epochs = 100, batch_size = 256,
  validation_split = 0.2
)


model %>% evaluate(test_x, test_y)
