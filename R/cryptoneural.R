# Rede Neural
library(keras)
library(tensorflow)
library(jsonlite)
library(dplyr)

cc <- fromJSON("https://min-api.cryptocompare.com/")

# Principais Criptomoedas (Para obter lista, ver codigo marketshare)
cryptocoins <- c("BTC","ETH","BCH","IOT","XRP","LTC")
cc_histoday_btc <- fromJSON("https://min-api.cryptocompare.com/data/histoday?fsym=BTC&tsym=USD&allData=true&e=CCCAGG")
cc_histoday_eth <- fromJSON("https://min-api.cryptocompare.com/data/histoday?fsym=ETH&tsym=USD&allData=true&e=CCCAGG")

df_btc <- as.data.frame(cc_histoday_btc)
df_eth <- as.data.frame(cc_histoday_eth)

df_btc <- df_btc[,c(4:10)]
df_eth <- df_eth[,c(4:10)]

colnames(df_btc) <- c("Data.time","Data.close.BTC","Data.high.BTC","Data.low.BTC","Data.open.BTC","Data.volumefrom.BTC", "Data.volumeto.BTC")
colnames(df_eth) <- c("Data.time","Data.close.ETH","Data.high.ETH","Data.low.ETH","Data.open.ETH","Data.volumefrom.ETH", "Data.volumeto.ETH")

# Em um primeiro momento, analisando somente ETH x BTC
df_btc <- df_btc %>% filter(Data.time >= df_eth$Data.time[1])

df <- merge(x = df_btc, y = df_eth, by = "Data.time")

s_futuro <- as.data.frame(df[2:nrow(df),"Data.close.ETH"])
s_passado <- as.data.frame(df[1:nrow(df)-1,"Data.close.ETH"])

df_combinado <- cbind(s_futuro, s_passado)
colnames(df_combinado) <- c("futuro","passado")
df_combinado <- mutate(df_combinado, pred = as.numeric(s_futuro > s_passado))

df <- mutate(df, high = Data.high.BTC / Data.high.ETH)
df <- mutate(df, low = Data.low.BTC / Data.low.ETH)
df <- mutate(df, open = Data.open.BTC / Data.open.ETH)
df <- mutate(df, close = Data.open.BTC / Data.close.ETH)

df <- df[,c(14:17)]

# removendo ultima linha do df
df <- df[1:nrow(df)-1,]

# adicionando coluna pred ao df
df$pred <- df_combinado$pred

features <- df[,c(1:4)]
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
  #optimizer = optimizer_rmsprop(),
  optimizer = "adadelta",
  metrics = c('accuracy')
)


history <- model %>% fit(
  train_x, train_y,
  epochs = 60, batch_size = 256,
  validation_split = 0.2
)


model %>% evaluate(test_x, test_y)
