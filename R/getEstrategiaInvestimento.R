##############################################################
# Analise Tecnica como Insumo para Previsoes de Stock Market #
# (c) Rodrigo Almeida - rodrigo.almeida@gmail.com            #
#                                                            #
# Esse codigo tem por objetivo experimentar estrategias de   #
# trading com base no historico de um ativo. E necessario    #
# compreender os objetivos de cada estrategia e selecionar   #
# a melhor estrategia para o ativo.                          #
# IMPORTANTE: nao avaliamos a correlacao entre variaveis.    #
##############################################################

# Lagging Indicators
# A lagging indicator is one that follows price movements and has less predictive qualities.
# The most well-known lagging indicators are the moving averages and Bollinger Bands®.
# The usefulness of these indicators tends to be lower during non-trending periods but highly useful
# during  trending periods. This is due to the fact that lagging indicators tend to focus more on the trend
# and produce fewer buy-and-sell signals.
# This allows the trader to capture more of the trend instead of being forced out of their position based
# on the volatile nature of the leading indicators.



getEstrategiaInvestimento <- function(COMISSAO = 0.01,
                                      acao = "PETR4.SA",
                                      inicio = "2017-01-12",
                                      fim = "2018-01-11")
{
# Carregando Pacotes
## Technical Trading Rules
library("TTR")
## Quantitative Financial Modeling Framework - Principal Biblioteca que Implementa os Metodos Financeiros
library("quantmod")
## Pacote para indicadores de Performance e Risk Analysis
library("PerformanceAnalytics")
## Pacote para trabalhar com Data
library("lubridate")

##### VARIAVEIS IMPORTANTES ###
  acoes <- c("BBDC4.SA","RADL3.SA","LAME4.SA","BRKM5.SA","OIBR4.SA",
             "PETR4.SA","TIMP3.SA","BBAS3.SA","USIM5.SA","VALE5.SA",
             "BBSE3.SA","UGPA3.SA","CMIG4.SA","ITSA4.SA",
             "SBSP3.SA","ENBR3.SA","EQTL3.SA","BBDC3.SA","JBSS3.SA",
             "CCRO3.SA","TBLE3.SA","PETR3.SA","MULT3.SA","BRML3.SA",
             "LREN3.SA","ECOR3.SA","PCAR4.SA","VIVT4.SA")
COMISSAO <- 0.01 # Comissao Sobre Operacoes
acao <- "PETR3.SA"
inicio <- "2017-01-12"
fim <- "2018-01-11"

################################
# Recuperando Dados da Bovespa #
################################

# RECUPERANDO DADOS DO ATIVO

stockData <- na.omit(getSymbols(acao, src = "yahoo", from = inicio, to = fim, auto.assign = FALSE))

##############################
# Stock Technical Indicators #
##############################

# Simple Moving Average
sma5 <- SMA(Cl(stockData),n=5)
sma21 <- SMA(Cl(stockData),n=21)

# Exponential Moving Average
ema5 <- EMA(Cl(stockData),n=5)
ema21 <- EMA(Cl(stockData),n=21)

# Bollinger Bands BB(20,2)
bb <- BBands(HLC(stockData),n=20,sd=2)

# Parabolic Stop and Reverse SAR(0.02,0.2)
sar <- SAR(cbind(Hi(stockData),Lo(stockData)),accel=c(0.02, 0.2))

######################################
# Leading Stock Technical Indicators #
######################################

# Average Directional Movement Index ADX(14)
adx <- ADX(HLC(stockData),n=14)

# Commodity Channel Index CCI(20,0.015)
cci <- CCI(HLC(stockData),n=20,c=0.015)

# Moving Averages Covergence/Divergence MACD(12,26,9)
macd <- MACD(Cl(stockData),nFast=12,nSlow=26,nSig=9)

# Rate Of Change ROC(21)
roc <- ROC(stockData,n=21)

# Relative Strength Index RSI(14)
rsi <- RSI(Cl(stockData),n=14)

# Stochastic Momentum Index SMI(13,2,25,9)
smi <- SMI(HLC(stockData),n=13,nFast=2,nSlow=25,nSig=9)

#  Williams %R(14)
wpr <- WPR(HLC(stockData),n=14)

#########################
# Stock Trading Signals #
#########################

### IMPORTANTE #######
# 1 para Compra
# 0 para manter Posicao
# -1 para Venda
#######################


# Single Indicator Trading Signals

#  Sinais ocorrem em momentos onde o preco das acoes cruzam alguns dos indicadores. Exemplo:
#  Price Crossover ocorrem quando o preco de fechamento das acoes cruzam a curva
#  Double Crossover quando as curvas se cruzam

## SMA(5 & 21) Price Crossover Trading Signals
#  Sinal de Compra: Previa(Fechamento < SMA5) -> Atual(Fechamento > SMA5)
#  Sinal de Venda: Previa(Fechamento > SMA5) -> Atual(Fechamento < SMA5)

sma5tr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sma5)&Cl(stockData)>sma5,1,ifelse(Lag(Cl(stockData))>Lag(sma5)&Cl(stockData)<sma5,-1,0)))
sma5tr[is.na(sma5tr)] <- 0
sma21tr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sma21)&Cl(stockData)>sma21,1,ifelse(Lag(Cl(stockData))>Lag(sma21)&Cl(stockData)<sma21,-1,0)))
sma21tr[is.na(sma21tr)] <- 0

## Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Signals
#  Sinal de Compra: Previa(EMA5 < EMA21) -> Atual(EMA5 > EMA21)
#  Sinal de Venda: Previa(EMA5 > EMA21) -> Atual(EMA5 < EMA21)

ema5tr <- Lag(ifelse(Lag(Cl(stockData))<Lag(ema5)&Cl(stockData)>ema5,1,ifelse(Lag(Cl(stockData))>Lag(ema5)&Cl(stockData)<ema5,-1,0)))
ema5tr[is.na(ema5tr)] <- 0
ema21tr <- Lag(ifelse(Lag(Cl(stockData))<Lag(ema21)&Cl(stockData)>ema21,1,ifelse(Lag(Cl(stockData))>Lag(ema21)&Cl(stockData)<ema21,-1,0)))
ema21tr[is.na(ema21tr)] <- 0

## Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Trading Signals
smatr <- Lag(ifelse(Lag(sma5)<Lag(sma21)&sma5>sma21,1,ifelse(Lag(sma5)>Lag(sma21)&sma5<sma21,-1,0)))
smatr[is.na(smatr)] <- 0
ematr <- Lag(ifelse(Lag(ema5)<Lag(ema21)&ema5>ema21,1,ifelse(Lag(ema5)>Lag(ema21)&ema5<ema21,-1,0)))
ematr[is.na(ematr)] <- 0


## Bollinger Bands BB(20,2)  Crossover Trading Signals
# Sinal de Compra: Previa(Fechamento < Lower Band) -> Atual(Fechamento > Lower Band)
# Sinal de Venda: Previa(Fechamento < Upper Band) -> Atual(Fechamento > Upper Band)

bbtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(bb[,1])&Cl(stockData)>bb[,1],1,ifelse(Lag(Cl(stockData))<Lag(bb[,3])&Cl(stockData)>bb[,3],-1,0)))
bbtr[is.na(bbtr)] <- 0

## Parabolic Stop And Reverse SAR(0.02,0.2) Trading Signals
# Sinal de Compra: Previa(Fechamento < SAR) -> Atual(Fechamento > SAR)
# Sinal de Venda: Previa(Fechamento > SAR) -> Atual(Fechamento < SAR)

sartr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sar)&Cl(stockData)>sar,1,ifelse(Lag(Cl(stockData))>Lag(sar)&Cl(stockData)<sar,-1,0)))
sartr[is.na(sartr)] <- 0


## Average Directional Movement Index ADX(14) Trading Signals
# Stock Trading: ADX(14) > +20
# Sinal de Compra: Previa(DI(14)p < DI(14)n) -> Atual(DI(14)p > DI(14)n) & ADX(14) > +20
# Sinal de Venda: Previa(DI(14)p > DI(14)n) -> Atual(DI(14)p < DI(14)n) & ADX(14) > +20
# ADX Value	Trend Strength
# 0-25	Absent or Weak Trend
# 25-50	Strong Trend
# 50-75	Very Strong Trend
# 75-100	Extremely Strong Trend
# Read more: ADX: The Trend Strength Indicator https://www.investopedia.com/articles/trading/07/adx-trend-indicator.asp#ixzz543YSQ8fm

adxtr <- Lag(ifelse(Lag(adx[,1])<Lag(adx[,2])&adx[,1]>adx[,2]&adx[,4]>20,1,ifelse(Lag(adx[,1])>Lag(adx[,2])&adx[,1]<adx[,2]&adx[,4]>20,-1,0)))
adxtr[is.na(adxtr)] <- 0

## Commodity Channel Index CCI(20,0.015) Trading Signals
# Sinal de Compra: Previa (CCI < -100) -> Atual(CCI > -100)
# Sinal de Venda: Previa (CCI < +100) -> Atual(CCI > +100)
# A CCI é calculada como a diferença entre Typical Price de um ativo e sua média móvel simples,
# dividido pelo desvio padrão do Typical Price.
# O valor 0,015 é apenas uma constante usada para que a maior parte dos valores fique dentro da faixa
# -100 a 100 e facilite a interpretação do indicador.
# CCI = (Typical Price – MMS(tp)) / (0,015 * Desvio Padrão (tp))
# Como vimos na fórmula, o CCI mede o nível de preço atual em relação a uma média de preços ao longo de um período n de tempo.
# Logo um CCI alto significa que os preços estão muito acima de sua média e um CCI baixo indica que os
# preços estão muito abaixo de sua média. Pela lógica, pode estar indicando tanto força da tendência,
# como também que o mercado está exagerando no movimento.

ccitr <- Lag(ifelse(Lag(cci)<(-100)&cci>(-100),1,ifelse(Lag(cci)<100&cci>100,-1,0)))
ccitr[is.na(ccitr)] <- 0


## Moving Averages Covergence/Divergence MACD(12,26,9) Trading Signals
#
#
smacdtr <- Lag(ifelse(Lag(macd[,1])<Lag(macd[,2])&macd[,1]>macd[,2],1,ifelse(Lag(macd[,1])>Lag(macd[,2])&macd[,1]<macd[,2],-1,0)))
smacdtr[is.na(smacdtr)] <- 0
cmacdtr <- Lag(ifelse(Lag(macd[,1])<0&macd[,1]>0,1,ifelse(Lag(macd[,1])>0&macd[,1]<0,-1,0)))
cmacdtr[is.na(cmacdtr)] <- 0


## Rate Of Change ROC(21) Trading Signals
roctr <- Lag(ifelse(Lag(roc[,4])<(-0.05)&roc[,4]>(-0.05),1,ifelse(Lag(roc[,4])<0.05&roc[,4]>0.05,-1,0)))
roctr[is.na(roctr)] <- 0


## Relative Strength Index RSI(14) Trading Signals
rsitr <- Lag(ifelse(Lag(rsi)<30&rsi>30,1,ifelse(Lag(rsi)<70&rsi>70,-1,0)))
rsitr[is.na(rsitr)] <- 0

## Stochastic Momentum Index SMI(13,2,25,9) Trading Signals
smitr <- Lag(ifelse(Lag(smi[,1])<Lag(smi[,2])&smi[,1]>smi[,2],1,ifelse(Lag(smi[,1])>Lag(smi[,2])&smi[,1]<smi[,2],-1,0)))
smitr[is.na(smitr)] <- 0

## Williams %R(14) Trading Signals
wprtr <- Lag(ifelse(Lag(wpr)>0.80&wpr<0.80,1,ifelse(Lag(wpr)>0.20&wpr<0.20,-1,0)))
wprtr[is.na(wprtr)] <- 0


# Multiple Indicators Trading Signals

## Commodity Channel Index CCI(20,0.015) and Simple Moving Average SMA(5) Trading Signals
ccismatr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sma5)&Cl(stockData)>sma5&cci<(-100),1,ifelse(Lag(Cl(stockData))>Lag(sma5)&Cl(stockData)<sma5&cci>100,-1,0)))
ccismatr[is.na(ccismatr)] <- 0

## Rate Of Change ROC(21) and Simple Moving Average SMA(5) Trading Signals
rocsmatr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sma5)&Cl(stockData)>sma5&roc[,4]<(-0.05),1,ifelse(Lag(Cl(stockData))>Lag(sma5)&Cl(stockData)<sma5&roc[,4]>0.05,-1,0)))
rocsmatr[is.na(rocsmatr)] <- 0


## Relative Strength Index RSI(14) and Simple Moving Average SMA(5) Trading Signals
rsismatr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sma5)&Cl(stockData)>sma5&rsi<30,1,ifelse(Lag(Cl(stockData))>Lag(sma5)&Cl(stockData)<sma5&rsi>70,-1,0)))
rsismatr[is.na(rsismatr)] <- 0


## Stochastic Momentum Index SMI(13,2,25,9) and Simple Moving Average SMA(5) Trading Signals
smismatr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sma5)&Cl(stockData)>sma5&smi[,1]<(-40),1,ifelse(Lag(Cl(stockData))>Lag(sma5)&Cl(stockData)<sma5&smi[,1]>40,-1,0)))
smismatr[is.na(smismatr)] <- 0


## Williams %R(14) and Simple Moving Average SMA(5) Trading Signals
wprsmatr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sma5)&Cl(stockData)>sma5&wpr>0.80,1,ifelse(Lag(Cl(stockData))>Lag(sma5)&Cl(stockData)<sma5&wpr<0.20,-1,0)))
wprsmatr[is.na(wprsmatr)] <- 0


############################
# Stock Trading Strategies #
############################

## Simple Moving Averages SMA(5 & 21) Price Crossover Trading Strategies
sma5pos <- ifelse(sma5tr>1,0,1)
for(i in 1:length(Cl(stockData))){sma5pos[i] <- ifelse(sma5tr[i]==1,1,ifelse(sma5tr[i]==-1,0,sma5pos[i-1]))}
sma5pos[is.na(sma5pos)] <- 1
sma5poscomp <- cbind(Cl(stockData),sma5,sma5tr,sma5pos)
colnames(sma5poscomp) <-c("Close",'sma5',"sma5tr","sma5pos")
sma21pos <- ifelse(sma21tr>1,0,1)
for(i in 1:length(Cl(stockData))){sma21pos[i] <- ifelse(sma21tr[i]==1,1,ifelse(sma21tr[i]==-1,0,sma21pos[i-1]))}
sma21pos[is.na(sma21pos)] <- 1
sma21poscomp <- cbind(Cl(stockData),sma21,sma21tr,sma21pos)
colnames(sma21poscomp) <-c("Close",'sma21',"sma21tr","sma21pos")


## Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Strategies
ema5pos <- ifelse(ema5tr>1,0,1)
for(i in 1:length(Cl(stockData))){ema5pos[i] <- ifelse(ema5tr[i]==1,1,ifelse(ema5tr[i]==-1,0,ema5pos[i-1]))}
ema5pos[is.na(ema5pos)] <- 1
ema5poscomp <- cbind(Cl(stockData),ema5,ema5tr,ema5pos)
colnames(ema5poscomp) <-c("Close",'ema5',"ema5tr","ema5pos")
ema21pos <- ifelse(ema21tr>1,0,1)
for(i in 1:length(Cl(stockData))){ema21pos[i] <- ifelse(ema21tr[i]==1,1,ifelse(ema21tr[i]==-1,0,ema21pos[i-1]))}
ema21pos[is.na(ema21pos)] <- 1
ema21poscomp <- cbind(Cl(stockData),ema21,ema21tr,ema21pos)
colnames(ema21poscomp) <-c("Close",'ema21',"ema21tr","ema21pos")


## Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Trading Strategies
smapos <- ifelse(smatr>1,0,1)
for(i in 1:length(Cl(stockData))){smapos[i] <- ifelse(smatr[i]==1,1,ifelse(smatr[i]==-1,0,smapos[i-1]))}
smapos[is.na(smapos)] <- 1
smaposcomp <- cbind(sma5,sma21,smatr,smapos)
colnames(smaposcomp) <-c("sma5",'sma21',"smatr","smapos")
emapos <- ifelse(ematr>1,0,1)
for(i in 1:length(Cl(stockData))){emapos[i] <- ifelse(ematr[i]==1,1,ifelse(ematr[i]==-1,0,emapos[i-1]))}
emapos[is.na(emapos)] <- 1
emaposcomp <- cbind(ema5,ema21,ematr,emapos)
colnames(emaposcomp) <-c("ema5",'ema21',"ematr","emapos")


## Bollinger Bands BB(20,2) Trading Strategy
bbpos <- ifelse(bbtr>1,0,1)
for(i in 1:length(Cl(stockData))){bbpos[i] <- ifelse(bbtr[i]==1,1,ifelse(bbtr[i]==-1,0,bbpos[i-1]))}
bbpos[is.na(bbpos)] <- 1
bbposcomp <- cbind(Cl(stockData),bb[,1],bb[,3],bbtr,bbpos)
colnames(bbposcomp) <-c("Close",'lower',"upper","bbtr","bbpos")

## Parabolic Stop And Reverse SAR(0.02,0.2) Trading Strategy
sarpos <- ifelse(sartr>1,0,1)
for(i in 1:length(Cl(stockData))){sarpos[i] <- ifelse(sartr[i]==1,1,ifelse(sartr[i]==-1,0,sarpos[i-1]))}
sarpos[is.na(sarpos)] <- 1
sarposcomp <- cbind(Cl(stockData),sar,sartr,sarpos)
colnames(sarposcomp) <-c("Close","sar","sartr","sarpos")

## Average Directional Movement Index ADX(14) Trading Strategy
adxpos <- ifelse(adxtr>1,0,1)
for(i in 1:length(Cl(stockData))){adxpos[i] <- ifelse(adxtr[i]==1,1,ifelse(adxtr[i]==-1,0,adxpos[i-1]))}
adxpos[is.na(adxpos)] <- 1
adxposcomp <- cbind(adx[,1],adx[,2],adx[,4],adxtr,adxpos)
colnames(adxposcomp) <-c("dip","din","adx","adxtr","adxpos")

## Commodity Channel Index CCI(20,0.015) Trading Strategy
ccipos <- ifelse(ccitr>1,0,1)
for(i in 1:length(Cl(stockData))){ccipos[i] <- ifelse(ccitr[i]==1,1,ifelse(ccitr[i]==-1,0,ccipos[i-1]))}
ccipos[is.na(ccipos)] <- 1
cciposcomp <- cbind(cci,ccitr,ccipos)
colnames(cciposcomp) <-c("cci","ccitr","ccipos")


## Moving Averages Covergence/Divergence MACD(12,26,9) Trading Strategies
smacdpos <- ifelse(smacdtr>1,0,1)
for(i in 1:length(Cl(stockData))){smacdpos[i] <- ifelse(smacdtr[i]==1,1,ifelse(smacdtr[i]==-1,0,smacdpos[i-1]))}
smacdpos[is.na(smacdpos)] <- 1
smacdposcomp <- cbind(macd[,1],macd[,2],smacdtr,smacdpos)
colnames(smacdposcomp) <-c("macd","signal","smacdtr","smacdpos")
cmacdpos <- ifelse(cmacdtr>1,0,1)
for(i in 1:length(Cl(stockData))){cmacdpos[i] <- ifelse(cmacdtr[i]==1,1,ifelse(cmacdtr[i]==-1,0,cmacdpos[i-1]))}
cmacdpos[is.na(cmacdpos)] <- 1
cmacdposcomp <- cbind(macd[,1],cmacdtr,cmacdpos)
colnames(cmacdposcomp) <-c("macd","cmacdtr","cmacdpos")


## Rate Of Change ROC(21) Trading Strategy
rocpos <- ifelse(roctr>1,0,1)
for(i in 1:length(Cl(stockData))){rocpos[i] <- ifelse(roctr[i]==1,1,ifelse(roctr[i]==-1,0,rocpos[i-1]))}
rocpos[is.na(rocpos)] <- 1
rocposcomp <- cbind(roc[,4],roctr,rocpos)
colnames(rocposcomp) <-c("roc","roctr","rocpos")

## Relative Strength Index RSI(14) Trading Strategy
rsipos <- ifelse(rsitr>1,0,1)
for(i in 1:length(Cl(stockData))){rsipos[i] <- ifelse(rsitr[i]==1,1,ifelse(rsitr[i]==-1,0,rsipos[i-1]))}
rsipos[is.na(rsipos)] <- 1
rsiposcomp <- cbind(rsi,rsitr,rsipos)
colnames(rsiposcomp) <-c("rsi","rsitr","rsipos")


## Stochastic Momentum Index SMI(13,2,25,9) Trading Strategy
smipos <- ifelse(smitr>1,0,1)
for(i in 1:length(Cl(stockData))){smipos[i] <- ifelse(smitr[i]==1,1,ifelse(smitr[i]==-1,0,smipos[i-1]))}
smipos[is.na(smipos)] <- 1
smiposcomp <- cbind(smi[,1],smi[,2],smitr,smipos)
colnames(smiposcomp) <-c("smi","signal","smitr","smipos")

## Williams %R(14) Trading Strategy
wprpos <- ifelse(wprtr>1,0,1)
for(i in 1:length(Cl(stockData))){wprpos[i] <- ifelse(wprtr[i]==1,1,ifelse(wprtr[i]==-1,0,wprpos[i-1]))}
wprpos[is.na(wprpos)] <- 1
wprposcomp <- cbind(wpr,wprtr,wprpos)
colnames(wprposcomp) <-c("wpr","wprtr","wprpos")

## Commodity Channel Index CCI(20,0.015) and Simple Moving Average SMA(5) Trading Strategy
ccismapos <- ifelse(ccismatr>1,0,1)
for(i in 1:length(Cl(stockData))){ccismapos[i] <- ifelse(ccismatr[i]==1,1,ifelse(ccismatr[i]==-1,0,ccismapos[i-1]))}
ccismapos[is.na(ccismapos)] <- 1
ccismaposcomp <- cbind(Cl(stockData),sma5,cci,ccismatr,ccismapos)
colnames(ccismaposcomp) <-c("Close","sma5","cci","ccismatr","ccismapos")

## Rate Of Change ROC(21) and Simple Moving Average SMA(5) Trading Strategy
rocsmapos <- ifelse(rocsmatr>1,0,1)
for(i in 1:length(Cl(stockData))){rocsmapos[i] <- ifelse(rocsmatr[i]==1,1,ifelse(rocsmatr[i]==-1,0,rocsmapos[i-1]))}
rocsmapos[is.na(rocsmapos)] <- 1
rocsmaposcomp <- cbind(Cl(stockData),sma5,roc[,4],rocsmatr,rocsmapos)
colnames(rocsmaposcomp) <-c("Close","sma5","roc","rocsmatr","rocsmapos")


## Relative Strength Index RSI(14) and Simple Moving Average SMA(5) Trading Strategy

rsismapos <- ifelse(rsismatr>1,0,1)
for(i in 1:length(Cl(stockData))){rsismapos[i] <- ifelse(rsismatr[i]==1,1,ifelse(rsismatr[i]==-1,0,rsismapos[i-1]))}
rsismapos[is.na(rsismapos)] <- 1
rsismaposcomp <- cbind(Cl(stockData),sma5,rsi,rsismatr,rsismapos)
colnames(rsismaposcomp) <-c("Close","sma5","rsi","rsismatr","rsismapos")


## Stochastic Momentum Index SMI(13,2,25,9) and Simple Moving Average SMA(5) Trading Strategy
smismapos <- ifelse(smismatr>1,0,1)
for(i in 1:length(Cl(stockData))){smismapos[i] <- ifelse(smismatr[i]==1,1,ifelse(smismatr[i]==-1,0,smismapos[i-1]))}
smismapos[is.na(smismapos)] <- 1
smismaposcomp <- cbind(Cl(stockData),sma5,smi[,1],smismatr,smismapos)
colnames(smismaposcomp) <-c("Close","sma5","smi","smismatr","smismapos")


## Williams %R(14) and Simple Moving Average SMA(5) Trading Strategy
wprsmapos <- ifelse(wprsmatr>1,0,1)
for(i in 1:length(Cl(stockData))){wprsmapos[i] <- ifelse(wprsmatr[i]==1,1,ifelse(wprsmatr[i]==-1,0,wprsmapos[i-1]))}
wprsmapos[is.na(wprsmapos)] <- 1
wprsmaposcomp <- cbind(Cl(stockData),sma5,wpr,wprsmatr,wprsmapos)
colnames(wprsmaposcomp) <-c("Close","sma5","wpr","wprsmatr","wprsmapos")


#####################################
# Strategies Performance Comparison #
#####################################

## CONCEITOS IMPORTANTES ##
# What is the 'Sharpe Ratio'
# The Sharpe ratio is the average return earned in excess of the risk-free rate per unit of volatility or total risk.
# Subtracting the risk-free rate from the mean return, the performance associated with risk-taking activities can be isolated.
# One intuition of this calculation is that a portfolio engaging in “zero risk” investment, such as the purchase of U.S.
# Treasury bills (for which the expected return is the risk-free rate), has a Sharpe ratio of exactly zero. Generally,
# the greater the value of the Sharpe ratio, the more attractive the risk-adjusted return.
# Read more: Sharpe Ratio | Investopedia https://www.investopedia.com/terms/s/sharperatio.asp#ixzz543v6GTy9

# What is an 'Annualized Total Return'
# An annualized total return is the geometric average amount of money earned by an investment each year over a given time period.
# It is calculated as a geometric average to show what an investor would earn over a period of time if the annual return was compounded.
# An annualized total return provides only a snapshot of an investment's performance and does not give investors any indication of
# its volatility.



estrategias <- c("SMA(5 & 21) & EMA(5 & 21)", # Estrategia 1
                 "BB(20,2)", # Estrategia 2
                 "SAR(0.02,0.2)", # Estrategia 3
                 "ADX(14)", # Estrategia 4
                 "CCI(20,0.015)", # Estrategia 5
                 "MACD(12,26,9)", # Estrategia 6
                 "ROC(21)", # Estrategia 7
                 "RSI(14)", # Estrategia 8
                 "SMI(13,2,25,9)", # Estrategia 9
                 "%R(14)", # Estrategia 10
                 "CCI(20,0.015) & SMA(5)", # Estrategia 11
                 "ROC(21) & SMA(5)",  # Estrategia 12
                 "RSI(14) & SMA(5)", # Estrategia 13
                 "SMI(13,2,25,9) & SMA(5)", # Estrategia 14
                 "%R(14) & SMA(5)") # Estragia 15

# 6.1. Moving Averages MA, Simple Moving Averages SMA(5 & 21), Exponential Moving Averages EMA(5 & 21) Strategies Performance Comparison

# Simple Moving Averages SMA(5 & 21) Price Crossover Strategies Performance Comparison
# Price Crossover Strategy Returns/Equity Curve
#
ret <- dailyReturn(Cl(stockData),type="arithmetic")
ret[1] <- 0
bhstrat <- ret
# Para calcular o retorno da estrategia, multiplica o valor obtido vezes a condição se haviamos comprado ou nao a acao
sma5strat <- ret*sma5pos
# TC para Trading Comissions
sma5stratc <- ifelse((sma5tr==1|sma5tr==-1)&sma5pos!=Lag(sma5pos),(ret-COMISSAO)*sma5pos,ret*sma5pos)
sma21strat <- ret*sma21pos
sma21stratc <- ifelse((sma21tr==1|sma21tr==-1)&sma21pos!=Lag(sma21pos),(ret-COMISSAO)*sma21pos,ret*sma21pos)
# Price Crossover Strategy Performance Comparison
smacomp <- cbind(sma5strat,sma5stratc,sma21strat,sma21stratc,bhstrat)
colnames(smacomp) <- c("SMA(5)","SMA(5) TC","SMA(21)","SMA(21) TC","BH")
#table.AnnualizedReturns(smacomp)
#charts.PerformanceSummary(smacomp)


# Estrategia 1:
# Exponential Moving Averages EMA (5 & 21) Price Crossover Strategies Performance Comparison
# Price Crossover Strategy Returns/Equity Curve
ema5strat <- ret*ema5pos
ema5stratc <- ifelse((ema5tr==1|ema5tr==-1)&ema5pos!=Lag(ema5pos),(ret-COMISSAO)*ema5pos,ret*ema5pos)
ema21strat <- ret*ema21pos
ema21stratc <- ifelse((ema21tr==1|ema21tr==-1)&ema21pos!=Lag(ema21pos),(ret-COMISSAO)*ema21pos,ret*ema21pos)
# Price Crossover Strategy Performance Comparison
emacomp <- cbind(ema5strat,ema5stratc,ema21strat,ema21stratc,bhstrat)
colnames(emacomp) <- c("EMA(5)","EMA(5) TC","EMA(21)","EMA(21) TC","BH")
#table.AnnualizedReturns(emacomp)
#charts.PerformanceSummary(emacomp)

# Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Strategies Performance Comparison
# Double Crossover Strategy Returns/Equity Curve
smastrat <- ret*smapos
smastratc <- ifelse((smatr==1|smatr==-1)&smapos!=Lag(smapos),(ret-COMISSAO)*smapos,ret*smapos)
emastrat <- ret*emapos
emastratc <- ifelse((ematr==1|ematr==-1)&emapos!=Lag(emapos),(ret-COMISSAO)*emapos,ret*emapos)
# Double Crossover Strategy Performance Comparison
macomp <- cbind(smastrat,smastratc,emastrat,emastratc,bhstrat)
colnames(macomp) <- c("SMA(5,21)","SMA(5,21) TC","EMA(5,21)","EMA(5,21) TC","BH")
est1 <- table.AnnualizedReturns(macomp)
#charts.PerformanceSummary(macomp)

# Estrategia 2:
# Bollinger Bands BB(20,2) Strategy Performance Comparison
# Bands Strategy Returns/Equity Curve
bbstrat <- ret*bbpos
bbstratc <- ifelse((bbtr==1|bbtr==-1)&bbpos!=Lag(bbpos),(ret-COMISSAO)*bbpos,ret*bbpos)
# Bands Strategy Performance Comparison Tables
bbcomp <- cbind(bbstrat,bbstratc,bhstrat)
colnames(bbcomp) <- c("BB(20,2)","BB(20,2) TC","BH")
est2 <- table.AnnualizedReturns(bbcomp)
#charts.PerformanceSummary(bbcomp)

# Estrategia 3:
# Parabolic Stop And Reverse SAR(0.02,0.2) Strategy Performance Comparison
# Stop And Reverse Strategy Returns/Equity Curve
sarstrat <- ret*sarpos
sarstratc <- ifelse((sartr==1|sartr==-1)&sarpos!=Lag(sarpos),(ret-COMISSAO)*sarpos,ret*sarpos)
# Stop And Reverse Strategy Performance Comparison
sarcomp <- cbind(sarstrat,sarstratc,bhstrat)
colnames(sarcomp) <- c("SAR(0.02,0.2)","SAR(0.02,0.2) TC","BH")
est3 <- table.AnnualizedReturns(sarcomp)
#charts.PerformanceSummary(sarcomp)

# Estrategia 4:
# Average Directional Movement Index ADX(14) Strategy Performance Comparison
# Band and Double Crossover Strategy Returns/Equity Curve
adxstrat <- ret*adxpos
adxstratc <- ifelse((adxtr==1|adxtr==-1)&adxpos!=Lag(adxpos),(ret-COMISSAO)*adxpos,ret*adxpos)
# Band and Double Crossover Strategy Performance Comparison
adxcomp <- cbind(adxstrat,adxstratc,bhstrat)
colnames(adxcomp) <- c("ADX(14)","ADX(14) TC","BH")
est4 <- table.AnnualizedReturns(adxcomp)
#charts.PerformanceSummary(adxcomp)

# Estrategia 5:
# Commodity Channel Index CCI(20,0.015) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
ccistrat <- ret*ccipos
ccistratc <- ifelse((ccitr==1|ccitr==-1)&ccipos!=Lag(ccipos),(ret-COMISSAO)*ccipos,ret*ccipos)
# Bands Crossover Strategy Performance Comparison
ccicomp <- cbind(ccistrat,ccistratc,bhstrat)
colnames(ccicomp) <- c("CCI(20,0.015)","CCI(20,0.015) TC","BH")
est5 <- table.AnnualizedReturns(ccicomp)
#charts.PerformanceSummary(ccicomp)


# Estrategia 6:
# Moving Averages Covergence/Divergence MACD(12,26,9) Strategies Performance Comparison
# Signal and Centerline Strategy Returns/Equity Curve
smacdstrat <- ret*smacdpos
smacdstratc <- ifelse((smacdtr==1|smacdtr==-1)&smacdpos!=Lag(smacdpos),(ret-COMISSAO)*smacdpos,ret*smacdpos)
cmacdstrat <- ret*cmacdpos
cmacdstratc <- ifelse((cmacdtr==1|cmacdtr==-1)&cmacdpos!=Lag(cmacdpos),(ret-COMISSAO)*cmacdpos,ret*cmacdpos)
# Signal and Centerline Strategy Performance Comparison
macdcomp <- cbind(smacdstrat,smacdstratc,cmacdstrat,cmacdstratc,bhstrat)
colnames(macdcomp) <- c("MACD(12,26,9)S","MACD(12,26,9)S TC","MACD(12,26)C","MACD(12,26)C TC","BH")
est6 <- table.AnnualizedReturns(macdcomp)
#charts.PerformanceSummary(macdcomp)

# Estrategia 7:
# Rate Of Change ROC(21) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
rocstrat <- ret*rocpos
rocstratc <- ifelse((roctr==1|roctr==-1)&rocpos!=Lag(rocpos),(ret-COMISSAO)*rocpos,ret*rocpos)
# Bands Crossover Strategy Performance Comparison
roccomp <- cbind(rocstrat,rocstratc,bhstrat)
colnames(roccomp) <- c("ROC(21)","ROC(21) TC","BH")
est7 <- table.AnnualizedReturns(roccomp)
#charts.PerformanceSummary(roccomp)

# Estrategia 8:
# Relative Strength Index RSI(14) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
rsistrat <- ret*rsipos
rsistratc <- ifelse((rsitr==1|rsitr==-1)&rsipos!=Lag(rsipos),(ret-COMISSAO)*rsipos,ret*rsipos)
# Bands Crossover Strategy Performance Comparison
rsicomp <- cbind(rsistrat,rsistratc,bhstrat)
colnames(rsicomp) <- c("RSI(14)","RSI(14) TC","BH")
est8 <- table.AnnualizedReturns(rsicomp)
#charts.PerformanceSummary(rsicomp)

# Estrategia 9:
# Stochastic Momentum Index SMI(13,2,25,9) Strategy Performance Comparison
# Signal Strategy Returns/Equity Curve
smistrat <- ret*smipos
smistratc <- ifelse((smitr==1|smitr==-1)&smipos!=Lag(smipos),(ret-COMISSAO)*smipos,ret*smipos)
# Signal Strategy Performance Comparison
smicomp <- cbind(smistrat,smistratc,bhstrat)
colnames(smicomp) <- c("SMI(13,2,25)","SMI(13,2,25) TC","BH")
est9 <- table.AnnualizedReturns(smicomp)
#charts.PerformanceSummary(smicomp)

# Estrategia 10:
# Williams %R(14) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
wprstrat <- ret*wprpos
wprstratc <- ifelse((wprtr==1|wprtr==-1)&wprpos!=Lag(wprpos),(ret-COMISSAO)*wprpos,ret*wprpos)
# Bands Crossover Strategy Performance Comparison
wprcomp <- cbind(wprstrat,wprstratc,bhstrat)
colnames(wprcomp) <- c("WPR(14)","WPR(14) TC","BH")
est10 <- table.AnnualizedReturns(wprcomp)
#charts.PerformanceSummary(wprcomp)


# Estrategia 11:
# Commodity Channel Index CCI(20,0.015) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
ccismastrat <- ret*ccismapos
ccismastratc <- ifelse((ccismatr==1|ccismatr==-1)&ccismapos!=Lag(ccismapos),(ret-COMISSAO)*ccismapos,ret*ccismapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
ccismacomp <- cbind(ccismastrat,ccismastratc,bhstrat)
colnames(ccismacomp) <- c("SMA(5),CCI(20,0.015)","SMA(5),CCI(20,0.015) TC","BH")
est11 <- table.AnnualizedReturns(ccismacomp)
#charts.PerformanceSummary(ccismacomp)

# Estrategia 12:
# Rate Of Change ROC(21) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
rocsmastrat <- ret*rocsmapos
rocsmastratc <- ifelse((rocsmatr==1|rocsmatr==-1)&rocsmapos!=Lag(rocsmapos),(ret-COMISSAO)*rocsmapos,ret*rocsmapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
rocsmacomp <- cbind(rocsmastrat,rocsmastratc,bhstrat)
colnames(rocsmacomp) <- c("SMA(5),ROC(21)","SMA(5),ROC(21) TC","BH")
est12 <- table.AnnualizedReturns(rocsmacomp)
#charts.PerformanceSummary(rocsmacomp)

# Estrategia 13:
# Relative Strength Index RSI(14) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
rsismastrat <- ret*rsismapos
rsismastratc <- ifelse((rsismatr==1|rsismatr==-1)&rsismapos!=Lag(rsismapos),(ret-COMISSAO)*rsismapos,ret*rsismapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
rsismacomp <- cbind(rsismastrat,rsismastratc,bhstrat)
colnames(rsismacomp) <- c("SMA(5),RSI(14)","SMA(5),RSI(14) TC","BH")
est13 <- table.AnnualizedReturns(rsismacomp)
#charts.PerformanceSummary(rsismacomp)

# Estrategia 14:
# Stochastic Momentum Index SMI(13,2,25,9) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
smismastrat <- ret*smismapos
smismastratc <- ifelse((smismatr==1|smismatr==-1)&smismapos!=Lag(smismapos),(ret-COMISSAO)*smismapos,ret*smismapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
smismacomp <- cbind(smismastrat,smismastratc,bhstrat)
colnames(smismacomp) <- c("SMA(5),SMI(13,2,25)","SMA(5),SMI(13,2,25) TC","BH")
est14 <- table.AnnualizedReturns(smismacomp)
#charts.PerformanceSummary(smismacomp)

# Estrategia 15:
# Williams %R(14) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
wprsmastrat <- ret*wprsmapos
wprsmastratc <- ifelse((wprsmatr==1|wprsmatr==-1)&wprsmapos!=Lag(wprsmapos),(ret-COMISSAO)*wprsmapos,ret*wprsmapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
wprsmacomp <- cbind(wprsmastrat,wprsmastratc,bhstrat)
colnames(wprsmacomp) <- c("SMA(5),WPR(14)","SMA(5),WPR(14) TC","BH")
est15 <- table.AnnualizedReturns(wprsmacomp)
#charts.PerformanceSummary(wprsmacomp)

## Escolhendo a estrategia com melhores resultados

listaEstrategias <- list(est1, est2, est3, est4, est5, est6, est7, est8, est9, est10, est11, est12, est13, est14, est15)

n <- length(listaEstrategias)

# Nenhuma Estrategia Escolhida
estrategiasVencedoras <- c()
valoresRetornoVencedores <- c()
# Atentar para atualizar essa Informacao caso mude alguma configuracao
valorBenchmark <- listaEstrategias[[1]][1,5]

# Mudar futuramente para algum dos metodos da familia Apply
for(i in 1:n) {
  # Estrategias com Mais de Uma Opcao
  if (i %in% c(1,6)) {
    if (valorBenchmark < listaEstrategias[[i]][1,2]) {
      valoresRetornoVencedores <- c(valoresRetornoVencedores, listaEstrategias[[i]][1,2])
        if (i == 1) {estrategiasVencedoras <- c(estrategiasVencedoras,"SMA(5,21)")}
        if (i == 6) {estrategiasVencedoras <- c(estrategiasVencedoras,"MACD(12,26,9)S")}
    }
    if (valorBenchmark < listaEstrategias[[i]][1,4]) {
      valoresRetornoVencedores <- c(valoresRetornoVencedores, listaEstrategias[[i]][1,4])
      if (i == 1) {estrategiasVencedoras <- c(estrategiasVencedoras,"EMA(5,21)")}
      if (i == 6) {estrategiasVencedoras <- c(estrategiasVencedoras,"MACD(12,26)C")}
    }
    # Estrategias Mais Simples
  } else if (valorBenchmark < listaEstrategias[[i]][1,2]) {
    estrategiasVencedoras <- c(estrategiasVencedoras,estrategias[i])
    valoresRetornoVencedores <- c(valoresRetornoVencedores, listaEstrategias[[i]][1,2])
  }
}

intervaloRetornado <- paste("Valor Anual Retornado no Periodo",inicio, "a", fim, sep = " ")
resultado <- cbind(estrategiasVencedoras, valoresRetornoVencedores)
resultado <- colnames("Estrategia",intervaloRetornado)

resultado

}

