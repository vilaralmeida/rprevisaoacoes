######################################################
# Stock Technical Analysis with R                    #
# (c) Diego Fernandez Garcia 2015-2017               #
# www.exfinsis.com                                   #
######################################################

# 1. Load R packages
library("TTR")
library("quantmod")
library("PerformanceAnalytics")

## 2. Data Downloading or Reading

# 2.1.Yahoo Finance
# getSymbols("SPY",src="yahoo",from="2016-01-01",to="2017-01-01")

# 2.2. Google Finance
# No Adjusted Close Prices
# getSymbols("SPY",src="google",from="2016-01-01",to="2017-01-01")

# 2.3. Data Reading
SPY <- read.csv("Stock-Technical-Analysis-Data.txt",header=TRUE)
SPY <- xts(SPY[,2:7],order.by=as.Date(SPY[,1]))

# 2.4. Technical Analysis Charts
lineChart(SPY,theme=chartTheme("white"))
barChart(SPY,theme=chartTheme("white"))
candleChart(SPY,theme=chartTheme("white"))

# 3. Stock Technical Indicators

# 3.1. Lagging Stock Technical Indicators

# 3.1.1. Moving Averages MA, Simple Moving Averages SMA(5 & 21), Exponential Moving Averages EMA(5 & 21)

# Simple Moving Average
sma5 <- SMA(Cl(SPY),n=5)
sma21 <- SMA(Cl(SPY),n=21)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addSMA(n=5,col="darkblue")
addSMA(n=21,col="darkred")
legend("bottomright",col=c("darkblue","darkred"),lty=1,legend=c("SMA(5)","SMA(21)"),cex=0.6)
# Manual Chart
plot(Cl(SPY),main="Simple Moving Averages SMA(5 & 21)",ylab="Prices",xlab="Date")
lines(sma5,col="darkblue")
lines(sma21,col="darkred")
legend("bottomright",col=c("black","darkblue","darkred"),lty=1,legend=c("SPY","SMA(5)","SMA(21)"),cex=0.6)

# Exponential Moving Average
ema5 <- EMA(Cl(SPY),n=5)
ema21 <- EMA(Cl(SPY),n=21)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addEMA(n=5,col="darkblue")
addEMA(n=21,col="darkred")
legend("bottomright",col=c("darkblue","darkred"),lty=1,legend=c("EMA(5)","EMA(21)"),cex=0.6)
# Manual Chart
plot(Cl(SPY),main="Exponential Moving Averages EMA(5 & 21)",ylab="Prices",xlab="Date")
lines(ema5,col="darkblue")
lines(ema21,col="darkred")
legend("bottomright",col=c("black","darkblue","darkred"),lty=1,legend=c("SPY","EMA(5)","EMA(21)"),cex=0.6)

# 3.1.2. Bollinger Bands BB(20,2)
bb <- BBands(HLC(SPY),n=20,sd=2)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addBBands(n=20,sd=2)
# Manual Chart
plot(Cl(SPY),main="Bollinger Bands BB(20,2)")
# Lower and Upper Bands
lines(bb[,1],col="darkred",lty=2)
lines(bb[,3],col="darkred",lty=2)
# Middle Band
lines(bb[,2],col="darkgray",lty=2)
legend("bottomright",col=c("black","darkred","darkgray","darkred"),lty=c(1,2,2,2),legend=c("SPY","Lower BB(20,2)","Middle BB(20,2)","Upper BB(20,2)"),cex=0.6)

# 3.1.3. Parabolic Stop and Reverse SAR(0.02,0.2)
sar <- SAR(cbind(Hi(SPY),Lo(SPY)),accel=c(0.02, 0.2))
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addSAR(accel=c(0.02, 0.2))
# Manual Chart
plot(Cl(SPY),main="Parabolic Stop and Reverse SAR(0.02,0.2)")
points(sar,col="darkblue")
legend("bottomright",col=c("black","darkblue"),lty=1,legend=c("SPY","SAR(0.02,0.2)"),cex=0.6)

# 3.2. Leading Stock Technical Indicators

# 3.2.1. Average Directional Movement Index ADX(14)
adx <- ADX(HLC(SPY),n=14)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addADX(n=14)
legend("right",col=c("blue","green","red"),lty=1,legend=c("ADX(14)","DI(14)+","DI(14)-"),cex=0.6)

# 3.2.2. Commodity Channel Index CCI(20,0.015)
cci <- CCI(HLC(SPY),n=20,c=0.015)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addCCI(n=20,c=0.015)

# 3.2.3. Moving Averages Covergence/Divergence MACD(12,26,9)
macd <- MACD(Cl(SPY),nFast=12,nSlow=26,nSig=9)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addMACD(fast=12,slow=26,signal=9)

# 3.2.4. Rate Of Change ROC(21)
roc <- ROC(SPY,n=21)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addROC(n=21)
legend("right",col="red",lty=1,legend="ROC(21)",cex=0.6)

# 3.2.5. Relative Strength Index RSI(14)
rsi <- RSI(Cl(SPY),n=14)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addRSI(n=14)

# 3.2.6. Stochastic Momentum Index SMI(13,2,25,9)
smi <- SMI(HLC(SPY),n=13,nFast=2,nSlow=25,nSig=9)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addSMI(n=13,fast=2,slow=25,signal=9)

# 3.2.7. Williams %R(14)
wpr <- WPR(HLC(SPY),n=14)
colnames(wpr) <- "wpr"
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addWPR(n=14)

# 4. Stock Trading Signals

# 4.1. Single Indicator Trading Signals

# 4.1.1. Moving Averages MA, Simple Moving Averages SMA(5 & 21), Exponential Moving Averages EMA(5 & 21) Trading Signals

# Simple Moving Averages SMA(5 & 21) Price Crossover Trading Signals
barChart(SPY,theme=chartTheme("white"))
addSMA(n=5,col="darkblue")
addSMA(n=21,col="darkred")
legend("bottomright",col=c("darkblue","darkred"),lty=1,legend=c("SMA(5)","SMA(21)"),cex=0.6)
# Price Crossover Trading Signals
sma5tr <- Lag(ifelse(Lag(Cl(SPY))<Lag(sma5)&Cl(SPY)>sma5,1,ifelse(Lag(Cl(SPY))>Lag(sma5)&Cl(SPY)<sma5,-1,0)))
sma5tr[is.na(sma5tr)] <- 0
sma21tr <- Lag(ifelse(Lag(Cl(SPY))<Lag(sma21)&Cl(SPY)>sma21,1,ifelse(Lag(Cl(SPY))>Lag(sma21)&Cl(SPY)<sma21,-1,0)))
sma21tr[is.na(sma21tr)] <- 0

# Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Signals
barChart(SPY,theme=chartTheme("white"))
addEMA(n=5,col="darkblue")
addEMA(n=21,col="darkred")
legend("bottomright",col=c("darkblue","darkred"),lty=1,legend=c("EMA(5)","EMA(21)"),cex=0.6)
# Price Crossover Trading Signals
ema5tr <- Lag(ifelse(Lag(Cl(SPY))<Lag(ema5)&Cl(SPY)>ema5,1,ifelse(Lag(Cl(SPY))>Lag(ema5)&Cl(SPY)<ema5,-1,0)))
ema5tr[is.na(ema5tr)] <- 0
ema21tr <- Lag(ifelse(Lag(Cl(SPY))<Lag(ema21)&Cl(SPY)>ema21,1,ifelse(Lag(Cl(SPY))>Lag(ema21)&Cl(SPY)<ema21,-1,0)))
ema21tr[is.na(ema21tr)] <- 0

# Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Trading Signals
# Double Crossover Trading Signals
smatr <- Lag(ifelse(Lag(sma5)<Lag(sma21)&sma5>sma21,1,ifelse(Lag(sma5)>Lag(sma21)&sma5<sma21,-1,0)))
smatr[is.na(smatr)] <- 0
ematr <- Lag(ifelse(Lag(ema5)<Lag(ema21)&ema5>ema21,1,ifelse(Lag(ema5)>Lag(ema21)&ema5<ema21,-1,0)))
ematr[is.na(ematr)] <- 0

# 4.1.2. Bollinger Bands BB(20,2) Trading Signals
barChart(SPY,theme=chartTheme("white"))
addBBands(n=20,sd=2)
# Bands Crossover Trading Signals
bbtr <- Lag(ifelse(Lag(Cl(SPY))<Lag(bb[,1])&Cl(SPY)>bb[,1],1,ifelse(Lag(Cl(SPY))<Lag(bb[,3])&Cl(SPY)>bb[,3],-1,0)))
bbtr[is.na(bbtr)] <- 0

# 4.1.3. Parabolic Stop And Reverse SAR(0.02,0.2) Trading Signals
barChart(SPY,theme=chartTheme("white"))
addSAR(accel=c(0.02, 0.2))
# Stop And Reverse Trading Signals
sartr <- Lag(ifelse(Lag(Cl(SPY))<Lag(sar)&Cl(SPY)>sar,1,ifelse(Lag(Cl(SPY))>Lag(sar)&Cl(SPY)<sar,-1,0)))
sartr[is.na(sartr)] <- 0

# 4.1.4. Average Directional Movement Index ADX(14) Trading Signals
barChart(SPY,theme=chartTheme("white"))
addADX(n=14)
legend("right",col=c("blue","green","red"),lty=1,legend=c("ADX(14)","DI(14)+","DI(14)-"),cex=0.6)
# Band and Double Crossover Trading Signals
adxtr <- Lag(ifelse(Lag(adx[,1])<Lag(adx[,2])&adx[,1]>adx[,2]&adx[,4]>20,1,ifelse(Lag(adx[,1])>Lag(adx[,2])&adx[,1]<adx[,2]&adx[,4]>20,-1,0)))
adxtr[is.na(adxtr)] <- 0

# 4.1.5. Commodity Channel Index CCI(20,0.015) Trading Signals
barChart(SPY,theme=chartTheme("white"))
addCCI(n=20,c=0.015)
# Bands Crossover Trading Signals
ccitr <- Lag(ifelse(Lag(cci)<(-100)&cci>(-100),1,ifelse(Lag(cci)<100&cci>100,-1,0)))
ccitr[is.na(ccitr)] <- 0

# 4.1.6. Moving Averages Covergence/Divergence MACD(12,26,9) Trading Signals
barChart(SPY,theme=chartTheme("white"))
addMACD(fast=12,slow=26,signal=9)
# Signal and Centerline Crossover Trading Signals
smacdtr <- Lag(ifelse(Lag(macd[,1])<Lag(macd[,2])&macd[,1]>macd[,2],1,ifelse(Lag(macd[,1])>Lag(macd[,2])&macd[,1]<macd[,2],-1,0)))
smacdtr[is.na(smacdtr)] <- 0
cmacdtr <- Lag(ifelse(Lag(macd[,1])<0&macd[,1]>0,1,ifelse(Lag(macd[,1])>0&macd[,1]<0,-1,0)))
cmacdtr[is.na(cmacdtr)] <- 0

# 4.1.7. Rate Of Change ROC(21) Trading Signals
barChart(SPY,theme=chartTheme("white"))
addROC(n=21)
legend("right",col="red",lty=1,legend="ROC(21)",cex=0.6)
# Bands Crossover Trading Signals
roctr <- Lag(ifelse(Lag(roc[,4])<(-0.05)&roc[,4]>(-0.05),1,ifelse(Lag(roc[,4])<0.05&roc[,4]>0.05,-1,0)))
roctr[is.na(roctr)] <- 0

# 4.1.8. Relative Strength Index RSI(14) Trading Signals
barChart(SPY,theme=chartTheme("white"))
addRSI(n=14)
# Bands Crossover Trading Signals
rsitr <- Lag(ifelse(Lag(rsi)<30&rsi>30,1,ifelse(Lag(rsi)<70&rsi>70,-1,0)))
rsitr[is.na(rsitr)] <- 0

# 4.1.9. Stochastic Momentum Index SMI(13,2,25,9) Trading Signals
barChart(SPY,theme=chartTheme("white"))
addSMI(n=13,fast=2,slow=25,signal=9)
# Signal Crossover Trading Signals
smitr <- Lag(ifelse(Lag(smi[,1])<Lag(smi[,2])&smi[,1]>smi[,2],1,ifelse(Lag(smi[,1])>Lag(smi[,2])&smi[,1]<smi[,2],-1,0)))
smitr[is.na(smitr)] <- 0

# 4.1.10. Williams %R(14) Trading Signals
barChart(SPY,theme=chartTheme("white"))
addWPR(n=14)
# Bands Crossover Trading Signals
wprtr <- Lag(ifelse(Lag(wpr)>0.80&wpr<0.80,1,ifelse(Lag(wpr)>0.20&wpr<0.20,-1,0)))
wprtr[is.na(wprtr)] <- 0

# 4.2. Multiple Indicators Trading Signals

# 4.2.1. Commodity Channel Index CCI(20,0.015) and Simple Moving Average SMA(5) Trading Signals
lineChart(SPY,theme=chartTheme("white"))
addCCI(n=20,c=0.015)
addSMA(n=5,col="darkblue")
legend("right",col=c("green","darkblue"),lty=1,legend=c("SPY","SMA5"),cex=0.6)
# Price Crossover and Bands Crossover Confirmation Trading Signals
ccismatr <- Lag(ifelse(Lag(Cl(SPY))<Lag(sma5)&Cl(SPY)>sma5&cci<(-100),1,ifelse(Lag(Cl(SPY))>Lag(sma5)&Cl(SPY)<sma5&cci>100,-1,0)))
ccismatr[is.na(ccismatr)] <- 0

# 4.2.2. Rate Of Change ROC(21) and Simple Moving Average SMA(5) Trading Signals
lineChart(SPY,theme=chartTheme("white"))
addROC(n=21)
addSMA(n=5,col="darkblue")
legend("right",col=c("green","darkblue","red"),lty=1,legend=c("SPY","SMA(5)","ROC(21)"),cex=0.6)
# Price Crossover and Bands Crossover Confirmation Trading Signals
rocsmatr <- Lag(ifelse(Lag(Cl(SPY))<Lag(sma5)&Cl(SPY)>sma5&roc[,4]<(-0.05),1,ifelse(Lag(Cl(SPY))>Lag(sma5)&Cl(SPY)<sma5&roc[,4]>0.05,-1,0)))
rocsmatr[is.na(rocsmatr)] <- 0

# 4.2.3. Relative Strength Index RSI(14) and Simple Moving Average SMA(5) Trading Signals
lineChart(SPY,theme=chartTheme("white"))
addRSI(n=14)
addSMA(n=5,col="darkblue")
legend("right",col=c("green","darkblue"),lty=1,legend=c("SPY","SMA(5)"),cex=0.6)
# Price Crossover and Bands Crossover Confirmation Trading Signals
rsismatr <- Lag(ifelse(Lag(Cl(SPY))<Lag(sma5)&Cl(SPY)>sma5&rsi<30,1,ifelse(Lag(Cl(SPY))>Lag(sma5)&Cl(SPY)<sma5&rsi>70,-1,0)))
rsismatr[is.na(rsismatr)] <- 0

# 4.2.4. Stochastic Momentum Index SMI(13,2,25,9) and Simple Moving Average SMA(5) Trading Signals
lineChart(SPY,theme=chartTheme("white"))
addSMI(n=13,fast=2,slow=25,signal=9)
addSMA(n=5,col="darkblue")
legend("right",col=c("green","darkblue"),lty=1,legend=c("SPY","SMA(5)"),cex=0.6)
# Price Crossover and Bands Crossover Confirmation Trading Signals
smismatr <- Lag(ifelse(Lag(Cl(SPY))<Lag(sma5)&Cl(SPY)>sma5&smi[,1]<(-40),1,ifelse(Lag(Cl(SPY))>Lag(sma5)&Cl(SPY)<sma5&smi[,1]>40,-1,0)))
smismatr[is.na(smismatr)] <- 0

# 4.2.5. Williams %R(14) and Simple Moving Average SMA(5) Trading Signals
lineChart(SPY,theme=chartTheme("white"))
addWPR(n=14)
addSMA(n=5,col="darkblue")
legend("right",col=c("green","darkblue"),lty=1,legend=c("SPY","SMA(5)"),cex=0.6)
# Price Crossover and Bands Crossover Confirmation Trading Signals
wprsmatr <- Lag(ifelse(Lag(Cl(SPY))<Lag(sma5)&Cl(SPY)>sma5&wpr>0.80,1,ifelse(Lag(Cl(SPY))>Lag(sma5)&Cl(SPY)<sma5&wpr<0.20,-1,0)))
wprsmatr[is.na(wprsmatr)] <- 0

# 5. Stock Trading Strategies

# 5.1. Moving Averages MA, Simple Moving Averages SMA(5 & 21), Exponential Moving Averages EMA(5 & 21) Trading Strategies

# Simple Moving Averages SMA(5 & 21) Price Crossover Trading Strategies

# Price Crossover Trading Strategies
sma5pos <- ifelse(sma5tr>1,0,1)
for(i in 1:length(Cl(SPY))){sma5pos[i] <- ifelse(sma5tr[i]==1,1,ifelse(sma5tr[i]==-1,0,sma5pos[i-1]))}
sma5pos[is.na(sma5pos)] <- 1
sma5poscomp <- cbind(Cl(SPY),sma5,sma5tr,sma5pos)
colnames(sma5poscomp) <-c("Close",'sma5',"sma5tr","sma5pos")
View(sma5poscomp)
sma21pos <- ifelse(sma21tr>1,0,1)
for(i in 1:length(Cl(SPY))){sma21pos[i] <- ifelse(sma21tr[i]==1,1,ifelse(sma21tr[i]==-1,0,sma21pos[i-1]))}
sma21pos[is.na(sma21pos)] <- 1
sma21poscomp <- cbind(Cl(SPY),sma21,sma21tr,sma21pos)
colnames(sma21poscomp) <-c("Close",'sma21',"sma21tr","sma21pos")
View(sma21poscomp)

# Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Strategies

# Price Crossover Trading Strategies
ema5pos <- ifelse(ema5tr>1,0,1)
for(i in 1:length(Cl(SPY))){ema5pos[i] <- ifelse(ema5tr[i]==1,1,ifelse(ema5tr[i]==-1,0,ema5pos[i-1]))}
ema5pos[is.na(ema5pos)] <- 1
ema5poscomp <- cbind(Cl(SPY),ema5,ema5tr,ema5pos)
colnames(ema5poscomp) <-c("Close",'ema5',"ema5tr","ema5pos")
View(ema5poscomp)
ema21pos <- ifelse(ema21tr>1,0,1)
for(i in 1:length(Cl(SPY))){ema21pos[i] <- ifelse(ema21tr[i]==1,1,ifelse(ema21tr[i]==-1,0,ema21pos[i-1]))}
ema21pos[is.na(ema21pos)] <- 1
ema21poscomp <- cbind(Cl(SPY),ema21,ema21tr,ema21pos)
colnames(ema21poscomp) <-c("Close",'ema21',"ema21tr","ema21pos")
View(ema21poscomp)

# Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Trading Strategies

# Double Crossover Trading Strategies
smapos <- ifelse(smatr>1,0,1)
for(i in 1:length(Cl(SPY))){smapos[i] <- ifelse(smatr[i]==1,1,ifelse(smatr[i]==-1,0,smapos[i-1]))}
smapos[is.na(smapos)] <- 1
smaposcomp <- cbind(sma5,sma21,smatr,smapos)
colnames(smaposcomp) <-c("sma5",'sma21',"smatr","smapos")
View(smaposcomp)
emapos <- ifelse(ematr>1,0,1)
for(i in 1:length(Cl(SPY))){emapos[i] <- ifelse(ematr[i]==1,1,ifelse(ematr[i]==-1,0,emapos[i-1]))}
emapos[is.na(emapos)] <- 1
emaposcomp <- cbind(ema5,ema21,ematr,emapos)
colnames(emaposcomp) <-c("ema5",'ema21',"ematr","emapos")
View(emaposcomp)

# 5.2. Bollinger Bands BB(20,2) Trading Strategy
# Bands Crossover Trading Strategy
bbpos <- ifelse(bbtr>1,0,1)
for(i in 1:length(Cl(SPY))){bbpos[i] <- ifelse(bbtr[i]==1,1,ifelse(bbtr[i]==-1,0,bbpos[i-1]))}
bbpos[is.na(bbpos)] <- 1
bbposcomp <- cbind(Cl(SPY),bb[,1],bb[,3],bbtr,bbpos)
colnames(bbposcomp) <-c("Close",'lower',"upper","bbtr","bbpos")
View(bbposcomp)

# 5.3. Parabolic Stop And Reverse SAR(0.02,0.2) Trading Strategy
# Stop And Reverse Trading Strategy
sarpos <- ifelse(sartr>1,0,1)
for(i in 1:length(Cl(SPY))){sarpos[i] <- ifelse(sartr[i]==1,1,ifelse(sartr[i]==-1,0,sarpos[i-1]))}
sarpos[is.na(sarpos)] <- 1
sarposcomp <- cbind(Cl(SPY),sar,sartr,sarpos)
colnames(sarposcomp) <-c("Close","sar","sartr","sarpos")
View(sarposcomp)

# 5.4. Average Directional Movement Index ADX(14) Trading Strategy
# Band and Double Crossover Trading Strategy
adxpos <- ifelse(adxtr>1,0,1)
for(i in 1:length(Cl(SPY))){adxpos[i] <- ifelse(adxtr[i]==1,1,ifelse(adxtr[i]==-1,0,adxpos[i-1]))}
adxpos[is.na(adxpos)] <- 1
adxposcomp <- cbind(adx[,1],adx[,2],adx[,4],adxtr,adxpos)
colnames(adxposcomp) <-c("dip","din","adx","adxtr","adxpos")
View(adxposcomp)

# 5.5. Commodity Channel Index CCI(20,0.015) Trading Strategy
# Bands Crossover Trading Strategy
ccipos <- ifelse(ccitr>1,0,1)
for(i in 1:length(Cl(SPY))){ccipos[i] <- ifelse(ccitr[i]==1,1,ifelse(ccitr[i]==-1,0,ccipos[i-1]))}
ccipos[is.na(ccipos)] <- 1
cciposcomp <- cbind(cci,ccitr,ccipos)
colnames(cciposcomp) <-c("cci","ccitr","ccipos")
View(cciposcomp)

# 5.6. Moving Averages Covergence/Divergence MACD(12,26,9) Trading Strategies
# Signal and Centerline Crossover Trading Strategies
smacdpos <- ifelse(smacdtr>1,0,1)
for(i in 1:length(Cl(SPY))){smacdpos[i] <- ifelse(smacdtr[i]==1,1,ifelse(smacdtr[i]==-1,0,smacdpos[i-1]))}
smacdpos[is.na(smacdpos)] <- 1
smacdposcomp <- cbind(macd[,1],macd[,2],smacdtr,smacdpos)
colnames(smacdposcomp) <-c("macd","signal","smacdtr","smacdpos")
View(smacdposcomp)
cmacdpos <- ifelse(cmacdtr>1,0,1)
for(i in 1:length(Cl(SPY))){cmacdpos[i] <- ifelse(cmacdtr[i]==1,1,ifelse(cmacdtr[i]==-1,0,cmacdpos[i-1]))}
cmacdpos[is.na(cmacdpos)] <- 1
cmacdposcomp <- cbind(macd[,1],cmacdtr,cmacdpos)
colnames(cmacdposcomp) <-c("macd","cmacdtr","cmacdpos")
View(cmacdposcomp)

# 5.7. Rate Of Change ROC(21) Trading Strategy
# Bands Crossover Trading Strategy
rocpos <- ifelse(roctr>1,0,1)
for(i in 1:length(Cl(SPY))){rocpos[i] <- ifelse(roctr[i]==1,1,ifelse(roctr[i]==-1,0,rocpos[i-1]))}
rocpos[is.na(rocpos)] <- 1
rocposcomp <- cbind(roc[,4],roctr,rocpos)
colnames(rocposcomp) <-c("roc","roctr","rocpos")
View(rocposcomp)

# 5.8. Relative Strength Index RSI(14) Trading Strategy
# Bands Crossover Trading Strategy
rsipos <- ifelse(rsitr>1,0,1)
for(i in 1:length(Cl(SPY))){rsipos[i] <- ifelse(rsitr[i]==1,1,ifelse(rsitr[i]==-1,0,rsipos[i-1]))}
rsipos[is.na(rsipos)] <- 1
rsiposcomp <- cbind(rsi,rsitr,rsipos)
colnames(rsiposcomp) <-c("rsi","rsitr","rsipos")
View(rsiposcomp)

# 5.9. Stochastic Momentum Index SMI(13,2,25,9) Trading Strategy
# Signal Crossover Trading Strategy
smipos <- ifelse(smitr>1,0,1)
for(i in 1:length(Cl(SPY))){smipos[i] <- ifelse(smitr[i]==1,1,ifelse(smitr[i]==-1,0,smipos[i-1]))}
smipos[is.na(smipos)] <- 1
smiposcomp <- cbind(smi[,1],smi[,2],smitr,smipos)
colnames(smiposcomp) <-c("smi","signal","smitr","smipos")
View(smiposcomp)

# 5.10. Williams %R(14) Trading Strategy
# Bands Crossover Trading Strategy
wprpos <- ifelse(wprtr>1,0,1)
for(i in 1:length(Cl(SPY))){wprpos[i] <- ifelse(wprtr[i]==1,1,ifelse(wprtr[i]==-1,0,wprpos[i-1]))}
wprpos[is.na(wprpos)] <- 1
wprposcomp <- cbind(wpr,wprtr,wprpos)
colnames(wprposcomp) <-c("wpr","wprtr","wprpos")
View(wprposcomp)

# 5.11. Commodity Channel Index CCI(20,0.015) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
ccismapos <- ifelse(ccismatr>1,0,1)
for(i in 1:length(Cl(SPY))){ccismapos[i] <- ifelse(ccismatr[i]==1,1,ifelse(ccismatr[i]==-1,0,ccismapos[i-1]))}
ccismapos[is.na(ccismapos)] <- 1
ccismaposcomp <- cbind(Cl(SPY),sma5,cci,ccismatr,ccismapos)
colnames(ccismaposcomp) <-c("Close","sma5","cci","ccismatr","ccismapos")
View(ccismaposcomp)

# 5.12. Rate Of Change ROC(21) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
rocsmapos <- ifelse(rocsmatr>1,0,1)
for(i in 1:length(Cl(SPY))){rocsmapos[i] <- ifelse(rocsmatr[i]==1,1,ifelse(rocsmatr[i]==-1,0,rocsmapos[i-1]))}
rocsmapos[is.na(rocsmapos)] <- 1
rocsmaposcomp <- cbind(Cl(SPY),sma5,roc[,4],rocsmatr,rocsmapos)
colnames(rocsmaposcomp) <-c("Close","sma5","roc","rocsmatr","rocsmapos")
View(rocsmaposcomp)

# 5.13. Relative Strength Index RSI(14) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
rsismapos <- ifelse(rsismatr>1,0,1)
for(i in 1:length(Cl(SPY))){rsismapos[i] <- ifelse(rsismatr[i]==1,1,ifelse(rsismatr[i]==-1,0,rsismapos[i-1]))}
rsismapos[is.na(rsismapos)] <- 1
rsismaposcomp <- cbind(Cl(SPY),sma5,rsi,rsismatr,rsismapos)
colnames(rsismaposcomp) <-c("Close","sma5","rsi","rsismatr","rsismapos")
View(rsismaposcomp)

# 5.14. Stochastic Momentum Index SMI(13,2,25,9) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
smismapos <- ifelse(smismatr>1,0,1)
for(i in 1:length(Cl(SPY))){smismapos[i] <- ifelse(smismatr[i]==1,1,ifelse(smismatr[i]==-1,0,smismapos[i-1]))}
smismapos[is.na(smismapos)] <- 1
smismaposcomp <- cbind(Cl(SPY),sma5,smi[,1],smismatr,smismapos)
colnames(smismaposcomp) <-c("Close","sma5","smi","smismatr","smismapos")
View(smismaposcomp)

# 5.15. Williams %R(14) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
wprsmapos <- ifelse(wprsmatr>1,0,1)
for(i in 1:length(Cl(SPY))){wprsmapos[i] <- ifelse(wprsmatr[i]==1,1,ifelse(wprsmatr[i]==-1,0,wprsmapos[i-1]))}
wprsmapos[is.na(wprsmapos)] <- 1
wprsmaposcomp <- cbind(Cl(SPY),sma5,wpr,wprsmatr,wprsmapos)
colnames(wprsmaposcomp) <-c("Close","sma5","wpr","wprsmatr","wprsmapos")
View(wprsmaposcomp)

# 6. Strategies Performance Comparison

# 6.1. Moving Averages MA, Simple Moving Averages SMA(5 & 21), Exponential Moving Averages EMA(5 & 21) Strategies Performance Comparison

# Simple Moving Averages SMA(5 & 21) Price Crossover Strategies Performance Comparison
# Price Crossover Strategy Returns/Equity Curve
ret <- dailyReturn(Cl(SPY),type="arithmetic")
ret[1] <- 0
bhstrat <- ret
sma5strat <- ret*sma5pos
sma5stratc <- ifelse((sma5tr==1|sma5tr==-1)&sma5pos!=Lag(sma5pos),(ret-0.01)*sma5pos,ret*sma5pos)
sma21strat <- ret*sma21pos
sma21stratc <- ifelse((sma21tr==1|sma21tr==-1)&sma21pos!=Lag(sma21pos),(ret-0.01)*sma21pos,ret*sma21pos)
# Price Crossover Strategy Performance Comparison
smacomp <- cbind(sma5strat,sma5stratc,sma21strat,sma21stratc,bhstrat)
colnames(smacomp) <- c("SMA(5)","SMA(5) TC","SMA(21)","SMA(21) TC","BH")
table.AnnualizedReturns(smacomp)
charts.PerformanceSummary(smacomp)

# Exponential Moving Averages EMA (5 & 21) Price Crossover Strategies Performance Comparison
# Price Crossover Strategy Returns/Equity Curve
ema5strat <- ret*ema5pos
ema5stratc <- ifelse((ema5tr==1|ema5tr==-1)&ema5pos!=Lag(ema5pos),(ret-0.01)*ema5pos,ret*ema5pos)
ema21strat <- ret*ema21pos
ema21stratc <- ifelse((ema21tr==1|ema21tr==-1)&ema21pos!=Lag(ema21pos),(ret-0.01)*ema21pos,ret*ema21pos)
# Price Crossover Strategy Performance Comparison
emacomp <- cbind(ema5strat,ema5stratc,ema21strat,ema21stratc,bhstrat)
colnames(emacomp) <- c("EMA(5)","EMA(5) TC","EMA(21)","EMA(21) TC","BH")
table.AnnualizedReturns(emacomp)
charts.PerformanceSummary(emacomp)

# Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Strategies Performance Comparison
# Double Crossover Strategy Returns/Equity Curve
smastrat <- ret*smapos
smastratc <- ifelse((smatr==1|smatr==-1)&smapos!=Lag(smapos),(ret-0.01)*smapos,ret*smapos)
emastrat <- ret*emapos
emastratc <- ifelse((ematr==1|ematr==-1)&emapos!=Lag(emapos),(ret-0.01)*emapos,ret*emapos)
# Double Crossover Strategy Performance Comparison
macomp <- cbind(smastrat,smastratc,emastrat,emastratc,bhstrat)
colnames(macomp) <- c("SMA(5,21)","SMA(5,21) TC","EMA(5,21)","EMA(5,21) TC","BH")
table.AnnualizedReturns(macomp)
charts.PerformanceSummary(macomp)

# 6.2. Bollinger Bands BB(20,2) Strategy Performance Comparison
# Bands Strategy Returns/Equity Curve
bbstrat <- ret*bbpos
bbstratc <- ifelse((bbtr==1|bbtr==-1)&bbpos!=Lag(bbpos),(ret-0.01)*bbpos,ret*bbpos)
# Bands Strategy Performance Comparison Tables
bbcomp <- cbind(bbstrat,bbstratc,bhstrat)
colnames(bbcomp) <- c("BB(20,2)","BB(20,2) TC","BH")
table.AnnualizedReturns(bbcomp)
charts.PerformanceSummary(bbcomp)

# 6.3. Parabolic Stop And Reverse SAR(0.02,0.2) Strategy Performance Comparison
# Stop And Reverse Strategy Returns/Equity Curve
sarstrat <- ret*sarpos
sarstratc <- ifelse((sartr==1|sartr==-1)&sarpos!=Lag(sarpos),(ret-0.01)*sarpos,ret*sarpos)
# Stop And Reverse Strategy Performance Comparison
sarcomp <- cbind(sarstrat,sarstratc,bhstrat)
colnames(sarcomp) <- c("SAR(0.02,0.2)","SAR(0.02,0.2) TC","BH")
table.AnnualizedReturns(sarcomp)
charts.PerformanceSummary(sarcomp)

# 6.4. Average Directional Movement Index ADX(14) Strategy Performance Comparison
# Band and Double Crossover Strategy Returns/Equity Curve
adxstrat <- ret*adxpos
adxstratc <- ifelse((adxtr==1|adxtr==-1)&adxpos!=Lag(adxpos),(ret-0.01)*adxpos,ret*adxpos)
# Band and Double Crossover Strategy Performance Comparison
adxcomp <- cbind(adxstrat,adxstratc,bhstrat)
colnames(adxcomp) <- c("ADX(14)","ADX(14) TC","BH")
table.AnnualizedReturns(adxcomp)
charts.PerformanceSummary(adxcomp)

# 6.5. Commodity Channel Index CCI(20,0.015) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
ccistrat <- ret*ccipos
ccistratc <- ifelse((ccitr==1|ccitr==-1)&ccipos!=Lag(ccipos),(ret-0.01)*ccipos,ret*ccipos)
# Bands Crossover Strategy Performance Comparison
ccicomp <- cbind(ccistrat,ccistratc,bhstrat)
colnames(ccicomp) <- c("CCI(20,0.015)","CCI(20,0.015) TC","BH")
table.AnnualizedReturns(ccicomp)
charts.PerformanceSummary(ccicomp)

# 6.6. Moving Averages Covergence/Divergence MACD(12,26,9) Strategies Performance Comparison
# Signal and Centerline Strategy Returns/Equity Curve
smacdstrat <- ret*smacdpos
smacdstratc <- ifelse((smacdtr==1|smacdtr==-1)&smacdpos!=Lag(smacdpos),(ret-0.01)*smacdpos,ret*smacdpos)
cmacdstrat <- ret*cmacdpos
cmacdstratc <- ifelse((cmacdtr==1|cmacdtr==-1)&cmacdpos!=Lag(cmacdpos),(ret-0.01)*cmacdpos,ret*cmacdpos)
# Signal and Centerline Strategy Performance Comparison
macdcomp <- cbind(smacdstrat,smacdstratc,cmacdstrat,cmacdstratc,bhstrat)
colnames(macdcomp) <- c("MACD(12,26,9)S","MACD(12,26,9)S TC","MACD(12,26)C","MACD(12,26)C TC","BH")
table.AnnualizedReturns(macdcomp)
charts.PerformanceSummary(macdcomp)

# 6.7. Rate Of Change ROC(21) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
rocstrat <- ret*rocpos
rocstratc <- ifelse((roctr==1|roctr==-1)&rocpos!=Lag(rocpos),(ret-0.01)*rocpos,ret*rocpos)
# Bands Crossover Strategy Performance Comparison
roccomp <- cbind(rocstrat,rocstratc,bhstrat)
colnames(roccomp) <- c("ROC(21)","ROC(21) TC","BH")
table.AnnualizedReturns(roccomp)
charts.PerformanceSummary(roccomp)

# 6.8. Relative Strength Index RSI(14) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
rsistrat <- ret*rsipos
rsistratc <- ifelse((rsitr==1|rsitr==-1)&rsipos!=Lag(rsipos),(ret-0.01)*rsipos,ret*rsipos)
# Bands Crossover Strategy Performance Comparison
rsicomp <- cbind(rsistrat,rsistratc,bhstrat)
colnames(rsicomp) <- c("RSI(14)","RSI(14) TC","BH")
table.AnnualizedReturns(rsicomp)
charts.PerformanceSummary(rsicomp)

# 6.9. Stochastic Momentum Index SMI(13,2,25,9) Strategy Performance Comparison
# Signal Strategy Returns/Equity Curve
smistrat <- ret*smipos
smistratc <- ifelse((smitr==1|smitr==-1)&smipos!=Lag(smipos),(ret-0.01)*smipos,ret*smipos)
# Signal Strategy Performance Comparison
smicomp <- cbind(smistrat,smistratc,bhstrat)
colnames(smicomp) <- c("SMI(13,2,25)","SMI(13,2,25) TC","BH")
table.AnnualizedReturns(smicomp)
charts.PerformanceSummary(smicomp)

# 6.10. Williams %R(14) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
wprstrat <- ret*wprpos
wprstratc <- ifelse((wprtr==1|wprtr==-1)&wprpos!=Lag(wprpos),(ret-0.01)*wprpos,ret*wprpos)
# Bands Crossover Strategy Performance Comparison
wprcomp <- cbind(wprstrat,wprstratc,bhstrat)
colnames(wprcomp) <- c("WPR(14)","WPR(14) TC","BH")
table.AnnualizedReturns(wprcomp)
charts.PerformanceSummary(wprcomp)

# 6.11. Commodity Channel Index CCI(20,0.015) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
ccismastrat <- ret*ccismapos
ccismastratc <- ifelse((ccismatr==1|ccismatr==-1)&ccismapos!=Lag(ccismapos),(ret-0.01)*ccismapos,ret*ccismapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
ccismacomp <- cbind(ccismastrat,ccismastratc,bhstrat)
colnames(ccismacomp) <- c("SMA(5),CCI(20,0.015)","SMA(5),CCI(20,0.015) TC","BH")
table.AnnualizedReturns(ccismacomp)
charts.PerformanceSummary(ccismacomp)

# 6.12. Rate Of Change ROC(21) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
rocsmastrat <- ret*rocsmapos
rocsmastratc <- ifelse((rocsmatr==1|rocsmatr==-1)&rocsmapos!=Lag(rocsmapos),(ret-0.01)*rocsmapos,ret*rocsmapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
rocsmacomp <- cbind(rocsmastrat,rocsmastratc,bhstrat)
colnames(rocsmacomp) <- c("SMA(5),ROC(21)","SMA(5),ROC(21) TC","BH")
table.AnnualizedReturns(rocsmacomp)
charts.PerformanceSummary(rocsmacomp)

# 6.13. Relative Strength Index RSI(14) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
rsismastrat <- ret*rsismapos
rsismastratc <- ifelse((rsismatr==1|rsismatr==-1)&rsismapos!=Lag(rsismapos),(ret-0.01)*rsismapos,ret*rsismapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
rsismacomp <- cbind(rsismastrat,rsismastratc,bhstrat)
colnames(rsismacomp) <- c("SMA(5),RSI(14)","SMA(5),RSI(14) TC","BH")
table.AnnualizedReturns(rsismacomp)
charts.PerformanceSummary(rsismacomp)

# 6.14. Stochastic Momentum Index SMI(13,2,25,9) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
smismastrat <- ret*smismapos
smismastratc <- ifelse((smismatr==1|smismatr==-1)&smismapos!=Lag(smismapos),(ret-0.01)*smismapos,ret*smismapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
smismacomp <- cbind(smismastrat,smismastratc,bhstrat)
colnames(smismacomp) <- c("SMA(5),SMI(13,2,25)","SMA(5),SMI(13,2,25) TC","BH")
table.AnnualizedReturns(smismacomp)
charts.PerformanceSummary(smismacomp)

# 6.15. Williams %R(14) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
wprsmastrat <- ret*wprsmapos
wprsmastratc <- ifelse((wprsmatr==1|wprsmatr==-1)&wprsmapos!=Lag(wprsmapos),(ret-0.01)*wprsmapos,ret*wprsmapos)
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
wprsmacomp <- cbind(wprsmastrat,wprsmastratc,bhstrat)
colnames(wprsmacomp) <- c("SMA(5),WPR(14)","SMA(5),WPR(14) TC","BH")
table.AnnualizedReturns(wprsmacomp)
charts.PerformanceSummary(wprsmacomp)
