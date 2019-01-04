## ARQUIVO PARA CONSTRUÇÃO DAS INFORMACOES QUE SERAO APRESENTADAS NO RELATORIO
source('~/Documents/GitHub/rprevisaoacoes/R/getEstrategiaInvestimento.R')

# Lista de Acoes:
acoes <- c("BBDC4.SA","RADL3.SA","LAME4.SA","BRKM5.SA","OIBR4.SA",
           "PETR4.SA","TIMP3.SA","BBAS3.SA","USIM5.SA","VALE5.SA")
## ANALISE DA ACAO BBDC4.SA ##
saida <- data.frame()
BBDC4 <- c("Nenhuma",0.2203)
RADL3 <- c("Nenhuma",0.3593)
LAME4 <- c("SAR(0.02,0.2)", 0.2778)
BRKM5 <- c("Nenhuma",0.2321)
OIBR4 <- c("CCI(20,0.015) & SMA(5)",0.7164)
PETR4 <- c("ADX(14)",0.1337)
TIMP3 <- c("Nenhuma",0.5310)
BBAS3 <- c("ADX(14)",0.3666)
USIM5 <- c("Nenhuma",1.2505)
VALE5 <- c("BB(20,2)",0.4006)

saida <- rbind(BBDC4,RADL3,LAME4,BRKM5,OIBR4,PETR4, TIMP3, BBAS3, USIM5, VALE5)
colnames(saida) <- c("Estrategia","Benchmark")


barChart(stockData,theme=chartTheme("white"))
addSAR(accel=c(0.02, 0.2))
# Manual Chart
plot(Cl(stockData),main="Parabolic Stop and Reverse SAR(0.02,0.2)")
points(sar,col="darkblue")
legend("bottomright",col=c("black","darkblue"),lty=1,legend=c("stockData","SAR(0.02,0.2)"),cex=0.6)

barChart(stockData,theme=chartTheme("white"))
addSMA(n=5,col="darkblue")
addSMA(n=21,col="darkred")
legend("bottomright",col=c("darkblue","darkred"),lty=1,legend=c("SMA(5)","SMA(21)"),cex=0.6)

barChart(stockData,theme=chartTheme("white"))
addSAR(accel=c(0.02, 0.2))
# Stop And Reverse Trading Signals
sartr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sar)&Cl(stockData)>sar,1,ifelse(Lag(Cl(stockData))>Lag(sar)&Cl(stockData)<sar,-1,0)))
sartr[is.na(sartr)] <- 0
