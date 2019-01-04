##############################################################
# Analise Tecnica como Insumo para Previsoes de Stock Market #
# (c) Rodrigo Almeida - rodrigo.almeida@gmail.com            #
#                                                            #
# Esse codigo tem por objetivo experimentar estrategias de   #
# trading com base no historico de um ativo. E necessario    #
# compreender os objetivos de cada estrategia e selecionar   #
# a melhor estrategia para o ativo.                          #
# IMPORTANTE: O foco são em estratégias do tipo LAGGING      #
##############################################################


#'
#' Avalia o Desempenho da Acao com Base de Cruzamento de Bandas EMA e SMA
#' @param X_SMA Janela Curta SMA. Por padrao 5.
#' @param Y_SMA Janela Longa SMA. Por padrao 21.
#' @param X_EMA Janela Curta EMA. Por padrao 5.
#' @param Y_EMA Janela Longa EMA. Por padrao 21.
#' @param inicio Data de Inicio da Avaliacao no formato YYYY-MM-DD. Por padrao "2017-01-12"
#' @param fim Data Fim da Avaliacao no formato YYYY-MM-DD. Por padrao "2018-01-11"
#' @param acao Acao que sera avaliada seguindo as medias Moveis. Por padrao "PETR3.SA"
#' @param COMISSAO Comissao para operar o ativo (compra e venda). Por padrao 0.01
#' @param ret Retorno Diario da acao para efeito de comparacao de Desempenho
#' @return Desempenho da Ação com Base em Cruzamento de Bandas EMA e SMA
#' @examples
#' calculateCrossBandsEMASAR() Avaliacao com Janela Curta de 5 e Janela longa de 21 para EMA e SMA da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' @import quantmod
#' @import PerformanceAnalytics
#' @import TTR
calculateCrossBandsEMASMA <-  function(X_SMA = 5,
                                       Y_SMA = 21,
                                       X_EMA = 5,
                                       Y_EMA = 21,
                          inicio = "2017-01-12",
                          fim = "2018-01-11",
                          stockData = NA,
                          COMISSAO = 0.01,
                          ret = NA) {

  if (is.na(stockData)) {
    stockData <- na.omit(getSymbols("PETR3.SA", src = "yahoo", from = inicio, to = fim, auto.assign = FALSE))
  }


  if(is.na(ret)) {
    ret <- dailyReturn(Cl(stockData),type="arithmetic")
  }

  # Simple Moving Average
  smaX <- SMA(Cl(stockData),n=X_SMA)
  smaY <- SMA(Cl(stockData),n=Y_SMA)

  # Exponential Moving Average
  emaX <- EMA(Cl(stockData),n=X_EMA)
  emaY <- EMA(Cl(stockData),n=Y_EMA)


  ## SMA(5 & 21) Price Crossover Trading Signals
  #  Sinal de Compra: Previa(Fechamento < smaX) -> Atual(Fechamento > smaX)
  #  Sinal de Venda: Previa(Fechamento > smaX) -> Atual(Fechamento < smaX)

  smaXtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(smaX)&Cl(stockData)>smaX,1,ifelse(Lag(Cl(stockData))>Lag(smaX)&Cl(stockData)<smaX,-1,0)))
  smaXtr[is.na(smaXtr)] <- 0
  smaYtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(smaY)&Cl(stockData)>smaY,1,ifelse(Lag(Cl(stockData))>Lag(smaY)&Cl(stockData)<smaY,-1,0)))
  smaYtr[is.na(smaYtr)] <- 0

  ## Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Signals
  #  Sinal de Compra: Previa(emaX < emaY) -> Atual(emaX > emaY)
  #  Sinal de Venda: Previa(emaX > emaY) -> Atual(emaX < emaY)

  emaXtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(emaX)&Cl(stockData)>emaX,1,ifelse(Lag(Cl(stockData))>Lag(emaX)&Cl(stockData)<emaX,-1,0)))
  emaXtr[is.na(emaXtr)] <- 0
  emaYtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(emaY)&Cl(stockData)>emaY,1,ifelse(Lag(Cl(stockData))>Lag(emaY)&Cl(stockData)<emaY,-1,0)))
  emaYtr[is.na(emaYtr)] <- 0

  ## Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Trading Signals
  smatr <- Lag(ifelse(Lag(smaX)<Lag(smaY)&smaX>smaY,1,ifelse(Lag(smaX)>Lag(smaY)&smaX<smaY,-1,0)))
  smatr[is.na(smatr)] <- 0
  ematr <- Lag(ifelse(Lag(emaX)<Lag(emaY)&emaX>emaY,1,ifelse(Lag(emaX)>Lag(emaY)&emaX<emaY,-1,0)))
  ematr[is.na(ematr)] <- 0

  #Simple Moving Averages SMA(5 & 21) Price Crossover Trading Strategies
  smaXpos <- ifelse(smaXtr>1,0,1)
  for(i in 1:length(Cl(stockData))){smaXpos[i] <- ifelse(smaXtr[i]==1,1,ifelse(smaXtr[i]==-1,0,smaXpos[i-1]))}
  smaXpos[is.na(smaXpos)] <- 1
  smaXposcomp <- cbind(Cl(stockData),smaX,smaXtr,smaXpos)
  colnames(smaXposcomp) <-c("Close",'smaX',"smaXtr","smaXpos")
  smaYpos <- ifelse(smaYtr>1,0,1)
  for(i in 1:length(Cl(stockData))){smaYpos[i] <- ifelse(smaYtr[i]==1,1,ifelse(smaYtr[i]==-1,0,smaYpos[i-1]))}
  smaYpos[is.na(smaYpos)] <- 1
  smaYposcomp <- cbind(Cl(stockData),smaY,smaYtr,smaYpos)
  colnames(smaYposcomp) <-c("Close",'smaY',"smaYtr","smaYpos")


  ## Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Strategies
  emaXpos <- ifelse(emaXtr>1,0,1)
  for(i in 1:length(Cl(stockData))){emaXpos[i] <- ifelse(emaXtr[i]==1,1,ifelse(emaXtr[i]==-1,0,emaXpos[i-1]))}
  emaXpos[is.na(emaXpos)] <- 1
  emaXposcomp <- cbind(Cl(stockData),emaX,emaXtr,emaXpos)
  colnames(emaXposcomp) <-c("Close",'emaX',"emaXtr","emaXpos")
  emaYpos <- ifelse(emaYtr>1,0,1)
  for(i in 1:length(Cl(stockData))){emaYpos[i] <- ifelse(emaYtr[i]==1,1,ifelse(emaYtr[i]==-1,0,emaYpos[i-1]))}
  emaYpos[is.na(emaYpos)] <- 1
  emaYposcomp <- cbind(Cl(stockData),emaY,emaYtr,emaYpos)
  colnames(emaYposcomp) <-c("Close",'emaY',"emaYtr","emaYpos")



  ## Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Trading Strategies
  smapos <- ifelse(smatr>1,0,1)
  for(i in 1:length(Cl(stockData))){smapos[i] <- ifelse(smatr[i]==1,1,ifelse(smatr[i]==-1,0,smapos[i-1]))}
  smapos[is.na(smapos)] <- 1
  smaposcomp <- cbind(smaX,smaY,smatr,smapos)
  colnames(smaposcomp) <-c("smaX",'smaY',"smatr","smapos")
  emapos <- ifelse(ematr>1,0,1)
  for(i in 1:length(Cl(stockData))){emapos[i] <- ifelse(ematr[i]==1,1,ifelse(ematr[i]==-1,0,emapos[i-1]))}
  emapos[is.na(emapos)] <- 1
  emaposcomp <- cbind(emaX,emaY,ematr,emapos)
  colnames(emaposcomp) <-c("emaX",'emaY',"ematr","emapos")

  ret[1] <- 0
  bhstrat <- ret
  # Para calcular o retorno da estrategia, multiplica o valor obtido vezes a condição se haviamos comprado ou nao a acao
  smaXstrat <- ret*smaXpos
  # TC para Trading Comissions
  smaXstratc <- ifelse((smaXtr==1|smaXtr==-1)&smaXpos!=Lag(smaXpos),(ret-COMISSAO)*smaXpos,ret*smaXpos)
  smaYstrat <- ret*smaYpos
  smaYstratc <- ifelse((smaYtr==1|smaYtr==-1)&smaYpos!=Lag(smaYpos),(ret-COMISSAO)*smaYpos,ret*smaYpos)

  emaXstrat <- ret*emaXpos
  emaXstratc <- ifelse((emaXtr==1|emaXtr==-1)&emaXpos!=Lag(emaXpos),(ret-COMISSAO)*emaXpos,ret*emaXpos)
  emaYstrat <- ret*emaYpos
  emaYstratc <- ifelse((emaYtr==1|emaYtr==-1)&emaYpos!=Lag(emaYpos),(ret-COMISSAO)*emaYpos,ret*emaYpos)

  # Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Strategies Performance Comparison
  # Double Crossover Strategy Returns/Equity Curve
  smastrat <- ret*smapos
  smastratc <- ifelse((smatr==1|smatr==-1)&smapos!=Lag(smapos),(ret-COMISSAO)*smapos,ret*smapos)
  emastrat <- ret*emapos
  emastratc <- ifelse((ematr==1|ematr==-1)&emapos!=Lag(emapos),(ret-COMISSAO)*emapos,ret*emapos)
  # Double Crossover Strategy Performance Comparison
  macomp <- cbind(smastrat,smastratc,emastrat,emastratc,bhstrat)
  colnames(macomp) <- c("SMA(X_SMA,Y_SMA)","SMA(X_SMA,Y_SMA) TC","EMA(X_EMA,Y_EMA)","EMA(X_EMA,Y_EMA) TC","BH")
  retorno <- table.AnnualizedReturns(macomp)
  #charts.PerformanceSummary(macomp)
  retorno

} # Fim do Metodo calculateCrossBandsEMASAR
