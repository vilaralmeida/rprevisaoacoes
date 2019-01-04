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
#' Avalia o Desempenho da Acao do Indicador Moving Average Convergence/Divergence
#' @param nFast Number of periods for fast moving average. Por padrao, 12.
#' @param nSlow Number of periods for slow moving average. Por padrao 26.
#' @param nSig  Number of periods for signal moving average. Por padrao 9.
#' @param inicio Data de Inicio da Avaliacao no formato YYYY-MM-DD. Por padrao "2017-01-12"
#' @param fim Data Fim da Avaliacao no formato YYYY-MM-DD. Por padrao "2018-01-11"
#' @param acao Acao que sera avaliada seguindo as medias Moveis. Por padrao "PETR3.SA"
#' @param COMISSAO Comissao para operar o ativo (compra e venda). Por padrao 0.01
#' @param ret Retorno Diario da acao para efeito de comparacao de Desempenho
#' @return Desempenho da Ação com Base no indicador MACD
#' @examples
#' calculateMACD() Avaliacao com Numero de Periodos Rapido 12, Lento 26 e Movimentacao de Sinal 9 da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' @import quantmod
#' @import PerformanceAnalytics
#' @import TTR
calculateMACD <-  function(            nFast = 12,
                                       nSlow = 26,
                                       nSig = 9,
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


  # Moving Averages Covergence/Divergence MACD(12,26,9)
  macd <- MACD(Cl(stockData),nFast=nFast,nSlow=nSlow,nSig=nSig)


  smacdtr <- Lag(ifelse(Lag(macd[,1])<Lag(macd[,2])&macd[,1]>macd[,2],1,ifelse(Lag(macd[,1])>Lag(macd[,2])&macd[,1]<macd[,2],-1,0)))
  smacdtr[is.na(smacdtr)] <- 0
  cmacdtr <- Lag(ifelse(Lag(macd[,1])<0&macd[,1]>0,1,ifelse(Lag(macd[,1])>0&macd[,1]<0,-1,0)))
  cmacdtr[is.na(cmacdtr)] <- 0

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


  ret[1] <- 0
  bhstrat <- ret

  smacdstrat <- ret*smacdpos
  smacdstratc <- ifelse((smacdtr==1|smacdtr==-1)&smacdpos!=Lag(smacdpos),(ret-COMISSAO)*smacdpos,ret*smacdpos)
  cmacdstrat <- ret*cmacdpos
  cmacdstratc <- ifelse((cmacdtr==1|cmacdtr==-1)&cmacdpos!=Lag(cmacdpos),(ret-COMISSAO)*cmacdpos,ret*cmacdpos)
  # Signal and Centerline Strategy Performance Comparison
  macdcomp <- cbind(smacdstrat,smacdstratc,cmacdstrat,cmacdstratc,bhstrat)
  colnames(macdcomp) <- c("MACD(12,26,9)S","MACD(12,26,9)S TC","MACD(12,26)C","MACD(12,26)C TC","BH")
  est6 <- table.AnnualizedReturns(macdcomp)
  #charts.PerformanceSummary(macdcomp)

}
