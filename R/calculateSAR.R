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
#' Avalia o Desempenho da Acao com Base na Analise Parabolica Stop-and-Reverse (SAR)
#' @param x Acceleration factor. Por padrao 0.02
#' @param y Maximum acceleration factor. Por padrao 0.2
#' @param inicio Data de Inicio da Avaliacao no formato YYYY-MM-DD. Por padrao "2017-01-12"
#' @param fim Data Fim da Avaliacao no formato YYYY-MM-DD. Por padrao "2018-01-11"
#' @param acao Acao que sera avaliada seguindo as medias Moveis. Por padrao "PETR3.SA"
#' @param COMISSAO Comissao para operar o ativo (compra e venda). Por padrao 0.01
#' @param ret Retorno Diario da acao para efeito de comparacao de Desempenho
#' @return Desempenho da Ação com Base na Analise Parabolica Stop-and-Reverse (SAR)
#' @examples
#' calculateSAR() Avaliacao com Fator de Aceleracao 0.02 e Fator Maximo de Aceleracao 0.2 da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' calculateSAR(y = 0.3) Avaliacao com Fator de Aceleracao 0.02 e Fator Maximo de Aceleracao 0.3 da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' @import quantmod
#' @import PerformanceAnalytics
#' @import TTR
calculateSAR <-  function(x = 0.02,
                          y = 0.2,
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

  # Parabolic Stop and Reverse SAR(0.02,0.2)
  sar <- SAR(cbind(Hi(stockData),Lo(stockData)),accel=c(x,y))


  ## Parabolic Stop And Reverse SAR(0.02,0.2) Trading Signals
  # Sinal de Compra: Previa(Fechamento < SAR) -> Atual(Fechamento > SAR)
  # Sinal de Venda: Previa(Fechamento > SAR) -> Atual(Fechamento < SAR)

  sartr <- Lag(ifelse(Lag(Cl(stockData))<Lag(sar)&Cl(stockData)>sar,1,ifelse(Lag(Cl(stockData))>Lag(sar)&Cl(stockData)<sar,-1,0)))
  sartr[is.na(sartr)] <- 0

  ## Parabolic Stop And Reverse SAR(0.02,0.2) Trading Strategy
  sarpos <- ifelse(sartr>1,0,1)
  for(i in 1:length(Cl(stockData))){sarpos[i] <- ifelse(sartr[i]==1,1,ifelse(sartr[i]==-1,0,sarpos[i-1]))}
  sarpos[is.na(sarpos)] <- 1
  sarposcomp <- cbind(Cl(stockData),sar,sartr,sarpos)
  colnames(sarposcomp) <-c("Close","sar","sartr","sarpos")

  ret[1] <- 0
  bhstrat <- ret

  # Parabolic Stop And Reverse SAR(0.02,0.2) Strategy Performance Comparison
  # Stop And Reverse Strategy Returns/Equity Curve
  sarstrat <- ret*sarpos
  sarstratc <- ifelse((sartr==1|sartr==-1)&sarpos!=Lag(sarpos),(ret-COMISSAO)*sarpos,ret*sarpos)
  # Stop And Reverse Strategy Performance Comparison
  sarcomp <- cbind(sarstrat,sarstratc,bhstrat)
  colnames(sarcomp) <- c("SAR(0.02,0.2)","SAR(0.02,0.2) TC","BH")
  retorno <- table.AnnualizedReturns(sarcomp)
  #charts.PerformanceSummary(sarcomp)
  retorno
} # Fim do Metodo calculateSAR
