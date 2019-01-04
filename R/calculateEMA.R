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
#' Avalia o Desempenho da Acao com Base na Analise de Medias Moveis Exponenciais
#' @param x Tamanho da Menor Janela de Avaliacao. Por padrão 5
#' @param y Tamanho da Maior Janela de Avaliacao. Por padrão 21
#' @param inicio Data de Inicio da Avaliacao no formato YYYY-MM-DD. Por padrao "2017-01-12"
#' @param fim Data Fim da Avaliacao no formato YYYY-MM-DD. Por padrao "2018-01-11"
#' @param acao Acao que sera avaliada seguindo as medias Moveis. Por padrao "PETR3.SA"
#' @param COMISSAO Comissao para operar o ativo (compra e venda). Por padrao 0.01
#' @param ret Retorno Diario da acao para efeito de comparacao de Desempenho
#' @return Desempenho da Ação com Base na Analise do Indicador EMA
#' @examples
#' calculateEMA() Avaliacao com Janela de 5 e 21 dias da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' calculateEMA(x = 7) Avaliacao com Janela de 7 e 21 dias da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' @import quantmod
#' @import PerformanceAnalytics
#' @import TTR
calculateEMA <- function(x = 5,
                         y = 21,
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

  # Exponential Moving Average
  emaX <- EMA(Cl(stockData),n=x)
  emaY <- EMA(Cl(stockData),n=y)


  # Single Indicator Trading Signals

  #  Sinais ocorrem em momentos onde o preco das acoes cruzam alguns dos indicadores. Exemplo:
  #  Price Crossover ocorrem quando o preco de fechamento das acoes cruzam a curva
  #  Double Crossover quando as curvas se cruzam

  ## Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Signals
  #  Sinal de Compra: Previa(emaX < emaY) -> Atual(emaX > emaY)
  #  Sinal de Venda: Previa(emaX > emaY) -> Atual(emaX < emaY)

  emaXtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(emaX)&Cl(stockData)>emaX,1,ifelse(Lag(Cl(stockData))>Lag(emaX)&Cl(stockData)<emaX,-1,0)))
  emaXtr[is.na(emaXtr)] <- 0
  emaYtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(emaY)&Cl(stockData)>emaY,1,ifelse(Lag(Cl(stockData))>Lag(emaY)&Cl(stockData)<emaY,-1,0)))
  emaYtr[is.na(emaYtr)] <- 0


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



  ret[1] <- 0
  bhstrat <- ret

  # Exponential Moving Averages EMA (5 & 21) Price Crossover Strategies Performance Comparison
  # Price Crossover Strategy Returns/Equity Curve
  emaXstrat <- ret*emaXpos
  emaXstratc <- ifelse((emaXtr==1|emaXtr==-1)&emaXpos!=Lag(emaXpos),(ret-COMISSAO)*emaXpos,ret*emaXpos)
  emaYstrat <- ret*emaYpos
  emaYstratc <- ifelse((emaYtr==1|emaYtr==-1)&emaYpos!=Lag(emaYpos),(ret-COMISSAO)*emaYpos,ret*emaYpos)
  # Price Crossover Strategy Performance Comparison
  emacomp <- cbind(emaXstrat,emaXstratc,emaYstrat,emaYstratc,bhstrat)
  colnames(emacomp) <- c("EMA(5)","EMA(5) TC","EMA(21)","EMA(21) TC","BH")
  #charts.PerformanceSummary(emacomp)
  retorno <- table.AnnualizedReturns(emacomp)
  retorno
} # Fim do Metodo calculateEMA
