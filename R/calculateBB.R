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
#' Avalia o Desempenho da Acao com Base na Analise de Bandas de Bollinger
#' @param n Number of periods for moving average. Por padrao 20.
#' @param sd The number of standard deviations to use. Por padrao 2.
#' @param inicio Data de Inicio da Avaliacao no formato YYYY-MM-DD. Por padrao "2017-01-12"
#' @param fim Data Fim da Avaliacao no formato YYYY-MM-DD. Por padrao "2018-01-11"
#' @param acao Acao que sera avaliada seguindo as medias Moveis. Por padrao "PETR3.SA"
#' @param COMISSAO Comissao para operar o ativo (compra e venda). Por padrao 0.01
#' @param ret Retorno Diario da acao para efeito de comparacao de Desempenho
#' @return Desempenho da Ação com Base na Analise do Indicador Bandas de Bolinger
#' @examples
#' calculateBB() Avaliacao com Numero de Periodos 20 e Desvio Padrao 2 da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' calculateBB(n = 10) Avaliacao com Numero de Periodos 10 e Desvio Padrao 2 da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' @import quantmod
#' @import PerformanceAnalytics
#' @import TTR
calculateBB <-  function(n = 20,
                         sd = 2,
                         inicio = "2017-01-12",
                         fim = "2018-01-11",
                         stockData = NA,
                         COMISSAO = 0.01,
                         ret = NA) {

  if (length(stockData) == 1) {
    stockData <- na.omit(getSymbols("PETR3.SA", src = "yahoo", from = inicio, to = fim, auto.assign = FALSE))
  }

  if(is.na(ret)) {
    ret <- dailyReturn(Cl(stockData),type="arithmetic")
  }


# Bollinger Bands BB(n,sd)
bb <- BBands(HLC(stockData),n=n,sd=sd)

## Bollinger Bands BB(20,2)  Crossover Trading Signals
# Sinal de Compra: Previa(Fechamento < Lower Band) -> Atual(Fechamento > Lower Band)
# Sinal de Venda: Previa(Fechamento < Upper Band) -> Atual(Fechamento > Upper Band)

bbtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(bb[,1])&Cl(stockData)>bb[,1],1,ifelse(Lag(Cl(stockData))<Lag(bb[,3])&Cl(stockData)>bb[,3],-1,0)))
bbtr[is.na(bbtr)] <- 0


## Bollinger Bands BB(20,2) Trading Strategy
bbpos <- ifelse(bbtr>1,0,1)
for(i in 1:length(Cl(stockData))){bbpos[i] <- ifelse(bbtr[i]==1,1,ifelse(bbtr[i]==-1,0,bbpos[i-1]))}
bbpos[is.na(bbpos)] <- 1
bbposcomp <- cbind(Cl(stockData),bb[,1],bb[,3],bbtr,bbpos)
colnames(bbposcomp) <-c("Close",'lower',"upper","bbtr","bbpos")

# Bollinger Bands BB(20,2) Strategy Performance Comparison
# Bands Strategy Returns/Equity Curve
ret[1] <- 0
bhstrat <- ret

bbstrat <- ret*bbpos
bbstratc <- ifelse((bbtr==1|bbtr==-1)&bbpos!=Lag(bbpos),(ret-COMISSAO)*bbpos,ret*bbpos)
# Bands Strategy Performance Comparison Tables
bbcomp <- cbind(bbstrat,bbstratc,bhstrat)
colnames(bbcomp) <- c("BB(20,2)","BB(20,2) TC","BH")
retorno <- table.AnnualizedReturns(bbcomp)
charts.PerformanceSummary(bbcomp)
retorno

} # Fim do Metodo calculateBB
