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
#' Avalia o Desempenho da Acao com Base na Analise de Medias Moveis Simples
#' @param x Tamanho da Menor Janela de Avaliacao. Por padrão 5
#' @param y Tamanho da Maior Janela de Avaliacao. Por padrão 21
#' @param inicio Data de Inicio da Avaliacao no formato YYYY-MM-DD. Por padrao "2017-01-12"
#' @param fim Data Fim da Avaliacao no formato YYYY-MM-DD. Por padrao "2018-01-11"
#' @param acao Acao que sera avaliada seguindo as medias Moveis. Por padrao "PETR3.SA"
#' @param COMISSAO Comissao para operar o ativo (compra e venda). Por padrao 0.01
#' @param ret Retorno Diario da acao para efeito de comparacao de Desempenho
#' @return Desempenho da Ação com Base na Analise do Indicador SMA
#' @examples
#' calculateSMA() Avaliacao com Janela de 5 e 21 dias da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' calculateSMA(x = 7) Avaliacao com Janela de 7 e 21 dias da Acao PETR3 no Periodo de 12/01/2017 a 11/01/2018 com comissao 0.01 da operacao
#' @import quantmod
#' @import PerformanceAnalytics
#' @import TTR
calculateSMA <- function(x = 5,
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


# Simple Moving Average
smaX <- SMA(Cl(stockData),n=x)
smaY <- SMA(Cl(stockData),n=y)


# Single Indicator Trading Signals

#  Sinais ocorrem em momentos onde o preco das acoes cruzam alguns dos indicadores. Exemplo:
#  Price Crossover ocorrem quando o preco de fechamento das acoes cruzam a curva
#  Double Crossover quando as curvas se cruzam

## SMA(X & Y) Price Crossover Trading Signals
#  Sinal de Compra: Previa(Fechamento < SMAX) -> Atual(Fechamento > SMAX)
#  Sinal de Venda: Previa(Fechamento > SMAX) -> Atual(Fechamento < SMAX)

smaXtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(smaX)&Cl(stockData)>smaX,1,ifelse(Lag(Cl(stockData))>Lag(smaX)&Cl(stockData)<smaX,-1,0)))
smaXtr[is.na(smaXtr)] <- 0
smaYtr <- Lag(ifelse(Lag(Cl(stockData))<Lag(smaY)&Cl(stockData)>smaY,1,ifelse(Lag(Cl(stockData))>Lag(smaY)&Cl(stockData)<smaY,-1,0)))
smaYtr[is.na(smaYtr)] <- 0


## Simple Moving Averages SMA(X & Y) Price Crossover Trading Strategies
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


ret <- dailyReturn(Cl(stockData),type="arithmetic")
ret[1] <- 0
bhstrat <- ret
# Para calcular o retorno da estrategia, multiplica o valor obtido vezes a condição se haviamos comprado ou nao a acao
smaXstrat <- ret*smaXpos
# TC para Trading Comissions
smaXstratc <- ifelse((smaXtr==1|smaXtr==-1)&smaXpos!=Lag(smaXpos),(ret-COMISSAO)*smaXpos,ret*smaXpos)
smaYstrat <- ret*smaYpos
smaYstratc <- ifelse((smaYtr==1|smaYtr==-1)&smaYpos!=Lag(smaYpos),(ret-COMISSAO)*smaYpos,ret*smaYpos)
# Price Crossover Strategy Performance Comparison
smacomp <- cbind(smaXstrat,smaXstratc,smaYstrat,smaYstratc,bhstrat)
colnames(smacomp) <- c("SMA(X)","SMA(X) TC","SMA(Y)","SMA(Y) TC","BH")
#charts.PerformanceSummary(smacomp)

retorno <- table.AnnualizedReturns(smacomp)
retorno

} # Fim do Metodo calculateSMA




