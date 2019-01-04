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

source("./R/calculateMACD.R")
source("./R/calculateCrossBandsEMASMA.R")
source("./R/calculateSAR.R")
source("./R/calculateBB.R")
source("./R/calculateEMA.R")
source("./R/calculateSMA.R")
library(PerformanceAnalytics)
library(quantmod)


#' Avalia Investimentos com Estrategias do tipo Lagging, voltadas para o vestidor com Perfil de Position
#' @param acoes Acoes utilizadas para Compor a Avaliacao. Observar Carteira de Grandes Corretoras.
#' @param dataInicio Data Inicio da Janela de Avaliacao no formato YYYY-MM-DD
#' @param dataFim Data Fim da Janela de Avaliacao no formato YYYY-MM-DD
#' @return Desempenho da Ação com Base na Analise dos Indicadores do tipo Lagging
#' @examples
#' avaliaInvestimentos() Calcula Estrategias de Investimento com Maior Probabilidade Retorno
#' @import quantmod
#' @import PerformanceAnalytics
#' @source calculateSMA
#' @source calculateEMA
#' @source calculateBB
#' @source calculateSAR
#' @source calculateCrossBandsEMASMA
#' @source calculateMACD

avaliaInvestimentos <- function(acoes = NA,
                                dataInicio = NA,
                                dataFim = NA) {

  if (length(acoes) == 1) {
  # Acoes analisadas que estao disponiveis no site yahoo finance
   acoes <- c("BBDC4.SA","RADL3.SA","LAME4.SA","BRKM5.SA","OIBR4.SA",
              "PETR4.SA","TIMP3.SA","BBAS3.SA","USIM5.SA","VALE5.SA",
              "BBSE3.SA","UGPA3.SA","CMIG4.SA","ITSA4.SA",
              "SBSP3.SA","ENBR3.SA","EQTL3.SA","BBDC3.SA","JBSS3.SA",
              "CCRO3.SA","PETR3.SA","MULT3.SA","BRML3.SA",
              "LREN3.SA","ECOR3.SA","PCAR4.SA","VIVT4.SA")
  }

  if (is.na(dataInicio)) {
    dataInicio = "2017-01-12"
  }

  if (is.na(dataFim)) {
    dataFim = "2018-08-11"
  }

  # Data.Frame com Informacoes de Retorno
  # retorno <- c()


  print("##########################################")
  nacoes <- length(acoes)
  saida <- paste("#### INICIANDO AVALIACAO DE",nacoes,"ACOES BOVESPA", sep = " ")
  print(saida)
  intervaloRetornado <- paste("Valor Anual Retornado no Periodo",dataInicio, "a", dataFim, sep = " ")
  print(intervaloRetornado)

   for (j in 1:nacoes) {
     acaoAvaliada <- paste("ACAO[",j,"]:",acoes[j], sep = " ")
     stockData <- na.omit(getSymbols(acoes[j], src = "yahoo", from = dataInicio, to = dataFim, auto.assign = FALSE))
     ret <- dailyReturn(Cl(stockData),type="arithmetic")
     est1 <- calculateSMA(stockData = stockData, ret = ret, inicio = dataInicio, fim = dataFim)
     est2 <- calculateEMA(stockData = stockData, ret = ret, inicio = dataInicio, fim = dataFim)
     est3 <- calculateBB(stockData = stockData, ret = ret, inicio = dataInicio, fim = dataFim)
     est4 <- calculateSAR(stockData = stockData, ret = ret, inicio = dataInicio, fim = dataFim)
     est5 <- calculateCrossBandsEMASMA(stockData = stockData, ret = ret, inicio = dataInicio, fim = dataFim)
     est6 <- calculateMACD(stockData = stockData, ret = ret, inicio = dataInicio, fim = dataFim)

     nomeEstrategias <- c("SMA","EMA","BB","SAR","EMASMA","MACD")

     ## Escolhendo a estrategia com melhores resultados

     listaEstrategias <- list(est1, est2, est3, est4, est5, est6)

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
         temp = FALSE
         valores = 0
         if (valorBenchmark < listaEstrategias[[i]][1,2]) {
           temp = TRUE
           valores <- listaEstrategias[[i]][1,2]
         }
         if (valorBenchmark < listaEstrategias[[i]][1,4]) {
           temp = TRUE
           if (valores < listaEstrategias[[i]][1,4] ) {
             valores <- listaEstrategias[[i]][1,4]
           }
         }

         if (temp) {
           estrategiasVencedoras <- c(estrategiasVencedoras,nomeEstrategias[i])
           valoresRetornoVencedores <- c(valoresRetornoVencedores, valores)
         }

         # Estrategias Mais Simples
       } else if (valorBenchmark < listaEstrategias[[i]][1,2]) {
         estrategiasVencedoras <- c(estrategiasVencedoras,nomeEstrategias[i])
         valoresRetornoVencedores <- c(valoresRetornoVencedores, listaEstrategias[[i]][1,2])
       }
     } # Fim do Loop de Estrategias

    print(acaoAvaliada)
    print("Estrategias Vencedoras: ")
    print(estrategiasVencedoras)
    print("Valores Retornados")
    print(valoresRetornoVencedores)



     #
     # if (length(estrategiasVencedoras) == 0) {
     #   resultado <- cbind(acoes[j], "Nenhuma", valorBenchmark)
     #   colnames(resultado) <- c("Acao","Estrategia",intervaloRetornado)
     #  } else {
     #   resultado <- cbind(acoes[j], estrategiasVencedoras, valoresRetornoVencedores)
     #   colnames(resultado) <- c("Acao","Estrategia",intervaloRetornado)
     # }

     # retorno <- cbind(retorno, resultado)
    print("----------*--------------*--------")
   } # Fim do Loop de Acoes


  print("##########################################")
  # retorno

} # Fim do Metodo avaliaInvestimentos
