# A Fibonacci retracement is a term used in technical analysis that refers to areas of support
# (price stops going lower) or resistance (price stops going higher). Fibonacci retracement levels use
# horizontal lines to indicate areas of support or resistance at the key Fibonacci levels before the
# trend continues in the original direction. These levels are created by drawing a trendline between the
# high and low and then dividing the vertical distance by the key Fibonacci ratios of
# 23.6%, 38.2%, 50%, 61.8% and 100%.

# acoes <- c("BBDC4.SA","RADL3.SA","LAME4.SA","BRKM5.SA","OIBR4.SA",
#            "PETR4.SA","TIMP3.SA","BBAS3.SA","USIM5.SA","VALE5.SA",
#            "KLBN11.SA","BBSE3.SA","UGPA3.SA","CMIG4.SA","ITSA4.SA",
#            "SBSP3.SA","ENBR3.SA","EQTL3.SA","BBDC3.SA","JBSS3.SA",
#            "CCRO3.SA","TBLE3.SA","PETR3.SA","MULT3.SA","BRML3.SA",
#            "LREN3.SA","ECOR3.SA","PCAR4.SA","VIVT4.SA")
#
# COMISSAO <- 0.01 # Comissao Sobre Operacoes
#
# acao <- tq_get("PETR4.SA", get = "stock.prices", from = from)
#
# hi <- last(Hi(acao),90)
# lo <- last(Lo(acao),90)
#
#
# FR100 <- max(hi)
# FR0 <- min(lo)
#
# last90 <- last(acao,90)
# last90$FR100 <- FR100
# last90$FR0 <- FR0
# last90$FR79 <- FR100 - (FR100 - FR0) * 0.786;
# last90$FR62 <- FR100 - (FR100 - FR0) * 0.618;
# last90$FR50 <- FR100 - (FR100 - FR0) * 0.500;
# last90$FR38 <- FR100 - (FR100 - FR0) * 0.382;
# last90$FR24 <- FR100 - (FR100 - FR0) * 0.236;




