######
# Trabalhando com Dados do GetHFData

library(GetHFData)
library(tidyverse)

datas <- c("2017-02-13", "2017-02-10", "2017-02-09", "2017-02-08", "2017-02-07")
ftp <- list()

for(i in 1:length(datas)){
  ftp[[i]] <- ghfd_get_available_tickers_from_ftp(datas[i], "BMF")
  ftp[[i]] <- ftp[[i]][1:5, 1:2]
}
names(ftp) <- datas
FTP <- do.call(rbind, ftp)

FTP <- FTP %>%
  mutate(data = row.names(.),
         data = gsub("\\.\\d", "", data),
         data = as.Date(data),
         tickers = as.character(tickers))

ggplot(FTP, aes(x = data, y = n.trades, group = tickers)) +
  geom_line(aes(colour = tickers)) +
  theme_classic()
