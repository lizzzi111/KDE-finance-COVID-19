rm(list = ls())

library(rjson)
library(dplyr)


setwd('D:/Academics/Academics/MEMS/Semester 3/Non-Semi PM/Research/Time varying kernel densities')

data_dax <- read.csv('data/DAX.csv', head = TRUE)
data_btc <- read.csv('data/btc.csv', head = TRUE)

data_dow <- read.csv('data/dowjones.csv', head = TRUE)
data_dow <- data_dow[which(data_dow$Adj.Close != 'null'),]
data_dow$Open <- as.numeric(data_dow$Open)
data_dow$High <- as.numeric(data_dow$High)
data_dow$Low <- as.numeric(data_dow$Low)
data_dow$Close <- as.numeric(data_dow$Close)
data_dow$Adj.Close <- as.numeric(data_dow$Adj.Close)
data_dow$Volume <- as.numeric(data_dow$Volume)

data_nq <- read.csv('data/nasdaq.csv', head = TRUE)
data_nq <- data_nq[which(data_dow$Adj.Close != 'null'),]

data_snp <- read.csv('data/snp500.csv', head = TRUE)
data_snp <- data_snp[which(data_dow$Adj.Close != 'null'),]

result<- jsonlite::fromJSON('data/crix.json')
data_crix <- as.data.frame(result)
data_crix = data_crix[1616:2346,]

head(data_crix)
tail(data_crix)

data_crix$returns = 0
for(i in 2:nrow(data_crix)) {
  data_crix$returns[i]<-(data_crix$price[i]-data_crix$price[i-1])/data_crix$price[i-1]*100
}
write.csv(data_crix, 'crix.csv')


returns <- function(data) {
  data$returns <- 0
  for(i in 2:nrow(data)) {
    data$returns[i]<-(data$Adj.Close[i]-data$Adj.Close[i-1])/data$Adj.Close[i-1]*100
  }
  return(data)
}


data_dax <- returns(data_dax)
write.csv(data_dax,'returns_dax.csv')

data_btc <- returns(data_btc)
write.csv(data_btc, 'returns_btc.csv')

data_dow <- returns(data_dow)
write.csv(data_dow, 'returns_dowcsv')

data_snp <- returns(data_dow)
write.csv(data_snp, 'returns_snp500.csv')



