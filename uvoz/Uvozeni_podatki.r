library(readr)
library(tidyr)
library(dplyr)
setwd('podatki/')
toplogredni_p <- read_csv("toplogredni.csv",locale=locale(encoding="Windows-1250"), na=c("...","-"))
toplogredni_p %>% rename(DEJAVNOSTI=DEJAVNOST)


izpust_co2 <- toplogredni_p %>% select(1,seq(2, 33, 4))
izpust_n2o <- toplogredni_p %>% select(1,seq(3, 33, 4))
izpust_ch4 <- toplogredni_p %>% select(1,seq(4, 33, 4))
izpust_sf6 <- toplogredni_p %>% select(1,seq(5, 33, 4))

colnames(izpust_co2)
okoljski_davki <- read_csv('okdavki.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
okoljske_investicije <- read_csv('okinvest.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
BDP <- read_csv('bdp.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
