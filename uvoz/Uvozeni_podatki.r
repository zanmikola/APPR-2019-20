library(readr)
library(tidyr)
library(dplyr)
setwd('podatki/')
toplogredni_p <- read_csv("toplogredni.csv",locale=locale(encoding="Windows-1250"), na=c("...","-"))
toplogredni_p <- toplogredni_p %>% rename(DEJAVNOSTI=DEJAVNOST)

toplogredni_po_dejavnosti <-toplogredni_p %>% slice(2,6,8,28, 30, 33, 35, 39, 45, 47, 
                                                    52, 56, 58, 64, 69, 71, 73, 76, 79,
                                                  83,85)


izpust_co2 <- toplogredni_p %>% select(1,seq(2, 33, 4))
izpust_n2o <- toplogredni_p %>% select(1,seq(3, 33, 4))
izpust_ch4 <- toplogredni_p %>% select(1,seq(4, 33, 4))
izpust_sf6 <- toplogredni_p %>% select(1,seq(5, 33, 4))

okoljske_investicije <- read_csv('okinvest.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))

okoljske_investicije <- okoljske_investicije %>% rename( '2010,\n (mio EUR)'= `2010 Investicije - SKUPAJ`,
                                                        '2011,\n (mio EUR)'= `2011 Investicije - SKUPAJ`,
                                                        '2012,\n (mio EUR)'= `2012 Investicije - SKUPAJ`,
                                                        '2013,\n (mio EUR)'= `2013 Investicije - SKUPAJ`,
                                                        '2014,\n (mio EUR)'= `2014 Investicije - SKUPAJ`,
                                                        '2015,\n (mio EUR)'= `2015 Investicije - SKUPAJ`,
                                                        '2016,\n (mio EUR)'= `2016 Investicije - SKUPAJ`,
                                                        '2017,\n (mio EUR)'= `2017 Investicije - SKUPAJ`)
okoljske_investicije[,-1] <- okoljske_investicije[,-1] /1000


okoljski_davki <- read_csv('okolje_davki.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
okoljski_davki <- okoljski_davki %>% rename( '2010,\n (mio €)'= `2010`,
                            '2011,\n (mio EUR)'= `2011`,
                            '2012,\n (mio EUR)'= `2012`,
                            '2013,\n (mio EUR)'= `2013`,
                            '2014,\n (mio EUR)'= `2014`,
                            '2015,\n (mio EUR)'= `2015`,
                            '2016,\n (mio EUR)'= `2016`,
                            '2017,\n (mio EUR)'= `2017`)
okoljski_davki <- okoljski_davki[c(-1,-24),]

BDP <- read_csv('bdp.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))

invest_regije_tisoceur<- read_csv('invest_regije.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
