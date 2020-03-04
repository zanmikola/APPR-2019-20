source("lib/libraries.r", encoding="UTF-8")

toplogredni_p <- read_csv("podatki/toplogredni.csv",locale=locale(encoding="Windows-1250"), na = c("...","-"))
toplogredni_p[is.na(toplogredni_p)] <- 0

dejavnosti1 <- as.vector(toplogredni_p$`DEJAVNOST`)


toplogredni_po_dejavnosti <- toplogredni_p %>% slice(grep("^[A-Z] ", dejavnosti1))

dejavnosti <- dejavnosti1[grep("^[A-Z] ", dejavnosti1)]


izpust_co2 <- toplogredni_po_dejavnosti %>% select(1,seq(2, 33, 4))
izpust_n2o <- toplogredni_po_dejavnosti %>% select(1,seq(3, 33, 4))
izpust_ch4 <- toplogredni_po_dejavnosti %>% select(1,seq(4, 33, 4))
izpust_sf6 <- toplogredni_po_dejavnosti %>% select(1,seq(5, 33, 4))

izpust_co2[,-1] <- izpust_co2[,-1] * 1000

izpust_co2 <- izpust_co2 %>% rename('2010 CO2 (Mg)'= '2010 CO2 (Gg)',
                                    '2011 CO2 (Mg)'= '2011 CO2 (Gg)',
                                    '2012 CO2 (Mg)'= '2012 CO2 (Gg)',
                                    '2013 CO2 (Mg)'= '2013 CO2 (Gg)',
                                    '2014 CO2 (Mg)'= '2014 CO2 (Gg)',
                                    '2015 CO2 (Mg)'= '2015 CO2 (Gg)',
                                    '2016 CO2 (Mg)'= '2016 CO2 (Gg)',
                                    '2017 CO2 (Mg)'= '2017 CO2 (Gg)')

test <- gather(izpust_co2, 'leto', 'izpust', 2:9)
test$leto <- gsub('CO2\\s.Mg.', ' ', test$leto) 

skupni_izpust <- izpust_ch4[-1] + izpust_co2[-1] + izpust_n2o[-1] + izpust_sf6[-1]

colnames(skupni_izpust) <- gsub('CO2', 'Skupno',colnames(izpust_co2[-1]))

skupni_izpust <- skupni_izpust %>% mutate('DEJAVNOSTI'=dejavnosti1[grep("^[A-Z] ", dejavnosti1)])
skupni_izpust <- skupni_izpust[c(9,1,2,3,4,5,6,7,8)]
#as.matrix(skupni_izpust)
#skupni_izpust <- as.data.frame(t(as.matrix(skupni_izpust)))
#colnames(skupni_izpust) <- dejavnosti1[grep("^[A-Z] ", dejavnosti1)]
#skupni_izpust <-skupni_izpust[-1,]
#rownames(skupni_izpust) <- c (1:8)




izpusti <- data.frame(rep(2010,21),dejavnosti,as.vector(skupni_izpust[,2]))
izpusti[22:42,] <- c(rep(2011,21),dejavnosti,as.vector(skupni_izpust[,3]))
izpusti[43:63,] <- c(rep(2012,21),dejavnosti,as.vector(skupni_izpust[,4]))
izpusti[64:84,] <- c(rep(2013,21),dejavnosti,as.vector(skupni_izpust[,5]))
izpusti[85:105,] <- c(rep(2014,21),dejavnosti,as.vector(skupni_izpust[,6]))
izpusti[106:126,] <- c(rep(2015,21),dejavnosti,as.vector(skupni_izpust[,7]))
izpusti[127:147,] <- c(rep(2016,21),dejavnosti,as.vector(skupni_izpust[,8]))
izpusti[148:168,] <- c(rep(2017,21),dejavnosti,as.vector(skupni_izpust[,9]))

colnames(izpusti) <- c('leto', 'dejavnost', 'izpust topogrednih plinov (Mg)')










okoljske_investicije <- read_csv('podatki/okoljske_invest.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))

okoljske_investicije <- okoljske_investicije %>% rename( '2010,\n (mio EUR)'= `2010 Namen - SKUPAJ`,
                                                        '2011,\n (mio EUR)'= `2011 Namen - SKUPAJ`,
                                                        '2012,\n (mio EUR)'= `2012 Namen - SKUPAJ`,
                                                        '2013,\n (mio EUR)'= `2013 Namen - SKUPAJ`,
                                                        '2014,\n (mio EUR)'= `2014 Namen - SKUPAJ`,
                                                        '2015,\n (mio EUR)'= `2015 Namen - SKUPAJ`,
                                                        '2016,\n (mio EUR)'= `2016 Namen - SKUPAJ`,
                                                        '2017,\n (mio EUR)'= `2017 Namen - SKUPAJ`)

okoljske_investicije[,-1] <- okoljske_investicije[,-1] / 1000

dejavnosti2 <- as.vector(okoljske_investicije$`SKD DEJAVNOST`)

okoljske_investicije_po_dejavnosti <- okoljske_investicije %>% slice(grep("^[A-Z] ", dejavnosti2))

okoljske_investicije_po_dejavnosti[20,] <- c("T Dejavnost gospodinjstev z zaposlenim hišnim osebjem; proizvodnja za lastno rabo
                                            ",NA,NA,NA,NA,NA,NA,NA,NA)
okoljske_investicije_po_dejavnosti[21,] <- c('U Dejavnost eksteritorialnih organizacij in teles'
                                             ,NA,NA,NA,NA,NA,NA,NA,NA)

okoljski_davki <- read_csv('podatki/okolje_davki.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
okoljski_davki %>% rename( '2010,\n (mio EUR)'= `2010`,
                            '2011,\n (mio EUR)'= `2011`,
                            '2012,\n (mio EUR)'= `2012`,
                            '2013,\n (mio EUR)'= `2013`,
                            '2014,\n (mio EUR)'= `2014`,
                            '2015,\n (mio EUR)'= `2015`,
                            '2016,\n (mio EUR)'= `2016`,
                            '2017,\n (mio EUR)'= `2017`)

okoljski_davki <- okoljski_davki[c(-1,-23,-24),]

BDP <- read_csv('podatki/bdp.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))

dejavnosti3 <- as.vector(BDP$`DEJAVNOSTI/TRANSAKCIJE`)

BDP_po_dejavnosti <- BDP %>% slice(grep("^[A-Z] ", dejavnosti3))

invest_regije <- read_csv("podatki/invest_regije.csv", locale=locale(encoding="Windows-1250"), na=c("...","-"))

invest_regije[,-1] <- invest_regije[,-1] / 1000

okoljski_davki <-mutate_each(okoljski_davki, funs(toupper))
BDP_po_dejavnosti <- mutate_each(BDP_po_dejavnosti, funs(toupper))
invest_regije <- mutate_each(invest_regije, funs(toupper))
toplogredni_po_dejavnosti <- mutate_each(toplogredni_po_dejavnosti, funs(toupper))






#______________


regije <- read.csv2(file="podatki/2711801.csv", header=TRUE, fileEncoding="Windows-1250", skip=1, dec= '.')

