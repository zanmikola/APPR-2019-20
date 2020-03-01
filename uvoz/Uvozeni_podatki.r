source("lib/libraries.r", encoding="UTF-8")

toplogredni_p <- read_csv("podatki/toplogredni.csv",locale=locale(encoding="Windows-1250"), na=c("...","-"))

dejavnosti1 <- as.vector(toplogredni_p$`DEJAVNOST`)


toplogredni_po_dejavnosti <- toplogredni_p %>% slice(grep("^[A-Z] ", dejavnosti1))


# izpust_co2 <- toplogredni_p %>% select(1,seq(2, 33, 4))
# izpust_n2o <- toplogredni_p %>% select(1,seq(3, 33, 4))
# izpust_ch4 <- toplogredni_p %>% select(1,seq(4, 33, 4))
# izpust_sf6 <- toplogredni_p %>% select(1,seq(5, 33, 4))


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
okoljske_investicije_po_dejavnosti[21,] <- c("U Dejavnost eksteritorialnih organizacij in teles
                                            ",NA,NA,NA,NA,NA,NA,NA,NA)

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
