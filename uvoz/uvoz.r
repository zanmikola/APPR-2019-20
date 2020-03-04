source("lib/libraries.r", encoding="UTF-8")

izpust_skupaj <- read_csv("podatki/toplogredni.csv",locale=locale(encoding="Windows-1250"), na = c("...","-"))
izpust_skupaj[is.na(izpust_skupaj)] <- 0
izpust_skupaj <- izpust_skupaj %>% slice(grep("^[A-Z] ",  as.vector(izpust_skupaj$`DEJAVNOST`)))


izpust_co2 <- izpust_skupaj %>% select(1,seq(2, 33, 4))
izpust_co2[,-1] <- izpust_co2[,-1] * 1000
izpust_co2 <- gather(izpust_co2, 'Leto', 'Izpust CO2 (Mg)', 2:9)
izpust_co2$Leto <- gsub(' CO2 \\(Gg\\)', '',izpust_co2$Leto)
izpust_co2$Leto <- as.numeric(izpust_co2$Leto)


izpust_n2o <- izpust_skupaj %>% select(1,seq(3, 33, 4))
izpust_n2o <- gather(izpust_n2o, 'Leto', 'Izpust N2O (Mg)', 2:9)
izpust_n2o$Leto <- gsub(' N2O \\(Mg\\)', '',izpust_n2o$Leto)
izpust_n2o$Leto <- as.numeric(izpust_n2o$Leto)

izpust_ch4 <- izpust_skupaj %>% select(1,seq(4, 33, 4))
izpust_ch4 <- gather(izpust_ch4, 'Leto', 'Izpust CH4 (Mg)', 2:9)
izpust_ch4$Leto <- gsub(' CH4 \\(Mg\\)', '',izpust_ch4$Leto)
izpust_ch4$Leto <- as.numeric(izpust_ch4$Leto)


izpust_sf6 <- izpust_skupaj %>% select(1,seq(5, 33, 4))
izpust_sf6 <- gather(izpust_sf6, 'Leto', 'Izpust SF6 (Mg)', 2:9)
izpust_sf6$Leto <- gsub(' SF6 \\(Mg\\)', '',izpust_sf6$Leto)
izpust_sf6$Leto <- as.numeric(izpust_sf6$Leto)

izpust_skupaj <- merge(izpust_ch4,izpust_sf6)
izpust_skupaj <- merge(izpust_skupaj,izpust_co2)
izpust_skupaj <- merge(izpust_skupaj, izpust_n2o)
izpust_skupaj <- izpust_skupaj %>% arrange(Leto)



okoljske_investicije <- read_csv('podatki/okoljske_invest.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
okoljske_investicije[is.na(okoljske_investicije)] <- 0
okoljske_investicije[,-1] <- okoljske_investicije[,-1] / 1000
okoljske_investicije <- okoljske_investicije %>% slice(grep("^[A-Z] ",  as.vector(okoljske_investicije$`SKD DEJAVNOST`)))
okoljske_investicije[20,] <- c("T Dejavnost gospodinjstev z zaposlenim hišnim osebjem; proizvodnja za lastno rabo"
                                                                 ,0,0,0,0,0,0,0,0)
okoljske_investicije[21,] <- c('U Dejavnost eksteritorialnih organizacij in teles'
                                                                 ,0,0,0,0,0,0,0,0)
okoljske_investicije <- gather(okoljske_investicije, 'Leto', 'Investicije mio €', 2:9)
okoljske_investicije$Leto <- gsub(' Namen . SKUPAJ', '',okoljske_investicije$Leto)
okoljske_investicije$Leto <- as.numeric(okoljske_investicije$Leto)
okoljske_investicije <- okoljske_investicije %>% rename(`DEJAVNOST` = `SKD DEJAVNOST`)
okoljske_investicije <- okoljske_investicije %>% mutate(DEJAVNOST=izpust_skupaj$DEJAVNOST)


okoljski_davki <- read_csv('podatki/okolje_davki.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
okoljski_davki[is.na(okoljski_davki)] <- 0
okoljski_davki <- okoljski_davki %>% slice(grep("^[A-Z] ",  as.vector(okoljski_davki$`DEJAVNOST`)))
okoljski_davki <- gather(okoljski_davki, 'Leto', 'Davki mio €', 2:9)
okoljski_davki$Leto <- gsub(' NAMEN . SKUPAJ', '',okoljski_davki$Leto)
okoljski_davki$Leto <- as.numeric(okoljski_davki$Leto)
okoljski_davki <- okoljski_davki %>% mutate(DEJAVNOST=izpust_skupaj$DEJAVNOST)

BDP <- read_csv('podatki/bdp.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
BDP[is.na(BDP)] <- 0
BDP <- BDP %>% select(1,seq(2, 17, 2))
BDP <- BDP %>% slice(grep("^[A-Z] ",  as.vector(BDP$`DEJAVNOSTI/TRANSAKCIJE`)))
BDP$`DEJAVNOSTI/TRANSAKCIJE` <- gsub('\\([0-9].*[0-9]\\)', '', BDP$`DEJAVNOSTI/TRANSAKCIJE`)
BDP <- gather(BDP, 'Leto', 'BDP mio €', 2:9)
BDP$Leto <- gsub(' Tekoče cene \\(mio EUR\\) Proizvodnja', '',BDP$Leto)
BDP$Leto <- as.numeric(BDP$Leto)
BDP <- BDP %>% rename(`DEJAVNOST` = `DEJAVNOSTI/TRANSAKCIJE`)
BDP <- BDP %>% mutate(DEJAVNOST=izpust_skupaj$DEJAVNOST)

zdruzena_tabela <- merge(izpust_skupaj, okoljske_investicije)
zdruzena_tabela <- merge(zdruzena_tabela, okoljski_davki)
zdruzena_tabela <- merge(zdruzena_tabela, BDP)
zdruzena_tabela <- zdruzena_tabela[c(2,1,3,4,5,6,7,8,9)]
zdruzena_tabela <- zdruzena_tabela %>% arrange(Leto)

zdruzena_tabela$Izpusti <- rowSums(zdruzena_tabela[3:6])
