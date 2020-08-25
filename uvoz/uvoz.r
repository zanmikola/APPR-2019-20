source("lib/libraries.r", encoding="UTF-8")

izpust_skupaj <- read_csv("podatki/toplogredni.csv",locale=locale(encoding="Windows-1250"), na = c("...","-"))
izpust_skupaj[is.na(izpust_skupaj)] <- 0
izpust_skupaj <- izpust_skupaj %>% slice(grep("^[A-Z] ",  as.vector(izpust_skupaj$`DEJAVNOST`)))


izpust_co2 <- izpust_skupaj %>% dplyr::select(1,seq(2, 33, 4))
izpust_co2[,-1] <- izpust_co2[,-1] * 1000
izpust_co2 <- gather(izpust_co2, 'Leto', 'Izpust_CO2', 2:9)
izpust_co2$Leto <- gsub(' CO2 \\(Gg\\)', '',izpust_co2$Leto)
izpust_co2$Leto <- parse_integer(izpust_co2$Leto)


izpust_n2o <- izpust_skupaj %>% dplyr::select(1,seq(3, 33, 4))
izpust_n2o <- gather(izpust_n2o, 'Leto', 'Izpust_N2O', 2:9)
izpust_n2o$Leto <- gsub(' N2O \\(Mg\\)', '',izpust_n2o$Leto)
izpust_n2o$Leto <- as.numeric(izpust_n2o$Leto)

izpust_ch4 <- izpust_skupaj %>% dplyr::select(1,seq(4, 33, 4))
izpust_ch4 <- gather(izpust_ch4, 'Leto', 'Izpust_CH4', 2:9)
izpust_ch4$Leto <- gsub(' CH4 \\(Mg\\)', '',izpust_ch4$Leto)
izpust_ch4$Leto <- as.numeric(izpust_ch4$Leto)


izpust_sf6 <- izpust_skupaj %>% dplyr::select(1,seq(5, 33, 4))
izpust_sf6 <- gather(izpust_sf6, 'Leto', 'Izpust_SF6', 2:9)
izpust_sf6$Leto <- gsub(' SF6 \\(Mg\\)', '',izpust_sf6$Leto)
izpust_sf6$Leto <- as.numeric(izpust_sf6$Leto)

izpust_skupaj <- merge(izpust_ch4,izpust_sf6)
izpust_skupaj <- merge(izpust_skupaj,izpust_co2)
izpust_skupaj <- merge(izpust_skupaj, izpust_n2o)
izpust_skupaj <- izpust_skupaj %>% arrange(Leto)
izpust_skupaj <- izpust_skupaj %>% rename('Dejavnost' = 'DEJAVNOST')



okoljske_investicije <- read_csv('podatki/okoljske_invest.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
okoljske_investicije[is.na(okoljske_investicije)] <- 0
okoljske_investicije[,-1] <- okoljske_investicije[,-1] / 1000
okoljske_investicije <- okoljske_investicije %>% slice(grep("^[A-Z] ",  as.vector(okoljske_investicije$`SKD DEJAVNOST`)))
okoljske_investicije[20,] <- c("T Dejavnost gospodinjstev z zaposlenim hišnim osebjem; proizvodnja za lastno rabo"
                                                                 ,0,0,0,0,0,0,0,0)
okoljske_investicije[21,] <- c('U Dejavnost eksteritorialnih organizacij in teles'
                                                                 ,0,0,0,0,0,0,0,0)
okoljske_investicije <- gather(okoljske_investicije, 'Leto', 'Investicije', 2:9)
okoljske_investicije$Leto <- gsub(' Namen . SKUPAJ', '',okoljske_investicije$Leto)
okoljske_investicije$Leto <- as.numeric(okoljske_investicije$Leto)
okoljske_investicije$`Investicije` <- as.numeric(okoljske_investicije$'Investicije')
okoljske_investicije <- okoljske_investicije %>% rename(`DEJAVNOST` = `SKD DEJAVNOST`)
okoljske_investicije <- okoljske_investicije %>% mutate(DEJAVNOST=izpust_skupaj$DEJAVNOST)



okoljski_davki <- read_csv('podatki/okolje_davki.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
okoljski_davki[is.na(okoljski_davki)] <- 0
okoljski_davki <- okoljski_davki %>% slice(grep("^[A-Z] ",  as.vector(okoljski_davki$`DEJAVNOST`)))
okoljski_davki <- gather(okoljski_davki, 'Leto', 'Davki', 2:9)
okoljski_davki$Leto <- gsub(' NAMEN . SKUPAJ', '',okoljski_davki$Leto)
okoljski_davki$Leto <- as.numeric(okoljski_davki$Leto)
okoljski_davki <- okoljski_davki %>% mutate(DEJAVNOST=izpust_skupaj$Dejavnost)
okoljski_davki <- okoljski_davki %>% rename('Dejavnost' = 'DEJAVNOST')

bdp <- read_csv('podatki/bdp_dodana_vrednost.csv',locale=locale(encoding="Windows-1250"), na=c("z","-"))
bdp[is.na(bdp)] <- 0
bdp <- bdp %>% slice(grep("^[A-Z]",  as.vector(bdp$`DEJAVNOSTI/TRANSAKCIJE`)))
bdp$`DEJAVNOSTI/TRANSAKCIJE` <- gsub('\\([0-9].*[0-9]\\)', '', bdp$`DEJAVNOSTI/TRANSAKCIJE`)
bdp <- gather(bdp, 'Leto', 'Proizvodnja', 2:9)
bdp$Leto <- gsub(' Tekoče cene \\(mio EUR\\) Dodana vrednost', '',bdp$Leto)
bdp$Leto <- as.numeric(bdp$Leto)
bdp <- bdp %>% rename(`Dejavnost` = `DEJAVNOSTI/TRANSAKCIJE`)

bdp_skupni <- bdp %>% filter(Dejavnost %in% c('Skupaj dejavnosti', 'Neto davki na proizvode', 'Bruto domači proizvod'))
bdp_skupni <- bdp_skupni %>% arrange(Leto)

bdp <- bdp[-(grep("Skupaj dejavnosti|Neto davki na proizvode|Bruto domači proizvod", bdp$Dejavnost)),]
bdp <- bdp %>% mutate(Dejavnost=izpust_skupaj$Dejavnost)


zdruzena_tabela <- merge(izpust_skupaj, okoljske_investicije)
zdruzena_tabela <- merge(zdruzena_tabela, okoljski_davki)
zdruzena_tabela <- merge(zdruzena_tabela, bdp)
zdruzena_tabela <- zdruzena_tabela[c(2,1,3,4,5,6,7,8,9)]
zdruzena_tabela <- zdruzena_tabela %>% arrange(Leto)

zdruzena_tabela$'Izpusti_mg' <- rowSums(zdruzena_tabela[3:6])



regije <- read.csv2(file="podatki/2711801.csv", header=TRUE, fileEncoding="Windows-1250", skip=1, dec= '.')
regije <- regije %>% rename('regija' = 'STATISTIČNA.REGIJA')
regije <- regije %>% rename('Namen' = 'INVESTICIJE')
regije <- regije %>% rename('Investicije' = 'Investicije.za.varstvo.okolja')
regije <- regije %>% rename('Leto' = 'LETO')

evropa <- izpust_skupaj <- read_csv("podatki/1.csv",locale=locale(encoding="Windows-1250"), na = c("...","-"))
evropa <- evropa %>% rename('Tisoc_ton' = 'Value')
evropa <- evropa[,-3]
evropa <- evropa[,-3]
evropa <- evropa[,-3]
evropa <- evropa[,-4]
evropa <- evropa %>% rename('Drzava' = 'GEO')
evropa <- evropa %>% rename('Leto' = 'TIME')
evropa$Drzava <- gsub('Germany \\(until 1990 former territory of the FRG\\)', 'Germany',evropa$Drzava)
evropa$Drzava <- gsub('Czechia', 'Czech Republic',evropa$Drzava)





























