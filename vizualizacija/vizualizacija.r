# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                             pot.zemljevida="OB", encoding="Windows-1250")
#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
#zemljevid <- fortify(zemljevid)

# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))


tabela1 <- zdruzena_tabela %>% group_by(Leto) %>% summarise('Investicije'=sum(`Investicije`),
                                                         'Davki' =sum(`Davki`),
                                                         'Proizvodnja' = sum(`Proizvodnja`))

davki_na_proizvodnjo <- bdp_skupni %>% filter(DEJAVNOST == 'Neto davki na proizvode')
davki_na_proizvodnjo <- davki_na_proizvodnjo[-1]
davki_na_proizvodnjo <- davki_na_proizvodnjo %>% rename('Neto_davki_na_proizvode' = `Proizvodnja`)

tabela1 <- merge(tabela1, davki_na_proizvodnjo)
tabela1$'BDP' <- rowSums(tabela1[4:5])
tabela1$`Proizvodnja` <- NULL
tabela1$`Neto_davki_na_proizvode` <- NULL


graf1 <- ggplot(tabela1, aes(x=Leto)) +
        geom_line(aes(y=tabela1$`Investicije`, colour='Investicije'))+
        geom_line(aes(y= tabela1$`Davki`, colour='Davki'))+
        scale_colour_manual("", values = c("Investicije" = "blue", "Davki" = "red"))+
        ylab('mio €') +
        ggtitle('Graf davkov in investicij')

graf1

vrednost <- tabela1$`Investicije`[1] 
tabela1$`Investicije` <- tabela1$`Investicije` / vrednost * 100
vrednost <- tabela1$`Davki`[1] 
tabela1$`Davki` <- tabela1$`Davki` / vrednost * 100
vrednost <- tabela1$`BDP`[1] 
tabela1$`BDP` <- tabela1$`BDP` / vrednost * 100

graf2 <- ggplot(tabela1, aes(x=Leto)) +
  geom_line(aes(y=tabela1$`Investicije`, colour='Investicije'))+
  geom_line(aes(y= tabela1$`Davki`, colour='Davki'))+
  geom_line(aes(y=tabela1$`BDP`, colour = 'BDP'))+
  scale_colour_manual("", values = c("Investicije" = "blue", "Davki" = "red", 'BDP' = 'green'))+
  ylab('Indeksi, referenčno leto 2010') +
  ggtitle('Graf davkov in investicij')

graf2


tabela2 <- tabela1 %>% rename('BPD' = 'BDP') %>% rename('Davek'= 'Davki')
tabela2 <- tabela2[,-2]
tabela2 <- gather(tabela2, 'Namen', 'indeks', 2:3)
tabela2 <- tabela2[c('Namen','Leto','indeks')]

graf3 <- ggplot(tabela2, aes(x=Leto, y=indeks, fill=Namen)) +
  geom_bar(stat="identity", position=position_dodge())+
  ylab('Indeksi, referenčno leto 2010')+
  theme_minimal()

graf3
#-------------------------------------------------------#

tabela3  <- zdruzena_tabela %>% group_by(DEJAVNOST) %>% summarise('Investicije'=sum(`Investicije`),
                                                                      'Davki' =sum(`Davki`),
                                                                      'Proizvodnja' = sum(`Proizvodnja`),
                                                                      'Izpust' = sum(`Izpusti_mg`))

davek <- tabela3$`Davki` / tabela3$`Izpust`
investicija <- tabela3$`Investicije` / tabela3$`Izpust`

tabela3$'Davek_na_mg' <- davek * 1000000
tabela3$'Investicija_na_mg' <- investicija * 1000000
vek <- c(LETTERS[1:21])
tabela3 <- tabela3 %>% mutate(DEJAVNOST = vek)
tabela3[is.na(tabela3)] <- 0
tabela3[21,6] <- 0
tabela3 <- tabela3[-(20:21),]
tabela3 <- tabela3[-15,]
#graf4 <- ggplot(tabela3, aes(x=DEJAVNOST)) +
##        geom_point(aes(y=tabela3$`Davek_na_mg`, colour='Davek'))+
#        geom_point(aes(y=tabela3$`Investicija_na_mg`, colour='Investicije'))+
#        scale_colour_manual("", values = c("Investicije" = "blue", "Davek" = "red"))+
#        ylab('Indeski, referenčno leto 2010') +
#        ggtitle('Graf davkov in investicij') 
  

#graf4





tabela4 <- tabela3[,c(1,6,7)] 
tabela4 <- tabela4 %>% rename('Investicija' = 'Investicija_na_mg') %>% rename('Davek'= 'Davek_na_mg')
tabela4 <- gather(tabela4, 'Namen', 'eur_na_Mg', 2:3)
tabela4 <- tabela4[c('Namen','DEJAVNOST','eur_na_Mg' )]
graf5.1 <- ggplot() +
  geom_bar(stat="identity",data = tabela4,aes(x=DEJAVNOST, y=eur_na_Mg, fill=Namen),position=position_dodge())+theme_bw() +
  theme(legend.position = c(0.7, 0.5))+
  ylab('Investirani € glede na enoto izpusta v Mg')+ggtitle('Investicije v zmanjšanje izpust toplogrednih plinov')
graf5.1
graf5.2 <- ggplot()+
  geom_bar(stat="identity",data = tabela4 %>% filter(eur_na_Mg < 300),aes(x=DEJAVNOST, y=eur_na_Mg, fill=Namen),position=position_dodge())+ theme_bw() +
  theme(legend.position = c(0.7, 0.5))+
  ylab('Investirani € glede na enoto izpusta v Mg')+ggtitle('Investicije v zmanjšanje izpust toplogrednih plinov')
graf5.2
graf5 <- plot_grid(graf5.1, graf5.2, labels = "AUTO")
graf5 

regije2 <- regije %>% filter(INVESTICIJE == 'Varstvo zraka in klime')
regije2 <- regije2 %>% group_by(LETO) %>% summarise('Investicije'=sum(`Investicije.za.varstvo.okolja`))
regije2 <- regije2 %>% rename('INV' = 'Investicije')
regije2 <- regije2 %>% rename('Leto' = 'LETO')
tabela4 <- zdruzena_tabela %>% group_by(Leto) %>% summarise('Investicije'=sum(`Investicije`))
tabela4[,2] <- tabela4[,2] * 1000

invest <- right_join(regije2, tabela4, by = 'Leto')
vrednost <- invest$`Investicije`[1] 
invest$`Investicije` <- invest$`Investicije` / vrednost * 100
vrednost <- invest$'INV'[1] 
invest$`INV` <- invest$`INV` / vrednost * 100
invest <- invest %>% rename('Zrak' ='INV') %>% rename('Splosno' =`Investicije`)

izpust<- zdruzena_tabela %>% group_by(Leto) %>% summarise('izpusti'=sum(`Izpusti_mg`))
vrednost <- izpust$`izpusti`[1] 
izpust$`izpusti` <- izpust$`izpusti` / vrednost * 100   

invest <- gather(invest, 'Namen', 'indeks', 2:3)

graf6 <- ggplot() +
  geom_bar(stat="identity",data = invest, aes(x=Leto, y=indeks, fill=Namen), position=position_dodge())+
  geom_line(data=izpust, aes(x=Leto,y=izpusti))+
  ylab('Investirani € glede na namen')+
  ggtitle('Investicije v zmanjšanje izpust toplogrednih plinov')
  


graf6
####

tabela5  <- zdruzena_tabela %>% group_by(Leto) %>% summarise('CH4'=sum(`Izpust_CH4`),
                                                                  'CO2' =sum(`Izpust_CO2`),
                                                                  'N2O' = sum(`Izpust_N2O`))
vrednost <- tabela5$`CH4`[1] 
tabela5$`CH4` <- tabela5$`CH4` / vrednost * 100
vrednost <- tabela5$`CO2`[1] 
tabela5$`CO2` <- tabela5$`CO2` / vrednost * 100
vrednost <- tabela5$`N2O`[1] 
tabela5$`N2O` <- tabela5$`N2O` / vrednost * 100
tabela5 <- tabela5[-1,]


tabela5 <- gather(tabela5, 'Plin', 'Mg', 2:4)


graf7 <- ggplot() +
  geom_bar(stat="identity",data = tabela5, aes(x=Leto, y=Mg, fill=Plin),position=position_dodge())+theme_bw() +
  theme(legend.position = c(0.8, 0.5))+
  ylab('Indeksi')+ggtitle('Izpust toplogrednih plinov glede na referenčno leto 2010')
graf7


#################

#evropa <- evropa %>%  group_by(GEO) %>% summarise('Tisocton'=sum(`Tisocton`))
evropa$TIME <- as.factor(evropa$TIME)
evropa$GEO <- gsub('Germany \\(until 1990 former territory of the FRG\\)', 'Germany',evropa$GEO)
evropa
graf8 <- ggplot(evropa, aes(x=TIME, y=Tisocton)) +
  geom_boxplot()+
  geom_jitter()+
  scale_y_log10()

graf8 



 ###############
 

regije1 <- regije %>% group_by(regija) %>% summarise('Investicije'=sum(`Investicije.za.varstvo.okolja`))
regije1[,1] <-as.character(regije1$regija)

slo <- readRDS("podatki/gadm36_SVN_1_sp.rds") %>% fortify()
colnames(slo)[11] <- 'regija'
slo$regija <- gsub('Notranjsko-kraška', 'Primorsko-notranjska', slo$regija)
slo$regija <- gsub('Spodnjeposavska', 'Posavska', slo$regija)
zem <- right_join(regije1,slo, by = c('regija'))

zemljevid.investicije <- ggplot() +
  geom_polygon(data = zem, aes(x = long, y = lat, group = group, fill =Investicije))+
  geom_path(data = zem, aes(x = long,y = lat, group = group),color = "white", size = 0.1) +
  xlab("") + ylab("") + ggtitle('Investicije') + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank())+
  scale_fill_viridis(option = "viridis", direction = -1)+
  coord_fixed()
 
zemljevid.investicije 


#Slovenija <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
#                             "SVN_adm1", encoding="UTF-8") %>% fortify()
#colnames(Slovenija)[12] <- 'regija'
#Slovenija$regija <- gsub('Notranjsko-kraška', 'Primorsko-notranjska', Slovenija$regija)
#Slovenija$regija <- gsub('Spodnjeposavska', 'Posavska', Slovenija$regija)




#NEKI2 <- right_join(regije1,Slovenija, by = c('regija'))

#zemljevid.inv <- ggplot() +
##  geom_polygon(data = NEKI2, aes(x = long, y = lat, group = group, fill =Investicije))+
#  geom_path(data = NEKI2, aes(x = long,y = lat, group = group),color = "white", size = 0.1) +
#  xlab("") + ylab("") + ggtitle('Investicije') + 
#  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank())+
#  scale_fill_viridis(option = "cividis", direction = -1)+
#  coord_equal()
  

#zemljevid.inv






regije3 <- regije %>% filter(INVESTICIJE == 'Varstvo zraka in klime')
regije3 <- regije3 %>% group_by(regija) %>% summarise('Investicije'=sum(`Investicije.za.varstvo.okolja`))
regije3[,1] <-as.character(regije3$regija)

slo <- readRDS("podatki/gadm36_SVN_1_sp.rds") %>% fortify()
colnames(slo)[11] <- 'regija'
slo$regija <- gsub('Notranjsko-kraška', 'Primorsko-notranjska', slo$regija)
slo$regija <- gsub('Spodnjeposavska', 'Posavska', slo$regija)
zem <- right_join(regije3,slo, by = c('regija'))

zemljevid.zrak <- ggplot() +
  geom_polygon(data = zem, aes(x = long, y = lat, group = group, fill =Investicije))+
  geom_path(data = zem, aes(x = long,y = lat, group = group),color = "white", size = 0.1) +
  xlab("") + ylab("") + ggtitle('Investicije') + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank())+
  scale_fill_viridis(option = "viridis", direction = -1)+
  coord_fixed()

zemljevid.zrak 

##############


map <- map_data("world")
eu <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czechia.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")

eu <- map_data("world", region = eu)
eu <- fortify(eu)
evropa <- evropa %>%  group_by(GEO) %>% summarise('Tisocton'=sum(`Tisocton`))
evropa <- evropa %>% rename('region' = 'GEO')
evropa
eumap <- right_join(evropa,eu, by = c('region'))

eumap1 <- ggplot()+
  geom_polygon(data = eumap, aes(x = long, y = lat, group = group, fill =Tisocton))+
  geom_path(data = eu, aes(x = long,y = lat, group = group),color = "white", size = 0.1) +
  xlab("") + ylab("") + ggtitle('Izpusti toplogrednih plinov') + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank())+
  scale_fill_viridis(option = "plasma", direction = -1)+
  coord_fixed()
eumap1
