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


tabela1 <- zdruzena_tabela %>% group_by(Leto) %>% summarise('Investicije mio €'=sum(`Investicije mio €`),
                                                         'Davki mio €' =sum(`Davki mio €`),
                                                         'Proizvodnja mio €' = sum(`Proizvodnja mio €`))

davki_na_proizvodnjo <- bdp_skupni %>% filter(DEJAVNOST == 'Neto davki na proizvode')
davki_na_proizvodnjo <- davki_na_proizvodnjo[-1]
davki_na_proizvodnjo <- davki_na_proizvodnjo %>% rename('Neto davki na proizvode' = `Proizvodnja mio €`)

tabela1 <- merge(tabela1, davki_na_proizvodnjo)
tabela1$'BDP mio €' <- rowSums(tabela1[4:5])
tabela1$`Proizvodnja mio €` <- NULL
tabela1$`Neto davki na proizvode` <- NULL


graf1 <- ggplot(tabela1, aes(x=Leto)) +
        geom_line(aes(y=tabela1$`Investicije mio €`, colour='Investicije'))+
        geom_line(aes(y= tabela1$`Davki mio €`, colour='Davki'))+
        scale_colour_manual("", values = c("Investicije" = "blue", "Davki" = "red"))+
        ylab('mio €') +
        ggtitle('Graf davkov in investicij')

graf1

vrednost <- tabela1$`Investicije mio €`[1] 
tabela1$`Investicije mio €` <- tabela1$`Investicije mio €` / vrednost * 100
vrednost <- tabela1$`Davki mio €`[1] 
tabela1$`Davki mio €` <- tabela1$`Davki mio €` / vrednost * 100
vrednost <- tabela1$`BDP mio €`[1] 
tabela1$`BDP mio €` <- tabela1$`BDP mio €` / vrednost * 100

graf2 <- ggplot(tabela1, aes(x=Leto)) +
  geom_line(aes(y=tabela1$`Investicije mio €`, colour='Investicije'))+
  geom_line(aes(y= tabela1$`Davki mio €`, colour='Davki'))+
  geom_line(aes(y=tabela1$`BDP mio €`, colour = 'BDP'))+
  scale_colour_manual("", values = c("Investicije" = "blue", "Davki" = "red", 'BDP' = 'green'))+
  ylab('Indeski, referenčno leto 2010') +
  ggtitle('Graf davkov in investicij')

graf2


tabela2  <- zdruzena_tabela %>% group_by(DEJAVNOST) %>% summarise('Investicije mio €'=sum(`Investicije mio €`),
                                                                      'Davki mio €' =sum(`Davki mio €`),
                                                                      'Proizvodnja mio €' = sum(`Proizvodnja mio €`),
                                                                      'Izpust (Mg)' = sum(`Izpusti (Mg)`))

davek <- tabela2$`Davki mio €` / tabela2$`Izpust (Mg)`
investicija <- tabela2$`Investicije mio €` / tabela2$`Izpust (Mg)`

tabela2$'Davek (€) na enoto izpusta (Mg)' <- davek * 1000000
tabela2$'Investicija (€) na enoto izpusta (Mg)' <- investicija * 1000000
vek <- c(LETTERS[1:21])
tabela2 <- tabela2 %>% mutate(DEJAVNOST = vek)
tabela2[is.na(tabela2)] <- 0
tabela2[21,6] <- 0
#tabela2 <- tabela2[-(20:21),]
tabela2 <- tabela2[-15,]
graf3 <- ggplot(tabela2, aes(x=DEJAVNOST)) +
        geom_point(aes(y=tabela2$`Davek (€) na enoto izpusta (Mg)`, colour='Davek'))+
        geom_point(aes(y=tabela2$`Investicija (€) na enoto izpusta (Mg)`, colour='Investicije'))+
        scale_colour_manual("", values = c("Investicije" = "blue", "Davek" = "red"))+
        ylab('Indeski, referenčno leto 2010') +
        ggtitle('Graf davkov in investicij')
  
      

graf3  

tabela3 <- tabela2[,c(1,6,7)] 
tabela3 <- tabela3 %>% rename('Investicija' = 'Investicija (€) na enoto izpusta (Mg)') %>% rename('Davek'= 'Davek (€) na enoto izpusta (Mg)')
tabela3 <- gather(tabela3, 'Namen', 'eur_na_Mg', 2:3)
tabela3 <- tabela3[c('Namen','DEJAVNOST','eur_na_Mg' )]
test <- ggplot() +
  geom_bar(stat="identity",data = tabela3 %>% mutate(subset = "all"),aes(x=DEJAVNOST, y=eur_na_Mg, fill=Namen),position=position_dodge())+
  geom_bar(stat="identity",data = tabela3 %>% filter(eur_na_Mg < 300) %>% mutate(subset = "small"),aes(x=DEJAVNOST, y=eur_na_Mg, fill=Namen),position=position_dodge())+
  facet_wrap(~ subset, scales = "free_y")

 
test


