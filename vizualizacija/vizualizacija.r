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


tabela2 <- zdruzena_tabela  %>% filter(Leto == 2010)

graf3 <- ggplot(tabela2, aes(x=DEJAVNOST)) +
        geom_point(aes(y=`Izpusti (Mg)`))
      
graf3  


