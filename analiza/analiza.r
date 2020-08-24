# 4. faza: Analiza podatkov

#podatki <- obcine %>% transmute(obcina, povrsina, gostota,
#                                gostota.naselij=naselja/povrsina) %>%
#  left_join(povprecja, by="obcina")
#row.names(podatki) <- podatki$obcina
#podatki$obcina <- NULL

# Število skupin
#n <- 5
#skupine <- hclust(dist(scale(podatki))) %>% cutree(n)
#


prileganje <- lm(data = izpust, izpusti ~ Leto)


l <- data.frame(Leto=seq(2018, 2025, 1))

napoved <- mutate(l, izpusti=predict(prileganje, l))
napoved

graf_regresija1 <- ggplot(izpust, aes(x=Leto, y=izpusti)) + geom_line() + 
  geom_smooth(method='lm', formula=y ~ poly(x,2,raw=TRUE), fullrange=TRUE, color='green') +
  scale_x_continuous('Leto', limits = c(2008,2023))+
  ylab('Indeksi izpusta toplogrednih plinov (referenčno leto 2010)')+
  ggtitle('Napoved izpusta toplogrednih plinov v Mg do leta 2013')
graf_regresija1

graf2

tabela_analiza <- tabela1 %>% dplyr::select(1,4)
tabela_analiza <- tabela_analiza %>% rename('bdp'='BDP')
tabela_analiza
tabela_analiza$'izpusti' <- izpust$izpusti
tabela_analiza

graf_regresija2 <- ggplot(tabela_analiza, aes(x=Leto, y=bdp)) + geom_point() + 
  geom_smooth(method='lm', formula=y ~ poly(x,2,raw=TRUE), fullrange=TRUE, color='blue') +
  scale_x_continuous('Leto', limits = c(2010,2023))+
  ylab('Indeksi rasti BDP-ja (referenčno leto 2010)')+
  ggtitle('Napoved rasti BDP-ja do leta 2013')

graf_regresija2
