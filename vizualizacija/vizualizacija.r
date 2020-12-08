# 3. faza: Vizualizacija podatkov
delo <- read_csv("podatki/delo.csv", locale=locale(encoding = "UTF-8"))
delo <- filter(delo, delo$DRŽAVA == "Slovenija")
delo <- filter(delo, delo$STAROST == "25-54")
delo <- filter(delo, delo$INDIKATOR == "Delež aktivne populacije")
delo <- delo %>% select(LETO, SPOL, VREDNOST)
  
graf_slovenija <- ggplot((data = delo), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Zaposlenost moških in žensk v Sloveniji')

# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                             pot.zemljevida="OB", encoding="Windows-1250")
# Če zemljevid nima nastavljene projekcije, jo ročno določimo
#proj4string(zemljevid) <- CRS("+proj=utm +zone=10+datum=WGS84")

#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))

# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
