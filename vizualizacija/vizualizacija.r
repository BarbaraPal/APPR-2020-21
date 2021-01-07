# 3. faza: Vizualizacija podatkov

# funkcija, ki izbere iz tabele samo določene države

sosednje.drzave <- function(tabela){
  drzave <- filter(tabela, tabela$DRZAVA == "Slovenija") %>%
    full_join(filter(tabela, tabela$DRZAVA == "Hrvaška")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Avstrija")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Italija")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Madžarska"))
  return(drzave)
}

prva.skupina <- function(tabela){
  drzave <- filter(tabela, tabela$DRZAVA == "Slovenija") %>%
    full_join(filter(tabela, tabela$DRZAVA == "Nemčija")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Belgija")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Luksemburg")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Francija"))
  return(drzave)
}

druga.skupina <- function(tabela){
  drzave <- filter(tabela, tabela$DRZAVA == "Slovenija") %>%
    full_join(filter(tabela, tabela$DRZAVA == "Nizozemska")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Danska")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Irska")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Velika Britanija"))
  return(drzave)
}

tretja.skupina <- function(tabela){
  drzave <- filter(tabela, tabela$DRZAVA == "Slovenija") %>%
    full_join(filter(tabela, tabela$DRZAVA == "Švedska")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Finska")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Estonija")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Latvija")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Litva"))
  return(drzave)
}

cetrta.skupina <- function(tabela){
  drzave <- filter(tabela, tabela$DRZAVA == "Slovenija") %>%
    full_join(filter(tabela, tabela$DRZAVA == "Grčija")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Portugalska")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Španija")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Malta")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Ciper"))
  return(drzave)
}

peta.skupina <- function(tabela){
  drzave <- filter(tabela, tabela$DRZAVA == "Slovenija") %>%
    full_join(filter(tabela, tabela$DRZAVA == "Češka")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Poljska")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Slovaška")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Bolgarija")) %>%
    full_join(filter(tabela, tabela$DRZAVA == "Romunija"))
  return(drzave)
}
########
# DELO #
########

# Slovenija

delo <- filter(shrani.delo, shrani.delo$DRZAVA == "Slovenija")
delo <- filter(delo, delo$STAROST == "25-54")
delo <- filter(delo, delo$INDIKATOR == "Delež aktivne populacije")
delo <- delo %>% select(LETO, SPOL, VREDNOST)
  
graf_slovenija <- ggplot((data = delo), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Zaposlenost moških in žensk v Sloveniji')

delo1 <- filter(shrani.delo, shrani.delo$DRZAVA == "Slovenija")
delo1 <- filter(delo1, delo1$STAROST == "25-54")
delo1 <- filter(delo1, delo1$INDIKATOR == "Delež celotne populacije")
delo1 <- delo1 %>% select(LETO, SPOL, VREDNOST)

graf_slovenija1 <- ggplot((data = delo1), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Zaposlenost moških in žensk v Sloveniji')
 

# Države EU

filter1 <- filter(shrani.delo, shrani.delo$INDIKATOR == "Delež aktivne populacije")
sosednje.drzave.all <- sosednje.drzave(filter1)
prva.skupina.all <- prva.skupina(filter1)
druga.skupina.all <- druga.skupina(filter1)
tretja.skupina.all <- tretja.skupina(filter1)
cetrta.skupina.all <- cetrta.skupina(filter1)
peta.skupina.all <- peta.skupina(filter1)

graf.primerjava.delo <- function(tabela){
  graf <- ggplot(data = tabela,
                 aes(x=as.numeric(LETO), y=VREDNOST, col=SPOL)) +
    geom_line() + 
    geom_point() + 
    facet_grid(STAROST~DRZAVA) + 
    scale_x_continuous('LETO', breaks = seq(2011, 2019, 2)) +
    ggtitle("Zaposlenost moških in žensk")
  print(graf)
}

sosednje.drzave.graf <- graf.primerjava.delo(sosednje.drzave.all)
prva.skupina.graf <- graf.primerjava.delo(prva.skupina.all)
druga.skupina.graf <- graf.primerjava.delo(druga.skupina.all)
tretja.skupina.graf <- graf.primerjava.delo(tretja.skupina.all)
cetrta.skupina.graf <- graf.primerjava.delo(cetrta.skupina.all)
peta.skupina.graf <- graf.primerjava.delo(peta.skupina.all)


# Zaposlenost žensk v državah EU (primerjava med državami) 

filtriranje1 <- function(tabela){
  tabela <- filter(tabela, tabela$STAROST== "25-54")
  tabela <- filter(tabela, tabela$SPOL== "ženske")
  tabela <- filter(tabela, tabela$INDIKATOR == "Delež celotne populacije")
  tabela <- tabela %>% select(LETO, DRZAVA, VREDNOST)
}

graf.zenske <- function(tabela){
  graf <- ggplot((data = tabela), aes(x= as.numeric(LETO), y= VREDNOST, col=DRZAVA)) + geom_point() + geom_line() +
    scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
    ggtitle("Zaposlenost žensk - primerjava")
  return(print(graf))
}

sosednje.drzave.zenske <- filtriranje1(sosednje.drzave(shrani.delo))
sosednje.drzave.zenske.graf <- graf.zenske(sosednje.drzave.zenske)

prva.skupina.zenske <- filtriranje1(prva.skupina(shrani.delo))
prva.skupina.zenske.graf <- graf.zenske(prva.skupina.zenske)

druga.skupina.zenske <- filtriranje1(druga.skupina(shrani.delo))
druga.skupina.zenske.graf <- graf.zenske(druga.skupina.zenske)

tretja.skupina.zenske <- filtriranje1(tretja.skupina(shrani.delo))
tretja.skupina.zenske.graf <- graf.zenske(tretja.skupina.zenske)

cetrta.skupina.zenske <- filtriranje1(cetrta.skupina(shrani.delo))
cetrta.skupina.zenske.graf <- graf.zenske(cetrta.skupina.zenske)

peta.skupina.zenske <- filtriranje1(peta.skupina(shrani.delo))
peta.skupina.zenske.graf <- graf.zenske(peta.skupina.zenske)


##################################
# ZAPOSLENOST GLEDE NA IZOBRAZBO #
##################################

# Slovenija

izobrazba <- filter(shrani.izobrazba, shrani.izobrazba$DRZAVA == "Slovenija")
izobrazba <- filter(izobrazba, izobrazba$STAROST == "25-54")
izobrazba <- izobrazba %>% select(LETO, SPOL, VREDNOST, INDIKATOR)
izobrazba.slovenija <- ggplot((data = izobrazba), aes(x= as.numeric(LETO), y= VREDNOST, col_double=SPOL, col=INDIKATOR)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Zaposlenost moških in žensk v Sloveniji glede na izobrazbo')

# EU
filter2 <- filter(shrani.izobrazba, shrani.izobrazba$STAROST == "25-54")

graf.izobrazba.eu <- function(tabela){
  graf <- ggplot((data = tabela), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
    scale_x_continuous('LETO', breaks = seq(2011, 2019, 2)) +
    ggtitle('Zaposlenost moških in žensk glede na izobrazbo') +
    facet_grid(INDIKATOR~DRZAVA)
  return(graf)
}

sosednje.izobrazba.graf <- graf.izobrazba.eu(sosednje.drzave(filter2))
prva.izobrazba.graf <- graf.izobrazba.eu(prva.skupina(filter2))
druga.izobrazba.graf <- graf.izobrazba.eu(druga.skupina(filter2))
tretja.izobrazba.graf <- graf.izobrazba.eu(tretja.skupina(filter2))
cetrta.izobrazba.graf <- graf.izobrazba.eu(cetrta.skupina(filter2))
peta.izobrazba.graf <- graf.izobrazba.eu(peta.skupina(filter2))

#########################
# POLOVIČNI DELOVNI ČAS #
#########################

# Slovenija
polovica <- filter(shrani.polovica, shrani.polovica$DRZAVA == "Slovenija") %>% 
  select(LETO, SPOL, VREDNOST)

graf.polovica.slo <- ggplot((data = polovica), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Polovični delovni čas')

# EU
graf.polovica.eu <- function(tabela){
  graf <- ggplot((data = tabela), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 2)) +
  facet_grid(~DRZAVA)
  ggtitle('Polovični delovni čas')
  print(graf)
}

#sosednje.polovica <- graf.polovica.eu(sosednje.drzave(shrani.polovica))
#prva.polovica <- graf.polovica.eu(prva.skupina(shrani.polovica))
#druga.polovca <- graf.polovica.eu(druga.skupina(shrani.polovica))
#tretja.polovica <- graf.polovica.eu(tretja.skupina(shrani.polovica))
#cetrta.polovica <- graf.polovica.eu(cetrta.skupina(shrani.polovica))
#peta.polovica <- graf.polovica.eu(peta.skupina(shrani.polovica))
##########################
# NEDOLOČENI DELOVNI ČAS #
##########################

# Slovenija
#nedolocen <- filter(shrani.nedolocen, shrani.nedolocen$DRZAVA == "Slovenija")
#nedolocen <- filter(nedolocen, nedolocen$RAZLOG == "Ni mogel najti službe za nedoločen čas")
#nedolocen <- filter(nedolocen, nedolocen$INDIKATOR == "Delež vseh zaposlenih")
#nedolocen <- nedolocen %>% select(LETO, SPOL, VREDNOST)

#graf.nedolocen.slo <- ggplot((data = nedolocen), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
#  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
#  ggtitle('Nedoločeni delovni čas')

# EU

#graf.nedolocen.eu <- function(tabela){
#  tabela <- filter(tabela, tabela$INDIKATOR == "Delež zaposlenih za nedoločen čas")
#  graf <- ggplot((data = tabela), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
#    scale_x_continuous('LETO', breaks = seq(2011, 2019, 2)) +
#    facet_grid(RAZLOG~DRZAVA)
#  ggtitle('Nedoločeni delovni čas')
#  print(graf)
#}

#sosednje.nedolocen <- graf.nedolocen.eu(sosednje.drzave(shrani.nedolocen))
#prva.nedolocen <- graf.nedolocen.eu(prva.skupina(shrani.nedolocen))
#druga.nedolocen <- graf.nedolocen.eu(druga.skupina(shrani.nedolocen))
#tretja.nedolocen <- graf.nedolocen.eu(tretja.skupina(shrani.nedolocen))
#cetrta.nedolocen <- graf.nedolocen.eu(cetrta.skupina(shrani.nedolocen))
#peta.nedolocen <- graf.nedolocen.eu(peta.skupina(shrani.nedolocen))


#################
# BREZPOSELNOST #
#################

# Slovenija
brezposelnost.vseh.slo <- filter(shrani.brezposelnost.delez.vseh, shrani.brezposelnost.delez.vseh$DRZAVA == "Slovenija") %>% 
  select(LETO, SPOL, DELEZ)
graf.brezposelnost.vseh.slo <- ggplot((data = brezposelnost.vseh.slo), aes(x= as.numeric(LETO), y= DELEZ, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Brezposelnost vseh')

brezposelnost.aktivni.slo <- filter(shrani.brezposelnost.delez.aktivnih, shrani.brezposelnost.delez.aktivnih$DRZAVA == "Slovenija") %>% 
  select(LETO, SPOL, DELEZ)
graf.brezposelnost.aktivni.slo <- ggplot((data = brezposelnost.aktivni.slo), aes(x= as.numeric(LETO), y= DELEZ, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Brezposelnost aktivnih')



#############################
# DOLGOTRAJNA BREZPOSELNOST #
#############################
#d.brezposelnost.slo <- filter(shrani.brezposelnost, shrani.brezposelnost$DRZAVA == "Slovenija") %>% 
#  select(LETO, SPOL, DELEZ)
#d.graf.brezposelnost.slo <- ggplot((data = d.brezposelnost.slo), aes(x= as.numeric(LETO), y= DELEZ, col=SPOL)) + geom_point() + geom_line() +
#  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
#  ggtitle('Dolgotrajna brezposelnost')

#z.d.brezposelnost.slo <- filter(shrani.zelo.dolgotrajna.brezposelnost, shrani.zelo.dolgotrajna.brezposelnost$DRZAVA == "Slovenija") %>% 
#  select(LETO, SPOL, DELEZ)
#z.d.graf.brezposelnost.slo <- ggplot((data = z.d.brezposelnost.slo), aes(x= as.numeric(LETO), y= DELEZ, col=SPOL)) + geom_point() + geom_line() +
#  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
#  ggtitle('Zelo dolgotrajna brezposelnost')



#############
# ZEMLJEVID #
#############

data("World")
tmap_mode("view")

evropa <- filter(World, World$continent == "Europe")
tm_shape(evropa) + tm_polygons()


# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                             pot.zemljevida="OB", encoding="Windows-1250")
# Če zemljevid nima nastavljene projekcije, jo ročno določimo
#proj4string(zemljevid) <- CRS("+proj=utm +zone=10+datum=WGS84")

#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))

