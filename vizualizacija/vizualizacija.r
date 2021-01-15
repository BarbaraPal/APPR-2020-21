# 3. faza: Vizualizacija podatkov

# funkcija, ki izbere iz tabele samo določene države

sosednje.drzave <- function(tabela){
  drzave <- tabela %>% filter(DRZAVA %in% c("Slovenija", "Hrvaška", "Avstrija", "Italija", "Madžarska"))
  return(drzave)
}

prva.skupina <- function(tabela){
  drzave <- tabela %>% filter(DRZAVA %in% c("Slovenija", "Nemčija", "Belgija", "Luksemburg", "Francija"))
  return(drzave)
}

druga.skupina <- function(tabela){
  drzave <- tabela %>% filter(DRZAVA %in% c("Slovenija", "Nizozemska", "Danska", "Irska", "Velika Britanija"))
  return(drzave)
}

tretja.skupina <- function(tabela){
  drzave <- tabela %>% filter(DRZAVA %in% c("Slovenija", "Švedska", "Finska", "Estonija", "Latvija", "Litva"))
  return(drzave)
}

cetrta.skupina <- function(tabela){
  drzave <- tabela %>% filter(DRZAVA %in% c("Slovenija", "Grčija", "Portugalska", "Španija", "Malta", "Ciper"))
  return(drzave)
}

peta.skupina <- function(tabela){
  drzave <- tabela %>% filter(DRZAVA %in% c("Slovenija", "Češka", "Poljska", "Slovaška", "Bolgarija", "Romunija"))
  return(drzave)
}
########
# DELO #
########

## Slovenija

# delež aktivne populacije
delo <- shrani.delo %>% filter(DRZAVA %in% "Slovenija", STAROST %in% "25-54", 
                               INDIKATOR %in% "Delež aktivne populacije") %>% 
  select(LETO, SPOL, VREDNOST)

graf_slovenija <- ggplot((data = delo), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Zaposlenost moških in žensk v Sloveniji (v %)')

# delež celotne populacije
delo1 <- shrani.delo %>% filter(DRZAVA %in% "Slovenija", STAROST %in% "25-54", 
                               INDIKATOR %in% "Delež celotne populacije") %>% 
  select(LETO, SPOL, VREDNOST)

graf_slovenija1 <- ggplot((data = delo1), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Zaposlenost moških in žensk v Sloveniji')
 

# Države EU
filtriranje2 <- shrani.delo %>% filter(INDIKATOR == "Delež celotne populacije")

graf.primerjava.delo <- function(tabela){
  graf <- ggplot(data = tabela,
                 aes(x=as.numeric(LETO), y=VREDNOST, col=SPOL)) +
    geom_line() + 
    geom_point() + 
    facet_grid(STAROST~DRZAVA) + 
    scale_x_continuous('LETO', breaks = seq(2011, 2019, 2)) +
    ggtitle("Zaposlenost moških in žensk po starostnih skupinah (v %)")
  return(graf)
}

sosednje.drzave.graf <- graf.primerjava.delo(sosednje.drzave(filtriranje2))
prva.skupina.graf <- graf.primerjava.delo(prva.skupina(filtriranje2))
druga.skupina.graf <- graf.primerjava.delo(druga.skupina(filtriranje2))
tretja.skupina.graf <- graf.primerjava.delo(tretja.skupina(filtriranje2))
cetrta.skupina.graf <- graf.primerjava.delo(cetrta.skupina(filtriranje2))
peta.skupina.graf <- graf.primerjava.delo(peta.skupina(filtriranje2))


# Zaposlenost žensk v državah EU (primerjava med državami) 

filtriranje1 <- function(tabela){
  tabela <- tabela %>% filter(STAROST == "25-54", SPOL == "ženske") %>% 
    select(LETO, DRZAVA, VREDNOST)
  return(tabela)
}

graf.zenske <- function(tabela){
  graf <- ggplot((data = tabela), aes(x= as.numeric(LETO), y= VREDNOST, col=DRZAVA)) + geom_point() + geom_line() +
    scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
    ggtitle("Zaposlenost žensk - primerjava")
  return(graf)
}

#sosednje.drzave.zenske.graf <- graf.zenske(filtriranje1(sosednje.drzave(filtriranje2)))
#prva.skupina.zenske.graf <- graf.zenske(filtriranje1(prva.skupina(filtriranje2)))
#druga.skupina.zenske.graf <- graf.zenske(filtriranje1(druga.skupina(filtriranje2)))
#tretja.skupina.zenske.graf <- graf.zenske(filtriranje1(tretja.skupina(filtriranje2)))
#cetrta.skupina.zenske.graf <- graf.zenske(filtriranje1(cetrta.skupina(filtriranje2)))
#peta.skupina.zenske.graf <- graf.zenske(filtriranje1(peta.skupina(filtriranje2)))


##################################
# ZAPOSLENOST GLEDE NA IZOBRAZBO #
##################################

# Slovenija

izobrazba <- shrani.izobrazba %>% filter(DRZAVA == "Slovenija", STAROST == "25-54") %>% 
  select(LETO, SPOL, VREDNOST, INDIKATOR)

izobrazba.slovenija <- ggplot((data = izobrazba), aes(x= as.numeric(LETO), y= VREDNOST, col_double=SPOL, col=INDIKATOR)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Zaposlenost moških in žensk v Sloveniji glede na izobrazbo')


# EU
filter2 <- shrani.izobrazba %>% filter(STAROST == "25-54", 
                                       INDIKATOR %in% c("Primarna(0-2)", "Sekundarna(3-4)", "Terciarna(5-8)"))

graf.izobrazba.eu <- function(tabela){
  graf <- ggplot((data = tabela), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
    scale_x_continuous('LETO', breaks = seq(2011, 2019, 2)) +
    ggtitle('Zaposlenost moških in žensk (od 25 do 54 let) glede na izobrazbo') +
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
polovica <- shrani.polovica %>% filter(DRZAVA == "Slovenija") %>% 
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
  return(graf)
}

sosednje.polovica <- graf.polovica.eu(sosednje.drzave(shrani.polovica))
prva.polovica <- graf.polovica.eu(prva.skupina(shrani.polovica))
druga.polovca <- graf.polovica.eu(druga.skupina(shrani.polovica))
tretja.polovica <- graf.polovica.eu(tretja.skupina(shrani.polovica))
cetrta.polovica <- graf.polovica.eu(cetrta.skupina(shrani.polovica))
peta.polovica <- graf.polovica.eu(peta.skupina(shrani.polovica))
##########################
# NEDOLOČENI DELOVNI ČAS #
##########################

# Slovenija
nedolocen <- shrani.nedolocen %>% filter(DRZAVA == "Slovenija", RAZLOG == "Ni mogel najti službe za nedoločen čas",
                                         INDIKATOR == "Delež vseh zaposlenih") %>%
  select(LETO, SPOL, VREDNOST)

graf.nedolocen.slo <- ggplot((data = nedolocen), aes(x= as.numeric(LETO), y= VREDNOST, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Nedoločeni delovni čas')

#################
# BREZPOSELNOST #
#################

# Slovenija
brezposelnost.vseh.slo <- filter(shrani.brezposelnost.delez.vseh, shrani.brezposelnost.delez.vseh$DRZAVA == "Slovenija") %>% 
  select(LETO, SPOL, DELEZ)
graf.brezposelnost.vseh.slo <- ggplot((data = brezposelnost.vseh.slo), aes(x= as.numeric(LETO), y= DELEZ, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Brezposelnost celotnega prebivalstva (v %)')

brezposelnost.aktivni.slo <- filter(shrani.brezposelnost.delez.aktivnih, shrani.brezposelnost.delez.aktivnih$DRZAVA == "Slovenija") %>% 
  select(LETO, SPOL, DELEZ)
graf.brezposelnost.aktivni.slo <- ggplot((data = brezposelnost.aktivni.slo), aes(x= as.numeric(LETO), y= DELEZ, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Brezposelnost aktivnih')

# EU
graf.brezposelnost.eu <- function(tabela){
  graf <- ggplot((data = tabela), aes(x= as.numeric(LETO), y= DELEZ, col=SPOL)) + geom_point() + geom_line() +
    scale_x_continuous('LETO', breaks = seq(2011, 2019, 2)) +
    facet_grid(~DRZAVA)
  ggtitle('Brezposelnost')
  return(graf)
}

sosednje.drzave.brezposelnost.vseh <- graf.brezposelnost.eu(sosednje.drzave(shrani.brezposelnost.delez.vseh))
prva.skupina.brezposelnost.vseh <- graf.brezposelnost.eu(prva.skupina(shrani.brezposelnost.delez.vseh))
druga.skupina.brezposelnost.vseh <- graf.brezposelnost.eu(druga.skupina(shrani.brezposelnost.delez.vseh))
tretja.skupina.brezposelnost.vseh <- graf.brezposelnost.eu(tretja.skupina(shrani.brezposelnost.delez.vseh))
cetrta.skupina.brezposelnost.vseh <- graf.brezposelnost.eu(cetrta.skupina(shrani.brezposelnost.delez.vseh))
peta.skupina.brezposelnost.vseh <- graf.brezposelnost.eu(peta.skupina(shrani.brezposelnost.delez.vseh))

sosednje.drzave.brezposelnost.aktivnih <- graf.brezposelnost.eu(sosednje.drzave(shrani.brezposelnost.delez.aktivnih))
prva.skupina.brezposelnost.aktivnih <- graf.brezposelnost.eu(prva.skupina(shrani.brezposelnost.delez.aktivnih))
druga.skupina.brezposelnost.aktivnih <- graf.brezposelnost.eu(druga.skupina(shrani.brezposelnost.delez.aktivnih))
tretja.skupina.brezposelnost.aktivnih <- graf.brezposelnost.eu(tretja.skupina(shrani.brezposelnost.delez.aktivnih))
cetrta.skupina.brezposelnost.aktivnih <- graf.brezposelnost.eu(cetrta.skupina(shrani.brezposelnost.delez.aktivnih))
peta.skupina.brezposelnost.aktivnih <- graf.brezposelnost.eu(peta.skupina(shrani.brezposelnost.delez.aktivnih))

#############################
# DOLGOTRAJNA BREZPOSELNOST #
#############################
d.brezposelnost.slo <- filter(shrani.brezposelnost, shrani.brezposelnost$DRZAVA == "Slovenija") %>% 
  select(LETO, SPOL, DELEZ)
d.graf.brezposelnost.slo <- ggplot((data = d.brezposelnost.slo), aes(x= as.numeric(LETO), y= DELEZ, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Dolgotrajna brezposelnost')

z.d.brezposelnost.slo <- filter(shrani.zelo.dolgotrajna.brezposelnost, shrani.zelo.dolgotrajna.brezposelnost$DRZAVA == "Slovenija") %>% 
  select(LETO, SPOL, DELEZ)
z.d.graf.brezposelnost.slo <- ggplot((data = z.d.brezposelnost.slo), aes(x= as.numeric(LETO), y= DELEZ, col=SPOL)) + geom_point() + geom_line() +
  scale_x_continuous('LETO', breaks = seq(2011, 2019, 1)) +
  ggtitle('Zelo dolgotrajna brezposelnost')


#############
# ZEMLJEVID #
#############
eu <- c("Belgium",
        "Bulgaria",
        "Czechia",
        "Denmark",
        "Germany",
        "Estonia",
        "Ireland",
        "Greece",
        "Spain",
        "France",
        "Croatia",
        "Italy",
        "Cyprus",
        "Latvia",
        "Lithuania",
        "Luxembourg",
        "Hungary",
        "Netherlands",
        "Austria",
        "Poland",
        "Portugal",
        "Romania",
        "Slovenia",
        "Slovakia",
        "Finland",
        "Sweden",
        "United Kingdom"
)
#data("World")
#tmap_mode("view")
#brezposelnost.eu19 <- shrani.brezposelnost.delez.aktivnih %>% filter(SPOL == "ženske", LETO == "2019", DRZAVA != "Malta") %>% 
#  select(DRZAVA, DELEZ) %>%
#  data.frame("id" = c(1:27))
#brezposelnost.eu191 <- data.frame(eu, "id" = c(1:27)) 
#brezposelnost123 <- inner_join(brezposelnost.eu19, brezposelnost.eu191, by= "id") %>% select(eu, DELEZ) %>% View
#evropa <- World %>% filter(continent %in% c("Europe", "Asia"), 
#                           name %in% c("Belgium", "Bulgaria", "Denmark", "Czech Rep.",
#                            "Germany", "Estonia" , "Ireland", "Greece", "Spain", "France", 
#                            "Croatia", "Italy", "Finland", "Sweden", "United Kingdom",
#                            "Latvia", "Lithuania","Luxembourg", "Hungary", "Netherlands",
#                            "Austria", "Poland",  "Portugal", "Romania", "Slovenia", "Slovakia",
#                            "N. Cyprus"
#                              ))
#tm_shape(evropa) + tm_polygons() + tm_style("cobalt")
#zemljevid.brezposelnost <-  tm_shape(merge(evropa, brezposelnost123, by.x="name", by.y="eu")) +
#  tm_polygons() +
#  tm_style("cobalt") 



