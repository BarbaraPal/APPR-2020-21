# Analiza podatkov s programom R, 2020/21

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2020/21

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/BarbaraPal/APPR-2020-21/master?urlpath=shiny/APPR-2020-21/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/BarbaraPal/APPR-2020-21/master?urlpath=rstudio) RStudio

## Analiza dela in brezposelnosti žensk v EU

V državah Evropske unije bi pričakovali, da razlik med spoloma v današnjem času (skorajda) ni. Zato bom analizirala stopnjo zaposlenosti pri obeh spolih glede na doseženo izobrazbo in starostno skupino. Prav tako me bodo zanimale razlike med spoloma pri delu za polovični in nedeločeni čas ter nezaposlenost in dolgoročna nezaposlenost. Primerjala bom podatke od leta 2011 do 2019, v 28 različnih državah. Iz tega bom potem naredila zaključke in napovedi za prihodnost. 

Podatki so uvoženi s strani [Eurostat](https://ec.europa.eu/eurostat/data/database) v obliki CSV in HTML.

# Tabele

1.tabela: Zaposlenost
* leto
* država
* spol
* starostna skupina
* indikator (aktivna populacija, celotna populacija)
* delež zaposlenosti (%)

2.tabela: Zaposlenost glede na izobrazbo
* leto
* država
* spol
* starostna skupina
* izobrazba
* delež zaposlenosti (%)

3.tabela: Delo za polovični delovni čas
* leto
* država
* spol
* delež zaposlenih (%)

4.tabela: Delo za nedoločen čas (z dodanim razlogom)
* leto
* država
* spol
* glavni razlog za zaposlitev za nedeločen čas
* indikator (delež vseh zaposlenih, delež zaposlenih za nedoločen čas)
* delež zaposlenih za nedoločeni čas (%)

5.tabela: Nezaposlenost in dolgoročna nezaposlenost
* leto
* država
* spol
* delež nezaposlenosti celotne populacije (%)
* delež nezaposlenosti aktivne populacije (%)
* delež dolgoročne nezaposlenosti med nezaposlenimi (%)
* delež dolgoročne nezaposlenosti med aktivno populacijo (%)

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `tmap` - za izrisovanje zemljevidov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-202021)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
