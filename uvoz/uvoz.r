# 2. faza: Uvoz podatkov

source("lib/libraries.r", encoding="UTF-8")

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

drzave <- c("Belgium", "Bulgaria", "Czechia", "Denmark", 
            "Germany (until 1990 former territory of the FRG)", "Estonia" , 
            "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus",
            "Latvia", "Lithuania","Luxembourg", "Hungary", "Malta", "Netherlands",
            "Austria", "Poland",  "Portugal", "Romania", "Slovenia", "Slovakia",
            "Finland", "Sweden", "United Kingdom")
drzaveslo <- c("Belgija", "Bolgarija", "Češka", "Danska", "Nemčija", "Estonija",
                 "Irska", "Grčija", "Španija", "Francija", "Hrvaška", "Italija", "Ciper",
                 "Latvija", "Litva", "Luksemburg", "Madžarska", "Malta", "Nizozemska",
                 "Avstrija", "Poljska", "Portugalska", "Romunija", "Slovenija", "Slovaška",
                 "Finska", "Švedska", "Velika Britanija")
starost1 <- c("From 15 to 24 years", "From 25 to 54 years", "From 55 to 64 years")
starostslo <- c("15-24", "25-54", "55-64")
spol1 <- c("Males", "Females")
spolslo <- c("moški", "ženske")
indikator.dela <- c("Active population", "Total employment (resident population concept - LFS)")
indikator.dela.slo <- c("Delež aktivne populacije", "Delež celotne populacije")
indikator.zaposlitve <- c("Less than primary, primary and lower secondary education (levels 0-2)",
                          "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)",
                          "Tertiary education (levels 5-8)",
                          "No response")
indikator.zaposlitve.slo <- c("Primarna(0-2)", "Sekundarna(3-4)", "Terciarna(5-8)",
                              "Brez odgovora")
razlog.nedolocen <- c("Could not find permanent job", "Did not want a permanent job",
                      "In education or training", "Probationary period", "No response")
razlog.nedolocen.slo <- c("Ni mogel najti službe za nedoločen čas", "Ni hotel službe za nedoločen čas",
                          "Izobraževanje ali usposabljanje", "Poskusna doba", "Ni odgovora")
enota.nedolocen <- c("Percentage of employees", "Percentage of employees with a temporary job")
enota.nedolocen.slo <- c("Delež vseh zaposlenih", "Delež zaposlenih za nedoločen čas")
  
# DELOVNO AKTIVNI IN ZAPOSLENI
uvoz.delo <- function(){
  delo <- read_csv("podatki/deloinaktivnost.csv",
                   locale = locale(encoding = "cp1250"),
                   na = c(""," ", ":")) %>% 
    separate(col = 1,
             into = c("AGE", "SEX", "TIME", "GEO", "UNIT", "INDIC_EM", "Value"),
             sep="(\\\",\\\")|(,\\\")") %>%
    mutate(
      Value=str_remove_all(Value, "(\\\")"),
    ) %>% 
    transmute(LETO=parse_integer(TIME), country=parse_character(GEO), age=parse_character(AGE),
              sex=parse_character(SEX), indicator=parse_character(INDIC_EM), VREDNOST=parse_number(Value))
  tab1 <- data.frame(DRZAVA=drzaveslo, ime=drzave, stringsAsFactors = FALSE)
  tab2 <- data.frame(STAROST=starostslo, age1=starost1, stringsAsFactors = FALSE)
  tab3 <- data.frame(SPOL=spolslo, sex1=spol1, stringsAsFactors = FALSE)
  tab4 <- data.frame(INDIKATOR=indikator.dela.slo, indicator1=indikator.dela, stringsAsFactors = FALSE)
  t1 <- delo %>% inner_join(tab1, c("country"="ime")) %>% select(-country)
  t2 <- t1 %>% inner_join(tab2, c("age"="age1")) %>% select(-age)
  t3 <- t2 %>% inner_join(tab3, c("sex"="sex1")) %>% select(-sex)
  t4 <- t3 %>% inner_join(tab4, c("indicator"="indicator1")) %>% select(-indicator)
  tabela1 <- t4[c(1,3,5,4,6,2)]
  return(tabela1)
}
shrani.delo <- uvoz.delo() %>%
  write_csv("podatki/delo.csv", na= "NA", append = FALSE, col_names = TRUE)

# IZOBRAZBA
uvoz.izobrazba <- function(){
  izobrazba <- read_csv("podatki/stopnja-zaposlenosti-glede-na-izobrazbo.csv",
                        locale = locale(encoding = "cp1250"),
                        na = c(""," ", ":")) %>% 
    separate(col = 1,
             into = c("SEX", "AGE", "GEO", "TIME", "ISCED11", "UNIT", "Value"),
             sep="(\\\",\\\")|(,\\\")") %>%
    mutate(
      Value=str_remove_all(Value, "(\\\")"),
    ) %>% 
    transmute(LETO=parse_integer(TIME), country=parse_character(GEO), age=parse_character(AGE),
              sex=parse_character(SEX), indicator=parse_character(ISCED11), VREDNOST=parse_number(Value)) %>% 
    drop_na()
  tab1 <- data.frame(DRZAVA=drzaveslo, ime=drzave, stringsAsFactors = FALSE)
  tab2 <- data.frame(STAROST=starostslo, age1=starost1, stringsAsFactors = FALSE)
  tab3 <- data.frame(SPOL=spolslo, sex1=spol1, stringsAsFactors = FALSE)
  tab4 <- data.frame(INDIKATOR=indikator.zaposlitve.slo, indicator1=indikator.zaposlitve, stringsAsFactors = FALSE)
  t1 <- izobrazba %>% inner_join(tab1, c("country"="ime")) %>% select(-country)
  t2 <- t1 %>% inner_join(tab2, c("age"="age1")) %>% select(-age)
  t3 <- t2 %>% inner_join(tab3, c("sex"="sex1")) %>% select(-sex)
  t4 <- t3 %>% inner_join(tab4, c("indicator"="indicator1")) %>% select(-indicator)
  tabela2 <- t4[c(1,3,5,4,6,2)]
  return(tabela2)
}
shrani.izobrazba <- uvoz.izobrazba() %>%
  write_csv("podatki/izobrazba.csv", na= "NA", append = FALSE, col_names = TRUE)

# POLOVIČNI DELOVNI ČAS
uvoz.polovica <- function(){
  polovica <- read_csv("podatki/polovični-delovni-čas.csv",
                       locale = locale(encoding = "cp1250"),
                       na = c(""," ", ":")) %>% 
    separate(col = 1,
             into = c("SEX", "TIME", "GEO", "AGE", "CITIZEN", "UNIT", "Value"),
             sep="(\\\",\\\")|(,\\\")") %>%
    mutate(
      Value=str_remove_all(Value, "(\\\")"),
    ) %>% 
    transmute(LETO=parse_integer(TIME), country=parse_character(GEO), age=parse_character(AGE),
              sex=parse_character(SEX), VREDNOST=parse_number(Value)) %>% 
    drop_na()
  tab1 <- data.frame(DRZAVA=drzaveslo, ime=drzave, stringsAsFactors = FALSE)
  tab2 <- data.frame(SPOL=spolslo, sex1=spol1, stringsAsFactors = FALSE)
  t1 <- polovica %>% inner_join(tab1, c("country"="ime")) %>% select(-country, -age)
  t2 <- t1 %>% inner_join(tab2, c("sex"="sex1")) %>% select(-sex)
  tabela3 <- t2[c(1,3,4,2)]
  return(tabela3)
}
shrani.polovica <- uvoz.polovica() %>%
  write_csv("podatki/polovica.csv", na= "NA", append = FALSE, col_names = TRUE)

# NEDOLOČENI DELOVNI ČAS
uvoz.nedolocen <- function(){
  nedolocen <- read_csv("podatki/nedoločen-čas.csv",
                        locale = locale(encoding = "cp1250"),
                        na = c(""," ", ":")) %>% 
    separate(col = 1,
             into = c( "AGE", "TIME", "GEO","SEX", "REASON", "UNIT", "Value"),
             sep="(\\\",\\\")|(,\\\")") %>%
    mutate(
      Value=str_remove_all(Value, "(\\\")"),
    ) %>% 
    transmute(LETO=parse_integer(TIME), country=parse_character(GEO), sex=parse_character(SEX),
              reason=parse_character(REASON), VREDNOST=parse_number(Value), unit = parse_character(UNIT)) %>% 
    drop_na()
  tab1 <- data.frame(DRZAVA=drzaveslo, ime=drzave, stringsAsFactors = FALSE)
  tab2 <- data.frame(SPOL=spolslo, sex1=spol1, stringsAsFactors = FALSE)
  tab3 <- data.frame(RAZLOG=razlog.nedolocen.slo, razlog1=razlog.nedolocen, stringsAsFactors = FALSE)
  tab4 <- data.frame(INDIKATOR=enota.nedolocen.slo, unit1=enota.nedolocen, stringsAsFactors = FALSE)
  t1 <- nedolocen %>% inner_join(tab1, c("country"="ime")) %>% select(-country)
  t2 <- t1 %>% inner_join(tab2, c("sex"="sex1")) %>% select(-sex)
  t3 <- t2 %>% inner_join(tab3, c("reason"="razlog1")) %>% select(-reason)
  t4 <- t3 %>% inner_join(tab4, c("unit"="unit1")) %>% select(-unit)
  tabela4 <- t4[c(1,3,4,5,6,2)]
  return(tabela4)
}
shrani.nedolocen <- uvoz.nedolocen() %>%
  write_csv("podatki/nedolocen.csv", na= "NA", append = FALSE, col_names = TRUE)

# BREZPOSELNOST VSEGA PREBIVALSTVA
stran <- read_html("podatki/brezposelnost1.html")
uvoz.brezposelnost.delez.vseh <- function(){
  tabela.delez.vseh <- stran %>% html_nodes(xpath="//table") %>%
    .[[1]] %>% html_table(dec=",") %>% 
    transmute(drzava=X1, LETO=parse_integer(X2), moški=parse_number(X3), ženske=parse_number(X4)) %>% 
    drop_na() %>% pivot_longer(c(ženske, moški), names_to="SPOL", values_to="DELEZ")
  tab1 <- data.frame(DRZAVA=drzaveslo, ime=drzave, stringsAsFactors = FALSE)
  t1 <- tabela.delez.vseh %>%
    inner_join(tab1, c("drzava"="ime")) %>% select(-drzava)
  tabela5 <- t1[c(1,4,2,3)]
  return(tabela5)
}
shrani.brezposelnost.delez.vseh <- uvoz.brezposelnost.delez.vseh() %>%
  write_csv("podatki/brezposelnost-delez-vseh.csv", na = "NA", append = FALSE, col_names = TRUE)

# BREZPOSELNOST AKTIVNEGA PREBIVALSTVA
uvoz.brezposelnost.delez.aktivnih <- function(){
  tabela.delez.aktivnih <- stran %>% html_nodes(xpath="//table") %>%
    .[[2]] %>% html_table(dec=",") %>% 
    transmute(drzava=X1, LETO=parse_integer(X2), moški=parse_number(X3), ženske=parse_number(X4)) %>% 
    drop_na() %>% pivot_longer(c(ženske, moški), names_to="SPOL", values_to="DELEZ")
  tab1 <- data.frame(DRZAVA=drzaveslo, ime=drzave, stringsAsFactors = FALSE)
  t1 <- tabela.delez.aktivnih %>%
    inner_join(tab1, c("drzava"="ime")) %>% select(-drzava)
  tabela6 <- t1[c(1,4,2,3)]
  return(tabela6)
}
shrani.brezposelnost.delez.aktivnih <- uvoz.brezposelnost.delez.aktivnih() %>%
  write_csv("podatki/brezposelnost-delez-aktivnih.csv", na = "NA", append = FALSE, col_names = TRUE)

# DOLGOTRAJNA BREZPOSELNOST
url <- read_html("podatki/dolgoročna-brezposelnost.html")
uvoz.dolgotrajna.brezposelnost <- function(){
  tabela.dolgorocna <- url %>% html_nodes(xpath="//table") %>%
    .[[1]] %>% html_table(dec=",") %>% 
    transmute(LETO=parse_integer(X1), drzava=X2, moški=parse_number(X3), ženske=parse_number(X4)) %>% 
    drop_na() %>% pivot_longer(c(ženske, moški), names_to="SPOL", values_to="DELEZ")
  tab1 <- data.frame(DRZAVA=drzaveslo, ime=drzave, stringsAsFactors = FALSE)
  t1 <- tabela.dolgorocna %>%
    inner_join(tab1, c("drzava"="ime")) %>% select(-drzava)
  tabela7 <- t1[c(1,4,2,3)]
  return(tabela7)
}
shrani.brezposelnost <- uvoz.dolgotrajna.brezposelnost() %>%
  write_csv("podatki/dolgotrajna-brezposelnost.csv", na = "NA", append = FALSE, col_names = TRUE)

# ZELO DOLGOTRAJNA BREZPOSELNOST
uvoz.zelo.dolgotrajna.brezposelnost <- function(){ 
  tabela.zelo.dolgotrajna <- url %>% html_nodes(xpath="//table") %>%
    .[[2]] %>% html_table(dec=",") %>% 
    transmute(LETO=parse_integer(X1), drzava=X2, moški=parse_number(X3), ženske=parse_number(X4)) %>% 
    drop_na() %>% pivot_longer(c(ženske, moški), names_to="SPOL", values_to="DELEZ")
  tab1 <- data.frame(DRZAVA=drzaveslo, ime=drzave, stringsAsFactors = FALSE)
  t1 <- tabela.zelo.dolgotrajna %>%
    inner_join(tab1, c("drzava"="ime")) %>% select(-drzava)
  tabela8 <- t1[c(1,4,2,3)]
  return(tabela8)
}
shrani.zelo.dolgotrajna.brezposelnost <- uvoz.zelo.dolgotrajna.brezposelnost() %>%
  write_csv("podatki/zelo-dolgotrajna-brezposelnost.csv", na= "NA", append = FALSE, col_names = TRUE)