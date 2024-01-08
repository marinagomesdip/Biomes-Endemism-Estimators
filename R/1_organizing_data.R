#Preparing an unic dataset from different sources
#Marina Morim Gomes
#marinagomesdiptera@mn.ufrj.br
#2022

#loading packages -------------------------------------
library(tidyverse)

#loading data -------------------------------------
#produced data frames
inma <- read_csv("./Data/material_inma.csv",
                 col_types = cols (
                   projeto = col_character(),
                   metodo_coleta = col_character(),
                   referencia = col_character()))
                 

inpa <- read_csv("./Data/material_inpa_noronha.csv",
                 col_types = cols(
                   projeto = col_character(),
                   referencia = col_character()),
                 locale = locale(encoding = "latin1"))

mzusp <- read_csv("./Data/material_mzusp.csv",
                  col_types = cols (
                    referencia = col_character()))

mnrj <- read_csv("./Data/sarcophagidaeMNRJ.csv",
                 col_types = cols (
                   referencia = col_character()))

literatura <- read_csv("./Data/sarcophagidaeliteratura.csv",
                       col_types = cols (
                         projeto = col_character(),
                         determinador = col_character(),
                         coletor = col_character()))

#downloads
gbif <- read_csv("./Data/GBIF.csv",
                 col_types = cols (
                   eventDate = col_character()))
                 
goeldi <- read_csv("./Data/goeldi_baixado.csv",
                    col_types = cols (
                      StartDate = col_character()))

specieslink <- read_csv("./Data/specieslink.csv",
                        locale = locale(encoding = "latin1"))

#merging data frames ------------------------------

   #merging produced data frames(equal columns) --------------------------

#adding a unique key to each observation
inma <- inma %>%
  mutate ( ID_name = 'INMA',
           ID_number = 1:n()) %>%
  unite ( ID, ID_name, ID_number)

inpa <- inpa %>%
  mutate ( ID_name = 'INPA',
           ID_number = 1:n()) %>%
  unite ( ID, ID_name, ID_number)

mzusp <- mzusp %>%
  mutate ( ID_name = 'MZUSP',
           ID_number = 1:n()) %>%
  unite ( ID, ID_name, ID_number)

mnrj <- mnrj %>%
  mutate ( ID_name = 'MNRJ',
           ID_number = 1:n()) %>%
  unite ( ID, ID_name, ID_number)

literatura <- literatura %>%
  mutate ( ID_name = 'LITER',
           ID_number = 1:n()) %>%
  unite ( ID, ID_name, ID_number)

#merging data frames
united <- union(inma, inpa)

united <- union(united, mzusp)

united <- union(united, mnrj)

united <- union(united, literatura)

   #merging downloaded data frames ----------------------

#Wrangle each data frame to unite them (the columns must be equivalent)
#STEPS:
#1 - select only relevant columns
#2 - rename the columns with the names of produced data frames
#3 - create columns that did not exist in the downloaded data frames
#4 - separate the column "data" to create "ano_inicio" and "ano_fim"
#5 - add a unique KEY 
#6 - covert type of variables (the type must be equivalent in all data frames)

goeldi <- goeldi %>%
  select (Genero, Especie, StartDate, 
          Determiner, País, Estado, Distrito, 
          Latitude, Longitude, LocalityName,
          pais = País,
          estado_ou_provincia = Estado,
          municipio = Distrito,
          localidade = LocalityName,
          latitude = Latitude,
          longitude = Longitude,
          data = StartDate,
          genero = Genero,
          especie = Especie,
          determinador = Determiner) %>%
  mutate(projeto = as.character(NA),
         metodo_coleta = as.character(NA),
         coletor = as.character(NA),
         referencia = as.character(NA),
         observacoes = as.character(NA)) %>%
  separate(data, into = c("dia", "mes", "ano_inicio"), sep = "/", remove = FALSE)%>% 
  select( - dia, - mes) %>%
  mutate(ano_fim = ano_inicio,
         ID_name = 'GOELDI',
         ID_number = 1:n()) %>%
  unite ( ID, ID_name, ID_number) %>%
  type_convert(cols(ano_inicio = "?", ano_fim = "?",.default = "c"))

specieslink <- specieslink %>%
  select (country, stateprovince, yearcollected, 
          monthcollected, daycollected, collector,
          identifiedby, county, locality, genus, longitude, latitude,
          species, notes,
          pais = country,
          estado_ou_provincia = stateprovince,
          localidade = locality,
          genero = genus,
          determinador = identifiedby,
          coletor = collector,
          municipio = county,
          especie = species,
          observacoes = notes) %>%
  mutate(projeto = as.character(NA),
         metodo_coleta = as.character(NA),
         referencia = as.character(NA),
         ano_inicio = yearcollected,
         ano_fim = ano_inicio,
         ID_name = 'SPECIESLINK',
         ID_number = 1:n()) %>%
  unite(data, daycollected, monthcollected, yearcollected, sep = "-") %>%
  unite (ID, ID_name, ID_number)

gbif <- gbif %>%
  select (countryCode, stateProvince, locality, 
          decimalLatitude, decimalLongitude, 
          coordinateUncertaintyInMeters, eventDate, genus,
          species, identifiedBy,
          pais = countryCode,
          estado_ou_provincia = stateProvince,
          localidade = locality,
          latitude = decimalLatitude,
          longitude = decimalLongitude,
          incerteza = coordinateUncertaintyInMeters,
          data = eventDate,
          genero = genus,
          determinador = identifiedBy) %>%
  separate(species, into = c("extragenero", "especie"), sep = " ") %>%
  separate(data, into = c("ano_inicio", "dia", "mes"), sep = "-", remove = FALSE) %>%
  select( - dia, - mes, - extragenero) %>%
  mutate(projeto = as.character(NA),
         municipio = as.character(NA),
         metodo_coleta = as.character(NA),
         coletor = as.character(NA),
         referencia = as.character(NA),
         observacoes = as.character(NA),
         ano_fim = as.double(ano_inicio),
         ID_name = 'GBIF',
         ID_number = 1:n()) %>%
  unite ( ID, ID_name, ID_number) %>%
  type_convert(cols(ano_inicio = "?",.default = "c"))

#merging data frames

united2 <- union(goeldi, specieslink) %>%
  mutate(incerteza = as.double(NA))

#In this step, we did not combine GBIF with the other datasets because this data 
#lacked a municipality column, which required us to adapt the qualifier to maintain 
#consistency in our approach.

      #qualifying the data frames ------------------------- 
#1. conditional function with x = "municipio" and y = "longitude" and z = "localidade"
qualifier <- function(x, y, z){
  case_when(
    !is.na(y) ~ print("otimo"),
    !is.na(x) & is.na(z) ~ print("ruim"),
    is.na(x) & !is.na(z) ~ print("ruim"),
    !is.na(x) & !is.na(z) ~ print("ruim"),
    is.na(x) & is.na(z) ~ print("pessimo"),
  )
}

#2. using mutate to create the qualifier column, whit the first brackets are 
#column number of municipio and the second are column number of longitude
united2 <- united2 %>% 
  mutate(qualificador = qualifier(municipio, longitude, localidade))

gbif<- gbif %>%
  mutate(qualificador = case_when(
    !is.na(longitude) ~ print("otimo"),
    is.na(localidade) ~ print("pessimo"),
    !is.na(localidade) ~ print("ruim"),
  ))

#WARNING: as GBIF does not include "municipio", the "ruim" qualifier refers to
#column "localidade"

   #merging gbif with others downloaded data frames ---------

united2 <- union(united2, gbif)

   #merging all data frames ------------------------------
#create variables from downloaded data frames

united <- united %>%
  mutate(incerteza = as.double(NA))

unitedfinal <- union(united, united2) 
#corrigindo qualificadores
unitedfinal2 <- unitedfinal %>%
  mutate(qualificador2 = case_when(
    localidade == "?" ~ print("pessimo"),
    localidade != "?" ~ qualificador,
    is.na(localidade) ~ qualificador,
  )) %>%
  select(-qualificador) %>%
  rename(qualificador=qualificador2)

#exporting final data frame --------------------------
write_csv(unitedfinal2, "./Data/Processados/1_planilhaunida.csv")