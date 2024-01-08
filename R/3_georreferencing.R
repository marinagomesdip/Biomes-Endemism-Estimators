#Georeferencing data
#Marina Morim Gomes
#marinagomesdiptera@mn.ufrj.br
#2022

#loading packages -------------------------------------
library(tidyverse)

#loading data -------------------------------------
original <- read_csv("./Data/Processados/2_planilhafiltrada.csv")
coordsmun <- read_csv("./Data/coordenadas_municipio.csv")
coordslocal <- read_csv("./Data/Specieslink_geoloc_paraR.csv")

#Join the dataframe with coords data ---------------------------------------
#mutating a column with municipio_estado so you don't have a problem with 
#the same municipalities names in different states
original <- original %>%
  mutate(municipio2 = municipio,
         estado2 = estado_ou_provincia,
         localidade2 = localidade,
         estado3 = estado_ou_provincia) %>%
  unite (chave, municipio2, estado2) %>%
  unite (chave2, localidade2, estado3)

coordsmun <- coordsmun %>%
  mutate(municipio2 = municipio,
         estado2 = estado_ou_provincia) %>%
  unite (chave, municipio2, estado2)

coordslocal <- coordslocal %>%
  mutate(localidade2 = localidade,
         estado2 = estado) %>%
  unite (chave2, localidade2, estado2)

#change the name of coords columns so you don't have problems in overwriting 
#the original columns in the union
coordsmun <- coordsmun %>%
  rename (latitude_cal = latitude,
          longitude_cal = longitude,
          incerteza_cal = incerteza) %>%
  select (latitude_cal, longitude_cal, incerteza_cal, chave)

coordslocal <- coordslocal %>%
  rename (latitude_cal2 = latitude,
          longitude_cal2 = longitude) %>%
  select (latitude_cal2, longitude_cal2, chave2)

#merging original with coordsmun and coordslocal to georreferencing data ---------- 
georreferenciada <- left_join(original, coordsmun, by = "chave")

georreferenciada <- left_join(georreferenciada, coordslocal, by = "chave2")

#Uniting the original coords with calculated coords, always keeping the original >> calculated ------------------

#As the prevailing coordinate should be the locality's, the process needs to be 
#done first with it, and then with the municipality's

####### Locality coordinates
#conditional to print the correct original latitude when its is associated with
#the obsevation and locality latitude when there is no original latitude:
georreferenciada <- georreferenciada %>% 
  mutate(latitude_comloc = ifelse(is.na(latitude), latitude_cal2, latitude),
         longitude_comloc = ifelse(is.na(longitude), longitude_cal2, longitude))

####### Municipalities coordinates
#conditional to print the correct original latitude when its is associated with
#the obsevation and calculated latitude when there is no original latitude
georreferenciada <- georreferenciada %>% 
  mutate(latitude_locmun = ifelse(is.na(latitude_comloc), latitude_cal, latitude_comloc),
         longitude_locmun = ifelse(is.na(longitude_comloc), longitude_cal, longitude_comloc))

#conditional to print the correct uncertainty associated with the coord
#used (original or calculated)
georreferenciada <- georreferenciada %>% 
  mutate(incerteza_locmun = ifelse(latitude_locmun == latitude_cal, incerteza_cal,
                                  incerteza))

#removing coords columns and keeping only the finals
georreffinal <- georreferenciada %>%
  select ( - latitude, - longitude, - incerteza, - latitude_cal, - longitude_cal,
           - incerteza_cal, - chave, - chave2, -latitude_cal2, - longitude_cal2, 
           - latitude_comloc, - longitude_comloc) %>%
  rename (latitude = latitude_locmun,
          longitude = longitude_locmun,
          incerteza = incerteza_locmun)

#exporting final data frame --------------------------
write_csv(georreffinal, "./Data/Processados/3_georreferenciada.csv")
