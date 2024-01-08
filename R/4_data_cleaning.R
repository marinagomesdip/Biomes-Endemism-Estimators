#Data Cleaning
#Workflow from Sara Mortara with modifications
#https://github.com/saramortara/data_cleaning
#marinagomesdiptera@mn.ufrj.br
#2023


#loading packages -------------------------------------
library(tidyverse)
library(CoordinateCleaner)

#loading data -------------------------------------
original <- read_csv("./Data/Processados/3_georreferenciada.csv")

#checking species taxonomy --------------------------------------

#creating a Species list name 
original <- original %>%
  unite (species_name, genero, especie, sep = " ", remove = FALSE) %>%
  select (-genero_especie)

spnames <- original %>%
  select (species_name) %>%
  distinct ()

#exporting this list to create a synonymy list based on data 
write_csv(spnames, "./Data/Processados/4_prasinonimos.csv")

#importing the synonymy list to compare with the data 
synonymy <- read_csv("./Data/sinonimos.csv")

#merging original data with synonymy
datacleantaxo <- left_join(original, synonymy, by = "species_name")

#counting how many names were corrected (wrong spelling and synonyms)
synonymy %>%
  count(nome_corrigido)

#correcting the database without overwriting the original data
#and filtering the columns to remove species that were not found
datacleantaxo <- datacleantaxo %>%
  select(-genero, -especie, -species_name) %>%
  separate(species_modified, into = c("genero", "especie"), sep = " ", remove = FALSE) %>%
  filter (nome_corrigido != "NA")

#checking species coordinates -------------------------------------

#Removing missing coordinate data (=NA)
datacleancoord <- datacleantaxo[!is.na(datacleantaxo$latitude) 
                   & !is.na(datacleantaxo$longitude),]

#using clean_coordinates to flag suspicious coordinates
datacleancoord <- clean_coordinates(x = datacleancoord, 
                               lon = "longitude",
                               lat = "latitude",
                               species = "species_modified", 
                               value = "spatialvalid")

#organizing the final dataframe
datacleancoord <- datacleancoord %>%
  rename(nome_cientifico = species_modified,
         nome_especie_modificado = nome_corrigido,
         coordenadas_validas = .val,
         latitude_longitudes_repetidas = .equ,
         coordenadas_zeradas = .zer,
         capitais_paises = .cap,
         centroide_pais = .cen,
         coordenadas_no_oceano = .sea,
         outlier_geografico = .otl,
         GBIF_matrix = .gbf,
         instituições = .inst,
         resumo_teste_coordenadas = .summary)

#exporting ---------------------------------------------
write_csv(datacleancoord, "./Data/Processados/4_dadoslimpos.csv")