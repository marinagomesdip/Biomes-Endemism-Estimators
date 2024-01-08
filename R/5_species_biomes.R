#Analyzing biome and endemism data
#Marina Morim Gomes
#marinagomesdiptera@mn.ufrj.br
#2023

#loading packages -------------------------------------
library(tidyverse)
library(sf)
library(geobr)

#loading data -------------------------------------
original <- read_csv("./Data/Processados/4_dadoslimpos.csv")
mma <- read_biomes(year = 2019, showProgress = FALSE)
muni <- read_municipality(code_muni = "all", year = 2020, showProgress = FALSE)

#CRS convert in Biome map
mma <- mma %>%
  filter(name_biome != "Sistema Costeiro") %>% 
  st_transform(crs = 4326)

#CRS convert in Municipality map
muni <- muni %>%
  st_transform(crs = 4326)

#Extracting biomes -----------------------------

#Organazing data to extract

#(removing coordinates flagged with ocean by CoordinateCleaner)
#(convert into spatial points)
distribution <- original %>%
  select(nome_cientifico, latitude, longitude) %>%
  distinct()

distribution2 <- distribution %>%
  st_as_sf(coords = c("longitude","latitude"), remove = FALSE)

#atributing the WGS84 CRS to the occurence point
st_crs(distribution2) <- 4326  

#removing points outside mma map 
distribution2 <- distribution2[mma, ]

#Spatial joing to extract values of biomes from MMA map 
biomas <- st_join(distribution2, mma["name_biome"])

#organizing data to export 
biomas <- biomas %>%
  rename(Biome = name_biome)

#removing columns i'm not interested and duplicates
biomas2 <- st_set_geometry(biomas, NULL) %>%
  select (nome_cientifico, Biome) %>%
  distinct() %>%
  mutate(Biome = recode(Biome,
                        "Mata Atlântica" = "Atlantic Rainforest",
                        "Amazônia" = "Amazon Forest"))

#exporting raw data ---------------------------------------------
write_csv(biomas,"./Data/Processados/5_biomas_por_especie.csv")

#QUESTIONS -------------------------------------------------------
#How many species per biome?(Table 1) ------------------------------
table1 <- biomas2 %>%
  count(Biome) 

table1$Biome <- factor(table1$Biome, levels = table1$Biome[order(table1$n)])

grafic1 <- ggplot(data = table1) + 
  geom_bar(mapping = aes(x= Biome, y= n), stat = "identity", color = "black",
           fill = "black", width = 0.5) +
  ylab("Species richeness") +
  xlab("Biome") +
  theme_bw()


#exporting 
ggsave("C1Figure1.png", plot = grafic1, width = 6, height = 3, dpi = 300)

#Which species exist in each biome? (Apendice2) ------------------------------
porbioma <- biomas2 %>%
  mutate(Biome = recode(Biome,
                        "Atlantic Rainforest" = "mata_atlantica",
                        "Amazon Forest" = "amazonia"))

SM2 <- porbioma %>%
  pivot_wider(names_from = Biome, values_from = Biome) %>%
  mutate(mata_atlantica = if_else(mata_atlantica == "mata_atlantica", 1, 0, missing = 0),
         amazonia = if_else(amazonia == "amazonia", 1, 0, missing = 0),
         Pampa = if_else(Pampa == "Pampa", 1, 0, missing = 0),
         Cerrado = if_else(Cerrado == "Cerrado", 1, 0, missing = 0),
         Pantanal = if_else(Pantanal == "Pantanal", 1, 0, missing = 0),
         Caatinga = if_else(Caatinga == "Caatinga", 1, 0, missing = 0))

#exporting
write_csv(SM2,"./Data/Processados/5_SM2.csv")

#Which species are endemic and how many endemic per biome?----------------------------------- 
#filtering only species thar occur in one biome
endemicas <- biomas2 %>%
  group_by(nome_cientifico) %>%
  filter(n_distinct(Biome) == 1) %>% 
  select(nome_cientifico, Biome) %>%
  distinct() %>%
  ungroup()

#exporting to validate data in literature
write_csv(endemicas,"./Data/Processados/5_endemicasparavalidar.csv")

#loading endemic list validated
validada <- read_csv("./Data/Processados/5_endemicasvalidadas.csv")

#using the quartil1 (Q1) to filter endemic species 
occs <- distribution %>%
  count(nome_cientifico)

    #calculating Q1
percentil_25 <- quantile(occs$n, 0.25)
print(percentil_25)
    #filtering occs removing the values <Q1
occs <- occs %>%
  filter(n > 2)
    #filtering species list to remove species with occs =<2
validada <- validada %>%
  semi_join(occs, by = "nome_cientifico")

#ploting in a ggplot2 graphic
endemiporbioma <- validada %>%
  count(Biome)

endemiporbioma$Biome <- factor(endemiporbioma$Biome, levels = endemiporbioma$Biome[order(endemiporbioma$n)])

# Creating a chart by combining richness and endemism data per biome

# Combining the two datasets
endemirich <- left_join(table1, endemiporbioma, by = "Biome")
#renaming columns and organizing the order of bar in graphic
endemirich <- endemirich %>% 
  rename(richness = n.x) %>%
  rename(endemic = n.y) %>%
  arrange(desc(richness))

endemirich$Biome <- factor(endemirich$Biome, levels = endemirich$Biome)
# Pivot to make it easy to create the chart
endemirich <- endemirich %>%
  pivot_longer(c(`richness`, `endemic`), names_to = "type", values_to = "n")

# Plotting the new object on the chart
grafic2 <- ggplot(data = endemirich, aes(x = Biome, y = n, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  scale_fill_manual(values=c("steelblue", "black")) +
  ylab("N") +
  theme_bw() +
  theme(legend.position = "none")
grafic2 

#Number of endemic species per genera
endemiporgen <- validada %>%
  separate(nome_cientifico, into = c("genus", "species")) %>%
  count(genus)

#exporting final results
ggsave("Figure1A.png", plot = grafic2, width = 6, height = 3, dpi = 300)
write_csv(validada,"./Data/Processados/5_finalendemiclist.csv")

#How many species have only one record in the data set? -------------
onlyone <- distribution %>%
  count(nome_cientifico) %>%
  filter(n == 1)

#How many species per genus per biome? -----

#Retrieving data from the original dataset
original <- original %>%
  unite(latlong, latitude, longitude, remove = FALSE, sep = "/")

biomas3 <- st_set_geometry(biomas, NULL) %>%
  unite(latlong, latitude, longitude, remove = FALSE, sep = "/") %>%
  distinct() %>%
  mutate(Biome = recode(Biome,
                        "Mata Atlântica" = "Atlantic Rainforest",
                        "Amazônia" = "Amazon Forest"))

originalcombiomas <- left_join(biomas3, original, by = "latlong") %>%
  filter (nome_cientifico.x == nome_cientifico.y) %>%
  select(nome_cientifico.x, latitude.x, longitude.x, Biome, projeto, pais, estado_ou_provincia,
         municipio, localidade, data, metodo_coleta, determinador, coletor, referencia, observacoes,
         ID, qualificador, incerteza, genero, especie)

generoporbioma <- originalcombiomas %>%
  group_by(genero, Biome) %>%
  summarise(count = n_distinct(especie))

generoporbioma <- generoporbioma %>%
  pivot_wider(names_from = Biome, values_from = count)

#exporting
write_csv(generoporbioma,"./Data/Processados/5_table_1.csv")

#Are the Number of Municipallity sampled and richness related? ------
biomuni <- st_join(biomas, muni["code_muni"]) %>%
  mutate(Biome = recode(Biome,
                        "Mata Atlântica" = "Atlantic Rainforest",
                        "Amazônia" = "Amazon Forest")) %>%
  group_by(Biome) %>%
  summarise(richness = n_distinct(nome_cientifico, na.rm = TRUE),
            mun_sampled = n_distinct(code_muni, na.rm = TRUE))

biomuni <- st_set_geometry(biomuni, NULL)
biomuni <- biomuni %>%
  arrange(desc(richness))
biomuni$Biome <- factor(biomuni$Biome, levels = biomuni$Biome)

biomuni <- biomuni %>% 
  pivot_longer(c(`richness`, `mun_sampled`), names_to = "type", values_to = "n")

#ploting
grafic3 <- ggplot(data = biomuni, aes(x = Biome, y = n, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  scale_fill_manual(values=c("steelblue", "black")) +
  ylab("N") +
  theme_bw() +
  theme(legend.position = "none")

#exporting 
ggsave("C1Figure3.png", plot = grafic3, width = 6, height = 3, dpi = 300)