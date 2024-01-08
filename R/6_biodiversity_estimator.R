#Species Accumulation curve 
#Sarcophagidae Biomes 
#Marina Morim Gomes 2023
#marinagomesdiptera@mn.ufrj.br

#loading packages -------------------------------------
library(tmap)
library(tidyverse)
library(sf)
library(geobr)
library(vegan)
library(iNEXT)

#loading data ---------------------------------------------
sarcodistr <- read_csv("./Data/Processados/4_dadoslimpos.csv")
mma <- read_biomes(year = 2019, showProgress = FALSE)
BR <- read_country(year = 2020,  showProgress = FALSE)

#Plotting the wealth values on a map using standardized units-----------------
#selecting occurence data and converting to sf object
sarcodistr <- sarcodistr %>%
  filter(pais == "Brasil") %>%
  select(nome_cientifico, latitude, longitude) %>%
  distinct() %>%
  rename(lon = longitude, lat = latitude) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

#CRS convert in map
mma <- mma %>%
  filter(name_biome != "Sistema Costeiro") %>% 
  st_transform(crs = 4326)

BR <- BR %>%
  st_transform(crs = 4326)

#visualizing the occurrence points on the map
tmap::tmap_mode(mode = "plot")

tm_shape(mma, 
         bbox = sarcodistr) +
  tm_polygons() +
  tm_shape(sarcodistr) +
  tm_dots(size = .1, col = "forestgreen")

#removing outliers 
sarcodistr <- sarcodistr[mma, ]

#creating hexagonal grid with each hexagon measuring 10 000 km²
grid_hex <- st_make_grid(
  x = mma, cellsize = 0.5, square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(areakm2 = sf::st_area(.)/1e6) %>% 
  rowid_to_column("id_hex")

#selecting hexagons only in Brazil limits 
grid_hex <- grid_hex[mma, ]

#spatial join of the occurrence data with the hexagons
grid_hex <- st_join(
  x = grid_hex, 
  y = sarcodistr,
  left = TRUE)

#aggregating number of species and occurences per hexagon
oco_riq <- grid_hex %>% 
  group_by(id_hex) %>% 
  summarise(occurrences = length(nome_cientifico[!is.na(nome_cientifico)]),
            richness = n_distinct(nome_cientifico, na.rm = TRUE))

#merging richness and biome information into a single dataset
oco_riq <- st_join(
  x = oco_riq, 
  y = mma,
  left = TRUE) %>%
  select(-code_biome, -year)

#ploting richness in the map
tmap_mode(mode = "plot")

mapa_riq <- tm_shape(oco_riq) +
  tm_polygons(col = "richness", breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                          11, 12, 20, 35, 50, 65, 80, 95, 120),
              palette = "Greys") +
  tm_graticules(lines = FALSE) +
  tm_compass() +
  tm_scale_bar() +
  tm_layout( legend.outside = TRUE,
             legend.outside.size = 0.2)

mapa_riq2 <- mapa_riq +
  tm_shape(mma) +
  tm_borders(col = "#404040", lwd = 1)

#Exporting map
tmap_save(tm = mapa_riq2, filename = "mapariqueza.png", width = 3000, height = 2800)

#ploting ocurrence per state
tmap_mode(mode = "plot")

mapa_oco <- tm_shape(oco_riq) +
  tm_polygons(col = "occurrences", breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                              50, 100, 150, 200, 250, 280),
              palette = "Greys") +
  tm_graticules(lines = FALSE) +
  tm_compass() +
  tm_scale_bar() +
  tm_layout( legend.outside = TRUE,
             legend.outside.size = 0.2)

mapa_oco2 <- mapa_oco +
  tm_shape(BR) +
  tm_borders(col = "#404040", lwd = 1)

#Exporting map
tmap_save(tm = mapa_oco2, filename = "mapastate.png", width = 3000, height = 2800)

#Biodiversity estimators--------------------------------------
#preparing the dataframe for the required format:--------------
#creating a new object:
obser <- grid_hex

#removing the geometry
st_geometry(obser) <- NULL

#generating a list of species for each hexagon, setting count = 0 to avoid errors in the pivot function
specieslist <- obser %>%
  select(nome_cientifico) %>%
  distinct() %>%
  filter(nome_cientifico != "NA") %>%
  mutate(hexago = "hex",
         count = 0) %>%
  pivot_wider(names_from = nome_cientifico, values_from = count)

#replicating that line 2124 times
novas_linhas <- replicate(8419, specieslist, simplify = FALSE)

#converting the list of new lines into a data frame
novas_linhas_df <- bind_rows(novas_linhas)

#combining the new lines with the original dataframe 'df' and converting it to a tibble
specieslist <- bind_rows(specieslist, novas_linhas_df) %>%
  as_tibble() %>%
  mutate(id_number = 1:n()) %>%
  unite(amostra_hex, hexago, id_number) %>%
  pivot_longer(c(`Nephochaetopteryx paraensis`, `Nephochaetopteryx paraensis`,
                 `Nephochaetopteryx psittacocercus`, `Peckia intermutans`,
                 `Peckia anguilla`, `Engelimyia inops`,
                 `Peckiamyia expuncta`, `Peckia florencioi`,
                 `Peckia tridentata`, `Peckia lambens`,
                 `Peckia ingens`, `Nephochaetopteryx sofiae`,
                 `Nephochaetopteryx utinguensis`, `Lepidodexia distincta`,
                 `Oxysarcodexia avuncula`, `Dexosarcophaga globulosa`,
                 `Tricharaea occidua`, `Peckia epimelia`, `Tricharaea canuta`,
                 `Tricharaea brasiliensis`, `Lepidodexia teffeensis`,
                 `Tricharaea brevicornis`, `Peckia trivittata`,
                 `Helicobia pilifera`, `Oxysarcodexia amorosa`,
                 `Oxysarcodexia angrensis`, `Oxysarcodexia fringidea`,
                 `Oxysarcodexia major`, `Oxysarcodexia thornax`,
                 `Peckia alvarengai`, `Peckia collusor`,
                 `Peckia pallidipilosa`, `Peckia smarti`,
                 `Peckia chrysostoma`, `Peckia pexata`,
                 `Peckiamyia abnormalis`, `Peckiamyia minutipenis`,
                 `Sarcofahrtiopsis cuneata`, `Titanogrypa luculenta`,
                 `Lepidodexia pacta`, `Nephochaetopteryx similis`,
                 `Argoravinia alvarengai`, `Dexosarcophaga ampullula`,
                 `Helicobia aurescens`, `Helicobia morionella`, 
                 `Oxysarcodexia adunca`, `Oxysarcodexia carvalhoi`,
                 `Oxysarcodexia nitida`, `Tricharaea ramirezi`,
                 `Tricharaea apicata`, `Titanogrypa alvarengai`,
                 `Retrocitomyia retrocita`, `Ravinia effrenata`, 
                 `Ravinia belforti`, `Nephochaetopteryx spinosa`,
                 `Oxysarcodexia timida`, `Dexosarcophaga transita`,
                 `Lepidodexia albida`, `Blaesoxipha tingomaria`,
                 `Blaesoxipha rimosa`, `Peckia subducta`,
                 `Argoravinia rufiventris`, `Argoravinia aurea`, 
                 `Nephochaetopteryx matinta`, `Nephochaetopteryx pallidifacies`,
                 `Nephochaetopteryx pallidiventris`, `Oxysarcodexia intona`,
                 `Duckemyia latifrons`, `Oxysarcodexia bakeri`,
                 `Peckia uncinata`, `Dexosarcophaga carvalhoi`,
                 `Ravinia advena`, `Ravinia aureopyga`,
                 `Oxysarcodexia wygodzinskyi`, `Oxysarcodexia parva`,
                 `Retrocitomyia mizuguchiana`, `Titanogrypa ihenringi`,
                 `Lipoptilocnema salobrensis`, `Lepidodexia fuscianalis`,
                 `Oxysarcodexia berlai`, `Retrocitomyia sisbiota`, 
                 `Peckia enderleini`, `Ravinia almeidai`,
                 `Orosarcophaga excisa`, `Tricharaea indonata`,
                 `Titanogrypa cryptopyga`, `Retrocitomyia fluminensis`, 
                 `Udamopyga setigena`, `Retrocitomyia paraguayensis`, 
                 `Titanogrypa larvicida`, `Lipoptilocnema crispula`, 
                 `Emblemasoma lutzi`, `Oxysarcodexia varia`, 
                 `Oxysarcodexia meridionalis`, `Dexosarcophaga paulistana`,
                 `Helicobia rapax`, `Helicobia pilipleura`, `Lipoptilocnema crispina`,
                 `Oxysarcodexia occulta`, `Dexosarcophaga limitata`,
                 `Helicobia chapadensis`, `Nephochaetopteryx molinai`, 
                 `Argoravinia catiae`, `Sinopiella rotunda`, 
                 `Blaesoxipha hunteri`, `Lepidodexia opima`, 
                 `Oxysarcodexia aura`, `Oxysarcodexia eberti`, 
                 `Lipoptilocnema misella`, `Tripanurga albicans`, 
                 `Sarcophaga polistensis`, `Peckia roppai`, `Sinopiella rufopilosa`, 
                 `Peckia resona`, `Blaesoxipha minensis`, 
                 `Lepidodexia rosaliae`, `Titanogrypa minensis`,
                 `Oxysarcodexia xanthosoma`, `Peckia urceola`,
                 `Oxysarcodexia paulistanensis`, `Oxysarcodexia terminalis`,
                 `Oxysarcodexia culmiforceps`, `Oxysarcodexia fraterna`,
                 `Oxysarcodexia vittata`, `Udamopyga proveta`,
                 `Oxysarcodexia mineirensis`, `Oxysarcodexia admixta`, `Oxysarcodexia diana`,
                 `Blaesoxipha brazil`, `Blaesoxipha convena`, 
                 `Udamopyga percita`, `Helicobia borgmeiri`, `Oxysarcodexia injuncta`,
                 `Dexosarcophaga aurifacies`, `Oxysarcodexia grandis`,
                 `Emdenimyia limai`, `Argoravinia paraensis`,
                 `Lepidodexia calliphorina`, `Oxysarcodexia bicolor`,
                 `Ravinia haemorrhoidalis`, `Chrysagria duodecimpunctata`,
                 `Oxysarcodexia marina`, `Boettcheria aurifera`,
                 `Blaesoxipha acridiophagoides`, `Emdenimyia lanei`,
                 `Lepidodexia sarcophagina`, `Microcerella halli`,
                 `Microcerella wygodzinskyi`, `Lipoptilocnema lanei`,
                 `Oxysarcodexia culminata`, `Oxysarcodexia confusa`,
                 `Peckia australis`, `Oxysarcodexia floricola`,
                 `Lepidodexia travassosi`, `Lepidodexia teutonia`,
                 `Udamopyga squamata`, `Nephochaetopteryx fuscipennis`,
                 `Nephochaetopteryx lamasi`, `Lepidodexia matogrossensis`,
                 `Oxysarcodexia riograndensis`, `Argoravinia brasiliana`,
                 `Oxysarcodexia modesta`, `Blaesoxipha denieri`,
                 `Microcerella pilicoxa`, `Microcerella analis`,
                 `Nephochaetopteryx cyaneiventris`, `Nephochaetopteryx travassosi`,
                 `Dexosarcophaga campina`, `Helicobia cametaensis`,
                 `Helicobia domquixote`, `Metopia fofo`,
                 `Dexosarcophaga pusilla`, `Lepidodexia ochristriga`,
                 `Lepidodexia grisea`, `Lepidodexia setifrons`,
                 `Lepidodexia vittata`, `Oxysarcodexia fluminensis`,
                 `Dexosarcophaga itaqua`, `Dexosarcophaga bicolor`,
                 `Tricharaea femoralis`, `Oxysarcodexia petropolitana`,
                 `Oxysarcodexia morretesi`, `Sarcophaga africa`,
                 `Titanogrypa chlorogaster`, `Udamopyga malacophila`,
                 `Blaesoxipha inornata`, `Dexosarcophaga bermudezi`,
                 `Blaesoxipha rudis`, `Lepidodexia bocainensis`,
                 `Blaesoxipha rocciai`, `Blaesoxipha lanei`,
                 `Engelimyia bosqi`, `Blaesoxipha caridei`,
                 `Blaesoxipha plinthopyga`, `Malacophagomyia filamenta`,
                 `Microcerella erythropyga`, `Nephochaetopteryx orbitalis`,
                 `Oxysarcodexia simplicoides`, `Lipoptilocnema savana`,
                 `Oxysarcodexia insolita`, `Oxysarcodexia villosa`,
                 `Dexosarcophaga avispaensis`, `Peckia veropeso`,
                 `Lepidodexia parva`, `Sarcophaga ruficornis`,
                 `Lepidodexia grisescens`, `Malacophagula neotropica`,
                 `Lepidodexia confusa`, `Dexosarcophaga lenkoi`,
                 `Helicobia setinervis`, `Retrocitomyia urumajoensis`,
                 `Villegasia almeidai`, `Peckia lutzi`,
                 `Peckia gulo`, `Dexosarcophaga tupinamba`,
                 `Lepidodexia egregia`, `Peckia hillifera`,
                 `Oxysarcodexia augusta`, `Lepidodexia brevigaster`,
                 `Blaesoxipha aix`, `Nephochaetopteryx tembe`,
                 `Oxysarcodexia inflata`, `Oxysarcodexia conclausa`,
                 `Lepidodexia cognata`, `Microcerella travassosi`,
                 `Lepidodexia malacophaga`, `Peckia pascoensis`,
                 `Dexosarcophaga tracua`, `Lepidodexia brevirostris`,
                 `Macronychia ornata`, `Rafaelia ampulla`,
                 `Lepidodexia tetraptera`, `Lepidodexia facialis`,
                 `Microcerella chaetosa`, `Orobrachycoma ornata`,
                 `Lepidodexia frontalis`, `Metopia brasiliana`,
                 `Lepidodexia aurea`, `Senotainia brasiliensis`,
                 `Orodexia ornata`, `Dexosarcophaga montana`,
                 `Lepidodexia centenaria`, `Lepidodexia oliveirai`,
                 `Oxysarcodexia xon`, `Oxysarcodexia digitata`,
                 `Orosarcophaga ornata`, `Tricharaea macrophthalma`,
                 `Emblemasoma zikani`, `Nephochaetopteryx angustifrons`,
                 `Nephochaetopteryx lopesi`, `Emblemasoma prosternale`,
                 `Dexosarcophaga angrensis`, `Lepidodexia lenti`,
                 `Boettcheria retroversa`, `Nephochaetopteryx flavipalpis`,
                 `Udamopyga diversa`, `Udamopyga neivai`,
                 `Oxysarcodexia comparilis`, `Oxysarcodexia ariozanoi`,
                 `Nephochaetopteryx biculcita`, `Nephochaetopteryx tinguensis`,
                 `Peckia adolenda`, `Titanogrypa fimbriata`,
                 `Helicobia iheringi`, `Lepidodexia fumipennis`,
                 `Lepidodexia diversa`, `Lepidodexia cyaneiventris`,
                 `Emblemasoma fumipenne`, `Dexosarcophaga inaequalis`,
                 `Boettcheria marstoni`, `Lepidodexia pseudoturbatus`,
                 `Lepidodexia propinquus`, `Lepidodexia camorim`,
                 `Retrocitomyia adolenda`, `Metopia cubitosetigera`,
                 `Dexosarcophaga rudicompages`, `Emblemasoma erro`,
                 `Thomazomyia fluminensis`, `Dexosarcophaga succinta`,
                 `Lepidodexia bufonivora`, `Peckia abrupta`,
                 `Microcerella austrohartigia`, `Helicobia biplagiata`,
                 `Sarcophaga guyanensis`, `Thomazomyia kempfi`,
                 `Rettenmeyerina serrata`, `Tapacura mariarum`,
                 `Malacophagomyia kesselringi`, `Peckia villegasi`),
               names_to = "especie", values_to = "count")

obser <- obser %>%
  select(id_hex,nome_cientifico) %>%
  mutate(count = ifelse(is.na(nome_cientifico), 0, 1)) %>%
  mutate(name = ifelse(is.na(nome_cientifico), "semespecie", NA)) %>%
  mutate(ID_number = 1:n()) %>%
  unite(vazios, name, ID_number, sep = "_") %>%
  mutate(especie = ifelse(is.na(nome_cientifico), vazios, nome_cientifico)) %>%
  select(-nome_cientifico, -vazios) %>%
  mutate(hexago = "hex") %>%
  unite(amostra_hex, hexago, id_hex, sep = "_")

obsertotal <- left_join(obser, specieslist, by = "amostra_hex") %>%
  mutate(count_final = ifelse(especie.x == especie.y, 1, 0)) %>%
  select(-count.x, -especie.x, -count.y) %>%
  pivot_wider(names_from = especie.y, values_from = count_final)

#function to replace c(0,1) with 1 and c(0,0) with 0
replace_values <- function(...) {
  values <- c(...)
  if (any(values == 1)) {
    return(1)
  } else {
    return(0)
  }
}

#applying the function rowwise with across to replace the values in the columns, except for 'amostra_hex'
obsertotal <- obsertotal %>%
  rowwise() %>%
  mutate(across(-amostra_hex, replace_values))

#splitting the dataset into subdatasets by biomes ----------------------------
#create a function to determine the majority biome in a transition zone
majority_biome <- function(biomes) {
  table_result <- table(biomes)
  names(table_result)[which.max(table_result)]
}

#group by id_hex and calculate the majority biome in case of multiple biomes
hexbiome <- oco_riq %>%
  group_by(id_hex) %>%
  summarise(bioma = ifelse(length(unique(name_biome)) > 1, majority_biome(name_biome), unique(name_biome))) %>%
  mutate(hexago = "hex") %>%
  unite(amostra_hex, hexago, id_hex, sep = "_")

#removing the geometry
st_geometry(hexbiome) <- NULL

#filtering only hexagons from the Atlantic Forest
hexMA <- hexbiome %>%
  filter(bioma == "Mata Atlântica") %>%
  select(-bioma)

hexMA <- left_join(hexMA, obsertotal, by = "amostra_hex")
#removing hexagons with richness = 0 and species that not occur in MA
hexMA <- hexMA %>%
  filter(if_any(-amostra_hex, ~. != 0)) %>%
  select(amostra_hex, where(~ is.numeric(.) && sum(.) != 0))

#filtering only hexagons from the Amazonia Forest
hexAM <- hexbiome %>%
  filter(bioma == "Amazônia") %>%
  select(-bioma)

hexAM <- left_join(hexAM, obsertotal, by = "amostra_hex")
#removing hexagons with richness = 0 and species that not occur in AM
hexAM <- hexAM %>%
  filter(if_any(-amostra_hex, ~. != 0)) %>%
  select(amostra_hex, where(~ is.numeric(.) && sum(.) != 0))

#filtering only hexagons from the Pantanal
hexPT <- hexbiome %>%
  filter(bioma == "Pantanal") %>%
  select(-bioma)

hexPT <- left_join(hexPT, obsertotal, by = "amostra_hex")
#removing hexagons with richness = 0 and species that not occur in PT
hexPT <- hexPT %>%
  filter(if_any(-amostra_hex, ~. != 0)) %>%
  select(amostra_hex, where(~ is.numeric(.) && sum(.) != 0))

#filtering only hexagons from the Cerrado
hexCE <- hexbiome %>%
  filter(bioma == "Cerrado") %>%
  select(-bioma)

hexCE <- left_join(hexCE, obsertotal, by = "amostra_hex")
#removing hexagons with richness = 0 and species that not occur in CE
hexCE <- hexCE %>%
  filter(if_any(-amostra_hex, ~. != 0)) %>%
  select(amostra_hex, where(~ is.numeric(.) && sum(.) != 0))

#filtering only hexagons from the Pampa
hexPP <- hexbiome %>%
  filter(bioma == "Pampa") %>%
  select(-bioma)

hexPP <- left_join(hexPP, obsertotal, by = "amostra_hex")
#removing hexagons with richness = 0 and species that not occur in PP
hexPP <- hexPP %>%
  filter(if_any(-amostra_hex, ~. != 0)) %>%
  select(amostra_hex, where(~ is.numeric(.) && sum(.) != 0))

#filtering only hexagons from the Caatinga
hexCA <- hexbiome %>%
  filter(bioma == "Caatinga") %>%
  select(-bioma)

hexCA <- left_join(hexCA, obsertotal, by = "amostra_hex")
#removing hexagons with richness = 0 and species that not occur in CA
hexCA <- hexCA %>%
  filter(if_any(-amostra_hex, ~. != 0)) %>%
  select(amostra_hex, where(~ is.numeric(.) && sum(.) != 0))


#calculating the estimators (based on https://analises-ecologicas.com/cap11.html)-----

#Mata Atlântica
#Estimators
estima <- poolaccum(hexMA, permutations = 100)
resultados_MA <- summary(estima, display = c("S", "chao", "jack1", "jack2", "boot"))
resultados_MA$chao <- setNames(resultados_MA$chao, c("Amostras", "Chao2", "Chao2.1", "Chao2.2"))
resultados_MA <- cbind(resultados_MA$chao[, 1:4], resultados_MA$S[, 2:4], resultados_MA$jack1[, 2:4],
                       resultados_MA$jack2[, 2:4], resultados_MA$boot[,2:4])

#rename columns
resultados_MA <- as.data.frame(resultados_MA) %>%
  setNames(make.unique(names(.))) %>%  
  rename(Amostras = N,
         Chao2 = Chao,
         C_inferior = "2.5%",
         C_superior = "97.5%",
         Riqueza = S, 
         R_inferior = "2.5%.1",
         R_superior = "97.5%.1",
         JACK1 = "Jackknife 1",
         J1_inferior = "2.5%.2",
         J1_superior = "97.5%.2",
         JACK2 = "Jackknife 2",
         J2_inferior = "2.5%.3",
         J2_superior = "97.5%.3",
         BOOT = Bootstrap,
         B_inferior = "2.5%.4",
         B_superior = "97.5%.4",)

#graphic
GraphicMA <- ggplot(resultados_MA) +
  geom_point(aes(y = Riqueza, x = Amostras, group = 1), size = 1, color = "black") +
  geom_point(aes(y = JACK1, x = Amostras, group = 1), size = 1, color = "steelblue") +
  geom_point(aes(y = JACK2, x = Amostras, group = 1), size = 1, color = "#871F78") + 
  geom_point(aes(y = BOOT, x = Amostras, group = 1), size = 1, color = "#A62A2A") + 
  geom_line(aes(y = Riqueza, x = Amostras), size = 1.2, color = "black") +
  geom_line(aes(y = JACK1, x = Amostras, group = 1), size = 1.2, color = "steelblue") +
  geom_line(aes(y = JACK2, x = Amostras, group = 1), size = 1.2, color = "#871F78") +
  geom_line(aes(y = BOOT, x = Amostras, group = 1), size = 1.2, color = "#A62A2A") +
  scale_x_continuous(breaks = seq(0, max(resultados_MA$Amostras), by = 20)) +
  labs(x = "Number of samples", y = "Species Richness") +
  theme_bw()

#exporting this Graphic
ggsave("EstimatorMA.png", plot = GraphicMA, width = 6, height = 4, dpi = 300)

#Extrapolation & Interpolation
#data preparing 
dados_inextMA <- hexMA %>%
  select(-amostra_hex) %>%
  select(where(~ is.numeric(.) && sum(.) != 0))

dados_inextMA <- as.incfreq(t(dados_inextMA))

iNEXTMA <- iNEXT(dados_inextMA, q = 0, datatype = "incidence_freq", 
                               endpoint = 180)
#Graphic
Graphic2MA <- ggiNEXT(iNEXTMA, type = 1) +
  scale_colour_manual(values = "black") +
  scale_fill_manual(values = "black") +
  labs(x = "Number of samples", y = "Species Richness") +
  theme_bw() +
  theme(legend.position="none")

ggsave("EstimatorMA2.png", plot = Graphic2MA, width = 6, height = 4, dpi = 300)

#Amazônia
#Estimators
estima <- poolaccum(hexAM, permutations = 100)
resultados_AM <- summary(estima, display = c("S", "chao", "jack1", "jack2", "boot"))
resultados_AM$chao <- setNames(resultados_AM$chao, c("Amostras", "Chao2", "Chao2.1", "Chao2.2"))
resultados_AM <- cbind(resultados_AM$chao[, 1:4], resultados_AM$S[, 2:4], resultados_AM$jack1[, 2:4],
                       resultados_AM$jack2[, 2:4], resultados_AM$boot[,2:4])

#rename columns
resultados_AM <- as.data.frame(resultados_AM) %>%
  setNames(make.unique(names(.))) %>%  
  rename(Amostras = N,
         Chao2 = Chao,
         C_inferior = "2.5%",
         C_superior = "97.5%",
         Riqueza = S, 
         R_inferior = "2.5%.1",
         R_superior = "97.5%.1",
         JACK1 = "Jackknife 1",
         J1_inferior = "2.5%.2",
         J1_superior = "97.5%.2",
         JACK2 = "Jackknife 2",
         J2_inferior = "2.5%.3",
         J2_superior = "97.5%.3",
         BOOT = Bootstrap,
         B_inferior = "2.5%.4",
         B_superior = "97.5%.4",)

#Graphic
GraphicAM <- ggplot(resultados_AM) +
  geom_point(aes(y = Riqueza, x = Amostras, group = 1), size = 1, color = "black") +
  geom_point(aes(y = JACK1, x = Amostras, group = 1), size = 1, color = "steelblue") +
  geom_point(aes(y = JACK2, x = Amostras, group = 1), size = 1, color = "#871F78") + 
  geom_point(aes(y = BOOT, x = Amostras, group = 1), size = 1, color = "#A62A2A") + 
  geom_line(aes(y = Riqueza, x = Amostras), size = 1.2, color = "black") +
  geom_line(aes(y = JACK1, x = Amostras, group = 1), size = 1.2, color = "steelblue") +
  geom_line(aes(y = JACK2, x = Amostras, group = 1), size = 1.2, color = "#871F78") +
  geom_line(aes(y = BOOT, x = Amostras, group = 1), size = 1.2, color = "#A62A2A") + 
  scale_x_continuous(breaks = seq(0, max(resultados_MA$Amostras), by = 20)) +
  labs(x = "Number of samples", y = "Species Richness") +
  theme_bw()

#exporting this Graphic
ggsave("EstimatorAM.png", plot = GraphicAM, width = 6, height = 4, dpi = 300)

#Extrapolation & Interpolation
#preparando dados 
dados_inextAM <- hexAM %>%
  select(-amostra_hex) %>%
  select(where(~ is.numeric(.) && sum(.) != 0))

dados_inextAM <- as.incfreq(t(dados_inextAM))

iNEXTAM <- iNEXT(dados_inextAM, q = 0, datatype = "incidence_freq", 
                 endpoint = 224)
## Gráfico
Graphic2AM <- ggiNEXT(iNEXTAM, type = 1) +
  scale_colour_manual(values = "black") +
  scale_fill_manual(values = "black") +
  labs(x = "Number of samples", y = "Species Richness") +
  theme_bw() +
  theme(legend.position="none")

ggsave("EstimatorAM2.png", plot = Graphic2AM, width = 6, height = 4, dpi = 300)

#Pantanal (REMOVIDO - SEM AMOSTRAS SUFICIENTES)
#Estimators
estima <- poolaccum(hexPT, permutations = 10)
resultados_PT <- summary(estima, display = c("S", "chao", "jack1", "jack2", "boot"))
resultados_PT$chao <- setNames(resultados_PT$chao, c("Amostras", "Chao2", "Chao2.1", "Chao2.2"))
resultados_PT <- cbind(resultados_PT$chao[, 1:4], resultados_PT$S[, 2:4], resultados_PT$jack1[, 2:4],
                       resultados_PT$jack2[, 2:4], resultados_PT$boot[,2:4])

#Renomeando as colunas após a combinação
resultados_PT <- as.data.frame(resultados_PT) %>%
  setNames(make.unique(names(.))) %>%  
  rename(Amostras = N,
         Chao2 = Chao,
         C_inferior = "2.5%",
         C_superior = "97.5%",
         Riqueza = S, 
         R_inferior = "2.5%.1",
         R_superior = "97.5%.1",
         JACK1 = "Jackknife 1",
         J1_inferior = "2.5%.2",
         J1_superior = "97.5%.2",
         JACK2 = "Jackknife 2",
         J2_inferior = "2.5%.3",
         J2_superior = "97.5%.3",
         BOOT = Bootstrap,
         B_inferior = "2.5%.4",
         B_superior = "97.5%.4",)

#Graphic
GraphicPT <- ggplot(resultados_PT) +
#  geom_point(aes(y = Chao2, x = Amostras, group = 1), size = 3, color = "darkorange") +
  geom_point(aes(y = Riqueza, x = Amostras, group = 1), size = 1, color = "black") +
#  geom_point(aes(y = JACK1, x = Amostras, group = 1), size = 3, color = "pink") +
#  geom_point(aes(y = JACK2, x = Amostras, group = 1), size = 3, color = "red") + 
#  geom_point(aes(y = BOOT, x = Amostras, group = 1), size = 3, color = "green") + 
#  geom_line(aes(y = Chao2, x = Amostras), color = "darkorange", size = 3) +
  geom_line(aes(y = Riqueza, x = Amostras), color = "black", size = 1) +
#  geom_line(aes(y = JACK1, x = Amostras, group = 1), size = 3, color = "pink") +
#  geom_line(aes(y = JACK2, x = Amostras, group = 1), size = 3, color = "red") +
#  geom_line(aes(y = BOOT, x = Amostras, group = 1), size = 3, color = "green") + 
  scale_x_continuous(breaks = seq(0, max(resultados_PT$Amostras), by = 50)) +
  labs(x = "Number of samples", y = "Species Richness")+
  theme_bw()

#exporting this Graphic
ggsave("EstimatorPT.png", plot = GraphicPT, width = 6, height = 4, dpi = 300)

#Extrapolation & Interpolation
#preparando dados 
#dados_inextPT <- hexPT %>%
#  select(-amostra_hex) %>%
#  select(where(~ is.numeric(.) && sum(.) != 0))

#dados_inextPT <- as.incfreq(t(dados_inextPT))

#iNEXTPT <- iNEXT(dados_inextPT, q = 0, datatype = "incidence_freq", 
#                 endpoint = 33)
## Gráfico
#Graphic2PT <- ggiNEXT(iNEXTPT, type = 1) +
#  scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +
#  scale_colour_manual(values = "darkorange") +
#  scale_fill_manual(values = "darkorange") +
#  labs(x = "Número de amostras", y = " Riqueza de espécies")

#ggsave("EstimatorPT2.png", plot = Graphic2PT, width = 6, height = 4, dpi = 300)

#Cerrado
#Estimators
estima <- poolaccum(hexCE, permutations = 100)
resultados_CE <- summary(estima, display = c("S", "chao", "jack1", "jack2", "boot"))
resultados_CE$chao <- setNames(resultados_CE$chao, c("Amostras", "Chao2", "Chao2.1", "Chao2.2"))
resultados_CE <- cbind(resultados_CE$chao[, 1:4], resultados_CE$S[, 2:4], resultados_CE$jack1[, 2:4],
                       resultados_CE$jack2[, 2:4], resultados_CE$boot[,2:4])

#Renomeando as colunas após a combinação
resultados_CE <- as.data.frame(resultados_CE) %>%
  setNames(make.unique(names(.))) %>%  
  rename(Amostras = N,
         Chao2 = Chao,
         C_inferior = "2.5%",
         C_superior = "97.5%",
         Riqueza = S, 
         R_inferior = "2.5%.1",
         R_superior = "97.5%.1",
         JACK1 = "Jackknife 1",
         J1_inferior = "2.5%.2",
         J1_superior = "97.5%.2",
         JACK2 = "Jackknife 2",
         J2_inferior = "2.5%.3",
         J2_superior = "97.5%.3",
         BOOT = Bootstrap,
         B_inferior = "2.5%.4",
         B_superior = "97.5%.4",)

#Graphic
GraphicCE <- ggplot(resultados_CE) +
  geom_point(aes(y = Riqueza, x = Amostras, group = 1), size = 1, color = "black") +
  geom_point(aes(y = JACK1, x = Amostras, group = 1), size = 1, color = "steelblue") +
  geom_point(aes(y = JACK2, x = Amostras, group = 1), size = 1, color = "#871F78") + 
  geom_point(aes(y = BOOT, x = Amostras, group = 1), size = 1, color = "#A62A2A") + 
  geom_line(aes(y = Riqueza, x = Amostras), size = 1.2, color = "black") +
  geom_line(aes(y = JACK1, x = Amostras, group = 1), size = 1.2, color = "steelblue") +
  geom_line(aes(y = JACK2, x = Amostras, group = 1), size = 1.2, color = "#871F78") +
  geom_line(aes(y = BOOT, x = Amostras, group = 1), size = 1.2, color = "#A62A2A") + 
  scale_x_continuous(breaks = seq(0, max(resultados_MA$Amostras), by = 20)) +
  labs(x = "Number of samples", y = "Species Richness") +
  theme_bw()

#exporting this Graphic
ggsave("EstimatorCE.png", plot = GraphicCE, width = 6, height = 4, dpi = 300)

#Extrapolation & Interpolation
#preparando dados 
dados_inextCE <- hexCE %>%
  select(-amostra_hex) %>%
  select(where(~ is.numeric(.) && sum(.) != 0))

dados_inextCE <- as.incfreq(t(dados_inextCE))

iNEXTCE <- iNEXT(dados_inextCE, q = 0, datatype = "incidence_freq", 
                 endpoint = 164)
## Gráfico
Graphic2CE <- ggiNEXT(iNEXTCE, type = 1) +
  scale_colour_manual(values = "black") +
  scale_fill_manual(values = "black") +
  labs(x = "Number of samples", y = "Species Richness") +
  theme_bw() +
  theme(legend.position="none")

ggsave("EstimatorCE2.png", plot = Graphic2CE, width = 6, height = 4, dpi = 300)

#Pampa (REMOVIDO - SEM AMOSTRAS SUFICIENTES)
#Estimators
estima <- poolaccum(hexPP, permutations = 100)
resultados_PP <- summary(estima, display = c("S", "chao", "jack1", "jack2", "boot"))
resultados_PP$chao <- setNames(resultados_PP$chao, c("Amostras", "Chao2", "Chao2.1", "Chao2.2"))
resultados_PP <- cbind(resultados_PP$chao[, 1:4], resultados_PP$S[, 2:4], resultados_PP$jack1[, 2:4],
                       resultados_PP$jack2[, 2:4], resultados_PP$boot[,2:4])

#Renomeando as colunas após a combinação
resultados_PP <- as.data.frame(resultados_PP) %>%
  setNames(make.unique(names(.))) %>%  
  rename(Amostras = N,
         Chao2 = Chao,
         C_inferior = "2.5%",
         C_superior = "97.5%",
         Riqueza = S, 
         R_inferior = "2.5%.1",
         R_superior = "97.5%.1",
         JACK1 = "Jackknife 1",
         J1_inferior = "2.5%.2",
         J1_superior = "97.5%.2",
         JACK2 = "Jackknife 2",
         J2_inferior = "2.5%.3",
         J2_superior = "97.5%.3",
         BOOT = Bootstrap,
         B_inferior = "2.5%.4",
         B_superior = "97.5%.4",)

#Graphic
GraphicPP <- ggplot(resultados_PP) +
#  geom_point(aes(y = Chao2, x = Amostras, group = 1), size = 3, color = "darkorange") +
  geom_point(aes(y = Riqueza, x = Amostras, group = 1), size = 1, color = "black") +
#  geom_point(aes(y = JACK1, x = Amostras, group = 1), size = 3, color = "pink") +
#  geom_point(aes(y = JACK2, x = Amostras, group = 1), size = 3, color = "red") + 
#  geom_point(aes(y = BOOT, x = Amostras, group = 1), size = 3, color = "green") + 
#  geom_line(aes(y = Chao2, x = Amostras), color = "darkorange", size = 3) +
  geom_line(aes(y = Riqueza, x = Amostras), color = "black", size = 1) +
#  geom_line(aes(y = JACK1, x = Amostras, group = 1), size = 3, color = "pink") +
#  geom_line(aes(y = JACK2, x = Amostras, group = 1), size = 3, color = "red") +
#  geom_line(aes(y = BOOT, x = Amostras, group = 1), size = 3, color = "green") + 
  scale_x_continuous(breaks = seq(0, max(resultados_PP$Amostras), by = 50)) +
  labs(x = "Number of samples", y = "Species Richness")+
  theme_bw()

#exporting this Graphic
ggsave("EstimatorPP.png", plot = GraphicPP, width = 6, height = 4, dpi = 300)

#Extrapolation & Interpolation
#preparando dados 
#dados_inextPP <- hexPP %>%
#  select(-amostra_hex) %>%
#  select(where(~ is.numeric(.) && sum(.) != 0))

#dados_inextPP <- as.incfreq(t(dados_inextPP))

#iNEXTPP <- iNEXT(dados_inextPP, q = 0, datatype = "incidence_freq", 
#                 endpoint = 41)
## Gráfico
#Graphic2PP <- ggiNEXT(iNEXTPP, type = 1) +
#  scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +
#  scale_colour_manual(values = "darkorange") +
#  scale_fill_manual(values = "darkorange") +
#  labs(x = "Número de amostras", y = " Riqueza de espécies")

#ggsave("EstimatorPP2.png", plot = Graphic2PP, width = 6, height = 4, dpi = 300)

#Caatinga
#Estimators
estima <- poolaccum(hexCA, permutations = 100)
resultados_CA <- summary(estima, display = c("S", "chao", "jack1", "jack2", "boot"))
resultados_CA$chao <- setNames(resultados_CA$chao, c("Amostras", "Chao2", "Chao2.1", "Chao2.2"))
resultados_CA <- cbind(resultados_CA$chao[, 1:4], resultados_CA$S[, 2:4], resultados_CA$jack1[, 2:4],
                       resultados_CA$jack2[, 2:4], resultados_CA$boot[,2:4])

#Renomeando as colunas após a combinação
resultados_CA <- as.data.frame(resultados_CA) %>%
  setNames(make.unique(names(.))) %>%  
  rename(Amostras = N,
         Chao2 = Chao,
         C_inferior = "2.5%",
         C_superior = "97.5%",
         Riqueza = S, 
         R_inferior = "2.5%.1",
         R_superior = "97.5%.1",
         JACK1 = "Jackknife 1",
         J1_inferior = "2.5%.2",
         J1_superior = "97.5%.2",
         JACK2 = "Jackknife 2",
         J2_inferior = "2.5%.3",
         J2_superior = "97.5%.3",
         BOOT = Bootstrap,
         B_inferior = "2.5%.4",
         B_superior = "97.5%.4",)

#Graphic
GraphicCA <- ggplot(resultados_CA) +
  geom_point(aes(y = Riqueza, x = Amostras, group = 1), size = 1, color = "black") +
  geom_point(aes(y = JACK1, x = Amostras, group = 1), size = 1, color = "steelblue") +
  geom_point(aes(y = JACK2, x = Amostras, group = 1), size = 1, color = "#871F78") + 
  geom_point(aes(y = BOOT, x = Amostras, group = 1), size = 1, color = "#A62A2A") + 
  geom_line(aes(y = Riqueza, x = Amostras), size = 1.2, color = "black") +
  geom_line(aes(y = JACK1, x = Amostras, group = 1), size = 1.2, color = "steelblue") +
  geom_line(aes(y = JACK2, x = Amostras, group = 1), size = 1.2, color = "#871F78") +
  geom_line(aes(y = BOOT, x = Amostras, group = 1), size = 1.2, color = "#A62A2A") + 
  scale_x_continuous(breaks = seq(0, max(resultados_MA$Amostras), by = 5)) +
  labs(x = "Number of samples", y = "Species Richness") +
  theme_bw()

#exporting this Graphic
ggsave("EstimatorCA.png", plot = GraphicCA, width = 6, height = 4, dpi = 300)

#Extrapolation & Interpolation
#preparando dados 
dados_inextCA <- hexCA %>%
  select(-amostra_hex) %>%
  select(where(~ is.numeric(.) && sum(.) != 0))

dados_inextCA <- as.incfreq(t(dados_inextCA))

iNEXTCA <- iNEXT(dados_inextCA, q = 0, datatype = "incidence_freq", 
                 endpoint = 36)
## Gráfico
Graphic2CA <- ggiNEXT(iNEXTCA, type = 1) +
  scale_colour_manual(values = "black") +
  scale_fill_manual(values = "black") +
  labs(x = "Number of samples", y = "Species Richness") +
  theme_bw() +
  theme(legend.position="none")

ggsave("EstimatorCA2.png", plot = Graphic2CA, width = 6, height = 4, dpi = 300)