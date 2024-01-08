#Correcting writing errors
#Marina Morim Gomes
#marinagomesdiptera@mn.ufrj.br
#2022

#loading packages -------------------------------------
library(tidyverse)

#loading data -------------------------------------
original <- read_csv("./Data/Processados/1_planilhaunida.csv")

#cleaning  -------------------------------------------
#removing observations with especimens not identified at species level
filtrada <- original %>%
  filter (especie != "NA") %>%
  filter (genero != "NA") %>%
  filter (especie != "sp. nov.")

#removing observations without any locality information
filtrada <- filtrada %>%
  filter (qualificador != "pessimo")

#correcting names spelled differently ----------------
# 1- checking existing names and codes
unique(filtrada$pais)
unique(filtrada$estado_ou_provincia)

#rewriting the names and codes
filtrada <- filtrada %>%
  mutate(pais = recode(pais,
                         "BR" = "Brasil",
                         "Paulau" = "Palau",
                         "US" = "EUA",
                         "Perú" = "Peru",
                         "Eua" = "EUA",
                         "IL" = "Israel",
                         "BR" = "Brasil",
                         "EG" = "Egito",
                         "CA" = "Canadá",
                         "CR" = "Costa Rica",
                         "PK" = "Paquistão",
                         "DE" = "Alemanha",
                         "FI" = "Finlândia",
                         "PF" = "Polinésia Francesa",
                         "BG" = "Búlgaria",
                         "SA" = "Arábia Saudita",
                         "EC" = "Equador",
                         "ZA" = "África do Sul",
                         "AU" = "Austrália",
                         "CN" = "China",
                         "KE" = "Quênia",
                         "IR" = "Irã",
                         "DE" = "Alemanha",
                         "TT" = "Trinidade e Tobago",
                         "BY" = "Bielorrúsia",
                         "CL" = "Chile",
                         "NL" = "Países Baixos",
                         "BS" = "Bahamas",
                         "RU" = "Rússia",
                         "NO" = "Noruega",
                         "TN" = "Tunísia",
                         "FR" = "França",
                         "CO" = "Colômbia",
                         "DK" = "Dinamarca",
                         "PA" = "Panamá",
                         "NZ" = "Nova Zelândia",
                         "AR" = "Argentina",
                         "MX" = "México",
                         "UA" = "Ucrânia"))

filtrada <- filtrada %>%
  mutate(estado_ou_provincia = recode(estado_ou_provincia,
                       "Pará" = "PA",
                       "Amazonas" = "AM",
                       "Acre" = "AC",
                       "Maranhão" = "MA",
                       "Amapá" = "AP",
                       "Mato Grosso" = "MT",
                       "Rondônia" = "RO",
                       "Paraíba" = "PB",
                       "Mato Grosso do Sul" = "MS",
                       "Bahia" = "BA",
                       "Rio de Janeiro" = "RJ",
                       "Brasil" = "NA",
                       "Rio Grande do Sul" = "RS",
                       "Paraná" = "PR",
                       "São Paulo" = "SP",
                       "Distrito Federal" = "DF",
                       "Pernambuco" = "PE",
                       "Minas Gerais" = "MG",
                       "Goiás" = "GO",
                       "Unknown" = "NA",
                       "Santa Catarina" = "SC",
                       "Espírito Santo" = "ES",
                       "Nova Teutonia" = "SC",
                       "Rj" = "RJ",
                       "EUA" = "AC"))

filtrada <- filtrada %>%
  mutate(projeto = recode(projeto,
                          "Dipterofauna do PN Itatiaia" = "Dipterofauna do PNI Itatiaia"))
                                      
filtrada <- filtrada %>%
  mutate(localidade = recode(localidade,
                                      "Fazenda Agua Limpa" = "Fazenda Água Limpa",
                                      "Gen. Sampaio" = "General Sampaio",
                                      "Gal. Sampaio" = "General Sampaio",
                                      "Humaita" = "Humaitá",
                                      "Gal. Sampaio" = "General Sampaio",
                                      "Timbaúva" = "Timbaúvas",
                                      "Buritis (Ribeirão Confins" = "Ribeirão Confins",
                                      "Fazenda do Glória" = "Fazenda Experimental do Glória",
                                      "Sierra do Cipó" = "Serra do Cipó",
                                      "Calado, Rio Doce" = "Rio Doce",
                                      "Rio Doce, Calado" = "Rio Doce",
                                      "Faz. Dr. José Mendes" = "Fazenda Dr. José Mendes",
                                      "BEP, Passo da Lontra" = "BEP Paço do Lontra",
                                      "BEP, Paço do Lontra - Paratudal" = "BEP Paço do Lontra",
                                      "Gorotire Xingu" = "Gorotire, Xingú",
                                      "Gorotire" = "Gorotire, Xingú",
                                      "Guama" = "Guamá",
                                      "Fazenda Taperinha, prox. Santarém" = "Fazenda Taperinha",
                                      "Fazenda Taperinha prox. Santarém" = "Fazenda Taperinha",
                                      "Instituto Agronomico do Norte" = "Instituto Agronômico do Norte",
                                      "Instituto Agr. Do Norte" = "Instituto Agronômico do Norte",
                                      "Campo da Palha" = "Campo do Palha",
                                      "Tracuateu" = "Tracuateua",
                                      "Ananinoeua" = "Ananindeua",
                                      "Teperinha" = "Taperinha",
                                      "IAN" = "I.A.N",
                                      "Pernambuco; Recife" = "Recife",
                                      "Iguassu" = "Iguaçu",
                                      "Iguaçú" = "Iguaçu",
                                      "Grajau" = "Grajaú",
                                      "Gávea, Jardim Botânico" = "Jardim Botânico",
                                      "Jardim Botânico, Gávea" = "Jardim Botânico",
                                      "Restinga da Marambaia" = "Restinga de Marambaia",
                                      "Marambaia" = "Restinga de Marambaia",
                                      "Ilha Marambaia" = "Restinga de Marambaia",
                                      "Muri" = "Mury",
                                      "Xerem" = "Xerém",
                                      "Jacarepagua" = "Jacarepaguá",
                                      "Reserva Biológica da União, trilha buracão" = "Reserva Biológica União, trilha buracão",
                                      "Parque Nacional do Itatiaia, Trilha Rui Braga" = "Parque Nacional do Itatiaia, Trilha Ruy Braga",
                                      "Parque Nacional do Itatiaia, Casa do Pesquisador" = "Parque Nacional do Itatiaia, casa do Pesquisador",
                                      "Parque Nacional do Itatiaia, Estrada da casa do pesquisador" = "Parque Nacional de Itatiaia, estrada casa do pesquisador",
                                      "Floresta da Tijuca" = "Parque Nacional da Tijuca",
                                      "Ilha do governador" = "Ilha do Governador",
                                      "Parque Nacional" = "Parque Nacional da Serra dos Órgãos",
                                      "Mage, Brazil" = "Magé",
                                      "Brasil; guanabara; Rio de Janeiro" = "Rio de Janeiro",
                                      "Rio de Janeiro, Brazil" = "Rio de Janeiro",
                                      "Rio de Janeiro; Dist. Federal" = "Rio de Janeiro",
                                      "Japuiba" = "Japuhyba",
                                      "Japuíba" = "Japuhyba",
                                      "Boracea" = "Estação Biológica da Boracéia",
                                      "Est. Biol. Boracéia" = "Estação Biológica da Boracéia",
                                      "Boracéa" = "Estação Biológica da Boracéia",
                                      "Boracéia" = "Estação Biológica da Boracéia",
                                      "Est.Biol.Boraceia" = "Estação Biológica da Boracéia",
                                      "Tinguá, REBIO Tinguá" = "Reserva Biológica do Tinguá",
                                      "Butantan" = "Butantã",
                                      "Butantan, Horto O. Cruz" = "Butantã, Horto O. Cruz",
                                      "Butantan, Horto O. Crus" = "Butantã, Horto O. Cruz",
                                      "Butantan, Horto Oswaldo Cruz" = "Butantã, Horto O. Cruz",
                                      "Butantan, Horto" = "Butantã, Horto O. Cruz",
                                      "[Not Stated]" = "NA",
                                      "Porto Cabral, Rio Paraná" = "Rio Paraná, Porto Cabral",
                                      "Rio Paraná, Porto cabral" = "Rio Paraná, Porto Cabral",
                                      "Galeão, Ilha do Governador" = "Ilha do Governador, Galeão",
                                      "Gorotire Xingu" = "Gorotire, Xingú",
                                      "Gorotire" = "Gorotire, Xingú",
                                      "Gorotire, S1W85" = "Gorotire, Xingú",
                                      "Gorotire - 51W 8S" = "Gorotire, Xingú",
                                      "Ilha de Maracá, Rio Uraricoera" = "Rio Uraricoera, Ilha de Maracá",
                                      "Atual represa três irmãos, Lussanvira" = "Lussanvira (atual Represa Três Irmãos)",
                                      "Lussanvira" = "Lussanvira (atual Represa Três Irmãos)",
                                      "Rio Cuminá-Cachoeira Tronco" = "Rio Cuminá, Cachoeira do Tronco",
                                      "Calado, Rio Doce" = "Rio Doce, Calado",
                                      "Repreza Rio Grande" = "Represa do Rio Grande",
                                      "Eugene Lefreve" = "Eugenio Lefreve",
                                      "Eug. Lefevre" = "Eugenio Lefreve",
                                      "Eug. Lefreve" = "Eugenio Lefreve",
                                      "Estação Eugênio Lefevre" = "Eugenio Lefreve",
                                      "Eugênio Lefevre" = "Eugenio Lefreve",
                                      "Cantareira/Horto Florestal" = "Cantareira, Horto Florestal",
                                      "Barueir" = "Barueri",
                                      "Repreza Camorim" = "Represa do Camorim",
                                      "Represa Camorim" = "Represa do Camorim",
                                      "Mendanha, Campo Grande" = "Campo Grande, Mendanha",
                                      "Cachoeira do Tronco, Rio Cuminá" = "Rio Cuminá, Cachoeira do Tronco",
                                      "Rio Ouminá, Cachoeira do Tronco" = "Rio Cuminá, Cachoeira do Tronco",
                                      "Campus de Pesquisa do MPEG" = "Campus de Pesquisa do MPEG",
                                      "Serra Norte, N-1 canga" = "Serra Norte, N1- Canga",
                                      "Horto de Cantareira" = "Cantareira, Horto Florestal",
                                      "Estação Ecológico de Tapacurá" = "Estação Ecológica de Tapacurá",
                                      "Eugenio Lefevre" = "Eugenio Lefreve", 
                                      "Ilha de Maraca, Rio Uraricoera" = "Rio Uraricoera, Ilha de Maracá",
                                      "Ilha dos Busios" = "Ilha dos Buzios",
                                      "Instituto Ows. Cruz" = "Instituto Oswaldo Cruz",
                                      "Itaquaquecetuba; Sao Paulo" = "Itaquaquecetuba",
                                      "Itaquaquecetuba; Sao Paulo; Brazil" = "Itaquaquecetuba",
                                      "Jussara" = "Jussaral",
                                      "Parque Utinga" = "Parque Estadual do Utinga",
                                      "Parque Nacional do Itatiaia, casa do Pesquisador" = "Parque Nacional do Itatiaia, casa do pesquisador",
                                      "Represa do Camorim" = "Represa Camorim",
                                      "Repreza Camorim" = "Represa Camorim",
                                      "REBIO União, Trilha Buracão" = "Reserva Biológica União, trilha buracão",
                                      "REBIO União, Trilha do buracão" = "Reserva Biológica União, trilha buracão",
                                      "REBIO União, Trilha três pontes" = "Reserva Biológica União, trilha três pontes",
                                      "Rebio União, trilha três pontes" = "Reserva Biológica União, trilha três pontes",
                                      "Rebio União, trilha três pontes, linha férra" = "Reserva Biológica União, trilha três pontes, linha férra",
                                      "Rebio União, trilha buracão" = "Reserva Biológica União, trilha buracão",
                                      "Rebio União, trilha interpretativa" = "Reserva Biológica União, trilha interpretativa",
                                      "Boca do Cuminá-Miri" = "Boca do Cuminá-Mirim",
                                      "R.Cuminá-miri" = "Rio Cuminá-Mirin",
                                      "Rio Cuminá-mirin" = "Rio Cuminá-Mirin",
                                      "Boca do Cuminá-miri" = "Boca do Cuminá-Mirim",
                                      "R. Cuminá-miri" = "R. Cuminá-miri",
                                      "Carvoeiro, Rio Negro - Rio Branco" = "Rio Negro - Rio Branco, Carvoeiro",
                                      "Urumajo" = "Urumajó",
                                      "Ilha de Maraca, Rio Uraricoera" = "Rio Uraricoera, Ilha de Maracá",
                                      "Serra das Andorinhas" = "Serra da Andorinha",
                                      "Serra Norte, Manganês" = "Serra Norte, Estação Manganês",
                                      "Serra Norte, N-1 canga" = "Serra Norte, N1- Canga"))

#Correcting misspelled species and genus names whit new columns 
filtrada <- filtrada %>%
  rename (genero_original = genero) %>%
  rename (especie_original = especie) %>%
  mutate (genero = genero_original,
          especie = especie_original)

filtrada <- filtrada %>%
  mutate(especie = recode(especie,
                             "(sarcophagula) occidua" = "occidua",
                             "(pattonella) resona" = "resona",
                             "(Sarcophagula) accidua" = "occidua",
                             "(Sarcophagula) femoralis" = "femoralis",
                             "plinthropyga" = "plinthopyga",
                             "duodecimpuctata" = "duodecimpunctata",
                             "carvolhoi" = "carvalhoi",
                             "aurencens" = "aurescens",
                             "borgmeieri" = "borgmeiri",
                             "minerensis" = "mineirensis"))%>%
  mutate(genero = recode(genero,
                          "Sarcophaga (Liopygia)" = "Sarcophaga",
                          "Blaexosipha" = "Blaesoxipha",
                          "Endenimyia" = "Emdenimyia",
                          "Trichaeae" = "Tricharaea"))

#removing observations of species that don't occur/don't have any record in BR -------
#in the dataframe

   #1 - creating a column with the full name of species (genus+species)
filtrada <- filtrada %>% 
  unite(genero_especie, genero, especie, remove = FALSE)

   #2 - Making a list of species occurring outside BR
dadossemBR <- filtrada %>%
  filter(pais != "Brasil")

listaespeciessemBR <- dadossemBR %>% 
  select(genero_especie) %>%
  distinct()

   #3 - Making a list of species occurring in BR
dadosBR <- filtrada %>%
  filter(is.na(pais) | pais == "Brasil")

listaespeciesBR <- dadosBR %>%
  select (genero_especie) %>%
  distinct()
#In this stage, it is possible to observe the number of species in my dataframe

  #3 - Join the two specieslist to find out what's outside the BR which also has 
#in BR
especiescomBR <- semi_join(listaespeciessemBR, listaespeciesBR, by = "genero_especie")

   #4 - filtering the dataframe "dadossemBR" with this new list and uniting this result
#with the dataset "dadosBR" to return to original dataframe
dadossemBR <- semi_join(dadossemBR, especiescomBR, by = "genero_especie")

filtrada <- union(dadosBR, dadossemBR)

#exporting final data frame --------------------------
write_csv(filtrada, "./Data/Processados/2_planilhafiltrada.csv")