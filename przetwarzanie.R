library(sf)
library(tmap)
library(dplyr)

##### wczytanie danych
setwd("D:/Studia/rok3/pracaInzynierska")
obwody <- read_sf("przetworzone/Obwody_2011.gpkg")
obwody <- st_transform(obwody, crs = 2180)
#obwody <- mutate(obwody, obwody_area = st_area(obwody))

#CLC
clc <- read_sf("przetworzone/clc.gpkg")
clc_urbanised <- clc[clc$CODE_12 %in% c("111", "112"), ]
clc_urbanised <- st_transform(clc_urbanised, crs = 2180)

#Urban Atlas
urbanatlas <- read_sf("przetworzone/PL005L2_POZNAN_UA2012_revised_v021.gpkg")
urbanatlas_urbanised <- urbanatlas[urbanatlas$code_2012 %in% c("11100", "11210", "11220", "11230", "11240", "11300"), ]
urbanatlas_urbanised <- st_transform(urbanatlas_urbanised, crs = 2180)

#BDOT
bdot <- read_sf("przetworzone/BDOT_PTZB.gpkg")

#Punkty adresowe
punktyAdresowe <- read_sf("przetworzone/punkty_adresowe.gpkg")
punktyAdresowe <- st_transform(punktyAdresowe, crs = 2180)


##### wyznaczanie powierzchni obszarow zurbanizowanych
#CLC - obwody
CLC_area <- st_intersection(clc_urbanised, obwody) %>% 
  mutate(CLC_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, CLC_area) %>% st_drop_geometry()

CLC_area_agg <- aggregate(CLC_area$CLC_area, by = list(CLC_area$OBWOD), FUN = sum)
colnames(CLC_area_agg) <- c("OBWOD", "CLC_area")

CLC_area_R <- aggregate(CLC_area$CLC_area, by = list(CLC_area$REJ), FUN = sum)
colnames(CLC_area_R) <- c("REJ", "CLC_area_R")


#Urban Atlas - obwody
UA_area <- st_intersection(urbanatlas_urbanised, obwody) %>% 
  mutate(UA_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, UA_area) %>% st_drop_geometry()

UA_area_agg <- aggregate(UA_area$UA_area, by = list(UA_area$OBWOD), FUN = sum)
colnames(UA_area_agg) <- c("OBWOD", "UA_area")

UA_area_R <- aggregate(UA_area$UA_area, by = list(UA_area$REJ), FUN = sum)
colnames(UA_area_R) <- c("REJ", "UA_area_R")

#BDOT - obwody
BDOT_area <- st_intersection(bdot, obwody) %>% 
  mutate(BDOT_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, BDOT_area) %>% st_drop_geometry()

BDOT_area_agg <- aggregate(BDOT_area$BDOT_area, by = list(BDOT_area$OBWOD), FUN = sum)
colnames(BDOT_area_agg) <- c("OBWOD", "BDOT_area")

BDOT_area_R <- aggregate(BDOT_area$BDOT_area, by = list(BDOT_area$REJ), FUN = sum)
colnames(BDOT_area_R) <- c("REJ", "BDOT_area_R")


#Punkty adresowe
punkt_obwod <- st_join(punktyAdresowe, obwody)
count_punkty <- as.data.frame(table(punkt_obwod$OBWOD))
colnames(count_punkty) <- c("OBWOD", "N_ADRESY")

count_punkty_R <- count_punkty %>% merge(obwody, by = "OBWOD") %>% dplyr::select(OBWOD, REJ, N_ADRESY)
count_punkty_R <- aggregate(count_punkty_R$N_ADRESY, by = list(count_punkty_R$REJ), FUN = sum)
colnames(count_punkty_R) <- c("REJ", "N_ADRESY_R")

#Populacja dla rejonow
pop_rej <- aggregate(obwody$TOTAL_POP, by = list(obwody$REJ), FUN = sum)
colnames(pop_rej) <- c("REJ", "POP_REJ")


#TABELA
obwod_table <- obwody %>% 
  merge(CLC_area_agg, by = "OBWOD") %>% 
  merge(UA_area_agg, by = "OBWOD") %>% 
  merge(BDOT_area_agg, by = "OBWOD") %>% 
  merge(count_punkty, by = "OBWOD") %>% 
  dplyr::select(OBWOD, REJ, N_ADRESY, CLC_area, UA_area, BDOT_area) %>% st_drop_geometry()

write.csv(obwod_table, "obwod_table.csv", row.names = F)


rej_table_R <- CLC_area_R %>% 
  merge(UA_area_R, by = "REJ") %>% 
  merge(BDOT_area_R, by = "REJ") %>% 
  merge(obwody, by = "REJ") %>% 
  merge(pop_rej, by = "REJ") %>%
  merge(count_punkty_R, by = "REJ") %>% 
  dplyr::select(OBWOD, REJ, POP_REJ, N_ADRESY_R, CLC_area_R, UA_area_R, BDOT_area_R)

write.csv(rej_table_R, "rej_table.csv", row.names = F)


rej_table <- CLC_area %>%
  merge(UA_area, by = "OBWOD") %>% 
  merge(BDOT_area, by = "OBWOD") %>%
  merge(count_punkty, by = "OBWOD") %>%
  merge(pop_rej, by = "REJ") %>% 
  dplyr::select(OBWOD, REJ, POP_REJ, CLC_area, UA_area, BDOT_area, N_ADRESY)







rej_table_whole <- rej_table_R %>% 
  merge(UA_area[, "UA_area"], by = "OBWOD") %>% 
  merge(CLC_area, by = "OBWOD") %>% 
  merge(BDOT_area, by = "OBWOD") %>% 
  merge(count_punkty, by = "OBWOD") %>% 
dplyr::select(OBWOD, REJ, POP_REJ, N_ADRESY, N_ADRESY_R, CLC_area, CLC_area_R, UA_area, UA_area_R, BDOT_area, BDOT_area_R)


rej_table_whole <- rej_table_R %>% left_join(
          CLC_area %>% dplyr::select(OBWOD, CLC_area),
          by = "OBWOD") %>% 
  left_join(UA_area %>% dplyr::select(OBWOD, UA_area),
            by = "OBWOD") %>% 
  left_join(BDOT_area %>% dplyr::select(OBWOD, BDOT_area),
            by = "OBWOD")


#BDOT_area_agg <- aggregate(BDOT_area[, 2], by = list(BDOT_area$OBWOD), FUN = sum)

# clc_percentage <- st_intersection(clc_urbanised, obwody) %>% 
#   mutate(CLC_area = st_area(.)) %>% 
#   mutate(CLC_cover = round(as.numeric((CLC_area/obwody_area) * 100)), 2) %>% 
#   dplyr::select(OBWOD, CLC_cover) %>% st_drop_geometry()




#NIE PROCENT POWIERZHCNI ZURBANIZOWANEJ W OBWODZIE TYLKO RZECZYWISTO POWIERZCHNIE 
#TO ZAGREGUJEMY DO REJONOW


#DRUGA TABELA to procent udzialu kazdego obwodu dla calego rejonu
#rozlozyc ludzi z rejonoow do obwodow

#liczba ludzi w obwodzie mniejszym
#liczba komorek danej klasy zurbanizowanej

#jaki procent powierzchni
#sumaryczna powierzchnia obszarow zurbanizowanych dla rejonow
#w obwodzie A stanowi procent powierzchni z rejonu
#mamy sto osob, rejon podzielony na dwa obwody
#w jednym obwodzie sa 2/3 populacji rejonu

#NA ZA TYDZIEN
#rozpisz harmonogram, jaki krok do kiedy zrobisz co po kolei z datami 
#Kroki poszczegolne jako tytuly rozdzialow


