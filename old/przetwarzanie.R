library(sf)
library(tmap)
library(dplyr)

##### wczytanie danych
obwody <- read_sf("initial-data/Obwody_2011.gpkg") %>% 
  st_transform(., crs = 2180)

rejony <- obwody %>% 
  group_by(REJ) %>% 
  summarize() %>% 
  ungroup()

#CLC
CLC <- read_sf("initial-data/clc.gpkg") %>% 
  .[.$CODE_12 %in% c("111", "112"), ] %>% 
  st_transform(., crs = 2180)

#Urban Atlas
UA <- read_sf("initial-data/PL005L2_POZNAN_UA2012_revised_v021.gpkg") %>% 
  .[.$code_2012 %in% c("11100", "11210", "11220", "11230", "11240", "11300"), ] %>% 
  st_transform(., crs = 2180)

#BDOT
BDOT <- read_sf("initial-data/BDOT_PTZB.gpkg")

#Punkty adresowe
PktAdr <- read_sf("initial-data/punkty_adresowe.gpkg") %>% 
  st_transform(., crs = 2180)

##### wyznaczanie powierzchni obszarow zurbanizowanych
#CLC - obwody
CLC_area <- st_intersection(CLC, obwody) %>% 
  mutate(CLC_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, CLC_area) 

CLC_area_O <- aggregate(CLC_area$CLC_area, by = list(CLC_area$OBWOD), FUN = sum)
colnames(CLC_area_O) <- c("OBWOD", "CLC_area")

obwody_CLC <- obwody %>% merge(CLC_area_O, by = "OBWOD", all = T)
obwody_CLC[is.na(obwody_CLC)] <- 0 

CLC_area_R <- aggregate(CLC_area$CLC_area, by = list(CLC_area$REJ), FUN = sum)
colnames(CLC_area_R) <- c("REJ", "CLC_area_R")

rejony_CLC <- rejony %>% merge(CLC_area_R, by = "REJ", all = T)
rejony_CLC[is.na(rejony_CLC)] <- 0 

#Urban Atlas - obwody
UA_area <- st_intersection(UA, obwody) %>% 
  mutate(UA_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, UA_area) %>% st_drop_geometry()

UA_area_O <- aggregate(UA_area$UA_area, by = list(UA_area$OBWOD), FUN = sum)
colnames(UA_area_O) <- c("OBWOD", "UA_area")

obwody_UA <- obwody %>% merge(UA_area_O, by = "OBWOD", all = T)
obwody_UA[is.na(obwody_UA)] <- 0 

UA_area_R <- aggregate(UA_area$UA_area, by = list(UA_area$REJ), FUN = sum)
colnames(UA_area_R) <- c("REJ", "UA_area_R")

rejony_UA <- rejony %>% merge(UA_area_R, by = "REJ", all = T)
rejony_UA[is.na(rejony_UA)] <- 0 

#BDOT - obwody
BDOT_area <- st_intersection(BDOT, obwody) %>% 
  mutate(BDOT_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, BDOT_area) %>% st_drop_geometry()

BDOT_area_O <- aggregate(BDOT_area$BDOT_area, by = list(BDOT_area$OBWOD), FUN = sum)
colnames(BDOT_area_O) <- c("OBWOD", "BDOT_area")

obwody_BDOT <- obwody %>% merge(BDOT_area_O, by = "OBWOD", all = T)
obwody_BDOT[is.na(obwody_BDOT)] <- 0 

BDOT_area_R <- aggregate(BDOT_area$BDOT_area, by = list(BDOT_area$REJ), FUN = sum)
colnames(BDOT_area_R) <- c("REJ", "BDOT_area_R")

rejony_BDOT <- rejony %>% merge(BDOT_area_R, by = "REJ", all = T)
rejony_BDOT[is.na(rejony_BDOT)] <- 0 

#Punkty adresowe
PktAdr_count <- st_join(PktAdr, obwody)
PktAdr_count <- as.data.frame(table(PktAdr_count$OBWOD))
colnames(PktAdr_count) <- c("OBWOD", "N_ADRESY")

obwody_PktAdr <- obwody %>% merge(PktAdr_count, by = "OBWOD", all = T)
obwody_PktAdr[is.na(obwody_PktAdr)] <- 0 

PktAdr_count_R <- PktAdr_count %>% merge(obwody, by = "OBWOD") %>% dplyr::select(REJ, N_ADRESY)
PktAdr_count_R <- aggregate(PktAdr_count_R$N_ADRESY, by = list(PktAdr_count_R$REJ), FUN = sum)
colnames(PktAdr_count_R) <- c("REJ", "N_ADRESY_R")

rejony_PktAdr <- rejony %>% merge(PktAdr_count_R, by = "REJ", all = T)
rejony_PktAdr[is.na(rejony_PktAdr)] <- 0 

#Populacja dla rejonow
rejony_POP <- aggregate(obwody$TOTAL_POP, by = list(obwody$REJ), FUN = sum)
colnames(rejony_POP) <- c("REJ", "POP_REJ")

#TABELA
obwody_df <- st_drop_geometry(obwody) %>% 
  merge(obwody_CLC, by = "OBWOD") %>% 
  merge(obwody_UA, by = "OBWOD") %>% 
  merge(obwody_BDOT, by = "OBWOD") %>% 
  merge(obwody_PktAdr, by = "OBWOD") %>% 
  merge(rejony_CLC, by = "REJ") %>% 
  merge(rejony_UA, by = "REJ") %>% 
  merge(rejony_BDOT, by = "REJ") %>% 
  merge(rejony_PktAdr, by = "REJ") %>% 
  merge(rejony_POP, by = "REJ") %>%
  dplyr::select(OBWOD, REJ, N_ADRESY, CLC_area, UA_area, BDOT_area, TOTAL_POP, N_ADRESY_R, CLC_area_R, UA_area_R, BDOT_area_R, POP_REJ)

write.csv(obwody_df, "obwody_df.csv", row.names = F)

#################################################################################

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


