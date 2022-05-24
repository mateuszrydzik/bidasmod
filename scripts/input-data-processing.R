library(sf)
library(tmap)
library(dplyr)

pth_obwody = "initial-data/Obwody_2011.gpkg"
pth_CLC = "initial-data/clc.gpkg"
pth_UA = "initial-data/PL005L2_POZNAN_UA2012_revised_v021.gpkg"
pth_BDOT = "initial-data/BDOT_PTZB.gpkg"
pth_PktAdr = "initial-data/punkty_adresowe.gpkg"

##### load data
obwody <- read_sf(pth_obwody) %>%  #obwody, lower ranked spatial units
  st_transform(., crs = 2180)

rejony <- obwody %>% #grouping to rejony, higher ranked spatial units
  group_by(REJ) %>% 
  summarize() %>% 
  ungroup()

#Corine Land Cover
CLC <- read_sf(pth_CLC) %>% 
  .[.$CODE_12 %in% c("111", "112"), ] %>% 
  st_transform(., crs = 2180)

#Urban Atlas
UA <- read_sf(pth_UA) %>% 
  .[.$code_2012 %in% c("11100", "11210", "11220", "11230", "11240", "11300"), ] %>% 
  st_transform(., crs = 2180)

#Baza Danych Obiektów Topograficznych
BDOT <- read_sf(pth_BDOT)

#Address points
PktAdr <- read_sf(pth_PktAdr) %>% 
  st_transform(., crs = 2180)

##### calculating area of urbanised surfaces
#CLC
CLC_area <- st_intersection(CLC, obwody) %>% 
  mutate(CLC_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, CLC_area) 

CLC_area_O <- aggregate(CLC_area$CLC_area, by = list(CLC_area$OBWOD), FUN = sum) #some units have discontinuous 
colnames(CLC_area_O) <- c("OBWOD", "CLC_area") #urbanised surfaces, aggregation sums them to one value

obwody_CLC <- obwody %>% merge(CLC_area_O, by = "OBWOD", all = T) #adding units that where excluded after intersection
obwody_CLC[is.na(obwody_CLC)] <- 0 #(they had no area)

CLC_area_R <- aggregate(CLC_area$CLC_area, by = list(CLC_area$REJ), FUN = sum) #sum areas to higher spatial units
colnames(CLC_area_R) <- c("REJ", "CLC_area_R")

rejony_CLC <- rejony %>% merge(CLC_area_R, by = "REJ", all = T)
rejony_CLC[is.na(rejony_CLC)] <- 0 

#UA
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

#BDOT
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

#Address points
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

#Population for higher spatial units
rejony_POP <- aggregate(obwody$TOTAL_POP, by = list(obwody$REJ), FUN = sum)
colnames(rejony_POP) <- c("REJ", "POP_REJ")

#### Output table
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

#write.csv(obwody_df, "obwody_df.csv", row.names = F)