library(dplyr)
library(sf)
source("decompose.R")
obwod <- read.csv("obwod_table.csv") #processed
rejon <- read.csv("rej_table.csv") #processed
obwody <- read_sf("Obwody_2011.gpkg") #initial

obwody_pop <- obwody %>% merge(obwod, by = "OBWOD") #get initial pop for obwody

whole <- obwod %>% merge(rejon, by = "OBWOD") %>% 
  dplyr::select(OBWOD, REJ.x, POP_REJ, N_ADRESY, N_ADRESY_R, CLC_area, CLC_area_R, UA_area, UA_area_R, BDOT_area, BDOT_area_R)

#whole$waga_clc = whole$CLC_area/whole$CLC_area_R
#whole$est_pop_clc = whole$waga_clc * whole$POP_REJ
#whole$est_pop_clc[is.na(whole$est_pop_clc)] <- 0


obwody_pop$CLC_POP <- decompose(whole$CLC_area, whole$CLC_area_R, whole$POP_REJ)
obwody_pop$UA_POP <- decompose(whole$UA_area, whole$UA_area_R, whole$POP_REJ)
obwody_pop$BDOT_POP <- decompose(whole$BDOT_area, whole$BDOT_area_R, whole$POP_REJ)
obwody_pop$PktAdr_POP <- decompose(whole$N_ADRESY, whole$N_ADRESY_R, whole$POP_REJ)

compare <- obwody_pop %>% dplyr::select(OBWOD, TOTAL_POP, CLC_POP, UA_POP, BDOT_POP, PktAdr_POP) %>% st_drop_geometry()

                                        