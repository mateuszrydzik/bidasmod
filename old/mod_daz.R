library(dplyr)
library(sf)
library(areal)
source("decompose.R")

obwody_df <- read.csv("obwody_df.csv")

obwody_df$CLC_POP <- decompose(obwody_df$CLC_area, obwody_df$CLC_area_R, obwody_df$POP_REJ)
obwody_df$UA_POP <- decompose(obwody_df$UA_area, obwody_df$UA_area_R, obwody_df$POP_REJ)
obwody_df$BDOT_POP <- decompose(obwody_df$BDOT_area, obwody_df$BDOT_area_R, obwody_df$POP_REJ)
obwody_df$PktAdr_POP <- decompose(obwody_df$N_ADRESY, obwody_df$N_ADRESY_R, obwody_df$POP_REJ)

decompose_df <- obwody_df %>% dplyr::select(OBWOD, TOTAL_POP, CLC_POP, UA_POP, BDOT_POP, PktAdr_POP)

length(decompose_df[decompose_df$TOTAL_POP == 0 & decompose_df$CLC_POP == 0,]$CLC_POP)
length(decompose_df[decompose_df$TOTAL_POP == 0,]$CLC_POP)

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

compare <- obwody_pop %>% dplyr::select(OBWOD, TOTAL_POP, CLC_POP, UA_POP, BDOT_POP, PktAdr_POP)
library(tmap)

tmap_mode("view")
tm_shape(rejon) +
  tm_polygons()
tm_shape(compare) +
  tm_polygons(col = "CLC_POP")

p1 = tm_shape(obwody) + tm_polygons()
p2 = tm_shape(obwody_clcp) + tm_polygons(col = "CLC_area")
p3 = tm_shape(compare) + tm_polygons(col = "BDOT_POP")
p4 = tm_shape(compare) + tm_polygons(col = "PktAdr_POP")
tm_shape(CLC_area_agg2_sf) + tm_polygons(col = "CLC_area") + tm_shape(clc_urbanised) + tm_polygons()
tmap_arrange(p1, p2)

tm_shape(obwody) +
  tm_polygons(col = "TOTAL_POP")

###########NEW

