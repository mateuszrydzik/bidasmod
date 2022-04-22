library(dplyr)

obwod <- read.csv("obwod_table.csv")
rejon <- read.csv("rej_table.csv")

whole <- obwod %>% merge(rejon, by = "OBWOD") %>% 
  dplyr::select(OBWOD, REJ.x, POP_REJ, N_ADRESY, N_ADRESY_R, CLC_area, CLC_area_R, UA_area, UA_area_R, BDOT_area, BDOT_area_R)

whole$waga_clc = whole$CLC_area/whole$CLC_area_R
whole$est_pop_clc = whole$waga_clc * whole$POP_REJ
whole$est_pop_clc[is.na(whole$est_pop_clc)] <- 0


