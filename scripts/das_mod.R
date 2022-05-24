library(dplyr)
library(sf)
library(areal)
source("decompose.R")

obwody_df <- read.csv("data/obwody_df.csv")

obwody_df$CLC_POP <- decompose(obwody_df$CLC_area, obwody_df$CLC_area_R, obwody_df$POP_REJ)
obwody_df$UA_POP <- decompose(obwody_df$UA_area, obwody_df$UA_area_R, obwody_df$POP_REJ)
obwody_df$BDOT_POP <- decompose(obwody_df$BDOT_area, obwody_df$BDOT_area_R, obwody_df$POP_REJ)
obwody_df$PktAdr_POP <- decompose(obwody_df$N_ADRESY, obwody_df$N_ADRESY_R, obwody_df$POP_REJ)

decompose_df <- obwody_df %>% dplyr::select(OBWOD, TOTAL_POP, CLC_POP, UA_POP, BDOT_POP, PktAdr_POP)