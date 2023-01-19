library(dplyr)
library(sf)
library(areal)
library(tmap)
source("functions/decompose.R")

### Modelowanie dazymetryczne

# Dekompozycja populacji z rejonów statystycznych do obwodów przy wykorzystaniu danych pomocniczych
obwody_df$bdot_pop <- decompose(obwody_df$bdot_area, obwody_df$bdot_area_r, obwody_df$TOTAL_POP_R)
obwody_df$clc_pop <- decompose(obwody_df$clc_area, obwody_df$clc_area_r, obwody_df$TOTAL_POP_R)
obwody_df$pktadr_pop <- decompose(obwody_df$pktadr_count, obwody_df$pktadr_count_r, obwody_df$TOTAL_POP_R)
obwody_df$ua_pop <- decompose(obwody_df$ua_area, obwody_df$ua_area_r, obwody_df$TOTAL_POP_R)

decompose_df <- obwody_df %>% dplyr::select(OBWOD, TOTAL_POP, bdot_pop, clc_pop, pktadr_pop, ua_pop)

### Metoda powierzchniowo-wagowa
rejony_with_pop <- rejony %>% merge(rejony_pop, by = "REJ")

awi <- aw_interpolate(obwody,
                     tid = OBWOD,
                     source = rejony_with_pop,
                     sid = REJ,
                     weight = "sum",
                     output = "tibble",
                     extensive = "TOTAL_POP_R")

# Utworzenie tabeli z rezultatami modelowania dazymetrycznego oraz metody powierzchniowo-wagowej
results <- merge(decompose_df, awi[, c("OBWOD","TOTAL_POP_R")], by = "OBWOD")
results <- merge(results, obwody[, c("OBWOD", "REJ")], by = "OBWOD") 
colnames(results) <- c("OBWOD", "POP", "bdot_pop", "clc_pop", "pktadr_pop", "ua_pop", "awi_pop", "REJ", "geom")
