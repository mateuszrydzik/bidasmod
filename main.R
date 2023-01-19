library(dplyr)
library(sf)
library(areal)
library(tmap)
source("functions/read_data.R")
source("functions/decompose.R")

#1. wczytanie danych
pth_obwody = "initial-data/Obwody_2011.gpkg"
pth_CLC = "initial-data/clc.gpkg"
pth_UA = "initial-data/PL005L2_POZNAN_UA2012_revised_v021.gpkg"
pth_BDOT = "initial-data/BDOT_PTZB.gpkg"
pth_PktAdr = "initial-data/punkty_adresowe.gpkg"

obwody <- read_data(pth_obwody, crs = 2180)

rejony <- obwody %>% 
  group_by(REJ) %>% 
  summarize() %>% 
  ungroup()

bdot <- read_data(pth_BDOT)

clc <- read_data(pth_CLC,
                 col_name = "CODE_12",
                 urban_list = c("111", "112"),
                 crs = 2180)

pkt_adr <- read_data(pth_PktAdr, crs=2180)

ua <- read_data(pth_UA,
                 col_name = "code_2012",
                 urban_list = c("11100", "11210", "11220", "11230", "11240", "11300"),
                 crs = 2180)

bdot_area <- st_intersection(bdot, obwody) %>% 
  mutate(bdot_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, bdot_area) %>% st_drop_geometry()

bdot_area_o <- aggregate(bdot_area$bdot_area, by = list(bdot_area$OBWOD), FUN = sum)
colnames(bdot_area_o) <- c("OBWOD", "bdot_area")

bdot_area_r <- aggregate(bdot_area$bdot_area, by = list(bdot_area$REJ), FUN = sum)
colnames(bdot_area_r) <- c("REJ", "bdot_area_r")


clc_area <- st_intersection(clc, obwody) %>% 
  mutate(clc_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, clc_area) %>% 
  st_drop_geometry()

clc_area_o <- aggregate(clc_area$clc_area, by = list(clc_area$OBWOD), FUN = sum) 
colnames(clc_area_o) <- c("OBWOD", "clc_area") 

clc_area_r <- aggregate(clc_area$clc_area, by = list(clc_area$REJ), FUN = sum) 
colnames(clc_area_r) <- c("REJ", "clc_area_r")

pktadr_count <- st_join(pktadr, obwody)
pktadr_count <- as.data.frame(table(pktadr_count$OBWOD))
colnames(pktadr_count) <- c("OBWOD", "pktadr_count")

pktadr_count_r <- pktadr_count %>% merge(obwody, by = "OBWOD") %>% dplyr::select(REJ, pktadr_count)
pktadr_count_r <- aggregate(pktadr_count_r$pktadr_count, by = list(pktadr_count_r$REJ), FUN = sum)
colnames(pktadr_count_r) <- c("REJ", "pktadr_count_r")

ua_area <- st_intersection(ua, obwody) %>% 
  mutate(ua_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, ua_area) %>%
  st_drop_geometry()

ua_area_o <- aggregate(ua_area$ua_area, by = list(ua_area$OBWOD), FUN = sum)
colnames(ua_area_o) <- c("OBWOD", "ua_area")

ua_area_r <- aggregate(ua_area$ua_area, by = list(ua_area$REJ), FUN = sum)
colnames(ua_area_r) <- c("REJ", "ua_area_r")

rejony_pop <- aggregate(obwody$TOTAL_POP, by = list(obwody$REJ), FUN = sum)
colnames(rejony_pop) <- c("REJ", "TOTAL_POP_R")

obwody_df <- st_drop_geometry(obwody) %>% 
  merge(bdot_area_o, by = "OBWOD", all = T) %>% 
  merge(bdot_area_r, by = "REJ", all = T) %>% 
  merge(clc_area_o, by = "OBWOD", all = T) %>% 
  merge(clc_area_r, by = "REJ", all = T) %>% 
  merge(pktadr_count, by = "OBWOD", all = T) %>% 
  merge(pktadr_count_r, by = "REJ", all = T) %>% 
  merge(ua_area_o, by = "OBWOD", all = T) %>% 
  merge(ua_area_r, by = "REJ", all = T) %>% 
  merge(rejony_pop, by = "REJ", all = T) %>% 
  dplyr::select(OBWOD, REJ, bdot_area, clc_area, ua_area, pktadr_count, bdot_area_r, clc_area_r,  ua_area_r, pktadr_count_r, TOTAL_POP, TOTAL_POP_R)

obwody_df[is.na(obwody_df)]  <- 0

#2. Modelowanie dazymetryczne oraz metoda powierzchniowo-wagowa
obwody_df$bdot_pop <- decompose(obwody_df$bdot_area, obwody_df$bdot_area_r, obwody_df$TOTAL_POP_R)
obwody_df$clc_pop <- decompose(obwody_df$clc_area, obwody_df$clc_area_r, obwody_df$TOTAL_POP_R)
obwody_df$pktadr_pop <- decompose(obwody_df$pktadr_count, obwody_df$pktadr_count_r, obwody_df$TOTAL_POP_R)
obwody_df$ua_pop <- decompose(obwody_df$ua_area, obwody_df$ua_area_r, obwody_df$TOTAL_POP_R)

decompose_df <- obwody_df %>% dplyr::select(OBWOD, TOTAL_POP, bdot_pop, clc_pop, pktadr_pop, ua_pop)

rejony_with_pop <- rejony %>% merge(rejony_pop, by = "REJ")

awi <- aw_interpolate(obwody,
                      tid = OBWOD,
                      source = rejony_with_pop,
                      sid = REJ,
                      weight = "sum",
                      output = "tibble",
                      extensive = "TOTAL_POP_R")

results <- merge(decompose_df, awi[, c("OBWOD","TOTAL_POP_R")], by = "OBWOD")
results <- merge(results, obwody[, c("OBWOD", "REJ")], by = "OBWOD") 
colnames(results) <- c("OBWOD", "POP", "bdot_pop", "clc_pop", "pktadr_pop", "ua_pop", "awi_pop", "REJ", "geom")