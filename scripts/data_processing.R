library(sf)
library(dplyr)
source("functions/read_data.R")

# Scieżki do danych
pth_obwody = "initial-data/Obwody_2011.gpkg"
pth_bdot = "initial-data/BDOT_PTZB.gpkg"
pth_clc = "initial-data/clc.gpkg"
pth_pktadr = "initial-data/punkty_adresowe.gpkg"
pth_ua = "initial-data/PL005L2_POZNAN_UA2012_revised_v021.gpkg"

##### Wczytanie danych
# Obwody spisowe i rejony statystyczne
obwody <- read_data(pth_obwody, crs = 2180) #wczytanie obwodów spisowych

rejony <- obwody %>% #Grupowanie obwodów do rejonow statystycznych
  group_by(REJ) %>% 
  summarize() %>% 
  ungroup()

# Baza Danych Obiektow Topograficznych
bdot <- read_data(pth_bdot)

# Corine Land Cover
clc <- read_data(pth_clc, col_name = "CODE_12", urban_list = c("111", "112"), crs = 2180)

# Punkty adresowe
pktadr <- read_data(pth_pktadr, crs=2180)

# Urban Atlas
ua <- read_data(pth_ua,
                 col_name = "code_2012",
                 urban_list = c("11100", "11210", "11220", "11230", "11240", "11300"),
                 crs = 2180)

#### Obliczenie powierzchni zurbanizowanej w jednostkach spisowych
# BDOT
bdot_area <- st_intersection(bdot, obwody) %>% 
  mutate(bdot_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, bdot_area) %>% st_drop_geometry()

# W niektorych obwodach dane pomocnicze występują w formie multipoligonu, co 
# prowadzi do wystepowania kilku rekordów tego samego obwodu. 
# Ta operacja sumuje powierzchnie do jednego rekordu obwodu.
bdot_area_o <- aggregate(bdot_area$bdot_area, by = list(bdot_area$OBWOD), FUN = sum)
colnames(bdot_area_o) <- c("OBWOD", "bdot_area")

#Agregacja powierzchni zurbanizowanej do rejonow spisowych.
bdot_area_r <- aggregate(bdot_area$bdot_area, by = list(bdot_area$REJ), FUN = sum)
colnames(bdot_area_r) <- c("REJ", "bdot_area_r")

# CLC
clc_area <- st_intersection(clc, obwody) %>% 
  mutate(clc_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, clc_area) %>% 
  st_drop_geometry()

clc_area_o <- aggregate(clc_area$clc_area, by = list(clc_area$OBWOD), FUN = sum) 
colnames(clc_area_o) <- c("OBWOD", "clc_area") 

clc_area_r <- aggregate(clc_area$clc_area, by = list(clc_area$REJ), FUN = sum) 
colnames(clc_area_r) <- c("REJ", "clc_area_r")

# Punkty adresowe
pktadr_count <- st_join(pktadr, obwody)
pktadr_count <- as.data.frame(table(pktadr_count$OBWOD))
colnames(pktadr_count) <- c("OBWOD", "pktadr_count")

pktadr_count_r <- pktadr_count %>% merge(obwody, by = "OBWOD") %>% dplyr::select(REJ, pktadr_count)
pktadr_count_r <- aggregate(pktadr_count_r$pktadr_count, by = list(pktadr_count_r$REJ), FUN = sum)
colnames(pktadr_count_r) <- c("REJ", "pktadr_count_r")

# UA
ua_area <- st_intersection(ua, obwody) %>% 
  mutate(ua_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, ua_area) %>%
  st_drop_geometry()

ua_area_o <- aggregate(ua_area$ua_area, by = list(ua_area$OBWOD), FUN = sum)
colnames(ua_area_o) <- c("OBWOD", "ua_area")

ua_area_r <- aggregate(ua_area$ua_area, by = list(ua_area$REJ), FUN = sum)
colnames(ua_area_r) <- c("REJ", "ua_area_r")

# Agregacja populacji z obwodow do rejonow statystycznych
rejony_pop <- aggregate(obwody$TOTAL_POP, by = list(obwody$REJ), FUN = sum)
colnames(rejony_pop) <- c("REJ", "TOTAL_POP_R")

# Wygenerowanie tabeli zawierającej powierzchnie danych pomocniczych
# dla obwodow oraz rejonow
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

# Obwody, ktore nie pokrywaly sie z danymi pomocniczymi, nie zostaly zaliczone do wynikowej tabeli.
# Ta operacja dolacza pozostałe obwody i przypisuje im wartość 0.
obwody_df[is.na(obwody_df)]  <- 0

# Mozliwy eksport tabeli do pliku csv.
#write.csv(obwody_df, "obwody_df.csv", row.names = F)