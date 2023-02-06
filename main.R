library(areal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(sf)
library(tmap)

source("functions/decompose.R")
source("functions/err_measures.R")
source("functions/read_data.R")

##################################
# 1. Wczytanie danych
# Sciezki do danych
pth_obwody = "initial-data/Obwody_2011.gpkg"
pth_bdot = "initial-data/BDOT_PTZB.gpkg"
pth_clc = "initial-data/clc.gpkg"
pth_pktadr = "initial-data/punkty_adresowe.gpkg"
pth_ua = "initial-data/PL005L2_POZNAN_UA2012_revised_v021.gpkg"

# Obwody spisowe i rejony statystyczne
obwody <- read_data(pth_obwody, crs = 2180)

# Grupowanie obwodów do rejonow statystycznych
rejony <- obwody %>% 
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

# Obliczenie powierzchni zurbanizowanej w jednostkach spisowych
bdot_area <- st_intersection(bdot, obwody) %>% 
  mutate(bdot_area = st_area(.)) %>% 
  dplyr::select(OBWOD, REJ, bdot_area) %>% st_drop_geometry()

# W niektorych obwodach dane pomocnicze występują w formie multipoligonu, co 
# prowadzi do wystepowania kilku rekordów tego samego obwodu. 
# Ta operacja sumuje powierzchnie do jednego rekordu obwodu.
bdot_area_o <- aggregate(bdot_area$bdot_area, by = list(bdot_area$OBWOD), FUN = sum)
colnames(bdot_area_o) <- c("OBWOD", "bdot_area")

# Agregacja powierzchni zurbanizowanej do rejonow spisowych.
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

# Obliczenie ilosci punktow adresowych
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

# Agregacja populacji z obwodow do rejonow statystycznych
rejony_pop <- aggregate(obwody$TOTAL_POP, by = list(obwody$REJ), FUN = sum)
colnames(rejony_pop) <- c("REJ", "TOTAL_POP_R")

# Wygenerowanie tabeli zawierajacej powierzchnie danych pomocniczych dla obwodów oraz rejonoww statystycznych
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

################################## 
#2. Modelowanie dazymetryczne oraz metoda powierzchniowo-wagowa

# Dekompozycja populacji z rejonow statystycznych do obwodow przy wykorzystaniu danych pomocniczych
obwody_df$bdot_pop <- decompose(obwody_df$bdot_area, obwody_df$bdot_area_r, obwody_df$TOTAL_POP_R)
obwody_df$clc_pop <- decompose(obwody_df$clc_area, obwody_df$clc_area_r, obwody_df$TOTAL_POP_R)
obwody_df$pktadr_pop <- decompose(obwody_df$pktadr_count, obwody_df$pktadr_count_r, obwody_df$TOTAL_POP_R)
obwody_df$ua_pop <- decompose(obwody_df$ua_area, obwody_df$ua_area_r, obwody_df$TOTAL_POP_R)

decompose_df <- obwody_df %>% dplyr::select(OBWOD, TOTAL_POP, bdot_pop, clc_pop, pktadr_pop, ua_pop)

# Metoda powierzchniowo-wagowa
rejony_with_pop <- rejony %>% merge(rejony_pop, by = "REJ")

awi <- aw_interpolate(obwody,
                      tid = OBWOD,
                      source = rejony_with_pop,
                      sid = REJ,
                      weight = "sum",
                      output = "tibble",
                      extensive = "TOTAL_POP_R")

# Utworzenie tabeli wynikowej
results <- merge(decompose_df, awi[, c("OBWOD","TOTAL_POP_R")], by = "OBWOD")
results <- merge(results, obwody[, c("OBWOD", "REJ")], by = "OBWOD") 
colnames(results) <- c("OBWOD", "POP", "bdot_pop", "clc_pop", "pktadr_pop", "ua_pop", "awi_pop", "REJ", "geom")

################################## 
#3. Analiza błędu

# Statystyki opisowe
results_summary <- cbind(summary(results$bdot_err, digits=2),
                         summary(results$clc_err, digits=2),
                         summary(results$pktadr_err, digits=2),
                         summary(results$ua_err, digits=2),
                         summary(results$awi_err, digits=2))
colnames(results_summary) <- c("BDOT" , "CLC", "PA", "UA", "AWI")

results_sd <- c(sd(results$bdot_err), sd(results$clc_err), sd(results$pktadr_err), sd(results$ua_err), sd(results$awi_err))
names(results_sd) <-c("BDOT" , "CLC", "PA", "UA", "AWI")


# Obliczenie bledu estymacji dla kazdego zbioru
results$clc_err <- results$POP - results$clc_pop
results$ua_err <- results$POP - results$ua_pop
results$bdot_err <- results$POP - results$bdot_pop
results$pktadr_err <- results$POP - results$pktadr_pop
results$awi_err <- results$POP - results$awi_pop


# Porownanie wynikow modelowania dazymetrycznego z metoda powierzchniowo-wagowa
bdot <- ggplot(results, aes(x = bdot_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "BDOT", y = "Obserwacje", x = "Błąd")

clc <- ggplot(results, aes(x = clc_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  scale_y_continuous(limits = c(0, 600)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Corine Land Cover", y = "Obserwacje", x = "Błąd")

pa <- ggplot(results, aes(x = pktadr_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Punkty adresowe", y = "Obserwacje", x = "Błąd")

ua <- ggplot(results, aes(x = ua_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  scale_y_continuous(limits = c(0, 600)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Urban Atlas", y = "Obserwacje", x = "Błąd")

int <- ggplot(results, aes(x = awi_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  scale_y_continuous(limits = c(0, 600)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Metoda pow-wag", y = "Obserwacje", x = "Błąd")

grid.arrange(bdot, clc, pa, ua, int, ncol=3)

# Wskazniki oceny bledow 
data.frame(
  BDOT=c(mpe(results$bdot_err), rmse(results$bdot_err), r2(results$POP, results$bdot_pop)),
  CLC=c(mpe(results$clc_err), rmse(results$clc_err), r2(results$POP, results$clc_err)),
  Pkt_adr=c(mpe(results$pktadr_err), rmse(results$pktadr_err), r2(results$POP, results$pktadr_err)),
  UA=c(mpe(results$ua_err), rmse(results$ua_err), r2(results$POP, results$ua_err)),
  AWI=c(mpe(results$awi_err), rmse(results$awi_err), r2(results$POP, results$awi_err)),
  row.names = c("MPE", "RMSE", "R2"))

# Dwuwymiarowe histogramy - zaleznosc między populacją a bledem estymacji
bdot <- ggplot(results, aes(x = bdot_err, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość") +
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "BDOT", y = "Populacja", x = "Błąd")

clc <- ggplot(results, aes(x = clc_err, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość") +
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Corine Land Cover", y = "Populacja", x = "Błąd")

pa <- ggplot(results, aes(x = pktadr_err, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość") +
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Punkty adresowe", y = "Populacja", x = "Błąd")

ua <- ggplot(results, aes(x = ua_err, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość") +
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Urban Atlas", y = "Populacja", x = "Błąd")

int <- ggplot(results, aes(x = awi_err, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość") +
  theme_linedraw() +
  scale_x_continuous(limits = c(-1500, 1000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Metoda pow-wag", y = "Obserwacje", x = "Błąd")

grid.arrange(bdot, clc, pa, ua, int, ncol=2)

# Przestrzenne mapy rozkladu bledu
results_sf <- st_as_sf(results)

# BDOT
# Mapa rozkladu dla obszaru FUA
fua_bdot <- tm_shape(results_sf) +
  tm_polygons(col = "bdot_err", title="Błąd", border.alpha = 0.2, palette = "RdGy") +
  tm_scale_bar(position=c("left", "bottom"), text.size = 0.8) +
  tm_compass(position = c("right", "top"))

# Mapa rozkladu dla obszaru miasta
poz_bdot <- tm_shape(results_sf, 
                     bbox = st_bbox(c(xmin = 345343.8710092928, xmax = 369175.0948500195,
                                      ymax = 517691.1373427557, ymin = 494191.43407221045), 
                                    crs = st_crs(2180))) +
  tm_polygons(col = "bdot_err", border.alpha = 0.2, legend.show = F, palette = "RdGy") +
  tm_scale_bar(position=c("left", "bottom"), text.size = 0.8) +
  tm_compass(position = c("right", "top"))

tmap_arrange(fua_bdot, poz_bdot, ncol=2)

# CLC
fua_clc <- tm_shape(results_sf) +
  tm_polygons(col = "clc_err", title="Błąd", border.alpha = 0.2, palette = "RdGy") +
  tm_scale_bar(position=c("left", "bottom"), text.size = 0.8) +
  tm_compass(position = c("right", "top"))

poz_clc <- tm_shape(results_sf, 
                     bbox = st_bbox(c(xmin = 345343.8710092928, xmax = 369175.0948500195,
                                      ymax = 517691.1373427557, ymin = 494191.43407221045), 
                                    crs = st_crs(2180))) +
  tm_polygons(col = "clc_err", border.alpha = 0.2, legend.show = F, palette = "RdGy") +
  tm_scale_bar(position=c("left", "bottom"), text.size = 0.8) +
  tm_compass(position = c("right", "top"))

tmap_arrange(fua_clc, poz_clc, ncol=2)

# Punkty adresowe
fua_pa <- tm_shape(results_sf) +
  tm_polygons(col = "pktadr_err", title="Błąd", border.alpha = 0.2, palette = "RdGy") +
  tm_scale_bar(position=c("left", "bottom"), text.size = 0.8) +
  tm_compass(position = c("right", "top"))

poz_pa <- tm_shape(results_sf, 
                   bbox = st_bbox(c(xmin = 345343.8710092928, xmax = 369175.0948500195,
                                    ymax = 517691.1373427557, ymin = 494191.43407221045), 
                                  crs = st_crs(2180))) +
  tm_polygons(col = "pktadr_err", border.alpha = 0.2, legend.show = F, palette = "RdGy") +
  tm_scale_bar(position=c("left", "bottom"), text.size = 0.8) +
  tm_compass(position = c("right", "top"))

tmap_arrange(fua_pa, poz_pa, ncol=2)

# UA
fua_ua <- tm_shape(results_sf) +
  tm_polygons(col = "ua_err", title="Błąd", border.alpha = 0.2, palette = "RdGy") +
  tm_scale_bar(position=c("left", "bottom"), text.size = 0.8) +
  tm_compass(position = c("right", "top"))

poz_ua <- tm_shape(results_sf, 
                   bbox = st_bbox(c(xmin = 345343.8710092928, xmax = 369175.0948500195,
                                    ymax = 517691.1373427557, ymin = 494191.43407221045), 
                                  crs = st_crs(2180))) +
  tm_polygons(col = "ua_err", border.alpha = 0.2, legend.show = F, palette = "RdGy") +
  tm_scale_bar(position=c("left", "bottom"), text.size = 0.8) +
  tm_compass(position = c("right", "top"))

tmap_arrange(fua_ua, poz_ua, ncol=2)

################################## 
#4. Analiza obszarow o zerowej estymowanej populacji

# Wyodrebnienie obwodow z obserwowana zerowa populacja
popZero <- results[results$POP==0, ]

# Zestawienie ilosci obwodow o zerowej liczbie populacji dla obserwacji oraz 
# wynikow modelowania
data.frame(
  obs=length(popZero$POP),
  bdot=length(popZero$bdot_pop[popZero$bdot_pop>0]),
  clc=length(popZero$clc_pop[popZero$clc_pop>0]),
  pktadr=length(popZero$pktadr_pop[popZero$pktadr_pop>0]),
  ua=length(popZero$ua_pop[popZero$ua_pop>0]),
  awi=length(popZero$awi_pop[popZero$awi_pop>0])
)

# Zestawienie wartości wskaznika blednej klasyfikacji obszarow zurbanizowanych
data.frame(
  bdot=zeropop_err(results$bdot_pop, results$POP),
  clc=zeropop_err(results$clc_pop, results$POP),
  pa=zeropop_err(results$pktadr_pop, results$POP),
  ua=zeropop_err(results$ua_pop, results$POP)
)

# Przestrzenny rozklad obwodow o blednej klasyfikacji obszarow zurbanizowanych
zeropop_bdot <- tm_shape(results_sf) +
  tm_fill(col = "gray") +
  tm_shape(results_sf[results_sf$bdot_pop == 0 &results_sf$POP > 0,]) +
  tm_fill(col="red") +
  tm_layout(title = "BDOT")  

zeropop_clc <- tm_shape(results_sf) +
  tm_fill(col = "gray") +
  tm_shape(results_sf[results_sf$clc_pop == 0 &results_sf$POP > 0,]) +
  tm_fill(col="red") +
  tm_layout(title = "CLC")

zeropop_pa <- tm_shape(results_sf) +
  tm_fill(col = "gray") +
  tm_shape(results_sf[results_sf$pktadr_pop == 0 &results_sf$POP > 0,]) +
  tm_fill(col="red") +
  tm_layout(title = "Pkt. adr.")  

zeropop_ua <- tm_shape(results_sf) +
  tm_fill(col = "gray") +
  tm_shape(results_sf[results_sf$ua_pop == 0 &results_sf$POP > 0,]) +
  tm_fill(col="red") +
  tm_layout(title = "UA")  

tmap_arrange(zeropop_bdot, zeropop_clc, zeropop_pa, zeropop_ua, ncol=2)