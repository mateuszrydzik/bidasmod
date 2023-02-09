library(tmap)
library(ggplot2)
library(gridExtra)
library(sf)
source("functions/err_measures.R")

# Obliczenie statystyk opisowych dla wyników modelowania
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

# Wykresy rozrzutu między estymowaną a obserwowaną populacją
bdot <- ggplot(results, aes(x = bdot_pop, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość", breaks=c(25, 50, 100, 200)) +
  theme_linedraw() +
  scale_x_continuous(limits = c(0, 2000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "BDOT", y = "Obserwacje", x = "Estymacje")

clc <- ggplot(results, aes(x = clc_pop, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość", breaks=c(25, 50, 100, 200)) +
  theme_linedraw() +
  scale_x_continuous(limits = c(0, 2000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Corine Land Cover", y = "Obserwacje", x = "Estymacje")

pa <- ggplot(results, aes(x = pktadr_pop, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość", breaks=c(25, 50, 100, 200)) +
  theme_linedraw() +
  scale_x_continuous(limits = c(0, 2000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Punkty adresowe", y = "Obserwacje", x = "Estymacje")

ua <- ggplot(results, aes(x = ua_pop, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość", breaks=c(25, 50, 100, 200)) +
  theme_linedraw() +
  scale_x_continuous(limits = c(0, 2000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Urban Atlas", y = "Obserwacje", x = "Estymacje")

int <- ggplot(results, aes(x = awi_pop, y = POP)) +
  geom_bin_2d(bins = 30) +
  scale_fill_binned(name = "Ilość", breaks=c(25, 50, 100, 200)) +
  theme_linedraw() +
  scale_x_continuous(limits = c(0, 2000)) +
  theme(text = element_text(size = 14)) +
  labs(title = "Metoda pow-wag", y = "Obserwacje", x = "Estymacje")

grid.arrange(bdot, clc, pa, ua, int, ncol=3)

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