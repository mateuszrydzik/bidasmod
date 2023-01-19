library(dplyr)
library(tmap)
source("functions/err_measures.R")

# Obliczenie błędów szacunku dla każdego zbioru

results$clc_err <- results$POP - results$clc_pop
results$ua_err <- results$POP - results$ua_pop
results$bdot_err <- results$POP - results$bdot_pop
results$pktadr_err <- results$POP - results$pktadr_pop
results$awi_err <- results$POP - results$awi_pop

# Porównanie wyników modelowania dazymetrycznego z metodą powierzchniowo-wagową

library(ggplot2)
library(gridExtra)

bdot <- ggplot(results, aes(x = bdot_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  labs(title = "BDOT", y = "Obserwacje", x = "Błąd")

clc <- ggplot(results, aes(x = clc_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  labs(title = "CLC", y = "Obserwacje", x = "Błąd")

pa <- ggplot(results, aes(x = pktadr_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  labs(title = "Punkty adresowe", y = "Obserwacje", x = "Błąd")

ua <- ggplot(results, aes(x = ua_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  labs(title = "Urban Atlas", y = "Obserwacje", x = "Błąd")

int <- ggplot(results, aes(x = awi_err)) +
  geom_histogram(binwidth = 22)+
  theme_linedraw() +
  labs(title = "Metoda pow-wag", y = "Obserwacje", x = "Błąd")

grid.arrange(clc, ua, bdot, pa, int, ncol=3)


# Porownanie wyników modelowania dazymetrycznego z oryginalnymi dla obszarow spisowych
# Ocena bledow za pomocą wskaźników

data.frame(
  BDOT=c(mpe(results$bdot_err), rmse(results$bdot_err), r2(results$POP, results$bdot_pop)),
  CLC=c(mpe(results$clc_err), rmse(results$clc_err), r2(results$POP, results$clc_err)),
  Pkt_adr=c(mpe(results$pktadr_err), rmse(results$pktadr_err), r2(results$POP, results$pktadr_err)),
  UA=c(mpe(results$ua_err), rmse(results$ua_err), r2(results$POP, results$ua_err)),
  AWI=c(mpe(results$awi_err), rmse(results$awi_err), r2(results$POP, results$awi_err)),
  row.names = c("MPE", "RMSE", "R2"))

tm_shape(results) +
  tm_polygons(col = "clc_err")

#2d density plots
bdot <- ggplot(results, aes(x = bdot_err, y = POP)) +
  geom_bin_2d(binwidth = 22) +
  scale_fill_continuous(name = "Ilość") +
  theme_linedraw() +
  labs(title = "BDOT", y = "Populacja", x = "Błąd")

clc <- ggplot(results, aes(x = clc_err, y = POP)) +
  geom_bin_2d(binwidth = 22) +
  scale_fill_continuous(name = "Ilość") +
  theme_linedraw() +
  labs(title = "CLC", y = "Populacja", x = "Błąd")

pktadr <- ggplot(results, aes(x = pktadr_err, y = POP)) +
  geom_bin_2d(binwidth = 22) +
  scale_fill_continuous(name = "Ilość") +
  theme_linedraw() +
  labs(title = "Punkty adresowe", y = "Populacja", x = "Błąd")

ua <- ggplot(results, aes(x = ua_err, y = POP)) +
  geom_bin_2d(binwidth = 22) +
  scale_fill_continuous(name = "Ilość") +
  theme_linedraw() +
  labs(title = "Urban Atlas", y = "Populacja", x = "Błąd")

#int <- ggplot(results, aes(x = awi_err)) +
#  geom_histogram(binwidth = 22)+
#  theme_bw() +
#  labs(title = "Metoda pow-wag", y = "Obserwacje", x = "Błąd")

grid.arrange(bdot, clc, pktadr, ua, ncol=2)

tm_shape(results_sf, bbox = st_bbox(c(xmin = 324682.4185, xmax = 490552.8874, ymax = 392897.5283, ymin = 522945.1219), crs = st_crs(2180))) +
  tm_polygons(col = "clc_err", border.alpha = 0.2) +
  tm_view(set.view = 5)



tm_shape(results_sf) +
  tm_polygons(col = "ua_err")
