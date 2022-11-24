library(dplyr)
library(tmap)

results$clcErr <- results$POP - results$clcPOP
results$uaErr <- results$POP - results$uaPOP
results$bdotErr <- results$POP - results$bdotPOP
results$paErr <- results$POP - results$paPOP
results$intErr <- results$POP - results$intPOP

#porownanie wynikow z modelowania dazymetrycznego z areal interpolation

library(ggplot2)
library(gridExtra)

clc <- ggplot(results, aes(x = clcErr)) +
  geom_histogram()+
  labs(title = "CLC", y = "Obserwacje", x = "Błąd")

ua <- ggplot(results, aes(x = uaErr)) +
  geom_histogram()+
  labs(title = "BDOT", y = "Urban Atlas", x = "Błąd")

bdot <- ggplot(results, aes(x = bdotErr)) +
  geom_histogram()+
  labs(title = "BDOT", y = "Obserwacje", x = "Błąd")

pa <- ggplot(results, aes(x = paErr)) +
  geom_histogram()+
  labs(title = "Punkty adresowe", y = "Obserwacje", x = "Błąd")

int <- ggplot(results, aes(x = intErr)) +
  geom_histogram()+
  labs(title = "Areal Interpolation", y = "Obserwacje", x = "Błąd")

grid.arrange(clc, ua, bdot, pa, int, ncol=3)


#porownanie danych zdekomponowanych z oryginalnymi dla obszarow spisowych
#ocena bledow

#MPE (najblizej zera)
mean(results$POP - results$clcPOP) #9.46
mean(results$POP - results$uaPOP) #5.21
mean(results$POP - results$bdotPOP) #5.70
mean(results$POP - results$paPOP) #7.55
mean(results$POP - results$intPOP) #1.383663e-15

#RMSE (jak najmniesze)
sqrt(mean((results$POP - results$clcPOP) ^2)) #186.66
sqrt(mean((results$POP - results$uaPOP) ^2)) #142.10
sqrt(mean((results$POP - results$bdotPOP) ^2)) #135.00
sqrt(mean((results$POP - results$paPOP) ^2)) #158.55
sqrt(mean((results$POP - results$intPOP) ^2)) #206.4

#R2 (0-1)
cor(results$POP, results$clcPOP) ^ 2 #0.27
cor(results$POP, results$uaPOP) ^ 2 #0.40
cor(results$POP, results$bdotPOP) ^ 2 #0.43
cor(results$POP, results$paPOP) ^ 2 #0.34
cor(results$POP, results$intPOP) ^ 2 #0.20

#CV wpolczynnik wariancji (0-1)


g1 <- ggplot(results, aes(x = clcErr, y = POP)) +
  geom_point() +
  xlab("Reszty") +
  ylab("Populacja")

g2 <- ggplot(results, aes(x = clcErr)) +
  geom_histogram()+
  xlab("Reszty") +
  ylab("Ilość")

grid.arrange(g2, g1, ncol=1)


ggplot(results, aes(x = POP, y = clcPOP)) +
  geom_point()

#2d density plots
ggplot(results, aes(x = resztaCLC, y = POP)) +
  geom_bin2d(bins=70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(results, aes(x = resztaCLC, y = POP)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")
