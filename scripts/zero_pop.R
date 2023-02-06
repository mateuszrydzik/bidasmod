library(tmap)
source("functions/err_measures.R")

# Utworzenie tabeli z obwodami, których wejściowe obserwacje nie posiadały 
# populacji

popZero <- results[results$POP==0, ]

# Zestawienie ilości obwodów o zerowej liczbie populacji dla obserwacji oraz 
# wyników modelowania
data.frame(
  obs=length(popZero$POP),
  bdot=length(popZero$bdot_pop[popZero$bdot_pop>0]),
  clc=length(popZero$clc_pop[popZero$clc_pop>0]),
  pktadr=length(popZero$pktadr_pop[popZero$pktadr_pop>0]),
  ua=length(popZero$ua_pop[popZero$ua_pop>0]),
  awi=length(popZero$awi_pop[popZero$awi_pop>0])
)

# Zestawienie wartości wskaźnika błędnej klasyfikacji obszarów zurbanizowanych
data.frame(
  bdot=zeropop_err(results$bdot_pop, results$POP),
  clc=zeropop_err(results$clc_pop, results$POP),
  pa=zeropop_err(results$pktadr_pop, results$POP),
  ua=zeropop_err(results$ua_pop, results$POP)
)

#  Przestrzenny rozkład obwodów o błędnej klasyfikacji obszarów zurbanizowanych
zeropop_clc <- tm_shape(results_sf) +
  tm_fill(col = "gray") +
tm_shape(results_sf[results_sf$clc_pop == 0 &results_sf$POP > 0,]) +
  tm_fill(col="red") +
  tm_layout(title = "CLC")

zeropop_bdot <- tm_shape(results_sf) +
  tm_fill(col = "gray") +
tm_shape(results_sf[results_sf$bdot_pop == 0 &results_sf$POP > 0,]) +
  tm_fill(col="red") +
  tm_layout(title = "BDOT")  
      
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
