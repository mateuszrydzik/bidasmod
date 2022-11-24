library(areal)
library(sf)
library(dplyr)
library(tmap)

pth_obwody = "initial-data/Obwody_2011.gpkg"

##### load data
obwody <- read_sf(pth_obwody) %>%  #obwody, lower ranked spatial units
  st_transform(., crs = 2180)

rejony <- obwody %>% #grouping to rejony, higher ranked spatial units
  group_by(REJ) %>% 
  summarize() %>% 
  ungroup()

rejony_POP <- aggregate(obwody$TOTAL_POP, by = list(obwody$REJ), FUN = sum) 
colnames(rejony_POP) <- c("REJ", "POP_REJ")
rejony_with_pop <- rejony %>% merge(rejony_POP, by = "REJ")
  
ap <- aw_interpolate(obwody, #granice jednostek docelowych (target_units)
                              tid = OBWOD, #id jednostek docelowych
                              source = rejony_with_pop,#dane zrodlowe
                              sid = REJ, #id jednostek zrodlowych (source units)
                              weight = "sum", 
                              output = "tibble", #wynik w jako tabela, moze tez być obiekt sf
                              extensive = "POP_REJ") #lista zmiennych do przeliczenia
tmap_mode("plot")
tm_shape(rejony_with_pop) +
  tm_polygons(col="POP_REJ")

ar_validate(rejony_with_pop_andObwod, obwody, varList = rejony_with_pop$POP_REJ, method = "aw", verbose = T)


#wywal to ponizej do innego pliku, zmien to ostateczne decompose_df na results

results <- merge(decompose_df, ap[, c("OBWOD","POP_REJ")], by = "OBWOD")
results <- merge(results, obwody[, c("OBWOD", "REJ")], by = "OBWOD") 
colnames(results) <- c("OBWOD", "POP", "clcPOP", "uaPOP", "bdotPOP", "paPOP", "intPOP", "REJ", "geom")
#colnames(decompose_df)[colnames(decompose_df) == 'POP_REJ'] <- 'interpolate_POP' 
st_write(results, "data/results.shp")
