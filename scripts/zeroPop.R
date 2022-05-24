popZero <- decompose_df[decompose_df$TOTAL_POP==0, ]
popZero_add
popZeroMerge <- merge(popZero, obwody, by="OBWOD")
popZeroMerge <- st_as_sf(popZeroMerge)
popZeroMerge <- popZeroMerge[, c("OBWOD", "TOTAL_POP.x", "CLC_POP", "UA_POP", "BDOT_POP", "PktAdr_POP", "geom")]
library(tmap)

tmap_mode("view")

tm_shape(popZeroMerge) +
  tm_polygons(col = "CLC_POP")

write_sf(popZeroMerge, "data/popZeroRedo.shp")
#po ponownym przetworzeniu danych z zawarciem wszystkich obwoow,
#wzrost obwodow bez populacji z 35 do 58
