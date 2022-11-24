popZero <- results[results$POP==0, ]
#popZeroMerge <- st_as_sf(popZeroMerge)
#popZeroMerge <- popZeroMerge[, c("OBWOD", "TOTAL_POP.x", "CLC_POP", "UA_POP", "BDOT_POP", "PktAdr_POP", "geom")]

length(popZero$POP) #58 obwodow z zerowa obserwowana populacja
length(popZero$clcPOP[popZero$clcPOP>0]) #47 obwodow z populacja estymowana CLC
length(popZero$uaPOP[popZero$uaPOP>0]) #37 obwodow z populacja estymowana UA
length(popZero$bdotPOP[popZero$bdotPOP>0]) #37 obwodow z populacja estymowana BDOT
length(popZero$paPOP[popZero$paPOP>0]) # 48 obwodow z populacja estymowana PA
length(popZero$intPOP[popZero$intPOP>0]) # 49 obwodow z populacja estymowana area interpolation

#ZNAJDZ WIERSZE GDZIE POPULACJA OBSERWOWANA NIE BYLA ROWNA ZERO, ALE POPULACJA Z DANYCH DODATKOWYCH WYNIOSLA ZERO
#NP Z DECOMPOSE WYSZLO ZE JEST 452 OBREBOW BEZ POPULACJI, Z DANYCH OBSERWOWANYCH BEZ POPULACJI BYLO TYLKO 58 OBREBOW
#CZYLI TO JEST 452/58

#ZROB Z NICH WSKAZNIK

length(results$POP[results$POP>0])

# 452 obwodow gdzie jest obserwowana POP ale CLC nie wyliczyl
length(results$clcPOP[results$clcPOP==0 & results$POP > 0])

# 207 obwodow gdzie jest obserwowana POP ale UA nie wyliczyl
length(results$uaPOP[results$uaPOP==0 & results$POP > 0])

# 172 obwodow gdzie jest obserwowana POP ale BDOT nie wyliczyl
length(results$bdotPOP[results$bdotPOP==0 & results$POP > 0])

# 178 obwodow gdzie jest obserwowana POP ale PA nie wyliczyl
length(results$paPOP[results$paPOP==0 & results$POP > 0])

# 0 obwodow gdzie jest obserwowana POP ale int nie wyliczyl
length(results$intPOP[results$intPOP==0 & results$POP > 0])


library(tmap)
tmap_mode("view")

tm_shape(popZeroMerge) +
  tm_polygons(col = "CLC_POP")

tm_shape(clcZero) +
  tm_polygons(col = "CLC_POP")

write_sf(popZeroMerge, "data/popZeroRedo.shp")
#po ponownym przetworzeniu danych z zawarciem wszystkich obwoow,
#wzrost obwodow bez populacji z 35 do 58
