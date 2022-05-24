library(areal)

ap <- aw_interpolate(obwody, #granice jednostek docelowych (target_units)
                              tid = OBWOD, #id jednostek docelowych
                              source = rejon_granice, #dane zrodlowe
                              sid = REJ, #id jednostek zrodlowych (source units)
                              weight = "sum", 
                              output = "tibble", #wynik w jako tabela, moze tez być obiekt sf
                              extensive = rejon$POP_REJ) #lista zmiennych do przeliczenia
