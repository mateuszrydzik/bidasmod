# Struktura projektu

### functions

dir; zawiera wszystkie zdefiniowane funkcje na potrzeby pracy

1.  decompose.R proces dekompozycji, wykorzystywane w modelowaniu dazymetrycznym

2.  err_measures.R miary oceny błędów, wykorzystywane w analizie błędów i obszarów o zerowej estymowanej populacji

3.  read_data.R wczytanie danych, wykorzystywane w przetwarzaniu danych wejściowych

### initial-data

dir; katalog dla danych wejściowych

### scripts

dir; skrypty obejmujące kolejne etapy pracy

1.  data_processing.R

    przetwarzanie danych wejściowych

2.  dasmod_awi.R

    modelowanie dazymetryczne oraz metoda powierzchniowo-wagowa

3.  err_compare.R

    analiza błędów

4.  zero_pop.R

    analiza obwodów o zerowej estymowanej populacji

### main.R

script; zawieraja niezbędne elementy wszystkich etapów do uzyskania wyników zawartych w pracy. Pozbawione komentarzy, pełni rolę streszczenia zawartości katalogu *scripts*
