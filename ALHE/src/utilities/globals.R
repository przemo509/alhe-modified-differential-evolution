# Plik zawierający zmienne o zasięgu globalnym wykorzystywane przez program.
# 
# Author: psadlo
###############################################################################
# Domyślnie argumenty funkcji w R są przekazywane przez wartość. Wykorzystanie
# zmiennych globalnych pozwala uniknąć częstego kopiowania większej ilości
# danych. Innym rozwiązaniem byłoby wykorzystanie obiektowych rozszerzeń języka
# R, które umożliwiają przekazywanie argumentów przez referencję.



###############################################################################
# parametry benchmarku CEC2005
###############################################################################
howManyRuns = 25;           # ile razy uruchomić każdą konfigurację, żeby wyciągnąć średni wynik

###############################################################################
# parametry algorytmu ewolucyjnego
###############################################################################

# rozmiar populacji - liczba punktów
populationSize = 50;

# stała skalująca wektor różnicowy v
paramF = 0.9;

# stała określająca prawdopodobieństwo przepisania współrzędnej z
# nowo powstałego punktu podczas krzyżowania
paramCR = 0.9;

###############################################################################
# parametry testowanej funckji celu
# wartości NULL powinny zostać nadpisane wewnątrz procedury loadFunction()
###############################################################################
loadFunction = NULL;        # procedura ładująca pozostałe parametry testowanych funkcji
loadDimsSpecifics = NULL;   # procedura ładująca parametry testowanych funkcji, zależne od liczby wymiarów
availableFunctions = NULL;  # lista funkcji, na których będziemy testować algorytm
availableDimensions = NULL; # liczby wymiarów, dla których testujemy każdą z funkcji

functionName = NULL;        # nazwa funkcji, wypisujemy ją dla celów identyfikacji
examinedFunction=NULL;      # definicja funkcji poddawanej optymalizacji
better=NULL;                # minimalizacja czy maksymalizacja funkcji ("min", "max")
limitLeft=NULL;             # granice przestrzeni poszukiwań (dla każdego wymiaru te same)
limitRight=NULL;            # j.w.
initLimitLeft=NULL;         # fragment przestrzeni, wewnątrz którego generujemy populację startową
initLimitRight=NULL;        # j.w.
terErr = NULL;              # dopuszczalna różnica między wartością rozwiązania znalezionego przez algorytm a rzeczywistym optimum
maxFES = NULL;              # maksymalna liczba ewaluacji funkcji celu, warunek definitywnie kończący jego pracę
optimum=NULL;               # punkt przestrzeni, dla którego funkcja osiąga optimum
optimumValue=NULL;          # wartość funkcji w swoim optimum
fixedAccuracy=NULL;         # wymagana przez benchmark dokładność (trzeba dla niej zmierzyć FES)
# jeśli nie nadpiszemy 'optimum' i 'optimumValue' algorytm będzie próbowal je
# wyznaczyć, a jako dodatkowy warunek stopu posłuży wówczas maxSpread()

###############################################################################
# zmienne wewnętrzne algorytmu, wykorzystywane w wielu miejscach
###############################################################################
dimensions = NULL;          # bierząca liczba wymiarów, po kolei z 'availableDimensions'
population = NULL;          # bierząca populacja punktów
currFES = NULL;             # liczba dotychczasowych  ewaluacji funkcji celu, nie może przekroczyć maxFES
plotData = NULL;            # dane dla wykresu 2D, żeby ich nie obliczać wielokrotnie