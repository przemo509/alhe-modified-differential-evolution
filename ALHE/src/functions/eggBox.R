# Funkcja wyglądająca jak kwadratowe, kartonowe pudełko do jajek.
# Żeby istniało jedno maksimum globalne dodatkowo nachylam wykres funkcji.
# 
# Author: Przemysław Sadło
###############################################################################

# nazwa funkcji, wyświetlana na starcie algorytmu w celu jej identyfikacji
functionName = "EggBox, nachylenie: 0.1";

# minimalizacja czy maksymalizacja funkcji ("min", "max")
better = "max";

# granice przestrzeni poszukiwań (dla każdego wymiaru)
limitLeft = -6;
limitRight = 6;

# fragment przestrzeni, wewnątrz którego generujemy populację startową
initLimitLeft = limitLeft;
initLimitRight = limitRight;

# punkt przestrzeni, dla którego funkcja osiąga optimum
optimum = c(4.8130704607058, 3.24226992395493);

# wartość funkcji w swoim optimum
optimumValue = 1.79543190402852;

# dopuszczalna różnica między wartością rozwiązania znalezionego przez algorytm a rzeczywistym optimum
accuracy = 1e-8;

# liczba rozpatrywanych wymiarów
dimensions = 2;

# maksymalna liczba iteracji algorytmu, warunek definitywnie kończący jego pracę
maxIterations = 1000;

# definicja funkcji poddawanej optymalizacji
examinedFunction = function(point) {
    x = point[[1]];
    y = point[[2]];
    result = sin(x)*cos(y)+0.1*x+0.1*y;
    return(result);
}
