# Funkcja wyglądająca jak kwadratowe, kartonowe pudełko do jajek.
# Żeby istniało jedno maksimum globalne dodatkowo nachylam wykres funkcji.
# 
# Author: Przemysław Sadło
###############################################################################

# minimalizacja czy maksymalizacja funkcji ("min", "max")
better = "max";

# granice przestrzeni poszukiwań (dla każdego wymiaru)
limitLeft = -6;
limitRight = 6;

# fragment przestrzeni, wewnątrz którego generujemy populację startową
initLimitLeft = limitLeft;
initLimitRight = limitRight;

# punkt przestrzeni, dla którego funkcja osiąga optimum
#optimum = c(4.7, 3.2);

# wartość funkcji w swoim optimum
#optimumValue = 1.7;

# dopuszczalna różnica między wartością rozwiązania znalezionego przez algorytm a rzeczywistym optimum
accuracy = 0.1;

# liczba rozpatrywanych wymiarów
dimensions = 2;

# maksymalna liczba iteracji algorytmu, warunek definitywnie kończący jego pracę
maxIterations = 1000;

# nazwa funkcji, wyświetlana na starcie algorytmu w celu jej identyfikacji
functionName = "EggBox, nachylenie: 0.1";

# definicja funkcji poddawanej optymalizacji
examinedFunction = function(point) {
    x = point[[1]];
    y = point[[2]];
    result = sin(x)*cos(y)+0.1*x+0.1*y;
    return(result);
}
