# Definicja funkcji F2: Shifted Schwefel’s Problem 1.2
# Funkcja pochodzi z benchmarku CEC 2005
# TODO link do CEC 2005
# 
# Author: Przemysław Sadło
###############################################################################

# nazwa funkcji, wyświetlana na starcie algorytmu w celu jej identyfikacji
functionName = "F2: Shifted Schwefel’s Problem 1.2";

# minimalizacja czy maksymalizacja funkcji ("min", "max")
better = "min";

# granice przestrzeni poszukiwań (dla każdego wymiaru)
limitLeft = -100;
limitRight = 100;

# fragment przestrzeni, wewnątrz którego generujemy populację startową
initLimitLeft = limitLeft;
initLimitRight = limitRight;

# punkt przestrzeni, dla którego funkcja osiąga optimum
optimum = scan("../data/schwefel_102_data.txt", quiet = TRUE);

# wartość funkcji w swoim optimum
optimumValue = scan("../data/fbias_data.txt", quiet = TRUE)[2];

# dopuszczalna różnica między wartością rozwiązania znalezionego przez algorytm a rzeczywistym optimum
accuracy = 1e-8;

# liczba rozpatrywanych wymiarów
dimensions = 2; # 10, 30, 50, max = 100

# maksymalna liczba iteracji algorytmu, warunek definitywnie kończący jego pracę
maxIterations = 1e+4*dimensions;

# nazwa funkcji, wyświetlana na starcie algorytmu w celu jej identyfikacji
functionName = "F2: Shifted Schwefel’s Problem 1.2";

# definicja funkcji poddawanej optymalizacji
examinedFunction = function(point) {
    z = point - optimum;
    result = 0;
    for(i in 1:dimensions) {
        partialResult = 0;
        for(j in 1:i) {
            partialResult = partialResult + z[j];
        }
        result = result + partialResult*partialResult;
    }
    result = result + optimumValue;
    return(result);
}
