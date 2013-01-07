# Definicja funkcji F3: Shifted Rotated High Conditioned Elliptic Function
# Funkcja pochodzi z benchmarku CEC 2005
# TODO link do CEC 2005
# 
# Author: Przemysław Sadło
###############################################################################

# nazwa funkcji, wyświetlana na starcie algorytmu w celu jej identyfikacji
functionName = "F3: Shifted Rotated High Conditioned Elliptic Function";

# minimalizacja czy maksymalizacja funkcji ("min", "max")
better = "min";

# granice przestrzeni poszukiwań (dla każdego wymiaru)
limitLeft = -100;
limitRight = 100;

# fragment przestrzeni, wewnątrz którego generujemy populację startową
initLimitLeft = limitLeft;
initLimitRight = limitRight;

# punkt przestrzeni, dla którego funkcja osiąga optimum
optimum = scan("../data/high_cond_elliptic_rot_data.txt", quiet = TRUE);

# wartość funkcji w swoim optimum
optimumValue = scan("../data/fbias_data.txt", quiet = TRUE)[3];

# dopuszczalna różnica między wartością rozwiązania znalezionego przez algorytm a rzeczywistym optimum
accuracy = 1e-8;

# liczba rozpatrywanych wymiarów
dimensions = 2; # 10, 30, 50

# maksymalna liczba iteracji algorytmu, warunek definitywnie kończący jego pracę
maxIterations = 1e+4*dimensions;

# pomocnicza macierz M, potrzebna do obliczania wartości funkcji
matrixFileName = paste("../data/elliptic_M_D", dimensions, ".txt", sep = "");
M = matrix(scan(matrixFileName, quiet = TRUE), ncol = dimensions);

# definicja funkcji poddawanej optymalizacji
examinedFunction = function(point) {
    z = t((point - optimum)[1:dimensions]%*%M);
    
    result = 0;
    for(i in 1:dimensions) {
        result = result + ((1e6)^((i-1)/(dimensions-1)))*z[i]*z[i];
    }
    result = result + optimumValue;
    return(result);
}
