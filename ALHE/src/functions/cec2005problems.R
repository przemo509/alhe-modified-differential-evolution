# Definicje funkcji z benchmarku CEC 2005
# TODO link do CEC 2005
# 
# Author: Przemysław Sadło
###############################################################################

library("cec2005benchmark");

availableFunctions = c(1, 2, 3); # lista funkcji z benchamrku, na których będziemy testować algorytm
availableDimensions = c(2, 10, 30, 50); # liczby wymiarów, dla których testujemy każdą z funkcji


loadFunction = function(functionNumber, dimensions) {
    functionName <<- paste("Funkcja CEC 2005 nr:", functionNumber);
    examinedFunction <<- function(point) {
        return(cec2005benchmark(functionNumber, point));
    }
    better <<- "min";
    limitLeft <<- -100;
    limitRight <<- 100;
    initLimitLeft <<- limitLeft;
    initLimitRight <<- limitRight;
    optimum <<- scan(paste0("../data/", optimumPointsData[functionNumber]), quiet = TRUE)[1:dimensions];
    optimumValue <<- scan("../data/fbias_data.txt", quiet = TRUE)[functionNumber]
    terErr <<- 1e-8;
    maxFES <<- 1e+4*dimensions;
}

optimumPointsData = c(
        "sphere_func_data.txt",
        "schwefel_102_data.txt",
        "high_cond_elliptic_rot_data.txt",
        "schwefel_102_data.txt"
        );