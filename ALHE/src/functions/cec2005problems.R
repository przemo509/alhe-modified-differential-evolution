# Ładowanie kolejnych funkcji z benchmarku CEC2005
# 
# Author: Przemysław Sadło
###############################################################################

library("cec2005benchmark");

availableDimensions = c(2, 10, 30, 50); # liczby wymiarów, dla których testujemy każdą z funkcji
availableFunctions = c(1); # lista funkcji z benchmarku, na których będziemy testować algorytm
names(availableFunctions) = c(
                              "F11: Shifted Rotated Weierstrass Function"
                             ); # nazwy funkcji do wyświetlenia w wynikach

loadFunction = function(functionNumber) {
    functionName <<- names(availableFunctions)[functionNumber];
    examinedFunction <<- functions(functionNumber);
    better <<- "min";
    limitLeft <<- -100; if(functionNumber==11) limitLeft <<- -0.5;
    limitRight <<- 100; if(functionNumber==11) limitRight <<- 0.5;
    initLimitLeft <<- limitLeft; # TODO 7 inaczej
    initLimitRight <<- limitRight;
    optimumValue <<- scan("../data/fbias_data.txt", quiet = TRUE)[functionNumber];
    fixedAccuracy <<- fixedAccuracyData[functionNumber];
    terErr <<- 1e-8;
}

loadDimsSpecifics = function(functionNumber, dimensions) {
    optimum <<- scan(paste0("../data/", optimumPointsData[functionNumber]), quiet = TRUE)[1:dimensions];
    maxFES <<- 1e+4*dimensions;
}

optimumPointsData = c(
        "sphere_func_data.txt",
        "schwefel_102_data.txt",
        "high_cond_elliptic_rot_data.txt",
        "schwefel_102_data.txt",
        "schwefel_206_data.txt",
        "rosenbrock_func_data.txt",
        "griewank_func_data.txt",
        "",
        "",
        "",
        "weierstrass_data.txt"
        );
        
fixedAccuracyData = c(
        1e-6, 1e-6, 1e-6, 1e-6, 1e-6, # f1-f5
        1e-2, 1e-2, 1e-2, 1e-2, 1e-2, 1e-2, 1e-2, 1e-2, 1e-2, 1e-2, 1e-2, # f6-f16
        1e-1, 1e-1, 1e-1, 1e-1, 1e-1, 1e-1, 1e-1, 1e-1, 1e-1 # f17-f25 
        );