# Ładowanie kolejnych funkcji z benchmarku CEC2005
# 
# Author: Przemysław Sadło
###############################################################################

library("cec2005benchmark");

availableDimensions = c(2, 10, 30, 50); # możliwe liczby wymiarów, dla których testujemy funkcje
availableFunctions = 1:25; # lista możliwych funkcji z benchmarku

loadFunction = function(functionNumber) {
    examinedFunction <<- function(points) {
        return(cec2005benchmark(functionNumber, points));
    };
    better <<- "min";
    terErr <<- 1e-8;
    optimumValue <<- scan("../data/fbias_data.txt", quiet = TRUE)[functionNumber];
    
    functionInfo = functionsInfo[[functionNumber]];
    functionName <<- functionInfo$name;
    limitLeft <<- functionInfo$limitLeft;
    limitRight <<- functionInfo$limitRight;
    initLimitLeft <<- functionInfo$initLimitLeft;
    initLimitRight <<- functionInfo$initLimitRight;
    fixedAccuracy <<- functionInfo$fixedAccuracy;
}

loadDimsSpecifics = function(functionNumber, dimensions) {
    functionInfo = functionsInfo[[functionNumber]];
    optimum <<- scan("../data/global_optima.txt", quiet = TRUE,
            skip = functionNumber-1, nlines = 1)[1:dimensions];
    maxFES <<- 1e+4*dimensions;
    
    # w pliku 'global_optima.txt' są jedynie optima globalne
    # funkcje 5, 8 oraz 20 mają ustawione granice przestrzeni przeszukiwań tak,
    # że te optima znajdują sie poza granicami, a wewnątrz granic obowiązują inne
    # optima 'globalne'
    if(functionNumber ==5) {
        optimum[1:ceiling(dimensions/4)] <<- -100;
        optimum[max(floor(0.75*dimensions), 1):dimensions] <<- 100;
    } else if(functionNumber == 8) {
        optimum[2*(1:floor(dimensions/2))-1] <<- -32;
    } else if(functionNumber == 20) {
        optimum[2*(1:floor(dimensions/2))] <<- 5;
    }
}

functionsInfo = list(
        `1` = list(
                name = "F1: Shifted Sphere Function",
                limitLeft = -100,
                limitRight = 100,
                initLimitLeft = -100,
                initLimitRight = 100,
                fixedAccuracy = 1e-6,
                optimumData = "sphere_func_data.txt"
        ),
        `2` = list(
                name = "F2: Shifted Schwefel’s Problem 1.2",
                limitLeft = -100,
                limitRight = 100,
                initLimitLeft = -100,
                initLimitRight = 100,
                fixedAccuracy = 1e-6,
                optimumData = "schwefel_102_data.txt"
        ),
        `3` = list(
                name = "F3: Shifted Rotated High Conditioned Elliptic Function",
                limitLeft = -100,
                limitRight = 100,
                initLimitLeft = -100,
                initLimitRight = 100,
                fixedAccuracy = 1e-6,
                optimumData = "high_cond_elliptic_rot_data.txt"
        ),
        `4` = list(
                name = "F4: Shifted Schwefel’s Problem 1.2 with Noise in Fitness",
                limitLeft = -100,
                limitRight = 100,
                initLimitLeft = -100,
                initLimitRight = 100,
                fixedAccuracy = 1e-6,
                optimumData = "schwefel_102_data.txt"
        ),
        `5` = list(
                name = "F5: Schwefel’s Problem 2.6 with Global Optimum on Bounds",
                limitLeft = -100,
                limitRight = 100,
                initLimitLeft = -100,
                initLimitRight = 100,
                fixedAccuracy = 1e-6,
                optimumData = "schwefel_206_data.txt"
        ),
        `6` = list(
                name = "F6: Shifted Rosenbrock’s Function",
                limitLeft = -100,
                limitRight = 100,
                initLimitLeft = -100,
                initLimitRight = 100,
                fixedAccuracy = 1e-2,
                optimumData = "rosenbrock_func_data.txt"
        ),
        `7` = list(
                name = "F7: Shifted Rotated Griewank’s Function without Bounds",
                limitLeft = -Inf, # brak granicy przestrzeni poszukiwań
                limitRight = Inf,
                initLimitLeft = 0, # optimum znajduje się poza obszarem inicjalizacji
                initLimitRight = 600,
                fixedAccuracy = 1e-2,
                optimumData = "griewank_func_data.txt"
        ),
        `8` = list(
                name = "F8: Shifted Rotated Ackley’s Function with Global Optimum on Bounds",
                limitLeft = -32,
                limitRight = 32,
                initLimitLeft = -32,
                initLimitRight = 32,
                fixedAccuracy = 1e-2,
                optimumData = "ackley_func_data.txt"
        ),
        `9` = list(
                name = "F9: Shifted Rastrigin’s Function",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-2,
                optimumData = "rastrigin_func_data.txt"
        ),
        `10` = list(
                name = "F10: Shifted Rotated Rastrigin’s Function",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-2,
                optimumData = "rastrigin_func_data.txt"
        ),
        `11` = list(
                name = "F11: Shifted Rotated Weierstrass Function",
                limitLeft = -0.5,
                limitRight = 0.5,
                initLimitLeft = -0.5,
                initLimitRight = 0.5,
                fixedAccuracy = 1e-2,
                optimumData = "weierstrass_data.txt"
        ),
        `12` = list(
                name = "F12: Schwefel’s Problem 2.13",
                limitLeft = -pi,
                limitRight = pi,
                initLimitLeft = -pi,
                initLimitRight = pi,
                fixedAccuracy = 1e-2,
                optimumData = "schwefel_213_data.txt"
        ),
        `13` = list(
                name = "F13: Expanded Extended Griewank’s plus Rosenbrock’s Function (F8F2)",
                limitLeft = -3,
                limitRight = 1,
                initLimitLeft = -3,
                initLimitRight = 1,
                fixedAccuracy = 1e-2,
                optimumData = "EF8F2_func_data.txt"
        ),
        `14` = list(
                name = "F14: Shifted Rotated Expanded Scaffer’s F6",
                limitLeft = -100,
                limitRight = 100,
                initLimitLeft = -100,
                initLimitRight = 100,
                fixedAccuracy = 1e-2,
                optimumData = "E_ScafferF6_func_data.txt"
        ),
        `15` = list(
                name = "F15: Hybrid Composition Function",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-2,
                optimumData = "hybrid_func1_data.txt"
        ),
        `16` = list(
                name = "F16: Rotated Hybrid Composition Function",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-2,
                optimumData = "hybrid_func1_data.txt"
        ),
        `17` = list(
                name = "F17: Rotated Hybrid Composition Function with Noise in Fitness",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-1,
                optimumData = "hybrid_func1_data.txt"
        ),
        `18` = list(
                name = "F18: Rotated Hybrid Composition Function",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-1,
                optimumData = "hybrid_func2_data.txt"
        ),
        `19` = list(
                name = "F19: Rotated Hybrid Composition Function with a Narrow Basin for the Global Optimum",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-1,
                optimumData = "hybrid_func2_data.txt"
        ),
        `20` = list(
                name = "F20: Rotated Hybrid Composition Function with the Global Optimum on the Bounds",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-1,
                optimumData = "hybrid_func2_data.txt"
        ),
        `21` = list(
                name = "F21: Rotated Hybrid Composition Function",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-1,
                optimumData = "hybrid_func3_data.txt"
        ),
        `22` = list(
                name = "F22: Rotated Hybrid Composition Function with High Condition Number Matrix",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-1,
                optimumData = "hybrid_func3_data.txt"
        ),
        `23` = list(
                name = "F23: Non-Continuous Rotated Hybrid Composition Function",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-1,
                optimumData = "hybrid_func3_data.txt"
        ),
        `24` = list(
                name = "F24: Rotated Hybrid Composition Function",
                limitLeft = -5,
                limitRight = 5,
                initLimitLeft = -5,
                initLimitRight = 5,
                fixedAccuracy = 1e-1,
                optimumData = "hybrid_func4_data.txt"
        ),
        `25` = list(
                name = "F25: Rotated Hybrid Composition Function without Bounds",
                limitLeft = -Inf, # brak ograniczeń
                limitRight = Inf,
                initLimitLeft = 2, # optimum znajduje się poza obszarem inicjalizacji
                initLimitRight = 5,
                fixedAccuracy = 1e-1,
                optimumData = "hybrid_func4_data.txt"
        )
);
