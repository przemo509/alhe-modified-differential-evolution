# Przykładowa funkcja do testowania.
# Jej wykres wygląda jak kwadratowe, kartonowe pudełko do jajek.
# Żeby istniało jedno maksimum globalne dodatkowo nachylam wykres funkcji.
# 
# Author: Przemysław Sadło
###############################################################################

availableFunctions = c(1);
names(availableFunctions) = c("EggBox, nachylenie: 0.1");
availableDimensions = c(2);

loadFunction = function(functionNumber) {
    functionName <<- names(availableFunctions)[functionNumber];
    examinedFunction <<- function(point) {
        x = point[[1]];
        y = point[[2]];
        result = sin(x)*cos(y)+0.1*x+0.1*y;
        return(result);
    }
    better <<- "max";
    limitLeft <<- -6;
    limitRight <<- 5.5;
    initLimitLeft <<- limitLeft;
    initLimitRight <<- limitRight;
    optimum <<- NULL;# c(4.8130704607058, 3.24226992395493);
    optimumValue <<- NULL; # 1.79543190402852;
    terErr <<- 1e-8;
}

loadDimsSpecifics = function(functionNumber, dimensions) {
    maxFES <<- 1e+4*dimensions;
}