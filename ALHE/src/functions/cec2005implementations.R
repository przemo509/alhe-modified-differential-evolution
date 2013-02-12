# Implementacje niektórych funkcji benchmarku CEC2005 w języku R
# Ich użycie jest około 12 razy szybsze niż tych z pakietu (mimo, że są skompilowane w C)
# 
# Author: psadlo
###############################################################################

myImplementation = TRUE;

functions = function(functionNumber) {
    if(myImplementation && !is.null(functionsVect[[functionNumber]])) {
        loggerCONSOLE("MOJE\n");
        return(functionsVect[[functionNumber]]);
    } else {
        loggerCONSOLE("NIE MOJE\n");
        return(function(point) {
            return(cec2005benchmark(functionNumber, point));
        });
    }
}

f1 = function(point) {
    z = point - optimum;
    result = 0;
    for(i in 1:dimensions) {
        result = result + z[i]*z[i];
    }
    result = result + optimumValue;
    return(result);
}

f2 = function(point) {
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

f3 = function(point) {
# pomocnicza macierz M, potrzebna do obliczania wartości funkcji 3
    matrixFileName = paste("../data/elliptic_M_D", dimensions, ".txt", sep = "");
    M = matrix(scan(matrixFileName, quiet = TRUE), ncol = dimensions);
    z = t((point - optimum)[1:dimensions]%*%M);
    
    result = 0;
    for(i in 1:dimensions) {
        result = result + ((1e6)^((i-1)/(dimensions-1)))*z[i]*z[i];
    }
    result = result + optimumValue;
    return(result);
}

f4 = function(point) {
    z = point - optimum;
    
    result = 0;
    for(i in 1:dimensions) {
        partialResult = 0;
        for(j in 1:i) {
            partialResult = partialResult + z[j];
        }
        result = result + partialResult*partialResult;
    }
    result = result*(1+0.4*abs(rnorm(1)));
    result = result + optimumValue;
    return(result);
}

f6 = function(point) {
    z = point - optimum + 1;
    
    result = 0;
    for(i in 1:(dimensions-1)) {
        result = result + (100*((z[i]*z[i]-z[i+1])*(z[i]*z[i]-z[i+1]))+(z[i]-1)*(z[i]-1));
    }
    result = result + optimumValue;
    return(result);
}

f7 = function(point) {
# pomocnicza macierz M, potrzebna do obliczania wartości funkcji 7
    matrixFileName = paste("../data/griewank_M_D", dimensions, ".txt", sep = "");
    M = matrix(scan(matrixFileName, quiet = TRUE), ncol = dimensions);
    z = t((point - optimum)[1:dimensions]%*%M);
    
    suma = 0.0;
    iloczyn = 1.0;
    for(i in 1:dimensions) {
        suma = suma + z[i]*z[i]/4000;
        iloczyn = iloczyn * cos(z[i]/sqrt(i));
    }
    result = suma - iloczyn + 1 + optimumValue;
    return(result);
}

f11 = function(point) {
# pomocnicza macierz M, potrzebna do obliczania wartości funkcji 11
    matrixFileName = paste("../data/weierstrass_M_D", dimensions, ".txt", sep = "");
    M = matrix(scan(matrixFileName, quiet = TRUE), ncol = dimensions);
    z = t((point - optimum)[1:dimensions]%*%M);
    
    a=0.5;
    b=3;
    k_max=20;
    
    wynik = 0.0;
    for(i in 1:dimensions) {
        podsuma = 0.0;
        for(k in 0:k_max) {
            podsuma = podsuma + a^k*cos(2*pi*b^k*(z[i]+0.5));
        }
        wynik = wynik + podsuma;
    }
    for(k in 0:k_max) {
        wynik = wynik - dimensions*a^k*cos(2*pi*b^k*0.5);
    }
    wynik = wynik + optimumValue;
    return(wynik);
}

f5 = f8 = f9 = f10 = NULL;
functionsVect = list(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11);