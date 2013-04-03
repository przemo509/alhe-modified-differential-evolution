# Metody służące do autoweryfikacji poprawności załadowanych funkcji testowych.
# Np. czy wartość funkcji w optimum wynosi tyle, ile podano w benchmarku CEC2005
# 
# Author: psadlo
###############################################################################



###############################################################################
# weryfikacja poprawności pliku z definicją funckji celu
###############################################################################

# sprawdzanie, czy jest w ogóle jak załadować parametry funkcji celu
verifyFunctionToLoad = function() {
    ok = TRUE;
    if(is.null(availableFunctions)) {ok = FALSE; loggerERROR("Brak parametru: 'availableFunctions'");}
    if(is.null(availableDimensions)) {ok = FALSE; loggerERROR("Brak parametru: 'availableDimensions'");}
    if(is.null(loadFunction)) {ok = FALSE; loggerERROR("Brak parametru: 'loadFunction'");}
    if(is.null(loadDimsSpecifics)) {ok = FALSE; loggerERROR("Brak parametru: 'loadDimsSpecifics'");}
    
    if(ok == FALSE) {
        msg = "Blad w pliku z zawierajacym definicje funkcji. Popraw go i uruchom skrypt ponownie.";
        loggerERROR(msg);
    }
}

# pocedura sprawdzająca poprawność pliku z definicją funkcji
# w szczególności obecność wymaganych parametrów
verifyLoadedFunction = function(functionNumber) {
    ok = TRUE;
    if(is.null(functionName)) {functionName <<- paste("Unnamed function", functionNumber); loggerWARN("Brak parametru: 'functionName'. Przyjeto: ", functionName);}
    if(is.null(better)) {ok = FALSE; loggerERROR("Brak parametru: 'better'");}
    if(is.null(limitLeft)) {ok = FALSE; loggerERROR("Brak parametru: 'limitLeft'");}
    if(is.null(limitRight)) {ok = FALSE; loggerERROR("Brak parametru: 'limitRight'");}
    if(is.null(initLimitLeft)) {ok = FALSE; loggerERROR("Brak parametru: 'initLimitLeft'");}
    if(is.null(initLimitRight)) {ok = FALSE; loggerERROR("Brak parametru: 'initLimitRight'");}
    if(is.null(optimum)) {loggerWARN("Brak parametru: 'optimum'. Wlaczam tryb wykrywania najlepszego punktu.");}
    if(is.null(optimumValue)) {loggerWARN("Brak parametru: 'optimumValue'. Wlaczam tryb wykrywania najlepszego punktu.");}
    if(is.null(fixedAccuracy)) {loggerERROR("Brak parametru: 'fixedAccuracy'.");}
    if(is.null(terErr)) {ok = FALSE; loggerERROR("Brak parametru: 'terErr'");}
    if(is.null(maxFES)) {ok = FALSE; loggerERROR("Brak parametru: 'maxFES'");}
    if(is.null(examinedFunction)) {ok = FALSE; loggerERROR("Brak parametru: 'examinedFunction'");}
    
    if(!is.null(better)) {
        if(better != "min" && better != "max") {ok = FALSE; loggerERROR("Parametr 'better' ma niedozwolona wartosc: [", better, "], dozwolone sa: [min, max]");}
    }
    if(!is.null(limitLeft) && !is.null(limitRight)) {
        if(limitLeft >= limitRight) {ok = FALSE; loggerERROR("Parametr 'limitLeft' musi byc mniejszy od 'limitRight'");}
    }
    if(!is.null(initLimitLeft) && !is.null(initLimitRight)) {
        if(initLimitLeft >= initLimitRight) {ok = FALSE; loggerERROR("Parametr 'initLimitLeft' musi byc mniejszy od 'initLimitRight'");}
    }
    if(!is.null(optimum) && !is.null(dimensions)) {
        if(length(optimum) < dimensions) {ok = FALSE; loggerERROR("Parametr 'optimum' ma mniej wymiarow niz wynosi wartosc 'dimensions'.");}
    }
    if(!is.null(examinedFunction) && !is.null(optimum) && !is.null(terErr) && !is.null(optimumValue)) {
        calculatedValue = examinedFunction(optimum); 
        if(calculatedValue < optimumValue-terErr || calculatedValue > optimumValue+terErr) {
            ok = FALSE;
            loggerERROR("Wartosc funkcji w 'optimum' nie wynosi 'optimumValue'");
        }
    }
    
    if(ok == FALSE) {
        msg = "Blad w definicji funkcji. Popraw ja i uruchom skrypt ponownie.";
        loggerERROR(msg);
        stop(paste0(msg, "\n "));
    }
}
