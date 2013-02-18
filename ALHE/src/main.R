# Czytaj README.txt
# 
# Author: Przemysław Sadło
###############################################################################

###############################################################################
# uruchomienie benchmarku, punkt startowy
###############################################################################
run = function() {
    rm(list = ls(all = TRUE));  # upewniamy się, że nie ma żadnych śmieci (np. z poprzednich uruchomień)
    							# wymazane zostają również wartości zmiennych lokalnych, dlatego nie można umieścić tego w init()
    init("functions/cec2005problems.R");
    loggerCONSOLE("===== START =====\n");
    
    allRuns = length(availableFunctions) * length(availableDimensions) * howManyRuns;
    currRun = 0;
    for (functionNumber in availableFunctions) {
        initFunction(functionNumber);
        for(dims in availableDimensions) {
            initDimsSpecifics(functionNumber, dims);
            for(runNo in 1:howManyRuns) {
                currRun = currRun + 1;
                loggerCONSOLE("Postep: ", 100 * currRun / allRuns, " %                             \r");
                if(alreadyTested(functionNumber, dims, runNo)) next;
                
                oldSeed = .Random.seed;
                loggerINFO("== Funkcja: ", functionNumber, ", wymiarow: ", dims, ", przebieg: ", runNo, ", OFF");
                resultOff = testFunction(stretchingOn=FALSE);
                
                
                .Random.seed <<- oldSeed;
                loggerINFO("== Funkcja: ", functionNumber, ", wymiarow: ", dims, ", przebieg: ", runNo, ", ON");
                resultOn = testFunction(stretchingOn=TRUE);
                
                saveResults(functionNumber, dims, runNo, resultOff, resultOn);
            }
        }
    }
    
    loggerCONSOLE("\n=====  END  =====\n");
    return("OK");
}

# ładowanie potrzebnych plików i weryfikowanie ich poprawności
init = function(functionFile) {
    runif(1);                           # inicjujemy ziarno losowości (po tym dopiero utworzy się '.Random.seed')
    source("utilities/globals.R");      # pierwsze, żeby inne pliki mogły nadpisać NULLe
    source("differentialEvolution.R");
    source("utilities/logging.R");
    initLogging();
    source("utilities/results_building.R");
    source("utilities/verification.R");
    source("utilities/visualisation.R");
    source(functionFile);
    verifyFunctionToLoad();
    initResults(); # musi być po verifyFunctionToLoad()
}

# inicjalizacja funkcji celu (w tym zmiennych globalnych i tego co od nich zależy, np. wykresu 2D)
initFunction = function(functionNumber) {
    loadFunction(functionNumber);
}

# inicjalizacja parametrów funkcji celu zależnych od liczby wymiarów: maxFES i optimum
initDimsSpecifics = function(functionNumber, dims) {
    dimensions <<- dims;
    loadDimsSpecifics(functionNumber, dimensions);
    verifyLoadedFunction(functionNumber);
    initVisualisation();
    showFunction2D();
    i=1;
}