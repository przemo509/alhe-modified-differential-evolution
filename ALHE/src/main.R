# Czytaj README.txt
# 
# Author: Przemysław Sadło
###############################################################################

###############################################################################
# uruchomienie benchmarku, punkt startowy
###############################################################################
rm(list = ls(all = TRUE));  # upewniamy się, że nie ma żadnych śmieci (np. z poprzednich uruchomień)
# wymazane zostają również wartości zmiennych lokalnych i parametrów funkcji, dlatego nie można umieścić tego w run()
run = function(startOver = FALSE) {
    init("functions/cec2005problems.R", startOver);
    loggerCONSOLE("===== START =====\n");
    
    allRuns = length(availableFunctions) * length(availableDimensions) * howManyRuns;
    currRun = 1;
    for (functionNumber in availableFunctions) {
        initFunction(functionNumber);
        for(dims in availableDimensions) {
            initDimsSpecifics(functionNumber, dims);
            for(runNo in 1:howManyRuns) {
                if(!alreadyTested(functionNumber, dims, runNo)) {
                    oldSeed = .Random.seed;
                    loggerINFO("=====================================================================");
                    loggerINFO("== Funkcja: ", functionNumber, ", wymiarow: ", dims, ", przebieg: ", runNo, ", OFF");
                    loggerINFO("=====================================================================");
                    resultOff = testFunction(stretchingOn = FALSE);
                    
                    .Random.seed <<- oldSeed;
                    loggerINFO("=====================================================================");
                    loggerINFO("== Funkcja: ", functionNumber, ", wymiarow: ", dims, ", przebieg: ", runNo, ", ON");
                    loggerINFO("=====================================================================");
                    resultOn = testFunction(stretchingOn = TRUE);
                    
                    saveResults(functionNumber, dims, runNo, resultOff, resultOn);
                }
                loggerCONSOLE("Postep: ", 100 * currRun / allRuns, " %                             \r");
                currRun = currRun + 1;
            }
        }
    }
    
    loggerCONSOLE("\n=====  END  =====\n");
    return("OK");
}

# ładowanie potrzebnych plików i weryfikowanie ich poprawności
init = function(functionFile, startOver) {
    runif(1);                           # inicjujemy ziarno losowości (po tym dopiero utworzy się '.Random.seed')
    source("utilities/globals.R");      # pierwsze, żeby inne pliki mogły nadpisać NULLe
    source("differentialEvolution.R");
    source("utilities/logging.R");
    initLogging();
    source("utilities/resultsBuilding.R");
    source("utilities/verification.R");
    source("utilities/visualisation.R");
    source(functionFile);
    verifyFunctionToLoad();
    initResults(startOver); # musi być po verifyFunctionToLoad()
}

# inicjalizacja funkcji celu (w tym zmiennych globalnych i tego co od nich zależy, np. wykresu 2D)
initFunction = function(functionNumber) {
    loadFunction(functionNumber);
}

# inicjalizacja parametrów funkcji celu zależnych od liczby wymiarów: maxFES i optimum
initDimsSpecifics = function(functionNumber, dims) {
    dimensions <<- dims;
    P_size <<- dimensions*10;
    loadDimsSpecifics(functionNumber, dimensions);
    verifyLoadedFunction(functionNumber);
    initVisualisation();
    showFunction2D();
}

# test wydajnościowy, powstanie plik z informacją o tym ile czasu wykonują się poszczególne funckje
runProf = function() {
    
    rm(list = ls(all = TRUE));
    init("functions/cec2005problems.R", TRUE);
    loggingLevel <<- LVL_NO_LOG;
    loggerCONSOLE("\n=====  START PROF  =====\n");
    profilerDataFile = paste0(logPath, "/profilerData.txt");
    Rprof(profilerDataFile, interval = 0.001);
    
    initFunction(1);
    initDimsSpecifics(1, 50);
    testFunction(stretchingOn = FALSE);
    testFunction(stretchingOn = TRUE);
    
    Rprof(NULL); # wyłączenie profilowania
    
    loggerCONSOLE("\n=====  END PROF  =====\n");
    summaryRprof(profilerDataFile);
}