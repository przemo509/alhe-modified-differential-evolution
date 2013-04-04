# Czytaj README.txt
# 
# Author: Przemysław Sadło
###############################################################################

###############################################################################
# uruchomienie benchmarku, punkt startowy
###############################################################################
rm(list = ls(all = TRUE));  # upewniamy się, że nie ma żadnych śmieci (np. z poprzednich uruchomień)
# wymazane zostają również wartości zmiennych lokalnych i parametrów funkcji, dlatego nie można umieścić tego w run()
run = function(functionNumbers = 1, dimensions = 2, howManyRuns = 25, startOver = FALSE, functionDefinitionFile = "cec2005problems.R") {
    cat("===== START =====\n");
    init(functionDefinitionFile);
    
    allRuns = length(functionNumbers) * length(dimensions) * howManyRuns;
    currRun = 1;
    for(dim in dimensions) {
        if(!dim %in% availableDimensions) {
            loggerERROR("Nieobslugiwana liczba wymiarow: ", dim, ". Obslugiwane sa tylko: [",
                    paste(availableDimensions, collapse = ", "), "]");
        }
        for (functionNumber in functionNumbers) {
            if(!functionNumber %in% availableFunctions) {
                loggerERROR("Nieobslugiwana funkcja: ", functionNumber, ". Obslugiwane sa tylko: [",
                        paste(availableFunctions, collapse = ", "), "]");
            }
            initFunction(functionNumber);
            initDimsSpecifics(functionNumber, dim);
            for(runNo in 1:howManyRuns) {
                if(!alreadyTested(functionNumber, dim, runNo) || startOver) {
                    oldSeed = .Random.seed;
                    loggerINFO("=====================================================================");
                    loggerINFO("== Funkcja: ", functionNumber, ", wymiarow: ", dim, ", przebieg: ", runNo, ", OFF");
                    loggerINFO("=====================================================================");
                    resultOff = testFunction(stretchingOn = FALSE);
                    
                    .Random.seed <<- oldSeed;
                    loggerINFO("=====================================================================");
                    loggerINFO("== Funkcja: ", functionNumber, ", wymiarow: ", dim, ", przebieg: ", runNo, ", ON");
                    loggerINFO("=====================================================================");
                    resultOn = testFunction(stretchingOn = TRUE);
                    
                    saveResults(functionNumber, dim, runNo, oldSeed, resultOff, resultOn);
                }
                loggerCONSOLE("Postep: ", 100 * currRun / allRuns, " %                             \r");
                currRun = currRun + 1;
            }
        }
    }
    
    loggerCONSOLE("\n=====  END  =====\n");
    mergeResultsParts(functionDefinitionFile);
    return("OK");
}

# ładowanie potrzebnych plików i weryfikowanie ich poprawności
init = function(functionFile) {
    runif(1);                           # inicjujemy ziarno losowości (po tym dopiero utworzy się '.Random.seed')
    source("utilities/logging.R");
    initLogging();
    loggerClockStart("init", "Inicjalizacja zrodel");
    source("utilities/globals.R");      # pierwsze, żeby inne pliki mogły nadpisać NULLe
    source("differentialEvolution.R");
    source("utilities/resultsBuilding.R");
    source("utilities/verification.R");
    source("utilities/visualisation.R");
    source(paste0("functions/", functionFile));
    verifyFunctionToLoad();
    loggerClockStop("init");
}

# inicjalizacja funkcji celu (w tym zmiennych globalnych i tego co od nich zależy, np. wykresu 2D)
initFunction = function(functionNumber) {
    loadFunction(functionNumber);
    drawHighResPlot(functionNumber);
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

# test wydajnościowy, powstanie plik z informacją o tym ile czasu wykonują się poszczególne funkcje
runProf = function() {
    
    rm(list = ls(all = TRUE));
    init("cec2005problems.R");
    loggingLevel <<- LVL_NO_LOG;
    loggerCONSOLE("\n=====  START PROF  =====\n");
    profilerDataFile = paste0(logPath, "/profilerData.txt");
    Rprof(profilerDataFile);
    
    initFunction(11);
    initDimsSpecifics(11, 50);
    testFunction(stretchingOn = FALSE);
    testFunction(stretchingOn = TRUE);
    
    Rprof(NULL); # wyłączenie profilowania
    
    loggerCONSOLE("\n=====  END PROF  =====\n");
    summaryRprof(profilerDataFile);
}

runDebug = function(seedFrom = c(1, 2, 1)) {
    seedFrom = as.integer(seedFrom);
    if(length(seedFrom) != 3) {
        stop("Zle dane wejsciowe. Przyklad: runDebug(c(1, 2, 1))")
    }
    
    init("cec2005problems.R");
    loggingLevel <<- LVL_NO_LOG;
    loggerCONSOLE("\n=====  START DEBUG  =====\n");
    
    functionNo = seedFrom[1];
    dim = seedFrom[2];
    runNo = seedFrom[3];
    resultPartFile = paste0(partsDir, functionNo, '/', dim, '/', runNo, ".txt");
    trySeed = scan(resultPartFile, nlines = 1, what = "list", quiet = TRUE);
    if(trySeed[1] != "seed") {
        stop(paste("Brak ziarna w pliku", resultPartFile));
    }
    oldSeed = as.integer(trySeed[-1]);
    oldResults = as.matrix(read.table(resultPartFile, skip = 1));
    initFunction(functionNo);
    initDimsSpecifics(functionNo, dim);
    runif(1);
    
    .Random.seed <<- oldSeed;
    newResultOff = testFunction(stretchingOn = FALSE);
    .Random.seed <<- oldSeed;
    newResultOn = testFunction(stretchingOn = TRUE);
    newResults = matrix(
            unlist(c(newResultOff, newResultOn)),
            ncol = length(resultFields),
            nrow = 2,
            dimnames = list(c("OFF", "ON"), resultFields),
            byrow = TRUE
    );
    if(!all.equal(oldResults, newResults)) {
        stop("Osiagnieto inne wyniki niz zapisano w pliku!");
    }
    
    loggerCONSOLE("\n=====  END DEBUG  =====\n");
}