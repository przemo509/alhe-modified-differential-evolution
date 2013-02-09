# Czytaj README.txt
# 
# Author: Przemysław Sadło
###############################################################################
# TUTORIAL:
# http://zoonek2.free.fr/UNIX/48_R/02.html
# rm(list = ls(all = TRUE))
# TODO poprawić komentarze, w tym autora, poprawić readme
#
# TODO sprawdzanie 2D + wygodniejsze i czytelniejsze wizualizacje + wypisywanie na konsolę

###############################################################################
# uruchomienie benchmarku, punkt startowy
###############################################################################
run = function() {
    print("===== START =====");
    shell("rm ../results/aa.txt");
    init("functions/cec2005problems.R");
    
    benchmarkResults = list();
    
    for (functionNumber in availableFunctions) {
        for(dims in availableDimensions) {
            initFunction(functionNumber, dims);
            benchmarkResults = testFunction();
        }
    }
    
    print("===== END =====");
}

# ładowanie potrzebnych plików i weryfikowanie ich poprawności
init = function(functionFile) {
    source("differentialEvolution.R");
    source("globals.R");
    source("logging.R");
    initLogging();
    source("verification.R");
    source("visualisation.R");
    source(functionFile); # ostatnie, bo nadpisuje kilka zmiennych globalnych
    verifyFunctionToLoad();
}

# inicjalizacja funkcji celu (w tym zmiennych globalnych i tego co od nich zależy, np. wykresu 2D)
initFunction = function(functionNumber, dims) {
    dimensions <<- dims;
    loadFunction(functionNumber, dimensions);
    verifyLoadedFunction();
    initVisualisation();
}