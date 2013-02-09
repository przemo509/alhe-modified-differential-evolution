# TODO: Add comment
# 
# Author: psadlo
###############################################################################


###############################################################################
# parametry określające poziom logowania
###############################################################################
NO_TEST = 0;				# nie wyświetlane są żadne rysunki, wykresy ani log na konsolę
SHOW_POPULATION = 1;		# pokazujemy wykres funkcji i populacje
DEBUG_ALL = 10;				# pokazujemy wszystkie punkty charakterystyczne
test = SHOW_POPULATION;

logFile = "../logs/log.txt";

initLogging = function() {
    options(digits.secs = 3, width = 2000);
    cat("", file=logFile, append=FALSE)
}

loggerINFO = function(...) {
    logger("[INFO]", ...);
}

loggerDEBUG = function(...) {
    logger("[DEBUG]", ...);
}

loggerERROR = function(...) {
    logger("[ERROR]", ...);
}

logger = function(level, ...) {
    cat(as.character.Date(Sys.time()), ' ', level, ' ', ...,
        sep="", fill=TRUE, file=logFile, append=TRUE);
}