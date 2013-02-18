# Narzędzia służące do logowania błędów i informacji o postępie algorytmu.
# 
# Author: psadlo
###############################################################################


###############################################################################
# parametry określające poziom logowania
###############################################################################
LVL_NO_LOG = 0;             # nie wyświetlane są żadne rysunki, wykresy ani log na konsolę
LVL_SHOW_POPULATION = 1;    # pokazujemy wykres funkcji i populacje
LVL_DEBUG = 10;             # pokazujemy wszystkie punkty charakterystyczne
loggingLevel = LVL_NO_LOG;

logFile = "../logs/log.txt";

initLogging = function() {
    options(digits.secs = 3, width = 2000); # TODO zapewnić że się zmieści w 80 znakach + wypisywać log po kilka tys. linii na raz
    cat("", file=logFile, append=FALSE)
}

# przepisuje żywcem, bez \n na końcu itp.
loggerCONSOLE = function(...) {
    cat(..., sep="");
    flush.console();
}

loggerINFO = function(...) {
    logger("[INFO ]", ...);
}

loggerDEBUG = function(...) {
    if(loggingLevel < LVL_DEBUG) return();
    logger("[DEBUG]", ...);
}

loggerWARN = function(...) {
    logger("[WARN ]", ...);
    logger("[WARN ]", ..., logDestination=""); # na konsolę też
}

loggerERROR = function(...) {
    logger("[ERROR]", ...);
    logger("[ERROR]", ..., logDestination=""); # na konsolę też
}

logger = function(level, ..., logDestination=logFile) {
    cat(as.character.Date(Sys.time()), ' ', level, ' ', ...,
        sep="", fill=TRUE, file=logDestination, append=TRUE);
}