# Narzędzia służące do logowania błędów i informacji o postępie algorytmu.
# 
# Author: psadlo
###############################################################################


###############################################################################
# parametry określające poziom logowania
###############################################################################
LVL_NO_LOG = 0;             # nie wyświetlane są żadne rysunki, wykresy ani log na konsolę ani do pliku
LVL_INFO = 1;               # tylko log o poziomie INFO do pliku, brak rysunków
LVL_SHOW_POPULATION = 2;    # pokazujemy ponadto wykres funkcji i populacje
LVL_DEBUG = 10;             # pokazujemy dodatkowo wszystkie punkty charakterystyczne dla algorytmu ewolucyjnego
loggingLevel = LVL_NO_LOG;

logPath = "../logs";
logFile = paste0(logPath, "/log.txt");

clocks = list();

initLogging = function() {
    options(digits.secs = 3, width = 200); # TODO wypisywać log po kilka tys. linii na raz jeśli profiler znajdzie tu wąskie gardło
    cat("", file = logFile, append = FALSE); # usunięcie poprzedniej zawartości
}

# procedura wołana przez inne, specjalizowane procedury logujące
logger = function(level, ..., logDestination = logFile) {
    cat(as.character.Date(Sys.time()), ' ', level, ' ', ...,
        sep = "", fill = TRUE, file = logDestination, append = TRUE);
}

# przepisuje żywcem, bez \n na końcu itp.
loggerCONSOLE = function(...) {
    cat(..., sep = "");
    flush.console();
}

loggerINFO = function(...) {
    logger("[INFO ]", ...);
}

loggerDEBUG = function(...) {
    if(loggingLevel < LVL_DEBUG){
        return();
    }
    logger("[DEBUG]", ...);
}

loggerWARN = function(...) {
    logger("[WARN ]", ...);
    logger("[WARN ]", ..., logDestination = ""); # na konsolę też
}

loggerERROR = function(...) {
    logger("[ERROR]", ...);
    logger("[ERROR]", ..., logDestination = ""); # na konsolę też
}

loggerClockStart = function(clockName, startMsg = "") {
    clocks[clockName] <<- list(Sys.time());
    if(startMsg == "") {
        return();
    }
    loggerCONSOLE(paste0(startMsg, '\n'));
    loggerINFO(startMsg);
}

loggerClockStop = function(clockName, stopMsg = "") {
    startTime = clocks[[clockName]];
    if(is.null(startTime)) {
        loggerERROR(paste0("Clock [", clockName, "] does not exists!"));
        return();
    }
    endTime = Sys.time();
    differenceInSeconds = round(as.numeric(endTime - startTime, units = "secs"), 2);
    if(stopMsg == "") {
        stopMsg = paste0("Clock [", clockName, "] stopped after ",
                differenceInSeconds, " minutes");
    } else {
        stopMsg = paste0(stopMsg, " Elapsed time: ", differenceInSeconds, " seconds.");
    }
    loggerCONSOLE(paste0(stopMsg, '\n'));
    logger("[CLOCK]", stopMsg);
    clocks[clockName] <<- NULL;
}

showProgress = function(iteration, bestPoint) {
    showPopulation();
    loggerDEBUG("It. [", iteration,
            "], Best [", P_values[bestPoint],
            "], Err [", abs(P_values[bestPoint] - optimumValue),
            "], Spread [", maxSpread(),
            "], Coords [", paste(P[bestPoint,], collapse = ", "), "]");
}