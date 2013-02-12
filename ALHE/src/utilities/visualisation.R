# Plik zawierający procedury pomocnicze służące do wizualizacji danych.
# 
# Author: psadlo
###############################################################################
# 

initVisualisation = function() {
    initDataForPlot2D();
}
# generujemy dane dla wykresu 2D, żeby nie obliczać tego za każdym razem, kiedy chcemy go rysować
initDataForPlot2D = function() {
    if(loggingLevel < LVL_DEBUG) return();
    
    xResolution = 50;
    yResolution = 50;
    x = seq(limitLeft, limitRight, length=xResolution);
    y = seq(limitLeft, limitRight, length=yResolution);
    
    z = matrix(ncol=length(x), nrow=length(y));
    loggerINFO("Generowanie danych do wykresu. Ten proces może potrwać parę sekund...");
    for(i in 1:length(x)) {
        for(j in 1:length(y)) {
            z[i,j] = value(c(x[i], y[j]));
        }
    }
    loggerINFO("Generowanie danych do wykresu - KONIEC.");
    
    currFES <<- 0; # zerujemy licznik wywołań funkcji, bo te powyższe nie dotyczą algorytmu
    
    plotData <<- list();
    plotData$x <<- x;
    plotData$y <<- y;
    plotData$z <<- z;
}

# procedura rysująca punkty na wykresie
showPopulation = function() {
    if(loggingLevel < LVL_DEBUG) return();
    
    showFunction2D();
    
    x = list();
    y = list();
    for(i in 1:populationSize) {
        x = c(x, population[[i]]$coords[1]);
        y = c(y, population[[i]]$coords[2]);
    }
    
    points(x, y, pch=19);
}

showFunction2D = function() {
    if(loggingLevel < LVL_DEBUG) return();
    
    zResolution = 100; # liczba kolorów
    # TODO robić to tylko raz dla funkcji
    image(plotData$x, plotData$y, plotData$z, xlab='X', ylab='Y', col=gray.colors(zResolution), useRaster=TRUE);
    contour(plotData$x, plotData$y, plotData$z, add=TRUE);
}

# procedura szczegółowo przedstawiająca na wykresie bieżącą sytuację
# rysuje wszystkie charakterystyczne punkty algorytmu ewolucji różnicowej
visualise = function(x_i, x, x_k, x_l, y, yFixed, z, betterPoint) {
    if(loggingLevel < LVL_DEBUG) return();
    
    showPopulation();
    
    showPoint(x_i, "red", 15);
    showPoint(x_k, "green", 15);
    showPoint(x_l, "green", 15);
    showLine(x_l, x_k, "green");
    showPoint(x, "blue");
    showLine(x, y, "red");
    showPoint(y, "orange");
    showPoint(yFixed, "yellow");
    showPoint(z, "purple", 20);
    showPoint(betterPoint, "cyan", 4);
    
    par(xpd=TRUE); # rysowanie poza wykresem (legenda)
    legend(limitRight-1, limitLeft+1,
            c("xi", "x", "xk, xl", "y", "z", "better"),
            fill=c("red", "blue", "green", "orange", "purple", "cyan"));
}

# procedura wyświetlająca punkt na wykresie
# symbole używane jako punkty można sprawdzić tu:
# http://rgraphics.limnology.wisc.edu/images/miscellaneous/pch.png
showPoint = function(point, color, pointType=19) {
    points(point[1], point[2], col=color, pch=pointType);
}

# procedura wyświetlająca linię na wykresie
showLine = function(startPoint, endPoint, color) {
    segments(startPoint[1], startPoint[2], endPoint[1], endPoint[2], col=color);
}
