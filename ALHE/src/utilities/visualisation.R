# Plik zawierający procedury pomocnicze służące do wizualizacji danych.
# 
# Author: psadlo
###############################################################################
# 

# generujemy dane dla wykresu 2D, żeby nie obliczać tego za każdym razem, kiedy chcemy go rysować
initVisualisation = function() {
    if(loggingLevel < LVL_DEBUG) return();
    
    xResolution = 1e3;
    yResolution = 1e3;
    zResolution = 1e5; # liczba kolorów
    
    xAxis = seq(limitLeft, limitRight, length=xResolution);
    yAxis = seq(limitLeft, limitRight, length=yResolution);
    xCoords = rep(xAxis, times = yResolution);
    yCoords = rep(yAxis, each = xResolution);
    xyCoords = cbind(xCoords, yCoords);
    
    results = value(xyCoords); # funkcja celu wołana jest raz dla miliona punktów, co trwa 0.1 sekundy
    
    resultsMatrix = matrix(results, ncol = xResolution);
    image(xAxis, yAxis, resultsMatrix, useRaster=TRUE,
          col=gray.colors(zResolution, start = 0, end = 1));
    contour(xAxis, yAxis, resultsMatrix, nlevels = 20, add=TRUE);
    functionPlot <<- recordPlot();
    
    currFES <<- 0; # zerujemy licznik wywołań funkcji, bo te powyższe nie dotyczą algorytmu
}

# procedura rysująca punkty na wykresie
showPopulation = function() {
    if(loggingLevel < LVL_SHOW_POPULATION) return();
    
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
    if(loggingLevel < LVL_SHOW_POPULATION) return();
    
    replayPlot(functionPlot);
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
