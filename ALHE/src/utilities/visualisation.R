# Plik zawierający procedury pomocnicze służące do wizualizacji danych.
# 
# Author: psadlo
###############################################################################
# 

# zapisujemy obrazek z wykresem funkcji
drawHighResPlot = function(functionNumber) {
    plotFile = paste0(partsDir, functionNumber, "/F_", functionNumber, "_plot.png");
    if(file.exists(plotFile)) {
        loggerCONSOLE("Generowanie wysokiej rozdzielczosci wykresu dla funkcji nr ", functionNumber, " pominieto - wykres juz istnieje.\n");
        return();
    }
    loggerClockStart("HD_plot", paste0("Generowanie wysokiej rozdzielczosci wykresu dla funkcji nr ", functionNumber));
    dir.create(paste0(partsDir, functionNumber));
    require("grDevices");
    
    xResolution = 1e2;
    yResolution = 1e2;
    zResolution = 1e3; # liczba kolorów
    
    xAxis = seq(dimMin(), dimMax(), length = xResolution);
    yAxis = seq(dimMin(), dimMax(), length = yResolution);
    xCoords = rep(xAxis, times = yResolution);
    yCoords = rep(yAxis, each = xResolution);
    xyCoords = cbind(xCoords, yCoords);
    results = value(xyCoords); # funkcja celu wołana jest raz dla miliona punktów, co trwa 0.1 sekundy
    
    resultsMatrix = matrix(results, ncol = xResolution);
    png(plotFile, width = 1040, height = 1060);
    
    image(xAxis, yAxis, resultsMatrix, useRaster = TRUE,
          col = gray.colors(zResolution, start = 0, end = 1));
    contour(xAxis, yAxis, resultsMatrix, nlevels = 20, add = TRUE, labcex = 0.8);
    
    dev.off();
    
    currFES <<- 0; # zerujemy licznik wywołań funkcji, bo te powyższe nie dotyczą algorytmu
    loggerClockStop("HD_plot");
}

dimMin = function() {
    if(is.infinite(limitLeft)) {
        return(-1000);
    } else {
        return(limitLeft);
    }
}

dimMax = function() {
    if(is.infinite(limitRight)) {
        return(1000);
    } else {
        return(limitRight);
    }
}
# generujemy dane dla wykresu 2D, żeby nie obliczać tego za każdym razem, kiedy chcemy go rysować
initVisualisation = function() {
    if(loggingLevel < LVL_SHOW_POPULATION) {
        return();
    }
    
    xResolution = 1e2;
    yResolution = 1e2;
    zResolution = 1e3; # liczba kolorów
    
    xAxis = seq(dimMin(), dimMax(), length = xResolution);
    yAxis = seq(dimMin(), dimMax(), length = yResolution);
    xCoords = rep(xAxis, times = yResolution);
    yCoords = rep(yAxis, each = xResolution);
    xyCoords = cbind(xCoords, yCoords);
    
    results = value(xyCoords); # funkcja celu wołana jest raz dla miliona punktów, co trwa 0.1 sekundy
    
    resultsMatrix = matrix(results, ncol = xResolution);
    image(xAxis, yAxis, resultsMatrix, useRaster = TRUE,
            col = gray.colors(zResolution, start = 0, end = 1));
    contour(xAxis, yAxis, resultsMatrix, nlevels = 20, add = TRUE);
    functionPlot <<- recordPlot();
    
    currFES <<- 0; # zerujemy licznik wywołań funkcji, bo te powyższe nie dotyczą algorytmu
}

# procedura rysująca punkty na wykresie
# dla większej ilości wymiarów, brane są i tak tylko dwa pierwsze
# dzięki temu możemy "obejrzeć" wyniki również dla większej liczby wymiarów
showPopulation = function() {
    if(loggingLevel < LVL_SHOW_POPULATION) {
        return();
    }
    
    showFunction2D();
    points(P[,1], P[,2], pch = 16, cex = 0.4, col = "red");
}

showFunction2D = function() {
    if(loggingLevel < LVL_SHOW_POPULATION) {
        return();
    }
    
    replayPlot(functionPlot); # odtwarzamy zapisany wcześniej wykres
}

# procedura szczegółowo przedstawiająca na wykresie bieżącą sytuację
# rysuje wszystkie charakterystyczne punkty algorytmu ewolucji różnicowej
visualise = function(x_i, x, x_k, x_l, y, yFixed, z) {
    if(loggingLevel < LVL_DEBUG) return();
    
    showPopulation();
    
    showPoint(P[x_i,], "red", 15);
    showPoint(P[x_k,], "green", 15);
    showPoint(P[x_l,], "green", 15);
    showLine(P[x_l,], P[x_k,], "green");
    showPoint(P[x,], "blue");
    showLine(P[x,], y, "red");
    showPoint(y, "orange");
    showPoint(yFixed, "yellow");
    showPoint(z, "purple", 20);
    
    par(xpd = TRUE); # rysowanie poza wykresem (legenda)
    legend(dimMax()-1, dimMin()+1,
           c("xi", "x", "xk, xl", "y", "z", "better"),
           fill = c("red", "blue", "green", "orange", "purple", "cyan"));
}

# procedura wyświetlająca punkt na wykresie
# symbole używane jako punkty można sprawdzić tu:
# http://rgraphics.limnology.wisc.edu/images/miscellaneous/pch.png
showPoint = function(point, color, pointType = 19) {
    points(point[1], point[2], col = color, pch = pointType);
}

# procedura wyświetlająca linię na wykresie
showLine = function(startPoint, endPoint, color) {
    segments(startPoint[1], startPoint[2], endPoint[1], endPoint[2], col = color);
}
