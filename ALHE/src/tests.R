# Testy wydajnościowe, czyli o ile lepiej wypada jedno podejście od drugiego.
# 1. testCecCalls() - wołanie funckji dla parametrów przygotowanych w różny sposób
# 
# Author: Przemo
###############################################################################

library("cec2005benchmark");
source("../src/utilities/logging.R");


# Poniższy test pokazuje przewagę wywoływania funkcji celu dla wielu punktów na raz
# zamiast w pętli dla pojedynczych punktów. Dodatkowo pozytywnie na wydajność wpływa
# użycie R-owych funkcji do budowania sekwencji i macierzy zamiast budowania ich w pętli.
testCecCalls = function(){
    initLogging();
    loggerINFO("START");
    
    xLen = 1000;
    yLen = 1000;
    zLen= 100000; # liczba kolorów
    lim = 100;
    xyLen = xLen * yLen;
    x = seq(-lim, lim, length = xLen);
    y = seq(-lim, lim, length = yLen);
    loggerINFO("Sekwencje utworzone");
    
    ########################################################################
    # nie testować sposobu 1 dla przypadku 1000x1000 bo się można nie doczekać
#    loggerINFO("Sposob 1 - cec w petli");
#    z1 = matrix(ncol = xLen, nrow = yLen);
#    loggerINFO("1. Macierz utworzona - pusta");
#    for(i in 1:xLen) {
#        for(j in 1:yLen) {
#            z1[i,j] = cecValue(c(x[i], y[j]));
#        }
#    }
#    loggerINFO("1. Wynik funkcji otrzymany, od razu macierz");
#    image(x, y, z1, col = gray.colors(zLen, start = 0, end = 1), useRaster = TRUE);
#    loggerINFO("1. Wykres narysowany");
#    contour(x, y, z1, nlevels = 20, add = TRUE);
#    loggerINFO("1. Kontury narysowane");
#    loggerINFO("Koniec sposobu 1");
    ########################################################################
    
    ########################################################################
    loggerINFO("Sposob 2 - macierz w petli");
    z2 = matrix(ncol = 2, nrow = xyLen);
    for(i in 1:xyLen) {
        xi = (i-1)%%xLen+1;
        yj = (i-1)%/%xLen + 1;
        z2[i,] = c(x[xi], y[yj]);rep
    }
    loggerINFO("2. Macierz wypelniona w petli");
    res2 = cecValue(z2);
    loggerINFO("2. Wynik funkcji otrzymany");
    resM2 = matrix(res2, ncol = xLen);
    loggerINFO("2. Wynik funkcji przeksztalcony na macierz");
    image(x, y, resM2, col = gray.colors(zLen, start = 0, end = 1), useRaster = TRUE);
    loggerINFO("2. Wykres narysowany");
    contour(x, y, resM2, nlevels = 20, add = TRUE);
    loggerINFO("2. Kontury narysowane");
    loggerINFO("Koniec sposobu 2");
    ########################################################################
    
    ########################################################################
    loggerINFO("Sposob 3 - brak petli");
    xx = rep(x, times = yLen);
    yy = rep(y, each = xLen);
    z3 = cbind(xx, yy);
    loggerINFO("3. Macierz wypelniona rep() + cbind()");
    res3 = cecValue(z3);
    loggerINFO("3. Wynik funkcji otrzymany");
    resM3 = matrix(res3, ncol = xLen);
    loggerINFO("3. Wynik funkcji przeksztalcony na macierz");
    image(x, y, resM3, col = gray.colors(zLen, start = 0, end = 1), useRaster = TRUE);
    loggerINFO("3. Wykres narysowany");
    contour(x, y, resM3, nlevels = 20, add = TRUE);
    loggerINFO("3. Kontury narysowane");
    loggerINFO("Koniec sposobu 3");
    ########################################################################
    
    ########################################################################
    loggerINFO("Zapisywanie i odtwarzanie wykresu");
    savedPlot = recordPlot();
    loggerINFO("Wykres zapisany");
    replayPlot(savedPlot);
    loggerINFO("Wykres odtworzony");
    ########################################################################
    
    loggerINFO("KONIEC");
    return();
}

cecValue = function(point) {
    res = cec2005benchmark(1, point);
    return(res);
}