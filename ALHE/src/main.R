# Czytaj README.txt
# 
# Author: Przemysław Sadło
###############################################################################
# TUTORIAL:
# http://zoonek2.free.fr/UNIX/48_R/02.html
# rm(list = ls(all = TRUE))
# TODO poprawić komentarza, w tym autor
#
##############################################################################
# parametry algorytmu ewolucyjnego
##############################################################################

# rozmiar populacji - liczba punktów
populationSize = 100;

# stała wymnażana przez wektor v
paramF = 0.9;

# stała określająca prawdopodobieństwo przepisania współrzędnej z
# nowo powstałego punktu podczas krzyżowania
paramCR = 0.9;

##############################################################################
# procedura główna, silnik algorytmu
##############################################################################
run = function() {
    verifyFunction();
    
	P = init(); # populacja startowa
    bestPointSoFar = bestFromPopulation(P);
    
    iteration = 1;
    while(!stopAlgorithm(P, bestPointSoFar, iteration)) {
        tempP = list(); # populacja przechowująca wyniki pojedynczej iteracji (żeby nie nadpisywać w P)
        for(i in 1:populationSize) {
            x_i = P[[i]];
        	x = sample(P, size = 1)[[1]];
            x_k = sample(P, size = 1)[[1]];
            x_l = sample(P, size = 1)[[1]];
            
            paramA = calculateStretching(v);
            v = paramF * paramA * (x_k - x_l);
            y = x + v;
            yFixed = applyLimitsToPoint(x, y);
            
            z = crossOver(x_i, yFixed);
            betterPoint = tournament(x_i, z);
            tempP[[i]] = betterPoint;
            
            visualise(P, iteration, x_i, x, x_k, x_l, y, yFixed, z, betterPoint);
        }
        P = tempP;
        bestPointSoFar = bestFromPopulation(P);
        iteration = iteration+1;
    }
    
    visualise(P, iteration, x_i, x, x_k, x_l, y, yFixed, z, betterPoint, forceVisualisation=TRUE);
    return(P);
}


###############################################################################

# pocedura sprawdzająca poprawność pliku z definicją funkcji
# w szczególności obecność wymaganych parametrów
verifyFunction = function() {
    ok = TRUE;
    if(!exists("better")) {ok = FALSE; print("Brak parametru: 'better'");}
    if(!exists("limitLeft")) {ok = FALSE; print("Brak parametru: 'limitLeft'");}
    if(!exists("limitRight")) {ok = FALSE; print("Brak parametru: 'limitRight'");}
    if(!exists("initLimitLeft")) {ok = FALSE; print("Brak parametru: 'initLimitLeft'");}
    if(!exists("initLimitRight")) {ok = FALSE; print("Brak parametru: 'initLimitRight'");}
    if(!exists("optimum")) {print("Brak parametru: 'optimum'. Wlaczam tryb wykrywania najlepszego punktu.");}
    if(!exists("optimumValue")) {print("Brak parametru: 'optimumValue'. Wlaczam tryb wykrywania najlepszego punktu.");}
    if(!exists("accuracy")) {ok = FALSE; print("Brak parametru: 'accuracy'");}
    if(!exists("dimensions")) {ok = FALSE; print("Brak parametru: 'dimensions'");}
    if(!exists("maxIterations")) {ok = FALSE; print("Brak parametru: 'maxIterations'");}
    if(!exists("functionName")) {ok = FALSE; print("Brak parametru: 'functionName'");}
    if(!exists("examinedFunction")) {ok = FALSE; print("Brak parametru: 'examinedFunction'");}
    
    if(exists("better")) {
        if(better != "min" && better != "max") {ok = FALSE; print(paste("Parametr 'better' ma niedozwolona wartosc: [", better, "], dozwolone sa: [min, max]"));}
    }
    if(exists("limitLeft") && exists("limitRight")) {
        if(limitLeft >= limitRight) {ok = FALSE; print("Parametr 'limitLeft' musi byc mniejszy od 'limitRight'");}
    }
    if(exists("initLimitLeft") && exists("initLimitRight")) {
        if(initLimitLeft >= initLimitRight) {ok = FALSE; print("Parametr 'initLimitLeft' musi byc mniejszy od 'initLimitRight'");}
    }
    if(exists("optimum") && exists("dimensions")) {
        if(length(optimum) > dimensions) {ok = FALSE; print("Parametr 'optimum' ma wiecej wymiarow niz wynosi wartosc 'dimensions'.");}
    }
    if(exists("examinedFunction") && exists("optimum") && exists("accuracy") && exists("optimumValue")) {
        if(examinedFunction(optimum)-accuracy < optimumValue || examinedFunction(optimum)+accuracy > optimumValue) {
            ok = FALSE;
            print("Wartosc funkcji w 'optimum' nie wynosi 'optimumValue'");
        }
    }
    
    if(ok == FALSE) {
        print("Blad w definicji funkcji. Popraw ja i uruchom skrypt ponownie.");
        quit("ask");
    }
}

# funkcja losująca populację startową
# każda współrzędna ma rozkład jednostajny
init = function() {
    population = list();
    
    for(i in 1:populationSize) {
        point = runif(dimensions, initLimitLeft, initLimitRight);
        population = c(population, list(point));
    }
    return(population);
}

# funkcja zwracająca punkt z populacji mający najlepszą wartość
bestFromPopulation = function(population) {
    best = population[[1]];
    for(i in 1:populationSize) {
        best = tournament(best, population[[i]]);
    }
    return(best);
}

# funkcja określająca warunek stopu
stopAlgorithm = function(population, best, iterationNo) {
    # warunek na liczbę iteracji
    if(iterationNo > maxIterations) return(TRUE);
    
    if(exists("optimumValue")) {
        # warunek na jakość najlepszego punktu
        bestValue = value(best);
        if(bestValue - accuracy >= optimumValue && bestValue + accuracy <= optimumValue) {
            return(TRUE);
        } else {
            return(FALSE);
        }
    } else {
        # jeśli nie znamy najlepszego punktu (np. chcemy go dopiero wyznaczyć)
        # to można rozpatrywać skupienie populacji
        if(maxSpread(population) < 0.001) {
            return(TRUE); # punkty są skupione wokół optimum
        } else {
            return(FALSE);
        }
    }
}

# funkcja wyznaczająca maksymalną rozpiętość wartości współrzędnej w jednym wymiarze
maxSpread = function(population) {
    spread = 0;
    
    for(i in 1:dimensions) {
        minCoord = maxCoord = population[[1]][[i]];
        for(j in 1:length(population)) {
            if(population[[j]][[i]] < minCoord) {
                minCoord = population[[j]][[i]];
            }
            if(population[[j]][[i]] > maxCoord) {
                maxCoord = population[[j]][[i]];
            }
        }
        if(maxCoord-minCoord > spread) {
            spread = maxCoord - minCoord;
        }
    }
    
    return(spread);
}

# funkcja obliczająca współczynnik skalujący "a"
calculateStretching = function(vec) {
    # TODO
    return(1);
}

# funkcja rzutująca punkt na krawędź przestrzeni przeszukiwań metodą bisekcji
applyLimitsToPoint = function(a, b) {
    startPoint = a;
    endPoint = b;
    bisectionAccuracy = 0.001;
    
    distance = maxDistanceToLimit(endPoint);
    if(distance < 0) return(endPoint); # jest wewnątrz
    
    while(TRUE) {
        midPoint = list();
        for(i in 1:dimensions) {
            midPoint = c(midPoint, (startPoint[[i]] + endPoint[[i]])/2);
        }
        
        distance = maxDistanceToLimit(midPoint);
        if(distance < 0 && distance > -bisectionAccuracy) { # midPoint na granicy, czyli znaleźliśmy przecięcie
            ret = midPoint;
            return(ret);
        } else if(distance > 0) { # midPoint poza przestrzenią
            endPoint = midPoint;
        } else { # midPoint "za głęboko" w przestrzeni
            startPoint = midPoint;
        }
    }
}

# funkcja obliczająca dla punktu największe wysunięcie poza przestrzeń
# ujemne oznacza położenie w granicach przestrzeni
maxDistanceToLimit = function(point) {
    maxDistance = -Inf;
    for(i in 1:dimensions) {
        coord = point[[i]];
        
        distanceRight = coord - limitRight;
        distanceLeft = limitRight - coord;
        maxDistance = max(c(maxDistance, distanceLeft, distanceRight));
    }
    return(maxDistance);
}

# funkcja krzyżująca dwa punkty
# przepisuje z p-stwem CR współrzędną z pierwszego z nich
crossOver = function(a, b) {
    return(b); # TODO
}

# funkcja zwracająca lepszy z dwóch punktów
tournament = function(a, b) {
    valueA = value(a);
    valueB = value(b);
    
    if((better == "max" && valueA > valueB) || (better == "min" && valueA < valueB)) {
        return(a);
    } else {
        return(b);
    }
}

# funkcja obliczająca wartość funkcji celu w punkcie
value = function(point) {
    result = examinedFunction(point);
    return(result);
}

########################################################################
# procedury pomocnicze, przydatne podczas debugowania i testowania

# parametry określające stopień testowania
NO_TEST = 0;				# nie wyświetane są żadne rysunki ani wykresy
SHOW_POPULATION = 1;		# pokazujemy wykres funkcji i populacje
DEBUG_ALL = 10;				# pokazujemy wszystkie punkty charakterystyczne
test = DEBUG_ALL;

# TODO sprawdzanie 2D

# procedura rysująca punkty na wykresie
# przydatna do testów
showPopulation = function(population, iteration) {
    if(dimensions == 2) {
        showPopulation2D(population);
    } else if(dimensions == 3) {
        showPopulation3D(population);
    }
    
    title(main = paste("Iteration ", iteration), font.main = 4,
          sub = paste("Max spread is ", maxSpread(population)));
}

showPopulation2D = function(population) {
    showFunction2D();
    
    x = list();
    y = list();
    for(i in 1:populationSize) {
        x = c(x, population[[i]][[1]]);
        y = c(y, population[[i]][[2]]);
    }
    
    points(x, y, pch=19);
}

showFunction2D = function() {
    xResolution = 50;
    yResolution = 50;
    zResolution = 100; # liczba kolorów
    x = seq(limitLeft, limitRight, length=xResolution);
    y = seq(limitLeft, limitRight, length=yResolution);
    
    z = outer(y, x, FUN=value);
    
    image(x, y, z, xlab='X', ylab='Y', col=gray.colors(zResolution));
    contour(x, y, z, add=TRUE);
}


showPopulation3D = function(population) {
    # TODO
}

# procedura przedstawiająca na wykresie bieżącą sytuację
visualise = function(population, iteration,
                     x_i, x, x_k, x_l, y, yFixed, z, betterPoint,
                     forceVisualisation=FALSE) {
                 
    if(test >= SHOW_POPULATION || forceVisualisation) {
        showPopulation(population, iteration);
    }
    
    if(test < DEBUG_ALL && !forceVisualisation) return();
    
    showPoint(x_i, "red");
    showPoint(x, "blue");
    showPoint(x_k, "green");
    showPoint(x_l, "green");
    showLine(x_l, x_k, "green");
    showPoint(y, "orange");
    showLine(x, y, "red");
    showLine(x, yFixed, "green");
    showPoint(z, "purple", 20);
    showPoint(betterPoint, "cyan", 20);
    
    par(xpd=TRUE); # rysowanie poza wykresem
    legend(limitRight-1, limitLeft+1,
           c("xi", "x", "xk, xl", "y", "z", "better"),
           fill=c("red", "blue", "green", "orange", "purple", "cyan"));
}

# procedura wyświetlająca punkt na wykresie
showPoint = function(point, color, pointType=19) {
    points(point[[1]], point[[2]], col=color, pch=pointType);
}

# procedura wyświetlająca linię na wykresie
showLine = function(startPoint, endPoint, color) {
    segments(startPoint[[1]], startPoint[[2]], endPoint[[1]], endPoint[[2]], col=color);
}







runStats = function() {
    iteration = 1;
    while(TRUE) {
        population = run();
        solution = bestFromPopulation(population);
        print(paste("solution nr ", iteration, " is ", solution, " and value is ", value(solution)));
#        if(solution[[1]] < 4.6 || solution[[1]] > 4.9 ||
#           solution[[2]] < 3.0 || solution[[2]] > 3.4) return();
        iteration = iteration +1;
    }
}