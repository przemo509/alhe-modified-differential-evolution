# Algorytm ewolujci różnicowej z modyfikacja współczynnika kierunkowego.
# 
# Author: psadlo
###############################################################################



###############################################################################
# procedura główna, silnik algorytmu
###############################################################################
testFunction = function(stretchingOn) {
    currFES <<- 0;
    
    initPopulation();
    bestPointSoFar = bestFromPopulation();
    partialResult = initResultPart();
    
    iteration = 1;
    while(!stopAlgorithm(bestPointSoFar, iteration)) {
        showPopulation();
        loggerDEBUG("It. [", iteration,
                 "], Best [", bestPointSoFar$value,
                 "], Err [", errorValue(bestPointSoFar),
                 "], Spread [", maxSpread(),
                 "], Coords [", paste(bestPointSoFar$coords, collapse=", "), "]");
        
        tempP = list(); # populacja przechowująca wyniki pojedynczej iteracji (żeby nie nadpisywać w P)
        for(i in 1:populationSize) {
            x_i = population[[i]];
            x = sample(population, size = 1)[[1]]$coords;
            x_k = sample(population, size = 1)[[1]]$coords;
            x_l = sample(population, size = 1)[[1]]$coords;
            
            v_Raw = x_k - x_l;
            paramA = calculateStretching(v_Raw, stretchingOn);
            v = paramF * paramA * v_Raw;
            y = x + v;
            yFixed = applyLimitsToPoint(x, y);
            
            z = list();
            z$coords = crossOver(x_i$coords, yFixed);
            z$value = value(z$coords);
            betterPoint = tournament(x_i, z);
            tempP[[i]] = betterPoint;
            
            partialResult = buildResultPartIfNeeded(partialResult, bestPointSoFar);
            visualise(x_i$coords, x, x_k, x_l, y, yFixed, z$coords, betterPoint$coords);
        }
        population <<- tempP;
        bestPointSoFar = bestFromPopulation();
        iteration = iteration+1;
    }
    
    showPopulation();
    loggerDEBUG("It. [", iteration,
            "], Best [", bestPointSoFar$value,
            "], Err [", errorValue(bestPointSoFar),
            "], Spread [", maxSpread(),
            "], Coords [", paste(bestPointSoFar$coords, collapse=", "), "]");
    partialResult = finishResultPart(partialResult, bestPointSoFar);
    return(partialResult);
}


# funkcja losująca populację startową
# każda współrzędna ma rozkład jednostajny
initPopulation = function() {
    population <<- list();
    
    for(i in 1:populationSize) {
        point = list();
        point$coords = runif(dimensions, initLimitLeft, initLimitRight);
        point$value = value(point$coords);
        population <<- c(population, list(point));
    }
}

# funkcja zwracająca punkt z populacji mający najlepszą wartość
bestFromPopulation = function() {
    best = population[[1]];
    for(i in 1:populationSize) {
        best = tournament(best, population[[i]]);
    }
    return(best);
}

# funkcja określająca warunek stopu
stopAlgorithm = function(best, iteration) {
    # podczas debugowania zbyt długo trwa rysowanie wszystkich punktów na wykresie
#    if(loggingLevel == LVL_DEBUG && iteration > 100) return(TRUE);
    # warunek na liczbę ewaluacji funkcji celu
    loggerINFO("D=[", dimensions, "], I=[", iteration, "], FES=[", currFES, "], BEST=[",best$value, "]");
    if(currFES > maxFES) return(TRUE);
    
    if(!is.null(optimumValue)) {
        # warunek na jakość najlepszego punktu
        accuracy = errorValue(best);
        if(accuracy < terErr) {
            return(TRUE);
        } else {
            return(FALSE);
        }
    } else {
        # jeśli nie znamy najlepszego punktu (np. chcemy go dopiero wyznaczyć)
        # to można rozpatrywać skupienie populacji
        if(maxSpread() < 0.001) {
            return(TRUE); # punkty są skupione wokół jednego optimum
        } else {
            return(FALSE);
        }
    }
}

# funkcja wyznaczająca maksymalną rozpiętość wartości współrzędnej w jednym wymiarze
maxSpread = function() {
    spread = 0;
    
    for(i in 1:dimensions) {
        minCoord = maxCoord = population[[1]]$coords[i];
        for(j in 1:populationSize) {
            if(population[[j]]$coords[i] < minCoord) {
                minCoord = population[[j]]$coords[i];
            }
            if(population[[j]]$coords[i] > maxCoord) {
                maxCoord = population[[j]]$coords[i];
            }
        }
        if(maxCoord-minCoord > spread) {
            spread = maxCoord - minCoord;
        }
    }
    
    return(spread);
}

# funkcja obliczająca współczynnik skalujący "a"
calculateStretching = function(vec, stretchingOn) {
    if(!stretchingOn || vec == 0) {
        return(1);
    }
    
    d = abs(vec);
    sumOfSquares = 0;
    for(i in 1:dimensions) {
        sumOfSquares = sumOfSquares + vec[i]*vec[i];
    }
    mod_d = sqrt(sumOfSquares); # moduł wektora
    d_norm = d / mod_d; # wektor znormalizowany
    maxCoord = max(d_norm);
    a = 1 / maxCoord;
    return(a);
}

# funkcja rzutująca punkt na krawędź przestrzeni przeszukiwań metodą bisekcji
applyLimitsToPoint = function(a, b) {
    startPoint = a;
    endPoint = b;
    bisectionAccuracy = 0.01;
    
    distance = maxDistanceToLimit(endPoint);
    if(distance < 0) {
        return(endPoint); # jest wewnątrz
    }
    
    while(TRUE) {
        midPoint = (startPoint + endPoint)/2;
        
        distance = maxDistanceToLimit(midPoint);
        if(distance <= 0 && distance > -bisectionAccuracy) { # midPoint na granicy, czyli znaleźliśmy przecięcie
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
        coord = point[i];
        
        distanceRight = coord - limitRight;
        distanceLeft = limitLeft - coord;
        maxDistance = max(c(maxDistance, distanceLeft, distanceRight));
    }
    return(maxDistance);
}

# funkcja krzyżująca dwa punkty
# przepisuje z p-stwem CR współrzędną z pierwszego z nich
crossOver = function(a, b) {
    probs = runif(dimensions);
    result = c();
    for(i in 1:dimensions) {
        if(probs[i] <= paramCR) {
            result = c(result, a[i]);
        } else {
            result = c(result, b[i]);
        }
    }
    return(result);
}

# funkcja zwracająca lepszy z dwóch punktów
tournament = function(a, b) {
    if((better == "max" && a$value > b$value) || (better == "min" && a$value < b$value)) {
        return(a);
    } else {
        return(b);
    }
}

# funkcja obliczająca wartość funkcji celu dla wielu punktów na raz
# argumentem jest macierz, której jeden wiersz odpowiada jednemu punktowi,
# a kolumny kolejnym współrzędnym punktu
value = function(points) {
    result = examinedFunction(point);
    currFES <<- length(points[,1]) + 1;
    return(result);
}

# funkcja zwracająca bieżący błąd, czyli różnicę wartości między opotimum a bieżącym najleszym punktem
errorValue = function(point) {
    return(abs(point$value - optimumValue));
}

