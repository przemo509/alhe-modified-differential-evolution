# Algorytm ewolujci różnicowej z modyfikacja współczynnika kierunkowego.
# 
# Author: psadlo
###############################################################################
# TODO komentarze do zmian macierzowych, czyli że wszędzie używamy indeksu itp


###############################################################################
# procedura główna, silnik algorytmu
###############################################################################
testFunction = function(stretchingOn) {
    currFES <<- 0;
    
    initPopulation();
    bestPointSoFar = bestFromPopulation();
    partialResult = initResultPart();
    
    iteration = 1;
    while(!stopAlgorithm(P_values[bestPointSoFar], iteration)) {
        showProgress(iteration, bestPointSoFar);
        
        zMatrix = matrix(nrow = P_size, ncol = dimensions);
        for(i in 1:P_size) {
            x = sample.int(P_size, size = 1);
            x_k = sample.int(P_size, size = 1);
            x_l = sample.int(P_size, size = 1);
            
            v_Raw = P[x_k,] - P[x_l,];
            paramA = calculateStretching(v_Raw, stretchingOn);
            v = paramF * paramA * v_Raw;
            y = P[x,] + v;
            yFixed = applyLimitsToPoint(P[x,], y);
            
            zMatrix[i,] = crossOver(P[i,], yFixed);
            visualise(i, x, x_k, x_l, y, yFixed, zMatrix[i,]);
        }
        zValues = value(zMatrix);
        
        # najlepiej zapisywać wynik od razu po value(), żeby być jak najbliżej currFes
        partialResult = buildResultPartIfNeeded(partialResult, P_values[bestPointSoFar]);
        
        if(better == "max") {
            P[zValues>P_values,] = zMatrix[zValues>P_values,];
        } else {
            P[zValues < P_values,] = zMatrix[zValues < P_values,];
        }
        
        bestPointSoFar = bestFromPopulation();
        iteration = iteration+1;
    }
    
    showProgress(iteration, bestPointSoFar);
    partialResult = finishResultPart(partialResult, P_values[bestPointSoFar]);
    return(partialResult);
}


# funkcja losująca populację startową
# każda współrzędna ma rozkład jednostajny
initPopulation = function() {
    P <<- matrix(
            runif(dimensions*P_size, initLimitLeft, initLimitRight),
            nrow = P_size,
            ncol = dimensions
    );
    P_values <<- value(P);
}

# funkcja zwracająca indeks najlepszego punktu w populacji
bestFromPopulation = function() {
    if(better == "max") {
        return(which.max(P_values));
    } else {
        return(which.min(P_values));
    }
}

# funkcja określająca warunek stopu
stopAlgorithm = function(bestValue, iteration) {
    # warunek na liczbę ewaluacji funkcji celu
    loggerINFO("D=[", dimensions, "], I=[", iteration, "], FES=[", currFES, "], BEST=[", bestValue, "]"); # TODO jakie info jest przydatne? i kiedy je wypisywać?
    if(currFES > maxFES) {
        return(TRUE);        
    }
    
    if(!is.null(optimumValue)) {
        # warunek na jakość najlepszego punktu
        accuracy = abs(bestValue - optimumValue)
        if(accuracy < terErr) {
            return(TRUE);
        } else {
            return(FALSE);
        }
    } else {
        # jeśli nie znamy najlepszego punktu (np. chcemy go dopiero wyznaczyć)
        # to można rozpatrywać skupienie populacji
        if(maxSpread() < 0.001) {
            return(TRUE); # punkty są skupione wokół jednego optimum (byćmoże lokalnego, ale trudno)
        } else {
            return(FALSE);range
        }
    }
}

# funkcja wyznaczająca maksymalną rozpiętość wartości współrzędnej w jednym wymiarze (czyli w kostce, nie w kuli)
maxSpread = function() {
    rangesByDimensions = apply(populationCoords, 2, range);                 # min i max
    spreadByDimensions = rangesByDimensions[2,] - rangesByDimensions[1,];   # max - min
    return(max(spreadByDimensions));
}

# funkcja obliczająca współczynnik skalujący "a"
calculateStretching = function(vec, stretchingOn) {
    if(!stretchingOn || vec == 0) {
        return(1);
    }
    
    d = abs(vec);
    sumOfSquares = sum(vec*vec);
    mod_d = sqrt(sumOfSquares);     # moduł wektora
    d_norm = d / mod_d;             # wektor znormalizowany
    maxCoord = max(d_norm);
    a = 1 / maxCoord;
    return(a);
}

# funkcja rzutująca punkt b na krawędź przestrzeni przeszukiwań metodą bisekcji wzdłuż kiernuku [a,b]
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
    distancesRight = point - limitRight;                    # wysunięcia poza prawe krawędzi kostki
    distancesLeft = limitLeft - point;                      # i poza lewe (też mają być dodatnie poza kostką, stąd zmiana znaku)
    return(max(max(distancesRight), max(distancesLeft)));   # zwracamy największe wysunięcie
}

# funkcja krzyżująca dwa punkty
# przepisuje z p-stwem CR współrzędną z pierwszego z nich
crossOver = function(a, b) {
    probs = runif(dimensions);
    result = a;
    result[probs>paramCR] = b[probs>paramCR];   # z punktu b brane są współrzędne z prawdopodobieństwem (1-CR)
    return(result);
}

# funkcja obliczająca wartość funkcji celu dla wielu punktów na raz
# argumentem jest macierz, której jeden wiersz odpowiada jednemu punktowi,
# a kolumny kolejnym współrzędnym punktu
value = function(points) {
    if(!is.matrix(points)) {
        msg = "Punkty musza miec postac macierzy!";
        loggerERROR(msg);
        stop(paste0(msg, "\n "));
    }
    result = examinedFunction(points);
    currFES <<- currFES + length(points[,1]);
    return(result);
}