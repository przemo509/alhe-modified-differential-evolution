# Czytaj README.txt
# 
# Author: Przemysław Sadło
###############################################################################
# TUTORIAL:
# http://zoonek2.free.fr/UNIX/48_R/02.html
# rm(list = ls(all = TRUE))
# TODO poprawić komentarz, w tym autora
#
##############################################################################
# parametry określające stopień testowania
##############################################################################
NO_TEST = 0;				# nie wyświetlane są żadne rysunki, wykresy ani log na konsolę
SHOW_POPULATION = 1;		# pokazujemy wykres funkcji i populacje
DEBUG_ALL = 10;				# pokazujemy wszystkie punkty charakterystyczne
test = SHOW_POPULATION;

# TODO sprawdzanie 2D + wygodniejsze i czytelniejsze wizualizacje + wypisywanie na konsolę
# TODO ptrzerobić na log
# TODO rozbić na pliki źródłowe


##############################################################################
# parametry algorytmu ewolucyjnego
##############################################################################

# rozmiar populacji - liczba punktów
populationSize = 20;

# stała skalująca wektor różnicowy v
paramF = 0.9;

# stała określająca prawdopodobieństwo przepisania współrzędnej z
# nowo powstałego punktu podczas krzyżowania
paramCR = 0.5; # TODO 0.9

# czy uwzględniamy dodatkowe skalowanie wektora różnicowego (zależne od jego kierunku)
stretchingOn = FALSE;

##############################################################################
# parametry funckji celu do nadpisania przez jej definicję wewnątrz procedury loadFunction()
# są to zmienne globalne, żeby uniknąć ich kopiowania (parametry funkcji są przekazywane w R przez wartość)
##############################################################################
#loadFunction - procedura ładująca pozostałe parametry testowanych funkcji
#availableFunctions - lista funkcji, na których będziemy testować algorytm
#availableDimensions - liczby wymiarów, dla których testujemy każdą z funkcji

functionName = NULL;	# nazwa funkcji, wypisujemy ją dla celów identyfikacji
examinedFunction=NULL;	# definicja funkcji poddawanej optymalizacji
better=NULL; 			# minimalizacja czy maksymalizacja funkcji ("min", "max")
limitLeft=NULL; 		# granice przestrzeni poszukiwań (dla każdego wymiaru te same)
limitRight=NULL;        # j.w.
initLimitLeft=NULL;		# fragment przestrzeni, wewnątrz którego generujemy populację startową
initLimitRight=NULL;	# j.w.
optimum=NULL;			# punkt przestrzeni, dla którego funkcja osiąga optimum
optimumValue=NULL;		# wartość funkcji w swoim optimum
terErr = NULL; 			# dopuszczalna różnica między wartością rozwiązania znalezionego przez algorytm a rzeczywistym optimum
maxFES = NULL; 			# maksymalna liczba ewaluacji funkcji celu, warunek definitywnie kończący jego pracę

##############################################################################
# zmienne wewnętrzne algorytmu, wykorzystywane w wielu miejscach, więc dla uniknięcia kopiowania wartości są one globalne
##############################################################################
dimensions = NULL;			# bierząca liczba wymiarów, po kolei z 'availableDimensions'
population = NULL;			# bierząca populacja punktów
currFES = NULL;				# liczba ewaluacji funkcji celu, nie może przekroczyć maxFES
plotData = NULL;			# dane dla wykresu 2D, żeby ich nie obliczać wielokrotnie

##############################################################################
# uruchomienie benchmarku porównania, punkt startowy
##############################################################################
run = function() {
    print("===== START =====");
    preVerifyFunction();
    
    benchmarkResults = matrix(nrow = length(availableFunctions), ncol = length(availableDimensions));
    
    for (functionNumber in availableFunctions) {
        for(dims in availableDimensions) {
            dimensions <<- dims;
            loadFunction(functionNumber, dimensions);
            verifyFunction();
            benchmarkResults[functionNumber, 1] = testFunction();
        }
    }
    
    print("===== END =====");
}

###############################################################################
# weryfikacja poprawności pliku z definicją funckji celu
###############################################################################

# sprawdzanie, czy jest w ogóle jak załadować parametry funkcji celu
preVerifyFunction = function() {
    ok = TRUE;
    if(is.null(availableFunctions)) {ok = FALSE; print("Brak parametru: 'availableFunctions'");}
    if(is.null(availableDimensions)) {ok = FALSE; print("Brak parametru: 'availableDimensions'");}
    if(is.null(loadFunction)) {ok = FALSE; print("Brak parametru: 'loadFunction'");}
    
    if(ok == FALSE) {
        print("Blad w definicji funkcji. Popraw ja i uruchom skrypt ponownie.");
        quit("ask");
    }
}

# pocedura sprawdzająca poprawność pliku z definicją funkcji
# w szczególności obecność wymaganych parametrów
verifyFunction = function() {
    ok = TRUE;
    if(is.null(functionName)) {ok = FALSE; print("Brak parametru: 'functionName'");}
    if(is.null(better)) {ok = FALSE; print("Brak parametru: 'better'");}
    if(is.null(limitLeft)) {ok = FALSE; print("Brak parametru: 'limitLeft'");}
    if(is.null(limitRight)) {ok = FALSE; print("Brak parametru: 'limitRight'");}
    if(is.null(initLimitLeft)) {ok = FALSE; print("Brak parametru: 'initLimitLeft'");}
    if(is.null(initLimitRight)) {ok = FALSE; print("Brak parametru: 'initLimitRight'");}
    if(is.null(optimum)) {print("Brak parametru: 'optimum'. Wlaczam tryb wykrywania najlepszego punktu.");}
    if(is.null(optimumValue)) {print("Brak parametru: 'optimumValue'. Wlaczam tryb wykrywania najlepszego punktu.");}
    if(is.null(terErr)) {ok = FALSE; print("Brak parametru: 'terErr'");}
    if(is.null(maxFES)) {ok = FALSE; print("Brak parametru: 'maxFES'");}
    if(is.null(examinedFunction)) {ok = FALSE; print("Brak parametru: 'examinedFunction'");}
    
    if(!is.null(better)) {
        if(better != "min" && better != "max") {ok = FALSE; print(paste("Parametr 'better' ma niedozwolona wartosc: [", better, "], dozwolone sa: [min, max]"));}
    }
    if(!is.null(limitLeft) && !is.null(limitRight)) {
        if(limitLeft >= limitRight) {ok = FALSE; print("Parametr 'limitLeft' musi byc mniejszy od 'limitRight'");}
    }
    if(!is.null(initLimitLeft) && !is.null(initLimitRight)) {
        if(initLimitLeft >= initLimitRight) {ok = FALSE; print("Parametr 'initLimitLeft' musi byc mniejszy od 'initLimitRight'");}
    }
    if(!is.null(optimum) && !is.null(dimensions)) {
        if(length(optimum) < dimensions) {ok = FALSE; print("Parametr 'optimum' ma mniej wymiarow niz wynosi wartosc 'dimensions'.");}
    }
    if(!is.null(examinedFunction) && !is.null(optimum) && !is.null(accuracy) && !is.null(optimumValue)) {
        calculatedValue = examinedFunction(optimum); 
        if(calculatedValue < optimumValue-accuracy || calculatedValue > optimumValue+accuracy) {
            ok = FALSE;
            print("Wartosc funkcji w 'optimum' nie wynosi 'optimumValue'");
        }
    }
    
    if(ok == FALSE) {
        print("Blad w definicji funkcji. Popraw ja i uruchom skrypt ponownie.");
        quit("ask");
    } else if(dimensions == 2 && test > NO_TEST) {
        generateDataForPlot2D();
    }
}

# generujemy dane dla wykresów 2D, żeby nie obliczać tego za każdym razem
generateDataForPlot2D = function() {
    xResolution = 50;
    yResolution = 50;
    x = seq(limitLeft, limitRight, length=xResolution);
    y = seq(limitLeft, limitRight, length=yResolution);
    
    z = matrix(ncol=length(x), nrow=length(y));
    for(i in 1:length(x)) {
        for(j in 1:length(y)) {
            z[i,j] = value(c(x[i], y[j]));
        }
    }
    
    currFES <<- 0; # zerujemy licznik wywołań funkcji, bo te powyższe nie dotyczą algorytmu
    
    plotData <<- list();
    plotData$x <<- x;
    plotData$y <<- y;
    plotData$z <<- z;
}

##############################################################################
# procedura główna, silnik algorytmu
##############################################################################
testFunction = function() {
    currFES <<- 0;
    
	init(); # inicjuje populację startową
    bestPointSoFar = bestFromPopulation();
    
    iteration = 1;
    while(!stopAlgorithm(bestPointSoFar)) {
        if(test >= SHOW_POPULATION) {
            showPopulation(iteration);
        }
        
        tempP = list(); # populacja przechowująca wyniki pojedynczej iteracji (żeby nie nadpisywać w P)
        for(i in 1:populationSize) {
            x_i = population[[i]];
        	x = sample(population, size = 1)[[1]]$coords;
            x_k = sample(population, size = 1)[[1]]$coords;
            x_l = sample(population, size = 1)[[1]]$coords;
            
            v_Raw = x_k - x_l;
            paramA = calculateStretching(v_Raw);
            v = paramF * paramA * v_Raw;
            y = x + v;
            yFixed = applyLimitsToPoint(x, y);
            
            z = list();
            z$coords = crossOver(x_i$coords, yFixed);
            z$value = value(z$coords);
            betterPoint = tournament(x_i, z);
            tempP[[i]] = betterPoint;
            
            if(test >= DEBUG_ALL) {
                visualise(iteration, x_i$coords, x, x_k, x_l, y, yFixed, z$coords, betterPoint$coords);
            }
                
            
        }
        population <<- tempP;
        bestPointSoFar = bestFromPopulation();
        iteration = iteration+1;
    }
    
    visualise(iteration, x_i$coords, x, x_k, x_l, y, yFixed, z$coords, betterPoint$coords);
    return(bestPointSoFar);
}


# funkcja losująca populację startową
# każda współrzędna ma rozkład jednostajny
init = function() {
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
        if(best$value < population[[i]]$value) {
            best = population[[i]];
        }
    }
    return(best);
}

# funkcja określająca warunek stopu
stopAlgorithm = function(best) {
    # warunek na liczbę ewaluacji funkcji celu
    #if(currFES > maxFES) return(TRUE);
    
    if(!is.null(optimumValue)) {
        # warunek na jakość najlepszego punktu
        if(best$value >= optimumValue-terErr && best$value <= optimumValue+terErr) {
            return(TRUE);
        } else {
            return(FALSE);
        }
    } else {
        # jeśli nie znamy najlepszego punktu (np. chcemy go dopiero wyznaczyć)
        # to można rozpatrywać skupienie populacji
        if(maxSpread() < 0.001) {
            return(TRUE); # punkty są skupione wokół optimum
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
calculateStretching = function(vec) {
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
    return(b); # TODO
}

# funkcja zwracająca lepszy z dwóch punktów
tournament = function(a, b) {
    if((better == "max" && a$value > b$value) || (better == "min" && a$value < b$value)) {
        return(a);
    } else {
        return(b);
    }
}

# funkcja obliczająca wartość funkcji celu w punkcie
value = function(point) {
    result = examinedFunction(point);
    currFES <<- currFES+1;
    return(result);
}

########################################################################
# procedury pomocnicze, przydatne podczas debugowania i testowania

# funkcja obliczająca wartość funkcji celu w punkcie (ale nie zwiększa currFES w odróżnieninu do value())
valueForTest = function(point) {
    result = examinedFunction(point);
    return(result);
}

# procedura rysująca punkty na wykresie
showPopulation = function(iteration) {
    bestPointSoFar = bestFromPopulation();
    print(paste("Iteration:", iteration, "; best value:", bestPointSoFar$value,
                    "; Accuracy is:", abs(bestPointSoFar$value-optimumValue),
                    "; Population spread is:", maxSpread(),
                    "; Best point's coordinates:"));
    print(bestPointSoFar$coords);
    
    if(dimensions == 2) {
        showPopulation2D();
    } else if(dimensions == 3) {
        showPopulation3D();
    }
    
    title(main = functionName, font.main = 4,
          sub = paste("Iteration:", iteration, "Max spread is", maxSpread()));
}

showPopulation2D = function() {
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
    zResolution = 100; # liczba kolorów
    image(plotData$x, plotData$y, plotData$z, xlab='X', ylab='Y', col=gray.colors(zResolution));
    contour(plotData$x, plotData$y, plotData$z, add=TRUE);
}


showPopulation3D = function() {
    # TODO
}

# procedura przedstawiająca na wykresie bieżącą sytuację
visualise = function(iteration, x_i, x, x_k, x_l, y, yFixed, z, betterPoint) {
    
    showPopulation(iteration);
    
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
    points(point[1], point[2], col=color, pch=pointType);
}

# procedura wyświetlająca linię na wykresie
showLine = function(startPoint, endPoint, color) {
    segments(startPoint[1], startPoint[2], endPoint[1], endPoint[2], col=color);
}







runStats = function() {
    iteration = 1;
    while(TRUE) {
        population = run();
        solution = bestFromPopulation();
        print(paste("solution nr ", iteration, " is ", solution, " and value is ", valueForTest(solution)));
#        if(solution[1] < 4.6 || solution[1] > 4.9 ||
#           solution[2] < 3.0 || solution[2] > 3.4) return();
        iteration = iteration +1;
    }
}