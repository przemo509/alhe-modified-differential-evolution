# Czytaj README.txt
# 
# Author: Przemo
###############################################################################
# TUTORIAL:
# http://zoonek2.free.fr/UNIX/48_R/02.html
####################################################

# procedura główna, silnik algorytmu
run = function() {
	limits = generateLimits();
	P = init(limits); # populacja startowa
    
    iteration = 1;
    while(!stopAlgorithm(P, iteration)) {
        tempP = list(); # populacja przechowująca wyniki pojedynczej iteracji (żeby nie nadpisywać w P)
        for(i in 1:length(P)) {
            x_i = P[[i]];
        	x = sample(P, size = 1)[[1]];
            x_k = sample(P, size = 1)[[1]];
            x_l = sample(P, size = 1)[[1]];
            
            v = pointsDifference(x_k, x_l);
            vF = multiply(v, paramF);
            paramA = calculateStretching(vF);
            vFA = multiply(vF, paramA);
            y = movePoint(x, vFA);
            yFixed = applyLimitsToPoint(x, y, limits);
            
            z = crossOver(x_i, yFixed, paramCR);
            betterPoint = better(x_i, z);
            tempP[[i]] = betterPoint;
            
            visualise(P, limits, iteration, x_i, x, x_k, x_l, y, yFixed, z, betterPoint);
        }
        P = tempP;
        iteration = iteration+1;
    }
    
    visualise(P, limits, iteration, x_i, x, x_k, x_l, y, yFixed, z, betterPoint, TRUE);
    return(P);
}


###############################################################################

# stała wymnażana przez wektor v
paramF = 0.9;

# stała określająca prawdopodobieństwo przepisania współrzędnej z
# nowo powstałego punktu podczas krzyżowania
paramCR = 0.9;

# funkcja definiujaca zakresy możliwych wartości dla każdego wymiaru
# jednocześnie definiuje liczbę wymiarów: length(limits)
generateLimits = function() {
    limits = list();
    limits = c(limits, list(list(left=-6, right=6)));
    limits = c(limits, list(list(left=-6, right=6)));
    return(limits);
}

# funkcja losująca populację startową
# każda współrzędna ma rozkład jednostajny
init = function(limits) {
    populationSize = 100;
    dimsCount = length(limits);
    
    population = list();
    for(j in 1:populationSize) {
        population[[j]] = list(); # rezerwujemy miejsce na punkty
    }
    
    for(i in 1:dimsCount) {
        left = limits[[i]]$left;
        right = limits[[i]]$right;
        # dla każdego wymiaru od razu losujemy współrzędne dla wszystkich punktów
        coordsList = runif(populationSize, left, right);
        
        # budujemy wszystkie punkty na raz (j), współrzędna po współrzędnej (i)
        for(j in 1:populationSize) {
            population[[j]]$coords = c(population[[j]]$coords, coordsList[[j]]);
        }
    }
    return(population);
}

# funkcja określająca warunek stopu
stopAlgorithm = function(population, iterationNo) {
    if(iterationNo > 2000) return(TRUE); # górne ograniczenie na liczbę iteracji
    
    if(maxSpread(population) < 0.01) {
        return(TRUE); # punkty są skupione wokół maksimum
    } else {
        return(FALSE);
    }
}

maxSpread = function(population) {
    spread = 0;
    
    dimsCount = length(population[[1]]$coords);
    for(i in 1:dimsCount) {
        minCoord = maxCoord = population[[1]]$coords[[i]];
        for(j in 1:length(population)) {
            if(population[[j]]$coords[[i]] < minCoord) {
                minCoord = population[[j]]$coords[[i]];
            }
            if(population[[j]]$coords[[i]] > maxCoord) {
                maxCoord = population[[j]]$coords[[i]];
            }
        }
        if(maxCoord-minCoord > spread) {
            spread = maxCoord - minCoord;
        }
    }
    
    return(spread);
}

# funckja zwracająca wektor będący różnicą punktów
pointsDifference = function(a, b) {
    if(length(a$coords) != length(b$coords)) return(list());
    
    # d = a-b
    d = list();
    for(i in 1:length(a$coords)) {
        d = c(d, a$coords[[i]] - b$coords[[i]])
    }
    return(d);
}

# funckja mnożąca wektor przez stałą liczbową
multiply = function(vec, number) {
    newVec = list();
    for(i in 1:length(vec)) {
        newVec = c(newVec, vec[[i]]*number);
    }
    return(newVec);
}

# funkcja obliczająca współczynnik skalujący "a"
calculateStretching = function(vec) {
    # TODO
    return(1);
}

# funkcja przesuwająca punkt o zadany wektor
movePoint = function(point, vec) {
    if(length(point$coords) != length(vec)) return(list());
    
    newPoint = list();
    for(i in 1:length(vec)) {
        newPoint$coords[[i]] = point$coords[[i]]+vec[[i]];
    }
    return(newPoint);
}

# funkcja rzutująca punkt na krawędź przestrzeni przeszukiwań metodą bisekcji
applyLimitsToPoint = function(a, b, spaceLimits) {
    startPoint = a;
    endPoint = b;
    accuracy = 0.001;
    
    distance = maxDistanceToLimit(endPoint, spaceLimits);
    if(distance < 0) return(endPoint); # jest wewnątrz
    
    while(TRUE) {
        midPoint = list();
        for(i in 1:length(spaceLimits)) {
            midPoint$coords = c(midPoint$coords, (startPoint$coords[[i]] + endPoint$coords[[i]])/2);
        }
        
        distance = maxDistanceToLimit(midPoint, spaceLimits);
        if(distance < 0 && distance > -accuracy) { # midPoint na granicy, czyli znaleźliśmy przecięcie
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
maxDistanceToLimit = function(point, spaceLimits) {
    maxDistance = -Inf;
    for(i in 1:length(spaceLimits)) {
        coord = point$coords[[i]];
        leftLimit = spaceLimits[[i]]$left;
        rightLimit = spaceLimits[[i]]$right;
        
        distanceRight = coord - rightLimit;
        distanceLeft = leftLimit - coord;
        maxDistance = max(c(maxDistance, distanceLeft, distanceRight));
    }
    return(maxDistance);
}

# funkcja krzyżująca dwa punkty
# przepisuje z p-stwem CR współrzędną z pierwszego z nich
crossOver = function(a, b, CR) {
    return(b); # TODO
}

# funkcja zwracająca lepszy z dwóch punktów
better = function(a, b) {
    # TODO
    valueA = value(a);
    valueB = value(b);
    if(valueA > valueB) {
        return(a);
    } else {
        return(b);
    }
}

#funkcja obliczająca wartość funkcji celu w punkcie
value = function(point) {
    x = point$coords[[1]];
    y = point$coords[[2]];
    return(value2D(x, y));
}

value2D = function(x, y) {
    z = sin(x)*cos(y)+0.001*x+0.001*y;
    return(z);
}

value3D = function(x, y, z) {
    t = 1; # TODO
    return(t);
}


########################################################################
# procedury pomocnicze, przydatne podczas debugowania i testowania

# parametry określające stopień testowania
NO_TEST = 0;				# nie wyświetane są żadne rysunki ani wykresy
SHOW_POPULATION = 1;		# pokazujemy wykres funkcji i populacje
DEBUG_ALL = 10;				# pokazujemy wszystkie punkty charakterystyczne
test = NO_TEST;

# TODO sprawdzanie 2D

# procedura rysująca punkty na wykresie
# przydatna do testów
showPopulation = function(population, spaceLimits, iteration) {
    if(length(population) == 0) return();
    
    if(length(population[[1]]$coords) == 2) {
        showPopulation2D(population, spaceLimits, value2D);
    } else if(length(population[[1]]$coords) == 3) {
        showPopulation3D(population, spaceLimits, value3D);
    }
    
    title(main = paste("Iteration ", iteration), font.main = 4,
          sub = paste("Max spread is ", maxSpread(population)));
}

showPopulation2D = function(population, spaceLimits, valueFunction) {
    showFunction2D(spaceLimits, valueFunction);
    
    pointsNo = length(population);
    x = list();
    y = list();
    for(i in 1:pointsNo) {
        x = c(x, population[[i]]$coords[[1]]);
        y = c(y, population[[i]]$coords[[2]]);
    }
    
    points(x, y, pch=19);
}

showFunction2D = function(spaceLimits, valueFunction) {
    xResolution = 50;
    yResolution = 50;
    zResolution = 100; # liczba kolorów
    x = seq(spaceLimits[[1]]$left, spaceLimits[[1]]$right, length=xResolution);
    y = seq(spaceLimits[[2]]$left, spaceLimits[[2]]$right, length=yResolution);
    
    z = outer(y, x, FUN=valueFunction);
    
    image(x, y, z, xlab='X', ylab='Y', col=gray.colors(zResolution));
    contour(x, y, z, add=TRUE);
}


showPopulation3D = function(population, spaceLimits, valueFunction) {
    # TODO
}

# procedura przedstawiająca na wykresie bieżącą sytuację
visualise = function(population, spaceLimits, iteration,
                     x_i, x, x_k, x_l, y, yFixed, z, betterPoint,
                     forceVisualisation=FALSE) {
                 
    if(test >= SHOW_POPULATION || forceVisualisation) {
        showPopulation(population, spaceLimits, iteration);
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
    legend(spaceLimits[[1]]$right-1, spaceLimits[[2]]$left+1,
           c("xi", "x", "xk, xl", "y", "z", "better"),
           fill=c("red", "blue", "green", "orange", "purple", "cyan"));
}

# procedura wyświetlająca punkt na wykresie
showPoint = function(point, color, pointType=19) {
    points(point$coords[[1]], point$coords[[2]], col=color, pch=pointType);
}

# procedura wyświetlająca linię na wykresie
showLine = function(startPoint, endPoint, color) {
    segments(startPoint$coords[[1]], startPoint$coords[[2]], endPoint$coords[[1]], endPoint$coords[[2]], col=color);
}







runUnusual = function() {
    iteration = 1;
    while(TRUE) {
        population = run();
        solution = average(population);
        print(paste("solution nr ", iteration, " is ", solution, " and value is ", value(solution)));
        if(solution$coords[[1]] < 4.7 || solution$coords[[1]] > 4.8 ||
           solution$coords[[2]] < 3.1 || solution$coords[[2]] > 3.2) return();
        iteration = iteration +1;
    }
}

average = function(population) {
    avgPoint = list();
    for(i in 1:length(population[[1]]$coords)) {
        coordsSum = 0;
        for(j in 1:length(population)) {
            coordsSum = coordsSum + population[[j]]$coords[[i]];
        }
        
        avgCoord = coordsSum / length(population);
        avgPoint$coords = c(avgPoint$coords, avgCoord);
    }
    return(avgPoint);
}