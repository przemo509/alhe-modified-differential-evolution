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
        showPopulation(P);
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
            yFixed = applyLimitsToPoint(y, limits);
            
            z = crossOver(x_i, yFixed, paramCR);
            betterPoint = better(x_i, z);
            tempP[[i]] = betterPoint;
            
            #visualise(x_i, x, x_k, x_l, y, yFixed, z, betterPoint);
        }
        P = tempP;
        iteration = iteration+1;
    }
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
    limits = c(limits, list(list(left=-5, right=5)));
    limits = c(limits, list(list(left=-1, right=1)));
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
            population[[j]]$coords = c(population[[j]]$coords, list(coordsList[[j]]));
        }
    }
    return(population);
}

# funkcja określająca warunek stopu
stopAlgorithm = function(population, iterationNo) {
    return(iterationNo > 20); # najprymitywniejszy warunek na liczbę iteracji
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
        newVec = c(newVec, list(vec[[i]]*number));
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

# funkcja rzutująca punkt na krawędź przestrzeni przeszukiwań
applyLimitsToPoint = function(point, spaceLimits) {
    # TODO
    newPoint = list();
    for(i in 1:length(point$coords)) {
        left = spaceLimits[[i]]$left;
        right = spaceLimits[[i]]$right;
        coord = point$coords[[i]];
        if(coord < left) {
            newPoint$coords[[i]] = left;
        } else if(coord > right) {
            newPoint$coords[[i]] = right;
        } else {
            newPoint$coords[[i]] = point$coords[[i]];
        }
    }
    return(newPoint);
}

# funkcja krzyżyjąca dwa punkty
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
    return(-point$coords[[1]]-point$coords[[2]]);
}


########################################################################
# procedury pomocnicze, przydatne podczas debugowania i testowania

# stała określająca czy testujemy program
# jeśli TRUE, to m.in. wyświetlają się wykresy z populacjami
test = TRUE;

# TODO sprawdzanie 2D

# procedura rysująca punkty na wykresie
# przydatna do testów
showPopulation = function(population) {
    if(!test) return;
    
    pointsNo = length(population);
    x = list();
    y = list();
    for(i in 1:pointsNo) {
        x = c(x, list(population[[i]]$coords[[1]]));
        y = c(y, list(population[[i]]$coords[[2]]));
    }
    
    plot(x, y);
}

# procedura przedstawiająca na wykresie bieżącą sytuację
visualise = function(x_i, x, x_k, x_l, y, yFixed, z, betterPoint) {
#    xCoords = c(x_i$coords[[1]],
#            x$coords[[1]],
#            x_k$coords[[1]],
#            x_l$coords[[1]],
#            y$coords[[1]],
#            z$coords[[1]],
#            betterPoint$coords[[1]]);
#    yCoords = c(x_i$coords[[2]],
#            x$coords[[2]],
#            x_k$coords[[2]],
#            x_l$coords[[2]],
#            y$coords[[2]],
#            z$coords[[2]],
#            betterPoint$coords[[2]]);
#    labels = c("x_i",
#            "x",
#            "x_k",
#            "x_l",
#            "y",
#            "z",
#            "betterPoint");
#    text(xCoords, yCoords, labels);
    
    showPoint(x_i, "red");
    showPoint(x, "blue");
#    showPoint(x_k, "green");
#    showPoint(x_l, "green");
    showLine(x_l, x_k, "green");
    showPoint(y, "red");
    showLine(x, y, "red");
    showLine(x, yFixed, "green");
    showPoint(z, "purple");
    showPoint(betterPoint, "pink");
}

# procedura wyświetlająca punkt na wykresie
showPoint = function(point, color) {
    if(!test) return;
    points(point$coords[[1]], point$coords[[2]], col=color);
}

# procedura wyświetlająca linię na wykresie
showLine = function(startPoint, endPoint, color) {
    if(!test) return;
    segments(startPoint$coords[[1]], startPoint$coords[[2]], endPoint$coords[[1]], endPoint$coords[[2]], col=color);
}