# Czytaj README.txt
# 
# Author: Przemo
###############################################################################
# TUTORIAL:
# http://zoonek2.free.fr/UNIX/48_R/02.html
####################################################

# procedura główna, wystarczy ją wywołać
run = function() {
	limits = generateLimits();
	populationSize = 10;
	population = init(populationSize, limits);
}


# funkcja losujaca populacje startowa
init = function(size, limits) {
    dimsCount = length(limits);
    coords = list();
	for(i in 1:dimsCount) {
        left = limits[[i]]$left;
        right = limits[[i]]$right;
		coord = runif(size, left, right);
		coords = c(coords, list(coord)); # append
	}
	population = list();
	return(population);
}

randomPointWithinLimits = function(limits) {
	point = list();
	for(limit in limits) {
		point
	}
}

###############################################################################
# koniec definicji funkcji
# poczatek komend uruchamiajacych
###############################################################################

# włączenie pozostałych plików źródłowych
# source("userDefined.r");

# wlaczanie debugowania funckji
# debug(run);
# debug(generateLimits);
# debug(init);

# uruchomienie programu
# run();
