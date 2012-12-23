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
	population = list();
	for(i in 1:size) {
		point = apply(limits, 1, randomPointWithinLimits);
		c(population, list(point)); # append
	}
	return population;
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

# wlaczanie debugowania funckji
debug(run);
debug(generateLimits);
debug(init);

# uruchomienie programu
run();
