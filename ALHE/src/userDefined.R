# TODO: Add comment
# 
# Author: Przemo
###############################################################################


# funkcja definiujaca zakresy mozliwych wartosci dla kazdego wymiaru
generateLimits = function() {
    limits = list();
    limits = c(limits, list(list(left=-3, right=-2)));
    limits = c(limits, list(list(left=-2, right=-1)));
    limits = c(limits, list(list(left=-1, right=-0)));
    limits = c(limits, list(list(left=0, right=1)));
    limits = c(limits, list(list(left=1, right=2)));
    return(limits);
}

# funkcja losujaca populacje startowa
init = function(size, limits) {
    dimsCount = length(limits);
    population = list();
    for(j in 1:size) {
        population[[j]] = list();
    }
    for(i in 1:dimsCount) {
        left = limits[[i]]$left;
        right = limits[[i]]$right;
        coordsList = runif(size, left, right);
        
        for(j in 1:size) {
            population[[j]]$coords = c(population[[j]]$coords, list(coordsList[[j]]));
        }
    }
    return(population);
}