# Zbieranie wyników cząstkowych podczas pracy algorytmu i ich łączenie w jedno zestawienie na koniec.
# 
# Author: psadlo
###############################################################################
# TODO zapisywać też współrzędne punktu, przynajmniej pierwsze dwie, żeby na wykresach znaleźć, ale mogą być wszystkie

resultsDir = "../results/"; 
resultsFile = paste0(resultsDir, "completed.txt");
allRunsFile = paste0(resultsDir, "all_runs.txt");
partsDir = paste0(resultsDir, "parts/");

# procedura sprawdzająca istnienie częściowych wyników
# dzięki nim możemy zatrzymywać i wznawiać testy od pewnego momentu a nie od początku
initResults = function() {
    if(!file.exists(resultsFile)) {
        file.create(resultsFile);
    }
    
    # spiszemy też wszystkie interesujące nas wyniki, można będzie taki plik przenieść na inny komputer,
    # usunąć z niego częś wpisów i uruchomic algorytm dla części danych
    cat("", file=allRunsFile);
    for (functionNumber in availableFunctions) {
        for(dims in availableDimensions) {
            for(runNo in 1:howManyRuns) {
                cat(paste(functionNumber, dims, runNo), file=allRunsFile, append=TRUE, fill=TRUE);
            }
        }
    }
}

alreadyTested = function(functionNumber, dimensions, runNo) {
    fileContent = readLines(resultsFile, warn=FALSE);
    for(row in fileContent) {
        if(row == paste(functionNumber, dimensions, runNo)) {
            return(TRUE);
        }
    }
    return(FALSE);
}

saveResults = function(functionNumber, dimensions, runNo, resultOff, resultOn) {
    dirPath = paste0(partsDir, functionNumber, '/', dimensions, '/');
    dir.create(dirPath, recursive=TRUE, showWarnings=FALSE);
    
    filePath = paste0(dirPath, runNo, ".txt");
    resultStringRow1 = paste(resultOff$err3, resultOff$err4, resultOff$err5, resultOff$errTerm, resultOff$fixedAccuracyFES);
    resultStringRow2 = paste(resultOn$err3,  resultOn$err4,  resultOn$err5,  resultOn$errTerm,  resultOn$fixedAccuracyFES);
    cat(paste0(resultStringRow1, '\n', resultStringRow2), file=filePath);
    
    # i jeszcze info o zapisaniu częściowego wyniku
    # TODO zapisywać raczej co zrobiono (albo też)
    cat(paste(functionNumber, dimensions, runNo), file=paste0(resultsDir, "completed.txt"),
        append=TRUE, fill=TRUE);
}


###############################################################################
# budowanie części wyniku
###############################################################################

initResultPart = function() {
    part = list();
    part$err3 = part$err4 = part$err5 = part$errTerm = 0.0;
    part$fixedAccuracyFES = Inf;
    return(part);
}

buildResultPartIfNeeded = function(partToUpdate, bestPointValue) {
    accuracy = abs(bestPointValue - optimumValue);
    
    # rejestrujemy dokładność, w trzech momentach: 1e3, 1e4 i 1e5 FES
    if(currFES == 1e3) {
        partToUpdate$err3 = accuracy;
    } else if(currFES == 1e4) {
        partToUpdate$err4 = accuracy;
    } else if(currFES == 1e5) {
        partToUpdate$err5 = accuracy;
    }
    
    # rejestrujemy liczbę FES wymaganą do osiągnięcia określonej dokładności
    if(accuracy < fixedAccuracy && is.infinite(partToUpdate$fixedAccuracyFES)) { # tylko pierwsze rejestrujemy
        partToUpdate$fixedAccuracyFES = currFES;
    }
    
    return(partToUpdate);
}

finishResultPart = function(partToUpdate, bestPointValue) {
    partToUpdate$errTerm = abs(bestPointValue - optimumValue);
    return(partToUpdate);    
}

###############################################################################
# składanie wyników cząstkowych w jedną tabelę
###############################################################################
fieldNames = c("err3OFF", "err4OFF", "err5OFF", "errTermOFF", "fesOFF",
        "err3ON", "err4ON", "err5ON", "errTermON", "fesON");

composeResultsParts = function() {
    fileContent = readLines(resultsFile, warn=FALSE);
    
    funcs = c();
    for(row in fileContent) {
        splitted = strsplit(row, split = " ")[[1]];
        funNo = splitted[1];
        dim = splitted[2];
        runNo = splitted[3];
        partPath = paste0(partsDir, funNo, '/', dim, '/', runNo, ".txt");
        partFileOFF = scan(partPath, skip = 0, nlines = 1, quiet = TRUE);
        names(partFileOFF) = fieldNames[1:5];
        partFileON = scan(partPath, skip = 1, nlines = 1, quiet = TRUE);
        names(partFileON) = fieldNames[6:10];
        partFile = c(partFileOFF, partFileON);
        
        rowName = paste0('f', funNo, 'd', dim);
        rowData = list();
        for(field in fieldNames) {
            rowData[field] = list(partFile[field]);
        }
        if(is.null(funcs[rowName][[1]])) {
            funcs = c(funcs, list(rowData));
            namesLength = length(names(funcs));
            namesLength = ifelse(namesLength==0, 1, namesLength);
            names(funcs)[namesLength] = rowName;
        } else {
            #TODO - niedokończone
#            mapply(c, first, second, SIMPLIFY=FALSE)
#            for(field in fieldNames) {
#                old = funcs[rowName][[1]][field];
#                new = rowData[field];
#                funcs[rowName][[1]][field]= c(old, list(new));   
#            }
        }
        
    }
    cat("END");
}