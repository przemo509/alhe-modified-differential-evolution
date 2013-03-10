# Zbieranie wyników cząstkowych podczas pracy algorytmu i ich łączenie w jedno zestawienie na koniec.
# 
# Author: psadlo
###############################################################################
# TODO zapisywać też współrzędne punktu, przynajmniej pierwsze dwie, żeby na wykresach znaleźć, ale mogą być wszystkie

resultsDir = "../results/"; 
resultsFile = paste0(resultsDir, "completed.txt");
allRunsFile = paste0(resultsDir, "all_runs.txt");
partsDir = paste0(resultsDir, "parts/");
resultFields = c("err3",  "err4",  "err5", "errTerm", "fixedFES", "termFES", "resultX", "resultY");
finalFields = 6; # 6 to liczba parametrów, które chcemy na końcowym wyniku (excelu)
resultFieldsDetails = c("1st",  "7th",  "13th", "19th", "25th", "mean", "std");
resultFieldsDetailsNumbers = c(1, 7, 13, 19, 25);

# procedura sprawdzająca istnienie częściowych wyników
# dzięki nim możemy zatrzymywać i wznawiać testy od pewnego momentu a nie od początku
initResults = function(startOver) {
    if(!file.exists(resultsFile) || startOver) {
        file.create(resultsFile);
    }
    
    # spiszemy też wszystkie interesujące nas wyniki, można będzie taki plik przenieść na inny komputer,
    # usunąć z niego częś wpisów i uruchomic algorytm dla części danych
    cat("", file = allRunsFile);
    for (functionNumber in availableFunctions) {
        for(dims in availableDimensions) {
            for(runNo in 1:howManyRuns) {
                cat(paste(functionNumber, dims, runNo), file = allRunsFile, append = TRUE, fill = TRUE);
            }
        }
    }
}

alreadyTested = function(functionNumber, dimensions, runNo) {
    fileContent = readLines(resultsFile, warn = FALSE);
    for(row in fileContent) {
        if(row == paste(functionNumber, dimensions, runNo)) {
            return(TRUE);
        }
    }
    return(FALSE);
}

saveResults = function(functionNumber, dimensions, runNo, resultOff, resultOn) {
    dirPath = paste0(partsDir, functionNumber, '/', dimensions, '/');
    dir.create(dirPath, recursive = TRUE, showWarnings = FALSE);
    filePath = paste0(dirPath, runNo, ".txt");
    
    resultFileContent = matrix(
            c(resultOff, resultOn),
            ncol = length(resultFields),
            nrow = 2,
            dimnames = list(c("OFF", "ON"), resultFields),
            byrow = TRUE
    );
    capture.output(print(resultFileContent, print.gap=3, digits=15), file = filePath);
    
    # i jeszcze info o zapisaniu częściowego wyniku
    # TODO zapisywać raczej co zrobiono (albo też)
    cat(paste(functionNumber, dimensions, runNo), file = paste0(resultsDir, "completed.txt"),
        append = TRUE, fill = TRUE);
}


###############################################################################
# budowanie części wyniku
###############################################################################

initResultPart = function() {
    part = list();
    for(i in 1:length(resultFields)) {
        part[resultFields[i]] = 0;
    }
    part[resultFields[5]] = part[resultFields[6]] = Inf; # fixedFES i termFES
    return(part);
}

buildResultPartIfNeeded = function(partToUpdate, bestPointValue) {
    accuracy = abs(bestPointValue - optimumValue);
    
    # rejestrujemy dokładność, w trzech momentach: 1e3, 1e4 i 1e5 FES
    if(currFES == 1e3) {
        partToUpdate[resultFields[1]] = accuracy;
    } else if(currFES == 1e4) {
        partToUpdate[resultFields[2]] = accuracy;
    } else if(currFES == 1e5) {
        partToUpdate[resultFields[3]] = accuracy;
    }
    
    # rejestrujemy liczbę FES wymaganą do osiągnięcia określonej dokładności
    if(accuracy < fixedAccuracy && is.infinite(partToUpdate[resultFields[5]][[1]])) { # tylko pierwsze rejestrujemy
        partToUpdate[resultFields[5]] = currFES;
    }
    
    return(partToUpdate);
}

finishResultPart = function(partToUpdate, bestPoint) {
    partToUpdate[resultFields[4]] = abs(P_values[bestPoint] - optimumValue);
    partToUpdate[resultFields[6]] = currFES;
    partToUpdate[resultFields[7]] = P[bestPoint,1];
    partToUpdate[resultFields[8]] = P[bestPoint,2];
    return(partToUpdate);    
}

###############################################################################
# składanie wyników cząstkowych w jedną tabelę
###############################################################################
fieldNames = c("err3OFF", "err4OFF", "err5OFF", "errTermOFF", "fesOFF",
        "err3ON", "err4ON", "err5ON", "errTermON", "fesON");

mergeResultsParts = function() {
    source("utilities/globals.R");
    source("utilities/logging.R");
    initLogging();
    
    results = collectParts();
    invisible(buildExcel(results));
}

collectParts = function() {
    completedFileContent = readLines(resultsFile, warn = FALSE);
    
    dims = list();
    for(row in completedFileContent) {
        funDimRun = strsplit(row, split = " ")[[1]];
        funNo = as.integer(funDimRun[1]);
        dim = as.integer(funDimRun[2]);
        runNo = as.integer(funDimRun[3]);
        partPath = paste0(partsDir, funNo, '/', dim, '/', runNo, ".txt");
        fileData = as.matrix(read.table(partPath));
        
        if(is.null(dims[[paste0(dim)]])) {
            dims[[paste0(dim)]] = list();
        }
        if(is.null(dims[[paste0(dim)]][[paste0(funNo)]])) {
            dims[[paste0(dim)]][[paste0(funNo)]] = list();
            dims[[paste0(dim)]][[paste0(funNo)]][["OFF"]] = matrix(nrow = finalFields, ncol = howManyRuns);
            dims[[paste0(dim)]][[paste0(funNo)]][["ON"]] = matrix(nrow = finalFields, ncol = howManyRuns);
        }
        for(i in 1:finalFields) {
            dims[[paste0(dim)]][[paste0(funNo)]][["OFF"]][i, runNo] = fileData[1, resultFields[i]];
            dims[[paste0(dim)]][[paste0(funNo)]][["ON"]][i, runNo] = fileData[2, resultFields[i]];
        }
    }
    return(dims);
}

buildExcel = function(results) {
    require("xlsx");
    wb = createWorkbook();
    
    dims = names(results);
    for(dimNo in 1:length(dims)) {
        sheet = createEmptySheet(wb, dims[dimNo]);
        
        funcsNumbers = names(results[[dims[dimNo]]]);
        for(funNo in 1:length(funcsNumbers)) {
            matrixOFF = results[[dims[dimNo]]][[funcsNumbers[funNo]]][["OFF"]];
            matrixON = results[[dims[dimNo]]][[funcsNumbers[funNo]]][["ON"]];
            matrixOFF = matrixOFF[, do.call(order, lapply(1:NROW(matrixOFF), function(row) matrixOFF[row, ]))];
            matrixON = matrixON[, do.call(order, lapply(1:NROW(matrixON), function(row) matrixON[row, ]))];
            
            endRow = 3 + finalFields*(length(resultFieldsDetailsNumbers)+2) + 2;
            startColumn = 3+(funNo-1)*2;
            rows = createRow(sheet, rowIndex = 2:endRow);
            cells = createCell(rows, colIndex = startColumn:startColumn+2);
            setCellValue(cells[[1, 1]], paste0('F', funcsNumbers[funNo]));
            setCellValue(cells[[2, 1]], "OFF");
            setCellValue(cells[[2, 2]], "ON");
            for(resultField in 1:NROW(matrixOFF)) {
                howManyFields = length(resultFieldsDetailsNumbers);
                for(resultFieldDetail in 1:howManyFields) {
                    rowNo = 2 + (howManyFields + 2) * (resultField-1) + resultFieldDetail;
                    field = resultFieldsDetailsNumbers[resultFieldDetail];
                    setCellValue(cells[[rowNo, 1]], matrixOFF[resultField, field]);
                    setCellValue(cells[[rowNo, 2]], matrixON[resultField, field]);
                }
                # mean
                # std
            }
        }
    }
    
    finalResultPath = paste0(resultsDir, "finalResult.xlsx")
    saveWorkbook(wb, finalResultPath);
    fullPath = paste0(getwd(), '/', finalResultPath);
    loggerCONSOLE("Excel created: ", fullPath, '\n');
    system(paste0("open ", fullPath));
}

createEmptySheet = function(wb, dim) {
    source("functions/cec2005problems.R");
    loadDimsSpecifics(1, as.integer(dim)); # ustawia maxFES
    
    sheet = createSheet(wb, sheetName = paste0("dim_", dim));
    
    # tytuł
    addMergedRegion(sheet, 1, 1, 1, 4);
    row = createRow(sheet, rowIndex = 1);
    cell = createCell(row, colIndex = 1)[[1, 1]];
    setCellValue(cell, "Tytuł");
    
    # dim
    addMergedRegion(sheet, 2, 3, 1, 2);
    row = createRow(sheet, rowIndex = 2);
    cell = createCell(row, colIndex = 1)[[1, 1]];
    setCellValue(cell, paste0("dim_", dim, ", maxFES = ", maxFES));
    
    for(i in 1:finalFields) {
        baseRow = 3 + 1 + (i-1)*length(resultFieldsDetails);
        endRow = baseRow + length(resultFieldsDetails) - 1;
        addMergedRegion(sheet, baseRow, endRow, 1, 1);
        row = createRow(sheet, rowIndex = baseRow:endRow);
        cells = createCell(row, colIndex = 1:2);
        setCellValue(cells[[1, 1]], resultFields[i]);
        
        for(j in 1:length(resultFieldsDetails)) {
            setCellValue(cells[[j, 2]], resultFieldsDetails[j]);
        }
    }
    
    return(sheet);
}