# Zbieranie wyników cząstkowych podczas pracy algorytmu i ich łączenie w jedno zestawienie na koniec.
# 
# Author: psadlo
###############################################################################
# TODO zapisywać też współrzędne punktu, przynajmniej pierwsze dwie, żeby na wykresach znaleźć, ale mogą być wszystkie

resultsDir = "../results/"; 
resultsFile = paste0(resultsDir, "completed.txt");
partsDir = paste0(resultsDir, "parts/");
resultFields = c("err3",  "err4",  "err5", "errTerm", "fixedFES", "termFES", "resultX", "resultY");
finalFields = 6; # 6 to liczba parametrów, które chcemy na końcowym wyniku (excelu), np. nie chcemy resultX ani resultY
resultFieldsDetails = c("1st (Best)",  "7th",  "13th (Median)", "19th", "25th (Worst)", "mean", "std");
resultFieldsDetailsNumbers = c(1, 7, 13, 19, 25);

# procedura sprawdzająca istnienie częściowych wyników
initResults = function(startOver) {
    if(!file.exists(resultsFile) || startOver) {
        file.create(resultsFile);
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

saveResults = function(functionNumber, dimensions, runNo, seed, resultOff, resultOn) {
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
    cat("seed", seed, '\n', file = filePath);
    capture.output(print(resultFileContent, print.gap=3, digits=15), file = filePath, append = TRUE);
    
    # i jeszcze info o zapisaniu częściowego wyniku
    # TODO zapisywać raczej co zrobiono (albo też)
    cat(paste(functionNumber, dimensions, runNo), file = resultsFile, append = TRUE, fill = TRUE);
}


###############################################################################
# budowanie części wyniku
###############################################################################

initResultPart = function() {
    part = list();
    for(i in 1:length(resultFields)) {
        part[resultFields[i]] = 0;
    }
    part[resultFields[5]] = part[resultFields[6]] = maxFES; # fixedFES i termFES
    return(part);
}

buildResultPartIfNeeded = function(partToUpdate, bestPointValue) {
    accuracy = abs(bestPointValue - optimumValue);
    
    # rejestrujemy dokładność, w trzech momentach: 1e3, 1e4 i 1e5 FES
    if(currFES >= 1e3 && partToUpdate[resultFields[1]] == 0) {
        partToUpdate[resultFields[1]] = accuracy;
    } else if(currFES >= 1e4 && partToUpdate[resultFields[2]] == 0) {
        partToUpdate[resultFields[2]] = accuracy;
    } else if(currFES >= 1e5 && partToUpdate[resultFields[3]] == 0) {
        partToUpdate[resultFields[3]] = accuracy;
    }
    
    # rejestrujemy liczbę FES wymaganą do osiągnięcia określonej dokładności
    if(accuracy < fixedAccuracy) {
        if(partToUpdate[resultFields[5]] == maxFES) {
            partToUpdate[resultFields[5]] = currFES;
        }
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
    
    checkRJava();
    results = collectParts();
    invisible(buildExcel(results));
}

checkRJava = function() {
    oldPath = Sys.getenv("PATH");
    javaHome = Sys.getenv("JAVA_HOME");
    newPath = paste0(oldPath, ';', javaHome, "\\jre\\bin\\server"); # ścieżka do jvm.dll
    Sys.setenv(PATH = newPath);
    
    if(newPath != Sys.getenv("PATH")) {
        loggerERROR("Nie udało się ustawić ścieżki do jvm.dll");
    }
}

xtfrm.runs_lines = function(a) {
    splitted = strsplit(a, " ");
    numbers = sapply(splitted, myStringToNumber);
    return(numbers);
}

myStringToNumber = function(line) {
    numbers = as.integer(line);
    result = 1e6 * numbers[1] + 1e3 * numbers[2] + 1e0 * numbers[3];
    return(result);
}

collectParts = function() {
    completedFileContent = readLines(resultsFile, warn = FALSE);
    class(completedFileContent) = "runs_lines";
    sortedContent = sort(completedFileContent);
    
    dims = list();
    for(row in sortedContent) {
        funDimRun = strsplit(row, split = " ")[[1]];
        funNo = as.integer(funDimRun[1]);
        dim = as.integer(funDimRun[2]);
        runNo = as.integer(funDimRun[3]);
        partPath = paste0(partsDir, funNo, '/', dim, '/', runNo, ".txt");
        fileData = as.matrix(read.table(partPath, skip = 1));
        
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
        sheet = createEmptySheet(wb, as.integer(dims[dimNo]));
        
        funcsNumbers = names(results[[dims[dimNo]]]);
        for(funNo in 1:length(funcsNumbers)) {
            matrixOFF = results[[dims[dimNo]]][[funcsNumbers[funNo]]][["OFF"]];
            matrixON = results[[dims[dimNo]]][[funcsNumbers[funNo]]][["ON"]];
            matrixOFF = t(apply(matrixOFF, 1, sort));
            matrixON = t(apply(matrixON, 1, sort));
            
            startRow = 2;
            rowsCount = 2 + finalFields*(length(resultFieldsDetailsNumbers)+2) + 2;
            startColumn = 3+(funNo-1)*2;
            addMergedRegion(sheet, 2, 2, startColumn, startColumn + 1);
            cells = CellBlock(sheet, startRow, startColumn, rowsCount, 2);
            CB.setBorder(cells, Border(position = "RIGHT"), 2:rowsCount, 2);
            CB.setColData(cells, paste0('F', funcsNumbers[funNo]), 1,
                    colStyle = CellStyle(wb) + 
                            Alignment(horizontal = "ALIGN_CENTER") + 
                            Font(wb, isBold = TRUE) +
                            Border(position = "TOP"));
            CB.setBorder(cells, Border(position = c("RIGHT", "TOP")), 1, 2);
            CB.setColData(cells, "OFF", 1, 1,
                    colStyle = CellStyle(wb) + 
                            Alignment(horizontal = "ALIGN_CENTER") + 
                            Font(wb, isBold = TRUE) +
                            Border(position = c("RIGHT", "TOP")));
            CB.setColData(cells, "ON", 2, 1,
                    colStyle = CellStyle(wb) + 
                            Alignment(horizontal = "ALIGN_CENTER") + 
                            Font(wb, isBold = TRUE) +
                            Border(position = c("RIGHT", "TOP")));
            
            for(resultField in 1:NROW(matrixOFF)) {
                howManyFields = length(resultFieldsDetailsNumbers);
                CB.setBorder(cells, Border(position = "TOP"), 2+(howManyFields+2)*(resultField-1)+1, c(1, 2));
                for(resultFieldDetail in 1:howManyFields) {
                    rowNo = 2 + (howManyFields + 2) * (resultField-1) + resultFieldDetail - 1;
                    field = resultFieldsDetailsNumbers[resultFieldDetail];
                    
                    value = matrixOFF[resultField, field];
                    CB.setColData(cells, value, 1, rowNo);
                    if(resultFields[resultField] == "fixedFES" && value >= maxFES) {
                        createCellComment(getCells(getRows(sheet, startRow + rowNo), startColumn)[[1]],
                                string = "Nie osiągnięto 'fixedAccuracy'");
                    } else if(resultFields[resultField] == "termFES" && value >= maxFES) {
                        createCellComment(getCells(getRows(sheet, startRow + rowNo), startColumn)[[1]],
                                string = "Nie osiągnięto 'terErr'");
                    }
                    
                    value = matrixON[resultField, field];
                    if(resultFields[resultField] == "fixedFES" && value >= maxFES) {
                        createCellComment(getCells(getRows(sheet, startRow + rowNo), startColumn + 1)[[1]],
                                string = "Nie osiągnięto 'fixedAccuracy'");
                    } else if(resultFields[resultField] == "termFES" && value >= maxFES) {
                        createCellComment(getCells(getRows(sheet, startRow + rowNo), startColumn + 1)[[1]],
                                string = "Nie osiągnięto 'terErr'");
                    }
                    CB.setColData(cells, value, 2, rowNo);
                }
                CB.setColData(cells, mean(matrixOFF[resultField, ]), 1, rowNo+1);
                CB.setColData(cells, mean(matrixON[resultField, ]), 2, rowNo+1);
                CB.setColData(cells, sd(matrixOFF[resultField, ]), 1, rowNo+2);
                CB.setColData(cells, sd(matrixON[resultField, ]), 2, rowNo+2);
                CB.setFill(cells, Fill(foregroundColor = "gray80"), c(rowNo+1, rowNo+1, rowNo+2, rowNo+2)+1, c(1, 2, 1, 2));
            }
            
            rowNo = 3 + length(resultFieldsDetails)*finalFields + 1;
            cells = CellBlock(sheet, rowNo, startColumn, 2, 2);
            fffNo = match("fixedFES", resultFields); #fixedFESFieldNo
            
            successFES = matrixOFF[fffNo, matrixOFF[fffNo, ] < maxFES];
            successCount = length(successFES);
            successRate = successCount / howManyRuns;
            if(successCount == 0) {
                successPerformance = "-";
            } else {
                successPerformance = mean(successFES) * howManyRuns / successCount;
            }
            CB.setColData(cells, paste0(100*round(successRate, digits = 2), '%'), 1,
                    colStyle = CellStyle(wb) + Alignment(horizontal = "ALIGN_RIGHT"));
            CB.setColData(cells, successPerformance, 1, 1,
                    colStyle = CellStyle(wb) + Alignment(horizontal = "ALIGN_RIGHT"));
            
            successFES = matrixON[fffNo, matrixON[fffNo, ] < maxFES];
            successCount = length(successFES);
            successRate = successCount / howManyRuns;
            if(successCount == 0) {
                successPerformance = "-";
            } else {
                successPerformance = mean(successFES) * howManyRuns / successCount;
            }
            CB.setColData(cells, paste0(100*round(successRate, digits = 2), '%'), 2,
                    colStyle = CellStyle(wb) + Alignment(horizontal = "ALIGN_RIGHT"));
            CB.setColData(cells, successPerformance, 2, 1,
                    colStyle = CellStyle(wb) + Alignment(horizontal = "ALIGN_RIGHT"));
            CB.setBorder(cells, Border(position = "TOP"), 1, 1:2);
            CB.setBorder(cells, Border(position = "RIGHT"), 1:2, 2);
            CB.setBorder(cells, Border(position = c("TOP", "BOTTOM")), 2, 1:2);
        }
    }
    
    finalResultPath = paste0(resultsDir, "finalResult.xlsx");
    saveWorkbook(wb, finalResultPath);
    fullPath = paste0(getwd(), '/', finalResultPath);
    loggerCONSOLE("Excel created: ", fullPath, "\nOpening result file...\n");
    system(paste0("open ", fullPath));
    loggerCONSOLE("=== END ===\n");
}

createEmptySheet = function(wb, dim) {
    source("functions/cec2005problems.R");
    loadDimsSpecifics(1, dim); # ustawia maxFES
    
    sheet = createSheet(wb, sheetName = paste0("dim_", dim));
    
    # tytuł
    addMergedRegion(sheet, 1, 1, 1, 4);
    cell = CellBlock(sheet, 1, 1, 1, 1);
    CB.setColData(cell, "Tytuł", 1, colStyle = CellStyle(wb) + Alignment(horizontal = "ALIGN_CENTER"));
    
    # dim
    cells = CellBlock(sheet, 2, 1, 2, 2);
    CB.setColData(cells, "dimensions:", 1);
    CB.setColData(cells, dim, 2);
    CB.setColData(cells, "maxFES:", 1, 1);
    CB.setColData(cells, maxFES, 2, 1);
    CB.setBorder(cells, Border(position = "TOP"), 1, 1:2);
    CB.setBorder(cells, Border(position = "LEFT"), 1:2, 1);
    CB.setBorder(cells, Border(position = "RIGHT"), 1:2, 2);
    
    for(i in 1:finalFields) {
        baseRow = 3 + 1 + (i-1)*length(resultFieldsDetails);
        rowsCount = length(resultFieldsDetails);
        addMergedRegion(sheet, baseRow, baseRow + rowsCount - 1, 1, 1);
        cells = CellBlock(sheet, baseRow, 1, rowsCount, 2);
        CB.setColData(cells, resultFields[i], 1,
                colStyle = CellStyle(wb) + 
                        Alignment(horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER") +
                        Border(position = "TOP"));
        CB.setBorder(cells, Border(position = "LEFT"), 1:rowsCount, 1);
        
        for(j in 1:length(resultFieldsDetails)) {
            borderPositions = c("LEFT", "RIGHT");
            bgColor = "white";
            if(j == 1) borderPositions = c(borderPositions, "TOP");
            if(j >= length(resultFieldsDetails)-1) bgColor = "gray80";
            CB.setColData(cells, resultFieldsDetails[j], 2, j-1, 
                    colStyle = CellStyle(wb) +
                            Border(position = borderPositions) +
                            Fill(foregroundColor = bgColor));
        }
    }
    
    rowNo = 3 + length(resultFieldsDetails)*finalFields + 1;
    addMergedRegion(sheet, rowNo, rowNo, 1, 2);
    CB.setColData(CellBlock(sheet, rowNo, 1, 1, 1), "Success Rate", 1, 
            colStyle = CellStyle(wb) +
                    Alignment(horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER") +
                    Border(position = c("LEFT", "TOP")));
    CB.setBorder(CellBlock(sheet, rowNo, 2, 1, 1), Border(position = c("RIGHT", "TOP")), 1, 1);
    
    rowNo = rowNo + 1;
    addMergedRegion(sheet, rowNo, rowNo, 1, 2);
    CB.setColData(CellBlock(sheet, rowNo, 1, 1, 1), "Success Performance", 1, 
            colStyle = CellStyle(wb) +
                    Alignment(horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER") +
                    Border(position = c("BOTTOM", "LEFT", "TOP")));
    CB.setBorder(CellBlock(sheet, rowNo, 2, 1, 1), Border(position = c("BOTTOM", "RIGHT", "TOP")), 1, 1);
    
    autoSizeColumn(sheet, c(1,2));
    
    return(sheet);
}