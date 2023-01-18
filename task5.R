modificationOfData <- function() {
  new_tmp <- ExpImp
  
  for (i in 2:ncol(new_tmp)) {
    str <- new_tmp[[i]]
    
    for (j in seq_along(str)) {
      
      if (is.na(str[j])) {
        new_tmp[[i]][j] <- 0
      } 
      else if (str[j] == "-") {
        new_tmp[[i]][j] <- 0
      }
      new_tmp[[i]] <- as.numeric(new_tmp[[i]])
    }
  }
  dataImportCol <- new_tmp[, grepl("Импорт", colnames(new_tmp))]
  dataExportCol <- new_tmp[, grepl("Экспорт", colnames(new_tmp))]
  tmp <- cbind(new_tmp[, 1], dataExportCol)
  
  fullTable <- cbind(tmp, dataImportCol)
  fullTable['СуммаИмпорта'] <- rowSums(dataImportCol, na.rm = TRUE)
  fullTable['СуммаЭкспорта'] <- rowSums(dataExportCol, na.rm = TRUE)
  return(fullTable)
}

load("ExpImp.RData")
sortedData <- modificationOfData()

print(sortedData)

filtrationOfData <- function(data) {
  rows <- which(data$СуммаЭкспорта > data$СуммаИмпорта)
  return(data[rows, 1])
}

data <- filtrationOfData(sortedData)
print(data)
