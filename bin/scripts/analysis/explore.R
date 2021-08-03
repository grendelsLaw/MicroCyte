explore <- function(fileName = "data/experiment.csv",
                    sortBy = "name_id",
                    categories = T,
                    cellCycle = T,
                    scheme = "schema.csv",
                    icellate = F,
                    vSize = F,
                    random = T,
                    saveFile = "data/experiment_explored.csv",
                    addend = T){
  
  #First, categories from the dataframe are defined
  if(is.data.frame(fileName)){
    dFrame <- fileName
  } else {
    dFrame <- read.csv(file = fileName)
  }
  dFrame["placeHolder"] <- dFrame[sortBy]
  if(categories == T){
    catNum <- as.numeric(readline(prompt = "How many categories would you like to explore: "))
    if (catNum > 0){
      print(names(dFrame)[1:length(dFrame)-1])
      catTab <- data.frame("catNum"=c(1:catNum))
      catTab$catName <- "imageID"

      for (i in 1:catNum){
        catName <- readline(prompt = paste0("What is the name of category ", i, "? "))
        dFrame$placeHolder <- paste0(dFrame$placeHolder, "_", catName, ".")
        dFrame$placeHolder <- paste0(dFrame$placeHolder, unlist(dFrame[catName]))
        catTab[catTab$catNum == i,]$catName <- catName
      }
      sortBy <- "placeHolder"
    }
  } else {
    catNum <- 0
  }
  
  datum <- data.frame("name_id" = unique(unlist(dFrame[sortBy])))
  
  schema <- read.csv(scheme)
  metaNames <- names(schema)[6:(length(names(schema))-1)]
  for (i in metaNames){
    datum[i] <- "Holder"
    for(j in 1:nrow(datum)){
      target <- unique(unlist(dFrame[dFrame$placeHolder == datum["name_id"][j,],][i]))
      datum[i][j,] <- target
    }
  }
  
  if (categories == T & catNum > 0){
    for (noi in names(datum)[2:length(names(datum))]){
      if (!exists("check")){
        check <- unlist(datum[noi])
      } else {
        check <- paste(check, unlist(datum[noi]), sep = "_")
      }
    }
    datum$totalName <- check
  }
  
  #This pulls the category data per population
  if (categories == T & catNum > 0){
    for (b in catTab$catName){
      datum[b] <- "Holder"
      for (c in 1:nrow(datum)){
        datum[b][c,] <- strsplit(datum["name_id"][c,], paste0(b, "."))[[1]][2]
        datum[b][c,] <- strsplit(datum[b][c,], "_")[[1]][1]
      }
    }
  }
  
  datum$total <- 0
  for (totalSet in unique(datum$name_id)){
    datum[datum$name_id == totalSet,]$total <- nrow(dFrame[dFrame["placeHolder"]==totalSet,])
    if (icellate != F & is.numeric(icellate)){
      source("bin/scripts/analysis/icellate.R")
      if(icellate >= nrow(dFrame[dFrame["placeHolder"]==totalSet,])){
        icellate(targetCells = dFrame[dFrame["placeHolder"]==totalSet,], 
                 folderName = totalSet, 
                 verifySize = vSize, 
                 samplingNumber = nrow(dFrame[dFrame["placeHolder"]==totalSet,]), 
                 randomize = random)
      } else {
        icellate(targetCells = dFrame[dFrame["placeHolder"]==totalSet,], 
                 folderName = totalSet, 
                 verifySize = vSize, 
                 samplingNumber = icellate, 
                 randomize = random)
      }
    }
  }
  if (categories == T & catNum > 0){
    datum$subsetTotal <- datum$total
    datum$total <- 0
    for (totalSet in unique(datum$totalName)){
      totalCount <- sum(subset(datum, totalName == totalSet)$subsetTotal)
      datum[datum$totalName == totalSet,]$total <- totalCount
    }
    datum$subsetPercent <- round(100*datum$subsetTotal/datum$total, 2)
  }
  
  if (cellCycle == T){
    datum$sPhase <- NA
    datum$G1 <- NA
    datum$G2 <- NA
    datum$re_sPhase <- NA
    datum$G3 <- NA
    datum$S_MFI <- NA
    datum$S_MDI <- NA
    datum$rS_MFI <- NA
    datum$rS_MDI <- NA
    for (a in unique(datum$name_id)){
      set <- dFrame[dFrame["placeHolder"]==a,]
      datum[datum$name_id ==a,]$sPhase <- 100*nrow(set[set$edu == "Positive",])/nrow(set)
      datum[datum$name_id ==a,]$G1 <- 100*nrow(set[set$edu == "Negative" & set$ploidy == "2N",])/nrow(set)
      datum[datum$name_id ==a,]$G2 <- 100*nrow(set[set$edu == "Negative" & set$ploidy == "4N",])/nrow(set)
      datum[datum$name_id ==a,]$re_sPhase <- 100*nrow(set[set$edu == "Positive" & set$ploidy == ">4N",])/nrow(set)
      datum[datum$name_id ==a,]$G3 <- 100*nrow(set[set$edu == "Negative" & set$ploidy == ">4N",])/nrow(set)
      datum[datum$name_id ==a,]$S_MFI <- mean(set[set$edu == "Positive" & set$ploidy != ">4N",]$edu_norm)
      datum[datum$name_id ==a,]$S_MDI <- mean(set[set$edu == "Positive" & set$ploidy != ">4N",]$dna_norm)
      datum[datum$name_id ==a,]$rS_MFI <- mean(set[set$edu == "Positive" & set$ploidy == ">4N",]$edu_norm)
      datum[datum$name_id ==a,]$rS_MDI <- mean(set[set$edu == "Positive" & set$ploidy == ">4N",]$dna_norm)
    }
  }
  if (addend == T){
    datum_total <- read.csv(saveFile)
    datum <- rbind(datum_total, datum)
  }
  write.csv(datum, file = saveFile, row.names = F)
}