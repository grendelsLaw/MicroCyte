#!/usr/bin/Rscript

unite <- function(dirz = "files", 
                  full = T,
                  pattern = F,
                  filename = "Experiment",
                  sampleSize = 1000){
  if(exists("cells")){
    rm(cells)
  }
  setwd(dirz)
  if (pattern != F){
    yList <- list.files(pattern = pattern)
    filename <- pattern
    print(paste0("Uniting sample datasets containing the phrase: ", pattern))
  } else {
    yList <- list.files()
    print(paste0("Uniting all sample datasets found in '", dirz,"'."))
  }
  for(yL in yList){
    if(!grepl(".csv",yL) & !grepl(".pdf", yL)){
      setwd(yL)
      if(paste0(yL,"_all_cells.csv") %in% list.files()){
        zList <- list.files(pattern = paste0(yL,"_all_cells.csv"))[1]
      } else {
        zList <- list.files(pattern = paste0(yL,"_all.csv"))[1]
      }
      if(!exists("cells")){
        cells <- read.csv(zList)
        if(full == F){
          cells <- cells[sample(nrow(cells), sampleSize),]
        }
      } else{
        interim <- read.csv(zList)
        if(full == F){
          interim <- interim[sample(nrow(interim), sampleSize),]
        }
        if(ncol(interim) != ncol(cells)){
          for(i in names(cells)[!names(cells) %in% names(interim)]){
            interim[i] <- 0
          }
          for(i in names(interim)[!names(interim) %in% names(cells)]){
            cells[i] <- 0
          }
        }
        cells <- rbind(cells, interim)
      }
      setwd("../")
    }
  }
  if("log2_dna" %in% names(cells)){
    write.csv(cells, file = paste0("../data/",filename, "_all_cells.csv"), row.names = F)
  } else{
    write.csv(cells, file = paste0("../data/",filename, "_all.csv"), row.names = F)
  }
  setwd("../")
}