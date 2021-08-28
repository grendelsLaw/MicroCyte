#!/usr/bin/Rscript

unite <- function(dirz = "files", 
                  full = T,
                  pattern = F,
                  filename = "Experiment",
                  sampleSize = 1000){
  # First, remove the 'cells' dataset, if it exists
  if(exists("cells")){
    rm(cells)
  }
  # Then, remove the 'peris' dataset, if it exists
  if(exists("peris")){
    rm(peris)
  }
  # An then, remove the 'rois' dataset, if it exists
  if(exists("rois")){
    rm(rois)
  }
  # Then we set the directory to the default
  setwd(dirz)
  # If a pattern has been called, the files names are parsed for that pattern
  # otherwise, the entire list is used
  if (pattern != F){
    yList <- list.files(pattern = pattern)
    filename <- pattern
    print(paste0("Uniting sample datasets containing the phrase: ", pattern))
  } else {
    yList <- list.files()
    print(paste0("Uniting all sample datasets found in '", dirz,"'."))
  }
  
  # For each directory within the folder, the concatonated lists are pulled together
  for(yL in yList){
    if(!grepl(".csv",yL) & !grepl(".pdf", yL)){
      setwd(yL)
      # all_cell files are prefentially pulled, but if not found, then the all files to pulled
      if(paste0(yL,"_all_cells.csv") %in% list.files()){
        zList <- list.files(pattern = paste0(yL,"_all_cells.csv"))[1]
      } else {
        zList <- list.files(pattern = paste0(yL,"_all.csv"))[1]
      }
      
      # If cells exist, the file is added to it
      # If cells doesn't exist, cells is created
      if(!exists("cells")){
        cells <- read.csv(zList)
        # If only a subset of the total is desired, only a random sampling of that number will be taken
        if(full == F){
          cells <- cells[sample(nrow(cells), sampleSize),]
        }
      } else{
        interim <- read.csv(zList)
        # If only a subset of the total is desired, only a random sampling of that number will be taken
        if(full == F){
          interim <- interim[sample(nrow(interim), sampleSize),]
        }
        
        # If the number of columns do not match, 'NA's are populated to allow row-binding
        if(ncol(interim) != ncol(cells)){
          for(i in names(cells)[!names(cells) %in% names(interim)]){
            interim[i] <- 0
          }
          for(i in names(interim)[!names(interim) %in% names(cells)]){
            cells[i] <- 0
          }
        }
        #cells are concatonated
        cells <- rbind(cells, interim)
      }
      # Now we see if roi or peri files exist
      periList <- list.files(pattern = "peri_all")
      roiList <- list.files(pattern = "roi_all")
      
      # If peri files exist, they are collected using the same system
      if (length(periList) != 0){
        if (!exists("peris")){
          peris <- read.csv(periList[1])
        } else {
          periInterim <- read.csv(periList[1])
          if(ncol(periInterim) != ncol(peris)){
            for(i in names(peris)[!names(peris) %in% names(periInterim)]){
              periInterim[i] <- 0
            }
            for(i in names(periInterim)[!names(periInterim) %in% names(peris)]){
              peris[i] <- 0
            }
          }
          peris <- rbind(peris, periInterim)
        }
      }
      
      # If roi files exist, they are collected using the same system
      if (length(roiList) != 0){
        if (!exists("rois")){
          rois <- read.csv(roiList[1])
        } else {
          roiInterim <- read.csv(roiList[1])
          if(ncol(roiInterim) != ncol(rois)){
            for(i in names(rois)[!names(rois) %in% names(roiInterim)]){
              roiInterim[i] <- 0
            }
            for(i in names(roiInterim)[!names(roiInterim) %in% names(rois)]){
              rois[i] <- 0
            }
          }
          rois <- rbind(rois, roiInterim)
        }
      }
      setwd("../")
    }
  }
  
  # if the file has already been opened in sirmixaplot and modified, its saved as an all_cels files
  # otherwise its saved as an all file
  if("log2_dna" %in% names(cells)){
    write.csv(cells, file = paste0("../data/",filename, "_all_cells.csv"), row.names = F)
  } else{
    write.csv(cells, file = paste0("../data/",filename, "_all.csv"), row.names = F)
  }
  
  # If peris exist, they are saved
  if (exists("peris")){
    write.csv(peris, file = paste0("../data/",filename, "_peri_all.csv"), row.names = F)
  }
  
  # If rois exist, they are saved
  if (exists("rois")){
    write.csv(rois, file = paste0("../data/",filename, "_roi_all.csv"), row.names = F)
  }
  # Then we back out into the main folder
  setwd("../")
}