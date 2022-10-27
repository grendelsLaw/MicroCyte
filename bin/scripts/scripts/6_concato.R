#!/usr/bin/Rscript

concato <- function(dirz = "files",
                    count = T,
                    normalization = T){
  setwd(dirz)
  xList <- list.files()
  for (xL in xList){
    print(paste0("Concatenating condition file ", xL))
    if (!grepl("ijm", xL) & xL != "Thumbs.db"){
      setwd(xL)
      if (exists("cells")){
        rm(cells)
      }
      if (exists("rois")){
        rm(rois)
      }
      if (exists("peris")){
        rm(peris)
      }
      roiGo <- FALSE
      periGo <- FALSE
      yList <- list.files()
      for(yL in yList){
        print(yL)
        if(!grepl(".csv",yL) & !grepl(".pdf", yL) & yL != "Thumbs.db"){
          setwd(yL)
          zList <- paste0(yL, "_WN_all.csv")
          rList <- paste0(yL, "_ROI_all.csv" )
          pList <- paste0(yL, "_PERI_all.csv" )
          if(!exists("cells")){
            cells <- read.csv(zList)
          } else{
            interim <- read.csv(zList)
            for(i in names(cells)[!names(cells) %in% names(interim)]){
              interim[i] <- 0
              print(i)
            }
            for(i in names(interim)[!names(interim) %in% names(cells)]){
              cells[i] <- 0
            }
            
            cells <- rbind(cells, interim)
            print("Cells bound")
          }
          
          if(rList %in% list.files()){
            roiGo <- TRUE
            if(!exists("rois")){
              rois <- read.csv(rList)
            } else{
              rinterim <- read.csv(rList)
              for(i in names(rois)[!names(rois) %in% names(rinterim)]){
                rinterim[i] <- 0
                print(i)
              }
              for(i in names(rinterim)[!names(rinterim) %in% names(rois)]){
                rois[i] <- 0
              }

              rois <- rbind(rois, rinterim)
            }
          }
          print("ROIs bound")
          
          if(pList %in% list.files()){
            periGo <- TRUE
            if(!exists("peris")){
              peris <- read.csv(pList)
            } else{
              pinterim <- read.csv(pList)
              for(i in names(peris)[!names(peris) %in% names(pinterim)]){
                pinterim[i] <- 0
              }
              for(i in names(pinterim)[!names(pinterim) %in% names(peris)]){
                peris[i] <- 0
              }
              
              peris <- rbind(peris, pinterim)
              print("Peris bound")
            }
          }

          setwd("../")
        }
      }
      if (normalization == T){
        normTargets <- names(cells)[grepl("Mean_", names(cells)) | grepl("IntDen", names(cells)) ]
        for (imageNum in unique(cells$image)){
          for (target_name in normTargets){
            cells[target_name][cells$image == imageNum,] <- cells[target_name][cells$image == imageNum,]/mean(cells[target_name][cells$image == imageNum,])
            cells[target_name][cells$image == imageNum,] <- cells[target_name][cells$image == imageNum,] + min(cells[target_name][cells$image == imageNum,])
            quantos <- quantile(unlist(cells[target_name][cells$image == imageNum & !is.na(cells[target_name][cells$image == imageNum,]),]))[2]
            cells[target_name][cells$image == imageNum,] <- cells[target_name][cells$image == imageNum,]/quantos
          }
        }
      }
      write.csv(cells, file = paste0(xL, "_all.csv"), row.names = F)
      if(roiGo == T){
        write.csv(rois, file = paste0(xL, "_roi_all.csv"), row.names = F)
      }
      if(periGo == T){
        write.csv(peris, file = paste0(xL, "_peri_all.csv"), row.names = F)
      }
      if(!exists("minNum")){
        minNum <- nrow(cells)
      } else{
        if(nrow(cells) < minNum){
          minNum <- nrow(cells)
        }
      }
      setwd("../")
    }
  }
  if(count == T){
    cat(paste0("The minimum cell count recorded is: ",minNum))
    cat("\n")
  }
  setwd("../")
}