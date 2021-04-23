suppressPackageStartupMessages(library(imager))
suppressPackageStartupMessages(library(magick))

icellate <- function(targetCells,
                     folderName = "singleCells",
                     dMultiplier = 0.3,
                     verifySize = T,
                     verifyImage = "overlay.png",
                     randomize = F,
                     samplingNumber = 5){
  #Set a parameter to allow size verification
  sizeVerified <- F
  
  # First, we'll create the folder in case it doesn't exist, then go to it
  if(!"icellates" %in% list.files()){
    dir.create("icellates")
  }
  setwd("icellates")
  # Now, since each set of images are likely to represent *some* kind of cluster, we'll create of cluster folder thatn can be added to
  if(!folderName %in% list.files()){
    dir.create(folderName)
    
  }
  setwd(folderName)
  
  # If you want to randomly sample a certain number of cells from the population, this bit will do that
  if(randomize == T){
    targetCells <- targetCells[sample(nrow(targetCells), samplingNumber), ]
  }
  
  for (tCell in 1:nrow(targetCells)){
    targetCell <- targetCells[tCell,]
    #Now we get the target folder and the target image of the targetCell
    targetNameID <- unique(targetCell$name_id)
    targetImage <- unique(targetCell$image)
    
    # With this, we can make a special folder for JUST THIS CELL
    cellIdDir <- paste0(targetNameID, "-", unique(targetCell$Number))
    if (!cellIdDir %in% list.files()){
      dir.create(cellIdDir)
    }
    setwd(cellIdDir)
    # We can also now get a list of the images and move them into the new cellIdDir
    imageList <- list.files(path = paste0("../../../files/", targetNameID, "/", targetImage,"/PNGS"), 
                            pattern = ".png", full.names = T)
    for (i in imageList){
      file.copy(i, "./")
    }
    
    #Now to interate through the images, and them as a dataframe
    for (i in list.files()){
      interim <- load.image(i)
      interim <- as.data.frame(interim)
      
      #Now that the datarframe has been made, we'll select ONLY the pixels within a certain distance from the mean of the cell
      
      if (sizeVerified == F & verifySize == T){
        while (TRUE){
          interim <- load.image(verifyImage)
          interim <- as.data.frame(interim)
          
          #First, lets figure out how big the area of the picture needs to be based on the imageType parameter
          areaName <- names(cells)[grepl("^Area_NUC", names(targetCell))]
          cellLength <- dMultiplier*round(sqrt(targetCell[areaName]/pi))
          cellLength <- as.integer(round(cellLength))
          
          # Now we identify the XY coordinate of the cell and trim the dataframe data to fit the calculated cellLength
          XName <- names(cells)[grepl("^X_NUC", names(targetCell))]
          XPos <- as.integer(round(targetCell[XName]))
          XMin <- round(XPos-cellLength)
          XMax <- round(XPos+cellLength)
          
          YName <- names(cells)[grepl("^Y_NUC", names(targetCell))]
          YPos <- as.integer(round(targetCell[YName]))
          YMin <- round(YPos-cellLength)
          YMax <- round(YPos+cellLength)
          
          # Now we remove all the pixels we don't care about
          interim <- interim[interim$x >= XMin & interim$x <= XMax,]
          interim <- interim[interim$y >= YMin & interim$y <= YMax,]
          
          #Now to reset the corrdinates
          interim$x <- 1+interim$x-min(interim$x)
          interim$y <- 1+interim$y-min(interim$y)
          
          # Now to convert the dataframe to an image object and save the image
          newPlot <- suppressWarnings(as.cimg(interim))
          plot(newPlot)
          print(paste0("dMultiplier is currently set to: ", dMultiplier))
          checkBox <- readline(prompt = "Does this look like the appropriate size (Y/n)? ")
          if (checkBox == "n"){
            dMultiplier <- as.numeric(readline(prompt = "What should the new dMultiplier be: "))
          } else{
            sizeVerified <- T
            break
          }
        }
      } else {
        #First, lets figure out how big the area of the picture needs to be based on the imageType parameter
        areaName <- names(cells)[grepl("^Area_NUC", names(targetCell))]
        cellLength <- dMultiplier*round(sqrt(targetCell[areaName]/pi))
        cellLength <- as.integer(round(cellLength))
        
        # Now we identify the XY coordinate of the cell and trim the dataframe data to fit the calculated cellLength
        XName <- names(cells)[grepl("^X_NUC", names(targetCell))]
        XPos <- as.integer(round(targetCell[XName]))
        XMin <- round(XPos-cellLength)
        XMax <- round(XPos+cellLength)
        
        YName <- names(cells)[grepl("^Y_NUC", names(targetCell))]
        YPos <- as.integer(round(targetCell[YName]))
        YMin <- round(YPos-cellLength)
        YMax <- round(YPos+cellLength)
        
        # Now we remove all the pixels we don't care about
        interim <- interim[interim$x >= XMin & interim$x <= XMax,]
        interim <- interim[interim$y >= YMin & interim$y <= YMax,]
        
        #Now to reset the corrdinates
        interim$x <- 1+interim$x-min(interim$x)
        interim$y <- 1+interim$y-min(interim$y)
        
        # Now to convert the dataframe to an image object and save the image
        newPlot <- suppressWarnings(as.cimg(interim))
      }
      
      newPlot <- cimg2magick(newPlot, rotate = T)
      image_write(newPlot, path = i, format = "png")
    }
    setwd("../")
  }
  setwd("../../")
}