#!/usr/bin/Rscript

reName <- function(){
  schema <- read.csv("../schema.csv")
  xList <- list.files(path = "../files/")
  for (a in xList){
    if (!grepl(".ijm", x = a)){
      aList <- list.files(path = paste0("../files/", a))
      for (aL in aList){
        if(a %in% unique(schema$notebook_id)){
          hit <- subset(schema, notebook_id == a)[1,]
        } else if (a %in% unique(schema$name_id)){
          hit <- subset(schema, name_id == a)[1,]
        }
        yList <- list.files(path = paste0("../files/", a,"/", aL), pattern = ".tif")
        for (b in yList){
          if(T %in% sapply(c("_CH1.tif", "_CH2.tif", "_CH3.tif", "_CH4.tif"), grepl, strsplit(b, ".tif")[[1]][1])){
            b_sub <- strsplit(b, "_")[[1]][2]
            b_sub <- strsplit(b_sub, ".tif")[[1]][1]
            x <- unique(hit[b_sub][,1])
            x <- paste0(x, ".tif")
            if (a != x){
              file.rename(paste0("../files/", a,"/", aL, "/", b), paste0("../files/", a, "/", aL, "/", x))
            }
          } else if(grepl("Overlay.tif", b)) {
            file.rename(paste0("../files/", a,"/", aL, "/", b), paste0("../files/", a, "/", aL, "/" ,"overlay.tif"))
          }
        }
        if("PNGS" %in% list.files(path = paste0("../files/", a, "/", aL))){
          yList <- list.files(path = paste0("../files/", a,"/", aL, "/PNGS"), pattern = ".png")
          for (b in yList){
            if(T %in% sapply(c("_CH1.tif", "_CH2.tif", "_CH3.tif", "_CH4.tif"), grepl, strsplit(b, ".tif")[[1]][1])){
              b_sub <- strsplit(b, "_")[[1]][2]
              b_sub <- strsplit(b_sub, ".png")[[1]][1]
              x <- unique(hit[b_sub][,1])
              x <- paste0(x, ".png")
              if (a != x){
                file.rename(paste0("../files/", a,"/", aL, "/PNGS/", b), paste0("../files/", a, "/", aL, "/PNGS/", x))
              }
            } else if(grepl("Overlay.png", b)) {
              file.rename(paste0("../files/", a,"/", aL, "/PNGS/", b), paste0("../files/", a, "/", aL, "/PNGS/overlay.png"))
            }
          }
        }
        if("originals" %in% list.files(path = paste0("../files/", a, "/", aL))){
          yList <- list.files(path = paste0("../files/", a,"/", aL, "/originals"), pattern = ".tif")
          for (b in yList){
            if(T %in% sapply(c("_CH1.tif", "_CH2.tif", "_CH3.tif", "_CH4.tif"), grepl, strsplit(b, ".tif")[[1]][1])){
              b_sub <- strsplit(b, "_")[[1]][2]
              b_sub <- strsplit(b_sub, ".tif")[[1]][1]
              x <- unique(hit[b_sub][,1])
              x <- paste0(x, ".tif")
              if (a != x){
                file.rename(paste0("../files/", a,"/", aL, "/originals/", b), paste0("../files/", a, "/", aL, "/originals/", x))
              }
            } else if(grepl("Overlay.tif", b)) {
              file.rename(paste0("../files/", a,"/", aL, "/originals/", b), paste0("../files/", a, "/", aL, "/originals/overlay.tif"))
            }
          }
        }
      }
    }
  }
}

# test if there is at least one argument and uses the first one: if not, it defaults to the "name" argument
reName()