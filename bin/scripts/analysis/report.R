report_mod <- function(px = "LMean_NUC_edu",
                    py = "LMean_NUC_n",
                    xi = 1.25,
                    yi = 1.1,
                    pause = F,
                    graphit = T,
                    saveit = T){
  if(!exists("sirmixaplot")){
    cat("Loading SirMixaPlot functions")
    cat("\n")
    source("../bin/scripts/analysis/SirMixaPlot.R")
    cat("\n")
  }
  px <<- px
  py <<- py
  flist <- list.files()
  for (a in flist){
    setwd(a)
    if(paste0(a, "_all_cells.csv") %in% list.files()){
      fz <- paste0(a, "_all_cells.csv")
    } else{
      fz <- paste0(a, "_all.csv")
    }
    sirmixaplot(filo = fz, run = F)
    if(graphit==T){
      grapho(df = cells, X = px, Y = py, Xn = px, Yn = py)
    }
    modthequad(xi = xi, yi = yi, saveit = saveit, pause = pause, graphit = graphit)
    if(!exists("quantifications")){
      quantifications <- data.frame("fileId" = unique(cells$name_id),
                                    "condition" = unique(cells$condition),
                                    "dilution" = unique(cells$dilution),
                                    "cellNum" = nrow(cells),
                                    "eduNum" = qt2+qt4,
                                    "eduPercent" = 100*(qt2+qt4)/nrow(cells),
                                    "nNum" = qt1+qt2,
                                    "nPercent" = 100*(qt1+qt2)/nrow(cells))
    } else{
      interim <- data.frame("fileId" = unique(cells$name_id),
                            "condition" = unique(cells$condition),
                            "dilution" = unique(cells$dilution),
                            "cellNum" = nrow(cells),
                            "eduNum" = qt2+qt4,
                            "eduPercent" = 100*(qt2+qt4)/nrow(cells),
                            "nNum" = qt1+qt2,
                            "nPercent" = 100*(qt1+qt2)/nrow(cells))
      quantifications <- rbind(quantifications, interim)
    }
    setwd("../")
  }
  write.csv(quantifications, paste0("../data/",px,"_by_",py,"_quantifications.csv"), row.names = F)
}

report_cat <- function(catz = c("edu_norm","tag"),
                       px = "LMean_NUC_edu",
                       py = "LMean_NUC_n",
                       xi = 1.25,
                       yi = 1.1,
                       saveit = T,
                       pause = F,
                       graphit = T){
  px <<- px
  py <<- py
  flist <- list.files()
  for (a in flist){
    setwd(a)
    if(paste0(a, "_all_cells.csv") %in% list.files()){
      fz <- paste0(a, "_all_cells.csv")
    } else{
      fz <- paste0(a, "_all.csv")
    }
    sirmixaplot(filo = fz, run = F)
    for(b in catz){
      for(c in unique(cells[b])){
        interim_cells <- subset(cell, cells[b] == c)
      }
    }
    if(graphit==T){
      grapho(df = cells, X = px, Y = py, Xn = px, Yn = py)
    }
    modthequad(xi = xi, yi = yi, saveit = saveit, pause = pause, graphit = graphit)
    if(!exists("quantifications")){
      quantifications <- data.frame("fileId" = unique(cells$name_id),
                                    "condition" = unique(cells$condition),
                                    "dilution" = unique(cells$dilution),
                                    "cellNum" = nrow(cells),
                                    "eduNum" = qt2+qt4,
                                    "eduPercent" = 100*(qt2+qt4)/nrow(cells),
                                    "nNum" = qt1+qt2,
                                    "nPercent" = 100*(qt1+qt2)/nrow(cells))
    } else{
      interim <- data.frame("fileId" = unique(cells$name_id),
                            "condition" = unique(cells$condition),
                            "dilution" = unique(cells$dilution),
                            "cellNum" = nrow(cells),
                            "eduNum" = qt2+qt4,
                            "eduPercent" = 100*(qt2+qt4)/nrow(cells),
                            "nNum" = qt1+qt2,
                            "nPercent" = 100*(qt1+qt2)/nrow(cells))
      quantifications <- rbind(quantifications, interim)
    }
    setwd("../")
  }
  write.csv(quantifications, paste0("../",px,"_by_",py,"_quantifications.csv"), row.names = F)
}