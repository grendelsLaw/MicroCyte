# #!/usr/bin/Rscript
# 
# #This assigns the arguments
# args = commandArgs(trailingOnly=TRUE)

# here's the actual function to automatically generate the directories based on the schema file
dirGen <- function(idType="name"){
  #First, lets generate the appropriate folders
  dirList <- list.files()
  if(!"files" %in% dirList){
    dir.create("files")
  }
  if(!"figures" %in% dirList){
    dir.create("figures")
  }
  if(!"data" %in% dirList){
    dir.create("data")
  }
  
  #reads the schema file
  schema <- read.csv("schema.csv")
  # if the passed argument is "number", the notebookID is used, otherwise a descriptive name is generated from the optional data tags
  if (idType == "number" | ncol(schema)<6){
    xList <- unique(schema$notebook_id)
  } else {
    for (a in 6:ncol(schema)){
      if (exists("xList") & names(schema)[a] != "name_id"){
        xList <- paste(xList, schema[,a], sep = "_")
      } else if (names(schema)[a] != "name_id"){
        xList <- schema[,a]
      }
      schema$name_id <- xList
      write.csv(schema, "schema.csv", row.names = F)
    }
  }
  # Directories are touched, unless they already exist
  for(a in xList){
    dirName <- paste0("files/", a)
    if(!dir.exists(dirName))
    dir.create(dirName)
  }
}

runType <- readline(prompt = "Generating directories. Should this be done by name or number (default - name): ")
dirGen(idType = runType)