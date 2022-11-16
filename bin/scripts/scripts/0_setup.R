# These are the packages required for MicroCyte R scripts:
recPackages <- c("ggplot2",
                 "ggpubr",
                 "ggtern",
                 "tidyverse",
                 "MASS",
                 "viridis",
                 "Rtsne",
                 "stringr",
                 "imager",
                 "magick",
                 "RecordLinkage",
                 "hash",
                 "plotly",
                 "Rtsne")

print("Checking installed packages...")
installedPackages <- installed.packages()
for (i in recPackages){
  if(!i %in% recPackages){
    print(paste0("Installing ", i, "..."))
    install.packages(i)
  }
}
print("Installation complete.")
