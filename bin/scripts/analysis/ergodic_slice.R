library(ggplot2)
library(viridis)
#source("bin/scripts/analysis/icellate.R")

ergodic_slicing <- function(df = cells,
                            subset_df = cells[cells$edu == "Positive",],
                            variable = "dna_norm",
                            bins = 10,
                            rate = 24,
                            rate_description = "doubling time (hours)",
                            icell = F,
                            icell_number = 3){
  list_df <- split(subset_df, cut(unlist(subset_df[variable]), breaks = bins))
  if(icell == T){
    for (bin_set in list_df){
      icellate(bin_set)
    }
  }
  histo_draft <- ggplot(data = subset_df, aes(x = unlist(subset_df[variable])))+
    geom_density()+
    xlab(variable)+
    theme_classic()
  print(histo_draft)
  ergo_df <- data.frame("bin_id" = c(1:bins),
                        "bin_rate" = 0,
                        "volume" = 0,
                        "mean_stat" = 0)
  tick <- 1
  for (bin_set in list_df){
    ergo_df[tick,]$volume <- nrow(bin_set)
    ergo_df[tick,]$bin_rate <- rate*((2-(nrow(subset_df)/nrow(df)))/(nrow(bin_set)/nrow(subset_df)))
    ergo_df[tick,]$mean_stat <- median(unlist(bin_set[variable]))
    tick <- tick +1
  }
  ergo_df$bin_rate <- ergo_df$bin_rate/mean(ergo_df[!is.infinite(ergo_df$bin_rate),]$bin_rate)
  cbins <- c(1:bins)
  ergo_df[is.na(ergo_df$mean_stat),]$mean_stat <- ""
  ergo_draft <- ggplot(data = ergo_df[!is.infinite(ergo_df$bin_rate),], aes(x=bin_id, y=bin_rate, fill=volume))+
    geom_bar(stat = "identity", position = position_dodge(0.7))+
    ylab("Mean bin progression rate (a.u. per hour)")+
    scale_x_continuous(breaks = cbins, labels = ergo_df$mean_stat)+
    xlab(paste0("Median size of ", variable))+
    theme_classic()+
    scale_fill_viridis()+
    theme(legend.position = "top")
  print(ergo_draft)
}