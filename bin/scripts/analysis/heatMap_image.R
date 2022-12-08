suppressPackageStartupMessages(library(imager))
suppressPackageStartupMessages(library(magick))

heatMap_image <- function(image_location,
                          heatColor = "inferno"
                          ){
  if(!grepl(pattern = ".png", x = image_location) & grepl(pattern = ".tif", x = image_location)){
    tick <- image_read(image_location)
    image_location <- str_replace(image_location, "tif", "png")
    image_write(tick, path = image_location, format = "png")
  }
  
  oldImage <- load.image(image_location)
  # plot(oldImage)
  # pause <- readline(prompt = "Pausing...1")
  thick <- as.data.frame(oldImage)
  check_gray <- as.data.frame(grayscale(oldImage))
  
  check_gray$R <- 0
  check_gray$G <- 0
  check_gray$B <- 0
  # 
  # print(head(check_gray))
  # pause <- readline(prompt = "Pausing...2")
  
  fake_plot <- ggplot(data = check_gray, aes(fill=value, x=x, y=y))+
    geom_point()+
    scale_fill_viridis_c(option = heatColor)
  fake_plot <- ggplot_build(fake_plot)
  fake_plot <- as.data.frame(fake_plot$data)
  # print(head(fake_plot))
  # pause <- readline(prompt = "Pausing...3")
  check_gray$hex <- fake_plot$fill
  # print(head(check_gray))
  # pause <- readline(prompt = "Pausing...4")
  # print(unique(check_gray$hex))
  
  for (viro in unique(check_gray$hex)){
    rgb_code <- col2rgb(viro)
    check_gray[check_gray$hex == viro,]$R <- rgb_code[1]
    check_gray[check_gray$hex == viro,]$G <- rgb_code[2]
    check_gray[check_gray$hex == viro,]$B <- rgb_code[3]
  }
  # print(head(check_gray))
  # pause <- readline(prompt = "Pausing...5")
  
  check_gray$R <- check_gray$R/255
  check_gray$G <- check_gray$G/255
  check_gray$B <- check_gray$B/255
  
  thick[thick$cc == 1,]$value <- check_gray$R
  thick[thick$cc == 2,]$value <- check_gray$G
  thick[thick$cc == 3,]$value <- check_gray$B
  
  newPlot <- suppressWarnings(as.cimg(thick))
  newPlot <- cimg2magick(newPlot, rotate = T)
  newPlot <- image_flop(newPlot)
  image_location <- str_replace(image_location, ".png", "_heatMapped.png")
  image_write(newPlot, path = image_location, format = "png")
}