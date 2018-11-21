#####################################################################################################
#
# Use PNGs to create Gif Animation
#
#####################################################################################################

# install imageMagick
# https://www.imagemagick.org/script/download.php#windows

# Tutorials
# https://ryanpeek.github.io/2016-10-19-animated-gif_maps_in_R/
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html

library(magick) # this is call to animate/read pngs


list.files(path = "C:/Users/joao_/Downloads/Gif/", pattern = "*.png", full.names = T) %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=100) %>% # animates, can opt for number of loops
  image_write("tech_100.gif") # write to current dir