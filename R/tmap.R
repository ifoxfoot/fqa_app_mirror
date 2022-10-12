
library(tidyverse)
library(sf)

tmap_function <- function(file_path){

  #read in data, simplify it
  regional_fqa_map <- read_sf(file_path)

  #set tmap to interactive
  tmap_mode("view")

  #make map
  tmap <- tm_shape(regional_fqa_map) +
            tm_polygons("FQA_database", legend.show = F, popup.vars = "Notes") +
            tmap_options(max.categories = 51) +
            tm_layout(title = "This Map Is Not Done!!")

  #return tmap
  return(tmap)
}
