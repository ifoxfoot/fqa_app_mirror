## code to prepare `fqa_map` dataset goes here
library(sf)

#read in data
fqa_map <- read_sf("data-raw/regional_fqa_simple.gpkg")

#save it
usethis::use_data(fqa_map, overwrite = TRUE, internal = TRUE)
