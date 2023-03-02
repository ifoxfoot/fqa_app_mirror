#' tmap
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
tmap_function <- function(file_path){

  #read in data
  regional_fqa_map <- read_sf(file_path)

  #set tmap to interactive
  suppressMessages(tmap_mode("view"))

  #make map
  tmap <- tm_shape(regional_fqa_map) +
    tm_polygons("FQA_database", legend.show = F,
                popup.vars = c("notes",
                               "total_species",
                               "total_native_species",
                               "certification")) +
    tmap_options(max.categories = 100) +
    tm_layout(title = "Regional FQA Lists")

  #return tmap
  return(tmap)
}
