#' tmap_function
#'
#' @description A function to generate the tmap of regional FQA databases
#'
#' @return A map
#'
#' @noRd
#' @import tmap
tmap_function <- function(){

  #set tmap to interactive
  suppressMessages(tmap_mode("view"))

  #make map
  tmap <- tm_shape(fqa_map) +
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
