#' cover_conversion
#'
#' @description A function that converts non standard cover classes into percent cover
#'
#' @return A shiny alert
#'
#' @noRd
cover_conversion <- function(x, cover_class){

  #if cover method is percent cover
  if(cover_class == "percent_cover") {
    return(x)
  }

  #if cover method is usfs_ecodata
  if(cover_class == "usfs_ecodata") {
    if(x == "1"){return(0.5)}
    else if(x == "3"){return(3)}
    else if(x == "10"){return(10)}
    else if(x == "20"){return(20)}
    else if(x == "30"){return(30)}
    else if(x == "40"){return(40)}
    else if(x == "50"){return(50)}
    else if(x == "60"){return(60)}
    else if(x == "70"){return(70)}
    else if(x == "80"){return(80)}
    else if(x == "90"){return(90)}
    else if(x == "98"){return(98)}
  }

  if(cover_class == "carolina_veg_survey"){
    if(x == "1"){return(0.1)}
    else if(x == "2"){return(0.5)}
    else if(x == "3"){return(1.5)}
    else if(x == "4"){return(3.5)}
    else if(x == "5"){return(7.5)}
    else if(x == "6"){return(17.5)}
    else if(x == "7"){return(37.5)}
    else if(x == "8"){return(62.5)}
    else if(x == "9"){return(85)}
    else if(x == "10"){return(97.5)}
  }

  if(cover_class == "daubenmire"){
    if(x == "1"){return(2.5)}
    else if(x == "2"){return(15)}
    else if(x == "3"){return(37.5)}
    else if(x == "4"){return(62.5)}
    else if(x == "5"){return(85)}
    else if(x == "6"){return(97.5)}
  }


  if(cover_class == "braun-blanquet"){
    if(x == "+"){return(0.1)}
    else if(x == "1"){return(2.5)}
    else if(x == "2"){return(15)}
    else if(x == "3"){return(37.5)}
    else if(x == "4"){return(62.5)}
    else if(x == "5"){return(87.5)}
  }

}
