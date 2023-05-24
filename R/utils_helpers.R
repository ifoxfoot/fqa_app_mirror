#' jscode
#'
#' @description A utils function using js to refocus curser
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
jscode <- "shinyjs.refocus = function(id) {
  document.getElementById(id).focus();}"
