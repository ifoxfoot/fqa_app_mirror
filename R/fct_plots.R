#' integer_breaks
#'
#' @description A function that returns integer breaks. Taken from Joshua Cook's website
#'
#' @return integer breaks
#'
#' @noRd
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

#' c_score_plot
#'
#' @description A function that returns a histogram of c scores
#'
#' @return A plot
#'
#' @noRd
#' @import ggplot2
c_score_plot <- function(input_data) {

  c_plot <- suppressWarnings(ggplot(data = input_data,
                                    aes(x = c,
                                        fill = nativity)) +
                               geom_histogram(col = "black", binwidth = 1, boundary = -0.5, na.rm = TRUE) +
                               scale_x_continuous(breaks = seq(0,10, by = 1), limits = c(-1, 11)) +
                               scale_y_continuous(breaks = integer_breaks()) +
                               labs(x = "Coefficient of Conservatism Value",
                                    y = "Number of Species",
                                    fill = "Native or Introduced") +
                               theme(text = element_text(size = 15)))

  return(c_plot)

}

#' binned_c_score_plot
#'
#' @description A function that returns a binned histogram of c scores
#'
#' @return A plot
#'
#' @noRd
#' @import ggplot2
binned_c_score_plot <- function(input_data) {

  dat <- input_data %>%
    dplyr::filter(stringr::str_detect(metrics, "% of Species")) %>%
    dplyr::mutate(metrics = stringr::str_remove(metrics, "% of Species with "))

  c_plot <- ggplot(data = dat) +
    geom_col(col = "black", aes(y = values, x = metrics, fill = values)) +
    labs(y = "Percent (%) of Species", x = "Coefficient of Conservatism Range") +
    ylim(0,100) +
    theme(text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
          legend.position = "none")

  return(c_plot)

}
