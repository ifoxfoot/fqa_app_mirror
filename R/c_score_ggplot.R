#this code renders a ggplot
c_score_plot <- function(input_data) {

  #ggplot
  c_plot <- ggplot(data = input_data,
                  aes(x = c,
                      fill = native)) +
    geom_histogram(col = "black") +
    scale_x_continuous(breaks = seq(0,10, by=1), limits = c(-1,11)) +
    labs(title = "Conservation Coefficient Histogram",
         x = "Conservation Coefficient Score",
         fill = "Native or Exotic")

  return(c_plot)

}
