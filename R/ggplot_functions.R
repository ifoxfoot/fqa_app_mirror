#this file contains functions to produce ggplots
c_score_plot <- function(input_data) {

  c_plot <- ggplot(data = input_data,
                  aes(x = c,
                      fill = native)) +
    geom_histogram(col = "black") +
    scale_x_continuous(breaks = seq(0,10, by=1), limits = c(-1,11)) +
    labs(x = "Conservation Coefficient Score",
         fill = "Native or Exotic")

  return(c_plot)

}

compare_plot <- function(input_data, db_name, db) {

  c_plot <- ggplot() +
    geom_freqpoly(data = input_data, aes(x = c,
                                         after_stat(density),
                                         color = "Data Entered"
                                         ),
                  size = 2,
                  binwidth = 1) +
    geom_freqpoly(data = db, aes(x = c,
                                 after_stat(density),
                                 color = {{db_name}}
                                 ),
                  size = 2,
                  binwidth = 1) +
    scale_colour_manual(values = c("#00A36C", "#FFA500"), name = "") +
    scale_x_continuous(breaks = seq(0, 10, by=1), limits = c(0, 10)) +
    labs(x = "Conservation Coefficient Score")


  return(c_plot)

}
