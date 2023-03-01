#this file contains functions to produce ggplots
c_score_plot <- function(input_data) {

  c_plot <- suppressWarnings(ggplot(data = input_data,
                  aes(x = c,
                      fill = nativity)) +
    geom_histogram(col = "black", binwidth = 1, boundary = -0.5, na.rm = TRUE) +
    scale_x_continuous(breaks = seq(0,10, by = 1), limits = c(-1, 11)) +
    labs(x = "Coefficient of Conservatism Value",
         y = "Number of Species",
         fill = "Native or Non-native") +
    theme(text = element_text(size = 15)))

  return(c_plot)

}

binned_c_score_plot <- function(input_data) {

  dat <- input_data %>%
    filter(str_detect(metrics, "% of Species")) %>%
    mutate(metrics = str_remove(metrics, "% of Species with "))

  c_plot <- ggplot(data = dat) +
    geom_col(col = "black", aes(y = values, x = metrics, fill = values)) +
    labs(y = "Percent (%) of Species", x = "Coefficient of Conservatism Range") +
    ylim(0,100) +
    theme(text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
            legend.position = "none")

  return(c_plot)

}


