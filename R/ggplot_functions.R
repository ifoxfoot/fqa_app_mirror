#this file contains functions to produce ggplots
c_score_plot <- function(input_data) {

  c_plot <- ggplot(data = input_data,
                  aes(x = c,
                      fill = native)) +
    geom_histogram(col = "black", binwidth = 1, boundary = -0.5) +
    scale_x_continuous(breaks = seq(0,10, by = 1), limits = c(-1, 11)) +
    labs(x = "Coefficient of Conservatism Value",
         y = "Number of Species",
         fill = "Native or Non-native") +
    theme(text = element_text(size = 15))

  return(c_plot)

}

binned_c_score_plot <- function(input_data) {

  dat <- input_data %>%
    filter(str_detect(metrics, "%")) %>%
    mutate(metrics = str_remove(metrics, "% of Species with "))

  c_plot <- ggplot(data = dat) +
    geom_col(col = "black", aes(y = values, x = metrics, fill = values)) +
    labs(y = "Percent", x = "") +
    theme(text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
            legend.position = "none")

  return(c_plot)

}

# compare_plot <- function(input_data, db_name, db) {
#
#   c_plot <- ggplot() +
#     geom_freqpoly(data = input_data, aes(x = c,
#                                          after_stat(density),
#                                          color = "Data Entered"
#                                          ),
#                   size = 2,
#                   binwidth = 1) +
#     geom_freqpoly(data = db, aes(x = c,
#                                  after_stat(density),
#                                  color = "db_name"
#                                  ),
#                   size = 2,
#                   binwidth = 1) +
#     scale_colour_manual(values = c("#00A36C", "#FFA500"), name = "",
#                         labels = c("Data Entered",
#                                    str_wrap(str_replace_all({{db_name}}, "_", " "), width = 20))) +
#     scale_x_continuous(breaks = seq(0, 10, by=1), limits = c(0, 10)) +
#     labs(x = "Coefficient of Conservatism Value") +
#     theme(text = element_text(size = 15))
#
#
#   return(c_plot)
#
# }
