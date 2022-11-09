cover_download <- function() {

  #download cover summary server
  output$download <- downloadHandler(
    #name of file based off of transect
    filename = function() {
      paste0("FQA_assessment_transect_", input$transect_id, ".zip")
    },
    #content of file
    content = function(file) {
      #set wd to temp directory
      tmpdir <- tempdir()
      setwd(tempdir())

      # Start a sink file with a CSV extension
      sink("FQI_metrics.csv")
      cat('\n')
      cat(paste0("Calculating metrics based on the ", input$db, " regional FQAI."))
      cat('\n')
      cat('\n')

      # Write metrics dataframe to the same sink
      write.csv(metrics() %>%
                  mutate(values = round(values, digits = 2)), row.names = F)
      cat('\n')
      cat('\n')

      # Write metrics dataframe to the same sink
      cat("Duration Frequency")
      cat('\n')
      write.csv(duration_table() %>%
                  mutate(values = round(percent, digits = 2)), row.names = F)
      cat('\n')
      cat('\n')

      # Write metrics dataframe to the same sink
      cat('Plot Summary Metrics')
      cat('\n')
      write.csv(plot_sum() %>%
                  mutate(across(where(is.numeric), round, digits = 2)), row.names = F)
      cat('\n')
      cat('\n')

      # Write metrics dataframe to the same sink
      cat('Species Summary Metrics')
      cat('\n')
      write.csv(species_sum() %>%
                  mutate(across(where(is.numeric), round, digits = 2)), row.names = F)
      cat('\n')
      cat('\n')

      # Write metrics dataframe to the same sink
      cat('Physiognomy Summary Metrics')
      cat('\n')
      write.csv(physiog_sum() %>%
                  mutate(across(where(is.numeric), round, digits = 2)), row.names = F)
      cat('\n')
      cat('\n')

      # Write data entered
      cat('Data Entered')
      cat('\n')
      write.csv(accepted(), row.names = F)

      # Close the sink
      sink()

      #now add two ggplots as pngs
      ggsave( "binned_hist.png", plot = binned_c_score_plot(metrics()),
              device = "png", bg = 'white')
      ggsave( "c_value_hist.png", plot = c_score_plot(accepted()), bg='#ffffff',
              device = "png")

      # Zip them up
      zip( file, c("FQI_metrics.csv", "binned_hist.png", "c_value_hist.png"))
    })
}
