
cover_download <- function(.data, db, cover) {

  #title
  title <- paste("Metrics calculated using the", db, "regional FQAI.")

  #all metrics
  all_mets <- all_cover_metrics(x = .data, key = "scientific_name",
                                db = db, cover_metric = cover )

  #species summary
  species_sum <- species_summary(x = .data, key = "scientific_name",
                                 db = db, cover_metric = cover )

  #plot summary
  plot_sum <- species_summary(x = .data, key = "scientific_name",
                              db = db, cover_metric = cover, plot_id = "plot_id")

  #accepted entries
  entries  <- accepted_entries(x = .data, key = "scientific_name",
                               db = db, native = F,
                               cover_weighted = T, cover_metric = cover,
                               allow_duplicates = T)

  #gather together
  report <- c(title, all_mets, species_sum, plot_sum, entries)

  return(report)

  }
