cover_help <- function(){
  alert <- shinyalert(title = "Instructions", html = T, className = "help",
             text = tagList(strong("Step 1."),
                            "Select your regional FQAI Database. If you are unsure
                            about which database to use, consult the map on the ‘About FQA’ tab.",
                            br(),
                            br(),
                            strong("Step 2."),
                            "Decide if you would like to enter observations manually
                            or if you would like to upload a file.",
                            br(),
                            br(),
                            strong("Step 3."),
                            "Enter your cover method. Percent Cover is the default
                            and is recommended as it is the most accurate. Other methods
                            are converted to percentage ranges, and then the midpoint
                            is used. For more information, see the ‘More’ tab.",
                            br(),
                            br(),
                            strong("Step 4."),
                            "Enter a Transect ID. This tool is designed to calculate
                            metrics for one transect at a time. If you are only calculating
                            metrics for a plot or quadrat, enter anything into the transect
                            ID. If you choose to download your report, your transect ID will
                            become your file name.",
                            br(),
                            br(),
                            strong("Step 5."),
                            "Select your plot ID. If you are only entering data for
                            one plot, keep the same ID throughout.",
                            br(),
                            br(),
                            strong("Step 6."),
                            "Select a species. At this time, only latin names are provided.
                            If you are not sure what latin name to use, consult the
                            regional database in the ‘View Regional FQA Lists’ tab. Note
                            that lists are not comprehensive and may not contain every
                            species in the region. Some lists also only contain native plants.",
                            br(),
                            br(),
                            strong("Step 7."),
                            "Enter the cover value.",
                            br(),
                            br(),
                            strong("Step 8."),
                            "Click add species. Repeat steps 5-7 until you have entered
                            all desired information. To delete an entry, select
                            the row and click ‘Delete Species’. To delete all entries
                            click ‘Delete All Entries’.",
                            br(), br(),
                            "Note-changing the regional database or the cover method will
                            automatically clear your entries.",
                            br(),
                            br(),
                            strong("Step 9."),
                            "Click `Calculate FQA Metrics` when you are ready. You
                            may come back to the data entry page to enter more data.
                            You may also download a report. To learn more about
                            the metrics calculated, visit the ‘More’ tab.",
                            br(),
                            br(),
                            "To report a mistake or unexpected issue on this webpage, contact _______"
                            ))

return(alert)

}

fqi_help <- function(){
  alert <- shinyalert(title = "Instructions", html = T, className = "help",
                      text = tagList(strong("Step 1."),
                                     "Select your regional FQAI Database.If you are unsure
                                     about which database to use, consult the map on the
                                     ‘About FQA’ tab.",
                                     br(),
                                     br(),
                                     strong("Step 2."),
                                     "Decide if you would like to enter observations
                                     manually or if you would like to upload a file.",
                                     br(),
                                     br(),
                                     strong("Step 3."),
                                     "To upload a file, select a file. The file must
                                     be a csv, tsv, or xlsx file. The file must
                                     contain a column containing scientific names
                                     of species. The file must reach to the
                                     top of the file-i.e. There cannot be a title or
                                     blank row above the columns. Select the name of
                                     the column that contains scientific names. You may
                                     delete the file by clicking ‘Delete Uploaded File’.",
                                     br(),
                                     br(),
                                     strong("Step 4."),
                                     "To Enter data manually, select species. At this
                                     time, only latin names are provided. If you are not
                                     sure what latin name to use, consult the regional
                                     database in the ‘View Regional FQA Lists’ tab. Note
                                     that lists are not comprehensive and may not contain
                                     every species in the region. Some lists also only
                                     contain native plants. Then click add species.
                                     You may add more species again. To delete a species,
                                     select the row and click ‘Delete Species’. To
                                     delete all entries click ‘Delete all Entries’",
                                     br(),
                                     br(),
                                     "Note-changing the regional database will automatically
                                     clear your entries.",
                                     br(),
                                     br(),
                                     strong("Step 5."),
                                     "Click `Calculate FQA Metrics` when you are ready.
                                     You may come back to the data entry page to enter
                                     more data. You may also download a report. To
                                     learn more about the metrics calculated, visit the ‘More’ tab.",
                                     br(),
                                     br(),
                                     "To report a mistake or unexpected issue on this
                                     webpage, contact _______"))
  return(alert)
}
