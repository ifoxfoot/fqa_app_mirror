fqi_help <- function(){
  alert <- shinyalert(title = "FQI Instructions", html = T, className = "help",
             text = tagList(strong("Step 1."),
                            "Select your regional FQA of interest. If you are unsure
                            about which regional FQA to use, consult the map on the
                            ‘About FQA’ tab.",
                            br(),
                            br(),
                            strong("Step 2."),
                            "Decide if you would like to enter observations manually
                            or if you would like to upload a file.",
                            br(),
                            br(),
                            strong("Step 3."),
                            "To upload a file, select a file from your device. The
                            file must be a .csv, .tsv, or .xlsx file. The file must
                            contain a column containing scientific names of species
                            or acronyms. The file must be formatted such that the
                            columns extend to the top of the spreadsheet, with row 1
                            containing column names. Select the name of the column
                            that contains scientific names or acronyms. You may delete
                            the file by clicking ‘Delete Uploaded File’.",
                            br(),
                            br(),
                            strong("Step 4."),
                            "To enter data manually, click on the field that says ‘species’,
                            then start typing Genus. As you type a pick list will appear, then
                            select full species name. Then click 'Add Species'. You may add
                            more species. To delete a species, select the row and click
                            ‘Delete Species’. To delete all entries click ‘Delete all Entries’.
                            At this time, you may only select species based on Latin names
                            or acronyms. If you are not sure what Latin name to use, consult
                            the regional database in the ‘View Regional FQA Lists’ tab. Note
                            that some regional FQAs are not comprehensive and may not contain
                            every species in the region.",
                            br(),
                            "NOTE: Changing the regional database will automatically
                            clear your entries.",
                            br(),
                            br(),
                            strong("Step 5."),
                            "Click `Calculate FQA Metrics` button, located upper
                            right. You may come back to the data entry page to enter
                            more data. You may also download a report. To learn
                            more about the metrics calculated, visit the ‘More’ tab.",
                            br(),
                            br(),
                            strong("Step 9."),
                            "Click `Calculate FQA Metrics` when you are ready. You
                            may come back to the data entry page to enter more data.
                            You may also download a report. To learn more about
                            the metrics calculated, visit the ‘More’ tab.",
                            br(),
                            br(),
                            "To report a mistake or unexpected issue on this webpage,
                            contact - brook.d.herman@usace.army.mil"
                            ))

return(alert)

}

cover_help <- function(){
  alert <- shinyalert(title = "Cover Weighted FQA Instructions", html = T, className = "help",
                      text = tagList(strong("Step 1."),
                                     "Select your regional FQA of interest. If you are unsure
                                     about which regional FQA to use, consult the map on the
                                     ‘About FQA’ tab.",
                                     br(),
                                     br(),
                                     strong("Step 2."),
                                     "Enter your cover method. Percent Cover is the default.
                                     Other methods use percent ranges, and then the
                                     midpoint of those ranges is used. For more information,
                                     see the ‘More’ tab.",
                                     br(),
                                     br(),
                                     strong("Step 3."),
                                     "Decide if you would like to enter observations manually
                                     or if you would like to upload a file",
                                     br(),
                                     br(),
                                     strong("Step 4."),
                                     "To upload a file, select a file from your device.
                                     The file must be a .csv, .tsv, or .xlsx file. The file must
                                     contain a column containing scientific names of species
                                     or acronyms. The file must be formatted such that the
                                     columns extend to the top of the spreadsheet, with row 1
                                     containing column names. Select the name of the column that
                                     contains scientific names or acronyms. Then select the column
                                     that contains cover values. If your file contains a transect
                                     with multiple plots you may optionally select a column with plot
                                     IDs to generate a plot summary. You may delete
                                     the file by clicking ‘Delete Uploaded File’.",
                                     br(),
                                     br(),
                                     strong("Step 4."),
                                     "To enter data manually, enter a transect ID
                                     (you may only enter data for one transect at a time).
                                     Then enter a plot ID. If you intend to calculate
                                     metrics for a single plot, keep the same plot ID
                                     throughout the data entry process. Click on the field that says
                                     ‘species’, then start typing Genus. As you type a pick
                                     list will appear. Select full species name, then click 'Add Species'.
                                     Then enter the coverage value. For more information on
                                     cover values consult the 'More' tab. To delete a species,
                                     select the row and click ‘Delete Species’.
                                     To delete all entries click ‘Delete all Entries’.
                                     At this time, you may only select species based on
                                     Latin names or acronyms. If you are not sure what
                                     Latin name to use, consult the regional database
                                     in the ‘View Regional FQA Lists’ tab. Note that
                                     some regional FQAs are not comprehensive and may
                                     not contain every species in the region.",
                                     br(),
                                     br(),
                                     "NOTE: Changing the regional database or cover method
                                     will automatically clear your entries.",
                                     br(),
                                     br(),
                                     strong("Step 5."),
                                     "Click `Calculate FQA Metrics` located upper right.
                                     You may come back to the data entry page to enter more
                                     data. You may also download a report. To learn more
                                     about the metrics calculated, visit the ‘More’ tab.",
                                     br(),
                                     br(),
                                     "To report a mistake or unexpected issue on this webpage,
                                     contact – brook.d.herman@usace.army.mil"))
  return(alert)
}
