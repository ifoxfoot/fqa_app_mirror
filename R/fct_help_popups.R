#' inventory_help
#'
#' @description A fct function that produces a help pop-up
#'
#' @return A shiny alert
#'
#' @noRd
inventory_help <- function(){
  alert <- showModal(modalDialog(title = "Inventory Instructions",
                                  tagList(strong("Step 1."),
                                                 "Select your regional FQA of interest. If you are unsure
                            about which regional FQA database to use, consult the map on the
                            'About FQA' tab.",
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
                            contain a column containing scientific names", strong("or"), "acronyms.
                            The file must be formatted such that the columns extend to
                            the top of the spreadsheet, with row 1 containing column
                            names. Select the name of the column that contains scientific
                            names or acronyms. You may delete the file by clicking
                            'Delete Uploaded File'.",
                                                 br(),
                                                 br(),
                                                 strong("Step 4."),
                                                 "To enter data manually, click on the
                                     field that says 'Select Species', then start
                                     typing in the Genus . As you type a pick list
                                     will appear. Click on the species you want,
                                     or when it is selected press the enter key.
                                     Click 'Add Species'. You may repeat this process
                                     multiple times. To delete a species, select the
                                     row and click 'Delete Species'. To delete all
                                     entries click 'Delete all Entries'. At this
                                     time, you may only select species based on
                                     scientific names or acronyms. If you are not
                                     sure what scientific name or acronym to use,
                                     consult the regional database in the 'View
                                     Regional FQA Databases'  tab. Note that some
                                     regional FQA databases are not comprehensive
                                     and may not contain every species in the region.",
                                                 br(),
                                                 "NOTE: Changing the regional database will automatically
                            clear your entries.",
                                                 br(),
                                                 br(),
                                                 strong("Step 5."),
                                                 "Click `Calculate FQA Metrics` button, located
                                     in the upper right. You may come back to the
                                     data entry page to enter more data. You may
                                     also download a report. To learn more about
                                     the metrics calculated, visit the 'Equations tab.",
                                                 br(),
                                                 br(),
                                                 "To report a mistake or unexpected issue on this webpage,
                                     contact - ecomodteam@usace.army.mil")))

  return(alert)
}

#' cover_help
#'
#' @description A function that produces a help pop-up
#'
#' @return A shiny alert
#'
#' @noRd
cover_help <- function(){
  alert <- showModal(modalDialog(title = "Cover Weighted FQA Instructions",
                                  tagList(strong("Step 1."),
                                                 "Select your regional FQA database of interest. If you are unsure
                                     about which regional FQA to use, consult the map on the
                                     'About FQA' tab.",
                                                 br(),
                                                 br(),
                                                 strong("Step 2."),
                                                 "Enter your cover method (e.g., method used to
                                     estimate abundance of species in plots). Percent
                                     Cover is the default (ranging from 1-100%).
                                     Other methods use percent ranges
                                     (e.g., cover categories), and then the midpoint
                                     of those ranges is used. For more information,
                                     see the 'More' tab.",
                                                 br(),
                                                 br(),
                                                 strong("Step 3."),
                                                 "Decide if you would like to enter observations manually
                                     or if you would like to upload a file.",
                                                 br(),
                                                 br(),
                                                 strong("Step 4."),
                                                 "To upload a file, select a file from your
                                     device. The file must be a .csv, .tsv, or
                                     .xlsx file. The file must contain a column
                                     containing scientific names or acronyms, a
                                     column that contains cover values, and a
                                     column that contains plot identification (ID)s.
                                     The file must be formatted such that the
                                     columns extend to the top of the spreadsheet,
                                     with row 1 containing column names. Select the
                                     name of the column that contains scientific
                                     names or acronyms. Then select the column that
                                     contains cover values. If your file contains a
                                     transect with multiple plots you must select
                                     the column with plot IDs to generate a plot
                                     summary. You may delete the file by clicking
                                     'Delete Uploaded File'.",
                                                 br(),
                                                 br(),
                                                 strong("Step 4."),
                                                 "To enter data manually, enter a transect ID
                                     (you may only enter data for one transect at a time).
                                     Then enter a plot ID. If you intend to calculate
                                     metrics for a single plot, keep the same plot
                                     ID throughout the data entry process. Click
                                     on the field that says, 'Select Species', then
                                     start typing in the Genus. As you type a pick
                                     list will appear. Select the species by clicking
                                     or pressing the enter key. Then enter the cover
                                     value. For more information on cover values
                                     consult the 'More' tab. Then click 'Add Species'.
                                     To delete a species, select the row and click
                                     'Delete Species'. To delete all entries click
                                     'Delete all Entries'. At this time, you may
                                     only select species based on scientific names
                                     or acronyms. If you are not sure what scientific
                                     name or acronym to use, consult the regional
                                     database in the 'View Regional FQA Databases'
                                     tab. Note that some regional FQA databases are
                                     not comprehensive and may not contain every
                                     species in the region.",
                                                 br(),
                                                 br(),
                                                 "NOTE: Changing the regional database or cover method
                                     will automatically clear your entries.",
                                                 br(),
                                                 br(),
                                                 strong("Step 5."),
                                                 "To calculate metrics click `Calculate FQA
                                     Metrics` located upper right. You may come
                                     back to the data entry page to enter more
                                     data. You may also download a report. To learn
                                     more about the metrics calculated, visit the
                                     'Equations ' tab.",
                                                 br(),
                                                 br(),
                                                 "To report a mistake or unexpected issue on this webpage,
                                     contact – ecomodteam@usace.army.mil")))
  return(alert)
}
