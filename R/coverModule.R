#UI-----------------------------------------------------------------------------
#dropdown list to select cover method
coverMethodUI <- function(id) {
  tagList(selectInput(NS(id, "cover_method"), label = "Cover Method",
                      choices = c(
                        "percent_cover",
                        "braun-blanquet",
                        "carolina_veg_survey",
                        "daubenmire",
                        "usfs_ecodata"))
)}

coverDataEntryUI <- function(id) {
  tagList(
    #plot id text input
    column(2, textInput(NS(id, "plot_id"), "Plot ID")),
    #select species
    column(4, uiOutput(NS(id, "select_species"))),
    #select cover value
    column(4, uiOutput(NS(id,"cover_value"))),
    #add species button
    column(2, actionAddSpecies(id = id))
  )
}



#Output-------------------------------------------------------------------------

#datatable of entered data
dataTableOutput("cover_DT_manual")

#plot output
plotOutput("cover_c_hist_manual")

#output table of metrics
tableOutput("cover_metrics_manual")


tableOutput("cover_species_manual")


tableOutput("cover_plot_manual")

#Server-------------------------------------------------------------------------
coverServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    #drop-down list based on region
    output$cover_value <- renderUI({
      cover_vals <-
        #list of what values appear in dropdown menu depending on cover_method_select
        if(input$cover_method == "braun-blanquet") {
          c("+", "1", "2", "3", "4", "5")
        }
      else  if(input$cover_method == "daubenmire") {
        c("1", "2", "3", "4", "5", "6")
      }
      else if(input$cover_method== "carolina_veg_survey"){
        c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
      }
      else  if(input$cover_method == "usfs_ecodata"){
        c("1", "3", "10", "20", "30", "40", "50", "60", "70", "80", "90", "98")
      }
      else{
        c("1":"100")
      }
      #create a dropdown option
      selectizeInput("cover_val", "Cover Value", c("", cover_vals),
                     selected = NULL,
                     multiple = FALSE)
    })

    #create an object with no values but correct col names to store inputs
    #cover_data <- reactiveVal({data_entered})

    #make it so add species button can't be clicked until all fields full
    observe({
      vals <- c(input$cover, input$species, input$plot_id)
      toggleState("add_species", !"" %in% vals)
    })

    #save edits
    observeEvent(input$add_species, {
      #combine entries into one-row df
      new_row <- data.frame(plot_id = input$plot_id,
                            scientific_name = input$add_species,
                            cover = input$cover)
      #bind new entry to table
      new_table = rbind(new_row, cover_data())
      #make it reactive
      cover_data(new_table)

      #reset drop down menu of latin names
      shinyjs::reset("add_species")
      shinyjs::reset("cover")
    })
#-------------------------------------------------------------------------------
    #render output table from manually entered species on data entry page
    output$cover_DT_manual <- DT::renderDT({
      datatable(cover_data_entered_manual(),
                selection = 'single',
                options = list(
                  scrollX = TRUE,
                  searching = FALSE,
                  lengthChange = FALSE))
    })

    #when delete species is clicked, delete row
    observeEvent(input$cover_delete_species,{
      #call table
      t = cover_data_entered_manual()
      #print table
      print(nrow(t))
      #if rows are selected, delete them
      if (!is.null(input$cover_DT_manual_rows_selected)) {
        t <- t[-as.numeric(input$cover_DT_manual_rows_selected),]
      }
      #else show the regular table
      cover_data_entered_manual(t)
    })

    #when delete all is clicked, clear all entries
    observeEvent(input$cover_delete_manual_entries, {
      #make an empty df
      empty_df <- data.frame(row.names = names(fqacalc::crooked_island))
      #assign it to the reactive value
      cover_data_entered_manual(empty_df)
    })

##second screen-----------------------------------------------------------------

    #metrics table output on cover page
    output$cover_metrics_manual <- renderTable({
      #requiring second screen
      req(input$shinyglide_index_cover == 1)

      fqacalc::all_cover_metrics(x = cover_data_entered_manual(),
                                 key = "scientific_name",
                                 db = input$db,
                                 cover_metric = input$cover_method_select)
    })

    #plot summary
    output$cover_plot_manual <- renderTable({
      #requiring second screen
      req(input$shinyglide_index_cover == 1)

      fqacalc::plot_summary(x = cover_data_entered_manual(),
                            key = "scientific_name",
                            db = input$db,
                            cover_metric = input$cover_method_select,
                            plot_id = "plot_id")
    })

    #species summary
    output$cover_species_manual <- renderTable({
      #requiring second screen
      req(input$shinyglide_index_cover == 1)

      fqacalc::species_summary(x = cover_data_entered_manual(),
                               key = "scientific_name",
                               db = input$db,
                               cover_metric = input$cover_method_select)
    })


    #ggplot output
    output$cover_c_hist_manual <- renderPlot({
      #requiring second screen
      req(input$shinyglide_index_cover == 1)

      c_score_plot(fqacalc::accepted_entries(x = cover_data_entered_manual(),
                                             key = "scientific_name",
                                             db = input$db,
                                             native = F,
                                             cover_metric = input$cover_method_select))
    })
  })
}
