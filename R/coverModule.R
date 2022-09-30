#first screen-------------------------------------------------------------------

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

#group of widgets to input cover data
coverDataEntryUI <- function(id) {
  tagList(
    #input transect ID
    fluidRow(
      column(4, textInput(NS(id, "transect_id"), "Transect ID"))),

    fluidRow(
    #plot id text input
    column(2, textInput(NS(id, "plot_id"), "Plot ID")),
    #select species
    column(4, uiOutput(NS(id, "select_species"))),
    #select cover value
    column(4, uiOutput(NS(id,"cover_value"))),
    #add species button
    column(2, actionAddSpecies(id = id))),


   fluidRow(
   #datatable of entered data
   dataTableOutput(NS(id, "cover_DT_manual")))
)}

#second screen-------------------------------------------------------------------------

#group of widgets to input cover data
coverOutputUI <- function(id) {
  tagList(

    downloadButtonUI(id = id),
   #plot output
   plotOutput(NS(id, "cover_c_hist_manual")),

   #output table of metrics
   tableOutput(NS(id, "cover_metrics_manual")),

   #output of species summary
   tableOutput(NS(id, "cover_species_manual")),

   #output of plot summary
   tableOutput(NS(id, "cover_plot_manual")),
)}

#Server-------------------------------------------------------------------------

coverServer <- function(id, shiny_glide) {
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
      selectizeInput(session$ns("cover_val"), "Cover Value", c("", cover_vals),
                     selected = NULL,
                     multiple = FALSE)
    })

    #make it so add species button can't be clicked until all fields full
    observe({
      vals <- c(input$transect_id, input$cover_val, input$species, input$plot_id)
      toggleState("add_species", !"" %in% vals)
    })

    #define table for data entered manually
    data_entered = data.frame()

    #create an object with no values but correct col names to store inputs
    cover_data <- reactiveVal({data_entered})

    #save edits
    observeEvent(input$add_species, {
      #combine entries into one-row df
      new_row <- data.frame(plot_id = input$plot_id,
                            scientific_name = input$species,
                            cover = input$cover_val)
      #bind new entry to table
      new_table = rbind(new_row, cover_data())
      #make it reactive
      cover_data(new_table)
      #reset drop down menu of latin names
      shinyjs::reset("add_species")
      shinyjs::reset("cover_val")
    })

    #render output table from manually entered species on data entry page
    output$cover_DT_manual <- DT::renderDT({
      datatable(cover_data(),
                selection = 'single',
                options = list(
                  scrollX = TRUE,
                  searching = FALSE,
                  lengthChange = FALSE))
    })

    #when delete species is clicked, delete row
    observeEvent(input$delete_species,{
      #call table
      t = cover_data()
      #print table
      print(nrow(t))
      #if rows are selected, delete them
      if (!is.null(input$cover_DT_manual_rows_selected)) {
        t <- t[-as.numeric(input$cover_DT_manual_rows_selected),]
      }
      #else show the regular table
      cover_data(t)
    })

    #when delete all is clicked, clear all entries
    observeEvent(input$delete_all, {
      #make an empty df
      empty_df <- data.frame(row.names = names(fqacalc::crooked_island))
      #assign it to the reactive value
      cover_data(empty_df)
    })

##second screen-----------------------------------------------------------------

      #download cover summary server
      output$download <- downloadHandler(
        filename = function() {
          paste0("transect_", input$transect_id, "_excelWorkbook.xlsx")
        },
        content = function(file) {
          # write workbook and first sheet
          write.xlsx(ouput$cover_metrics_manual, file, sheetName = "general_cover_metrics", append = FALSE)

          # add other sheets for each dataframe
          listOtherFiles <- list(plot_summary = output$cover_plo_manual,
                                 species_summary = output$cover_species_manual)
          for(i in 1:length(listOtherFiles)) {
            write.xlsx(listOtherFiles[i], file,
                       sheetName = names(listOtherFiles)[i], append = TRUE)
          }
        }
      )

    #metrics table output on cover page
    output$cover_metrics_manual <- renderTable({
      #requiring second screen
      req(shiny_glide() == 1)

      fqacalc::all_cover_metrics(x = cover_data(),
                                 key = "scientific_name",
                                 db = input$db,
                                 cover_metric = input$cover_method)
    })

    #plot summary
    output$cover_plot_manual <- renderTable({
      #requiring second screen
      req(shiny_glide() == 1)

      fqacalc::plot_summary(x = cover_data(),
                            key = "scientific_name",
                            db = input$db,
                            cover_metric = input$cover_method,
                            plot_id = "plot_id")
    })

    #species summary
    output$cover_species_manual <- renderTable({
      #requiring second screen
      req(shiny_glide() == 1)

      fqacalc::species_summary(x = cover_data(),
                               key = "scientific_name",
                               db = input$db,
                               cover_metric = input$cover_method)
    })


    #ggplot output
    output$cover_c_hist_manual <- renderPlot({
      #requiring second screen
      req(shiny_glide() == 1)

      c_score_plot(fqacalc::accepted_entries(x = cover_data(),
                                             key = "scientific_name",
                                             db = input$db,
                                             native = F,
                                             cover_metric = input$cover_method))
    })
  })
}
