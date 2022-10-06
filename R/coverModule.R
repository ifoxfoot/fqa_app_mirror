#sidebar------------------------------------------------------------------------

#side bar UI function
coverSideBarUI <- function(id) {
  tagList(

    #input regional data base
    selectInput(NS(id, "db"), label = "Select Regional FQAI Database",
                choices = fqacalc::db_names(),
                selected = "michigan_2014"),

    #input data entry method
    radioButtons(NS(id, "input_method"), label = "Select Data Entry Method",
                 choices = c( "Enter Species Manually" = "enter",
                              "Upload a File" = "upload")),

    #select cover method
    selectInput(NS(id, "cover_method"), label = "Cover Method",
                choices = c(
                  "percent_cover",
                  "braun-blanquet",
                  "carolina_veg_survey",
                  "daubenmire",
                  "usfs_ecodata")),

    #when data entry method is upload, allow user to upload files
    conditionalPanel(

      condition = "input['cover-input_method'] == 'enter'",

      #delete species button
      actionButton(NS(id, "delete_species"), "Delete Species"),

      #button to delete all entries
      actionButton(NS(id, "delete_all"), "Delete All Entries")

      ),#conditional parenthesis

    #when data entry method is upload, allow user to upload files
    conditionalPanel(

      condition = "input['cover-input_method'] == 'upload'",

      "UNDER CONSTRUCTION"

    ) #conditional parenthesis

  )}

#main panel----------------------------------------------------------------------

#group of widgets to input cover data
coverMainPanelUI <- function(id) {
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
    column(3, uiOutput(NS(id,"cover_value"))),
    #add species button
    column(3, actionButton(NS(id, "add_species"), "Add Species",
                           style = "margin-top: 30px; height: 40px;"))),


   fluidRow(
   #datatable of entered data
   dataTableOutput(NS(id, "cover_DT_manual")))
)}

#second screen-------------------------------------------------------------------------

#group of widgets to input cover data
coverOutputUI <- function(id) {

  tagList(

    fluidRow(
      #title
      column(5, h3(textOutput(NS(id, "title")))),

      #download button
      column(2, downloadButton(NS(id, "download"),
                               label = "Download", class = "downloadButton",
                               style = "margin-top: 25px; height: 40px;"))),

    fluidRow(
      #plot output
      box(plotOutput(NS(id, "cover_c_hist_manual"))),

      #output table of metrics
      box(tableOutput(NS(id, "cover_metrics_manual")))),

    #output of species summary
    fluidRow(box(title = "Species Summary", status = "primary",
    tableOutput(NS(id, "cover_species_manual")), width = 12, style = "overflow-x: scroll")),

    #output of plot summary
    fluidRow(box(title = "Plot Summary", status = "primary",
    tableOutput(NS(id, "cover_plot_manual")), width = 12, style = "overflow-x: scroll")),
)}

#Server-------------------------------------------------------------------------

coverServer <- function(id, cover_glide) {
  #call server fun for species dropdown
  selectSpeciesServer(id = id)

  #start module function
  moduleServer(id, function(input, output, session) {

    #drop-down list of cover values based on cover metric input
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

      if (input$cover_method != "percent_cover") {
      #create a dropdown option
      selectizeInput(session$ns("cover_val"), "Cover Value", c("", cover_vals),
                     selected = NULL,
                     multiple = FALSE) }
      #else allow numeric input
      else { numericInput(session$ns("cover_val"), "Cover Value",
                          value = 0, min = 0, max = 100)}
    })

    #call server fun for species dropdown
    selectSpeciesServer(id)

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
      shinyjs::reset("species")
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

    #initialize reactives
    entries <- reactiveVal()
    cover_metrics <- reactiveVal()
    species_sum <- reactiveVal()
    plot_sum <- reactiveVal()

    #updating reactive values
    observe({
      #requiring second screen to update reactive values
      req(cover_glide() == 1)

      #update reactives
      entries(fqacalc::accepted_entries(x = cover_data(),
                                        key = "scientific_name",
                                        db = input$db,
                                        native = F,
                                        cover_metric = input$cover_method))

      cover_metrics(fqacalc::all_cover_metrics(x = cover_data(),
                               key = "scientific_name",
                               db = input$db,
                               cover_metric = input$cover_method))

      species_sum(fqacalc::species_summary(x = cover_data(),
                                           key = "scientific_name",
                                           db = input$db,
                                           cover_metric = input$cover_method))

      plot_sum(fqacalc::plot_summary(x = cover_data(),
                                     key = "scientific_name",
                                     db = input$db,
                                     cover_metric = input$cover_method,
                                     plot_id = "plot_id"))

      })

    #render title
    output$title <- renderText({paste("Calculating metrics based on",
                                      input$db)})

    #download cover summary server
    output$download <- downloadHandler(
      #name of file based off of transect
      filename = function() {
        paste0("transect_", input$transect_id, ".zip")
      },
      #content of file
      content = function(fname) {
        #set wd to temp directory
        tmpdir <- tempdir()
        setwd(tempdir())

        #list names of files to zip
        fs <- c("data_entered.csv", "all_metrics.csv",
                "plot_summary.csv", "species_summary.csv")

        #write csvs
        write.csv(entries(), file = "data_entered.csv")
        write.csv(cover_metrics(), file = "all_metrics.csv")
        write.csv(plot_sum(), file = "plot_summary.csv")
        write.csv(species_sum(), file = "species_summary.csv")

        #zip files, name them
        zip(zipfile=fname, files=fs)
        if(file.exists(paste0(fname, ".zip")))
        {file.rename(paste0(fname, ".zip"), fname)}
      },
      contentType = "application/zip"
    )

    #metrics table output on cover page
    output$cover_metrics_manual <- renderTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive cover metrics
      cover_metrics()
    })

    #plot summary
    output$cover_plot_manual <- renderTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive plot summary
      plot_sum()
    })

    #species summary
    output$cover_species_manual <- renderTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive species summary
      species_sum()
    })

    #ggplot output
    output$cover_c_hist_manual <- renderPlot({
      #requiring second screen
      req(cover_glide() == 1)
      #ggplot function with call to reactive
      c_score_plot(entries())
    })
  })
}
