#' cover UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cover_ui <- function(id){
  ns <- NS(id)
  tagList(

    shinyglide::glide(
      id = ns( "glide"),
      next_label = paste("Calculate FQA Metrics ", icon("arrow-right")),
      previous_label = paste(icon("arrow-left"), "Go Back to Data Entry"),
      controls_position = "bottom",
      height = "100%",
      keyboard = FALSE,

      shinyglide::screen(
        next_condition = "output['cover-next_condition'] == 'TRUE'",

        fluidRow(
          sidebarPanel(

            titlePanel("Enter Data"),

            #help button
            shinyWidgets::circleButton(ns( "help"), icon = icon("question"),
                         style = "position:absolute; top:5px; right:5px;",
                         status = "primary"),

            #input regional data base
            selectInput(ns( "db"), label = "Select Regional FQAI Database",
                        choices = fqacalc::db_names()$fqa_db,
                        selected = "michigan_2014"),

            #select cover method
            selectInput(ns( "cover_method"), label = "Cover Method",
                        choices = c(
                          "percent_cover",
                          "braun-blanquet",
                          "carolina_veg_survey",
                          "daubenmire",
                          "usfs_ecodata")),

            #when db has incomplete acronyms hide acronym option
            conditionalPanel(

              condition = "output['cover-complete_acronym'] == 'TRUE'",
              #input key argument
              shinyWidgets::radioGroupButtons(ns( "key"), label = "Enter Species Using: ",
                                choices = c("Scientific Names" = "name",
                                            "Acronyms" = "acronym"),
                                justified = TRUE,
                                checkIcon = list(yes = icon("ok",
                                                            lib = "glyphicon"))),
            ),

            #input data entry method
            shinyWidgets::prettyRadioButtons(ns( "input_method"), label = "Select Data Entry Method",
                               choices = c( "Enter Species Manually" = "enter",
                                            "Upload a File" = "upload")),


            #when data entry method is upload, allow user to upload files
            conditionalPanel(

              condition = "input['cover-input_method'] == 'upload'",

              #input file upload widget
              fileInput(ns( "upload"), NULL, buttonLabel = "Upload...", multiple = F),

              #input what column to use to bind to FQA database
              uiOutput(ns( "species_colname")),

              #input what column to use as cover
              uiOutput(ns( "cover_colname")),

              #input what column to use as plot id
              uiOutput(ns( "plot_colname")),

              #input button to delete uploaded file
              actionButton(ns( "upload_delete_all"), "Delete Uploaded File")

            ), #conditional parenthesis

            #when data entry method is enter, allow user to enter data
            conditionalPanel(

              condition = "input['cover-input_method'] == 'enter'",

              #delete species button
              actionButton(ns( "delete_species"), "Delete Species"),
              br(),
              br(),
              #button to delete all entries
              actionButton(ns( "delete_all"), "Delete All Entries", class = "btn-danger")

            )#conditional parenthesis

          ),#sidebarPanel parenthesis

          mainPanel(

            textOutput(ns( "next_condition")),
            textOutput(ns( "complete_acronym")),

            #when user wants to upload a file but hasn't yet, show instructions
            conditionalPanel("input['cover-input_method'] == 'upload' && output['cover-file_is_uploaded'] != true",
                             br(),
                             h3("File uploads must have one column containing either scientific names
                              or acronyms. The columns must go to the top of the file such that row 1
                              is the column name."),
                             br(),
                             #temporary download file button
                             a(href="www/crooked_island.xlsx", "ESA 2023 Download Sample Data Here!",
                               download=NA, target="self")
                             ),

            #when user uploads file, show uploaded table
            conditionalPanel("input['cover-input_method'] == 'upload'",
                             br(),
                             br(),
                             DT::dataTableOutput(ns( "upload_table"))),

            #when user wants enters species manually, show widgets and data entered
            conditionalPanel(
              condition = "input['cover-input_method'] == 'enter'",

              #widgets for data entry
              fluidRow(
                column(4, textInput(ns( "transect_id"), "Transect ID"))),
              fluidRow(
                column(2, textInput(ns( "plot_id"), "Plot ID")),
                column(4, selectizeInput(ns( "select_species"), label = "Select Species",
                                         choices = NULL, selected = NULL)),
                column(3, uiOutput(ns("cover_value"))),
                column(3, shinyjs::disabled(actionButton(ns( "add_species"), "Add Species",
                                                style = "margin-top: 30px; height: 40px;")))),
              #table of data entered
              fluidRow(
                column(12,DT::dataTableOutput(ns( "cover_DT_manual")))),

            )#conditional panel parenthesis

          )#main panel parenthesis

        )#fluid row parenthesis

      ),#screen 1 parenthesis

      shinyglide::screen(

        #download button
        downloadButton(ns("download"),
                       label = "Download", class = "downloadButton",
                       style = "position: absolute; top: 0px; right: 10px;"),
        br(),
        #title
        column(12, align = "center",
               h3(textOutput(ns( "title")))),


        #boxes with key values
        bslib::layout_column_wrap(
          width = 1/3,
          bslib::value_box(
            title = "Species Richness",
            value = htmlOutput(ns("species_richness")),
            showcase = icon("seedling", class = "fa-3x")
          ),
          bslib::value_box(
            title = "Mean C",
            value = htmlOutput(ns("mean_c")),
            showcase = icon("pagelines", class = "fa-3x")
          ),
          bslib::value_box(
            title = "Total FQI",
            value = htmlOutput(ns("fqi")),
            showcase = icon("spa", class = "fa-3x")
          )
        ),
        br(),

        #histograms
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header("Binned Histogram of C Values"),
            bslib::card_body(plotOutput(ns("binned_c_score_plot")))
          ),
          bslib::card(
            bslib::card_header("Histogram of C Values"),
            bslib::card_body(plotOutput(ns("c_hist")))
          )
        ),
        br(),

        #1st row of small tables
        bslib::layout_column_wrap(
          width = 1/3,
          bslib::card(
            bslib::card_header("FQI Metrics"),
            bslib::card_body(tableOutput(ns("c_metrics")))
          ),
          bslib::card(
            bslib::card_header("Cover-Weighted Metrics"),
            bslib::card_body(tableOutput(ns("cover_classs")))
          ),
          bslib::card(
            bslib::card_header("Species Richness Metrics"),
            bslib::card_body(tableOutput(ns("species_mets")))
          )
        ),
        br(),

        #2nd row of small tables
        bslib::layout_column_wrap(
          width = 1/3,
          bslib::card(
            bslib::card_header("Wetness Metrics"),
            bslib::card_body(tableOutput(ns("wetness")))
          ),
          bslib::card(
            bslib::card_header("Duration Metrics"),
            bslib::card_body(tableOutput(ns("duration_table")))
          ),
          bslib::card(
            bslib::card_header("C Value Percentages"),
            bslib::card_body(tableOutput(ns("proportion")))
          )
        ),
        br(),

        #output of terms
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Relative Cover Terms"),
            bslib::card_body(includeMarkdown("rmarkdowns/rel_terms.Rmd"))
            )
          ),
        br(),

        #output of physiog summary
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Physiognomy Summary"),
            bslib::card_body(tableOutput(ns( "cover_physiog_manual")))
          )
        ),
        br(),

        #output of plot summary
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Plot Summary"),
            bslib::card_body(DT::dataTableOutput(ns( "cover_plot_manual")))
          )
        ),
        br(),

        #output of species summary
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Species Summary"),
            bslib::card_body(DT::dataTableOutput(ns("cover_species_manual")))
          )
        ),
        br(),

        #output of accepted entries
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Data Entered"),
            bslib::card_body(DT::dataTableOutput(ns("accepted")))
          )
        ),
        br()

      )#screen two parenthesis

    )#glide parenthesis

  )
}

#' cover Server Functions
#'
#' @noRd
mod_cover_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #creating a reactive value for glide page, used as input to server fun
    cover_glide <- reactive({input$shinyglide_index_glide})

    #making input method reactive
    input_method <- reactive({input$input_method})

    #reactive key
    key <- reactiveVal()

    #update key
    observe({
      if(any(is.na(fqacalc::view_db(input$db)$acronym) &
             fqacalc::view_db(input$db)$name_origin == "accepted_scientific_name"))
        key("name")
      else(key(input$key))
    })

    #initialize reactives to hold data entered/uploaded
    file_upload <- reactiveVal({data.frame()})
    data_entered <- reactiveVal({data.frame()})

    #help popup
    observeEvent(input$help, {
      cover_help()
    })

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
        selectizeInput(ns("cover_val"), "Cover Value", c("", cover_vals),
                       selected = NULL,
                       multiple = FALSE) }
      #else allow numeric input
      else { numericInput(ns("cover_val"), "Cover Value",
                          value = 0, min = 0, max = 100)}
    })

    #create reactive for complete acronym test
    complete_acronym <- reactiveVal({})

    #test if db contains complete acronyms (T/F), store in reactive
    observeEvent(input$db, {
      regional_fqai <- fqacalc::view_db(input$db)
      acronym <- if( any(is.na(regional_fqai$acronym)
                         & regional_fqai$name_origin == "accepted_scientific_name") )
      {FALSE} else {TRUE}
      complete_acronym(acronym)
    })

    #create complete acronym output
    output$complete_acronym <- renderText(
      complete_acronym()
    )

    #hide complete_acronym output
    observe({shinyjs::hide("complete_acronym",)})
    outputOptions(output, "complete_acronym", suspendWhenHidden=FALSE)

    #file upload server-------------------------------------------------------------

    #init cover column check reactive
    cover_column_is_good <- reactiveVal(FALSE)

    #init columns are good check reactive
    columns_are_good <- reactiveVal(FALSE)

    #init reactive to produce list of column names of file upload
    column_names <- reactiveVal()

    #When file is uploaded, upload and store in reactive object above
    observeEvent(input$upload, {
      #require that a file be uploaded
      req(input$upload)
      #getting extension
      ext <- tools::file_ext(input$upload$name)
      #reading in differently based on extension
      new_file <- switch(ext,
                         csv = vroom::vroom(input$upload$datapath, delim = ","),
                         tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
                         xlsx = readxl::read_excel(input$upload$datapath),
                         validate("Invalid file; Please upload a .csv, .tsv, or .xlsx file")) %>%
        #drop empty data
        dplyr::filter(., rowSums(is.na(.)) != ncol(.)) %>%
        as.data.frame(.)
      #store upload in reactive object
      file_upload(new_file)
    })

    #if file is uploaded, show T, else F
    output$file_is_uploaded <- reactive({
      return(dim(file_upload())[1] != 0)
    })
    #send to UI for conditional panel, but hide it
    outputOptions(output, "file_is_uploaded", suspendWhenHidden = FALSE)

    #store column names in reactive values
    observe({
      req(input$upload)
      column_names(c("", colnames(file_upload())))
    })

    #drop-down list (for species column) based on the file uploaded
    observeEvent(input$upload,{
      output$species_colname <- renderUI({
        #create key variable
        key_var <- if(key() == "name") {"Scientific Names"} else {"Acronyms"}
        #create a dropdown option
        selectizeInput(ns("species_column"),
                       paste0("Which Column Contains ", key_var, "?"),
                       column_names(), selected = NULL)
      })
    })

    #drop-down list (for cover column) based on the file uploaded
    observeEvent(input$upload,{
      output$cover_colname <- renderUI({
        #create a dropdown option
        selectizeInput(ns("cover_column"), "Which Column Contains Cover Data?",
                       column_names(), selected = NULL)
      })
    })

    #drop-down list (for plot-id column) based on the file uploaded
    observeEvent(input$upload,{
      output$plot_colname <- renderUI({
        #create a dropdown option
        selectizeInput(ns("plot_column"), "Which Column Contains Plot IDs?",
                       c("NA", column_names()), selected = "")
      })
    })

    #render output table from uploaded file
    output$upload_table <- DT::renderDT({
      DT::datatable(file_upload(),
                selection = 'single',
                options = list(autoWidth = TRUE,
                               scrollX = TRUE,
                               searching = FALSE,
                               lengthChange = FALSE)
      )
    })

    #check cover values
    observe({
      req(nrow(file_upload()) > 0, input$cover_column)
      cover_vals <- c(file_upload()[,input$cover_column])
      cover_column_is_good(
        if(input$cover_method == "percent_cover"
           & is.numeric(cover_vals)
           & max(cover_vals) <= 100
           & min(cover_vals) > 0) {TRUE}
        else if(input$cover_method == "braun-blanquet" &
                !any(!cover_vals %in% c("+", "1":"5"))) {TRUE}
        else if(input$cover_method == "daubenmire" &
                !any(!cover_vals %in% c("1":"6"))) {TRUE}
        else if(input$cover_method == "carolina_veg_survey" &
                !any(!cover_vals %in% c("1":"10"))) {TRUE}
        else if(input$cover_method == "usfs_ecodata" &
                !any(!cover_vals %in% c("1", "3", "10", "20", "30", "40", "50",
                                        "60", "70", "80", "90", "98"))) {TRUE}
        else {FALSE})
    })

    #check input of columns
    observe({
      req(nrow(file_upload()) > 0, input$species_column, input$cover_column, input$plot_column)
      columns <- c(input$species_column, input$cover_column, input$plot_column)
      if ( length(unique(columns)) == 3 &
           !input$species_column %in% c("cover", "plot_id") &
           !input$cover_column %in% c("plot_id", "name", "acronym") &
           !input$plot_column %in% c("cover", "name", "acronym"))
      { columns_are_good(TRUE) }
      else ( columns_are_good(FALSE) )

      #send alert if columns need fixing
      if(columns_are_good() == FALSE) {
        showModal(modalDialog("The columns selected for species,
                               cover, or plot ID must be unique.
                               Additionally, the species column cannot
                               be set to 'cover' or 'plot_id', the cover column
                               cannot be set to 'plot_id', 'name' or 'acronym',
                               and the plot ID column cannot be set to 'cover',
                               'name' or 'acronym'"))
      }
    })

    #warnings for bad data in file upload
    observe({

      req(columns_are_good() == TRUE)

      plot_col <- if(input$plot_column == "NA"){NULL} else {input$plot_column}
      #list to store warnings
      warning_list <- list()
      #file upload renames
      upload_renamed <- file_upload() %>%
        dplyr::rename(
          "cover" = input$cover_column,
          !!as.name(key()) := !!input$species_column
          )

      #catch warnings
      withCallingHandlers(
        fqacalc::accepted_entries(x = upload_renamed,
                                  key = key(),
                                  db = input$db,
                                  native = FALSE,
                                  cover = TRUE,
                                  allow_duplicates = TRUE,
                                  cover_class = input$cover_method,
                                  allow_no_c = FALSE,
                                  allow_non_veg = TRUE,
                                  plot_id = plot_col),
        #add to list
        message=function(w) {warning_list <<- c(warning_list, list(w$message))})
      #show each list item in notification
      for(i in warning_list) {
        showNotification(ui = i,
                         duration = NULL,
                         type = "error") }
    })

    #when delete all is clicked, clear all entries
    observeEvent(input$upload_delete_all, {
      #make an empty df
      empty_df <- data.frame()
      #replace reactive file upload with empty file
      file_upload(empty_df)
      accepted(empty_df)
      columns_are_good(FALSE)
      #reset upload button
      shinyjs::reset("upload")
      shinyjs::reset("species_column")
      shinyjs::reset("cover_column")
      shinyjs::reset("plot_column")
    })




    #manually enter data------------------------------------------------------------

    #species drop-down list based on regional database selected
    observe({
      req(input$db)
      #create list names based on regional database selected
      names <- if(key() == "name")
      {c("", "UNVEGETATED GROUND", "UNVEGETATED WATER", unique(fqacalc::view_db(input$db)$name))}
      else {c("", "GROUND", "WATER", unique(fqacalc::view_db(input$db)$acronym))}
      #create a dropdown option
      updateSelectizeInput(session, "select_species",
                           choices =  names,
                           selected = character(0),
                           server = TRUE)
    })

    #make it so add species button can't be clicked until all fields full
    observe({
      vals <- c(input$transect_id, input$cover_val, input$select_species, input$plot_id)
      if (input$cover_method != "percent_cover") {
        shinyjs::toggleState("add_species", !"" %in% vals) }
      else { shinyjs::toggleState("add_species", input$cover_val > 0 & input$cover_val <= 100)}
    })

    #make it so transect cant be changed after the fact
    observe({
      shinyjs::toggleState("transect_id", nrow(data_entered()) == 0)
    })

    #When add species is clicked, add row
    observeEvent(input$add_species, {
      #create df with data entered
      if(key() == "name") {
        new_entry <- data.frame(plot_id = c(input$plot_id),
                                acronym = c(NA),
                                name = c(input$select_species),
                                cover = c(cover_conversion(input$cover_val, input$cover_method)))
      } else {
        new_entry <- data.frame(plot_id = c(input$plot_id),
                                acronym = c(input$select_species),
                                name = c(NA),
                                cover = c(cover_conversion(input$cover_val, input$cover_method)))
      }
      #bind new entry to table
      if(nrow(accepted() > 0)) {
        new_entry<- rbind(new_entry, accepted() %>%
                            dplyr::select(plot_id, acronym, name, cover))
      }
      #make it reactive
      data_entered(new_entry)
      #reset species drop down from scratch
      names <- if(key() == "name")
      {c("", "UNVEGETATED GROUND", "UNVEGETATED WATER", unique(fqacalc::view_db(input$db)$name))}
      else {c("", "GROUND", "WATER", unique(fqacalc::view_db(input$db)$acronym))}

      updateSelectizeInput(session, "select_species",
                           choices =  names,
                           selected = character(0),
                           server = TRUE)

      #reset cover value
      shinyjs::reset("cover_val")
      #reset so cursor is on plot id widget
      shinyjs::js$refocus("cover-plot_id")
    })

    #this allows popups for warnings about duplicates/non-matching species
    observeEvent(input$add_species,{
      nrow(data_entered()) > 0
      #list to store warnings
      warning_list <- list()
      #catch warnings
      withCallingHandlers(
        fqacalc::accepted_entries(x = data_entered(),
                                  key = key(),
                                  db = input$db,
                                  native = FALSE,
                                  cover = TRUE,
                                  allow_duplicates = TRUE,
                                  plot_id = "plot_id",
                                  cover_class = "percent_cover",
                                  allow_no_c = FALSE,
                                  allow_non_veg = TRUE,
                                  wetland_warning = FALSE),
        #add to list
        message=function(w) {warning_list <<- c(warning_list, list(w$message))})
      #show each list item in notification
      for(i in warning_list) {
        showNotification(ui = i,
                         duration = NULL,
                         type = "error") }
    })

    #render output table from manually entered species on data entry page
    output$cover_DT_manual <- DT::renderDT({
      DT::datatable(accepted(),
                selection = 'single',
                options = list(
                  scrollX = TRUE,
                  searching = FALSE,
                  lengthChange = FALSE))
    })

    #when delete species is clicked, delete row
    observeEvent(input$delete_species,{
      #call table
      t = accepted()
      #print table
      print(nrow(t))
      #if rows are selected, delete them
      if (!is.null(input$cover_DT_manual_rows_selected)) {
        t <- t[-as.numeric(input$cover_DT_manual_rows_selected),]
      }
      #else show the regular table
      accepted(t)
    })

    #when delete all is clicked, clear all entries
    observeEvent(input$delete_all, {
      empty_df <- data.frame()
      data_entered(empty_df)
      accepted(empty_df)
    })

    ##accepted df ------------------------------------------------------------------

    #initialize reactives
    accepted <- reactiveVal(data.frame())
    confirm_db <- reactiveVal("empty")
    confirm_cover <- reactiveVal("empty")
    previous_dbs <- reactiveValues(prev = "michigan_2014")
    previous_covers <- reactiveValues(prev = "percent_cover")

    #store current and previous db value in reactive element
    observeEvent(input$db, {
      previous_dbs$prev <- c(tail(previous_dbs$prev, 1), input$db)
    })

    #store current and previous cover method value in reactive element
    observeEvent(input$cover_method, {
      previous_covers$prev <- c(tail(previous_covers$prev, 1), input$cover_method)
    })

    #if input method is enter, accepted is from data_entered
    observe({
      #set accepted to be empty
      req(input_method() == "enter")
      accepted(data.frame())

      req(input_method() == "enter" & nrow(data_entered()) > 0)
      suppressMessages(accepted(fqacalc::accepted_entries(x = data_entered(),
                                                          key = key(),
                                                          db = input$db,
                                                          native = FALSE,
                                                          cover = TRUE,
                                                          allow_duplicates = TRUE,
                                                          plot_id = "plot_id",
                                                          cover_class = "percent_cover",
                                                          allow_no_c = TRUE,
                                                          allow_non_veg = TRUE)))
    })

    #if input method is upload, accepted is from file_uploaded
    observe({
      #require file to be uploaded
      req(input_method() == "upload")
      #set accepted to be empty
      accepted(data.frame())
      #require columns to be good
      req( columns_are_good(),
           nrow(file_upload()) > 0,
           input$species_column != "",
           input$cover_column != "",
           input$plot_column != "")

      #if plot column is set, include plot column
      if( input$plot_column != "NA" ) {

        suppressMessages(accepted(fqacalc::accepted_entries(x = file_upload() %>%
                                                              dplyr::rename(!!key() := !!input$species_column,
                                                                            cover = input$cover_column,
                                                                            plot_id = input$plot_column),
                                                            key = key(),
                                                            db = input$db,
                                                            native = FALSE,
                                                            cover = TRUE,
                                                            allow_duplicates = TRUE,
                                                            cover_class = input$cover_method,
                                                            allow_no_c = TRUE,
                                                            allow_non_veg = TRUE,
                                                            plot_id = "plot_id"))) }
      else {

        #if plot column is not set, do not include in accepted entries
        suppressMessages(accepted(fqacalc::accepted_entries(x = file_upload() %>%
                                                              dplyr::rename(!!key() := !!input$species_column,
                                                                            cover = input$cover_column),
                                                            key = key(),
                                                            db = input$db,
                                                            native = FALSE,
                                                            cover = TRUE,
                                                            allow_duplicates = TRUE,
                                                            cover_class = input$cover_method,
                                                            allow_no_c = TRUE,
                                                            allow_non_veg = TRUE)))
      }
    })

    #if db is changed and there is already data entered, show popup
    observeEvent(input$db, {
      req(nrow(data_entered()) > 0 || nrow(file_upload()) > 0)
      #code for popup
      if(confirm_db() != "empty") {
        confirm_db("empty") }
      else{
        showModal(modalDialog(
          "Changing the regional database will delete your current data entries.
          Are you sure you want to proceed?",
          footer = tagList(actionButton(ns("confirm_db_change"), "Proceed",
                                        class = "btn-danger"),
                           actionButton(ns("cancel_db_change"), "Cancel"))
          ))
          }
    })

    observeEvent(input$confirm_db_change, {
      #store confirmation in reactive value
      confirm_db(TRUE)
      empty_df <- data.frame()
      data_entered(empty_df)
      file_upload(empty_df)
      accepted(empty_df)
      shinyjs::reset("upload")
      shinyjs::reset("species_column")
      shinyjs::reset("cover_column")
      shinyjs::reset("plot_column")
      removeModal()
      confirm_db("empty")
    })

    #if confirm db is false, reset db to previous value
    observeEvent(input$cancel_db_change, {
      confirm_db(FALSE)
      updateSelectInput(session, inputId = "db",
                        selected = previous_dbs$prev[1])
      removeModal()
    })

    #if cover method is changed and there is already data entered, show popup
    observeEvent(input$cover_method, {
      req(nrow(data_entered()) > 0 || nrow(file_upload()) > 0)
      #code for popup
      if(confirm_cover() != "empty") {
        confirm_cover("empty") }
      else{
        showModal(modalDialog(
          "Changing the cover method will delete your current data entries.
        Are you sure you want to proceed?",
          footer = tagList(actionButton(ns("confirm_cover_change"), "Proceed",
                                        class = "btn-danger"),
                           actionButton(ns("cancel_cover_change"), "Cancel"))
        ))
        }
    })

    observeEvent(input$confirm_cover_change, {
      #store confirmation in reactive value
      confirm_cover(TRUE)
      empty_df <- data.frame()
      data_entered(empty_df)
      file_upload(empty_df)
      accepted(empty_df)
      shinyjs::reset("upload")
      shinyjs::reset("species_column")
      shinyjs::reset("cover_column")
      shinyjs::reset("plot_column")
      removeModal()
      confirm_cover("empty")
    })

    #if confirm cover is false, reset cover to previous value
    observeEvent(input$cancel_cover_change, {
      confirm_cover(FALSE)
      updateSelectInput(session, inputId = "cover_method",
                        selected = previous_covers$prev[1])
      removeModal()
    })

    #wetland warnings
    observeEvent(list(input$db,input$confirm_db_change, input$confirm_cover_change),
                 ignoreInit = TRUE, {
      req( if(input_method() == "enter") {dim(data_entered())[1] == 0}
           else {dim(file_upload())[1] == 0})
      if( all(is.na(fqacalc::view_db(input$db)$w)) ) {
        showModal(modalDialog(paste(input$db, "does not have wetland coefficients,
                                       wetland metrics cannot be calculated.")))
      }
      if( input$db == "wyoming_2017") {
        showModal(modalDialog("The Wyoming FQA database is associated with multiple
                                 wetland indicator status regions. This package defaults
                                 to the Arid West wetland indicator region when
                                 calculating Wyoming metrics."))
      }
      if ( input$db == "colorado_2020" ){
        showModal(modalDialog(title = icon("circle-exclamation"),
        "The Colorado FQA database is associated with
                               multiple wetland indicator status regions. This
                               package defaults to the Western Mountains,
                               Valleys, and Coasts indicator region when calculating
                               Colorado metrics."))
      }
    })

    #create boolean that shows if data is entered or not for next condition
    output$next_condition <- renderText(
      nrow(accepted()) > 0
    )

    #hide next condition output
    observe({shinyjs::hide("next_condition",)})
    outputOptions(output, "next_condition", suspendWhenHidden=FALSE)

    ##second screen-----------------------------------------------------------------

    #initializing reactives for outputs
    metrics <- reactiveVal()
    physiog_table <- reactiveVal()
    duration_table <- reactiveVal()
    species_sum <- reactiveVal()
    plot_sum <- reactiveVal()
    physiog_sum <- reactiveVal()
    #data_download <- reactiveVal()

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
        csv_path <- file.path(tmpdir, "FQI_metrics.csv")

        # Start a sink file with a CSV extension
        sink(csv_path)
        cat('\n')
        cat(paste0("Calculating metrics based on the ", input$db, " regional FQA database for transect ", input$transect_id))
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        write.csv(metrics() %>%
                    dplyr::mutate(values = round(values, digits = 2)), row.names = F)
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        cat("Duration Frequency")
        cat('\n')
        write.csv(duration_table() %>%
                    dplyr::mutate(percent = round(percent, digits = 2)), row.names = F)
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        if(!is.null(plot_sum())){
          cat('Plot Summary Metrics')
          cat('\n')
          write.csv(plot_sum() %>%
                      dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 2)), row.names = F)
          cat('\n')
          cat('\n')
        }

        # Write metrics dataframe to the same sink
        cat('Species Summary Metrics')
        cat('\n')
        write.csv(species_sum() %>%
                    dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 2)), row.names = F)
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        cat('Physiognomy Summary Metrics')
        cat('\n')
        write.csv(physiog_sum() %>%
                    dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 2)), row.names = F)
        cat('\n')
        cat('\n')

        # Write data entered
        cat('Species Entered')
        cat('\n')
        write.csv(accepted(), row.names = F)

        # Close the sink
        sink()

        #now add two ggplots as pngs
        ggplot2::ggsave(filename = file.path(tmpdir, "binned_hist.png"),
                        plot = binned_c_score_plot(metrics()),
                        device = "png", bg = "white")
        ggplot2::ggsave(filename = file.path(tmpdir, "c_value_hist.png"),
                        plot = c_score_plot(accepted()),
                        device = "png", bg = "white")

        # Zip them up
        zip::zip(file,
                 file.path(tmpdir, c("FQI_metrics.csv", "binned_hist.png", "c_value_hist.png")),
                 mode = "cherry-pick")
      })

    #updating reactive values
    observe({
      #requiring second screen to update reactive values
      req(cover_glide() == 1)

      withProgress(message = "Calculating FQA metrics..", {
        incProgress(1)
        metrics(suppressMessages(fqacalc::transect_summary(x = accepted(),
                                                           key = "name",
                                                           db = input$db,
                                                           cover_class = "percent_cover",
                                                           allow_no_c = TRUE)))

        incProgress(1)
        species_sum(suppressMessages(fqacalc::species_summary(x = accepted(),
                                                              key = "name",
                                                              db = input$db,
                                                              cover_class = "percent_cover",
                                                              allow_no_c = TRUE))
        )

        incProgress(1)
        physiog_sum(suppressMessages(fqacalc::physiog_summary(x = accepted(),
                                                              key = "name",
                                                              db = input$db,
                                                              cover_class = "percent_cover",
                                                              allow_no_c = TRUE)))
        incProgress(1)
        if(input$input_method == "enter") {
          if(length(unique(accepted()$plot_id)) == 1) {
            plot_sum(NULL)} else {
              plot_sum(suppressMessages(fqacalc::plot_summary(x = accepted(),
                                                              key = "name",
                                                              db = input$db,
                                                              cover_class = "percent_cover",
                                                              plot_id = "plot_id",
                                                              allow_no_c = TRUE))) }
        }
        incProgress(1)
        if (input$input_method == "upload") {
          if(input$plot_column == "NA") {
            plot_sum(NULL)} else {
              plot_sum(suppressMessages(fqacalc::plot_summary(x = file_upload()
                                                              %>% dplyr::rename(!!as.name(key()) := input$species_column,
                                                                                cover = input$cover_column,
                                                                                plot_id = input$plot_column),
                                                              key = key(),
                                                              db = input$db,
                                                              cover_class = input$cover_method,
                                                              plot_id = "plot_id",
                                                              allow_no_c = TRUE)))
            }
        }

        incProgress(1)
        duration_cats <- data.frame(duration = c("annual", "perennial", "biennial"),
                                    number = rep.int(0, 3),
                                    percent = rep.int(0,3))

        dur <- accepted() %>%
          dplyr::group_by(duration) %>%
          dplyr::summarise(number = dplyr::n()) %>%
          dplyr::mutate(percent = round((number/sum(number))*100, 2)) %>%
          rbind(duration_cats %>% dplyr::filter(!duration %in% accepted()$duration)) %>%
          dplyr::mutate(number = as.integer(number))
        incProgress(1)
        #store in reactive
        duration_table(dur)
      })
    })

    #render title
    output$title <- renderText({paste("Calculating metrics based on",
                                      input$db, "for transect",
                                      input$transect_id)})

    #species richness
    output$species_richness <- renderUI({
      req(cover_glide() == 1)
      shiny::p(
        suppressMessages(fqacalc::species_richness(x = accepted(),
                                                   db = input$db,
                                                   native = F,
                                                   allow_no_c = TRUE)),
        style = "font-size: 40px;"
      )
    })

    #mean C
    output$mean_c <- renderUI({
      req(cover_glide() == 1)
      shiny::p(
        round(suppressMessages(fqacalc::mean_c(x = accepted(),
                                               db = input$db,
                                               native = F)), 2),
        style = "font-size: 40px;"
      )
    })

    #total fqi
    output$fqi <- renderUI({
      req(cover_glide() == 1)
      shiny::p(
        round(suppressMessages(fqacalc::FQI(x = accepted(),
                                            db = input$db,
                                            native = F)), 2),
        style = "font-size: 40px;"
      )
    })

    #C metrics table output
    output$c_metrics <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Mean C", "Native Mean C", "Total FQI",
                                     "Native FQI", "Adjusted FQI"))
    })

    #cover metrics table output
    output$cover_classs <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Cover-Weighted Mean C", "Cover-Weighted Native Mean C",
                                     "Cover-Weighted FQI", "Cover-Weighted Native FQI"))
    })

    #wetness table output
    output$wetness <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Mean Wetness", "Native Mean Wetness", "% Hydrophytes"))
    })

    #nativity table output
    output$species_mets <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Total Species Richness",
                                     "Native Species Richness",
                                     "Exotic Species Richness")) %>%
        dplyr::mutate(values = as.integer(values))
    })

    #proportion table output
    output$proportion <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("% of Species with no C Value",
                                     "% of Species with 0 C Value",
                                     "% of Species with 1-3 C Value",
                                     "% of Species with 4-6 C Value",
                                     "% of Species with 7-10 C Value"))
    })

    #ggplot output
    output$c_hist <- renderPlot({
      req(cover_glide() == 1)
      c_score_plot(accepted() %>%
                     dplyr::filter(!acronym %in% c("WATER", "GROUND")))
    })

    #ggplot output
    output$binned_c_score_plot <- renderPlot({
      req(cover_glide() == 1)
      binned_c_score_plot(input_data = metrics())
    })

    #duration table output
    output$duration_table <- renderTable({
      req(cover_glide() == 1)
      duration_table()
    })

    #plot summary
    output$cover_plot_manual <- DT::renderDataTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive plot summary
      DT::datatable(
        if( is.null(plot_sum()) ) {NULL}
        else{
          plot_sum() %>%
            dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 2))
        },
        #options
        options = list(scrollX=TRUE,
                       scrollY= TRUE,
                       paging = TRUE,
                       pageLength = 20,
                       searching = TRUE,
                       fixedColumns = TRUE,
                       autoWidth = TRUE,
                       ordering = TRUE))
    })

    #species summary
    output$cover_species_manual <- DT::renderDataTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive species summary
      DT::datatable(species_sum() %>%
                  dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 2)),
                #options
                options = list(scrollX=TRUE,
                               scrollY= TRUE,
                               paging = TRUE,
                               pageLength = 20,
                               searching = TRUE,
                               fixedColumns = TRUE,
                               autoWidth = TRUE,
                               ordering = TRUE))
    })

    #physiog summary
    output$cover_physiog_manual <- renderTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive species summary
      physiog_sum()
    })

    #accepted summary
    output$accepted <- DT::renderDataTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive species summary
      DT::datatable(accepted(),
                    #options
                    options = list(scrollX=TRUE,
                                   scrollY= TRUE,
                                   paging = TRUE,
                                   pageLength = 20,
                                   searching = TRUE,
                                   fixedColumns = TRUE,
                                   autoWidth = TRUE,
                                   ordering = TRUE))
    })

  })
}

## To be copied in the UI
# mod_cover_ui("cover_1")

## To be copied in the server
# mod_cover_server("cover_1")
