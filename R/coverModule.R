#UI------------------------------------------------------------------------

coverUI <- function(id) {

  tagList(

    glide(
      id = NS(id, "glide"),
      next_label = paste("Calculate FQA Metrics ", icon("arrow-right")),
      previous_label = paste(icon("arrow-left"), "Go Back to Data Entry"),
      controls_position = "bottom",
      height = "100%",

      screen(
        next_condition = "output['cover-next_condition'] == 'TRUE'",

        fluidRow(
          sidebarPanel(

            #title of side bar
            titlePanel("Enter Data"),

            #help button
            circleButton(NS(id, "help"), icon = icon("question"),
                         style = "position:absolute; top:5px; right:5px;",
                         status = "primary"),

            #input regional data base
            selectInput(NS(id, "db"), label = "Select Regional FQAI Database",
                        choices = fqacalc::db_names()$name,
                        selected = "michigan_2014"),

            #select cover method
            selectInput(NS(id, "cover_method"), label = "Cover Method",
                        choices = c(
                          "percent_cover",
                          "braun-blanquet",
                          "carolina_veg_survey",
                          "daubenmire",
                          "usfs_ecodata")),

            #input key argument
            radioGroupButtons(NS(id, "key"), label = "Join by: ",
                              choices = c("Scientific Names" = "scientific_name",
                                          "Acronyms" = "acronym"),
                              justified = TRUE,
                              checkIcon = list(yes = icon("ok",
                                                          lib = "glyphicon"))),

            #input data entry method
            prettyRadioButtons(NS(id, "input_method"), label = "Select Data Entry Method",
                         choices = c( "Enter Species Manually" = "enter",
                                      "Upload a File" = "upload")),


            #when data entry method is upload, allow user to upload files
            conditionalPanel(

              condition = "input['cover-input_method'] == 'upload'",

              #input file upload widget
              fileInput(NS(id, "upload"), NULL, buttonLabel = "Upload...", multiple = F),

              #input what column to use to bind to FQA database
              uiOutput(NS(id, "species_colname")),

              #input what column to use as cover
              uiOutput(NS(id, "cover_colname")),

              #input what column to use as plot id
              uiOutput(NS(id, "plot_colname")),

              #input button to delete uploaded file
              actionButton(NS(id, "upload_delete_all"), "Delete Uploaded File")

            ), #conditional parenthesis

            #when data entry method is upload, allow user to upload files
            conditionalPanel(

              condition = "input['cover-input_method'] == 'enter'",

              #delete species button
              actionButton(NS(id, "delete_species"), "Delete Species"),
              br(),
              br(),
              #button to delete all entries
              actionButton(NS(id, "delete_all"), "Delete All Entries", class = "btn-danger")

            )#conditional parenthesis

          ),#sidebarPanel parenthesis

          mainPanel(

            textOutput(NS(id, "next_condition")),

            #when user wants to upload a file but hasn't yet, show instructions
            conditionalPanel("input['cover-input_method'] == 'upload' && output['cover-file_is_uploaded'] != true",
                             br(),
                             h3("File uploads must have one column containing either scientific names
                              or acronyms. The column must go to the top of the file such that row 1
                              is the column name.")),

            #when user uploads file, show uploaded table
            conditionalPanel("input['cover-input_method'] == 'upload'",
                             br(),
                             br(),
                             dataTableOutput(NS(id, "upload_table"))),

            #when user wants enters species manually, show widgets and data entered
            conditionalPanel(
              condition = "input['cover-input_method'] == 'enter'",

              #widgets for data entry
              fluidRow(
                column(4, textInput(NS(id, "transect_id"), "Transect ID"))),
              fluidRow(
                column(2, textInput(NS(id, "plot_id"), "Plot ID")),
                column(4, uiOutput(NS(id, "select_species"))),
                column(3, uiOutput(NS(id,"cover_value"))),
                column(3, disabled(actionButton(NS(id, "add_species"), "Add Species",
                                                style = "margin-top: 30px; height: 40px;")))),
              #table of data entered
              fluidRow(
                column(12,dataTableOutput(NS(id, "cover_DT_manual"))))

            )#conditional panel parenthesis

          )#main panel parenthesis

        )#fluid row parenthesis

      ),#screen 1 parenthesis

      screen(

        #download button
        downloadButton(NS(id, "download"),
                       label = "Download", class = "downloadButton",
                       style = "position: absolute; top: 0px; right: 0px;"),
        br(),
        #title
        column(12, align = "center",
               h3(textOutput(NS(id, "title")))),


        #boxes with key values
        fluidRow(
          valueBox(
            htmlOutput(NS(id,"species_richness")),
            "Species Richness", color = "navy"
          ),
          valueBox(
            htmlOutput(NS(id,"mean_c")),
            "Mean C",
            icon = icon("seedling"), color = "olive"
          ),
          valueBox(
            htmlOutput(NS(id,"fqi")),
            "Total FQI",
            icon = icon("pagelines"), color = "green"
          )
        ),#fluidRow parenthesis

        #all mets and graph
        fluidRow(
          box(plotOutput(NS(id,"binned_c_score_plot")),
              title = "Binned Histogram of C Values"),
          box(plotOutput(NS(id,"c_hist")),
              title = "Histogram of C Values")
        ),

        #small tables
        fluidRow(
          column(4,
                 box(tableOutput(NS(id,"c_metrics")), title = "FQI Metrics", width = NULL),
                 box(tableOutput(NS(id,"wetness")), title = "Wetness Metrics", width = NULL)),
          column(4,
                 box(tableOutput(NS(id,"cover_metrics")), title = "Cover-Weighted Metrics", width = NULL),
                 box(tableOutput(NS(id,"duration_table")), title = "Duration Metrics", width = NULL)),
          column(4,
                 box(tableOutput(NS(id,"species_mets")), title = "Species Richness Metrics", width = NULL),
                 box(tableOutput(NS(id,"proportion")), title = "C Value Percentages", width = NULL))
        ),

        #output of physiog summary
        fluidRow(box(title = "Physiognomy Summary", status = "primary",
                     tableOutput(NS(id, "cover_physiog_manual")), width = 12,
                     style = "overflow-x: scroll")),

        #output of species summary
        fluidRow(box(title = "Species Summary", status = "primary",
                     tableOutput(NS(id, "cover_species_manual")), width = 12,
                     style = "overflow-x: scroll")),

        #output of plot summary
        fluidRow(box(title = "Plot Summary", status = "primary",
                     tableOutput(NS(id, "cover_plot_manual")), width = 12, style = "overflow-x: scroll"))

      )#screen two parenthesis

    )#glide parenthesis

  )#taglist parenthesis

}

#Server-------------------------------------------------------------------------

coverServer <- function(id) {

  #start module function
  moduleServer(id, function(input, output, session) {

    #creating a reactive value for glide page, used as input to server fun
    cover_glide <- reactive({input$shinyglide_index_glide})

    #making input method reactive
    input_method <- reactive({input$input_method})

    #define table for data entered manually
    data_entered = data.frame()

    #initialize reactives to hold data entered/uploaded
    file_upload <- reactiveVal()
    data_entered <- reactiveVal({data_entered})

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
        selectizeInput(session$ns("cover_val"), "Cover Value", c("", cover_vals),
                       selected = NULL,
                       multiple = FALSE) }
      #else allow numeric input
      else { numericInput(session$ns("cover_val"), "Cover Value",
                          value = 0, min = 0, max = 100)}
    })

#file upload server-------------------------------------------------------------

    #if file is uploaded, show T, else F
    output$file_is_uploaded <- reactive({
      return(!is.null(input$upload))
    })
    outputOptions(output, "file_is_uploaded", suspendWhenHidden = FALSE)

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
        filter(., rowSums(is.na(.)) != ncol(.)) %>%
        as.data.frame(.)
      #store upload in reactive object
      file_upload(new_file)
    })

    #drop-down list (for species column) based on the file uploaded
    observeEvent(input$upload,{
      output$species_colname <- renderUI({
        #create list cols
        colnames <- c("", colnames(file_upload()))
        #create key variable
        key_var <- if(input$key == "scientific_name") {"Scientific Names"} else {"Acronyms"}
        #create a dropdown option
        selectizeInput(session$ns("species_column"),
                       paste0("Which Column Contains ", key_var, "?"),
                       colnames, selected = NULL)
      })
    })

    #drop-down list (for cover column) based on the file uploaded
    observeEvent(input$upload,{
      output$cover_colname <- renderUI({
        #create list cols
        colnames <- c("", colnames(file_upload()))
        #create a dropdown option
        selectizeInput(session$ns("cover_column"), "Which Column Contains Cover Data?",
                       colnames, selected = NULL)
      })
    })

    #drop-down list (for plot-id column) based on the file uploaded
    observeEvent(input$upload,{
      output$plot_colname <- renderUI({
        #create list cols
        colnames <- c("", colnames(file_upload()))
        #create a dropdown option
        selectizeInput(session$ns("plot_column"), "(Optional) Which Column Contains Plot IDs?",
                       colnames, selected = NULL)
      })
    })

    #render output table from uploaded file
    output$upload_table <- DT::renderDT({
      datatable(file_upload(),
                selection = 'single',
                options = list(autoWidth = TRUE,
                               scrollX = TRUE,
                               searching = FALSE,
                               lengthChange = FALSE)
      )
    })

    #when delete all is clicked, clear all entries
    observeEvent(input$upload_delete_all, {
      #make an empty df
      empty_df <- NULL
      #replace reactive file upload with empty file
      file_upload(empty_df)
      accepted(empty_df)
      #reset upload button
      shinyjs::reset("upload")
      shinyjs::reset("species_column")
    })

    #warnings for bad data in file upload
    observe({
      req(nrow(file_upload()) > 0, input$species_column, input$cover_column)
      accepted_file <- accepted_entries(file_upload() %>%
                                          rename(!!as.name(input$key) := input$species_column),
                                        key = input$key,
                                        db = input$db,
                                        native = FALSE,
                                        allow_no_c = TRUE,
                                        allow_duplicates = TRUE,
                                        allow_non_veg = TRUE)

      #if there are unrecognized plants, show warning
      for(i in c(file_upload()[,input$species_column])){
        if( !toupper(i) %in% accepted_file[,input$key]) {
          shinyalert(text = strong(paste("Species", i, "is not recognized. It will be removed.")),
                     type = "warning",  html = T, className = "alert")
        }
      }
      #if there plants with no c, show warning
      plants_no_c <- unassigned_plants(file_upload() %>%
                                         rename(!!as.name(input$key) := input$species_column),
                                       key = input$key,
                                       db = input$db)
      if( nrow(plants_no_c) > 0 ) {
        for(i in plants_no_c[, input$key]) {
          shinyalert(text = strong(paste("Species", i, "is recognized but has not been
                                         assigned a C score. It will be included in species
                                         richness and mean wetness metrics but excluded
                                         from mean C and FQI metrics.")),
                     type = "warning",  html = T, className = "alert")
        }
      }
      #if there are duplicate species in same plot, show warning
      if( !input$plot_column %in% c("", NULL) ) {
        if( any(duplicated(file_upload() %>%
                                       select(input$species_column, input$plot_column))) ){
        shinyalert(text = strong(
          "Duplicate species are detected. Duplicates will only be counted once."),
          type = "warning",  html = T, className = "alert")
    }
    }
    })

#manually enter data------------------------------------------------------------

    #species drop-down list based on regional list selected
    output$select_species <- renderUI({
      #create list names based on regional list selected
      names <- if(input$key == "scientific_name")
      {c("", unique(fqacalc::view_db(input$db)$scientific_name))}
      else {c("", unique(fqacalc::view_db(input$db)$acronym))}
      #create a dropdown option
      selectizeInput(session$ns("species"), "Select Species", names,
                     selected = NULL,
                     multiple = FALSE)
    })

    #make it so add species button can't be clicked until all fields full
    observe({
      vals <- c(input$transect_id, input$cover_val, input$species, input$plot_id)
      if (input$cover_method != "percent_cover") {
        toggleState("add_species", !"" %in% vals) }
      else { toggleState("add_species", input$cover_val > 0 & input$cover_val <= 100)}
    })

    #make it so transect cant be changed after the fact
    observe({
      toggleState("transect_id", nrow(data_entered()) == 0)
    })

    #When add species is clicked, add row
    observeEvent(input$add_species, {
      #create row with data entered
      if(input$key == "scientific_name") {
        new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                                  dplyr::filter(scientific_name %in% input$species)) }
      else {new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                                      dplyr::filter(acronym %in% input$species))}
      #add plot id and cover value
      new_entry <- new_entry %>%
        mutate(plot_id = input$plot_id, cover = input$cover_val)
      #bind new entry to table
      new_table = rbind(new_entry, data_entered())
      #make it reactive
      data_entered(new_table)
      #reset drop down menus
      shinyjs::reset("species")
      shinyjs::reset("cover_val")
    })

    #if there are duplicate species in same plot, show warning, delete dups
    observeEvent(input$add_species, {
      req(nrow(data_entered()) > 1)
      if( any(duplicated(data_entered() %>% select(plot_id, scientific_name))) ){
        shinyalert(text = strong(
          "Duplicate species are detected in the same plot.
          Duplicates will only be counted once."),
          type = "warning",  html = T, className = "alert")

        data_entered(data_entered()[!duplicated(data_entered()[c(1,2)]),])
      }
    })

    #if there are unassigned species show warning
    observeEvent(input$add_species, {
      plants_no_c <- unassigned_plants(data_entered(), db = input$db)

      if( nrow(plants_no_c) > 0 ){
        for(i in c(plants_no_c$scientific_name)) {
          shinyalert(text = strong(paste("Species", i, "is recognized but has not been
                                         assigned a C score. It will be included in species
                                         richness and mean wetness metrics but excluded
                                         from mean C and FQI metrics")),
                     type = "warning",  html = T, className = "alert")
        }
      }
    })

    #render output table from manually entered species on data entry page
    output$cover_DT_manual <- DT::renderDT({
      datatable(data_entered(),
                selection = 'single',
                options = list(
                  scrollX = TRUE,
                  searching = FALSE,
                  lengthChange = FALSE))
    })

    #when delete species is clicked, delete row
    observeEvent(input$delete_species,{
      #call table
      t = data_entered()
      #print table
      print(nrow(t))
      #if rows are selected, delete them
      if (!is.null(input$cover_DT_manual_rows_selected)) {
        t <- t[-as.numeric(input$cover_DT_manual_rows_selected),]
      }
      #else show the regular table
      data_entered(t)
    })

    #when delete all is clicked, clear all entries
    observeEvent(input$delete_all, {
      data_entered(data_entered)
      accepted(data_entered)
    })

    ##accepted df ------------------------------------------------------------------

    #initialize reactives
    accepted <- reactiveVal()
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
      req(input_method() == "enter")
      req(nrow(data_entered()) > 0)
      accepted(fqacalc::accepted_entries(x = data_entered(),
                                         key = "scientific_name",
                                         db = input$db,
                                         native = FALSE,
                                         cover = TRUE,
                                         allow_duplicates = TRUE,
                                         cover_metric = input$cover_method,
                                         allow_no_c = TRUE))
    })

    #if input method is upload, accepted is from file_uploaded
    observe({
      req(input_method() == "upload")
      accepted(data.frame())

      req(input_method() == "upload", nrow(file_upload()) > 0, input$species_column, input$cover_column)
      accepted(fqacalc::accepted_entries(x = file_upload() %>% rename(!!as.name(input$key) := input$species_column,
                                                                      cover = input$cover_column),
                                         key = input$key,
                                         db = input$db,
                                         native = FALSE,
                                         cover = TRUE,
                                         allow_duplicates = TRUE,
                                         cover_metric = input$cover_method,
                                         allow_no_c = TRUE))
    })

    #if db is changed and there is already data entered, show popup
    observeEvent(input$db, {
      req(nrow(accepted()) > 0)
      #code for popup
      if(confirm_db() != "empty") {
        confirm_db("empty") }
      else{
        shinyalert(text = strong(
          "Changing the regional database will delete your current data entries.
        Are you sure you want to proceed?"),
        showCancelButton = T,
        showConfirmButton = T, confirmButtonText = "Proceed",
        confirmButtonCol = "red", type = "warning",
        html = T, inputId = "confirm_db_change")}
    })

    observeEvent(input$confirm_db_change, {
      #store confirmation in reactive value
      confirm_db(input$confirm_db_change)
      #create an empty df
      empty_df <- data.frame()
      #if confirm db is true and method is enter, reset entered data
      if(confirm_db() == TRUE & input$input_method == "enter") {
        data_entered(empty_df)
        accepted(empty_df)
        confirm_db("empty")}
      #if confirm db is true and method is upload, reset uploaded data
      if(confirm_db() == TRUE & input$input_method == "upload") {
        file_upload(empty_df)
        accepted(empty_df)
        shinyjs::reset("upload")
        shinyjs::reset("species_column")
        shinyjs::reset("cover_column")
        confirm_db("empty")}
      #if confirm db is false, reset db to previous value
      if (confirm_db() == FALSE) {
        updateSelectInput(session, inputId = "db",
                          selected = previous_dbs$prev[1])}
    })

    #if cover method is changed and there is already data entered, show popup
    observeEvent(input$cover_method, {
      req(nrow(accepted()) > 0)
      #code for popup
      if(confirm_cover() != "empty") {
        confirm_cover("empty") }
      else{
        shinyalert(text = strong(
          "Changing the cover method will delete your current data entries.
        Are you sure you want to proceed?"),
        showCancelButton = T,
        showConfirmButton = T, confirmButtonText = "Proceed",
        confirmButtonCol = "red", type = "warning",
        html = T, inputId = "confirm_cover_change", className = "alert")}
    })

    observeEvent(input$confirm_cover_change, {
      #store confirmation in reactive value
      confirm_cover(input$confirm_cover_change)
      #create an empty df
      empty_df <- data.frame()
      #if confirm cover is true and method is enter, reset entered data
      if(confirm_cover() == TRUE & input$input_method == "enter") {
        data_entered(empty_df)
        accepted(empty_df)
        confirm_cover("empty")}
      #if confirm db is true and method is upload, reset uploaded data
      if(confirm_cover() == TRUE & input$input_method == "upload") {
        file_upload(empty_df)
        accepted(empty_df)
        shinyjs::reset("upload")
        shinyjs::reset("species_column")
        shinyjs::reset("cover_column")
        confirm_cover("empty")}
      #if confirm db is false, reset db to previous value
      if (confirm_cover() == FALSE) {
        updateSelectInput(session, inputId = "cover_method",
                          selected = previous_covers$prev[1])}
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
    data_download <- reactiveVal()

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
                    mutate(percent = round(percent, digits = 2)), row.names = F)
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
        cat('Species Entered')
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

    #updating reactive values
    observe({
      #requiring second screen to update reactive values
      req(cover_glide() == 1)

      metrics(fqacalc::transect_summary(x = accepted(),
                                        key = "scientific_name",
                                        db = input$db,
                                        cover_metric = input$cover_method,
                                        allow_no_c = TRUE))

      species_sum(fqacalc::species_summary(x = accepted(),
                                           key = "scientific_name",
                                           db = input$db,
                                           cover_metric = input$cover_method,
                                           allow_no_c = TRUE) %>%
                    mutate(c = as.integer(c),
                           w = as.integer(w),
                           coverage = as.integer(coverage))
      )

      physiog_sum(fqacalc::physiog_summary(x = accepted(),
                                           key = "scientific_name",
                                           db = input$db,
                                           cover_metric = input$cover_method,
                                           allow_no_c = TRUE))

      if(input$input_method == "enter") {
        plot_sum(fqacalc::plot_summary(x = data_entered(),
                                       key = "scientific_name",
                                       db = input$db,
                                       cover_metric = input$cover_method,
                                       plot_id = "plot_id",
                                       allow_no_c = TRUE)) }

      if (input$input_method == "upload") {
        if(input$plot_column == "") {
        plot_sum(fqacalc::plot_summary(x = file_upload() %>%
                                         rename(!!as.name(input$key) := input$species_column,
                                                cover = input$cover_column) %>%
                                         mutate(plot_id = "1"),
                                       key = input$key,
                                       db = input$db,
                                       cover_metric = input$cover_method,
                                       plot_id = "plot_id",
                                       allow_no_c = TRUE))
      }

      if(input$input_method == "upload" & input$plot_column != "") {
        plot_sum(fqacalc::plot_summary(x = file_upload()
                                       %>% rename(!!as.name(input$key) := input$species_column,
                                                  cover = input$cover_column,
                                                  plot_id = input$plot_column),
                                       key = input$key,
                                       db = input$db,
                                       cover_metric = input$cover_method,
                                       plot_id = "plot_id",
                                       allow_no_c = TRUE))
      }
      }

      # data_download(merge(fqacalc::accepted_entries(accepted(),
      #                                               db = input$db,
      #                                               native = FALSE,
      #                                               cover_weighted = TRUE,
      #                                               cover_metric = input$cover_method,
      #                                               allow_duplicates = TRUE,
      #                                               allow_no_c = TRUE),
      #                     data_entered()) %>%
      #                 dplyr::select(plot_id, everything()))
    })


    #get duration table
    observe({
      req(nrow(accepted()) > 0 & cover_glide() == 1)

      duration_cats <- data.frame(duration = c("annual", "perennial", "biennial"),
                                  frequency = rep.int(0, 3),
                                  percent = rep.int(0,3))

      dur <- accepted() %>%
        group_by(duration) %>%
        summarise(frequency = n()) %>%
        mutate(percent = round((frequency/sum(frequency))*100, 2)) %>%
        rbind(duration_cats %>% filter(!duration %in% accepted()$duration)) %>%
        mutate(frequency = as.integer(frequency))

      #store in reactive
      duration_table(dur)
    })

    #render title
    output$title <- renderText({paste("Calculating metrics based on",
                                      input$db)})

    #species richness
    output$species_richness <- renderUI({
      req(cover_glide() == 1)
      round(
        fqacalc::species_richness(x = accepted(), db = input$db, native = F, allow_no_c = TRUE),
        2)
    })

    #mean C
    output$mean_c <- renderUI({
      req(cover_glide() == 1)
      round(fqacalc::mean_c(x = accepted(), db = input$db, native = F), 2)
    })

    #total fqi
    output$fqi <- renderUI({
      req(cover_glide() == 1)
      round(fqacalc::FQI(x = accepted(), db = input$db, native = F), 2)
    })

    #C metrics table output
    output$c_metrics <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Mean C", "Native Mean C", "Total FQI",
                                     "Native FQI", "Adjusted FQI"))
    })

    #cover metrics table output
    output$cover_metrics <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Cover-Weighted Mean C", "Cover-Weighted Native Mean C",
                                     "Cover-Weighted FQI", "Cover-Weighted Native FQI"))
    })

    #wetness table output
    output$wetness <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Mean Wetness", "Native Mean Wetness"))
    })

    #nativity table output
    output$species_mets <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Total Species Richness",
                                     "Native Species Richness",
                                     "Exotic Species Richness")) %>%
        mutate(values = as.integer(values))
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
      c_score_plot(accepted())
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

    #physiog summary
    output$cover_physiog_manual <- renderTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive species summary
      physiog_sum()
    })

  })
}
