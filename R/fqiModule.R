#UI------------------------------------------------------------------------

fqiUI <- function(id) {

  tagList(

    glide(
      id = NS(id, "glide"),
      next_label = paste("Calculate FQA Metrics ", icon("arrow-right")),
      previous_label = paste(icon("arrow-left"), "Go Back to Data Entry"),
      controls_position = "bottom",
      height = "100%",

    screen(
      next_condition = "output['fqi-next_condition'] == 'TRUE'",

      fluidRow(
        sidebarPanel(

          titlePanel("Enter Data"),

          #help button
          circleButton(NS(id, "help"), icon = icon("question"),
                       style = "position:absolute; top:5px; right:5px;",
                       status = "primary"),

            #input regional data base
            selectInput(NS(id, "db"), label = "Select Regional FQAI Database",
                        choices = fqacalc::db_names()$name,
                        selected = "michigan_2014"),

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

              condition = "input['fqi-input_method'] == 'upload'",

              #input file upload widget
              fileInput(NS(id, "upload"), NULL, buttonLabel = "Upload...", multiple = F),

              #input what column to use to bind to FQA database
              uiOutput(NS(id, "species_colname")),

              #input button to delete uploaded file
              actionButton(NS(id, "upload_delete_all"), "Delete Uploaded File")

            ), #conditional 1 parenthesis

            #when data entry method is enter, allow user to enter data manually
            conditionalPanel(

              condition = "input['fqi-input_method'] == 'enter'",

              #input latin name
              selectizeInput(NS(id, "select_species"), label = "Select Species",
                       choices = NULL, selected = NULL, multiple = TRUE),

              fluidRow(
                #input add species button
                actionButton(NS(id, "add_species"), "Add Species",
                             style = "margin-left: 15px;"),
                #input delete speces button
                actionButton(NS(id, "delete_species"), "Delete Species",
                             style = "margin-left: 10px;")),

              br(),

              #button to delete all entries
              actionButton(NS(id, "manual_delete_all"), "Delete All Entries", class = "btn-danger")

            ) #conditional 2 parenthesis

        ), #side bar panel

        mainPanel(

          textOutput(NS(id, "next_condition")),

          #when user wants to upload a file but hasn't yet, show instructions
          conditionalPanel("input['fqi-input_method'] == 'upload' && output['fqi-file_is_uploaded'] != true",
                           br(),
                           h3("File uploads must have one column containing either scientific names
                              or acronyms. The columns must go to the top of the file such that row 1
                              is the column name.")),

          #when user uploads file, show uploaded table
          conditionalPanel("input['fqi-input_method'] == 'upload'",
                           br(),
                           br(),
                           dataTableOutput(NS(id, "upload_table"))),

          #when user enters species manually, show what they enter
          conditionalPanel("input['fqi-input_method'] == 'enter'",
                           br(),
                           br(),
                           dataTableOutput(NS(id, "manual_table")))

        )#main panel parenthesis

      )#fluidRow parenthesis

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

      fluidRow(
        column(4,
               box(tableOutput(NS(id,"c_metrics")), title = "FQI Metrics", width = NULL),
               box(tableOutput(NS(id,"duration_table")), title = "Duration Metrics", width = NULL,
                   style = "overflow-x: scroll")),
        column(4,
               box(tableOutput(NS(id,"wetness")), title = "Wetness Metrics", width = NULL),
               box(tableOutput(NS(id,"pysiog_table")), title = "Physiognomy Metrics", width = NULL,
                   style = "overflow-x: scroll")),
        column(4,
               box(tableOutput(NS(id,"species_mets")), title = "Species Richness Metrics", width = NULL),
               box(tableOutput(NS(id,"proportion")), title = "C Value Percentages", width = NULL))
      )

    )#screen 2 parenthesis

  )#glide parenthesis

  )#taglist parenthesis

}

#server-------------------------------------------------------------------------

fqiServer <- function(id) {

  #start module function
  moduleServer(id, function(input, output, session) {

    #creating a reactive value for glide page, used as input to server fun
    fqi_glide <- reactive({input$shinyglide_index_glide})

    #making input method reactive
    input_method <- reactive({input$input_method})

    #define table for data entered manually
    data_entered <- data.frame()

    #initialize reactives to hold data entered/uploaded
    file_upload <- reactiveVal()
    data_entered <- reactiveVal({data_entered})

    #help popup
    observeEvent(input$help, {
      fqi_help()
    })

#file upload server-------------------------------------------------------------

    #if file is uploaded, show T, else F
    output$file_is_uploaded <- reactive({
      return(!is.null(file_upload()))
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
        #create var for key
        key_var <- if(input$key == "scientific_name") {"Scientific Names"} else {"Acronyms"}
        #create a dropdown option
        selectizeInput(session$ns("species_column"),
                       paste0("Which Column Contains ", key_var, "?"),
                       colnames, selected = NULL)
      })
    })

    #warnings for bad data in file upload
    observeEvent(input$species_column, {
      req(nrow(file_upload()) >= 1)
      req(input$species_column != "")

      accepted_file <- accepted_entries(file_upload() %>%
                                          rename(!!as.name(input$key) := input$species_column),
                                        key = input$key,
                                        db = input$db,
                                        native = FALSE,
                                        allow_no_c = TRUE,
                                        allow_duplicates = TRUE,
                                        allow_non_veg = TRUE)

      #if there are unrecognized plants, show warning and remove
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
      if( nrow(plants_no_c) > 0 ){
        for(i in plants_no_c[, input$key]) {
          shinyalert(text = strong(paste("Species", i, "is recognized but has not been
                                         assigned a C score. It will be included in species
                                         richness and mean wetness metrics but excluded
                                         from mean C and FQI metrics.")),
                     type = "warning",  html = T, className = "alert")
         }
      }

      #if there are duplicate species, show warning, delete dups
      if( any(duplicated(file_upload() %>% select(input$species_column))) ){
        shinyalert(text = strong(
          "Duplicate species are detected. Duplicates will only be counted once."),
          type = "warning",  html = T, className = "alert")
      }
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

#manually enter data------------------------------------------------------------

    #species drop-down list based on regional list selected
    observe({
      #create list names based on regional list selected
      names <- if(input$key == "scientific_name")
      {c("", unique(fqacalc::view_db(input$db)$scientific_name))}
      else {c("", unique(fqacalc::view_db(input$db)$acronym))}
      #create a dropdown option
      updateSelectizeInput(session, "select_species",
                           choices =  names,
                           selected = character(0),
                           server = TRUE)
    })

    #When add species is clicked, add row
    observeEvent(input$add_species, {
      #find species
      if(input$key == "scientific_name") {
      new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                                dplyr::filter(scientific_name %in% input$select_species)) }
      else {new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                                       dplyr::filter(acronym %in% input$select_species))}
      #bind new entry to table
      new_table = rbind(new_entry, data_entered())
      #update reactive to new table
      data_entered(new_table)
      #reset drop down menu of latin names
      shinyjs::reset("select_species")
    })

    #if there are duplicate species, show warning, delete dups
    observeEvent(input$add_species, {
      req(nrow(data_entered()) > 1)
      if( any(duplicated(data_entered() %>% select(scientific_name))) ){
        shinyalert(text = strong(
          "Duplicate species are detected.
          Duplicates will only be counted once."),
          type = "warning",  html = T, className = "alert")

        data_entered(data_entered()[!duplicated(data_entered()[1]),])
      }
    })

    #if there are no C species, show warning
    observeEvent(input$add_species, {
      req(data_entered() > 0)
      plants_no_c <- unassigned_plants(data_entered(),
                                       key = "scientific_name",
                                       db = input$db)

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
    output$manual_table <- DT::renderDT({
      datatable(data_entered(),
                selection = 'single',
                options = list(autoWidth = TRUE,
                               scrollX = TRUE,
                               searching = FALSE,
                               lengthChange = FALSE)
      )
    })

    #when delete species is clicked, delete row
    observeEvent(input$delete_species,{
      #call table
      t = data_entered()
      #print table
      print(nrow(t))
      #if rows are selected, delete them
      if (!is.null(input$manual_table_rows_selected)) {
        t <- t[-as.numeric(input$manual_table_rows_selected),]
      }
      #else show the regular table
      data_entered(t)
    })

    #when delete all is clicked, clear all entries
    observeEvent(input$manual_delete_all, {
      empty_df <- data.frame()
      data_entered(empty_df)
      accepted(empty_df)
    })

#creating accepted df ----------------------------------------------------------

    #initialize reactives
    accepted <- reactiveVal()
    confirm_db <- reactiveVal("empty")
    previous_dbs <- reactiveValues(prev = "michigan_2014")

    #store current and previous db value in reactive element
    observeEvent(input$db, {
      previous_dbs$prev <- c(tail(previous_dbs$prev, 1), input$db)
    })

    #if input method is enter, accepted is from data_entered
    observe({
      req(input_method() == "enter")
      accepted(distinct(data_entered()))
    })

    #if input method is upload, accepted is from file upload
    observe({
      req(input_method() == "upload")
      accepted(data.frame())

      req(input_method() == "upload", nrow(file_upload()) > 0, input$species_column)
      accepted(fqacalc::accepted_entries(x = file_upload() %>%
                                           rename(!!as.name(input$key) := input$species_column),
                                         key = input$key,
                                         db = input$db,
                                         native = FALSE,
                                         allow_duplicates = FALSE,
                                         allow_non_veg = FALSE,
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
        html = T, inputId = "confirm_db_change", className = "alert")}
    })

    observeEvent(input$confirm_db_change, {
      #store confirmation in reactive value
      confirm_db(input$confirm_db_change)
      #create an empty df
      empty_df <- data.frame()
      #if confirm db is true and method is enter, reset entered data
      if(confirm_db() == TRUE) {
        data_entered(empty_df)
        file_upload(empty_df)
        shinyjs::reset("upload")
        shinyjs::reset("species_column")
        accepted(empty_df)
        confirm_db("empty")}
      #if confirm db is false, reset db to previous value
      if (confirm_db() == FALSE) {
        updateSelectInput(session, inputId = "db",
                          selected = previous_dbs$prev[1])}
    })

    #create boolean that shows if data is entered or not for next condition
    output$next_condition <- renderText(
      nrow(accepted()) > 0
          )

    #hide next condition output
    observe({shinyjs::hide("next_condition",)})
    outputOptions(output, "next_condition", suspendWhenHidden=FALSE)

#second screen -----------------------------------------------------------------

    #initializing reactives for outputs
    metrics <- reactiveVal()
    physiog_table <- reactiveVal()
    duration_table <- reactiveVal()

    #download cover summary server
    output$download <- downloadHandler(
      #name of file based off of transect
      filename = function() {
        paste0("FQA_assessment_", Sys.Date(), ".zip")
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

        cat("Physiognomy Metrics")
        cat('\n')
        write.csv(physiog_table() %>%
                    mutate(percent = round(percent, digits = 2)), row.names = F)
        cat('\n')
        cat('\n')

        cat("Duration Metrics")
        cat('\n')
        write.csv(duration_table() %>%
                    mutate(percent = round(percent, digits = 2)), row.names = F)
        cat('\n')
        cat('\n')

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

    #get all metrics
    observe({
      req(fqi_glide() == 1)
      metrics(fqacalc::all_metrics(x = accepted(), db = input$db, allow_no_c = TRUE))
    })

    #get pysiog and duration table
    observe({
      req(nrow(accepted()) > 0 & fqi_glide() == 1)

      #write df with all cats to include
       physiog_cats <- data.frame(physiognomy = c("tree", "shrub", "vine", "forb", "grass",
                                                 "sedge", "rush", "fern", "bryophyte"),
                                 frequency = rep.int(0, 9),
                                 percent = rep.int(0,9))

       duration_cats <- data.frame(duration = c("annual", "perennial", "biennial"),
                                   frequency = rep.int(0, 3),
                                   percent = rep.int(0,3))

       #count observations in accepted data
       phys <- accepted() %>%
         group_by(physiognomy) %>%
         summarise(frequency = n()) %>%
         mutate(percent = round((frequency/sum(frequency))*100, 2)) %>%
         rbind(physiog_cats %>% filter(!physiognomy %in% accepted()$physiognomy)) %>%
         mutate(frequency = as.integer(frequency))

       dur <- accepted() %>%
         group_by(duration) %>%
         summarise(frequency = n()) %>%
         mutate(percent = round((frequency/sum(frequency))*100, 2)) %>%
         rbind(duration_cats %>% filter(!duration %in% accepted()$duration)) %>%
         mutate(frequency = as.integer(frequency))

       #store in reactive
       physiog_table(phys)
       duration_table(dur)
    })

    #render title
    output$title <-
      renderText({paste("Calculating metrics based on ", input$db)})

    #species richness
    output$species_richness <- renderUI({
      req(fqi_glide() == 1)
      round(
        fqacalc::species_richness(x = accepted(), db = input$db, native = F, allow_no_c = TRUE),
        2)
    })

    #mean C
    output$mean_c <- renderUI({
      req(fqi_glide() == 1)
      round(fqacalc::mean_c(x = accepted(), db = input$db, native = F), 2)
    })

    #total fqi
    output$fqi <- renderUI({
      req(fqi_glide() == 1)
      round(fqacalc::FQI(x = accepted(), db = input$db, native = F), 2)
    })

    #metrics table output
    output$c_metrics <- renderTable({
      req(fqi_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Mean C", "Native Mean C", "Total FQI",
                                     "Native FQI", "Adjusted FQI"))
    })

    #wetness table output
    output$wetness <- renderTable({
      req(fqi_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Mean Wetness", "Native Mean Wetness"))
    })

    #nativity table output
    output$species_mets <- renderTable({
      req(fqi_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Total Species Richness",
                                     "Native Species Richness",
                                     "Exotic Species Richness")) %>%
                        mutate(values = as.integer(values))
    })

    #proportion table output
    output$proportion <- renderTable({
      req(fqi_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("% of Species with no C Value",
                                     "% of Species with 0 C Value",
                                     "% of Species with 1-3 C Value",
                                     "% of Species with 4-6 C Value",
                                     "% of Species with 7-10 C Value"))
    })

    #ggplot output
    output$c_hist <- renderPlot({
      req(fqi_glide() == 1)
      c_score_plot(accepted())
    })

    #ggplot output
    output$binned_c_score_plot <- renderPlot({
      req(fqi_glide() == 1)
      binned_c_score_plot(metrics())
    })

    #physiog table output
    output$pysiog_table <- renderTable({
      req(fqi_glide() == 1)
      physiog_table()
    })

    #duration table output
    output$duration_table <- renderTable({
      req(fqi_glide() == 1)
      duration_table()
    })

  })

}
