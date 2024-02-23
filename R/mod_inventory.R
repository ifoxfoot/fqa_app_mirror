#' inventory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inventory_ui <- function(id){
  ns <- NS(id)
  tagList(

    shinyglide::glide(
      id = ns("glide"),
      next_label = paste("Calculate FQA Metrics ", icon("arrow-right")),
      previous_label = paste(icon("arrow-left"), "Go Back to Data Entry"),
      controls_position = "bottom",
      height = "100%",
      keyboard = FALSE,

      shinyglide::screen(
        next_condition = "output['inventory-next_condition'] == 'TRUE'",

        fluidRow(
          sidebarPanel(

            titlePanel("Enter Data"),

            #help button
            shinyWidgets::circleButton(ns("help"), icon = icon("question"),
                         style = "position:absolute; top:5px; right:5px;",
                         status = "primary"),

            #input regional data base
            selectInput(ns("db"), label = "Select Regional FQA Database",
                        choices = fqacalc::db_names()$fqa_db,
                        selected = "michigan_2014"),

            #when db has incomplete acronyms hide acronym option
            conditionalPanel(

              condition = "output['inventory-complete_acronym'] == 'TRUE'",
              #input key argument
              shinyWidgets::radioGroupButtons(ns("key"), label = "Enter Species Using: ",
                                choices = c("Scientific Names" = "name",
                                            "Acronyms" = "acronym"),
                                selected = "name",
                                justified = TRUE,
                                checkIcon = list(yes = icon("ok",
                                                            lib = "glyphicon"))),
            ),

            #input data entry method
            shinyWidgets::prettyRadioButtons(ns("input_method"), label = "Select Data Entry Method",
                               choices = c( "Enter Species Manually" = "enter",
                                            "Upload a File" = "upload")),



            #when data entry method is upload, allow user to upload files
            conditionalPanel(

              condition = "input['inventory-input_method'] == 'upload'",

              #input file upload widget
              fileInput(ns("upload"), NULL, buttonLabel = "Upload...", multiple = F),

              #input what column to use to bind to FQA database
              uiOutput(ns("species_colname")),

              #input button to delete uploaded file
              actionButton(ns("upload_delete_all"), "Delete Uploaded File")

            ), #conditional 1 parenthesis

            #when data entry method is enter, allow user to enter data manually
            conditionalPanel(

              condition = "input['inventory-input_method'] == 'enter'",

              fluidRow(
                column(8,
                #input latin name
                selectizeInput(ns("select_species"), label = "Select Species",
                             choices = NULL, selected = NULL, multiple = TRUE)),
                column(4,
                #input add species button
                actionButton(ns("add_species"), "Add Species",
                             style = "margin-top: 30px; height: 60px;"))
                ),

              br(),

              bslib::layout_column_wrap(
                width = 1/2,
                #input delete species button
                actionButton(ns("delete_species"), "Delete Species"),
                #button to delete all entries
                actionButton(ns("manual_delete_all"), "Delete All Entries",
                             class = "btn-danger")
                )

            ) #conditional 2 parenthesis

          ), #side bar panel

          mainPanel(

            textOutput(ns("next_condition")),
            textOutput(ns("complete_acronym")),

            #when user wants to upload a file but hasn't yet, show instructions
            conditionalPanel("input['inventory-input_method'] == 'upload' && output['inventory-file_is_uploaded'] != true",
                             br(),
                             h3("File uploads must have one column containing either scientific names
                              or acronyms. The columns must go to the top of the file such that row 1
                              is the column name."),
                             br(),
                             #temporary download file button
                             a(href="www/crooked_island.xlsx", "ESA 2023 Download Sample Data Here!",
                               download=NA, target="self"),
            ),

            #when user uploads file, show uploaded table
            conditionalPanel("input['inventory-input_method'] == 'upload'",
                             br(),
                             br(),
                             DT::dataTableOutput(NS(id, "upload_table"))),

            #when user enters species manually, show what they enter
            conditionalPanel("input['inventory-input_method'] == 'enter'",
                             br(),
                             br(),
                             DT::dataTableOutput(NS(id, "manual_table")))

          )#main panel parenthesis

        )#fluidRow parenthesis

      ),#screen 1 parenthesis

      shinyglide::screen(

        #download button
        downloadButton(ns("download"),
                       label = "Download", class = "downloadButton",
                       style = "position: absolute; top: 0px; right: 10px;"),
        br(),
        #title
        column(12, align = "center",
               h3(textOutput(ns("title")))),

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
            bslib::card_header("Wetness Metrics"),
            bslib::card_body(tableOutput(ns("wetness")))
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
            bslib::card_header("Physiognomy Metrics"),
            bslib::card_body(tableOutput(ns("pysiog_table")))
          ),
          bslib::card(
            bslib::card_header("Duration Metrics"),
            bslib::card_body(tableOutput(NS(id,"duration_table")))
          ),
          bslib::card(
            bslib::card_header("C Value Percentages"),
            bslib::card_body(tableOutput(ns("proportion")))
          )
        ),
        br(),

        #big table (accepted entries)
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
          bslib::card_header("Data Entered"),
          status = "primary",
          bslib::card_body(DT::dataTableOutput(ns( "accepted"))),
          style = "overflow-x: auto;l")),
        br()


      )#screen 2 parenthesis

    )#glide parenthesis

  )
}

#' inventory Server Functions
#'
#' @noRd
mod_inventory_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #creating a reactive value for glide page, used as input to server fun
    fqi_glide <- reactive({input$shinyglide_index_glide})

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
      inventory_help()
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
    outputOptions(output, "file_is_uploaded", suspendWhenHidden = FALSE)

    #drop-down list (for species column) based on the file uploaded
    observeEvent(input$upload,{
      output$species_colname <- renderUI({
        #create list cols
        colnames <- c("", colnames(file_upload()))
        #create var for key
        key_var <- if(key() == "name") {"Scientific Names"} else {"Acronyms"}
        #create a dropdown option
        selectizeInput(ns("species_column"),
                       paste0("Which Column Contains ", key_var, "?"),
                       colnames, selected = NULL)
      })
    })

    #warnings for bad data in file upload
    observeEvent(input$species_column, {
      req(nrow(file_upload()) >= 1 & input$species_column != "")
      #list to store warnings
      warning_list <- list()
      #catch warnings
      withCallingHandlers(
        accepted_file <- fqacalc::accepted_entries(file_upload() %>%
                                            dplyr::rename(!!as.name(key()) := input$species_column),
                                          key = key(),
                                          db = input$db,
                                          native = FALSE),
        #add to list
        message=function(w) {warning_list <<- c(warning_list, list(w$message))})
      #show each list item in notification
      for(i in warning_list) {
        showNotification(ui = i,
                         duration = NULL,
                         type = "error") }
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

    #when delete all is clicked, clear all entries
    observeEvent(input$upload_delete_all, {
      #make an empty df
      empty_df <- data.frame()
      #replace reactive file upload with empty file
      file_upload(empty_df)
      accepted(empty_df)
      #reset upload button
      shinyjs::reset("upload")
      shinyjs::reset("species_column")
    })

    #manually enter data------------------------------------------------------------

    #species drop-down list based on regional database selected
    observe({
      req(input$db)
      #create list names based on regional database selected
      names <- if(key() == "name")
      {c("", unique(fqacalc::view_db(input$db)$name))}
      else {c("", unique(fqacalc::view_db(input$db)$acronym))}
      #create a dropdown option
      updateSelectizeInput(session, "select_species",
                           choices =  names,
                           selected = character(0),
                           server = TRUE)
    })

    #When add species is clicked, add row
    observeEvent(input$add_species, {
      #list species
      new_entry <-  c(input$select_species)
      #bind new entry to table
      new_row <- fqadata::fqa_db %>%
        dplyr::filter(fqa_db == input$db) %>%
        dplyr::filter(!!as.name(key()) %in% c(new_entry))
      #update reactive to new table
      if (nrow(data_entered() > 0)) {
        data_entered( rbind(new_row, accepted()) )
      } else data_entered(new_row)
      #reset drop down menu of latin names
      shinyjs::reset("select_species")
    })

    #this allows popups for warnings about duplicates/non-matching species
    observeEvent(input$add_species,{
      req(nrow(data_entered()) > 0)
      #list to store warnings
      warning_list <- list()
      #catch warnings
      withCallingHandlers(
        fqacalc::accepted_entries(x = data_entered(),
                                  key = key(),
                                  db = input$db,
                                  native = FALSE,
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
    output$manual_table <- DT::renderDT({
      DT::datatable(accepted(),
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
      t = accepted()
      #print table
      print(nrow(t))
      #if rows are selected, delete them
      if (!is.null(input$manual_table_rows_selected)) {
        t <- t[-as.numeric(input$manual_table_rows_selected),]
      }
      #else show the regular table
      accepted(t)
    })

    #when delete all is clicked, clear all entries
    observeEvent(input$manual_delete_all, {
      empty_df <- data.frame()
      data_entered(empty_df)
      accepted(empty_df)
    })

    #creating accepted df ----------------------------------------------------------

    #initialize reactives
    accepted <- reactiveVal(data.frame())
    confirm_db <- reactiveVal("empty")
    previous_dbs <- reactiveValues(prev = "michigan_2014")

    #store current and previous db value in reactive element
    observeEvent(input$db, {
      previous_dbs$prev <- c(tail(previous_dbs$prev, 1), input$db)
    })

    #if input method is enter, accepted is from data_entered
    observe({
      req(input_method() == "enter")
      accepted(data.frame())

      req(input_method() == "enter", nrow(data_entered()) > 0)
      accepted(suppressMessages(fqacalc::accepted_entries(x = data_entered(),
                                                          key = key(),
                                                          db = input$db,
                                                          native = FALSE,
                                                          allow_duplicates = FALSE,
                                                          allow_non_veg = FALSE,
                                                          allow_no_c = TRUE)))
    })

    #if input method is upload, accepted is from file upload
    observe({
      req(input_method() == "upload")
      accepted(data.frame())

      req(input_method() == "upload", nrow(file_upload()) > 0, input$species_column)
      accepted(suppressMessages(fqacalc::accepted_entries(x = file_upload() %>%
                                                            dplyr::rename(!!as.name(key()) := input$species_column),
                                                          key = key(),
                                                          db = input$db,
                                                          native = FALSE,
                                                          allow_duplicates = FALSE,
                                                          allow_non_veg = FALSE,
                                                          allow_no_c = TRUE)))
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
        shinyWidgets::updateRadioGroupButtons(session, inputId = "key",
                                label = "Enter Species Using: ",
                                choices = c("Scientific Names" = "name",
                                            "Acronyms" = "acronym"),
                                justified = TRUE,
                                checkIcon = list(yes = icon("ok",
                                                            lib = "glyphicon")))
        removeModal()
        confirm_db("empty")
    })

    #if confirm db is false, reset db to previous value
    observeEvent(input$cancel_db_change, {
      confirm_db(FALSE)
      updateSelectInput(session, inputId = "db",
                        label = "Select Regional FQA Database",
                        choices = fqacalc::db_names()$fqa_db,
                        selected = previous_dbs$prev[1])
      removeModal()
    })

    #wetland warnings
    observeEvent(list(input$db, input$confirm_db_change), ignoreInit = TRUE, {
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
        showModal(modalDialog("The Colorado FQA database is associated with
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
        csv_path <- file.path(tmpdir, "FQI_metrics.csv")

        # Start a sink file with a CSV extension
        sink(csv_path)
        cat('\n')
        cat(paste0("Calculating metrics based on the ", input$db, " regional FQA database."))
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        write.csv(metrics() %>%
                    dplyr::mutate(values = round(values, digits = 2)), row.names = F)
        cat('\n')
        cat('\n')

        cat("Physiognomy Metrics")
        cat('\n')
        write.csv(physiog_table() %>%
                    dplyr::mutate(percent = round(percent, digits = 2)), row.names = F)
        cat('\n')
        cat('\n')

        cat("Duration Metrics")
        cat('\n')
        write.csv(duration_table() %>%
                    dplyr::mutate(percent = round(percent, digits = 2)), row.names = F)
        cat('\n')
        cat('\n')

        cat('Species Entered')
        cat('\n')
        write.csv(accepted(), row.names = F)

        # Close the sink
        sink()

        #now add two ggplots as pngs
        ggplot2::ggsave(filename = file.path(tmpdir, "binned_hist.png"),
                        plot = binned_c_score_plot(metrics()),
                        device = "png", bg = 'white')
        ggplot2::ggsave(filename = file.path(tmpdir, "c_value_hist.png"),
                        plot = c_score_plot(accepted()), bg='#ffffff',
                        device = "png")

        # Zip them up
        zip::zip(file,
                 file.path(tmpdir, c("FQI_metrics.csv", "binned_hist.png", "c_value_hist.png")),
                 mode = "cherry-pick")
      })

    #get all metrics
    observe({
      req(fqi_glide() == 1)
      metrics(suppressMessages(fqacalc::all_metrics(x = accepted(), db = input$db, allow_no_c = TRUE)))
    })

    #get pysiog and duration table
    observe({
      req(nrow(accepted()) > 0 & fqi_glide() == 1)

      #write df with all cats to include
      physiog_cats <- data.frame(physiognomy = c("tree", "shrub", "vine", "forb", "grass",
                                                 "sedge", "rush", "fern", "bryophyte"),
                                 number = rep.int(0, 9),
                                 percent = rep.int(0,9))

      duration_cats <- data.frame(duration = c("annual", "perennial", "biennial"),
                                  number = rep.int(0, 3),
                                  percent = rep.int(0,3))

      #count observations in accepted data
      phys <- accepted() %>%
        dplyr::group_by(physiognomy) %>%
        dplyr::summarise(number = dplyr::n()) %>%
        dplyr::mutate(percent = round((number/sum(number))*100, 2)) %>%
        rbind(physiog_cats %>% dplyr::filter(!physiognomy %in% accepted()$physiognomy)) %>%
        dplyr::mutate(number = as.integer(number))

      dur <- accepted() %>%
        dplyr::group_by(duration) %>%
        dplyr::summarise(number = dplyr::n()) %>%
        dplyr::mutate(percent = round((number/sum(number))*100, 2)) %>%
        rbind(duration_cats %>% dplyr::filter(!duration %in% accepted()$duration)) %>%
        dplyr::mutate(number = as.integer(number))

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
      shiny::p(
        round(suppressMessages(fqacalc::species_richness(x = accepted(),
                                                         db = input$db,
                                                         native = F,
                                                         allow_no_c = TRUE)), 2),
        style = "font-size: 40px;"
      )
    })

    #mean C
    output$mean_c <- renderUI({
      req(fqi_glide() == 1)
      shiny::p(
        round(suppressMessages(fqacalc::mean_c(x = accepted(),
                                               db = input$db,
                                               native = F)), 2),
        style = "font-size: 40px;"
      )
    })

    #total fqi
    output$fqi <- renderUI({
      req(fqi_glide() == 1)
      shiny::p(
        round(suppressMessages(fqacalc::FQI(x = accepted(),
                                            db = input$db,
                                            native = F)), 2),
        style = "font-size: 40px;"
      )
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
        dplyr::filter(metrics %in% c("Mean Wetness",
                                     "Native Mean Wetness",
                                     "% Hydrophytes"))
    })

    #nativity table output
    output$species_mets <- renderTable({
      req(fqi_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Total Species Richness",
                                     "Native Species Richness",
                                     "Exotic Species Richness")) %>%
        dplyr::mutate(values = as.integer(values))
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

    #accepted summary
    output$accepted <- DT::renderDataTable({
      #requiring second screen
      req(fqi_glide() == 1)
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
# mod_inventory_ui("inventory_1")

## To be copied in the server
# mod_inventory_server("inventory_1")
