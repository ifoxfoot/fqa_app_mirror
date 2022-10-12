#sidebar ------------------------------------------------------------------------

#side bar UI function
fqiSideBarUI <- function(id) {

  tagList(

    #input regional data base
    selectInput(NS(id, "db"), label = "Select Regional FQAI Database",
                choices = fqacalc::db_names()$name,
                selected = "michigan_2014"),

    #input data entry method
    radioButtons(NS(id, "input_method"), label = "Select Data Entry Method",
                 choices = c( "Enter Species Manually" = "enter",
                              "Upload a File" = "upload")),


    #when data entry method is upload, allow user to upload files
    conditionalPanel(

      condition = "input['fqi-input_method'] == 'upload'",

      #input file upload widget
      fileInput(NS(id, "upload"), NULL, buttonLabel = "Upload...", multiple = F),

      #input what column to use to bind to FQA database
      uiOutput(NS(id, "FQI_colname")),

      #input button to delete uploaded file
      actionButton(NS(id, "upload_delete_all"), "Delete Uploaded File")

    ), #conditional 1 parenthesis

    #when data entry method is enter, allow user to enter data manually
    conditionalPanel(

      condition = "input['fqi-input_method'] == 'enter'",

      #input latin name
      uiOutput(NS(id, "latin_name")),

      #input add species button
      actionButton(NS(id, "add_species"), "Add Species"),

      #input delete speces button
      actionButton(NS(id, "delete_species"), "Delete Species"),

      #button to delete all entries
      actionButton(NS(id, "manual_delete_all"), "Delete All Entries")

    ) #conditional 2 parenthesis
  )}

#Main panel ---------------------------------------------------------------------

fqiMainPanelUI <- function(id) {

  tagList(

    #when user uploads file, show uploaded table
    conditionalPanel("input['fqi-input_method'] == 'upload' && input.FQI_uploaded_file != 0",

                     dataTableOutput(NS(id, "upload_table"))),


    #when user enters species manually, show what they enter
    conditionalPanel("input['fqi-input_method'] == 'enter'",

                     dataTableOutput(NS(id, "manual_table"))),

  )}

#Outputs------------------------------------------------------------------------

fqiOutputUI <- function(id) {

  tagList(

    fluidRow(
      #title
      column(7, h3(textOutput(NS(id, "title")))),

      #download button
      column(2, downloadButton(NS(id, "download"),
                               label = "Download", class = "downloadButton",
                               style = "margin-top: 20px; height: 40px;"))),

    #boxes with key values
    fluidRow(
      valueBox(
        htmlOutput(NS(id,"species_richness")),
        "Species Richness",
        icon = icon("tree"), color = "orange"
      ),

      valueBox(
        htmlOutput(NS(id,"mean_c")),
        "Mean C",
        icon = icon("seedling"), color = "orange"
      ),
      valueBox(
        htmlOutput(NS(id,"fqi")),
        "Total FQI",
        icon = icon("pagelines"), color = "orange"
      )
    ),#fluidRow parenthesis

    #all mets and graph
    fluidRow(
      box(plotOutput(NS(id,"compare_plot")),
          title = "Compare Frequency of C Scores to Regional FQAI"),
      box(plotOutput(NS(id,"c_hist")),
          title = "Histogram of C Scores")
    ),

    fluidRow(
      column(4,
        box(tableOutput(NS(id,"c_metrics")), title = "FQI Metrics", width = NULL)),
      column(4,
        box(tableOutput(NS(id,"wetness")), title = "Wetness", width = NULL)),
      column(4,
        box(tableOutput(NS(id,"species_mets")), title = "Species Richness", width = NULL))
      ),

    fluidRow(
      column(4,
             box(tableOutput(NS(id,"proportion")), title = "Proportion", width = NULL)),
      column(4,
             box(tableOutput(NS(id,"pysiog_table")), title = "Pysiognomy Table", width = NULL)),
      column(4,
             box(tableOutput(NS(id,"duration_table")), title = "Duration Table", width = NULL))
    )

  )}

#server-------------------------------------------------------------------------

fqiServer <- function(id, fqi_glide) {

  #start module function
  moduleServer(id, function(input, output, session) {

    #making input method reactive
    input_method <- reactive({input$input_method})

    #define table for data entered manually
    data_entered <- data.frame()

    #initialize reactives to hold data entered/uploaded
    file_upload <- reactiveVal()
    data_entered <- reactiveVal({data_entered})

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
                         validate("Invalid file; Please upload a .csv or .tsv file")) %>%
        #drop empty data
        filter(., rowSums(is.na(.)) != ncol(.)) %>%
        as.data.frame(.)
      #store upload in reactive object
      file_upload(new_file)
    })

    #column name drop-down list based on the file uploaded
    output$FQI_colname <- renderUI({
      #create list cols
      colnames <- c("", colnames(file_upload()))
      #create a dropdown option
      selectizeInput(session$ns("FQI_column"), "Which Column Contains Latin Names?",
                     colnames, selected = NULL)
    })

    #this allows popups for warnings about duplicates/non-matching species
    observeEvent(input$FQI_column,{
      req(input$FQI_column)
      #list to store warnings
      warning_list <- list()
      #catch warnings
      withCallingHandlers(
        fqacalc::accepted_entries(x = file_upload()
                                  %>% rename("scientific_name" = input$FQI_column),
                                  key = "scientific_name",
                                  db = input$db,
                                  native = F),
        #add to list
        message=function(w) {warning_list <<- c(warning_list, list(w$message))})
      #show each list item in notification
      lapply(warning_list, showNotification, type = "error", duration = NULL)
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
      shinyjs::reset("FQI_column")
    })

#manually enter data------------------------------------------------------------

    #species drop-down list based on region
    output$latin_name <- renderUI({
      #create list of latin names based on regional list selected
      latin_names <- c("", unique(fqacalc::view_db(input$db)$scientific_name))
      #create a dropdown option
      selectizeInput(session$ns("species"), "Select Species", latin_names,
                     selected = NULL,
                     multiple = TRUE)
    })

    #When add species is clicked, add row
    observeEvent(input$add_species, {
      #find species
      new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                                dplyr::filter(scientific_name %in% input$species))
      #bind new entry to table
      new_table = rbind(new_entry, data_entered())
      #these lines discourage using multiple regional databases when entering data
      one_region <- length(unique(new_table$db)) <= 1
      shinyFeedback::feedbackDanger("FQI_db", !one_region,
                                    "selecting multiple regions is not recommended")
      #update reactive to new table
      data_entered(new_table)
      #reset drop down menu of latin names
      shinyjs::reset("species")
    })

    #this allows popups for warnings about duplicates/non-matching species
    observeEvent(input$add_species,{
      req(input$add_species)
      #list to store warnings
      warning_list <- list()
      #catch warnings
      withCallingHandlers(
        fqacalc::accepted_entries(x = data_entered(),
                                  key = "scientific_name",
                                  db = input$db,
                                  native = FALSE),
        #add to list
        message=function(w) {warning_list <<- c(warning_list, list(w$message))})
      #show each list item in notification
      lapply(warning_list, showNotification, type = "error", duration = NULL)
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
      #make an empty df
      empty_df <- data.frame()
      #assign it to the reactive value
      data_entered(empty_df)
      accepted(empty_df)
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

#second screen ----------------------------------------------------------------

    #initializing reactives for outputs
    accepted <- reactiveVal()
    metrics <- reactiveVal()
    physiog_table <- reactiveVal()
    duration_table <- reactiveVal()

    #download cover summary server
    output$download <- downloadHandler(
      #name of file based off of transect
      filename = function() {
        paste0(input$db, "_FQA_assessment_", Sys.Date(), ".zip")
      },
      #content of file
      content = function(fname) {
        #set wd to temp directory
        tmpdir <- tempdir()
        setwd(tempdir())

        #list names of files to zip
        fs <- c("data_entered.csv", "all_metrics.csv", "physiognomy_table.csv", "duration_table.csv")

        #write csvs
        write.csv(accepted(), file = "data_entered.csv")
        write.csv(metrics(), file = "all_metrics.csv")
        write.csv(physiog_table(), file = "physiognomy_table.csv")
        write.csv(duration_table(), file = "duration_table.csv")

        #zip files, name them
        zip(zipfile=fname, files=fs)
        if(file.exists(paste0(fname, ".zip")))
        {file.rename(paste0(fname, ".zip"), fname)}
      },
      contentType = "application/zip"
    )


    #if input method is enter, accepted is from data_entered
    observe({
      req(input_method() == "enter")
      accepted(distinct(data_entered()))
    })

    #if input method is enter, accepted is from file upload
    observe({
      req(input_method() == "upload", !is.null(file_upload()), input$FQI_column)
      accepted(fqacalc::accepted_entries(x = file_upload() %>%
                                           rename("scientific_name" = input$FQI_column),
                                         key = "scientific_name",
                                         db = input$db,
                                         native = F))
    })

    #get all metrics
    observe({
      req(nrow(accepted()) > 0 & fqi_glide() == 1)
      metrics(fqacalc::all_metrics(x = accepted(), db = input$db))
    })

    #get pysiog and duration table
    observe({
      req(nrow(accepted()) > 0 & fqi_glide() == 1)

      #write df with all cats to include
       physiog_cats <- data.frame(physiognomy = c("tree", "shrub", "vine", "forb", "grass",
                                                 "sedge", "rush", "fern", "bryophyte"),
                                 count = rep.int(0, 9),
                                 percent = rep.int(0,9))

       duration_cats <- data.frame(duration = c("annual", "perennial", "biennial"),
                                   count = rep.int(0, 3),
                                   percent = rep.int(0,3))

       #count observations in accepted data
       phys <- accepted() %>%
                       group_by(physiognomy) %>%
                       summarise(count = n()) %>%
                       mutate(percent = round((count/sum(count))*100, 2)) %>%
                       rbind(physiog_cats %>% filter(!physiognomy %in% accepted()$physiognomy))

       dur <- accepted() %>%
                        group_by(duration) %>%
                        summarise(count = n()) %>%
                        mutate(percent = round((count/sum(count))*100, 2)) %>%
                        rbind(duration_cats %>% filter(!duration %in% accepted()$duration))

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
        fqacalc::species_richness(x = accepted(), db = input$db, native = F),
        3)
    })

    #mean C
    output$mean_c <- renderUI({
      req(fqi_glide() == 1)
      round(fqacalc::mean_c(x = accepted(), db = input$db, native = F), 3)
    })

    #total fqi
    output$fqi <- renderUI({
      req(fqi_glide() == 1)
      round(fqacalc::FQI(x = accepted(), db = input$db, native = F), 3)
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
                                     "Exotic Species Richness"))
    })

    #proportion table output
    output$proportion <- renderTable({
      req(fqi_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Proportion of Species with < 1 C score",
                                     "Proportion of Species with 1-3.9 C score",
                                     "Proportion of Species with 4-6.9 C score",
                                     "Proportion of Species with 7-10 C score"))
    })

    #ggplot output
    output$c_hist <- renderPlot({
      req(fqi_glide() == 1)
      c_score_plot(accepted())
    })

    #ggplot output
    output$compare_plot <- renderPlot({
      req(fqi_glide() == 1)
      compare_plot(input_data = accepted(),
                   db_name = as.character(input$db),
                   db = fqacalc::view_db(input$db))
    })

    #physiog table output
    output$pysiog_table <- renderTable({
      req(fqi_glide() == 1)
      physiog_table()
    })

    #physiog table output
    output$duration_table <- renderTable({
      req(fqi_glide() == 1)
      duration_table()
    })

  })

}
