#sidebar ------------------------------------------------------------------------

#side bar UI function
fqiSideBarUI <- function(id) {

  tagList(

    #input regional data base
    selectInput(NS(id, "db"), label = "Select Regional FQAI Database",
                choices = fqacalc::db_names(),
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

fqiOutputUI <- function(id) {

  tagList(

  )}

#server-------------------------------------------------------------------------

fqiServer <- function(id, fqi_glide) {

  #start module function
  moduleServer(id, function(input, output, session) {

    #define table for data entered manually
    data_entered = data.frame()

    #initialize reactives
    file_upload <- reactiveVal()
    data_entered <- reactiveVal({data_entered})
    accepted <- reactiveVal()
    #all_metrics <- reactiveVal()

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
      req(input$upload)
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

    #when data is entered find accepted values and store them reactivly
    observeEvent(input$FQI_column,{
      req(input$FQI_column)
      #calculate accepted entries
      accepted <- fqacalc::accepted_entries(x = file_upload() %>%
                                              rename("scientific_name" = input$FQI_column),
                                            key = "scientific_name",
                                            db = input$db,
                                            native = F)
      #store it
      accepted(accepted)
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
      #reset upload button
      shinyjs::reset("FQI_uploaded_file")
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
      empty_df <- data.frame(row.names = names(fqacalc::crooked_island))
      #assign it to the reactive value
      data_entered(empty_df)
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

    #render title
    output$FQI_regional_list_manual <-
      renderText({paste("Calculating metrics based on ", input$db)})

    render_metrics <- function (input) {

    }

    # #metrics table output on FQA page
    # output$FQI_DT_metrics_upload <- renderTable({
    #   #requiring second screen
    #   req(fqi_glidei == 1)
    #
    #   fqacalc::all_metrics(x = FQI_file_upload()
    #                        %>% rename("scientific_name" = input$FQI_column),
    #                        key = "scientific_name",
    #                        db = input$FQI_db)
    #
    #
    # })
    #
    # #ggplot output
    # output$FQI_c_hist_upload <- renderPlot({
    #   #requiring second screen
    #   req(fqi_glidei == 1)
    #
    #   c_score_plot(FQI_accepted_upload())
    # })

    # output$fqi_species_richness_manual <- renderUI({
    #   #requiring second screen
    #   req(fqi_glidei == 1)
    #   round(
    #     fqacalc::species_richness(x = data_entered_manual(), db = input$FQI_db, native = F),
    #     3)
    # })
    #
    # output$fqi_mean_c_manual <- renderUI({
    #   #requiring second screen
    #   req(fqi_glidei == 1)
    #   round(fqacalc::mean_c(x = data_entered_manual(), db = input$FQI_db, native = F), 3)
    # })
    #
    # output$fqi_fqi_manual <- renderUI({
    #   #requiring second screen
    #   req(fqi_glidei == 1)
    #   round(fqacalc::FQI(x = data_entered_manual(), db = input$FQI_db, native = F), 3)
    # })
    #
    # #metrics table output on FQA page
    # output$FQI_DT_metrics_manual <- renderTable({
    #   #requiring second screen
    #   req(fqi_glidei == 1)
    #   fqacalc::all_metrics(x = data_entered_manual(), db = input$FQI_db)
    # })
    #
    # #ggplot output
    # output$FQI_c_hist_manual <- renderPlot({
    #   #requiring second screen
    #   req(fqi_glidei == 1)
    #   c_score_plot(distinct(data_entered_manual()))
    # })

  })

}
