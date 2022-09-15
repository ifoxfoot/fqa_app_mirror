#load package
library(shiny) #for app
library(fqacalc) #for fqai metrics
library(tidyverse) #for data wrangling/displaying
library(shinyglide) #for glide panels
library(DT) #for displaying tables
library(shinyjs) #for reset buttons
library(shinyFeedback) #for warning messages near widgets
library(rhandsontable) #for editable tables
library(bslib) #interactive theme
library(thematic) #for theme r graphics

#define table for data entered manually
data_entered_manual = data.frame()

#thematic for theme of plots
thematic::thematic_shiny()


#define UI for application (User Interface)
ui <- fluidPage(

  #testing theme
  theme = bslib::bs_theme(version = 4),

  #controls where notifications are displayed
  tags$head(
    tags$style(
      HTML("#shiny-notification-panel {
             position:fixed;
             top: calc(50%);
             left: calc(50% - 150px);
             }
             "
      )
    )
  ),

  #this css edits the shiny glide button positioning
  # tags$style(
  #   ".glide-controls { position: absolute; top: 18px; right: 15px; width: 160px; }"
  # ),

  #call this package for reset function
  useShinyjs(),

  #call this package for warning/validation messages
  shinyFeedback::useShinyFeedback(),

  #initiate navbar
  navbarPage("FQA",

             #setting bootstrap to version 4
             theme = bslib::bs_theme(version = 4),

#FQI TAB------------------------------------------------------------------------

    #tab panel 1
    tabPanel("Calculate FQA Metrics",


             #allow glide to be used in this tab
             glide(
               #labels for glide buttons
               next_label = "Calculate FQA Metrics",
               previous_label = "Go Back to Data Entry",
               #customizing where they appear
               custom_controls = div(class = "glide-controls", glideControls()),
               controls_position = "bottom",
               height = "100%",

               screen(

             fluidRow(
               sidebarPanel(

                 titlePanel("Enter Data"),

                 #input regional data base
                 selectInput("FQI_db", label = "Select Regional FQAI Database",
                          choices = fqacalc::db_names(),
                          selected = "michigan_2014"),

                 #input data entry method
                 radioButtons("FQI_method", label = "Select Data Entry Method",
                           choices = c("Upload a File" = "upload",
                                       "Enter Species Manually" = "enter")),

                 #when data entry method is upload, allow user to upload files
                 conditionalPanel(

                   condition = "input.FQI_method == 'upload'",

                   #input file upload widget
                   fileInput("FQI_uploaded_file", NULL, buttonLabel = "Upload...", multiple = F),


                   #input what column to use to bind to FQA database
                   uiOutput("FQI_colname"),

                   #input button to delete uploaded file
                   actionButton("FQI_delete_upload", "Delete Uploaded File")

                   ), #conditional 1 parenthesis


                #when data entry method is enter, allow user to enter data manually
                conditionalPanel(

                  condition = "input.FQI_method == 'enter'",

                  #input latin name
                  uiOutput("FQI_latin_name"),

                  #input add species button
                  actionButton("FQI_add_species", "Add Species"),

                  #input delete speces button
                  actionButton("FQI_delete_species", "Delete Species"),

                  #button to delete all entries
                  actionButton("FQI_delete_manual_entries", "Delete All Entries")

                ), #conditional 2 parenthesis

               ), #side bar panel

            mainPanel(

              #when user uploads file, show uploaded table
              conditionalPanel("input.FQI_method == 'upload' && input.FQI_uploaded_file != 0",
                               dataTableOutput("FQI_DT_upload")),


              #when user enters species manually, show what they enter
              conditionalPanel("input.FQI_method == 'enter'",
                               dataTableOutput("FQI_DT_manual")),

              )#main panel parenthesis

             )#fluid row parenthesis

            ),#screen 1 parenthesis

            screen(
              #banner telling you what regional list you're using
              h3(textOutput({"FQI_regional_list_manual"})),

                conditionalPanel(
                  condition = "input.FQI_method == 'upload' && input.FQI_column",
                  #output table of metrics
                  fluidRow(
                  column(4, tableOutput("FQI_DT_metrics_upload")),
                  #output
                  column(8, plotOutput("FQI_c_hist_upload"))
                  )#fuildrow parenthesis
                  ),#conditional 1 parenthesis

                conditionalPanel(
                  condition = "input.FQI_method == 'enter'",
                  #output table of metrics
                  fluidRow(
                  column(4, tableOutput("FQI_DT_metrics_manual")),
                  #output
                  column(8, plotOutput("FQI_c_hist_manual"))
                  )#fluidrow parenthesis
                )#conditional 2 parenthesis

              )#screen 2 parenthesis

            )#glide parenthesis

          ),#tab panel 1 parenthesis

# COVER TAB --------------------------------------------------------------------

    #tab panel 2
    tabPanel("Caclulate Cover-Weighted FQA Metrics",

             #allow glide to be used in this tab
             glide(
               #labels for glide buttons
               next_label = "Calculate FQA Metrics",
               previous_label = "Go Back to Data Entry",
               #customizing where they appear
               custom_controls = div(class = "glide-controls", glideControls()),
               controls_position = "bottom",
               height = "100%",

               screen(
                 fluidRow(

                   sidebarPanel(

                     #title of side bar
                     titlePanel("Enter Data"),

                     #input regional data base
                     selectInput("cover_db", label = "Select Regional FQAI Database",
                                 choices = fqacalc::db_names(),
                                 selected = "michigan_2014"),

                     #input data entry method
                     radioButtons("cover_input_method", label = "Select Data Entry Method",
                                  choices = c( "Enter Species Manually" = "enter",
                                               "Upload a File" = "upload")),

                     #when data entry method is upload, allow user to upload files
                     conditionalPanel(

                       condition = "input.cover_input_method == 'upload'",

                       "UNDER CONSTRUCTION"

                     ), #conditional 1 parenthesis


                     #when data entry cover_input_method is enter, allow user to enter data manually
                     conditionalPanel(

                       condition = "input.cover_input_method == 'enter'",

                       #select cover method to use
                       selectInput("cover_method_select",
                                   "Cover Method",
                                   choices = c(
                                     "percent_cover",
                                     "braun-blanquet",
                                     "carolina_veg_survey",
                                     "daubenmire",
                                     "usfs_ecodata"
                                   ))

                     ), #conditional 2 parenthesis

                   ),#sidebarPanel parenthesis

                   mainPanel(
                     conditionalPanel(
                       condition = "input.cover_input_method == 'enter'",

                       #manually entered data for cover metrics
                       rHandsontableOutput("cover_manual_table")),

                     )#main panel parenthesis

                   )#fluid row parenthesis

                 ),#screen 1 parenthesis

               screen(

                 #conditional panel when cover input method is manual entry
                   conditionalPanel(
                     condition = "input.cover_input_method == 'enter'",

                     fluidRow(

                       column(7,
                              #text saying which list user is using
                              h3(textOutput({"cover_regional_list_manual"})),
                              #plot output
                              plotOutput("cover_c_hist_manual")),

                     #output table of metrics
                     column(5,tableOutput("cover_metrics_manual")),

                   )#fluid row parenthesis

                 )#conditional 1 parenthesis

               )#screen two parenthesis

             )#glide parenthesis

    ),#tab panel 2 parenthesis

# ABOUT ------------------------------------------------------------------------

  tabPanel("About FQA",
           #rmarkdown here
             includeHTML("rmarkdowns/about_fqa2.html")
           ),#tab panel 3 parenthesis

  )#navbar parenthesis

)#ui parenthesis

server <- function(input, output, session) {

  #interactive theme
  #bs_themer()

# UPLOAD FILE FQI --------------------------------------------------------------

  #create reactive object where uploads will be stored
  FQI_file_upload <- reactiveVal()

  #create reactive object where accepted entries will be stored
  FQI_accepted_upload <- reactiveVal()

  #When file is uploaded, upload and store in reactive object above
  observeEvent(input$FQI_uploaded_file, {
    #require that a file be uploaded
    req(input$FQI_uploaded_file)
    #getting extension
    ext <- tools::file_ext(input$FQI_uploaded_file$name)
    #reading in differently based on extension
    new_file <- switch(ext,
           csv = vroom::vroom(input$FQI_uploaded_file$datapath, delim = ","),
           tsv = vroom::vroom(input$FQI_uploaded_file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")) %>%
      #drop empty data
      filter(., rowSums(is.na(.)) != ncol(.)) %>%
      as.data.frame(.)
    #store upload in reactive object
    FQI_file_upload(new_file)
    })


  #species drop-down list based on region
  output$FQI_colname <- renderUI({
    #create list of latin names based on regional list selected
    colnames <- c("", colnames(FQI_file_upload()))
    #create a dropdown option
    selectizeInput("FQI_column", "Which Column Contains Latin Names?",
                   colnames, selected = NULL)
  })

  #this allows popups for warnings about duplicates/non-matching species
  observeEvent(input$FQI_column,{
    req(input$FQI_column)
    #list to store warnings
    warning_list <- list()
    #catch warnings
    withCallingHandlers(
      fqacalc::accepted_entries(x = FQI_file_upload()
                                %>% rename("scientific_name" = input$FQI_column),
                                key = "scientific_name",
                                db = input$FQI_db,
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
    accepted <- fqacalc::accepted_entries(x = FQI_file_upload() %>%
                                            rename("scientific_name" = input$FQI_column),
                                key = "scientific_name",
                                db = input$FQI_db,
                                native = F)
    #store it
    FQI_accepted_upload(accepted)
  })

  #render output table from uploaded file
  output$FQI_DT_upload <- DT::renderDT({
    datatable(FQI_file_upload(),
              selection = 'single',
              options = list(autoWidth = TRUE,
                             scrollX = TRUE,
                             searching = FALSE,
                             lengthChange = FALSE)
              )
  })

  #when delete all is clicked, clear all entries
  observeEvent(input$FQI_delete_upload, {
    #make an empty df
    empty_df <- NULL
    #replace reactive file upload with empty file
    FQI_file_upload(empty_df)
    #reset upload button
    shinyjs::reset("FQI_uploaded_file")
  })

  #metrics table output on FQA page
  output$FQI_DT_metrics_upload <- renderTable({
    all_metrics <- fqacalc::all_metrics(x = FQI_file_upload()
                                %>% rename("scientific_name" = input$FQI_column),
                                key = "scientific_name",
                                db = input$FQI_db)

    all_metrics
  })

  #ggplot output
  output$FQI_c_hist_upload <- renderPlot({
    c_score_plot(FQI_accepted_upload())
  })


#ENTER MANUALLY FQI-------------------------------------------------------------

  #create an object with no values but correct col names to store inputs
  data_entered_manual <- reactiveVal({data_entered_manual})

  #species drop-down list based on region
  output$FQI_latin_name <- renderUI({
    #create list of latin names based on regional list selected
    latin_names <- c("", unique(fqacalc::view_db(input$FQI_db)$scientific_name))
    #create a dropdown option
    selectizeInput("FQI_species", "Select Species", latin_names,
                   selected = NULL,
                   multiple = TRUE)
    })

  #When add species is clicked, add row
  observeEvent(input$FQI_add_species, {
    #find species
    new_entry <- data.frame(fqacalc::view_db(input$FQI_db) %>%
                              dplyr::filter(scientific_name %in% input$FQI_species))
    #bind new entry to table
    new_table = rbind(new_entry, data_entered_manual())
    #these lines discourage using multiple regional databases when entering data
    one_region <- length(unique(new_table$fqa_db)) <= 1
    shinyFeedback::feedbackDanger("FQI_db", !one_region,
                                  "selecting multiple regions is not recommended")
    #print table
    data_entered_manual(new_table)
    #reset drop down menu of latin names
    shinyjs::reset("FQI_species")
    })

  #this allows popups for warnings about duplicates/non-matching species
  observeEvent(input$FQI_add_species,{
    req(input$FQI_add_species)
    #list to store warnings
    warning_list <- list()
    #catch warnings
    withCallingHandlers(
      fqacalc::accepted_entries(x = data_entered_manual(),
                                key = "scientific_name",
                                db = input$FQI_db,
                                native = FALSE),
      #add to list
      message=function(w) {warning_list <<- c(warning_list, list(w$message))})
    #show each list item in notification
    lapply(warning_list, showNotification, type = "error", duration = NULL)
  })


  #when delete species is clicked, delete row
  observeEvent(input$FQI_delete_species,{
    #call table
    t = data_entered_manual()
    #print table
    print(nrow(t))
    #if rows are selected, delete them
    if (!is.null(input$FQI_DT_manual_rows_selected)) {
      t <- t[-as.numeric(input$FQI_DT_manual_rows_selected),]
    }
    #else show the regular table
    data_entered_manual(t)
    })

  #when delete all is clicked, clear all entries
  observeEvent(input$FQI_delete_manual_entries, {
    #make an empty df
    empty_df <- data.frame(row.names = names(fqacalc::crooked_island))
    #assign it to the reactive value
    data_entered_manual(empty_df)
  })

  output$FQI_regional_list_manual <- renderText({paste("Calculating metrics based on ",
                                                input$FQI_db)})

  #render output table from manually entered species on data entry page
  output$FQI_DT_manual <- DT::renderDT({
    datatable(data_entered_manual(),
              selection = 'single',
              options = list(autoWidth = TRUE,
                             scrollX = TRUE,
                             searching = FALSE,
                             lengthChange = FALSE)
              )
  })

  #metrics table output on FQA page
  output$FQI_DT_metrics_manual <- renderTable({
    fqacalc::all_metrics(x = data_entered_manual(), db = input$FQI_db)
  })

  #ggplot output
  output$FQI_c_hist_manual <- renderPlot({
    c_score_plot(FQI_accepted_upload(unique(data_entered_manual())))
  })

# ENTER MANUALLY COVER----------------------------------------------------------

  output$cover_regional_list_manual <- renderText({paste("Calculating metrics based on ",
                                                       input$cover_db)})

  #create reactive list of species depending on db for dropdown menus in table
  cover_species <- reactive({
    #create list of latin names based on regional list selected
    cover_species <-  c(unique(fqacalc::view_db(input$cover_db)$scientific_name))
  })

  #list of what values appear in dropdown menu depending on cover_method_select
  cover_method <- reactive({
    if(input$cover_method_select == "braun-blanquet") {
      c("+", "1", "2", "3", "4", "5")
    }
    else  if(input$cover_method_select == "daubenmire") {
      c("1", "2", "3", "4", "5", "6")
    }
    else if(input$cover_method_select == "carolina_veg_survey"){
      c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
    }
    else  if(input$cover_method_select == "usfs_ecodata"){
      c("1", "3", "10", "20", "30", "40", "50", "60", "70", "80", "90", "98")
    }
    else{
      c("1":"100")
    }
  })


  #create a df object with no values to store inputs from rhandsontable
  cover_data_entered <- reactive({
    #define table for data entered manually
    start_data = data.frame(plot_id = as.integer(NA),
                            scientific_name = factor(NA, levels = cover_species()),
                            cover = factor(NA, levels = cover_method()))
  })

  #render rhandsontable for editing
  output$cover_manual_table <- renderRHandsontable({
    rhandsontable(cover_data_entered(),
                  #allows viewer to see dropdown
                  overflow = "visible",
                  #gets rid of row names
                  rowHeaders = NULL,
                  #controls size
                  stretchH = "all",
                  height = 400
                  )
  })


  #metrics table output on FQA page
  output$cover_metrics_manual <- renderTable({
    fqacalc::all_cover_metrics(x = hot_to_r(input$cover_manual_table),
                               key = "scientific_name",
                               db = input$cover_db,
                               cover_metric = input$cover_method_select)
  })


  #ggplot output
  output$cover_c_hist_manual <- renderPlot({
    c_score_plot(fqacalc::accepted_entries(x = hot_to_r(input$cover_manual_table),
                                          key = "scientific_name",
                                          db = input$cover_db,
                                          native = F,
                                          cover_metric = input$cover_method_select))
  })

# ABOUT ------------------------------------------------------------------------




}#server brackets

#run the application
shinyApp(ui, server)
