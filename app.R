#load package
library(shiny) #for app
library(fqacalc) #for fqai metrics
library(tidyverse) #for data wrangling/displaying
library(shinyglide) #for glide panels
library(DT) #for dsiplaying tables
library(shinyjs) #for reset buttons
library(shinyFeedback) #for warning messages near widgets

#define table for data entered manually
data_entered = data.frame()

#define UI for application (User Interface)
ui <- fluidPage(

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

  #call this package for reset function
  useShinyjs(),
  #call this package for warningish messages
  shinyFeedback::useShinyFeedback(),

  navbarPage("FQA",

    #tab panel 1
    tabPanel("Calculate FQA Metrics",

             #allow glide to be used in this tab
             glide(
               #labels for glide buttons
               next_label = "Calculate FQA Metrics",
               previous_label = "Go Back to Data Entry",
               #customizing where they appear
               controls_position = "bottom",
               height = "100%",

               screen(

             fluidRow(
               sidebarPanel(

                 titlePanel("Enter Data"),

                 #input regional data base
                 selectInput("db", label = "Select Regional FQAI Database",
                          choices = fqacalc::db_names(),
                          selected = "michigan_2014"),

                 #input data entry method
                 radioButtons("method", label = "Select Data Entry Method",
                           choices = c("Upload a File" = "upload",
                                       "Enter Species Manually" = "enter")),

                 #when data entry method is upload, allow user to upload files
                 conditionalPanel(

                   condition = "input.method == 'upload'",

                   #input file upload widget
                   fileInput("uploaded_file", NULL, buttonLabel = "Upload...", multiple = F),


                   #input what column to use to bind to FQA database
                   uiOutput("colname"),

                   #input button to delete uploaded file
                   actionButton("delete_upload", "Delete Uploaded File")

                   ), #conditional 1 parenthesis


                #when data entry method is enter, allow user to enter data manually
                conditionalPanel(

                  condition = "input.method == 'enter'",

                  #input latin name
                  uiOutput("latin_name"),

                  #input add species button
                  actionButton("add_species", "Add Species"),

                  #input delete speces button
                  actionButton("delete_species", "Delete Species"),

                  #button to delete all entries
                  actionButton("delete_manual_entries", "Delete All Entries")

                ), #conditional 2 parenthesis

               ), #side bar panel

            mainPanel(

              #when user uploads file, show uploaded table
              conditionalPanel("input.method == 'upload' && input.uploaded_file != 0",
                               dataTableOutput("DT_upload")),


              #when user enters species manually, show what they enter
              conditionalPanel("input.method == 'enter'",
                               dataTableOutput("DT_manual")),

              )#main panel parenthesis

             )#fluid row parenthesis

            ),#screen 1 parenthesis

            screen(

              fluidRow(

                conditionalPanel(
                  condition = "input.method == 'upload' && input.column",
                  #output table of metrics
                  column(4,
                  tableOutput("DT_metrics_upload")),
                  #output
                  column(8,
                  plotOutput("c_hist_upload"))
                  ),#conditional 1 parenthesis

                conditionalPanel(
                  condition = "input.method == 'enter'",
                  #output table of metrics
                  column(4,
                  tableOutput("DT_metrics_manual")),
                  #output
                  column(8,
                  plotOutput("c_hist_manual"))
                )#conditional 2 parenthesis



                )#fluid Row parenthesis

              )#screen 2 parenthesis

            )#glide parenthesis

          ),#tab panel 1 parenthesis

    #tab panel 2
    tabPanel("Caclulate FQA Transect Metrics",
           fluidRow(
             mainPanel(
                       )#main panel parenthesis
                   )#fluid row parenthesis
           ),#tab panel 2 parenthesis

  )#navbar parenthesis

)#ui parenthesis

server <- function(input, output, session) {


#UPLOAD FILE--------------------------------------------------------------------

  #create reactive object where uploads will be stored
  file_upload <- reactiveVal()

  #When file is uploaded, upload and store in reactive object above
  observeEvent(input$uploaded_file, {
    #require that a file be uploaded
    req(input$uploaded_file)
    #getting extension
    ext <- tools::file_ext(input$uploaded_file$name)
    #reading in differently based on extension
    new_file <- switch(ext,
           csv = vroom::vroom(input$uploaded_file$datapath, delim = ","),
           tsv = vroom::vroom(input$uploaded_file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")) %>%
      #drop empty data
      filter(., rowSums(is.na(.)) != ncol(.)) %>%
      as.data.frame(.)
    #store upload in reactive object
    file_upload(new_file)
    })

  #this allows popups for warnings about duplicates/non-matching species
  observeEvent(input$column,{
    #require the column to be selected
    req(input$column)
    #catch the warning
    a <- tryCatch(fqacalc::all_metrics(x = file_upload()
                                          %>% rename("scientific_name" = input$column),
                                       key = "scientific_name",
                                       db = input$db),

                  warning = function(w) {w} )
   #store the message
   mess <- a$message
   #show notification
   showNotification(mess, duration = NULL, type = "error")
   })


  #species drop-down list based on region
  output$colname <- renderUI({
    #create list of latin names based on regional list selected
    colnames <- c("", colnames(file_upload()))
    #create a dropdown option
    selectizeInput("column", "Which Column Contains Latin Names?",
                   colnames, selected = NULL)
  })

  #render output table from uploaded file
  output$DT_upload <- DT::renderDT({
    datatable(file_upload(),
              selection = 'single',
              options = list(autoWidth = TRUE,
                             scrollX = TRUE))
  })

  #when delete all is clicked, clear all entries
  observeEvent(input$delete_upload, {
    #make an empty df
    empty_df <- NULL
    #replace reactive file upload with empty file
    file_upload(empty_df)
    #reset upload button
    shinyjs::reset("uploaded_file")
  })


  #metrics table output on FQA page
  output$DT_metrics_upload <- renderTable({
    all_metrics <- fqacalc::all_metrics(x = file_upload()
                                %>% rename("scientific_name" = input$column),
                                key = "scientific_name",
                                db = input$db)

    all_metrics
  })




  #ggplot output
  output$c_hist_upload <- renderPlot({

    #ggplot
    graph <- ggplot(data = fqacalc::accepted_entries
                (x = as.data.frame(as.data.frame(file_upload()) %>%
                                    rename("scientific_name" = input$column)),
                                  key = "scientific_name",
                                  db = input$db),
       aes(x = c,
           fill = native)) +
      geom_histogram(col = "black") +
      scale_x_continuous(breaks = seq(0,10, by=1), limits = c(-1,11)) +
      labs(title = "Conservation Coefficient Histogram",
           x = "Conservation Coefficient Score",
           fill = "Native or Exotic") +
      theme_classic() +
      theme(title = element_text(face="bold"))


  #call graph
  graph
  })


#ENTER SPECIES MANUALLY---------------------------------------------------------
  #species drop-down list based on region
  output$latin_name <- renderUI({
    #create list of latin names based on regional list selected
    latin_names <- c("", unique(fqacalc::view_db(input$db)$scientific_name))
    #create a dropdown option
    selectizeInput("species", "Select Species", latin_names,
                   selected = NULL,
                   multiple = TRUE)
    })

  #create an object with no values to store inputs
  data_entered <- reactiveVal({
    data_entered
    })

  #When add species is clicked, add row
  observeEvent(input$add_species, {
    #find species
    new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                              dplyr::filter(scientific_name %in% input$species))
    #bind new entry to table
    new_table = rbind(new_entry, data_entered())

    #these lines discourage using multiple regional databases when entering data
    one_region <- length(unique(new_table$fqa_db)) == 1
    shinyFeedback::feedbackDanger("db", !one_region,
                                  "selecting multiple regions is not recommended")

    #print table
    data_entered(new_table)
    #reset drop down menu of latin names
    shinyjs::reset("species")
    })


  #when delete species is clicked, delete row
  observeEvent(input$delete_species,{
    #call table
    t = data_entered()
    #print table
    print(nrow(t))
    #if rows are selected, delete them
    if (!is.null(input$DT_manual_rows_selected)) {
      t <- t[-as.numeric(input$DT_manual_rows_selected),]
    }
    #else show the regular table
    data_entered(t)
    })

  #when delete all is clicked, clear all entries
  observeEvent(input$delete_manual_entries, {
    #make an empty df
    empty_df <- data.frame(row.names = names(fqacalc::crooked_island))
    #assign it to the reactive value
    data_entered(empty_df)
  })

  #render output table from manually entered species on data entry page
  output$DT_manual <- DT::renderDT({
    datatable(data_entered(),
              selection = 'single',
              options = list(autoWidth = TRUE,
                             scrollX = TRUE))
  })

  #metrics table output on FQA page
  output$DT_metrics_manual <- renderTable({
    fqacalc::all_metrics(x = data_entered(), db = input$db)
  })

  #ggplot output
  output$c_hist_manual <- renderPlot({
    ggplot(data = unique(data_entered()),
           aes(x = c,
               fill = native)) +
      geom_histogram(col = "black") +
      scale_x_continuous(breaks = seq(0,10, by=1), limits = c(-1,11)) +
      labs(title = "Conservation Coefficient Histogram",
           x = "Conservation Coefficient Score",
           fill = "Native or Exotic") +
      theme_classic() +
      theme(title = element_text(face = "bold"))
  })

  }#server brackets

#run the application
shinyApp(ui, server)
