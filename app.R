#load package
library(shiny)
library(fqacalc)
library(tidyverse)
library(shinyglide)
library(DT)

#define table for data entered manually
data_entered = data.frame(row.names = names(fqacalc::crooked_island))

#define UI for application (User Interface)
ui <- fluidPage(

  navbarPage("FQA",

    #tab panel 1
    tabPanel("Calculate FQA Metrics",

             #allow glide to be used in this tab
             glide(
               #labels for glide buttons
               next_label = "Calculate FQA Metrics",
               previous_label = "Go Back to Data Entry",

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
                               DTOutput("DT_upload")),


              #when user enters species manually, show what they enter
              conditionalPanel("input.method == 'enter'",
                               DTOutput("DT_manual")),#conditional panel parenthesis

              )#main panel parenthesis

             )#fluid row parenthesis

            ),#screen 1 parenthesis

            screen(
              fluidRow(

                conditionalPanel(
                  condition = "input.method == 'upload'",
                  #output table of metrics
                  tableOutput("DT_all_upload"),
                  #output
                  plotOutput("c_hist_upload")
                  ),#conditional 1 parenthesis

                conditionalPanel(
                  condition = "input.method == 'enter'",
                  #output table of metrics
                  tableOutput("DT_metrics_manual"),
                  #output
                  plotOutput("c_hist_manual")
                )#conditional 2 parenthesis


                )#fuildRow parenthesis

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


#RENDER FILE UPLOAD
#-------------------------------------------------------------------------------
  #creating reactive upload
  file_upload <- reactive({
    #require that a file be uploaded
    req(input$uploaded_file)
    #getting extension
    ext <- tools::file_ext(input$uploaded_file$name)
    #reading in differently based on extension
    switch(ext,
           csv = vroom::vroom(input$uploaded_file$datapath, delim = ","),
           tsv = vroom::vroom(input$uploaded_file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file"))
    })#file upload reactive parenthesis

  #render output table from uploaded file
  output$DT_upload <- renderDT(
    datatable(file_upload(),
              selection = 'single',
              options = list(autoWidth = FALSE,
                             scrollX = TRUE,
                             dom = 't')
  ))

  # #when delete all is clicked, clear all entries
  # observeEvent(input$delete_upload, {
  #   #make an empty df
  #   file_upload() <- NULL
  # })

  # #metrics table output on FQA page
  # output$DT_metrics_upload <- renderTable({
  #   fqacalc::all_metrics(x = file_upload(), db = input$db)
  # })
  #
  # #ggplot output
  # output$c_hist_upload <- renderPlot({
  #   ggplot(data = unique(file_upload()),
  #          aes(x = file_upload()$c,
  #              fill = file_upload()$native)) +
  #     geom_histogram(bins = 11) +
  #     labs(title = "Conservation Coefficient Histogram",
  #          x = "Conservation Coefficient Score",
  #          fill = "Native or Exotic") +
  #     theme_classic()
  # })


#ENTER SPECIES MANUALLY
#-------------------------------------------------------------------------------
  #species drop-down list based on region
  output$latin_name <- renderUI({
    #create list of latin names based on regional list selected
    latin_names <- c("", unique(fqacalc::view_db(input$db)$scientific_name))
    #create a dropdown option
    selectizeInput("species", "Select Species", latin_names, selected = NULL)
    })# latin names parenthesis

  #create an object with no values to store inputs
  data_entered <- reactiveVal(data_entered)

  #When add species is clicked, add row
  observeEvent(input$add_species, {
    #find species
    new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                              dplyr::filter(scientific_name == input$species))
    #bind new entry to table
    new_table = rbind(new_entry, data_entered())
    #print table
    data_entered(new_table)
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
  output$DT_manual <- renderDT({
    datatable(data_entered(),
              selection = 'single',
              options = list(autoWidth = FALSE,
                             scrollX = TRUE,
                             dom = 't'))
  })

  #metrics table output on FQA page
  output$DT_metrics_manual <- renderTable({
    fqacalc::all_metrics(x = data_entered(), db = input$db)
  })

  #ggplot output
  output$c_hist_manual <- renderPlot({
    ggplot(data = unique(data_entered()),
           aes(x = data_entered()$c,
               fill = data_entered()$native)) +
      geom_histogram(bins = 11) +
      labs(title = "Conservation Coefficient Histogram",
           x = "Conservation Coefficient Score",
           fill = "Native or Exotic") +
      theme_classic()
  })

  }#server brackets

#run the application
shinyApp(ui, server)
