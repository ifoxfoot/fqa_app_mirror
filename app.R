#load package
library(shiny)
library(fqacalc)
library(tidyverse)
library(DT)

#define table
this_table = data.frame(row.names = names(fqacalc::crooked_island))

#define UI for application (User Interface)
ui <- fluidPage(

  navbarPage("FQA",

    #tab panel 1
    tabPanel("Calculate FQA Metrics",
           fluidRow(
            sidebarPanel(

              #input regional data base
              selectInput("db", label = "Select Regional FQAI Database",
                          choices = fqacalc::db_names()),

              #input data entry method
              radioButtons("method", label = "Select Data Entry Method",
                           choices = c("Upload a File" = "upload",
                                       "Enter Species Manually" = "enter")),

              #when data entry method is upload, allow user to uploat
              conditionalPanel(

                condition = "input.method == 'upload'",

                #input file upload
                fileInput("uploaded_file", NULL, buttonLabel = "Upload...", multiple = F),

                ), #conditional 1 parenthesis

              #when data entry method is not upload, enter manually
              conditionalPanel(

                condition = "input.method != 'upload'",

                #input latin name
                uiOutput("latin_names"),

                #input add species button
                actionButton("add_species", "Add Species"),

                #input delete speces button
                actionButton("delete_species", "Delete Species")

                ), #conditional 2 parenthesis

              ),#side panel parenthesis

            mainPanel(

              #when user uploads table show that
              conditionalPanel("input.method = 'upload' && input.uploaded_file != 0",
                               DTOutput("DT_upload")),


              #when user enters species manually show that
              conditionalPanel("input.method != 'upload' && input.add_species != 0",
                               DTOutput("DT_manual")),#conditional panel parenthesis

              )#main panel parenthesis

            )#fluid row parenthesis

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
  output$DT_upload <- renderDT(file_upload())

  #ENTER SPECIES MANUALLY

  #species drop-down list based on region
  output$latin_names <- renderUI({

    #create list of latin names based on regional list selected
    latin_names <- unique(fqacalc::view_db(input$db)$scientific_name)

    #create a dropdown option
    selectizeInput("latin_names", "Select Species", latin_names)

    })# latin names parenthesis

  #create an object with no values to store inputs
  this_table <- reactiveVal(this_table)

  #When add species is clicked, add row
  observeEvent(input$add_species, {

    #find species
    new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                              dplyr::filter(scientific_name == input$latin_names))

    #bind new entry to table
    t = rbind(new_entry, this_table())

    #pring table
    this_table(t)

    })

  #when delete species is clicked, delete row
  observeEvent(input$delete_species,{

    #call table
    t = this_table()

    #print table
    print(nrow(t))

    #if rows are selected, delete them
    if (!is.null(input$DT_manual_rows_selected)) {
      t <- t[-as.numeric(input$DT_manual_rows_selected),]
    }

    #else show the regular table
    this_table(t)

    })

  #render output table from manually entered species
  output$DT_manual <- renderDT({
    datatable(this_table(), selection = 'single', options = list(dom = 't'))
    })

  }#server brackets

#run the application
shinyApp(ui, server)
