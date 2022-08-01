#load package
library(shiny)
library(fqacalc)
library(tidyverse)
library(DT)


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

              conditionalPanel("input.method = 'upload' && input.uploaded_file != 0",
                               DTOutput("DT1")),


              conditionalPanel("input.method != 'upload' && input.add_species != 0",
                               DTOutput("DT2")),#conditional panel parenthesis

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

  #output for fqa calc species list
  output$latin_names <- renderUI({
    latin_names <- unique(fqacalc::view_db(input$db)$scientific_name)
    selectizeInput("latin_names", "Select Species", latin_names)
    })# latin names parenthesis


  #create an object with no values
  store <- reactiveValues()

  #when add species button is clicked, add species to df
  observeEvent(input$add_species,{

    #find species
     new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                              dplyr::filter(scientific_name == input$latin_names))

      #if there is a value already stored, add new species, else store new entry
    if("value" %in% names(store)){
      store$value <- bind_rows(store$value, new_entry)
    } else {
      store$value <- new_entry
    }

     })#observe event parenthesis
#
#   #when delete species button is clicked, delete species from df
#   observeEvent(input$delete_species,{
#
#     #find species
#     new_entry <- data.frame(fqacalc::view_db(input$db) %>%
#                               dplyr::filter(scientific_name == input$latin_names))
#
#     #if there is a value already stored, add new species, else store new entry
#     if("value" %in% names(store)){
#       store$value<-bind_rows(store$value, new_entry)
#     } else {
#       store$value <- new_entry
#     }
#
#   })#observe event parenthesis


  #render output table from uploaded file
  output$DT1 <- renderDT(input$uploaded_file)

  #render output table from manually entered species
  output$DT2 <- renderDT({
    store$value
    })#output data table parenthesis





  }#server brackets

#run the application
shinyApp(ui, server)
