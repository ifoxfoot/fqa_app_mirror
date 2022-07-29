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

              #input one
              selectInput("db", label = "Select Regional FQAI Database",
                          choices = fqacalc::db_names()),

              #input two
              fileInput("upload", NULL, buttonLabel = "Upload...", multiple = F),

              #input three
              uiOutput("latin_names"),

              #input four
              actionButton("add_species_manual", "Add Species"),


              ),#side panel parenthesis

            mainPanel(

              conditionalPanel("input.add != 0",
                               DTOutput("DT2")
                               )#conditional panel parenthesis

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

  observeEvent(input$add_species_manual,{
    new_entry <- data.frame(fqacalc::view_db(input$db) %>%
                              dplyr::filter(scientific_name == input$latin_names))
    if("value" %in% names(store)){
      store$value<-bind_rows(store$value, new_entry)
    } else {
      store$value <- new_entry
    }

    })#observe event parenthesis


  output$DT2 <- renderDT({
    store$value

    })#output data table parenthesis





  }#server brackets

#run the application
shinyApp(ui, server)
