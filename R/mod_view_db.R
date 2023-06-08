#' view_db UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_view_db_ui <- function(id){
  ns <- NS(id)
  tagList(

    column(12,

           fluidRow(
             div(
               class = "d-flex",
               style = "margin-top: 32px;",

               #select database to view/download
               selectInput(ns("db"), label = "Select Regional FQA Database",
                           choices = fqacalc::db_names()$fqa_db,
                           selected = "michigan_2014"),

               #download button
               downloadButton(ns("download"),
                              label = "Download",
                              class = "downloadButton",
                              style = "margin-top: 32px; height: 38px; margin-left: 10px;"
               )
             )
           ),

           fluidRow(
             strong("Citation"),
             #citation output
             textOutput(NS(id, "citation"))),

           fluidRow(
             #table of db output
             DT::dataTableOutput(ns("regional_database_table")))

    )
  )
}

#' view_db Server Functions
#'
#' @noRd
mod_view_db_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #download button server code
    output$download <- downloadHandler(
      filename = function() {
        paste(input$db, "_", Sys.Date(), '.csv')
      },
      content = function(con) {
        write.csv(fqacalc::view_db(input$db), con)
      })

    #citation text output
    output$citation <- renderText({
      req(input$db)
      dplyr::filter(fqadata::fqa_citations, fqa_db == input$db)$citation
    })

    #fqa datatable output
    output$regional_database_table <- DT::renderDataTable({
      req(input$db)
      DT::datatable(fqacalc::view_db(input$db),
                    options = list(scrollX=TRUE,
                                   scrollY= TRUE,
                                   paging = FALSE, searching = TRUE,
                                   fixedColumns = TRUE, autoWidth = TRUE,
                                   ordering = TRUE))
    })

  })
}

