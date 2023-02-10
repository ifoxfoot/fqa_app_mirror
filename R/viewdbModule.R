#UI-----------------------------------------------------------------------------

viewUI <- function(id) {
  tagList(

    column(12,

    fluidRow(

      #select database to view/download
      selectInput(NS(id, "db"), label = "Select Regional FQA Database",
                  choices = fqacalc::db_names()$fqa_db,
                  selected = "michigan_2014"),

      #download button
      downloadButton(NS(id, "download"),
                     label = "Download",
                     class = "downloadButton",
                     style = "margin-top: 32px; height: 38px; margin-left: 10px;"
                     )
      ),

    fluidRow(
      strong("Citation"),
      #table of db
      textOutput(NS(id, "citation"))),

    fluidRow(
    #table of db
    dataTableOutput(NS(id, "regional_database_table")))

    )
  )}

#Server-------------------------------------------------------------------------

viewServer <- function(id) {
   moduleServer(id, function(input, output, session) {

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
      filter(fqadata::fqa_citations, fqa_db == input$db)$citation
    })

    #fqa datatable output
    output$regional_database_table <- renderDataTable({
      req(input$db)
      datatable(fqacalc::view_db(input$db),
                #options
                options = list(scrollX=TRUE,
                               scrollY= TRUE,
                               paging = FALSE, searching = TRUE,
                               fixedColumns = TRUE, autoWidth = TRUE,
                               ordering = TRUE))
      })
    })
  }
