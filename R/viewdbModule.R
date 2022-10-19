#UI-----------------------------------------------------------------------------

viewUI <- function(id) {
  tagList(
    fluidRow(

      #select database to view/download
      selectInput(NS(id, "db"), label = "Select Regional FQAI Database",
                  choices = fqacalc::db_names()$name,
                  selected = "michigan_2014"),

      #download button
      downloadButton(NS(id, "download"),
                     label = "Download",
                     class = "downloadButton",
                     style = "margin-top: 32px; height: 38px; margin-left: 10px;"
                     )
      ),

    #table of db
    dataTableOutput(NS(id, "regional_database_table"))
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

    #fqa datatable output
    output$regional_database_table <- renderDataTable({
      datatable(fqacalc::view_db(input$db),
                #options
                options = list(scrollX=TRUE,
                               scrollY= TRUE,
                               lengthMenu = c(7,10,15),
                               paging = TRUE, searching = TRUE,
                               fixedColumns = TRUE, autoWidth = TRUE,
                               ordering = TRUE))
      })
    })
  }
