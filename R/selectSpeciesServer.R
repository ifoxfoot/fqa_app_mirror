#server UI output
selectSpeciesServer <- function(id, shiny_glide) {
  moduleServer(id, function(input, output, session) {

    #species drop-down list based on regional list selected
    output$select_species <- renderUI({
      #create list of latin names based on regional list selected
      latin_names <- c("", unique(fqacalc::view_db(input$db)$scientific_name))
      #create a dropdown option
      selectizeInput(session$ns("species"), "Species", latin_names,
                    selected = NULL,
                    multiple = FALSE)
    })

    })
}
