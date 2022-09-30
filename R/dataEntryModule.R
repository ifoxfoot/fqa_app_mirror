
#dropdown select regional fqa list
selectFQAI <- function(id) {
  tagList(
    #select database to view/download
    selectInput(NS(id, "db"), label = "Select Regional FQAI Database",
                choices = fqacalc::db_names(),
                selected = "michigan_2014")
  )}

#radio button to select data entry method
radioDataEntry <- function(id) {
  tagList(
  radioButtons(NS(id, "input_method"), label = "Select Data Entry Method",
               choices = c( "Enter Species Manually" = "enter",
                          "Upload a File" = "upload"))
  )}

#action button to add species
actionAddSpecies <- function(id) {
  tagList(
    #button to add species
    actionButton(NS(id,"add_species"), "Add Species"),
  )}

#button to delete species
actionDeleteSpecies <- function(id) {
  tagList(
  #input delete species button
  actionButton(NS(id, "delete_species"), "Delete Species"),
  )}

#button to delete all manual entries
actionDeleteAll <- function(id) {
  tagList(
    #button to delete all entries
    actionButton(NS(id, "delete_all"), "Delete All Entries")
  )}

#download button
downloadButtonUI <- function(id) {
  downloadButton(NS(id, "download"), label = "Download", class = "downloadButton")
}

#server UI output
selectSpeciesServer <- function(id) {
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
