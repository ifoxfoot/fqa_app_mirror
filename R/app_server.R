#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

# HELP POPUPS ------------------------------------------------------------------

  #reactives to store tab clicks
  fqi_tab <- reactiveVal(0)
  cover_tab <- reactiveVal(0)

  #if click on tab, add one to reactive
  observeEvent(input$navbar, {
    if(input$navbar == "Calculate FQA Metrics") {
      fqi_tab(fqi_tab() + 1)
    }
    if(input$navbar == "Caclulate Cover-Weighted FQA Metrics") {
      cover_tab(cover_tab() + 1)
    }
  })

  #help popup only on first click
  observe({
    if(fqi_tab() == 1 & input$navbar == "Calculate FQA Metrics")
    {fqi_help()}
    if(cover_tab() == 1 & input$navbar == "Caclulate Cover-Weighted FQA Metrics")
    {cover_help()}
  })

  #interactive theme
  #bs_themer()

# ABOUT SERVER------------------------------------------------------------------

  #interactive map output
  output$tmap <- renderTmap({
    tmap_function("spatial_data/regional_fqa_simple.gpkg")
  })

# VIEW SERVER-------------------------------------------------------------------

  #call to view Module server function
  mod_view_db_server("view_db_1")

# INVENTORY SERVER -------------------------------------------------------------------

  #call to inventory Module server
  mod_inventory_server("inventory_1")

# COVER SERVER------------------------------------------------------------------

  #call to cover Module server function
  coverServer("cover")

}
