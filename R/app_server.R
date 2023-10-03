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
  inventory_tab <- reactiveVal(0)
  cover_tab <- reactiveVal(0)

  #if click on tab, add one to reactive
  observeEvent(input$navbar, {
    if(input$navbar == "Calculate Inventory Metrics") {
      inventory_tab(inventory_tab() + 1)
    }
    if(input$navbar == "Calculate Cover-Weighted FQA Metrics") {
      cover_tab(cover_tab() + 1)
    }
  })

  #help popup only on first click
  observe({
    if(inventory_tab() == 1 & input$navbar == "Calculate Inventory Metrics")
    {inventory_help()}
    if(cover_tab() == 1 & input$navbar == "Calculate Cover-Weighted FQA Metrics")
    {cover_help()}
  })

# ABOUT SERVER------------------------------------------------------------------

  #interactive map output
  output$tmap <- tmap::renderTmap({
    tmap_function()
  })

# VIEW SERVER-------------------------------------------------------------------

  #call to view Module server function
  mod_view_db_server("view_db")

# INVENTORY SERVER -------------------------------------------------------------------

  #call to inventory Module server
  mod_inventory_server("inventory")

# COVER SERVER------------------------------------------------------------------

  #call to cover Module server function
  mod_cover_server("cover")

}
