#load packages
library(shiny) #for app
library(fqacalc) #for fqai metrics
library(dplyr) #for data wrangling/piping
library(stringr) #for string manipulation
library(ggplot2) #for graphics
library(shinyglide) #for glide panels
library(readxl) #for reading in xl files
library(DT) #for displaying tables
library(shinyjs) #for using javascript
library(shinyalert) #for popups
library(shinyWidgets) #for some custom widgets
library(shinydashboard) #for boxes
library(sf) #for spatial data
library(tmap) #for interactive map
library(rlang)
#library(bslib) #for theme
#library(thematic) #for theme r graphics (was not working with plot)

#thematic for theme of plots
#thematic::thematic_shiny()

# JS cursor refocus function
jscode <- "shinyjs.refocus = function(id) {
  document.getElementById(id).focus();}"

#define UI for application (User Interface)
ui <- fluidPage(

  #testing theme
  theme = bslib::bs_theme(
    version = 4,
    # bootswatch = "yeti",
    # primary = "#5988B2", secondary = "#5988B2",
    # font_scale = 1.2
  ),

  #changing color of download button
  tags$head(tags$style(
    ".downloadButton{background:#3c8dbc;}
    .downloadButton{color: #fff;}")),

  #changing width of navbar
  tags$head(tags$style(".container-fluid {padding-right:0px; padding-left: 0px}")),

  #changing width of tab contents
  tags$head(tags$style(".tab-content {padding-right:30px;padding-left:30px;}")),

  #sets min width of sidebar panel
  tags$head(tags$style(".well { min-width: 280px; }")),

  #force shiny alerts to load
  useShinyalert(force = TRUE),

  #css to change shinyalert text
  tags$head(
    tags$style(
      HTML(".sweet-alert p {
          color: #000000;
          font-size: 16px;
          font-weight: 300;}"
      )
    )
  ),

  #css to change shinyalert text
  tags$head(
    tags$style(
      HTML(".help.sweet-alert p {
          text-align: left;}"
      )
    )
  ),

  #css to place shinyglide buttons
  tags$style(
    ".next-screen {
    position: absolute;
    top: 0px; right: 0px;
    }"
  ),

  tags$style(
    ".prev-screen {
    position: absolute;
    top: 0px; left: 0px;
    }"
  ),

  #set background color
  setBackgroundColor(color = "#F2F4F4"),

  #call this package for boxes
  useShinydashboard(),

  #call this package for reset function
  useShinyjs(),

  #create function for resetting cursor in certain position
  extendShinyjs(text = jscode, functions = "refocus"),

  #initiate navbar
  navbarPage("FQA",
             id = "navbar",

             #setting bootstrap to version 4
             theme = bslib::bs_theme(version = 4),

# ABOUT TAB---------------------------------------------------------------------

             tabPanel("About FQA",

                      #rmarkdown output here
                      includeMarkdown("rmarkdowns/about_fqa.Rmd"),

                      #tmap output here
                      tmapOutput("tmap"),

                      #some spacing at the bottom of the page
                      br(),
                      br(),
                      br(),

             ),#tab panel  parenthesis

# VIEW TAB----------------------------------------------------------------------

             tabPanel("View Regional FQA Lists",

                      #vall to view UI module
                      viewUI("view"),

                      #some spacing at the bottom of the page
                      br(),
                      br(),
                      br(),

             ),#tabPanel parenthesis

#FQI TAB------------------------------------------------------------------------

             tabPanel("Calculate FQA Metrics",

                      fqiUI("fqi")

             ),#tab panel parenthesis

# COVER TAB---------------------------------------------------------------------

             tabPanel("Caclulate Cover-Weighted FQA Metrics",

                      coverUI("cover")

             ),#tab panel parenthesis

             #footer = img(src = "ERDC.png", style = "hight: 80px; width: 160px")

# Help TAB---------------------------------------------------------------------

navbarMenu("More",
           tabPanel("About This App",
                    includeMarkdown("rmarkdowns/about_app.Rmd")),
           tabPanel("Equations",
                    shiny::withMathJax(),
                    includeMarkdown("rmarkdowns/equations.Rmd")),
           tabPanel("Cover Methods",
                    includeMarkdown("rmarkdowns/cover_mets.Rmd"),),
           tabPanel("Additional Resources")

),#tab panel parenthesis

  )#navbar parenthesis

)#ui parenthesis

server <- function(input, output, session) {

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

# FQI SERVER -------------------------------------------------------------------

  #call to fqi Module server
  fqiServer("fqi")

# COVER SERVER------------------------------------------------------------------

  #call to cover Module server function
  coverServer("cover")

# ABOUT SERVER------------------------------------------------------------------

  #interactive map output
  output$tmap <- renderTmap({
    tmap_function("spatial_data/regional_fqa_simple.gpkg")
  })

# VIEW SERVER-------------------------------------------------------------------

  #call to view Module server function
  viewServer("view")

}#server brackets

#run the application
shinyApp(ui, server)
