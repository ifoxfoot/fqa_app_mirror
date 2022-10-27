#load package
library(shiny) #for app
library(fqacalc) #for fqai metrics
library(tidyverse) #for data wrangling/displaying
library(rlang) #for .data and := functions
library(shinyglide) #for glide panels
library(readxl) #for reading in xl files
library(DT) #for displaying tables
library(shinyjs) #for reset buttons, hiding elements,
library(shinyalert) #for popups
library(shinyFeedback) #for warning messages near widgets
library(shinyWidgets) #for dashboard layout
library(shinydashboard) #for boxes
library(sf) #for spatial data
library(tmap) #for interactive map
library(bslib) #interactive theme
library(thematic) #for theme r graphics

#thematic for theme of plots
thematic::thematic_shiny()

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
  useShinyalert(force=TRUE),

  # #css to center shinyalert popups
  # tags$head(
  #   tags$style(
  #     HTML(".alert {
  #            position: fixed;
  #            top: calc(10%);
  #            left: calc(50%);
  #            height: calc(40%);
  #            }"
  #     )
  #   )
  # ),

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

  #call this package for warning/validation messages
  #shinyFeedback::useShinyFeedback(),

  #initiate navbar
  navbarPage("FQA",

             #setting bootstrap to version 4
             theme = bslib::bs_theme(version = 4),

# ABOUT TAB---------------------------------------------------------------------

             tabPanel("About FQA",

                      #rmarkdown output here
                      includeHTML("rmarkdowns/about_fqa2.html"),

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
           tabPanel("About this app"),
           tabPanel("FQA Assessments"),
           tabPanel("Cover-Weighted FQA Assessments"),
           tabPanel("Formulas",
                    includeHTML("rmarkdowns/equations.html")),
           tabPanel("Cover Methods"),
           tabPanel("Additional Resources"),
           tabPanel("Citations")

),#tab panel parenthesis

  )#navbar parenthesis

)#ui parenthesis

server <- function(input, output, session) {

  #interactive theme
  #bs_themer()

# FQI SERVER -------------------------------------------------------------------

  #call to fqi module server
  fqiServer("fqi")

# COVER SERVER------------------------------------------------------------------

  #call to coverModule server function
  coverServer("cover")

# ABOUT SERVER------------------------------------------------------------------

  #interactive map output
  output$tmap <- renderTmap({
    tmap_function("spatial_data/regional_fqa_simple.gpkg")
  })

# VIEW SERVER-------------------------------------------------------------------

  #call to viewModule server function
  viewServer("view")

}#server brackets

#run the application
shinyApp(ui, server)
