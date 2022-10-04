#load package
library(shiny) #for app
library(fqacalc) #for fqai metrics
library(tidyverse) #for data wrangling/displaying
library(shinyglide) #for glide panels
library(DT) #for displaying tables
library(shinyjs) #for reset buttons
library(shinyFeedback) #for warning messages near widgets
library(shinyWidgets) #for dashboard layout
library(shinydashboard) #for boxes
library(tmap) #for interactive map
library(sf) #for spatial data
library(bslib) #interactive theme
library(thematic) #for theme r graphics

#define table for data entered manually
data_entered = data.frame()

#thematic for theme of plots
thematic::thematic_shiny()

#define UI for application (User Interface)
ui <- fluidPage(

  #testing theme
  theme = bslib::bs_theme(version = 4),

  #controls where notifications are displayed
  tags$head(
    tags$style(
      HTML("#shiny-notification-panel {
             position:fixed;
             top: calc(40%);
             left: calc(50%);
             opacity: 1;
             }
             "
      )
    )
  ),

  #changing color of download button
  tags$head(tags$style(
    ".downloadButton{background:#007bff;} .downloadButton{color: #fff;}")),

  #set background color
  #setBackgroundColor(color = "ghostwhite"),

  #call this pacakge for boxes
  useShinydashboard(),

  #call this package for reset function
  useShinyjs(),

  #call this package for warning/validation messages
  shinyFeedback::useShinyFeedback(),

  #initiate navbar
  navbarPage("FQA",

             #setting bootstrap to version 4
             theme = bslib::bs_theme(version = 4),

#FQI TAB------------------------------------------------------------------------

    #tab panel 1
    tabPanel("Calculate FQA Metrics",


             #allow glide to be used in this tab
             glide(
               id = "fqi",
               #labels for glide buttons
               next_label = "Calculate FQA Metrics",
               previous_label = "Go Back to Data Entry",
               #customizing where they appear
               custom_controls = div(class = "glide-controls", glideControls()),
               controls_position = "top",
               height = "100%",

               screen(

                 fluidRow(

               sidebarPanel(

                 titlePanel("Enter Data"),

                 fqiSideBarUI("fqi")

               ), #side bar panel

              mainPanel(

                fqiMainPanelUI("fqi")

              )#main panel parenthesis

                 )#fluidRow parenthesis

            ),#screen 1 parenthesis

            screen(
              # #banner telling you what regional list you're using
              # h3(textOutput({"FQI_regional_list_manual"})),
              #
              #   conditionalPanel(
              #     condition = "input[fqi-input_method] == 'upload'",
              #     #output table of metrics
              #     fluidRow(
              #     column(4, tableOutput("FQI_DT_metrics_upload")),
              #     #output
              #     column(8, plotOutput("FQI_c_hist_upload"))
              #     )#fuildrow parenthesis
              #     ),#conditional 1 parenthesis
              #
              #   conditionalPanel(
              #     condition = "input[fqi-input_method] == 'enter'",
              #     #output table of metrics
              #     fluidRow(
              #       valueBox(
              #         htmlOutput("fqi_species_richness_manual"),
              #         "Species Richness",
              #         icon = icon("tree"), color = "green"
              #       ),
              #       valueBox(
              #         htmlOutput("fqi_mean_c_manual"),
              #         "Mean C",
              #         icon = icon("seedling"), color = "green"
              #       ),
              #       valueBox(
              #         htmlOutput("fqi_fqi_manual"),
              #         "Total FQI",
              #         icon = icon("pagelines"), color = "green"
              #       )
              #
              #     ),#fluidRow partenthesis
              #     fluidRow(
              #     column(4, tableOutput("FQI_DT_metrics_manual")),
              #     #output
              #     column(8, plotOutput("FQI_c_hist_manual"))
              #     )#fluidrow parenthesis
              #  )#conditional 2 parenthesis

              )#screen 2 parenthesis

            )#glide parenthesis

          ),#tab panel 1 parenthesis

# COVER TAB --------------------------------------------------------------------

    #tab panel 2
    tabPanel("Caclulate Cover-Weighted FQA Metrics",

             #allow glide to be used in this tab
             glide(
               id = "cover",
               #labels for glide buttons
               next_label = "Calculate FQA Metrics",
               previous_label = "Go Back to Data Entry",
               #customizing where they appear
               custom_controls = div(class = "glide-controls", glideControls()),
               controls_position = "top",
               height = "100%",

               screen(
                 fluidRow(
                   sidebarPanel(

                     #title of side bar
                     titlePanel("Enter Data"),

                     #call to cover module
                     coverSideBarUI("cover")

                   ),#sidebarPanel parenthesis

                   mainPanel(

                     conditionalPanel(

                       condition = "input['cover-input_method'] == 'enter'",

                       #buttons for cover data entry
                       coverMainPanelUI("cover")

                       )#conditional panel parenthesis

                     )#main panel parenthesis

                   )#fluid row parenthesis

                 ),#screen 1 parenthesis

               screen(

                  #conditional panel when cover input method is manual entry
                   conditionalPanel(

                     condition = "input['cover-input_method'] == 'enter'",

                     coverOutputUI("cover")

                 )#conditional 1 parenthesis

               )#screen two parenthesis

             )#glide parenthesis

    ),#tab panel 2 parenthesis

# ABOUT ------------------------------------------------------------------------

  tabPanel("About FQA",

           #rmarkdown output here
           includeHTML("rmarkdowns/about_fqa2.html"),

           #tmap output here
           tmapOutput("tmap"),

           #some spacing at the bottom of the page
           br(),
           br(),
           br(),

           ),#tab panel 3 parenthesis

# VIEW DB ------------------------------------------------------------------------

   tabPanel("View Regional FQA Lists",

            #vall to view UI module
            viewUI("view"),

            #some spacing at the bottom of the page
            br(),
            br(),
            br(),

            )#tabPanel parenthesis

  )#navbar parenthesis

)#ui parenthesis

server <- function(input, output, session) {


  #interactive theme
  #bs_themer()

# UPLOAD FILE FQI --------------------------------------------------------------

  #creating a reactive value for glide page, used as input to server fun
  fqi_glide <- reactive({input$shinyglide_index_cover})

  #call to fqi module server
  fqiServer("fqi", fqi_glide)

# COVER----------------------------------------------------------

  #creating a reactive value for glide page, used as input to server fun
  cover_glide <- reactive({input$shinyglide_index_cover})

  #call to coverModule server function
  coverServer("cover", cover_glide)


# ABOUT ------------------------------------------------------------------------

  #interactive map output
  output$tmap <- renderTmap({
    tmap_function("spatial_data/regional_fqa_simple.gpkg")
  })

# VIEW -------------------------------------------------------------------------

  #call to viewModule server function
  viewServer("view")

}#server brackets

#run the application
shinyApp(ui, server)
