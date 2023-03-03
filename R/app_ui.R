#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
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

    tags$head(
      tags$style(HTML(".view-citations.shiny-text-output.shiny-bound-output {
                display:block;
                padding:9.5px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:16px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px;
            }"))),

    #force shiny alerts to load
    shinyalert::useShinyalert(force = TRUE),

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
    shinyWidgets::setBackgroundColor(color = "#F2F4F4"),

    #call this package for boxes
    shinyWidgets::useShinydashboard(),

    #call this package for reset function
    shinyjs::useShinyjs(),

    #create function for resetting cursor in certain position
    shinyjs::extendShinyjs(text = jscode, functions = "refocus"),

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
                        tmap::tmapOutput("tmap"),

                        #some spacing at the bottom of the page
                        br(),
                        br(),
                        br(),

               ),#tab panel  parenthesis

# VIEW TAB----------------------------------------------------------------------

               tabPanel("View Regional FQA Databases",

                        #vall to view UI module
                        mod_view_db_ui("view_db_1"),

                        #some spacing at the bottom of the page
                        br(),
                        br(),
                        br(),

               ),#tabPanel parenthesis

#INVENTORY TAB------------------------------------------------------------------------

               tabPanel("Calculate Inventory Metrics",

                        mod_inventory_ui("inventory_1")

               ),#tab panel parenthesis

# COVER TAB---------------------------------------------------------------------

               tabPanel("Caclulate Cover-Weighted FQA Metrics",

                        mod_cover_ui("cover_1")

               ),#tab panel parenthesis

               #footer = img(src = "ERDC.png", style = "hight: 80px; width: 160px")

# Help TAB---------------------------------------------------------------------

               navbarMenu("More",
                          tabPanel("About This App",
                                   includeMarkdown("rmarkdowns/about_app.Rmd")),
                          tabPanel("Equations",
                                   shiny::withMathJax(),
                                   includeMarkdown("rmarkdowns/equations.Rmd")),
                          tabPanel("Cover Classes",
                                   includeMarkdown("rmarkdowns/cover_mets.Rmd"),),
                          tabPanel("Additional Resources")

               ),#tab panel parenthesis

    )#navbar parenthesis
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "fqashiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
