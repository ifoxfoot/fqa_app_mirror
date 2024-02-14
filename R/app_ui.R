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

    #call this package for reset function
    shinyjs::useShinyjs(),

    #create function for resetting cursor in certain position
    shinyjs::extendShinyjs(text = jscode, functions = "refocus"),

    #initiate navbar
    navbarPage(
      title = tags$div(img(src='www/usace_logo2.png',
                  style="position: relative;
                         margin:0px 0px -25px;
                         display:right-align;",
                  height = 30)),

      windowTitle="Floristic Quality Assessment",

      id = "navbar",
               #setting bootstrap to version 5
               theme = bslib::bs_theme(version = 5,
                                       #bootswatch = "yeti",
                                       base_font = c("Arial", "monospace"),
                                       #primary = "#5988B2", secondary = "#5988B2",
                                       #font_scale = 1.2
                                       ),

# ABOUT TAB---------------------------------------------------------------------

               tabPanel("About FQA",

                        #rmarkdown output here
                        htmltools::includeMarkdown("rmarkdowns/about_fqa.md"),

                        #tmap output here
                        tmap::tmapOutput("tmap"),

                        #some spacing at the bottom of the page
                        br(),
                        br(),
                        br()

               ),#tab panel  parenthesis

# VIEW TAB----------------------------------------------------------------------

               tabPanel("View Regional FQA Databases",

                        #vall to view UI module
                        mod_view_db_ui("view_db"),

                        #some spacing at the bottom of the page
                        br(),
                        br(),
                        br()

               ),#tabPanel parenthesis

#INVENTORY TAB------------------------------------------------------------------------

               tabPanel("Calculate Inventory Metrics",

                        mod_inventory_ui("inventory"),

                        #some spacing at the bottom of the page
                        br(),
                        br(),
                        br()

               ),#tab panel parenthesis

# COVER TAB---------------------------------------------------------------------

               tabPanel("Calculate Cover-Weighted FQA Metrics",

                        mod_cover_ui("cover"),

                        #some spacing at the bottom of the page
                        br(),
                        br(),
                        br()

               ),#tab panel parenthesis


# Help TAB---------------------------------------------------------------------

               navbarMenu("More",
                          tabPanel("About This App",
                                   includeMarkdown("rmarkdowns/about_app.Rmd"),
                                   #some spacing at the bottom of the page
                                   br(),
                                   br(),
                                   br()),
                          tabPanel("Equations",
                                   shiny::withMathJax(
                                     includeMarkdown("rmarkdowns/equations.Rmd")),
                                   #some spacing at the bottom of the page
                                   br(),
                                   br(),
                                   br()),
                          tabPanel("Cover Classes",
                                   includeMarkdown("rmarkdowns/cover_mets.Rmd"),
                                   #some spacing at the bottom of the page
                                   br(),
                                   br(),
                                   br()),
                          tabPanel("Additional Resources",
                                   includeMarkdown("rmarkdowns/resources.Rmd"),
                                   #some spacing at the bottom of the page
                                   br(),
                                   br(),
                                   br())



               ),#tab panel parenthesis

       footer =
  fluidRow(
         tags$div(
           class = "footer",
           img(src = "www/army_star.png",
               style="margin-top:10px;
               padding-right:10px;
               padding-bottom:10px",
               height = 60),
           img(src = "www/USACE_text.png",
               style="margin-top:10px;
               padding-right:5px;
               padding-bottom:8px",
               height = 60),
           img(src = "www/ERDC_logo.png",
               style="margin-top:10px;
               padding-right:10px;
               padding-bottom:10px",
               height = 60)
         )
       )


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
