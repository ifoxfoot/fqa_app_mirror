#UI------------------------------------------------------------------------
#side bar UI function
coverUI <- function(id) {

  tagList(

    #allow glide to be used in this tab
    glide(
      id = NS(id, "glide"),
      #next_condition = "",
      #labels for glide buttons
      next_label = "Calculate FQA Metrics",
      previous_label = "Go Back to Data Entry",
      #customizing where they appear
      custom_controls = div(class = "glide-controls", glideControls()),
      controls_position = "top",
      height = "100%",

      screen(

        next_condition = "output['cover-next_condition'] == 'TRUE'",

        fluidRow(
          sidebarPanel(

            #title of side bar
            titlePanel("Enter Data"),

            #input regional data base
            selectInput(NS(id, "db"), label = "Select Regional FQAI Database",
                        choices = fqacalc::db_names()$name,
                        selected = "michigan_2014"),

            #input data entry method
            radioButtons(NS(id, "input_method"), label = "Select Data Entry Method",
                         choices = c( "Enter Species Manually" = "enter",
                                      "Upload a File" = "upload")),

            #select cover method
            selectInput(NS(id, "cover_method"), label = "Cover Method",
                        choices = c(
                          "percent_cover",
                          "braun-blanquet",
                          "carolina_veg_survey",
                          "daubenmire",
                          "usfs_ecodata")),

            #when data entry method is upload, allow user to upload files
            conditionalPanel(

              condition = "input['cover-input_method'] == 'enter'",

              #delete species button
              actionButton(NS(id, "delete_species"), "Delete Species"),

              #button to delete all entries
              actionButton(NS(id, "delete_all"), "Delete All Entries")

            ),#conditional parenthesis

            #when data entry method is upload, allow user to upload files
            conditionalPanel(

              condition = "input['cover-input_method'] == 'upload'",

              "UNDER CONSTRUCTION"

            ) #conditional parenthesis

          ),#sidebarPanel parenthesis

          mainPanel(


            conditionalPanel(

              condition = "input['cover-input_method'] == 'enter'",

              textOutput(NS(id, "next_condition")),

              #input transect ID
              fluidRow(
                column(4, textInput(NS(id, "transect_id"), "Transect ID"))),

              fluidRow(
                #plot id text input
                column(2, textInput(NS(id, "plot_id"), "Plot ID")),
                #select species
                column(4, uiOutput(NS(id, "select_species"))),
                #select cover value
                column(3, uiOutput(NS(id,"cover_value"))),
                #add species button, but initialize as disabled
                column(3, disabled(actionButton(NS(id, "add_species"), "Add Species",
                                                style = "margin-top: 30px; height: 40px;")))),


              fluidRow(
                column(12,
                       #datatable of entered data
                       dataTableOutput(NS(id, "cover_DT_manual")))
              )

            )#conditional panel parenthesis

          )#main panel parenthesis

        )#fluid row parenthesis

      ),#screen 1 parenthesis

      screen(

        #conditional panel when cover input method is manual entry
        conditionalPanel(

          condition = "input['cover-input_method'] == 'enter'",

          fluidRow(
            #title
            column(7, h3(textOutput(NS(id, "title")))),

            #download button
            column(2, downloadButton(NS(id, "download"),
                                     label = "Download", class = "downloadButton",
                                     style = "margin-top: 20px; height: 40px;"))),

          #boxes with key values
          fluidRow(
            valueBox(
              htmlOutput(NS(id,"species_richness")),
              "Species Richness",
              icon = icon("tree"), color = "orange"
            ),

            valueBox(
              htmlOutput(NS(id,"mean_c")),
              "Mean C",
              icon = icon("seedling"), color = "orange"
            ),
            valueBox(
              htmlOutput(NS(id,"fqi")),
              "Total FQI",
              icon = icon("pagelines"), color = "orange"
            )
          ),#fluidRow parenthesis

          #all mets and graph
          fluidRow(
            box(plotOutput(NS(id,"compare_plot")),
                title = "Compare Frequency of C Scores to Regional FQAI"),
            box(plotOutput(NS(id,"c_hist")),
                title = "Histogram of C Scores")
          ),

          fluidRow(
            column(4,
                   box(tableOutput(NS(id,"c_metrics")), title = "FQI Metrics", width = NULL)),
            column(4,
                   box(tableOutput(NS(id,"cover_metrics")), title = "Cover-Weighted Metrics", width = NULL)),
            column(4,
                   box(tableOutput(NS(id,"species_mets")), title = "Species Richness Metrics", width = NULL))
          ),

          fluidRow(
            column(4,
                   box(tableOutput(NS(id,"wetness")), title = "Wetness Metrics", width = NULL)),
            column(4,
                   box(tableOutput(NS(id,"proportion")), title = "C-Score Proportions", width = NULL)),
            column(4,
                   box(tableOutput(NS(id,"duration_table")), title = "Duration Breakdown", width = NULL))
          ),

          #output of physiog summary
          fluidRow(box(title = "Physiognomy Summary", status = "primary",
                       tableOutput(NS(id, "cover_physiog_manual")), width = 12,
                       style = "overflow-x: scroll")),

          #output of species summary
          fluidRow(box(title = "Species Summary", status = "primary",
                       tableOutput(NS(id, "cover_species_manual")), width = 12,
                       style = "overflow-x: scroll")),

          #output of plot summary
          fluidRow(box(title = "Plot Summary", status = "primary",
                       tableOutput(NS(id, "cover_plot_manual")), width = 12, style = "overflow-x: scroll"))

        )#conditional 1 parenthesis

      )#screen two parenthesis

    )#glide parenthesis

  )#taglist parenthesis

  }


#Server-------------------------------------------------------------------------

coverServer <- function(id) {

  #call server fun for species dropdown
  selectSpeciesServer(id = id)

  #start module function
  moduleServer(id, function(input, output, session) {

    #creating a reactive value for glide page, used as input to server fun
    cover_glide <- reactive({input$shinyglide_index_glide})

    #drop-down list of cover values based on cover metric input
    output$cover_value <- renderUI({
      cover_vals <-
      #list of what values appear in dropdown menu depending on cover_method_select
      if(input$cover_method == "braun-blanquet") {
        c("+", "1", "2", "3", "4", "5")
      }
      else  if(input$cover_method == "daubenmire") {
        c("1", "2", "3", "4", "5", "6")
      }
      else if(input$cover_method== "carolina_veg_survey"){
        c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
      }
      else  if(input$cover_method == "usfs_ecodata"){
        c("1", "3", "10", "20", "30", "40", "50", "60", "70", "80", "90", "98")
      }
      if (input$cover_method != "percent_cover") {
      #create a dropdown option
      selectizeInput(session$ns("cover_val"), "Cover Value", c("", cover_vals),
                     selected = NULL,
                     multiple = FALSE) }
      #else allow numeric input
      else { numericInput(session$ns("cover_val"), "Cover Value",
                          value = 0, min = 0, max = 100)}
    })

    #call server fun for species dropdown
    selectSpeciesServer(id)

    #make it so add species button can't be clicked until all fields full
    observe({
      vals <- c(input$transect_id, input$cover_val, input$species, input$plot_id)
      if (input$cover_method != "percent_cover") {
        toggleState("add_species", !"" %in% vals) }
      else { toggleState("add_species", input$cover_val > 0 & input$cover_val <= 100)}
    })

    #define table for data entered manually
    data_entered = data.frame()

    #create an object with no values but correct col names to store inputs
    cover_data <- reactiveVal({data_entered})

    #save edits
    observeEvent(input$add_species, {
      #combine entries into one-row df
      new_row <- data.frame(plot_id = input$plot_id,
                            scientific_name = input$species,
                            cover = input$cover_val)
      #bind new entry to table
      new_table = rbind(new_row, cover_data())
      #make it reactive
      cover_data(new_table)
      #reset drop down menu of latin names
      shinyjs::reset("species")
      shinyjs::reset("cover_val")
    })

    #render output table from manually entered species on data entry page
    output$cover_DT_manual <- DT::renderDT({
      datatable(cover_data(),
                selection = 'single',
                options = list(
                  scrollX = TRUE,
                  searching = FALSE,
                  lengthChange = FALSE))
    })

    #when delete species is clicked, delete row
    observeEvent(input$delete_species,{
      #call table
      t = cover_data()
      #print table
      print(nrow(t))
      #if rows are selected, delete them
      if (!is.null(input$cover_DT_manual_rows_selected)) {
        t <- t[-as.numeric(input$cover_DT_manual_rows_selected),]
      }
      #else show the regular table
      cover_data(t)
    })

    #when delete all is clicked, clear all entries
    observeEvent(input$delete_all, {
      #assign it to the reactive value
      cover_data(data_entered)
      accepted(data_entered)
    })

##accepted df ------------------------------------------------------------------
    #initialize reactives
    accepted <- reactiveVal()
    confirm_db <- reactiveVal("empty")
    confirm_cover <- reactiveVal("empty")
    previous_dbs <- reactiveValues(prev = "michigan_2014")
    previous_covers <- reactiveValues(prev = "percent_cover")

    #store current and previous db value in reactive element
    observeEvent(input$db, {
      previous_dbs$prev <- c(tail(previous_dbs$prev, 1), input$db)
    })

    #store current and previous cover method value in reactive element
    observeEvent(input$cover_method, {
      previous_covers$prev <- c(tail(previous_covers$prev, 1), input$cover_method)
    })

    #update reactive
    observe({
      accepted(data_entered)
      req(nrow(cover_data()) > 0)
      accepted(fqacalc::accepted_entries(x = cover_data(),
                                         key = "scientific_name",
                                         db = input$db,
                                         native = F,
                                         cover_metric = input$cover_method))
    })

    #create boolean that shows if data is entered or not for next condition
    output$next_condition <- renderText(
      nrow(accepted()) > 0
    )

    #hide next condition output
    observe({shinyjs::hide("next_condition",)})
    outputOptions(output, "next_condition", suspendWhenHidden=FALSE)

    #if db is changed and there is already data entered, show popup
    observeEvent(input$db, {
      req(nrow(accepted()) > 0)
      #code for popup
      if(confirm_db() != "empty") {
        confirm_db("empty") }
      else{
        shinyalert(text = strong(
          "Changing the regional database will delete your current data entries.
        Are you sure you want to proceed?"),
        showCancelButton = T,
        showConfirmButton = T, confirmButtonText = "Proceed",
        confirmButtonCol = "red", type = "warning",
        html = T, inputId = "confirm_db_change")}
    })

    observeEvent(input$confirm_db_change, {
      #store confirmation in reactive value
      confirm_db(input$confirm_db_change)
      #create an empty df
      empty_df <- data.frame()
      #if confirm db is true and method is enter, reset entered data
      if(confirm_db() == TRUE ) {
        cover_data(empty_df)
        accepted(empty_df)
        confirm_db("empty")}
      #if confirm db is false, reset db to previous value
      if (confirm_db() == FALSE) {
        updateSelectInput(session, inputId = "db",
                          selected = previous_dbs$prev[1])}
    })

    #if cover method is changed and there is already data entered, show popup
    observeEvent(input$cover_method, {
      req(nrow(accepted()) > 0)
      #code for popup
      if(confirm_cover() != "empty") {
        confirm_cover("empty") }
      else{
        shinyalert(text = strong(
          "Changing the cover method will delete your current data entries.
        Are you sure you want to proceed?"),
        showCancelButton = T,
        showConfirmButton = T, confirmButtonText = "Proceed",
        confirmButtonCol = "red", type = "warning",
        html = T, inputId = "confirm_cover_change", className = "alert")}
    })

    observeEvent(input$confirm_cover_change, {
      #store confirmation in reactive value
      confirm_cover(input$confirm_cover_change)
      #create an empty df
      empty_df <- data.frame()
      #if confirm db is true and method is enter, reset entered data
      if(confirm_cover() == TRUE ) {
        cover_data(empty_df)
        accepted(empty_df)
        confirm_cover("empty")}
      #if confirm db is false, reset db to previous value
      if (confirm_cover() == FALSE) {
        updateSelectInput(session, inputId = "cover_method",
                          selected = previous_covers$prev[1])}
    })

    #if there are duplicates, show warning
    observeEvent(input$add_species, {
      req(nrow(cover_data()) > 1)
      if( any(duplicated(cover_data() %>% select(plot_id, scientific_name))) ){
        shinyalert(text = strong(
          "Duplicate species are detected in the same plot.
          Duplicates will only be counted once, and their cover values will be added together."),
          type = "warning",  html = T, className = "alert")}
    })

##second screen-----------------------------------------------------------------

    #initializing reactives for outputs
    metrics <- reactiveVal()
    physiog_table <- reactiveVal()
    duration_table <- reactiveVal()
    species_sum <- reactiveVal()
    plot_sum <- reactiveVal()
    physiog_sum <- reactiveVal()

    #updating reactive values
    observe({
      #requiring second screen to update reactive values
      req(cover_glide() == 1)

      metrics(fqacalc::all_cover_metrics(x = cover_data(),
                               key = "scientific_name",
                               db = input$db,
                               cover_metric = input$cover_method))

      species_sum(fqacalc::species_summary(x = cover_data(),
                                           key = "scientific_name",
                                           db = input$db,
                                           cover_metric = input$cover_method))

      physiog_sum(fqacalc::physiog_summary(x = cover_data(),
                                           key = "scientific_name",
                                           db = input$db,
                                           cover_metric = input$cover_method))

      plot_sum(fqacalc::plot_summary(x = cover_data(),
                                     key = "scientific_name",
                                     db = input$db,
                                     cover_metric = input$cover_method,
                                     plot_id = "plot_id"))

      })

    #get duration table
    observe({
      req(nrow(accepted()) > 0 & cover_glide() == 1)

      duration_cats <- data.frame(duration = c("annual", "perennial", "biennial"),
                                  count = rep.int(0, 3),
                                  percent = rep.int(0,3))

      dur <- accepted() %>%
        group_by(duration) %>%
        summarise(count = n()) %>%
        mutate(percent = round((count/sum(count))*100, 2)) %>%
        rbind(duration_cats %>% filter(!duration %in% accepted()$duration))

      #store in reactive
      duration_table(dur)
    })

    #render title
    output$title <- renderText({paste("Calculating metrics based on",
                                      input$db)})

    #download cover summary server
    output$download <- downloadHandler(
      #name of file based off of transect
      filename = function() {
        paste0("FQA_assessment_transect_", input$transect_id, ".csv")
      },
      #content of file
      content = function(fname) {
        #set wd to temp directory
        tmpdir <- tempdir()
        setwd(tempdir())

        # Start a sink file with a CSV extension
        sink(fname)
        cat('\n')
        cat(paste0("Calculating metrics based on the ", input$db, " regional FQAI."))
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        cat('Cover-Weighted FQI Metrics')
        cat('\n')
        write.csv(metrics(), row.names = F)
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        cat("Duration Frequency")
        cat('\n')
        write.csv(duration_table(), row.names = F)
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        cat('Plot Summary Metrics')
        cat('\n')
        write.csv(plot_sum(), row.names = F)
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        cat('Species Summary Metrics')
        cat('\n')
        write.csv(species_sum(), row.names = F)
        cat('\n')
        cat('\n')

        # Write metrics dataframe to the same sink
        cat('Physiognomy Summary Metrics')
        cat('\n')
        write.csv(physiog_sum(), row.names = F)
        cat('\n')
        cat('\n')

        # Write data entered
        cat('Data Entered')
        cat('\n')
        write.csv(accepted(), row.names = F)

        # Close the sink
        sink()
      })

    #species richness
    output$species_richness <- renderUI({
      req(cover_glide() == 1)
      round(
        fqacalc::species_richness(x = accepted(), db = input$db, native = F),
        3)
    })

    #mean C
    output$mean_c <- renderUI({
      req(cover_glide() == 1)
      round(fqacalc::mean_c(x = accepted(), db = input$db, native = F), 3)
    })

    #total fqi
    output$fqi <- renderUI({
      req(cover_glide() == 1)
      round(fqacalc::FQI(x = accepted(), db = input$db, native = F), 3)
    })

    #C metrics table output
    output$c_metrics <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Mean C", "Native Mean C", "Total FQI",
                                     "Native FQI", "Adjusted FQI"))
    })

    #cover metrics table output
    output$cover_metrics <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Cover-Weighted Mean C", "Cover-Weighted Native Mean C",
                                     "Cover-Weighted FQI", "Cover-Weighted Native FQI"))
    })

    #wetness table output
    output$wetness <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Mean Wetness", "Native Mean Wetness"))
    })

    #nativity table output
    output$species_mets <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Total Species Richness",
                                     "Native Species Richness",
                                     "Exotic Species Richness"))
    })

    #proportion table output
    output$proportion <- renderTable({
      req(cover_glide() == 1)
      metrics() %>%
        dplyr::filter(metrics %in% c("Proportion of Species with < 1 C score",
                                     "Proportion of Species with 1-3.9 C score",
                                     "Proportion of Species with 4-6.9 C score",
                                     "Proportion of Species with 7-10 C score"))
    })

    #ggplot output
    output$c_hist <- renderPlot({
      req(cover_glide() == 1)
      c_score_plot(accepted())
    })

    #ggplot output
    output$compare_plot <- renderPlot({
      req(cover_glide() == 1)
      compare_plot(input_data = accepted(),
                   db_name = as.character(input$db),
                   db = fqacalc::view_db(input$db))
    })

    #duration table output
    output$duration_table <- renderTable({
      req(cover_glide() == 1)
      duration_table()
    })

    #plot summary
    output$cover_plot_manual <- renderTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive plot summary
      plot_sum()
    })

    #species summary
    output$cover_species_manual <- renderTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive species summary
      species_sum()
    })

    #physiog summary
    output$cover_physiog_manual <- renderTable({
      #requiring second screen
      req(cover_glide() == 1)
      #call to reactive species summary
      physiog_sum()
    })

  })
}
