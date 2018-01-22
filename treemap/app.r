library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(highcharter)
source('utils.r')

ui <- dashboardPage(
  dashboardHeader(title = "EEBO Treemap Visualization", titleWidth = "100%"),

  dashboardSidebar(
    width = 250,
    selectInput("level_1",
                label = "Top level hierarchy",
                choices = feat,
                selected = "Year"),

    selectInput("level_2",
                label = "Second level hierarchy",
                choices = remaining("Year")),

    selectInput("level_3",
                label = "Third level hierarchy",
                choices = remaining("Year", "None")),

    div(style = "display:inline-block;width:100%;text-align:center;",
        actionButton("createTreemap", "Create Treemap"))
  ),

  dashboardBody(

    # controls notification positioning
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position: fixed;
             top: calc(90%);
             left: calc(50%);
             }")
      )
    ),

    # `withSpinner` adds the loading dots
    highchartOutput("treemap", width = "100%", height = "500px") %>%
      withSpinner(type = 7)
  )
)


server <- function(input, output, session) {

  # update drop down menu items
  observe({
    updateSelectInput(
      session,
      inputId = "level_2",
      label = "Second level hierarchy",
      choices = remaining(input$level_1)
    )
  })

  observe({
    updateSelectInput(
      session,
      inputId = "level_3",
      label = "Third level hierarchy",
      choices = remaining(input$level_1, input$level_2)
    )
  })

  # load a serialize highcharter plot from disk and render it
  output$treemap <- renderHighchart({

    # only update/create a treemap when the user hits the Create Treemap button
    if (length(input$createTreemap) != 0) {
      if (input$createTreemap > 0) {

        # displays for fixed time. preferably you could use one of the APIs to
        # Progress objects so the notification only lasts during the time
        # it takes the plot to load. unfortunately, this is not possible.
        # Progress objects can only update progress notifications based on
        # progress in R code. here, however, the expensive operation
        # is the JavaScript rendering of a treemap with many cells. R only reads
        # a serialized object from disk, which is nearly instanteous.
        #
        # see SO link below for details and a reproducible example
        # https://stackoverflow.com/questions/44876480/shiny-display-notification-until-browser-is-no-longer-busy

        showNotification(
          "Generating treemap. May take several seconds.",
          duration = 3.5,
          type = "warning",
          closeButton = FALSE
        )

        # progress <- shiny::Progress$new()
        # on.exit(progress$close())
        # progress$set(message = 'Generating treemap. May take several seconds.')

        isolate({load_hctreemap(input$level_1, input$level_2, input$level_3)})
      }
    }
  })
}

shinyApp(ui = ui, server = server)
