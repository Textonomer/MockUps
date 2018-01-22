#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage(

  dashboardHeader(title = "Textonomer: Interactive Tools for Exploring Textual Corpora",
                  titleWidth = "100%"),

  dashboardSidebar(disable = TRUE),

  dashboardBody(

    shiny::fluidRow(
      shinydashboard::box(title = "Graphical Network Analysis Tool",
                          "This tool allows researchers to explore word co-occurence via an interactive network analysis tools based on n-grams.",
                          hr(),
                          shiny::actionButton(inputId='ab1',
                                              label="Try the Interactive Network Graph!",
                                              icon = icon("share-alt"),
                                              onclick = "window.open('https://textonomer.shinyapps.io/network_graph/', '_blank')"
                          ),
                          width = 4),
      shinydashboard::box(title = "Treemap Visualization",
                          "This tools interactively creates treemaps to explore the density of words in a corpus.",
                          hr(),
                          shiny::actionButton(inputId='ab1',
                                              label="Try the Interactive Treemap!",
                                              icon = icon("tree"),
                                              onclick = "window.open('https://textonomer.shinyapps.io/treemap/', '_blank')"
                          ),
                          width = 4),
      align = "center"
    )
  )
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)

