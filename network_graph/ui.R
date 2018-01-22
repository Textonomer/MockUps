library(shiny)
library(shinydashboard)
source('server.R', local=FALSE)

dashboardPage(
  dashboardHeader(title='Graphical Text Analyzer'),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidPage(
      fluidRow(
        # Column for choosing which options to edit
        shiny::column(width=4,
          box(width=12, collapsible = TRUE, title = "Data Choice Options:",
              selectizeInput("corpusChooser", "Which Corpus to Visualize:", choices=list.files(path="./Individual_Corpa")),
              actionButton("chooseCorpus", "Select Corpus"),
              tags$style(type='text/css', "#chooseCorpus { width:35%; display:block; margin: auto}")
          ),
          conditionalPanel(condition = "input.chooseCorpus>0",
            box(width=12, title = "Graph Options:", collapsible = TRUE,
                fluidRow(shiny::column(12, selectizeInput("centerWord", "Central Word:", choices=NULL))),
                fluidRow(shiny::column(6, textInput(inputId='numNodes', label='# Of Nodes in Path', value="3"),
                                       checkboxInput("useNodeSize", label="Disable Node Sizing based on Frequency"),
                                       checkboxInput("removeSimpleStopWords", label="Remove Simple Stop Words"),
                                       checkboxInput("removeGlasglowStopWords", 
                                                     label=a("Remove Glasgow Stop Words", 
                                                             href="http://ir.dcs.gla.ac.uk/resources/linguistic_utils/stop_words", 
                                                             target="blank"))),
                         shiny::column(6, textInput(inputId='numEdges', label='# Of Edges per Node', value="3"),
                                       checkboxInput("useEdgeSize", label="Disable Edge Sizing based on Frequency"),
                                       checkboxInput("removeLextecStopWords", 
                                                     label=a("Remove Lextec Stop Words", 
                                                             href="http://www.lextek.com/manuals/onix/stopwords1.html", 
                                                             target="blank"))
                         )
                ),
                fluidRow(actionButton("createGraph", "Create Graph"),
                         tags$style(type='text/css', "#createGraph { width:35%; display:block; margin: auto}"),
                         textOutput("errorMessages")
                )
            )
          )
        ),
        # Column for the graph
        shiny::column(width = 8,
                      box(width=12, visNetworkOutput("graphViz", height = "600px"))
        )
      )
    )
  )
)