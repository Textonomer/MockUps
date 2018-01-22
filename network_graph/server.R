library(shiny)
library(shinydashboard)
library(visNetwork)
source("stopwords.R")

buildStopWords <- function(useSimple=F, useGlasglow=F, useLextec=F) {
  stopWords <- c()
  if (useSimple) {
    stopWords <- c(stopWords, simpleStopWords)
  }
  if (useGlasglow) {
    stopWords <- c(stopWords, glasgowStopWords)
  }
  if (useLextec) {
    stopWords <- c(stopWords, lextecStopWords)
  }
  return(stopWords)
}

getMostFrequentLinks <- function(corpusDat, wordToGet, isWord1, numLinks, stopWords=c()) {
  if (isWord1) {
    outOne <- subset(corpusDat, word1 == wordToGet & !(word2 %in% stopWords))
    if (nrow(outOne) > numLinks) {
      outOne <- outOne[order(outOne$frequency, decreasing = TRUE)[1:numLinks],]
    }
  }
  else {
    outOne <- subset(corpusDat, word2 == wordToGet & !(word1 %in% stopWords))
    if (nrow(outOne) > numLinks) {
      outOne <- outOne[order(outOne$frequency, decreasing = TRUE)[1:numLinks],]
    }
  }
  return(outOne)
}

defineGraph <- function(corpusDat, centerWord, numNodesOut, numEdgePerNode, 
                        useNodeSizing, useEdgeSizing, stopWords) {
  minFreq <- min(corpusDat$frequency)
  totalWords <- sum(corpusDat$frequency/minFreq)
  pregraphDF <- getMostFrequentLinks(corpusDat, centerWord, TRUE, numEdgePerNode, stopWords)
  # Recursively add nodes to the right side until we've gone far enough out.
  rightDF <- pregraphDF
  for (i in 1:(numNodesOut - 1)) {
    nextRight <- corpusDat[0,]
    for (word in rightDF$word2) {
      nextRight <- rbind(nextRight, getMostFrequentLinks(corpusDat, word, TRUE, 
                                                         numEdgePerNode, stopWords))
    }
    if (i != 1) {
      pregraphDF <- rbind(pregraphDF, rightDF) 
    }
    rightDF <- nextRight
  }
  # Do the same for the left side
  leftDF <- getMostFrequentLinks(corpusDat, centerWord, FALSE, numEdgePerNode, stopWords)
  for (i in 1:(numNodesOut - 1)) {
    nextLeft <- corpusDat[0,]
    for (word in leftDF$word1) {
      nextLeft <- rbind(nextLeft, getMostFrequentLinks(corpusDat, word, FALSE, 
                                                       numEdgePerNode, stopWords))
    }
    pregraphDF <- rbind(pregraphDF, leftDF)
    leftDF <- nextLeft
  }

  graphWords <- unique(c(pregraphDF$word1, pregraphDF$word2))
  if (useNodeSizing) {
    nodeVals = unlist(lapply(graphWords, function(x) corpusDat$word1_freq[corpusDat$word1 == x][1]))
  }
  else {
    nodeVals = rep(1, length(graphWords))
  }
  nodeTitles <- unlist(lapply(graphWords, function(x) paste0("Word frequency:", as.character(round(corpusDat$word1_freq[corpusDat$word1 == x][1] * totalWords)))))
  nodeDF <- data.frame(id = 1:length(graphWords), 
                       label = graphWords, 
                       value = nodeVals,
                       title = nodeTitles,
                       stringsAsFactors = FALSE)
  nodeDF$value <- nodeDF$value/min(nodeDF$value)
  
  # Take the minimum frequency and divide by it to determine how thick arrows should be
  # This way, get a relative sizing. I.e. the thicker ones were seen 3x more often than thinner ones
  minFreq = min(pregraphDF$frequency)
  first_width = ifelse(useEdgeSizing, pregraphDF$frequency[1]/minFreq, 1)
  if (first_width > 3) {
    first_width = 3
  }
  edgeDF <- data.frame(from=nodeDF$id[nodeDF$label == pregraphDF$word1[1]],
                       to=nodeDF$id[nodeDF$label == pregraphDF$word2[1]],
                       width=first_width,
                       arrows="to",
                       title=paste0("Edge frequency:", as.character(round(pregraphDF$frequency[1] * totalWords))),
                       stringsAsFactors = FALSE)
  for (rowNum in 2:nrow(pregraphDF)) {
    from_node = nodeDF$id[nodeDF$label == pregraphDF$word1[rowNum]]
    to_node = nodeDF$id[nodeDF$label == pregraphDF$word2[rowNum]]
    edge_width = ifelse(useEdgeSizing, pregraphDF$frequency[rowNum]/minFreq, 1)
    title = paste0("Edge frequency:", as.character(round(pregraphDF$frequency[rowNum] * totalWords)))
    edge_dir = "to"
    if (edge_width > 3) {
      edge_width <- 3
    }
    if (nrow(subset(edgeDF, from == from_node & to == to_node)) == 0) {
      edgeDF <- rbind(edgeDF, c(from_node, to_node, edge_width, edge_dir, title)) 
    }
  }
  return(list(nodeDF, edgeDF))
}

shinyServer(function(input, output, session) {
  
  rValues <- reactiveValues()
  
  loadInCorpus <- observe({
    if (input$chooseCorpus > 0) {
      isolate({
        rValues$corpusDF <- read.csv(paste0("Individual_Corpa/", input$corpusChooser), stringsAsFactors = FALSE)
        rValues$freqDF <- rValues$corpusDF
        minFreq <- min(rValues$corpusDF$word1_freq)
        choices <- unique(paste0(rValues$corpusDF$word1, " - ", rValues$corpusDF$word1_freq/minFreq))
        freqs <- unlist(lapply(choices, function(x) as.integer(strsplit(x, " - ")[[1]][2])))
        updateSelectizeInput(session, "centerWord", choices = choices[order(freqs, decreasing = TRUE)], server = TRUE)
      })
    }
  })
  
  # Remove stop words from the dropdown
  removeSWs <- observe({
    if(input$removeSimpleStopWords | input$removeGlasglowStopWords | input$removeLextecStopWords |
       !input$removeSimpleStopWords | !input$removeGlasglowStopWords | !input$removeLextecStopWords) {
      isolate ({
        tempCorpus <- rValues$corpusDF
        if (input$removeSimpleStopWords) {
          tempCorpus <- subset(tempCorpus, !(word1 %in% simpleStopWords))
        }
        if (input$removeGlasglowStopWords) {
          tempCorpus <- subset(tempCorpus, !(word1 %in% glasgowStopWords))
        }
        if (input$removeLextecStopWords) {
          tempCorpus <- subset(tempCorpus, !(word1 %in% lextecStopWords))
        }
        minFreq <- min(tempCorpus$word1_freq)
        choices <- unique(paste0(tempCorpus$word1, " - ", tempCorpus$word1_freq/minFreq))
        freqs <- unlist(lapply(choices, function(x) as.integer(strsplit(x, " - ")[[1]][2])))
        updateSelectizeInput(session, "centerWord", choices = choices[order(freqs, decreasing = TRUE)], server = TRUE)
      })
    }
  })
  
  # Only render the rest of the UI when the chooseCorpus button is pressed
  output$corpusSelections <- renderUI({
    if (input$chooseCorpus > 0) {
      isolate({
        box(width=12, title = "Graph Options:", collapsible = TRUE,
            fluidRow(shiny::column(12, selectizeInput("centerWord", "Central Word:", choices=NULL))),
            fluidRow(shiny::column(6, textInput(inputId='numNodes', label='# Of Nodes in Path', value="3"),
                                   checkboxInput("useNodeSize", label="Disable Node Sizing based on Frequency"),
                                   checkboxInput("removeSimpleStopWords", label="Remove Simple Stop Words"),
                                   checkboxInput("removeGlasglowStopWords", label="Remove Glasglow Stop Words")),
                     shiny::column(6, textInput(inputId='numEdges', label='# Of Edges per Node', value="3"),
                                   checkboxInput("useEdgeSize", label="Disable Edge Sizing based on Frequency"),
                                   checkboxInput("removeLextecStopWords", label="Remove Lextec Stop Words")
                     )
            ),
            fluidRow(actionButton("createGraph", "Create Graph"),
                     tags$style(type='text/css', "#createGraph { width:35%; display:block; margin: auto}"),
                     textOutput("errorMessages")
            )
        )
      })
    }
  })
  
  output$graphViz <- renderVisNetwork({
    if (length(input$createGraph) != 0) {
      if(input$createGraph > 0) {
        isolate({
          # Reverse the sign on these two because the box is for disabling.
          useNodeSizing <- ifelse(as.logical(input$useNodeSize), FALSE, TRUE)
          useEdgeSizing <- ifelse(as.logical(input$useEdgeSize), FALSE, TRUE)
          stopWordsToUse <- buildStopWords(input$removeSimpleStopWords, input$removeGlasglowStopWords, input$removeLextecStopWords)
          nodesAndEdges <- defineGraph(rValues$corpusDF, strsplit(input$centerWord, " - ")[[1]][1], as.numeric(input$numNodes),
                                       as.numeric(input$numEdges), useNodeSizing, useEdgeSizing, stopWordsToUse)
          visNetwork(nodesAndEdges[[1]], nodesAndEdges[[2]]) %>%
            visOptions(manipulation = TRUE)
        })
      }  
    }
  })
  
})