handleEnrichmentNetwork <- function(networkId) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Network.</p>")
    if (existEnrichmentResults()){
      handleNetworkCallbackFunction <- switch(
        networkId,
        "network1" = handleFunctionVsGeneNetwork,
        "network2" = handleFunctionVsFunctionNetwork,
        "network3" = handleGeneVsGeneNetwork
      )
      handleNetworkCallbackFunction()
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create network with these inputs.")
  }, finally = {
    removeModal()
  })
}

handleFunctionVsGeneNetwork <- function() {
  source <- input$network1_sourceSelect
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      id = "network1", plotType = "network",
      sourceSelect = source,
      mode = input$network1_mode, slider = input$network1_slider 
    )
    
    networkEdgelist <- separateRows(enrichmentFilteredData)
    networkEdgelist <- keepEdgelistColumns(networkEdgelist)
    arenaEdgelist$network1 <<- networkEdgelist
    renderShinyDataTable(
      shinyOutputId = "network1_edgelist",
      networkEdgelist,
      caption = "Edgelist",
      fileName = paste0(source, "_edgelist")
    )
    
    constructVisNetwork(
      networkId = "network1",
      networkEdgelist,
      sourceColumnName = "Source Id",
      targetColumnName = "Target Gene",
      weightColumn = NULL
    )
  }
}

keepEdgelistColumns <- function(gprofilerNetworkData) {
  gprofilerNetworkData <- gprofilerNetworkData[, c(
    "Source", "Term_ID_noLinks", "Function", "Positive Hits")]
  colnames(gprofilerNetworkData) <-
    c("Source Database", "Source Id", "Source Name", "Target Gene")
  return(gprofilerNetworkData)
}

constructVisNetwork <- function(networkId, networkEdgelist,
                                sourceColumnName, targetColumnName,
                                weightColumn) {
  graph <- createGraph(networkEdgelist, sourceColumnName,
                       targetColumnName, weightColumn)
  data <- toVisNetworkData(graph)
  nodes <- data$nodes
  row.names(nodes) <- NULL
  nodes$font.size <- 24
  nodes <- appendGroupsAndTitles(networkId, nodes, networkEdgelist)
  edges <- data$edges
  edges <- appendWidth(edges)
  layout <- names(LAYOUT_CHOICES)[match(
    input$network1_layout, LAYOUT_CHOICES)]
  renderShinyVisNetwork(networkId, nodes, edges, layout)
}

createGraph <- function(networkEdgelist, sourceColumnName,
                        targetColumnName, weightColumn) {
  graph <- igraph::graph_from_edgelist(
    as.matrix(networkEdgelist[, c(sourceColumnName, targetColumnName)]),
    directed = FALSE
  )
  if (!is.null(weightColumn))
    E(graph)$weight <- networkEdgelist[[weightColumn]]
  else
    E(graph)$weight <- 1
  return(graph)
}

appendGroupsAndTitles <- function(networkId, nodes, networkEdgelist) {
  nodes$group <-
    networkEdgelist$`Source Database`[match(
      nodes$label, networkEdgelist$`Source Id`)]
  nodes$title <- networkEdgelist$`Source Name`[match(
    nodes$label, networkEdgelist$`Source Id`)]
  if (networkId == "network1") {
    nodes$group[is.na(nodes$group)] <- "Gene"
  } else if (networkId == "network2") {
    nodes$group[is.na(nodes$group)] <-
      networkEdgelist$`Target Database`[match(
        nodes$label[is.na(nodes$group)], networkEdgelist$`Target Id`)]
    nodes$title[is.na(nodes$title)] <- networkEdgelist$`Target Name`[match(
      nodes$label[is.na(nodes$title)], networkEdgelist$`Target Id`)]
  } else if (networkId == "network3") {
    nodes$group <- "Gene"
  }
  return(nodes)
}

appendWidth <- function(edges) {
  edges$width <- mapRange(edges$weight,
                          newMin = EDGE_WIDTH_MIN, newMax = EDGE_WIDTH_MAX)
  edges$title <- edges$weight
  return(edges)
}

handleFunctionVsFunctionNetwork <- function() {
  source <- input$network2_sourceSelect
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      id = "network2", plotType = "network",
      sourceSelect = source,
      mode = input$network2_mode, slider = input$network2_slider 
    )
    
    networkEdgelist <- extractFunctionVsFunctionEdgelist(enrichmentFilteredData,
                                                         input$network2_thresholdSlider,
                                                         simplifyForNetwork = T)
    if (existEnoughEdges("network2", networkEdgelist)) {
      arenaEdgelist$network2 <<- networkEdgelist
      renderShinyDataTable(
        shinyOutputId = "network2_edgelist",
        networkEdgelist,
        caption = "Edgelist",
        fileName = paste0(source, "_edgelist")
      )
      
      constructVisNetwork(
        networkId = "network2",
        networkEdgelist,
        sourceColumnName = "Source Id",
        targetColumnName = "Target Id",
        weightColumn = "Similarity Score %"
      )
    }
  }
}

existEnoughEdges <- function(networkId, networkEdgelist) {
  exist <- F
  if (nrow(networkEdgelist) > 0){
    exist <- T
  } else {
    renderWarning("Cannot form edgelist with these filters.")
    resetEdgelist_ViewAndArenaObjects(networkId)
  }
  return(exist)
}

handleGeneVsGeneNetwork <- function() {
  source <- input$network3_sourceSelect
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      id = "network3", plotType = "network",
      sourceSelect = source,
      mode = input$network3_mode, slider = input$network3_slider 
    )
    
    networkEdgelist <- extractGeneVsGeneEdgelist(enrichmentFilteredData,
                                                 input$network3_thresholdSlider,
                                                 simplifyForNetwork = T)
    if (existEnoughEdges("network3", networkEdgelist)) {
      arenaEdgelist$network3 <<- networkEdgelist
      renderShinyDataTable(
        shinyOutputId = "network3_edgelist",
        networkEdgelist,
        caption = "Edgelist",
        fileName = paste0(source, "_edgelist")
      )
      
      constructVisNetwork(
        networkId = "network3",
        networkEdgelist,
        sourceColumnName = "Source Name",
        targetColumnName = "Target Name",
        weightColumn = "Common Functions"
      )
    }
  }
}
