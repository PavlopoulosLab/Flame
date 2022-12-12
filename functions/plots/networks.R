handleEnrichmentNetwork <- function(enrichmentType, enrichmentTool, networkId) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Network.</p>")
    if (existEnrichmentResults(enrichmentType, enrichmentTool)){
      handleNetworkCallbackFunction <- switch(
        networkId,
        "network1" = handleFunctionVsGeneNetwork,
        "network2" = handleFunctionVsFunctionNetwork,
        "network3" = handleGeneVsGeneNetwork
      )
      type_Tool <- paste(enrichmentType, enrichmentTool, sep = "_")
      handleNetworkCallbackFunction(enrichmentType, enrichmentTool, type_Tool)
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create network with the chosen settings.
                  Please adjust the data sources or filter values.")
  }, finally = {
    removeModal()
  })
}

handleFunctionVsGeneNetwork <- function(enrichmentType, enrichmentTool, type_Tool) {
  source <- input[[paste(type_Tool, "network1_sourceSelect", sep = "_")]]
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      enrichmentType, enrichmentTool,                                         
      outputId = paste(type_Tool, "network1", sep = "_"),
      sourceSelect = source,
      mode = input[[paste(type_Tool, "network1_mode", sep = "_")]],
      slider = input[[paste(type_Tool, "network1_slider", sep = "_")]])
    
    networkEdgelist <- separateRows(enrichmentFilteredData)
    networkEdgelist <- keepEdgelistColumns(networkEdgelist)
    arenaEdgelist[[paste(type_Tool, "network1", sep = "_")]] <<- networkEdgelist
    renderShinyDataTable(
      shinyOutputId = paste(type_Tool, "network1_edgelist", sep = "_"),
      networkEdgelist,
      caption = "Edgelist",
      fileName = paste(type_Tool, paste(source, collapse = "_"),
                       "network1_edgelist", sep = "_")
    )
    
    constructVisNetwork(
      type_Tool,
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

constructVisNetwork <- function(type_Tool, networkId, networkEdgelist,
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
    input[[paste(type_Tool, networkId, "layout", sep = "_")]], LAYOUT_CHOICES)]
  renderShinyVisNetwork(paste(type_Tool, networkId, sep = "_"), nodes, edges, layout)
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

handleFunctionVsFunctionNetwork <- function(enrichmentType, enrichmentTool, type_Tool) {
  source <- input[[paste(type_Tool, "network2_sourceSelect", sep = "_")]]
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      enrichmentType, enrichmentTool,
      outputId = paste(type_Tool, "network2", sep = "_"),
      sourceSelect = source,
      mode = input[[paste(type_Tool, "network2_mode", sep = "_")]],
      slider = input[[paste(type_Tool, "network2_slider", sep = "_")]])
    
    networkEdgelist <- 
      extractFunctionVsFunctionEdgelist(
        enrichmentType, enrichmentTool, enrichmentFilteredData,
        input[[paste(type_Tool, "network2_thresholdSlider", sep = "_")]],
        simplifyForNetwork = T
      )
    if (existEnoughEdges(enrichmentType, enrichmentTool, "network2", networkEdgelist)) {
      arenaEdgelist[[paste(type_Tool, "network2", sep = "_")]] <<- networkEdgelist
      renderShinyDataTable(
        shinyOutputId = paste(type_Tool, "network2_edgelist", sep = "_"),
        networkEdgelist,
        caption = "Edgelist",
        fileName = paste(type_Tool, paste(source, collapse = "_"),
                         "network2_edgelist", sep = "_")
      )
      
      constructVisNetwork(
        type_Tool,
        networkId = "network2",
        networkEdgelist,
        sourceColumnName = "Source Id",
        targetColumnName = "Target Id",
        weightColumn = "Similarity Score %"
      )
    }
  }
}

existEnoughEdges <- function(enrichmentType, enrichmentTool,
                             networkId, networkEdgelist) {
  exist <- F
  if (nrow(networkEdgelist) > 0){
    exist <- T
  } else {
    renderWarning("The current chosen filters cannot produce enough edges to form a network.
                  Please adjust the data sources or filter values.")
    resetEdgelist_ViewAndArenaObjects(enrichmentType, enrichmentTool, networkId)
  }
  return(exist)
}

handleGeneVsGeneNetwork <- function(enrichmentType, enrichmentTool, type_Tool) {
  source <- input[[paste(type_Tool, "network3_sourceSelect", sep = "_")]]
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      enrichmentType, enrichmentTool,
      outputId = paste(type_Tool, "network3", sep = "_"),
      sourceSelect = source,
      mode = input[[paste(type_Tool, "network3_mode", sep = "_")]],
      slider = input[[paste(type_Tool, "network3_slider", sep = "_")]])
    
    networkEdgelist <- 
      extractGeneVsGeneEdgelist(
        enrichmentFilteredData,
        input[[paste(type_Tool, "network3_thresholdSlider", sep = "_")]],
        simplifyForNetwork = T
      )
    if (existEnoughEdges(enrichmentType, enrichmentTool, "network3", networkEdgelist)) {
      arenaEdgelist[[paste(type_Tool, "network3", sep = "_")]] <<- networkEdgelist
      renderShinyDataTable(
        shinyOutputId = paste(type_Tool, "network3_edgelist", sep = "_"),
        networkEdgelist,
        caption = "Edgelist",
        fileName = paste(type_Tool, paste(source, collapse = "_"),
                         "network3_edgelist", sep = "_")
      )
      
      constructVisNetwork(
        type_Tool,
        networkId = "network3",
        networkEdgelist,
        sourceColumnName = "Source Name",
        targetColumnName = "Target Name",
        weightColumn = "Common Functions"
      )
    }
  }
}
