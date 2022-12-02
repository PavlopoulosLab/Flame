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
                                                         input$network2_thresholdSlider)
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

calculateEdgeTotalGenes <- function(totalGenesEdgelist) {
  totalGenesEdgelistCopy <- totalGenesEdgelist
  colnames(totalGenesEdgelistCopy) <- c("TermsCopy", "HitsCopy")
  totalGenesEdgelist <- merge(totalGenesEdgelist , totalGenesEdgelistCopy)
  totalGenesEdgelist$`Positive Hits` <-
    paste(totalGenesEdgelist$`Positive Hits`,
          totalGenesEdgelist$HitsCopy,
          sep = ", ")
  totalGenesEdgelist$HitsCopy <- NULL
  totalGenesEdgelist <-
    tidyr::separate_rows(totalGenesEdgelist, `Positive Hits`, sep = ", ")
  totalGenesEdgelist <- distinct(totalGenesEdgelist)
  totalGenesEdgelist$`Positive Hits` <- NULL
  totalGenesEdgelist <- data.table::setDT(
    totalGenesEdgelist)[, list(`Total Genes` = .N), names(totalGenesEdgelist)]
  colnames(totalGenesEdgelist)[1:2] <- c("Term_ID_noLinks.x", "Term_ID_noLinks.y")
  return(totalGenesEdgelist)
}

calculateEdgeCommonGenes <- function(commonGenesEdgelist) {
  commonGenesEdgelist <-
    tidyr::separate_rows(commonGenesEdgelist, `Positive Hits`, sep = ", ")
  commonGenesEdgelist <- merge(
    commonGenesEdgelist, commonGenesEdgelist,
    by.x = "Positive Hits", by.y = "Positive Hits"
  )
  commonGenesEdgelist$`Positive Hits` <- NULL
  # Create common genes counts column
  commonGenesEdgelist <- data.table::setDT(
    commonGenesEdgelist)[, list(`Common Genes` = .N), names(commonGenesEdgelist)]
  return(commonGenesEdgelist)
}

calculateSimilarityScore <- function(functionsEdgelist) {
  functionsEdgelist$`Similarity Score %` <-
    functionsEdgelist$`Common Genes` / functionsEdgelist$`Total Genes` * 100
  functionsEdgelist$`Similarity Score %` <-
    format(round(functionsEdgelist$`Similarity Score %`, 2))
  return(functionsEdgelist)
}

removeDuplicateSelfAndOppositeEdges <- function(networkEdgelist, weightColumn) {
  graph <- igraph::graph_from_data_frame(networkEdgelist, directed = F)
  igraph::E(graph)$weight <- networkEdgelist[[weightColumn]]
  graph <- igraph::simplify(
    graph,
    remove.multiple = T,
    remove.loops = T,
    edge.attr.comb = "first"
  )
  networkEdgelist <- appendEdgelistColumns(graph, weightColumn)
  networkEdgelist[[weightColumn]] <-
    as.numeric(networkEdgelist[[weightColumn]])
  return(networkEdgelist)
}

appendEdgelistColumns <- function(graph, weightColumn) {
  if (weightColumn == "Similarity Score %") {
    graphEdgelist <- as.data.frame(
      cbind(
        igraph::get.edgelist(graph),
        igraph::E(graph)$`Common Genes`,
        igraph::E(graph)$`Total Genes`,
        igraph::E(graph)$weight
      )
    )
    colnames(graphEdgelist) <-
      c("Source Node", "Target Node",
        "Common Genes", "Total Genes", "Similarity Score %")
  } else if (weightColumn == "Common Functions") {
    graphEdgelist <- as.data.frame(
      cbind(
        igraph::get.edgelist(graph),
        igraph::E(graph)$weight
      )
    )
    colnames(graphEdgelist) <-
      c("Source Node", "Target Node", "Common Functions")
  }
  return(graphEdgelist)
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

filterBySliderThreshold <- function(edgelist, weightColumn, thresholdSlider) {
  edgelist <-
    edgelist[
      edgelist[[weightColumn]] >= thresholdSlider, , drop = F
    ]
  return(edgelist)
}

appendSourceDatabasesAndIds <- function(functionsEdgelist) {
  gprofilerNetworkData <- gprofilerTransformedResult[, c(
    "Source", "Term_ID_noLinks", "Function")]
  functionsEdgelist <- merge(functionsEdgelist, gprofilerNetworkData,
                             by.x = "Source Node", by.y = "Term_ID_noLinks")
  functionsEdgelist <- merge(functionsEdgelist, gprofilerNetworkData,
                             by.x = "Target Node", by.y = "Term_ID_noLinks")
  colnames(functionsEdgelist) <-
    c("Target Id", "Source Id", "Common Genes", "Total Genes",
      "Similarity Score %", "Source Database", "Source Name",
      "Target Database", "Target Name")
  functionsEdgelist <-
    functionsEdgelist[, c(
      "Source Database", "Source Id", "Source Name",
      "Target Database", "Target Id", "Target Name",
      "Common Genes", "Total Genes", "Similarity Score %"
    )]
  return(functionsEdgelist)
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
                                                 input$network3_thresholdSlider)
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
