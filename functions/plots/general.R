handleDatasourcePicker <- function(enrichmentType, toolName, componentId) {
  tryCatch({
    type_Tool <- paste(enrichmentType, toolName, sep = "_")
    datasources <- input[[paste(type_Tool, componentId, "sourceSelect", sep = "_")]]
    maxSliderValue <- calculateMaxSliderValue(enrichmentType, toolName, datasources)
    
    sliderId <- paste(type_Tool, componentId, "slider", sep = "_")
    updateShinySliderInput(shinyOutputId = sliderId,
                           min = 1, maxSliderValue)
    if (componentId == "network3") {
      updateShinySliderInput(
        shinyOutputId = paste(type_Tool, "network3_thresholdSlider", sep = "_"),
        min = 1, maxSliderValue,
        value = round(maxSliderValue / 10)
      )
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not update slider filter values properly.")
  })
}

calculateMaxSliderValue <- function(enrichmentType, toolName, datasources) {
  enrichmentResult <- getGlobalEnrichmentResult(enrichmentType, toolName)
  maxSliderValue <- nrow(
    subset(
      enrichmentResult,
      Source %in% datasources
    )
  )
  return(maxSliderValue)
}

existEnrichmentResults <- function(enrichmentType, enrichmentTool) {
  enrichmentResult <- getGlobalEnrichmentResult(enrichmentType, enrichmentTool)
  exist <- F
  if (nrow(enrichmentResult) > 0){
    exist <- T
  } else
    renderWarning(paste0(
      "Execute ", enrichmentType, " enrichment analysis 
      with ", enrichmentTool, " first."
    ))
  return(exist)
}

isSourceNotNull <- function(sourceSelect) {
  isNotNull <- F
  if (!is.null(sourceSelect)){
    isNotNull <- T
  } else
    renderWarning("Select at least one datasource.")
  return(isNotNull)
}

filterAndPrintTable <- function(enrichmentType, enrichmentTool,
                                outputId, sourceSelect, mode, slider) {
  enrichmentFilteredData <-
    filterTopData(enrichmentType, enrichmentTool, sourceSelect, mode, slider)
  renderEnrichmentTable(
    shinyOutputId = paste0(outputId, "_table"),
    enrichmentFilteredData,
    caption = "Enrichment Results",
    fileName = paste(outputId, paste(sourceSelect, collapse = "_"), sep = "_"),
    mode = "Positive Hits",
    hiddenColumns = c(0, 11, 12),
    expandableColumn = 11
  )
  return(enrichmentFilteredData)
}

filterTopData <- function(enrichmentType, enrichmentTool,
                          sourceSelect, mode, slider) {
  enrichmentResult <- getGlobalEnrichmentResult(enrichmentType, enrichmentTool)
  filteredData <- subset(
    enrichmentResult,
    Source %in% sourceSelect
  )
  if (mode == "Enrichment Score") {
    filteredData <-
      filteredData[order(-filteredData$`Enrichment Score %`), ]
  } # else already sorted by descending -log10Pvalue
  filteredData <- head(filteredData, slider)
  filteredData$`Positive Hits` <-
    gsub(",", ", ", filteredData$`Positive Hits`)
  return(filteredData)
}

separateRows <- function(enrichmentData) {
  enrichmentData <- enrichmentData[, c(
    "Source", "Term_ID_noLinks", "Function", "Positive Hits",
    "Enrichment Score %", "-log10Pvalue", "Intersection Size")]
  enrichmentData <-
    tidyr::separate_rows(enrichmentData, `Positive Hits`, sep = ", ")
  return(enrichmentData)
}

calculatePlotHeight <- function(entriesCount) {
  height <- entriesCount * SINGLE_BAR_HEIGHT_PX + MIN_BAR_HEIGHT_PX
  return(height)
}

extractFunctionVsFunctionEdgelist <- function(enrichmentType, enrichmentTool,
                                              enrichmentData,
                                              thresholdSlider = NULL,
                                              simplifyForNetwork = F) {
  functionsEdgelist <- enrichmentData[, c("Term_ID_noLinks", "Positive Hits")]
  totalGenesEdgelist <- calculateEdgeTotalGenes(functionsEdgelist)
  commonGenesEdgelist <- calculateEdgeCommonGenes(functionsEdgelist)
  functionsEdgelist <- merge(
    commonGenesEdgelist, totalGenesEdgelist,
    by = c("Term_ID_noLinks.x", "Term_ID_noLinks.y")
  )
  functionsEdgelist <- calculateSimilarityScore(functionsEdgelist)
  weightColumn <- "Similarity Score %"
  if (simplifyForNetwork) {
    functionsEdgelist <-
      removeDuplicateSelfAndOppositeEdges(functionsEdgelist, weightColumn)
  } else {
    functionsEdgelist <- tuneForHeatmap(functionsEdgelist)
  }
  if (!is.null(thresholdSlider)) {
    functionsEdgelist <- filterBySliderThreshold(functionsEdgelist,
                                                 weightColumn,
                                                 thresholdSlider)
  }
  functionsEdgelist <- appendSourceDatabasesAndIds(enrichmentType, enrichmentTool, functionsEdgelist)
  functionsEdgelist <- functionsEdgelist[order(-functionsEdgelist$`Similarity Score %`), ]
  return(functionsEdgelist)
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
  totalGenesEdgelist <- dplyr::distinct(totalGenesEdgelist)
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

tuneForHeatmap <- function(functionsEdgelist) {
  functionsEdgelist$`Similarity Score %` <- as.numeric(functionsEdgelist$`Similarity Score %`)
  colnames(functionsEdgelist) <-
    c("Source Node", "Target Node",
      "Common Genes", "Total Genes", "Similarity Score %")
  return(functionsEdgelist)
}

filterBySliderThreshold <- function(edgelist, weightColumn, thresholdSlider) {
  edgelist <-
    edgelist[
      edgelist[[weightColumn]] >= thresholdSlider, , drop = F
    ]
  return(edgelist)
}

appendSourceDatabasesAndIds <- function(enrichmentType, enrichmentTool, functionsEdgelist) {
  enrichedNetworkData <- getGlobalEnrichmentResult(enrichmentType, enrichmentTool)
  enrichedNetworkData <- enrichedNetworkData[, c(
    "Source", "Term_ID_noLinks", "Function")]
  functionsEdgelist <- merge(functionsEdgelist, enrichedNetworkData,
                             by.x = "Source Node", by.y = "Term_ID_noLinks")
  functionsEdgelist <- merge(functionsEdgelist, enrichedNetworkData,
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

extractGeneVsGeneEdgelist <- function(enrichmentData, thresholdSlider = NULL,
                                      simplifyForNetwork = F) {
  genesEdgelist <- enrichmentData[, c("Term_ID_noLinks", "Positive Hits")]
  genesEdgelist <-
    tidyr::separate_rows(genesEdgelist, `Positive Hits`, sep = ", ")
  genesEdgelist <- merge(genesEdgelist, genesEdgelist,
                         by.x = "Term_ID_noLinks", by.y = "Term_ID_noLinks")
  genesEdgelist$`Term_ID_noLinks` <- NULL
  # Create common functions counts column
  genesEdgelist <- data.table::setDT(
    genesEdgelist)[, list(`Common Functions` = .N), names(genesEdgelist)]
  weightColumn <- "Common Functions"
  if (simplifyForNetwork) {
    genesEdgelist <-
      removeDuplicateSelfAndOppositeEdges(genesEdgelist, weightColumn)
  }
  if (!is.null(thresholdSlider)) {
    genesEdgelist <- filterBySliderThreshold(genesEdgelist,
                                             weightColumn,
                                             thresholdSlider)
  }
  colnames(genesEdgelist) <- c("Source Name", "Target Name", "Common Functions")
  return(genesEdgelist)
}
