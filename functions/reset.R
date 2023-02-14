resetTextMiningResults <- function() {
  currentTextminingResult <<- c()
  output$extracted_text <- renderUI({ HTML("") })
  output$extracted_terms <- renderDataTable(c())
  shinyjs::hide("textmining_tagger_results")
}

resetCombination <- function() {
  combinationResult <<- data.frame()
  hideTab(inputId = "toolTabsPanel", target = "Combination")
}

resetEnrichmentResults <- function(type, tool) {
  type_tool <- paste(type, tool, sep = "_")
  resetGlobalVariables(type_tool, tool)
  resetResultBoxes(type_tool)
  resetTables(type, tool)
  resetPlots(type_tool)
}

resetGlobalVariables <- function(type_tool, tool) {
  enrichmentResults[[type_tool]] <<- data.frame()
  if (tool == "gProfiler")
    gprofilerResult <<- list()
  lapply(NETWORK_IDS, function(networkId){
    arenaEdgelist[[paste(type_tool, networkId, sep = "_")]] <<- data.frame()
  })
}

resetResultBoxes <- function(type_tool) {
  output[[paste(type_tool, "enrichment_parameters", sep = "_")]] <- renderText("")
  output[[paste(type_tool, "genesNotFound", sep = "_")]] <- renderText("")
  output[[paste(type_tool, "notConverted", sep = "_")]] <- renderText("")
  shinyjs::hide(paste(type_tool, "conversionBoxes", sep = "_"))
  renderShinyDataTable(paste(type_tool, "conversionTable", sep = "_"), data.frame())
}

resetTables <- function(type, tool) {
  switch(
    type,
    "functional" = {
      lapply(TAB_NAMES, function(tabName) {
        output[[paste("functional", tool, "table", tabName, sep = "_")]] <-
          renderDataTable(c())
      })
    },
    "literature" = {
      output$literature_aGoTool_table_pubmed <- renderDataTable(c())
    }
  )
}

resetPlots <- function(type_tool) {
  resetNetworkPlots(type_tool)
  resetNetworkTables(type_tool)
  resetHeatmapPlots(type_tool)
  resetHeatmapTables(type_tool)
  resetBarchartPlot(type_tool)
  resetBarchartTable(type_tool)
  resetScatterPlot(type_tool)
  resetScatterTable(type_tool)
  resetManhattanPlot()
  resetManhattanTable()
}

resetNetworkPlots <- function(type_tool) {
  lapply(NETWORK_IDS, function(networkId) {
    outputId <- paste(type_tool, networkId, sep = "_")
    output[[outputId]] <- renderVisNetwork({})
    shinyjs::hide(outputId)
  })
}

resetNetworkTables <- function(type_tool) {
  lapply(NETWORK_IDS, function(networkId) {
    output[[paste(type_tool, networkId, "table", sep = "_")]] <-
      renderDataTable(c())
  })
  lapply(NETWORK_IDS, function(networkId) {
    output[[paste(type_tool, networkId, "edgelist", sep = "_")]] <-
      renderDataTable(c())
  })
}

resetHeatmapPlots <- function(type_tool) {
  lapply(HEATMAP_IDS, function(heatmapId) {
    output[[paste(type_tool, heatmapId, sep = "_")]] <-
      renderPlotly(c())
  })
}

resetHeatmapTables <- function(type_tool) {
  lapply(HEATMAP_IDS, function(heatmapId) {
    output[[paste(type_tool, heatmapId, "table", sep = "_")]] <- renderDataTable(c())
  })
}

resetBarchartPlot <- function(type_tool) {
  output[[paste(type_tool, "barchart", sep = "_")]] <- renderPlotly(c())
}

resetBarchartTable <- function(type_tool) {
  output[[paste(type_tool, "barchart_table", sep = "_")]] <- DT::renderDataTable(data.frame())
}

resetScatterPlot <- function(type_tool) {
  output[[paste(type_tool, "scatterPlot", sep = "_")]] <- renderPlotly(c())
}

resetScatterTable <- function(type_tool) {
  output[[paste(type_tool, "scatterPlot_table", sep = "_")]] <- DT::renderDataTable(data.frame())
}

resetManhattanPlot <- function() {
  output$manhattan <- renderPlotly(c())
}

resetManhattanTable <- function() {
  output$manhattan_table <- DT::renderDataTable(data.frame())
}

resetEdgelist_ViewAndArenaObjects <- function(enrichmentType, enrichmentTool, networkId) {
  arenaEdgelist[[paste(enrichmentType, enrichmentTool, networkId, sep = "_")]] <<- data.frame()
  output[[paste(enrichmentType, enrichmentTool, networkId, "edgelist", sep = "_")]] <- renderDataTable(c())
  output[[paste(enrichmentType, enrichmentTool, networkId, sep = "_")]] <- renderVisNetwork({})
  shinyjs::hide(paste(enrichmentType, enrichmentTool, networkId, sep = "_"))
}
