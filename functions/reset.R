resetTextMiningResults <- function() {
  currentTextminingResult <<- c()
  output$extracted_text <- renderUI({ HTML("") })
  output$extracted_terms <- renderDataTable(c())
  shinyjs::hide("textmining_tagger_results")
}

resetEnrichmentResults <- function() {
  resetGlobalVariables()
  resetResultBoxes()
  resetTables()
  resetPlots()
}

resetGlobalVariables <- function() {
  enrichmentResults[[currentType_Tool]] <<- data.frame()
  if (currentEnrichmentTool == "gProfiler")
    gprofilerResult <<- list()
  lapply(NETWORK_IDS, function(networkId){
    arenaEdgelist[[paste(currentEnrichmentType, currentEnrichmentTool,
                         networkId, sep = "_")]] <<- data.frame()
  })
}

resetResultBoxes <- function() {
  output[[paste(currentEnrichmentType, currentEnrichmentTool,
                "enrichment_parameters", sep = "_")]] <- renderText("")
  output[[paste(currentEnrichmentType, currentEnrichmentTool,
                "genesNotFound", sep = "_")]] <- renderText("")
  output[[paste(currentEnrichmentType, currentEnrichmentTool,
                "notConverted", sep = "_")]] <- renderText("")
  shinyjs::hide(paste(currentEnrichmentType, currentEnrichmentTool,
                      "conversionBoxes", sep = "_"))
  renderShinyDataTable(paste(currentEnrichmentType, currentEnrichmentTool,
                             "conversionTable", sep = "_"), data.frame())
}

resetTables <- function() {
  switch(
    currentEnrichmentType,
    "functional" = {
      output[[paste("functional", currentEnrichmentTool,
                    "table_all", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_gomf", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_gocc", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_gobp", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_kegg", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_reac", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_wp", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_tf", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_mirna", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_corum", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_hpa", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_hp", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_uniprot", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_pfam", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_interpro", sep = "_")]] <- renderDataTable(c())
      output[[paste("functional", currentEnrichmentTool,
                    "table_do", sep = "_")]] <- renderDataTable(c())
    },
    "literature" = {
      output$literature_aGoTool_table_pubmed <- renderDataTable(c())
    }
  )
}

resetPlots <- function() {
  resetNetworkPlots()
  resetNetworkTables()
  resetHeatmapPlots()
  resetHeatmapTables()
  resetBarchartPlot()
  resetBarchartTable()
  resetScatterPlot()
  resetScatterTable()
  resetManhattanPlot()
  resetManhattanTable()
}

resetNetworkPlots <- function() {
  lapply(NETWORK_IDS, function(networkId) {
    outputId <- paste(currentEnrichmentType, currentEnrichmentTool,
                      networkId, sep = "_")
    output[[outputId]] <- renderVisNetwork({})
    shinyjs::hide(outputId)
  })
}

resetNetworkTables <- function() {
  lapply(NETWORK_IDS, function(networkId) {
    output[[paste(currentEnrichmentType, currentEnrichmentTool,
                  networkId, "table", sep = "_")]] <-
      renderDataTable(c())
  })
  lapply(NETWORK_IDS, function(networkId) {
    output[[paste(currentEnrichmentType, currentEnrichmentTool,
                  networkId, "edgelist", sep = "_")]] <-
      renderDataTable(c())
  })
}

resetHeatmapPlots <- function() {
  lapply(HEATMAP_IDS, function(heatmapId) {
    output[[paste(currentEnrichmentType, currentEnrichmentTool,
                  heatmapId, sep = "_")]] <-
      renderPlotly(c())
  })
}

resetHeatmapTables <- function() {
  lapply(HEATMAP_IDS, function(heatmapId) {
    output[[paste(currentEnrichmentType, currentEnrichmentTool,
                  heatmapId, "table", sep = "_")]] <- renderDataTable(c())
  })
}

resetBarchartPlot <- function() {
  output[[paste(currentEnrichmentType, currentEnrichmentTool,
                "barchart", sep = "_")]] <- renderPlotly(c())
}

resetBarchartTable <- function() {
  output[[paste(currentEnrichmentType, currentEnrichmentTool,
                "barchart_table", sep = "_")]] <- DT::renderDataTable(data.frame())
}

resetScatterPlot <- function() {
  output[[paste(currentEnrichmentType, currentEnrichmentTool,
                "scatterPlot", sep = "_")]] <- renderPlotly(c())
}

resetScatterTable <- function() {
  output[[paste(currentEnrichmentType, currentEnrichmentTool,
                "scatterPlot_table", sep = "_")]] <- DT::renderDataTable(data.frame())
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
