resetEnrichmentResults <- function() {
  resetGlobalVariables()
  resetResultBoxes()
  resetTables()
  resetPlots()
}

resetGlobalVariables <- function() {
  switch(
    currentEnrichmentType,
    "functional" = {
      gprofilerResult <<- list()
      functionalEnrichmentResult <<- data.frame()
      lapply(NETWORK_IDS, function(networkId){
        arenaEdgelist[[paste0("functional_", networkId)]] <<- data.frame()
      })
    },
    "literature" = {
      literatureEnrichmentResult <<- data.frame()
      lapply(NETWORK_IDS, function(networkId){
        arenaEdgelist[[paste0("literature_", networkId)]] <<- data.frame()
      })
    }
  )
}

resetResultBoxes <- function() {
  output[[paste0(currentEnrichmentType, "_enrichment_parameters")]] <-
    renderText("")
  output[[paste0(currentEnrichmentType, "_genesNotFound")]] <-
    renderText("")
  output[[paste0(currentEnrichmentType, "_notConverted")]] <-
    renderText("")
  shinyjs::hide(paste0(currentEnrichmentType, "_conversionBoxes"))
  renderShinyDataTable(paste0(currentEnrichmentType, "_conversionTable"),
                       data.frame())
}

resetTables <- function() {
  switch(
    currentEnrichmentType,
    "functional" = {
      output$functional_table_all <- renderDataTable(c())
      output$functional_table_gomf <- renderDataTable(c())
      output$functional_table_gocc <- renderDataTable(c())
      output$functional_table_gobp <- renderDataTable(c())
      output$functional_table_kegg <- renderDataTable(c())
      output$functional_table_reac <- renderDataTable(c())
      output$functional_table_wp <- renderDataTable(c())
      output$functional_table_tf <- renderDataTable(c())
      output$functional_table_mirna <- renderDataTable(c())
      output$functional_table_corum <- renderDataTable(c())
      output$functional_table_hpa <- renderDataTable(c())
      output$functional_table_hp <- renderDataTable(c())
      output$functional_table_uniprot <- renderDataTable(c())
      output$functional_table_pfam <- renderDataTable(c())
      output$functional_table_interpro <- renderDataTable(c())
      output$functional_table_do <- renderDataTable(c())
    },
    "literature" = {
      output$literature_table_pubmed <- renderDataTable(c())
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
    outputId <- paste(currentEnrichmentType, networkId, sep = "_")
    output[[outputId]] <- renderVisNetwork({})
    shinyjs::hide(outputId)
  })
}

resetNetworkTables <- function() {
  lapply(NETWORK_IDS, function(networkId) {
    output[[paste(currentEnrichmentType, networkId, "table", sep = "_")]] <-
      renderDataTable(c())
  })
  lapply(NETWORK_IDS, function(networkId) {
    output[[paste(currentEnrichmentType, networkId, "edgelist", sep = "_")]] <-
      renderDataTable(c())
  })
}

resetHeatmapPlots <- function() {
  lapply(HEATMAP_IDS, function(heatmapId) {
    output[[paste(currentEnrichmentType, heatmapId, sep = "_")]] <-
      renderPlotly(c())
  })
}

resetHeatmapTables <- function() {
  lapply(HEATMAP_IDS, function(heatmapId) {
    output[[paste(currentEnrichmentType, heatmapId, "table", sep = "_")]] <-
      renderDataTable(c())
  })
}

resetBarchartPlot <- function() {
  output[[paste0(currentEnrichmentType, "_barchart")]] <- renderPlotly(c())
}

resetBarchartTable <- function() {
  output[[paste0(currentEnrichmentType, "_barchart_table")]] <-
    DT::renderDataTable(data.frame())
}

resetScatterPlot <- function() {
  output[[paste0(currentEnrichmentType, "_scatterPlot")]] <- renderPlotly(c())
}

resetScatterTable <- function() {
  output[[paste0(currentEnrichmentType, "_scatterPlot_table")]] <-
    DT::renderDataTable(data.frame())
}

resetManhattanPlot <- function() {
  output$manhattan <- renderPlotly(c())
}

resetManhattanTable <- function() {
  output$manhattan_table <- DT::renderDataTable(data.frame())
}

resetEdgelist_ViewAndArenaObjects <- function(enrichmentType, networkId) {
  arenaEdgelist[[paste(enrichmentType, networkId, sep = "_")]] <<- data.frame()
  output[[paste(enrichmentType, networkId, "edgelist", sep = "_")]] <- renderDataTable(c())
  output[[paste(enrichmentType, networkId, sep = "_")]] <- renderVisNetwork({})
  shinyjs::hide(paste(enrichmentType, networkId, sep = "_"))
}
