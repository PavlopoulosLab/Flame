handleHeatmap <- function(heatmapId) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Heatmap.</p>")
    if (existEnrichmentResults()){
      handleHeatmapCallbackFunction <- switch(
        heatmapId,
        "heatmap1" = handleFunctionVsGeneHeatmap,
        "heatmap2" = handleFunctionVsFunctionHeatmap,
        "heatmap3" = handleGeneVsGeneHeatmap
      )
      handleHeatmapCallbackFunction()
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create heatmap with these inputs.")
  }, finally = {
    removeModal()
  })
}

handleFunctionVsGeneHeatmap <- function() {
  source <- input$heatmap1_sourceSelect
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      id = "heatmap1", plotType = "heatmap",
      sourceSelect = source,
      mode = input$heatmap1_mode, slider = input$heatmap1_slider 
    )
    
    constructFunctionsVsGeneHeatmap(enrichmentFilteredData)
  }
}

constructFunctionsVsGeneHeatmap <- function(enrichmentFilteredData) {
  heatmapTable <- separateRows(enrichmentFilteredData)
  heatmapTable$GeneExists <- 1
  
  heatmap1_axis <- input$heatmap1_axis
  yColumn <- switch(
    heatmap1_axis,
    "Functions-Genes" = {
      yAxisColumn = "Term_ID_noLinks"
      xAxisColumn = "Positive Hits"
      "Term_ID_noLinks"
    },
    "Genes-Functions" = {
      yAxisColumn = "Positive Hits"
      xAxisColumn = "Term_ID_noLinks"
      "Positive Hits"
    }
  )
  entriesCount <- length(unique(heatmapTable[[yColumn]]))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap("heatmap1", heatmapTable,
                color = DATASOURCE_COLORS[[input$heatmap1_sourceSelect]],
                yAxisColumn, xAxisColumn,
                weightColumn = "GeneExists",
                height = height)
}

handleFunctionVsFunctionHeatmap <- function() {
  source <- input$heatmap2_sourceSelect
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      id = "heatmap2", plotType = "heatmap",
      sourceSelect = source,
      mode = input$heatmap2_mode, slider = input$heatmap2_slider 
    )

    constructFunctionsVsFunctionHeatmap(enrichmentFilteredData)
  }
}

constructFunctionsVsFunctionHeatmap <- function(enrichmentFilteredData) {
  heatmapTable <- extractFunctionVsFunctionEdgelist(enrichmentFilteredData)
  
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap("heatmap2", heatmapTable,
                color = DATASOURCE_COLORS[[input$heatmap2_sourceSelect]],
                yAxisColumn = "Source Id",
                xAxisColumn = "Target Id",
                weightColumn = "Similarity Score %",
                height = height)
}

handleGeneVsGeneHeatmap <- function() {
  source <- input$heatmap3_sourceSelect
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      id = "heatmap3", plotType = "heatmap",
      sourceSelect = source,
      mode = input$heatmap3_mode, slider = input$heatmap3_slider 
    )
    
    constructGeneVsGeneHeatmap(enrichmentFilteredData)
  }
}

constructGeneVsGeneHeatmap <- function(enrichmentFilteredData) {
  heatmapTable <- extractGeneVsGeneEdgelist(enrichmentFilteredData)
  
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap("heatmap3", heatmapTable,
                color = DATASOURCE_COLORS[[input$heatmap3_sourceSelect]],
                yAxisColumn = "Source Name",
                xAxisColumn = "Target Name",
                weightColumn = "Common Functions",
                height = height)
}
