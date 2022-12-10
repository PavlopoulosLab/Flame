handleHeatmap <- function(enrichmentType, heatmapId) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Heatmap.</p>")
    if (existEnrichmentResults(enrichmentType)){
      handleHeatmapCallbackFunction <- switch(
        heatmapId,
        "heatmap1" = handleFunctionVsGeneHeatmap,
        "heatmap2" = handleFunctionVsFunctionHeatmap,
        "heatmap3" = handleGeneVsGeneHeatmap
      )
      handleHeatmapCallbackFunction(enrichmentType)
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create heatmap with these inputs.")
  }, finally = {
    removeModal()
  })
}

handleFunctionVsGeneHeatmap <- function(enrichmentType) {
  source <- input[[paste0(enrichmentType, "_heatmap1_sourceSelect")]]
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(enrichmentType,
      id = paste0(enrichmentType, "_heatmap1"), plotType = "heatmap",
      sourceSelect = source,
      mode = input[[paste0(enrichmentType, "_heatmap1_mode")]],
      slider = input[[paste0(enrichmentType, "_heatmap1_slider")]])
    
    constructFunctionsVsGeneHeatmap(enrichmentType, enrichmentFilteredData)
  }
}

constructFunctionsVsGeneHeatmap <- function(enrichmentType, enrichmentFilteredData) {
  heatmapTable <- separateRows(enrichmentFilteredData)
  heatmapTable$GeneExists <- 1
  
  heatmap1_axis <- input[[paste0(enrichmentType, "_heatmap1_axis")]]
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
  renderHeatmap(enrichmentType, "heatmap1", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste0(enrichmentType,
                                                         "_heatmap1_sourceSelect")]]]],
                yAxisColumn, xAxisColumn,
                weightColumn = "GeneExists",
                height = height)
}

handleFunctionVsFunctionHeatmap <- function(enrichmentType) {
  source <- input[[paste0(enrichmentType, "_heatmap2_sourceSelect")]]
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(enrichmentType,
      id = paste0(enrichmentType, "_heatmap2"), plotType = "heatmap",
      sourceSelect = source,
      mode = input[[paste0(enrichmentType, "_heatmap2_mode")]],
      slider = input[[paste0(enrichmentType, "_heatmap2_slider")]])

    constructFunctionsVsFunctionHeatmap(enrichmentType, enrichmentFilteredData)
  }
}

constructFunctionsVsFunctionHeatmap <- function(enrichmentType, enrichmentFilteredData) {
  heatmapTable <- extractFunctionVsFunctionEdgelist(enrichmentType, enrichmentFilteredData)
  
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap(enrichmentType, "heatmap2", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste0(enrichmentType,
                                                         "_heatmap2_sourceSelect")]]]],
                yAxisColumn = "Source Id",
                xAxisColumn = "Target Id",
                weightColumn = "Similarity Score %",
                height = height)
}

handleGeneVsGeneHeatmap <- function(enrichmentType) {
  source <- input[[paste0(enrichmentType, "_heatmap3_sourceSelect")]]
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(enrichmentType,
      id = paste0(enrichmentType, "_heatmap3"), plotType = "heatmap",
      sourceSelect = source,
      mode = input[[paste0(enrichmentType, "_heatmap3_mode")]],
      slider = input[[paste0(enrichmentType, "_heatmap3_slider")]])
    
    constructGeneVsGeneHeatmap(enrichmentType, enrichmentFilteredData)
  }
}

constructGeneVsGeneHeatmap <- function(enrichmentType, enrichmentFilteredData) {
  heatmapTable <- extractGeneVsGeneEdgelist(enrichmentFilteredData)
  
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap(enrichmentType, "heatmap3", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste0(enrichmentType, "_heatmap3_sourceSelect")]]]],
                yAxisColumn = "Source Name",
                xAxisColumn = "Target Name",
                weightColumn = "Common Functions",
                height = height)
}
