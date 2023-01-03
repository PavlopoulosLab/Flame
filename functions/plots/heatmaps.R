handleHeatmap <- function(enrichmentType, enrichmentTool, heatmapId) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Heatmap.</p>")
    if (existEnrichmentResults(enrichmentType, enrichmentTool)){
      handleHeatmapCallbackFunction <- switch(
        heatmapId,
        "heatmap1" = handleFunctionVsGeneHeatmap,
        "heatmap2" = handleFunctionVsFunctionHeatmap,
        "heatmap3" = handleGeneVsGeneHeatmap
      )
      type_Tool <- paste(enrichmentType, enrichmentTool, sep = "_")
      handleHeatmapCallbackFunction(enrichmentType, enrichmentTool, type_Tool)
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create heatmap with these inputs.")
  }, finally = {
    removeModal()
  })
}

handleFunctionVsGeneHeatmap <- function(enrichmentType, enrichmentTool, type_Tool) {
  source <- input[[paste(type_Tool, "heatmap1_sourceSelect", sep = "_")]]
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      enrichmentType, enrichmentTool,
      outputId = paste(type_Tool, "heatmap1", sep = "_"),
      sourceSelect = source,
      mode = input[[paste(type_Tool, "heatmap1_mode", sep = "_")]],
      slider = input[[paste(type_Tool, "heatmap1_slider", sep = "_")]])
    
    constructFunctionsVsGeneHeatmap(enrichmentType, 
                                    type_Tool, enrichmentFilteredData)
  }
}

constructFunctionsVsGeneHeatmap <- function(enrichmentType,
                                            type_Tool, enrichmentFilteredData) {
  heatmapTable <- separateRows(enrichmentFilteredData)
  heatmapTable$GeneExists <- 1
  
  uiTermKeyword <- str_to_title(UI_TERM_KEYWORD[[enrichmentType]])
  heatmap1_axis <- input[[paste(type_Tool, "heatmap1_axis", sep = "_")]]
  if (heatmap1_axis == paste0(uiTermKeyword, "-Genes")) {
    yAxisColumn <- "Term_ID_noLinks"
    xAxisColumn <- "Positive Hits"
  } else {
    yAxisColumn <- "Positive Hits"
    xAxisColumn <- "Term_ID_noLinks"
  }
  entriesCount <- length(unique(heatmapTable[[yAxisColumn]]))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap(type_Tool, "heatmap1", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste(
                  type_Tool, "heatmap1_sourceSelect", sep = "_")]]]],
                yAxisColumn, xAxisColumn,
                weightColumn = "GeneExists",
                height = height)
}

handleFunctionVsFunctionHeatmap <- function(enrichmentType, enrichmentTool, type_Tool) {
  source <- input[[paste(type_Tool, "heatmap2_sourceSelect", sep = "_")]]
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      enrichmentType, enrichmentTool,
      outputId = paste(type_Tool, "heatmap2", sep = "_"),
      sourceSelect = source,
      mode = input[[paste(type_Tool, "heatmap2_mode", sep = "_")]],
      slider = input[[paste(type_Tool, "heatmap2_slider", sep = "_")]])

    constructFunctionsVsFunctionHeatmap(enrichmentType, enrichmentTool,
                                        type_Tool, enrichmentFilteredData)
  }
}

constructFunctionsVsFunctionHeatmap <- function(enrichmentType, enrichmentTool,
                                                type_Tool, enrichmentFilteredData) {
  heatmapTable <- extractFunctionVsFunctionEdgelist(enrichmentType, enrichmentTool,
                                                    enrichmentFilteredData)
  
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap(type_Tool, "heatmap2", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste(
                  type_Tool, "heatmap2_sourceSelect", sep = "_")]]]],
                yAxisColumn = "Source Id",
                xAxisColumn = "Target Id",
                weightColumn = "Similarity Score %",
                height = height)
}

handleGeneVsGeneHeatmap <- function(enrichmentType, enrichmentTool, type_Tool) {
  source <- input[[paste(type_Tool, "heatmap3_sourceSelect", sep = "_")]]
  
  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      enrichmentType, enrichmentTool,
      outputId = paste(type_Tool, "heatmap3", sep = "_"),
      sourceSelect = source,
      mode = input[[paste(type_Tool, "heatmap3_mode", sep = "_")]],
      slider = input[[paste(type_Tool, "heatmap3_slider", sep = "_")]])
    
    constructGeneVsGeneHeatmap(type_Tool, enrichmentFilteredData)
  }
}

constructGeneVsGeneHeatmap <- function(type_Tool, enrichmentFilteredData) {
  heatmapTable <- extractGeneVsGeneEdgelist(enrichmentFilteredData)
  
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap(type_Tool, "heatmap3", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste(
                  type_Tool, "heatmap3_sourceSelect", sep = "_")]]]],
                yAxisColumn = "Source Name",
                xAxisColumn = "Target Name",
                weightColumn = "Common Functions",
                height = height)
}
