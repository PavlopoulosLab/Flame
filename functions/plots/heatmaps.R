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
  
  uiTermKeyword <- stringr::str_to_title(UI_TERM_KEYWORD[[enrichmentType]])
  heatmap1_axis <- input[[paste(type_Tool, "heatmap1_axis", sep = "_")]]
  drawFormatColumun <- input[[paste(type_Tool, "heatmap1_drawFormat", sep = "_")]]
  if (heatmap1_axis == paste0(uiTermKeyword, "-Genes")) {
    yAxisColumn <- drawFormatColumun
    xAxisColumn <- "Positive Hits"
  } else {
    yAxisColumn <- "Positive Hits"
    xAxisColumn <- drawFormatColumun
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
  
  drawFormatColumun <- switch(
    input[[paste(type_Tool, "heatmap2_drawFormat", sep = "_")]],
    "Term_ID" = "Id",
    "Function" = "Name"
  )
  yAxisColumn <- paste0("Source ", drawFormatColumun)
  xAxisColumn <- paste0("Target ", drawFormatColumun)
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap(type_Tool, "heatmap2", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste(
                  type_Tool, "heatmap2_sourceSelect", sep = "_")]]]],
                yAxisColumn = yAxisColumn,
                xAxisColumn = xAxisColumn,
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
