handleBarchart <- function(enrichmentType, enrichmentTool) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Barchart.</p>")
    if (existEnrichmentResults(enrichmentType, enrichmentTool)) {
      type_Tool <- paste(enrichmentType, enrichmentTool, sep = "_")
      sourceSelect <- input[[paste(type_Tool, "barchart_sourceSelect", sep = "_")]]
      mode <- input[[paste(type_Tool, "barchart_mode", sep = "_")]]
      
      if (isSourceNotNull(sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(
          enrichmentType, enrichmentTool,
          outputId = paste(type_Tool, "barchart", sep = "_"),
          sourceSelect = sourceSelect,
          mode = mode,
          slider = input[[paste(type_Tool, "barchart_slider", sep = "_")]])
        constructBarchart(type_Tool, enrichmentFilteredData, mode)
      }
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create barchart with these inputs.")
  }, finally = {
    removeModal()
  })
}

constructBarchart <- function(type_Tool, enrichmentFilteredData, mode) {
  column <- switch(
    mode,
    "Enrichment Score" = "Enrichment Score %",
    "-log10Pvalue"
  )
  enrichmentFilteredData$Term_ID <-
    factor(
      enrichmentFilteredData$Term_ID,
      levels = unique(enrichmentFilteredData$Term_ID)[order(enrichmentFilteredData[[column]],
                                                            decreasing = F)])
  height <- calculatePlotHeight(nrow(enrichmentFilteredData))
  renderBarchart(paste(type_Tool, "barchart", sep = "_"),
                 enrichmentFilteredData, column, height)
}
