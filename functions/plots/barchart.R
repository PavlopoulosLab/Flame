handleBarchart <- function(enrichmentType) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Barchart.</p>")
    if (existEnrichmentResults(enrichmentType)) {
      sourceSelect <- input[[paste0(enrichmentType, "_barchart_sourceSelect")]]
      mode <- input[[paste0(enrichmentType, "_barchart_mode")]]
      
      if (isSourceNotNull(sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(enrichmentType,
          id = paste0(enrichmentType, "_barchart"), plotType = "barchart",
          sourceSelect = sourceSelect,
          mode = mode, slider = input[[paste0(enrichmentType, "_barchart_slider")]])
        constructBarchart(enrichmentType, enrichmentFilteredData, mode)
      }
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create barchart with these inputs.")
  }, finally = {
    removeModal()
  })
}

constructBarchart <- function(enrichmentType, enrichmentFilteredData, mode) {
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
  renderBarchart(paste0(enrichmentType, "_barchart"),
                 enrichmentFilteredData, column, height)
}
