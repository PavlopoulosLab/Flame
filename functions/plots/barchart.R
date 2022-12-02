handleBarchart <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Barchart.</p>")
    if (existEnrichmentResults()){
      barchart_sourceSelect <- input$barchart_sourceSelect
      barchart_mode <- input$barchart_mode
      
      if (isSourceNotNull(barchart_sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(
          id = "barchart", plotType = "barchart",
          sourceSelect = barchart_sourceSelect,
          mode = barchart_mode, slider = input$barchart_slider 
        )
        constructBarchart(enrichmentFilteredData, barchart_mode)
      }
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create barchart with these inputs.")
  }, finally = {
    removeModal()
  })
}

constructBarchart <- function(barchartData, barchart_mode) {
  column <- switch(
    barchart_mode,
    "Enrichment Score" = "Enrichment Score %",
    "-log10Pvalue"
  )
  barchartData$Term_ID <-
    factor(barchartData$Term_ID,
           levels = unique(barchartData$Term_ID)[order(barchartData[[column]],
                                                       decreasing = F)])
  height <- calculatePlotHeight(nrow(barchartData))
  renderBarchart("barchart", barchartData, column, height)
}
