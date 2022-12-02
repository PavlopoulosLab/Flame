handleScatterPlot <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Scatter Plot.</p>")
    if (existEnrichmentResults()){
      scatter_sourceSelect <- input$scatter_sourceSelect

      if (isSourceNotNull(scatter_sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(
          id = "scatterPlot", plotType = "scatterPlot",
          sourceSelect = scatter_sourceSelect,
          mode = input$scatter_mode, slider = input$scatter_slider 
        )
        constructScatterPlot(enrichmentFilteredData)
      }
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create scatterplot with these inputs.")
  }, finally = {
    removeModal()
  })
}

constructScatterPlot <- function(scatterData) {
  scatterData <- addJitter(scatterData)
  renderScatterPlot("scatterPlot", scatterData)
}

addJitter <- function(scatterData) {
  size <- nrow(scatterData)
  scatterData$`Enrichment Score %_jittered` <-
    scatterData$`Enrichment Score %` + runif(size, min = -0.5, max = 0.5)
  scatterData$`-log10Pvalue_jittered` <-
    scatterData$`-log10Pvalue` + runif(size, min = -0.005, max = 0.005)
  return(scatterData)
}
