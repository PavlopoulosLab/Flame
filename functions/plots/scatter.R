handleScatterPlot <- function(enrichmentType) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Scatter Plot.</p>")
    if (existEnrichmentResults(enrichmentType)){
      sourceSelect <- input[[paste0(enrichmentType, "_scatter_sourceSelect")]]

      if (isSourceNotNull(sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(enrichmentType,
          id = paste0(enrichmentType, "_scatterPlot"), plotType = "scatterPlot",
          sourceSelect = sourceSelect,
          mode = input[[paste0(enrichmentType, "_scatter_mode")]],
          slider = input[[paste0(enrichmentType, "_scatter_slider")]])
        
        constructScatterPlot(enrichmentType, enrichmentFilteredData)
      }
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create scatterplot with these inputs.")
  }, finally = {
    removeModal()
  })
}

constructScatterPlot <- function(enrichmentType, scatterData) {
  scatterData <- addJitter(scatterData)
  renderScatterPlot(paste0(enrichmentType, "_scatterPlot"), scatterData)
}

addJitter <- function(scatterData) {
  size <- nrow(scatterData)
  scatterData$`Enrichment Score %_jittered` <-
    scatterData$`Enrichment Score %` + runif(size, min = -0.5, max = 0.5)
  scatterData$`-log10Pvalue_jittered` <-
    scatterData$`-log10Pvalue` + runif(size, min = -0.005, max = 0.005)
  return(scatterData)
}
