handleScatterPlot <- function(enrichmentType, enrichmentTool) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Scatter Plot.</p>")
    if (existEnrichmentResults(enrichmentType, enrichmentTool)){
      type_Tool <- paste(enrichmentType, enrichmentTool, sep = "_")
      sourceSelect <- input[[paste(type_Tool, "scatterPlot_sourceSelect", sep = "_")]]

      if (isSourceNotNull(sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(
          enrichmentType, enrichmentTool,
          outputId = paste(type_Tool, "scatterPlot", sep = "_"),
          sourceSelect = sourceSelect,
          mode = input[[paste(type_Tool, "scatterPlot_mode", sep = "_")]],
          slider = input[[paste(type_Tool, "scatterPlot_slider", sep = "_")]])
        
        constructScatterPlot(type_Tool, enrichmentFilteredData)
      }
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create scatterplot with these inputs.")
  }, finally = {
    removeModal()
  })
}

constructScatterPlot <- function(type_Tool, scatterData) {
  scatterData <- addJitter(scatterData)
  renderScatterPlot(paste(type_Tool, "scatterPlot", sep = "_"), scatterData)
}

addJitter <- function(scatterData) {
  size <- nrow(scatterData)
  scatterData$`Enrichment Score %_jittered` <-
    scatterData$`Enrichment Score %` + runif(size, min = -0.5, max = 0.5)
  scatterData$`-log10Pvalue_jittered` <-
    scatterData$`-log10Pvalue` + runif(size, min = -0.005, max = 0.005)
  return(scatterData)
}
