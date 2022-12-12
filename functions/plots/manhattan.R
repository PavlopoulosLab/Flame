handleManhattanPlot <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Manhattan Plot.</p>")
    if (existEnrichmentResults("functional", "gProfiler")) {
      resetManhattanTable()
      if (validGprofilerResult())
        renderManhattanPlot()
      else
        renderWarning("The Manhattan plot is currently available
                      only for the GPROFILER analysis pipeline.")
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not draw Manhattan plot.")
  }, finally = {
    removeModal()
  })
}

handleManhattanClick <- function() {
  tryCatch({
    currentTermID <- event_data("plotly_click")$key
    manhattanTable <- enrichmentResults[[currentType_Tool]][match(
      currentTermID, enrichmentResults[[currentType_Tool]]$Term_ID_noLinks), ]
    renderManhattanEnrichmentTable(manhattanTable)
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not print selected entries.")
  })
}

handleManhattanSelect <- function() {
  tryCatch({
    currentTermIDs <- event_data("plotly_selected")$key
    manhattanTable <- 
      enrichmentResults[[currentType_Tool]][which(
        enrichmentResults[[currentType_Tool]]$Term_ID_noLinks %in% currentTermIDs
      ), ]
    renderManhattanEnrichmentTable(manhattanTable)
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not print selected entries.")
  })
}
