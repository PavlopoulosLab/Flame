handleManhattanPlot <- function(networkId) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Manhattan Plot.</p>")
    resetManhattanTable()
    renderManhattanPlot()
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
    manhattanTable <- gprofilerTransformedResult[match(
      currentTermID,
      gprofilerTransformedResult$Term_ID_noLinks), ]
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
      gprofilerTransformedResult[which(
        gprofilerTransformedResult$Term_ID_noLinks %in% currentTermIDs
      ), ]
    renderManhattanEnrichmentTable(manhattanTable)
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not print selected entries.")
  })
}
