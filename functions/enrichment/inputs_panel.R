handleFunctionalEnrichmentOrganismSelection <- function() {
  tryCatch({
    updateAvailableTools()
  }, error = function(e) {
    cat(paste("Error: ", e))
    renderError("Unexpected error occured.")
  })
}

handleFunctionalEnrichmentToolSelection <- function() {
  tryCatch({
    updateAvailableDatasources()
    updateAvailableNamespaces()
    updateAvailableSignificanceMetrics()
  }, error = function(e) {
    cat(paste("Error: ", e))
    renderError("Unexpected error occured.")
  })
}
