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


handleBackgroundModeSelection <- function(choice, enrichmentType) {
  tryCatch({
    updateBackgroundMode(choice, enrichmentType)
  }, error = function(e) {
    cat(paste("Error: ", e))
    renderError("Unexpected error occured.")
  })
}

handleBackgroundListUpdate <- function(enrichmentType) {
  tryCatch({
    updateBackgroundListChoices(enrichmentType)
  }, error = function(e) {
    cat(paste("Error: ", e))
    renderError("Unexpected error occured.")
  })
}