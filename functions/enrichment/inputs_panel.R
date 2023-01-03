handleFunctionalEnrichmentOrganismSelection <- function() {
  tryCatch({
    # updateAvailableTools() # TODO
    # updateAvailableNamespaces()
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

getChoicesUnion <- function(toolCapitalNames) {
  choices <- list()
  for (tool in toolCapitalNames) {
    toolChoices <- eval(parse(text = paste0(tool, "_DATASOURCES_PRINT")))
    choices <- c(choices, toolChoices)
  }
  choices <- choices[!duplicated(choices)]
  return(choices)
}

getChoicesIntersection <- function(toolCapitalNames) {
  choices <- list()
  for (tool in toolCapitalNames) {
    toolChoices <- eval(parse(text = paste0(tool, "_NAMESPACES")))
    choices <- c(choices, toolChoices)
  }
  toolCount <- length(toolCapitalNames)
  if (toolCount > 1) {
    # fromLast = T, to keep ENSP first for aGOtool
    choiceNames <- names(table(names(choices)))[table(names(choices)) == toolCount]
    choices <- choices[choiceNames]
  }
  return(choices)
}

decideChoiceSelected <- function(toolCapitalNames) {
  selected <- ""
  if ("AGOTOOL" %in% toolCapitalNames)
    selected <- "ENSP"
  else if ("GPROFILER" %in% toolCapitalNames)
    selected <- "USERINPUT"
  return(selected)
}

getAvailableSignificanceMetrics <- function(toolCapitalNames) {
  choices <- ""
  toolCount <- length(toolCapitalNames)
  if (toolCount == 1){
    shinyjs::enable("functional_enrichment_metric")
    choices <- eval(parse(text = paste0(toolCapitalNames, "_METRICS")))
  } else {
    shinyjs::disable("functional_enrichment_metric")
    choices <- DEFAULT_METRIC_TEXT
  }
  return(choices)
}
