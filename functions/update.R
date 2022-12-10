updateAvailableTools <- function() {
  # TODO
  # updateSelectInput(session, "functional_enrichment_organism",
  #                   choices = choices)
}

updateAvailableDatasources <- function() {
  toolCapitalName <- toupper(input$functional_enrichment_tool)
  choices <- eval(
    parse(text = paste0(toolCapitalName, "_DATASOURCES_PRINT")))
  selected <- eval(
    parse(text = paste0(toolCapitalName, "_DATASOURCES_DEFAULT_SELECTED")))
  updatePickerInput(session, "functional_enrichment_datasources",
                    choices = choices, selected = selected)
}

updateAvailableNamespaces <- function() { # TODO organism/tool combination
  toolCapitalName <- toupper(input$functional_enrichment_tool)
  choices <- eval(
    parse(text = paste0(toolCapitalName, "_NAMESPACES")))
  updateSelectInput(session, "functional_enrichment_namespace",
                    choices = choices)
}

updateAvailableSignificanceMetrics <- function() {
  toolCapitalName <- toupper(input$functional_enrichment_tool)
  choices <- eval(
    parse(text = paste0(toolCapitalName, "_METRICS")))
  updateSelectInput(session, "functional_enrichment_metric",
                    choices = choices)
}

updatePlotControlPanels <- function() {
  selectedDataSource <- updatePlotDataSources()
  updatePlotSliderInputs(selectedDataSource)
}

updatePlotDataSources <- function(){
  sources <- switch(
    currentEnrichmentType,
    "functional" = unique(functionalEnrichmentResult$Source),
    "literature" = "PUBMED"
  )
  selected <- sources[1]
  
  updatePickerInput(session, paste0(currentEnrichmentType, "_scatter_sourceSelect"),
                    choices = sources, selected = selected)
  updatePickerInput(session, paste0(currentEnrichmentType, "_barchart_sourceSelect"),
                    choices = sources, selected = selected)
  lapply(HEATMAP_IDS, function(heatmapId) {
    updateSelectInput(session, paste(currentEnrichmentType, heatmapId, "sourceSelect", sep = "_"),
                      choices = sources, selected = selected)
  })
  lapply(NETWORK_IDS, function(networkId) {
    updatePickerInput(session, paste(currentEnrichmentType, networkId, "sourceSelect", sep = "_"),
                      choices = sources, selected = selected)
  })
  return(selected)
}

updatePlotSliderInputs <- function(selectedDataSource) {
  maxSliderValue <- switch(
    currentEnrichmentType,
    "functional" = nrow(
      functionalEnrichmentResult[grepl(selectedDataSource,
                                       functionalEnrichmentResult$Source), ]),
    "literature" = nrow(literatureEnrichmentResult)
  )
   
  updateShinySliderInput(shinyOutputId = paste0(currentEnrichmentType,
                                                "_scatter_slider"),
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = paste0(currentEnrichmentType,
                                                "_barchart_slider"),
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = paste0(currentEnrichmentType,
                                                "_heatmap1_slider"),
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = paste0(currentEnrichmentType,
                                                "_heatmap2_slider"),
                         minSliderValue = 2, maxSliderValue)
  updateShinySliderInput(shinyOutputId = paste0(currentEnrichmentType,
                                                "_heatmap3_slider"),
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = paste0(currentEnrichmentType,
                                                "_network1_slider"),
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = paste0(currentEnrichmentType,
                                                "_network2_slider"),
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = paste0(currentEnrichmentType,
                                                "_network3_slider"),
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = paste0(currentEnrichmentType,
                                                "_network3_thresholdSlider"),
                         minSliderValue = 1, maxSliderValue)
}

updateShinySliderInput <- function(shinyOutputId, minSliderValue, maxSliderValue,
                                   value = DEFAULT_SLIDER_VALUE) {
  updateSliderInput(
    session, shinyOutputId,
    min = minSliderValue, max = maxSliderValue,
    value = value, step = 1
  )
}
