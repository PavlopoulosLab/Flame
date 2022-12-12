updateAvailableTools <- function() {
  # TODO
  # updateSelectInput(session, "functional_enrichment_organism",
  #                   choices = choices)
}

updateAvailableDatasources <- function() {
  toolCapitalNames <- toupper(input$functional_enrichment_tool)
  choices <- getChoicesUnion(toolCapitalNames)
  selected <- eval(
    parse(text = paste0("AGOTOOL", "_DATASOURCES_DEFAULT_SELECTED")))
  updatePickerInput(session, "functional_enrichment_datasources",
                    choices = choices, selected = selected)
}

updateAvailableNamespaces <- function() { # TODO organism/tool combination
  toolCapitalNames <- toupper(input$functional_enrichment_tool)
  choices <- getChoicesIntersection(toolCapitalNames)
  selected <- decideChoiceSelected(toolCapitalNames)
  updateSelectInput(session, "functional_enrichment_namespace",
                    choices = choices, selected = selected)
}

updateAvailableSignificanceMetrics <- function() {
  toolCapitalNames <- toupper(input$functional_enrichment_tool)
  choices <- getAvailableSignificanceMetrics(toolCapitalNames)
  updateSelectInput(session, "functional_enrichment_metric", choices = choices)
}

updatePlotControlPanels <- function() {
  selectedDataSource <- updatePlotDataSources()
  updatePlotSliderInputs(selectedDataSource)
}

updatePlotDataSources <- function(){
  sources <- switch(
    currentEnrichmentType,
    "functional" = unique(enrichmentResults[[currentType_Tool]]$Source),
    "literature" = "PUBMED"
  )
  selected <- sources[1]
  
  updatePickerInput(session, paste(currentEnrichmentType, currentEnrichmentTool,
                                   "scatter_sourceSelect", sep = "_"),
                    choices = sources, selected = selected)
  updatePickerInput(session, paste(currentEnrichmentType, currentEnrichmentTool,
                                    "barchart_sourceSelect", sep = "_"),
                    choices = sources, selected = selected)
  lapply(HEATMAP_IDS, function(heatmapId) {
    updateSelectInput(session, paste(currentEnrichmentType, currentEnrichmentTool,
                                     heatmapId, "sourceSelect", sep = "_"),
                      choices = sources, selected = selected)
  })
  lapply(NETWORK_IDS, function(networkId) {
    updatePickerInput(session, paste(currentEnrichmentType, currentEnrichmentTool,
                                     networkId, "sourceSelect", sep = "_"),
                      choices = sources, selected = selected)
  })
  return(selected)
}

updatePlotSliderInputs <- function(selectedDataSource) {
  maxSliderValue <- switch(
    currentEnrichmentType,
    "functional" = nrow(
      enrichmentResults[[currentType_Tool]][grepl(
        selectedDataSource, enrichmentResults[[currentType_Tool]]$Source), ]),
    "literature" = nrow(enrichmentResults[[currentType_Tool]])
  )
   
  updateShinySliderInput(
    shinyOutputId = paste(currentEnrichmentType, currentEnrichmentTool,
                          "scatter_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste0(currentEnrichmentType, currentEnrichmentTool,
                           "barchart_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste0(currentEnrichmentType, currentEnrichmentTool,
                           "heatmap1_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste0(currentEnrichmentType, currentEnrichmentTool,
                           "heatmap2_slider", sep = "_"),
    minSliderValue = 2, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste0(currentEnrichmentType, currentEnrichmentTool,
                           "heatmap3_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste0(currentEnrichmentType, currentEnrichmentTool,
                           "network1_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste0(currentEnrichmentType, currentEnrichmentTool,
                           "network2_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste0(currentEnrichmentType, currentEnrichmentTool,
                           "network3_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste0(currentEnrichmentType, currentEnrichmentTool,
                           "network3_thresholdSlider", sep = "_"),
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
