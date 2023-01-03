updateUserInputLists <- function(inputDF, listName) {
  colnames(inputDF) <- listName
  userInputLists <<- c(userInputLists, list(inputDF))
  names(userInputLists)[length(userInputLists)] <<- listName
}

updateListBoxes <- function() {
  updateCheckboxGroupInput(session, "checkboxLists", choices = names(userInputLists))
  updateSelectInput(session, "functional_enrichment_file", choices = names(userInputLists))
  updateSelectInput(session, "literature_enrichment_file", choices = names(userInputLists))
  updateSelectInput(session, "selectView", choices = names(userInputLists))
  updateSelectInput(session, "selectUpset", choices = names(userInputLists))
  updateSelectInput(session, "gconvert_select", choices = names(userInputLists))
  updateSelectInput(session, "gorth_select", choices = names(userInputLists))
  updateSelectInput(session, "aGOtoolSelect", choices = names(userInputLists))
  updateSelectInput(session, "literatureSelect", choices = names(userInputLists))
  updateSelectInput(session, "STRINGnetworkSelect", choices = names(userInputLists))
}

updateVolcanoSliders <- function(maxLog10PValue, maxLogFC) {
  updateShinySliderInput(
    shinyOutputId = "volcano_pvalue_slider",
    minSliderValue = 0, maxSliderValue = ceiling(maxLog10PValue),
    value = input$volcano_pvalue_slider,
    step = DEFAULT_VOLCANO_10LOGPVALUE_STEP
  )
  updateShinySliderInput(
    shinyOutputId = "volcano_fc_slider",
    minSliderValue = 0, maxSliderValue = ceiling(maxLogFC),
    value = input$volcano_fc_slider,
    step = DEFAULT_VOLCANO_LOGFC_STEP
  )
}

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
  
  allPlotIds <- c(NETWORK_IDS, HEATMAP_IDS, "barchart", "scatterPlot")
  lapply(allPlotIds, function(plotId) {
    updatePickerInput(
      session, paste(currentType_Tool, plotId, "sourceSelect", sep = "_"),
      choices = sources, selected = selected
    )
  })
  return(selected)
}

updatePlotSliderInputs <- function(selectedDataSource) {
  maxSliderValue <- switch(
    currentEnrichmentType,
    "functional" = nrow(enrichmentResults[[currentType_Tool]][grepl(
        selectedDataSource, enrichmentResults[[currentType_Tool]]$Source), ]),
    "literature" = nrow(enrichmentResults[[currentType_Tool]])
  )
  
  updateShinySliderInput(
    shinyOutputId = paste(currentType_Tool, "scatterPlot_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste(currentType_Tool, "barchart_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste(currentType_Tool, "heatmap1_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste(currentType_Tool, "heatmap2_slider", sep = "_"),
    minSliderValue = 2, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste(currentType_Tool, "heatmap3_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste(currentType_Tool, "network1_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste(currentType_Tool, "network2_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste(currentType_Tool, "network3_slider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(
    shinyOutputId = paste(currentType_Tool, "network3_thresholdSlider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
}

updateShinySliderInput <- function(shinyOutputId, minSliderValue, maxSliderValue,
                                   value = DEFAULT_SLIDER_VALUE, step = 1) {
  updateSliderInput(
    session, shinyOutputId,
    min = minSliderValue, max = maxSliderValue,
    value = value, step = step
  )
}
