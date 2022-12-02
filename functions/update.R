updatePlotControlPanels <- function() {
  selectedDataSource <- updatePlotDataSources()
  updatePlotSliderInputs(selectedDataSource)
}

updatePlotDataSources <- function(){
  sources <- unique(gprofilerTransformedResult$Source)
  if (!is.na(match("GO:MF", sources))) selected <- "GO:MF"
  else if (!is.na(match("GO:CC", sources))) selected <- "GO:CC"
  else if (!is.na(match("GO:BP", sources))) selected <- "GO:BP"
  else if (!is.na(match("KEGG", sources))) selected <- "KEGG"
  else if (!is.na(match("REAC", sources))) selected <- "REAC"
  else if (!is.na(match("WP", sources))) selected <- "WP"
  else if (!is.na(match("TF", sources))) selected <- "TF"
  else if (!is.na(match("HP", sources))) selected <- "HP"
  else if (!is.na(match("HPA", sources))) selected <- "HPA"
  else if (!is.na(match("MIRNA", sources))) selected <- "MIRNA"
  else if (!is.na(match("CORUM", sources))) selected <- "CORUM"
  
  updatePickerInput(session, "scatter_sourceSelect",
                    choices = sources, selected = selected)
  updatePickerInput(session, "barchart_sourceSelect",
                    choices = sources, selected = selected)
  lapply(heatmapIds, function(heatmapId) {
    updateSelectInput(session, paste0(heatmapId, "_sourceSelect"),
                      choices = sources, selected = selected)
  })
  lapply(networkIds, function(networkId) {
    updatePickerInput(session, paste0(networkId, "_sourceSelect"),
                      choices = sources, selected = selected)
  })
  return(selected)
}

updatePlotSliderInputs <- function(selectedDataSource) {
  maxSliderValue <- nrow(
    gprofilerTransformedResult[grepl(selectedDataSource,
                                     gprofilerTransformedResult$Source), ]
  )
  updateShinySliderInput(shinyOutputId = "scatter_slider",
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = "barchart_slider",
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = "heatmap1_slider",
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = "heatmap2_slider",
                         minSliderValue = 2, maxSliderValue)
  updateShinySliderInput(shinyOutputId = "heatmap3_slider",
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = "network1_slider",
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = "network2_slider",
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = "network3_slider",
                         minSliderValue = 1, maxSliderValue)
  updateShinySliderInput(shinyOutputId = "network3_thresholdSlider",
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
