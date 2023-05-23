updateUserInputLists <- function(inputDF, listName) {
  colnames(inputDF) <- listName
  userInputLists <<- c(userInputLists, list(inputDF))
  names(userInputLists)[length(userInputLists)] <<- listName
}

updateListBoxes <- function() {
  updateCheckboxGroupInput(session, "checkboxLists", choices = names(userInputLists))
  updateSelectInput(session, "functional_enrichment_file", choices = names(userInputLists))
  updateSelectInput(session, "functional_enrichment_background_list", choices = names(userInputLists))
  updateSelectInput(session, "literature_enrichment_file", choices = names(userInputLists))
  updateSelectInput(session, "literature_enrichment_background_list", choices = names(userInputLists))
  updateSelectInput(session, "selectView", choices = names(userInputLists))
  updateSelectInput(session, "selectUpset", choices = names(userInputLists))
  updateSelectInput(session, "gconvert_select", choices = names(userInputLists))
  updateSelectInput(session, "gorth_select", choices = names(userInputLists))
  updateSelectInput(session, "aGOtoolSelect", choices = names(userInputLists))
  updateSelectInput(session, "literatureSelect", choices = names(userInputLists))
  updateSelectInput(session, "STRINGnetworkSelect", choices = names(userInputLists))
  updateBackgroundListChoices("functional")
  updateBackgroundListChoices("literature")
  toggleUpsetTab()
}

updateVolcanoSliders <- function(maxLog10PValue, maxLogFC) {
  updateShinySliderInput(
    shinyOutputId = "volcano_pvalue_slider",
    minSliderValue = 0, maxSliderValue = ceiling(maxLog10PValue),
    value = input$volcano_pvalue_slider,
    step = DEFAULT_VOLCANO_LOG10PVALUE_STEP
  )
  updateShinySliderInput(
    shinyOutputId = "volcano_fc_slider",
    minSliderValue = 0, maxSliderValue = ceiling(maxLogFC),
    value = input$volcano_fc_slider,
    step = DEFAULT_VOLCANO_LOG2FC_STEP
  )
}

updateVolcanoMetricsConversionText <- function(log10pvalue, log2fc) {
  conversionText <- sprintf(
    "pvalue = %f\nFC = %f",
    10 ^ -log10pvalue,
    2 ^ log2fc
  )
  renderShinyText("volcanoMetricConversions", conversionText)
}

updateAvailableTools <- function() {
  inputOrganism <- input$functional_enrichment_organism
  if (inputOrganism != "") {
    # always trigger a changed-tool event to correctly update namespaces
    updatePickerInput(session, "functional_enrichment_tool",
                      choices = NULL, selected = character(0))
    taxid <- ORGANISMS[ORGANISMS$print_name == inputOrganism, ]$taxid
    organismShortName <- ORGANISMS[ORGANISMS$print_name == inputOrganism, ]$short_name
    choices <- names(which(sapply(TOOL_ORGANISMS, function(tool) taxid %in% tool)))
    selected <- choices[1]
    if (isSpecialOrganism(organismShortName))
      selected <- SPECIAL_PREFERRED_TOOL[[organismShortName]]
    updatePickerInput(session, "functional_enrichment_tool",
                      choices = choices, selected = selected)
  }
}

isSpecialOrganism <- function(organismShortName) {
  return(organismShortName %in% SPECIAL_ORGANISMS)
}

updateAvailableDatasources <- function() {
  toolCapitalNames <- toupper(input$functional_enrichment_tool)
  choices <- getChoicesUnion(toolCapitalNames)
  updatePickerInput(session, "functional_enrichment_datasources",
                    choices = choices, selected = DATASOURCES_DEFAULT_SELECTED)
}

getChoicesUnion <- function(toolCapitalNames) {
  choices <- c()
  for (tool in toolCapitalNames) {
    prefix <- ""
    if (tool == "ENRICHR") {
      prefix <- getEnrichrVariablePrefix()
    }
    choices <- c(choices, DATASOURCES[[paste0(prefix, tool)]])
  }
  choices <- filterDatasourcePrintChoices(unique(choices))
  return(choices)
}

filterDatasourcePrintChoices <- function(choices) {
  filtered_DATASOURCES_PRINT <- DATASOURCES_PRINT
  for (i in length(DATASOURCES_PRINT):1) {
    filtered_DATASOURCES_PRINT[[i]][!filtered_DATASOURCES_PRINT[[i]] %in% choices] <- NULL
    if (length(filtered_DATASOURCES_PRINT[[i]]) == 0)
      filtered_DATASOURCES_PRINT[[i]] <- NULL
  }
  return(filtered_DATASOURCES_PRINT)
}

addNewDatasourcesOnly <- function(toolCategoryChoices, choices) {
  if (!identical(toolCategoryChoices[[1]], choices[[names(toolCategoryChoices)]]))
    for (i in 1:length(toolCategoryChoices[[1]])) {
      if (!toolCategoryChoices[[1]][[i]] %in% choices[[names(toolCategoryChoices)]])
        choices[[names(toolCategoryChoices)]] <-
          c(choices[[names(toolCategoryChoices)]], toolCategoryChoices[[1]][i])
    }
  return(choices)
}

updateAvailableNamespaces <- function() {
  organismShortName <-
    ORGANISMS[ORGANISMS$print_name == input$functional_enrichment_organism, ]$short_name
  if (!is.na(organismShortName)) {
    toolCapitalNames <- toupper(input$functional_enrichment_tool)
    choices <- getNamespaceChoices(toolCapitalNames)
    selected <- choices[1]
    if (isSpecialOrganism(organismShortName) &&
        all(toolCapitalNames == "ENRICHR"))
      selected <- SPECIAL_PREFERRED_NAMESPACE[[organismShortName]]
    updateSelectInput(session, "functional_enrichment_namespace",
                      choices = choices, selected = selected)
  } else {
    shinyjs::disable("functional_enrichment_namespace")
    updateSelectInput(session, "functional_enrichment_namespace",
                      choices = c("ENSEMBL Protein ID" = "ENSP"),
                      selected = "ENSP")
  }
}

getNamespaceChoices <- function(toolCapitalNames) {
  if (length(toolCapitalNames) == 1) {
    shinyjs::enable("functional_enrichment_namespace")
    prefix <- getNamespacePrefix(toolCapitalNames)
    choices <- NAMESPACES[[paste0(prefix, toolCapitalNames)]]
    organismShortName <-
      ORGANISMS[ORGANISMS$print_name == input$functional_enrichment_organism, ]$short_name
    if (isSpecialOrganism(organismShortName) && toolCapitalNames == "GPROFILER")
      choices <- c(NAMESPACES[["SPECIAL"]][[organismShortName]], choices)
  } else {
    shinyjs::disable("functional_enrichment_namespace")
    choices <- DEFAULT_NAMESPACE_TEXT
  }
  return(choices)
}

getNamespacePrefix <- function(toolCapitalNames) {
  prefix <- ""
  if (toolCapitalNames == "ENRICHR") {
    prefix <- switch(
      ORGANISMS[ORGANISMS$print_name ==
                  input[["functional_enrichment_organism"]], ]$short_name,
      "dmelanogaster" = "FLY_",
      "scerevisiae" = "YEAST_"
    )
  }
  return(prefix)
}

updateAvailableSignificanceMetrics <- function() {
  toolCapitalNames <- toupper(input$functional_enrichment_tool)
  choices <- getAvailableSignificanceMetrics(toolCapitalNames)
  updateSelectInput(session, "functional_enrichment_metric", choices = choices)
}

getAvailableSignificanceMetrics <- function(toolCapitalNames) {
  if (length(toolCapitalNames) == 1) {
    shinyjs::enable("functional_enrichment_metric")
    choices <- METRICS[[toolCapitalNames]]
  } else {
    shinyjs::disable("functional_enrichment_metric")
    choices <- DEFAULT_METRIC_TEXT
  }
  return(choices)
}

updatePlotControlPanels <- function() {
  selectedDataSource <- updatePlotDataSources()
  updatePlotSliderInputs(selectedDataSource)
}

updatePlotDataSources <- function(){
  sources <- switch(
    currentEnrichmentType,
    "functional" = ENRICHMENT_DATASOURCES[
      which(ENRICHMENT_DATASOURCES %in% unique(enrichmentResults[[currentType_Tool]]$Source))
    ],
    "literature" = "PUBMED"
  )
  selected <- sources[1]
  
  lapply(ALL_PLOT_IDS, function(plotId) {
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
  if (maxSliderValue > MAX_SLIDER_VALUE)
    maxSliderValue <- MAX_SLIDER_VALUE
  
  lapply(ALL_PLOT_IDS, function(plotId) {
    updateShinySliderInput(
      shinyOutputId = paste(currentType_Tool, plotId, "slider", sep = "_"),
      minSliderValue = 1, maxSliderValue)
  })
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

updateAvailableStringNamespaces <- function() {
  if (input$string_network_organism != "") {
    if (!is.na(
      ORGANISMS[ORGANISMS$print_name == input$string_network_organism, ]$short_name
    )) {
      shinyjs::enable("STRING_namespace")
      updateSelectInput(session, "STRING_namespace",
                        choices = NAMESPACES[["AGOTOOL"]],
                        selected = "ENSP")
    } else {
      shinyjs::disable("STRING_namespace")
      updateSelectInput(session, "STRING_namespace",
                        choices = c("User Input" = "USERINPUT"),
                        selected = "USERINPUT")
    }
  }
}


updateBackgroundMode <- function(choice, enrichmentType) {
  if (choice == "genome") {
    shinyjs::hide(paste0(enrichmentType, "_enrichment_background_container"))
    # this is only for enrichmentType = 'functional',
    # since 'literature' only has aGOtool anyway
    updatePickerInput(session, "functional_enrichment_tool",
                      choices = ENRICHMENT_TOOLS, selected = DEFAULT_TOOL)
  }
  else {
    shinyjs::show(paste0(enrichmentType, "_enrichment_background_container"))
    # this is only for enrichmentType = 'functional',
    # since 'literature' only has aGOtool anyway
    updatePickerInput(session, "functional_enrichment_tool",
                      choices = c("aGOtool", "gProfiler", "WebGestalt"), selected = DEFAULT_TOOL)
  }
}

updateBackgroundListChoices <- function(enrichmentType) {
  # the code below makes sure that the input list is NOT a choice for the background
  # first, we get the user input value
  userInputVal <- input[[paste0(enrichmentType, "_enrichment_file")]]
  # then, we remove the user input from the list of choices (locally, not globally)
  background_choices <- names(userInputLists)
  background_choices <- background_choices[background_choices != userInputVal]
  updateSelectInput(session, paste0(enrichmentType, "_enrichment_background_list"), choices = background_choices)
}
