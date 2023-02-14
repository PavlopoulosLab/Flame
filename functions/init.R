initializeServerApp <- function() {
  toggleUpsetTab()
  hideTab("inputPlots", "Volcano Plot")
  session$sendCustomMessage("handler_setListLimit", LISTNAME_NCHAR_LIMIT)
  lapply(ENRICHMENT_TOOLS, function(toolName) {
    session$sendCustomMessage("handler_hideSourceTabs",
                              paste0("functional_", toolName))
  })
  initialiazeOrganismSelectors()
  initializeDatasources()
  initializeVolcanoPanel()
  initializeEnrichmentResults()
  initializeArenaEdgelist()
  hideConversionBoxes()
  hideVisNetworks()
  hideEnrichmentTabs()
}

toggleUpsetTab <- function() {
  if (length(userInputLists) > 1) {
    showTab("inputPlots", "UpSet Plot")
    updateTabsetPanel(session, "inputPlots", selected = "UpSet Plot")
  }
  else
    hideTab("inputPlots", "UpSet Plot")
}

initialiazeOrganismSelectors <- function() {
  selected <- "Homo sapiens (Human) [NCBI Tax. ID: 9606]"
  gProfiler_printNames <- ORGANISMS[!is.na(ORGANISMS$short_name), ]$print_name
  aGoTool_printNames <- ORGANISMS[ORGANISMS$taxid %in% TOOL_ORGANISMS$aGOtool, ]$print_name
  updateSelectizeInput(session, 'textmining_organism',
                       choices = aGoTool_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'functional_enrichment_organism',
                       choices = ORGANISMS$print_name, selected = selected, server = T)
  updateSelectizeInput(session, 'literature_enrichment_organism',
                       choices = aGoTool_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'string_network_organism',
                       choices = aGoTool_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'gconvert_organism',
                       choices = gProfiler_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'gorth_organism',
                       choices = gProfiler_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'gorth_target', choices = gProfiler_printNames[gProfiler_printNames != selected],
                       selected = "Mus musculus (Mouse) [NCBI Tax. ID: 10090]", server = T)
}

initializeDatasources <- function() {
  for (datasourceName in names(DATASOURCES_CODES)) {
    DATASOURCES[[datasourceName]] <<- names(DATASOURCES_CODES[[datasourceName]])
  }
}

initializeVolcanoPanel <- function() {
  output$volcanoSelectionInfo <- renderText({
    "Pick the box or lasso from the plot toolbar and then select items. Double-click to reset view."
  })
  shinyjs::hide("volcanoSelectionInfo")
  initializeVolcanoMetricsConversionText()
  shinyjs::hide("volcanoPanel")
}

initializeVolcanoMetricsConversionText <- function() {
  updateVolcanoMetricsConversionText(DEFAULT_VOLCANO_LOG10PVALUE_THRESHOLD,
                                     DEFAULT_VOLCANO_LOG2FC_THRESHOLD)
}

initializeEnrichmentResults <- function() {
  for (enrichmentTool in ENRICHMENT_TOOLS) {
    newItem <- list(data.frame())
    names(newItem) <- paste("functional", enrichmentTool, sep = "_")
    enrichmentResults <<- c(enrichmentResults, newItem)
  }
  newItem <- list(data.frame())
  names(newItem) <- paste("literature", "aGoTool", sep = "_")
  enrichmentResults <<- c(enrichmentResults, newItem)
}

initializeArenaEdgelist <- function() {
  for (enrichmentTool in ENRICHMENT_TOOLS) {
    for (networkId in NETWORK_IDS) {
      newItem <- list(data.frame())
      names(newItem) <- paste("functional", enrichmentTool, networkId, sep = "_")
      arenaEdgelist <<- c(arenaEdgelist, newItem)
    }
  }
  for (networkId in NETWORK_IDS) {
    newItem <- list(data.frame())
    names(newItem) <- paste("literature", "aGoTool", networkId, sep = "_")
    arenaEdgelist <<- c(arenaEdgelist, newItem)
  }
}

hideConversionBoxes <- function() {
  lapply(ENRICHMENT_TOOLS, function(enrichmentTool) {
    shinyjs::hide(paste("functional", enrichmentTool,
                        "conversionBoxes", sep = "_"))
  })
  shinyjs::hide("literature_aGOtool_conversionBoxes")
}

hideVisNetworks <- function() {
  lapply(ENRICHMENT_TOOLS, function(enrichmentTool) {
    lapply(NETWORK_IDS, function(networkId) {
      shinyjs::hide(paste("functional", enrichmentTool, networkId, sep = "_"))
    })
  })
  lapply(NETWORK_IDS, function(networkId) {
    shinyjs::hide(paste("literature", "aGoTool", networkId, sep = "_"))
  })
}

hideEnrichmentTabs <- function() {
  lapply(c(ENRICHMENT_TOOLS, "Combination"), function(toolName) {
    hideTab(inputId = "toolTabsPanel", target = toolName)
  })
}
