prepareCombinationTab <- function() {
  if (existTwoToolResults()) {
    showTab(inputId = "toolTabsPanel", target = "Combination")
    createGlobalComboTable()
    choices <- ENRICHMENT_DATASOURCES[
      which(ENRICHMENT_DATASOURCES %in% unique(combinationResult$Source))
    ]
    updatePickerInput(session, "combo_datasources",
                      choices = choices, selected = NULL) # to always trigger the event
    updatePickerInput(session, "combo_datasources",
                      choices = choices, selected = choices)
  }
}

existTwoToolResults <- function() {
  exist <- F
  functionalEnrichmentResults <- enrichmentResults[grep("^functional", names(enrichmentResults))]
  if (length(functionalEnrichmentResults[lengths(functionalEnrichmentResults) > 0]) > 1) {
    exist <- T
  }
  return(exist)
}

createGlobalComboTable <- function() {
  functionalEnrichmentResults <- enrichmentResults[grep("^functional", names(enrichmentResults))]
  combinationResult <<- dplyr::bind_rows(functionalEnrichmentResults, .id = "Tool")
  combinationResult <<- combinationResult[, c("Source", "Term_ID", "Function", "Term_ID_noLinks", "Tool")]
  combinationResult$Tool <<- sapply(strsplit(combinationResult$Tool, "_"), "[[", 2)
  termToolMatching <- combinationResult %>%
    group_by(Term_ID_noLinks) %>%
    summarise(Tools = toString(Tool)) %>%
    ungroup()
  
  combinationResult <<- plyr::join(termToolMatching, combinationResult, type = "left",
                                   by = "Term_ID_noLinks")
  combinationResult <<- distinct(combinationResult , Term_ID_noLinks, Tools,
                                 .keep_all = T)
  combinationResult <<- combinationResult[, c("Source", "Term_ID", "Function",
                                               "Term_ID_noLinks", "Tools")]
  combinationResult$Rank <<- lengths(
    regmatches(
      combinationResult$Tools, gregexpr(",", combinationResult$Tools)
    )
  ) + 1
}

handleComboSourceSelect <- function() {
  filteredCombinationResult <- filterComboTable()
  filteredCombinationResult <-
    filteredCombinationResult[order(-filteredCombinationResult$Rank), ]
  renderShinyDataTable(
    "combo_table", filteredCombinationResult, caption = "Term-tool combinations",
    fileName = "combination", hiddenColumns = c(3), filter = "top"
  )
  executeComboUpsetPlot(filteredCombinationResult)
}

filterComboTable <- function() {
  datasources <- input$combo_datasources
  filteredCombinationResult <- combinationResult
  
  if (isSourceNotNull(datasources)) {
    filteredCombinationResult <- subset(
      filteredCombinationResult,
      Source %in% datasources
    )
  }
  return(filteredCombinationResult)
}

executeComboUpsetPlot <- function(filteredCombinationResult) {
  toolTermLists <- extractToolTermLists(filteredCombinationResult)
  renderUpset("upsetjsCombo", toolTermLists, upsetjs::generateDistinctIntersections)
}

extractToolTermLists <- function(filteredCombinationResult) {
  filteredCombinationResult <- 
    tidyr::separate_rows(filteredCombinationResult, `Tools`, sep = ", ")
  filteredCombinationResult <- filteredCombinationResult[, c("Tools",
                                                             "Term_ID_noLinks")]
  filteredCombinationResult <- filteredCombinationResult %>%
    dplyr::group_by(Tools) %>%
    dplyr::mutate(`Term_ID_noLinks` = paste(`Term_ID_noLinks`, collapse = ","))
  filteredCombinationResult <- distinct(filteredCombinationResult)
  
  toolTermLists <- as.list(strsplit(filteredCombinationResult$Term_ID_noLinks, ","))
  names(toolTermLists) <- filteredCombinationResult$Tools
  return(toolTermLists)
}

handleComboUpsetClick <- function() {
  tryCatch({
    upsetjs_click <- input$upsetjsCombo_click
    if (!identical(as.character(upsetjs_click$elems), character(0))) {
      elements <- as.data.frame(as.character(upsetjs_click$elems))
      colnames(elements) <- "Term_ID_noLinks"
      elements <- plyr::join(elements, combinationResult, type = "left",
                             by = "Term_ID_noLinks")
      renderShinyDataTable("combo_upsetClick_table", elements,
                           caption = "UpSet Clicked Terms",
                           fileName = "combo_upsetClick", hiddenColumns = c(0),
                           filter = "top")
    }
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with UpSet Plot click.")
  })
}
