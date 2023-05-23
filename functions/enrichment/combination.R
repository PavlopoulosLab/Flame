prepareCombinationTab <- function() {
  if (existTwoToolResults()) {
    showTab(inputId = "toolTabsPanel", target = "Combination")
    createGlobalComboTable()
    
    maxRank <- max(combinationResult$Rank, na.rm = TRUE)
    updateSliderInput(session, "combo_rank_slider",
                      value = maxRank, max = maxRank)
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
  tryCatch({
    filteredCombinationResult <- filterComboTable(combinationResult)
    filteredCombinationResult <-
      filteredCombinationResult[order(-filteredCombinationResult$Rank), ]
    renderShinyDataTable(
      "combo_table", filteredCombinationResult, caption = "Term-tool combinations",
      fileName = "combination", hiddenColumns = c(3), filter = "top"
    )
    executeComboUpsetPlot(filteredCombinationResult)
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with combination results.")
  })
}

filterComboTable <- function(filteredCombinationResult) {
  datasources <- input$combo_datasources
  
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

handleComboNetwork <- function() {
  tryCatch({
    comboResult_forNetwork <- parseComboVisNetwork()
    constructComboVisNetwork(comboResult_forNetwork)
    renderShinyDataTable(
      shinyOutputId = "combo_network_table",
      comboResult_forNetwork,
      caption = "Combination Network",
      fileName = "combo_network")
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with combinatorial network.")
  })
}

parseComboVisNetwork <- function() {
  functionalEnrichmentResults <- enrichmentResults[grep("^functional",
                                                        names(enrichmentResults))]
  comboResult_forNetwork <- dplyr::bind_rows(functionalEnrichmentResults,
                                              .id = "Tool")
  comboResult_forNetwork <- comboResult_forNetwork[, c("Source", "Function",
                                                        "Positive Hits", "Tool")]
  comboResult_forNetwork <- filterComboTable(comboResult_forNetwork)
  comboResult_forNetwork$Tool <-
    sapply(strsplit(comboResult_forNetwork$Tool, "_"), "[[", 2)
  comboResult_forNetwork <- comboResult_forNetwork %>%
    tidyr::separate_rows(`Positive Hits`, sep = ",")
  comboResult_forNetwork <- comboResult_forNetwork %>%
    group_by(Source, Function, `Positive Hits`) %>%
    summarise(Tool = paste(unique(Tool), collapse = ", "), .groups = "drop")
  comboResult_forNetwork <- comboResult_forNetwork %>% 
    mutate(Rank = sapply(strsplit(Tool, ","), length))
  comboResult_forNetwork <-
    comboResult_forNetwork %>% filter(Rank >= input$combo_rank_slider)
  comboResult_forNetwork <- comboResult_forNetwork %>% arrange(desc(Rank))
  
  return(comboResult_forNetwork)
}

constructComboVisNetwork <- function(comboResult_forNetwork) {
  graph <- createComboGraph(comboResult_forNetwork)
  data <- toVisNetworkData(graph)
  
  nodes <- data$nodes
  row.names(nodes) <- NULL
  nodes$title <- nodes$label
  nodes$font.size <- 24
  nodes$group <-
    comboResult_forNetwork$Source[match(
      nodes$label, comboResult_forNetwork$Function)]
  nodes$group[is.na(nodes$group)] <- "Gene"
  
  edges <- data$edges
  
  layout <- names(LAYOUT_CHOICES)[match(
    input$combo_network_layout, LAYOUT_CHOICES)]
  
  renderShinyVisNetwork("combo_visNetwork", nodes, edges, layout)
}

createComboGraph <- function(comboResult_forNetwork) {
  graph <- igraph::graph_from_edgelist(
    as.matrix(comboResult_forNetwork[, c("Function", "Positive Hits")]),
    directed = FALSE
  )
  E(graph)$weight <- 1
  E(graph)$title <- comboResult_forNetwork$Tool
  return(graph)
}
