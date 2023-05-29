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
    
    choices <- unlist(strsplit(unique(combinationResult$Tools), split = ","))
    choices <- unique(trimws(choices))
    updatePickerInput(session, "combo_tool_picker",
                      choices = choices, selected = NULL) # trigger
    updatePickerInput(session, "combo_tool_picker",
                      choices = choices, selected = choices)
    
    maxRank <- max(combinationResult$Rank, na.rm = TRUE)
    updateSliderInput(session, "combo_rank_slider",
                      value = maxRank, max = maxRank)
    shinyjs::show("functional_enrichment_all_clear")
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
  combinationResult <<- combinationResult[, c("Source", "Term_ID", "Function", "Term_ID_noLinks", "Tool", "P-value")]
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
  combinedStatistics <- calculateCombinedStatistics()
  combinationResult <<- plyr::join(combinationResult, combinedStatistics, type = "left",
                                   by = "Term_ID_noLinks")
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
    if (areComboInputsNotEmpty()) {
      comboResult_forNetwork <- parseComboVisNetwork()

      if (nrow(comboResult_forNetwork) > 0) {
        constructComboVisNetwork(comboResult_forNetwork)
        renderShinyDataTable(
          shinyOutputId = "combo_network_table",
          comboResult_forNetwork,
          caption = "Combination Network",
          fileName = "combo_network")
      } else
        renderWarning("Filters resulted in empty table.")
    }
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with combinatorial network.")
  })
}

areComboInputsNotEmpty <- function() {
  isNotEmpty <- F
  if (!is.null(input$combo_datasources)){
    isNotEmpty <- T
  } else
    renderWarning("Select at least one datasource.")
  
  if (is.null(input$combo_tool_picker) || length(input$combo_tool_picker) < 2) {
    isNotEmpty <- F
    renderWarning("Select at least two tools.")
  }
  return(isNotEmpty)
}

parseComboVisNetwork <- function() {
  functionalEnrichmentResults <- enrichmentResults[grep("^functional",
                                                        names(enrichmentResults))]
  comboResult_forNetwork <- dplyr::bind_rows(functionalEnrichmentResults,
                                              .id = "Tool")
  comboResult_forNetwork <- comboResult_forNetwork[, c("Source", "Function",
                                                        "Positive Hits", "Tool")]

  comboResult_forNetwork <- subset(comboResult_forNetwork,
     Source %in% input$combo_datasources)

  comboResult_forNetwork$Tool <-
    sapply(strsplit(comboResult_forNetwork$Tool, "_"), "[[", 2)
  comboResult_forNetwork <- subset(comboResult_forNetwork,
    Tool %in% input$combo_tool_picker)
  
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

isToolNotNull <- function(sourceSelect) {
  isNotNull <- F
  if (!is.null(sourceSelect)){
    isNotNull <- T
  } else
    renderWarning("Select at least one datasource.")
  return(isNotNull)
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
  E(graph)$width <- comboResult_forNetwork$Rank
  return(graph)
}

calculateCombinedStatistics <- function() {
  functionalEnrichmentResults <- enrichmentResults[grep("^functional", names(enrichmentResults))]
  unique_terms <- unique(
    unlist(
      lapply(names(functionalEnrichmentResults), function(i) {
        id_nolinks <- unlist(functionalEnrichmentResults[[i]]["Term_ID_noLinks"])
        return(id_nolinks)
      })
    )
  )
  statistics <- data.frame(Term_ID_noLinks = unique_terms)
  chi_squared <- c()
  pvalues <- c()
  logpvals <- c()
  for (term in statistics$Term_ID_noLinks) {
    pvals <- c()
    for (i in names(functionalEnrichmentResults)) {
      if(term %in% unlist(functionalEnrichmentResults[[i]]["Term_ID_noLinks"])) {
        pval <- functionalEnrichmentResults[[i]][functionalEnrichmentResults[[i]]["Term_ID_noLinks"] == term,]["P-value"]
        pvals <- c(pvals, as.numeric(pval))
      }
    }
    if(length(pvals) > 1) {
      # X^2 with Fisher's method
      logps <- lapply(pvals, function(p) {
        return(log(p))
      })
      chi <- -2 * sum(unlist(logps))
      pvalue <- as.numeric(pchisq(chi, length(pvals), lower.tail = F)) # derive p-value from X^2
      logp <- -1 * log10(pvalue)
    }
    else {
      pvalue <- as.numeric(pvals[1])
      chi <- NA
      logp <- -1 * log10(pvalue)
    }
    chi_squared <- c(chi_squared, chi)
    pvalues <- c(pvalues, pvalue)
    logpvals <- c(logpvals, logp)
  }
  statistics$chi_squared <- formatC(chi_squared, format="g", digits = 3)
  statistics$pvalues <- formatC(pvalues, format="e", digits = 2)
  statistics$logpvals <- formatC(logpvals, format="e", digits = 3) 
  names(statistics) <- c("Term_ID_noLinks", "X<sup>2</sup>", "P-value", "-log10Pvalue")
  return(statistics)
}
