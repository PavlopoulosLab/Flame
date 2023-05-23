handleEnrichment <- function(enrichmentType) {
  tryCatch({
    if (existInputGeneLists()) {
      currentEnrichmentType <<- enrichmentType
      if (existInputOrganism()) {
        if (existEnrichmentTool()) {
          resetCombination()
          
          tools <- switch(
            currentEnrichmentType,
            "functional" = input$functional_enrichment_tool,
            "literature" = "aGOtool"
          )
          currentUserList <<-
            unlist(userInputLists[names(userInputLists) ==
                                    input[[paste0(currentEnrichmentType,
                                                  "_enrichment_file")]]][[1]])
          currentOrganism <<- 
            ORGANISMS[ORGANISMS$print_name ==
                                  input[[paste0(currentEnrichmentType,
                                                "_enrichment_organism")]], ]$taxid
          
          if(input[[paste0(currentEnrichmentType, "_enrichment_background_choice")]] == "genome") {
            currentBackgroundList <<- c()
          }
          else {
            currentBackgroundList <<-  unlist(userInputLists[names(userInputLists) ==
                                    input[[paste0(currentEnrichmentType,
                                                  "_enrichment_background_list")]]][[1]])
          }
          
          lapply(tools, function(toolName) {
            currentEnrichmentTool <<- toolName
            currentType_Tool <<-
              paste(currentEnrichmentType, currentEnrichmentTool, sep = "_")
            currentSignificanceMetric <<- decideToolMetric()
            start_time <- proc.time()
            handleEnrichmentWithTool()
            end_time <- proc.time()
            print((end_time - start_time)[3])
          })
          
          prepareCombinationTab()
        }
      }
    }
  }, error = function(e) {
    cat(paste("Functional enrichment analysis error:  ", e))
    renderError("Error during enrichment. Try again in a while.")
  }, finally = {
    removeModal()
  })
}

existInputGeneLists <- function() {
  exist <- F
  if (length(userInputLists) > 0)
    exist <- T
  else
    renderWarning("Upload an input list first.")
  return(exist)
}

existInputOrganism <- function() {
  notEmpty <- F
  if (input[[paste0(currentEnrichmentType, "_enrichment_organism")]] != "")
    notEmpty <- T
  else
    renderWarning("Select an organism from the list.")
  return(notEmpty)
}

existEnrichmentTool <- function() {
  exist <- F
  if (!is.null(input[[paste0(currentEnrichmentType, "_enrichment_tool")]]))
    exist <- T
  else
    renderWarning("Select at least one enrichment tool.")
  return(exist)
}

decideToolMetric <- function() {
  if (input[[paste0(currentEnrichmentType, "_enrichment_metric")]] == DEFAULT_METRIC_TEXT)
    currentSignificanceMetric <<- METRICS[[toupper(currentEnrichmentTool)]][[1]]
  else
    currentSignificanceMetric <<-
      input[[paste0(currentEnrichmentType, "_enrichment_metric")]]
}

handleEnrichmentWithTool <- function() {
  renderModal(
    paste0(
      "<h2>Please wait.</h2><br />
      <p>Executing ", currentEnrichmentType, " enrichment
      with ", currentEnrichmentTool, ".</p>"
    )
  )
  if (existDataSources()) {
    session$sendCustomMessage("handler_hideSourceTabs", currentType_Tool)
    resetEnrichmentResults(currentEnrichmentType, currentEnrichmentTool)
    inputGenesConversionTable <- geneConvert(currentUserList)
    if (validInputGenesConversionTable(inputGenesConversionTable)) {
      if(length(currentBackgroundList)==0)
        backgroundGenesConversionTable <- NULL
      else
        backgroundGenesConversionTable <- geneConvert(currentBackgroundList)
      runEnrichmentAnalysis(inputGenesConversionTable$target, backgroundGenesConversionTable$target)
      if (validEnrichmentResult()) {
        showTab(inputId = "toolTabsPanel", target = currentEnrichmentTool)
        noHitGenesCheckList <- executeNamespaceRollback(inputGenesConversionTable)
        printParameters()
        if (input[[paste0(currentEnrichmentType, "_enrichment_namespace")]] != "USERINPUT") {
          shinyjs::show(paste(currentType_Tool,
                              "conversionBoxes", sep = "_"))
          printUnconvertedGenes(inputGenesConversionTable$input)
          printConversionTable(inputGenesConversionTable)
        }
        findAndPrintNoHitGenes(noHitGenesCheckList)
        printResultTables()
        updatePlotControlPanels()
        updateTabsetPanel(session, "toolTabsPanel", selected = currentEnrichmentTool)
      }
    }
  }
}

existDataSources <- function() {
  exist <- F
  if (!is.null(input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]))
    exist <- T
  else
    renderWarning("Select at least one datasource.")
  return(exist)
}

geneConvert <- function(geneList) {
  currentNamespace <<- input[[paste0(currentEnrichmentType, "_enrichment_namespace")]]
  if (currentNamespace == DEFAULT_NAMESPACE_TEXT)
    currentNamespace <<- getDefaultTargetNamespace()
  if (currentNamespace != "USERINPUT") {
    if (currentEnrichmentTool == "aGOtool")
      inputGenesConversionTable <- stringPOSTConvertENSP(geneList,
                                                         currentOrganism)
    else
      inputGenesConversionTable <- gProfilerConvert(geneList, currentNamespace)
  } else {
    inputGenesConversionTable <- data.frame(
      "input" = geneList,
      "target" = geneList,
      "name" = geneList
    )
  }
  return(inputGenesConversionTable)
}

getDefaultTargetNamespace <- function() {
  shortName <- ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name
  switch(
    currentEnrichmentTool,
    "aGOtool" = "ENSP",
    "gProfiler" = "USERINPUT",
    "WebGestalt" = "ENTREZGENE_ACC",
    "enrichR" = {
      if (shortName == "scerevisiae" || shortName == "dmelanogaster")
        "USERINPUT"
      else
        "ENTREZGENE"
    }
  )
}

stringPOSTConvertENSP <- function(userList, organism) {
  url <- "https://string-db.org/api/json/get_string_ids"
  params <- list(
    "identifiers" = paste0(userList, collapse = "%0d"),
    "species" = organism
  )
  request <- httr::POST(url, body = params)
  if (isPOSTResponseValid(request)) {
    inputGenesConversionTable <- jsonlite::fromJSON(
      rawToChar(httr::content(request, "raw")))
    inputGenesConversionTable <-
      inputGenesConversionTable[, c("queryItem", "stringId", "preferredName")]
    colnames(inputGenesConversionTable) <- c("input", "target", "name")
  } else
    inputGenesConversionTable <- NULL
  return(inputGenesConversionTable)
}

gProfilerConvert <- function(geneList, target) {
  inputGenesConversionTable <- gprofiler2::gconvert(
    geneList,
    organism = ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name,
    target = target, mthreshold = 1, filter_na = T
  )
  inputGenesConversionTable <- inputGenesConversionTable[, c("input", "target", "name")]
  return(inputGenesConversionTable)
}

validInputGenesConversionTable <- function(inputGenesConversionTable) {
  valid <- F
  if (!is.null(inputGenesConversionTable))
    valid <- T
  else
    renderWarning("No valid genes for analysis found.")
  return(valid)
}

runEnrichmentAnalysis <- function(userInputList, user_reference = NULL) {
  tool <- toupper(currentEnrichmentTool)
  if (tool == "AGOTOOL") {
    runAGoTool(userInputList, currentOrganism, user_reference)
  } else if (tool == "GPROFILER") {
    runGprofiler(userInputList, user_reference)
  } else if (tool == "WEBGESTALT") {
    runWebgestalt(userInputList, user_reference)
  } else if (tool == "ENRICHR") {
    runEnrichr(userInputList)
  }
}

validEnrichmentResult <- function() {
  valid <- F
  enrichmentResult <- getGlobalEnrichmentResult(currentEnrichmentType,
                                                currentEnrichmentTool)
  if (nrow(enrichmentResult) > 0)
    valid <- T
  else {
    renderWarning(paste0(
      stringr::str_to_title(currentEnrichmentType), " enrichment with ",
      currentEnrichmentTool, " could not return any valid results.")) 
    hideTab(inputId = "toolTabsPanel", target = currentEnrichmentTool)
  }
  return(valid)
}

getGlobalEnrichmentResult <- function(enrichmentType, toolName) {
  return(enrichmentResults[[paste(enrichmentType, toolName, sep = "_")]])
}

executeNamespaceRollback <- function(inputGenesConversionTable) {
  rollBackNamesFlag <- ifelse(input[[paste0(currentEnrichmentType,
                                            "_enrichment_inputConversion")]] ==
                                "Original input names", T, F)
  if (rollBackNamesFlag) {
    enrichmentResults[[currentType_Tool]] <<-
      rollBackConvertedNames(enrichmentResults[[currentType_Tool]],
                             inputGenesConversionTable)
    noHitGenesCheckList <- currentUserList
  } else
    noHitGenesCheckList <- inputGenesConversionTable$target
  return(noHitGenesCheckList)
}

rollBackConvertedNames <- function(enrichmentOutput, inputGenesConversionTable) {
  enrichmentOutput <- tidyr::separate_rows(enrichmentOutput,
                                           `Positive Hits`, sep = ",")
  enrichmentOutput <- merge(enrichmentOutput, inputGenesConversionTable,
                            by.x = "Positive Hits", by.y = "target")
  enrichmentOutput <-
    enrichmentOutput[, !(names(enrichmentOutput) %in% c("Positive Hits", "name"))]
  colnames(enrichmentOutput)[match("input", colnames(enrichmentOutput))] <-
    "Positive Hits"
  enrichmentOutput <- enrichmentOutput %>% group_by(Term_ID) %>%
    mutate(`Positive Hits` = paste(`Positive Hits`, collapse = ","))
  if (currentEnrichmentTool == "WebGestalt") # Term_ID_noLinks already generated
    enrichmentOutput <-
    enrichmentOutput[, c("Source", "Term_ID", "Function",  "P-value",
                         "-log10Pvalue", "Term Size", "Query size",
                         "Intersection Size", "Enrichment Score %",
                         "Positive Hits", "Term_ID_noLinks")]
  return(as.data.frame(distinct(enrichmentOutput)))
}

printParameters <- function() {
  parametersOutput <- paste0(
    "File: ", input[[paste0(currentEnrichmentType, "_enrichment_file")]],
    "\nOrganism: ", ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$print_name,
    "\nBackground: ", input[[
      paste0(currentEnrichmentType, "_enrichment_background_choice")]],
    "\nDatasources: ", decideToolSelectedDatasources(),
    "\nNamespace: ", currentNamespace,
    "\nSignificance metric: ", currentSignificanceMetric, 
    "\nSignificance threshold: ", input[[
      paste0(currentEnrichmentType, "_enrichment_threshold")]]
  )
  renderShinyText(paste(
    currentType_Tool, "enrichment_parameters", sep = "_"), parametersOutput)
}

decideToolSelectedDatasources <- function() {
  inputSelectedDatasources <-
    input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
  prefix <- ""
  if (currentEnrichmentTool == "enrichR")
    prefix <- getEnrichrVariablePrefix()
  toolDatasources <- DATASOURCES[[paste0(prefix, toupper(currentEnrichmentTool))]]
  inputSelectedDatasources <-
    inputSelectedDatasources[inputSelectedDatasources %in% toolDatasources]
  inputSelectedDatasources <- paste(inputSelectedDatasources, collapse = ", ")
  return(inputSelectedDatasources)
}

printUnconvertedGenes <- function(convertedInputs) {
  shinyOutputId <- paste(currentType_Tool,
                         "notConverted", sep = "_")
  unconvertedInputs <- currentUserList[!currentUserList %in% convertedInputs]
  unconvertedInputsCount <- length(unconvertedInputs)
  if (unconvertedInputsCount > 0) {
    unconvertedInputs <- paste(unconvertedInputs, collapse=", ")
    prompt <- sprintf(
      "%d input item(s) could not be converted to the target namespace:\n%s",
      unconvertedInputsCount,
      unconvertedInputs
    )
    renderShinyText(shinyOutputId, prompt)
  } else
    renderShinyText(shinyOutputId, "-")
}

printConversionTable <- function(conversionTable) {
  shinyOutputId <- paste(currentType_Tool, "conversionTable", sep = "_")
  fileName <- paste(currentType_Tool, "conversion_table", sep = "_")
  colnames(conversionTable) <- c("Input", "Target", "Name")
  renderShinyDataTable(shinyOutputId, conversionTable,
                       fileName = fileName)
}

findAndPrintNoHitGenes <- function(convertedInputs) {
  noHitGenes <- findNoHitGenes(convertedInputs)
  printNoHitGenes(noHitGenes)
}

findNoHitGenes <- function(convertedInputs) {
  enrichmentResult <- getGlobalEnrichmentResult(currentEnrichmentType, currentEnrichmentTool)
  allHitGenes <- paste(enrichmentResult$`Positive Hits`, collapse = ",")
  allHitGenes <- strsplit(allHitGenes, ",")[[1]]
  allHitGenes <- unique(allHitGenes)
  noHitGenes <- convertedInputs[!convertedInputs %in% allHitGenes]
  return(noHitGenes)
}

printNoHitGenes <- function(noHitGenes) {
  shinyOutputId <- paste(currentType_Tool, "genesNotFound", sep = "_")
  noHitGenesCount <- length(noHitGenes)
  if (noHitGenesCount > 0) {
    noHitGenes <- paste(noHitGenes, collapse=", ")
    prompt <- sprintf(
      "%d input item(s) not found in any result term:\n%s",
      noHitGenesCount,
      noHitGenes
    )
    renderShinyText(shinyOutputId, prompt)
  } else
    renderShinyText(shinyOutputId, "-")
}

printResultTables <- function() {
  formatResultTable()
  switch(
    currentEnrichmentType,
    "functional" = printFunctionalResultTable(),
    "literature" = printLiteratureResultTable()
  )
}

formatResultTable <- function() {
  enrichmentResults[[currentType_Tool]] <<-
    enrichmentResults[[currentType_Tool]][order(
      -enrichmentResults[[currentType_Tool]]$`-log10Pvalue`), ]
  
  if (is.null(enrichmentResults[[currentType_Tool]]$Term_ID_noLinks)) { # if no links returned from tool
    enrichmentResults[[currentType_Tool]]$Term_ID_noLinks <<-
      enrichmentResults[[currentType_Tool]]$Term_ID
    
    switch(
      currentEnrichmentType,
      "functional" = attachDBLinks(),
      "literature" = attachLinks("PUBMED", "https://pubmed.ncbi.nlm.nih.gov/", gSub = "PMID:")
    )
  }
}

printFunctionalResultTable <- function() {
  shinyOutputId <- paste(currentType_Tool, "table_all", sep = "_")
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "all")
  datasources <- input$functional_enrichment_datasources
  lapply(datasources, function(datasource) {
    partialId <- as.character(TAB_NAMES[datasource])
    shinyOutputId <- paste(currentType_Tool, "table", partialId, sep = "_")
    tabPosition <- match(datasource, ENRICHMENT_DATASOURCES)
    printResultTable(shinyOutputId, tabPosition, datasource)
  })
}

printResultTable <- function(shinyOutputId, tabPosition, datasource) {
  if (datasource == "all" || datasource == "pubmed")
    transformedResultPartial <- enrichmentResults[[currentType_Tool]]
  else
    transformedResultPartial <-
      enrichmentResults[[currentType_Tool]][grepl(
        paste0("^", datasource, "$"),
        enrichmentResults[[currentType_Tool]]$Source), ]
  
  if (nrow(transformedResultPartial) > 0) {
    transformedResultPartial$`Positive Hits` <-
      gsub(",", ", ", transformedResultPartial$`Positive Hits`)
    session$sendCustomMessage("handler_showSourceTab",
                              list(prefix = currentType_Tool,
                                   tabPosition = tabPosition))
    caption = "Enrichment Results"
    fileName <- paste(currentType_Tool, datasource, sep = "_")
    mode <- "Positive Hits"
    hiddenColumns <- c(0, 11, 12)
    expandableColumn <- 11
    renderEnrichmentTable(shinyOutputId, transformedResultPartial,
                          caption, fileName, mode,
                          hiddenColumns, expandableColumn)
  }
}

printLiteratureResultTable <- function() {
  shinyOutputId <- paste(currentType_Tool, "table_pubmed", sep = "_")
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "pubmed")
}

handleEnrichmentResultClear <- function(enrichmentType, toolName) {
  resetCombination()
  resetEnrichmentResults(enrichmentType, toolName)
  hideTab(inputId = "toolTabsPanel", target = toolName)
  prepareCombinationTab()
}
