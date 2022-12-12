handleEnrichment <- function(enrichmentType) {
  tryCatch({
    if (existInputGeneLists()) {
      currentEnrichmentType <<- enrichmentType
      if (existInputOrganism()) {
        if (existEnrichmentTool()) {
          tools <- switch(
            currentEnrichmentType,
            "functional" = input$functional_enrichment_tool,
            "literature" = "aGOtool"
          )
          currentUserList <<-
            unlist(userInputLists[file_names ==
                                    input[[paste0(currentEnrichmentType,
                                                  "_enrichment_file")]]][[1]])
          currentOrganism <<- 
            ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$print_name ==
                                  input[[paste0(currentEnrichmentType,
                                                "_enrichment_organism")]], ]$gprofiler_ID
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
    renderWarning("First upload at least one gene list.")
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
    currentSignificanceMetric <<-
      eval(parse(text = paste0(toupper(currentEnrichmentTool), "_METRICS")))[[1]]
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
    resetEnrichmentResults()
    inputGenesConversionTable <- geneConvert()
    if (validInputGenesConversionTable(inputGenesConversionTable)) {
      runEnrichmentAnalysis(inputGenesConversionTable$target)
      if (validEnrichmentResult()) {
        printParameters()
        if (input[[paste0(currentEnrichmentType, "_enrichment_namespace")]] != "USERINPUT") {
          shinyjs::show(paste(currentType_Tool,
                              "conversionBoxes", sep = "_"))
          printUnconvertedGenes(inputGenesConversionTable$input)
          printConversionTable(inputGenesConversionTable)
        }
        findAndPrintNoHitGenes(inputGenesConversionTable$target)
        printResultTables()
        updatePlotControlPanels()
      }
    }
  }
}

existDataSources <- function() { # TODO check if need different condition for each tool during multiple
  exist <- F
  if (!is.null(input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]))
    exist <- T
  else
    renderWarning("Select at least one datasource.")
  return(exist)
}

geneConvert <- function() {
  target = input[[paste0(currentEnrichmentType, "_enrichment_namespace")]]
  if (target != "USERINPUT"){
    inputGenesConversionTable <- gprofiler2::gconvert(
      currentUserList,
      organism = currentOrganism,
      target = target
    )
    inputGenesConversionTable <- inputGenesConversionTable[inputGenesConversionTable$target != "nan", ] 
    inputGenesConversionTable <- inputGenesConversionTable[, c("input", "target", "name")]
  } else {
    inputGenesConversionTable <- data.frame(
      "input" = currentUserList,
      "target" = currentUserList,
      "name" = currentUserList
    )
  }
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

runEnrichmentAnalysis <- function(userInputList) {
  tool <- toupper(currentEnrichmentTool)
  if (tool == "AGOTOOL") {
    taxid <- 
      ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$gprofiler_ID == currentOrganism, ]$Taxonomy_ID
    runAGoTool(userInputList, taxid)
  } else if (tool == "GPROFILER") {
    runGprofiler(userInputList)
  } else { # TODO remove
    renderWarning(paste0(tool, " pipeline has not been coded yet."))
  }
}

validEnrichmentResult <- function() {
  valid <- F
  enrichmentResult <- getGlobalEnrichmentResult(currentEnrichmentType, currentEnrichmentTool)
  if (nrow(enrichmentResult) > 0)
    valid <- T
  else
    renderWarning(paste0(
      str_to_title(currentEnrichmentType), " enrichment with ",
      currentEnrichmentTool, " could not return any valid results."))
  return(valid)
}

getGlobalEnrichmentResult <- function(enrichmentType, toolName) {
  return(enrichmentResults[[paste(enrichmentType, toolName, sep = "_")]])
}

printParameters <- function() {
  parametersOutput <- paste0(
    "File: ", input[[paste0(currentEnrichmentType, "_enrichment_file")]],
    "\nOrganism: ", currentOrganism,
    "\nDatasources: ", decideToolSelectedDatasources(),
    "\nNamespace: ", input[[paste0(
      currentEnrichmentType, "_enrichment_namespace")]],
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
  toolDatasources <- eval(parse(text = paste0(toupper(currentEnrichmentTool), "_DATASOURCES")))
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
      "%d gene(s) could not be converted to the target namespace:\n%s",
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
      "%d converted gene(s) not found in any function:\n%s",
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
  enrichmentResults[[currentType_Tool]]$Term_ID_noLinks <<-
    enrichmentResults[[currentType_Tool]]$Term_ID
    
  switch(
    currentEnrichmentType,
    "functional" = {
      attachDBLinks()
    },
    "literature" = {
      attachLiteratureDBLinks()
    }
  )
}

printFunctionalResultTable <- function() {
  shinyOutputId <- paste(currentType_Tool, "table_all", sep = "_")
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "all")
  datasources <- input$functional_enrichment_datasources
  lapply(datasources, function(datasource) {
    partialId <- switch(
      datasource,
      "INTERPRO" = "interpro",
      "PFAM" = "pfam",
      "UNIPROT" = "uniprot",
      "GO:MF" = "gomf",
      "GO:CC" = "gocc",
      "GO:BP" = "gobp",
      "KEGG" = "kegg",
      "REAC" = "reac",
      "WP" = "wp",
      "DO" = "do",
      "BTO" = "brenda",
      "TF" = "tf",
      "MIRNA" = "mirna",
      "CORUM" = "corum",
      "HPA" = "hpa",
      "HP" = "hp"
    )
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
