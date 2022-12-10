handleEnrichment <- function() {
  tryCatch({
    renderModal(paste0("<h2>Please wait.</h2><br /><p>Executing ", currentEnrichmentType, " enrichment.</p>"))
    if (existInputGeneLists()) {
      if (isInputOrganismNotEmpty()) {
        if (existDataSources()) {
          session$sendCustomMessage("handler_hideSourceTabs", currentEnrichmentType)
          resetEnrichmentResults()
          inputGenes <-
            unlist(inputGeneLists[file_names ==
                               input[[paste0(currentEnrichmentType,
                                 "_enrichment_file")]]][[1]])
          organism <- 
            ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$print_name ==
                                  input[[paste0(currentEnrichmentType,
                                    "_enrichment_organism")]], ]$gprofiler_ID
          inputGenesConversionTable <- geneConvert(inputGenes, organism)
          if (validInputGenesConversionTable(inputGenesConversionTable)) {
            runEnrichmentAnalysis(inputGenesConversionTable$target, organism)
            if (validEnrichmentResult()) {
              printParameters(organism)
              if (input[[paste0(currentEnrichmentType, "_enrichment_namespace")]] != "USERINPUT") {
                shinyjs::show(paste0(currentEnrichmentType, "_conversionBoxes"))
                printUnconvertedGenes(inputGenes, inputGenesConversionTable$input)
                printConversionTable(inputGenesConversionTable)
              }
              findAndPrintNoHitGenes(inputGenesConversionTable$target)
              printResultTables(organism)
              updatePlotControlPanels()
            }
          }
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
  if (length(inputGeneLists) > 0)
    exist <- T
  else
    renderWarning("First upload at least one gene list.")
  return(exist)
}

isInputOrganismNotEmpty <- function() {
  notEmpty <- F
  if (input[[paste0(currentEnrichmentType, "_enrichment_organism")]] != "")
    notEmpty <- T
  else
    renderWarning("Select an organism from the list.")
  return(notEmpty)
}

existDataSources <- function() {
  exist <- F
  if (!is.null(input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]))
    exist <- T
  else
    renderWarning("Select at least one datasource.")
  return(exist)
}

geneConvert <- function(inputGenes, organism) {
  target = input[[paste0(currentEnrichmentType, "_enrichment_namespace")]]
  if (target != "USERINPUT"){
    inputGenesConversionTable <- gprofiler2::gconvert(
      inputGenes,
      organism = organism,
      target = target
    )
    inputGenesConversionTable <- inputGenesConversionTable[inputGenesConversionTable$target != "nan", ] 
    inputGenesConversionTable <- inputGenesConversionTable[, c("input", "target", "name")]
  } else {
    inputGenesConversionTable <- data.frame(
      "input" = inputGenes,
      "target" = inputGenes,
      "name" = inputGenes
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

runEnrichmentAnalysis <- function(userInputList, organism) {
  tool <- toupper(input[[paste0(currentEnrichmentType, "_enrichment_tool")]])
  if (tool == "AGOTOOL") {
    taxid <- 
      ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$gprofiler_ID == organism, ]$Taxonomy_ID
    runAGoTool(userInputList, taxid)
  } else if (tool == "GPROFILER") {
    runGprofiler(userInputList, organism)
  } else { # TODO remove
    renderWarning(paste0(tool, " pipeline has not been coded yet."))
  }
}

validEnrichmentResult <- function() {
  valid <- F
  enrichmentResult <- switchGlobalEnrichmentResult(currentEnrichmentType)
  if (nrow(enrichmentResult) > 0)
    valid <- T
  else
    renderWarning(paste0(str_to_title(currentEnrichmentType),
                         " enrichment could not return any valid results."))
  return(valid)
}

switchGlobalEnrichmentResult <- function(enrichmentType) {
  switch(
    enrichmentType,
    "functional" = functionalEnrichmentResult,
    "literature" = literatureEnrichmentResult
  )
}

printParameters <- function(organism) {
  parametersOutput <- paste0(
    "File: ", input[[paste0(currentEnrichmentType, "_enrichment_file")]],
    "\nOrganism: ", organism,
    "\nCorrection Method: ", input[[paste0(currentEnrichmentType, "_enrichment_metric")]], 
    "\nUser threshold: ", input[[paste0(currentEnrichmentType, "_enrichment_threshold")]],
    "\nID type Output: ", input[[paste0(currentEnrichmentType, "_enrichment_namespace")]],
    "\nDatabases: ")
  parametersOutput <- paste0(parametersOutput,
                             paste(input[[paste0(currentEnrichmentType, "_enrichment_datasources")]],
                                   collapse=', ')
  )
  renderShinyText(paste0(currentEnrichmentType, "_enrichment_parameters"),
                  parametersOutput)
}

printUnconvertedGenes <- function(inputGenes, convertedInputs) {
  shinyOutputId <- paste0(currentEnrichmentType, "_notConverted")
  unconvertedInputs <- inputGenes[!inputGenes %in% convertedInputs]
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
  shinyOutputId <- paste0(currentEnrichmentType, "_conversionTable")
  fileName <- "conversion_table"
  colnames(conversionTable) <- c("Input", "Target", "Name")
  renderShinyDataTable(shinyOutputId, conversionTable,
                       fileName = fileName)
}

findAndPrintNoHitGenes <- function(convertedInputs) {
  noHitGenes <- findNoHitGenes(convertedInputs)
  printNoHitGenes(noHitGenes)
}

findNoHitGenes <- function(convertedInputs) {
  enrichmentResult <- switchGlobalEnrichmentResult(currentEnrichmentType)
  allHitGenes <- paste(enrichmentResult$`Positive Hits`, collapse = ",")
  allHitGenes <- strsplit(allHitGenes, ",")[[1]]
  allHitGenes <- unique(allHitGenes)
  noHitGenes <- convertedInputs[!convertedInputs %in% allHitGenes]
  return(noHitGenes)
}

printNoHitGenes <- function(noHitGenes) {
  shinyOutputId <- paste0(currentEnrichmentType, "_genesNotFound")
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

printResultTables <- function(organism) {
  formatResultTable(organism)
  enrichmentResult <- switch(
    currentEnrichmentType,
    "functional" = printFunctionalResultTable(),
    "literature" = printLiteratureResultTable()
  )
}

formatResultTable <- function(organism) {
  enrichmentResult <- switch(
    currentEnrichmentType,
    "functional" = {
      functionalEnrichmentResult <<-
        functionalEnrichmentResult[order(-functionalEnrichmentResult$`-log10Pvalue`), ]
      functionalEnrichmentResult$Term_ID_noLinks <<-
        functionalEnrichmentResult$Term_ID
      attachDBLinks(organism)
    },
    "literature" = {
      literatureEnrichmentResult <<-
        literatureEnrichmentResult[order(-literatureEnrichmentResult$`-log10Pvalue`), ]
      literatureEnrichmentResult$Term_ID_noLinks <<-
        literatureEnrichmentResult$Term_ID
      attachLiteratureDBLinks()
    }
  )
}

printFunctionalResultTable <- function() {
  shinyOutputId <- "functional_table_all"
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
    shinyOutputId <- paste0("functional_table_", partialId)
    tabPosition <- match(datasource, ENRICHMENT_DATASOURCES)
    printResultTable(shinyOutputId, tabPosition, datasource)
  })
}

printResultTable <- function(shinyOutputId, tabPosition, datasource) {
  if (datasource == "all")
    transformedResultPartial <- functionalEnrichmentResult
  else if (datasource == "pubmed")
    transformedResultPartial <- literatureEnrichmentResult
  else
    transformedResultPartial <-
      functionalEnrichmentResult[grepl(paste0("^", datasource, "$"),
                                       functionalEnrichmentResult$Source),]
  
  if (nrow(transformedResultPartial) > 0) {
    transformedResultPartial$`Positive Hits` <-
      gsub(",", ", ", transformedResultPartial$`Positive Hits`)
    session$sendCustomMessage("handler_showSourceTab",
                              list(prefix = currentEnrichmentType,
                                   tabPosition = tabPosition))
    caption = "Enrichment Results"
    fileName <- paste0(input[[paste0(currentEnrichmentType,
                                     "_enrichment_file")]], "_all_enriched")
    mode <- "Positive Hits"
    hiddenColumns <- c(0, 11, 12)
    expandableColumn <- 11
    renderEnrichmentTable(shinyOutputId, transformedResultPartial,
                          caption, fileName, mode,
                          hiddenColumns, expandableColumn)
  }
}

printLiteratureResultTable <- function() {
  shinyOutputId <- "literature_table_pubmed"
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "pubmed")
}
