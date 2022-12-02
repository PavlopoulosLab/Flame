handleFunctionalEnrichment <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Executing functional enrichment.</p>")
    resetGprofilerResults()
    if (existinputGeneLists()) {
      if (existDataSources()) {
        session$sendCustomMessage("handler_disableSourcesTabs", "gprofiler")
        if (isInputOrganismNotEmpty()) {
          organism <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$print_name == input$organism, ]$gprofiler_ID
          inputGenes <- unlist(inputGeneLists[file_names == input$selectEnrichFile][[1]])
          inputGenesConversionTable <- geneConvert(inputGenes, organism)
          
          if (validInputGenesConversionTable(inputGenesConversionTable)){
            gprofilerResult <<- runGprofiler(inputGenesConversionTable$target, organism)
            if (validGprofilerResult(gprofilerResult)) {
              printParameters(organism)
              if (input$gconvertTargetGprofiler != "USERINPUT"){
                shinyjs::show("conversionBoxes")
                printUnconvertedGenes(inputGenes, inputGenesConversionTable$input)
                printConversionTable(inputGenesConversionTable)
              }
              gprofilerTransformedResult <<- transformResultTable()
              findAndPrintNoHitGenes(inputGenesConversionTable$target)
              printResultTables(organism)
              updatePlotControlPanels()
            }
          }
        }
      }
    }
  }, error = function(e) {
    cat(paste("gprofiler error:  ", e))
    renderError("Error during enrichment. Try again in a while.")
  }, finally = {
    removeModal()
  })
}

existinputGeneLists <- function() {
  exist <- F
  if (length(inputGeneLists) > 0)
    exist <- T
  else
    renderWarning("First upload at least one gene list.")
  return(exist)
}

existDataSources <- function() {
  exist <- F
  if (!is.null(input$datasources))
    exist <- T
  else
    renderWarning("Select at least one data source.")
  return(exist)
}

isInputOrganismNotEmpty <- function() {
  notEmpty <- F
  if (input$organism != "")
    notEmpty <- T
  else
    renderWarning("Select an organism from the list.")
  return(notEmpty)
}

geneConvert <- function(inputGenes, organism) {
  target = input$gconvertTargetGprofiler
  if (target != "USERINPUT"){
    inputGenesConversionTable <- gconvert(
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

runGprofiler <- function(genes, organism){
  gprofilerResult <- gost(genes,
                          source = input$datasources,
                          evcodes = T,
                          organism = organism,
                          correction_method = input$threshold,
                          user_threshold = input$user_pvalue
  )
  return(gprofilerResult)
}

# invalid result test gene LGALS3
validGprofilerResult <- function(gprofilerResult) {
  valid <- F
  if (!is.null(gprofilerResult))
    valid <- T
  else
    renderWarning("Functional enrichment could not return any valid resutls.")
  return(valid)
}

printParameters <- function(organism) {
  parametersOutput <- paste0(
    "File: ", input$selectEnrichFile,
    "\nOrganism: ", organism,
    "\nCorrection Method: ", input$threshold, 
    "\nUser threshold: ", input$user_pvalue,
    "\nID type Output: ", input$gconvertTargetGprofiler,
    "\nDatabases: ")
  parametersOutput <- paste0(parametersOutput,
                             paste(input$datasources,
                                   collapse=', ')
  )
  output$gprofParameters <- renderText(parametersOutput)
}

printUnconvertedGenes <- function(inputGenes, convertedInputs) {
  shinyOutputId <- "notConverted"
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
  shinyOutputId <- "conversionTable"
  fileName <- "conversion_table"
  colnames(conversionTable) <- c("Input", "Target", "Name")
  renderShinyDataTable(shinyOutputId, conversionTable,
                       fileName = fileName)
}

transformResultTable <- function() {
  gprofilerTransformedResult <- gprofilerResult$result[, c(
    "source", "term_id", "term_name", "p_value", "term_size",
    "query_size", "intersection_size", "intersection"
  )]
  colnames(gprofilerTransformedResult) <- c(
    "Source", "Term_ID", "Function", "P-value", "Term Size",
    "Query size", "Intersection Size", "Positive Hits"
  )
  gprofilerTransformedResult$`-log10Pvalue` <- as.numeric(
    format(
      -log10(gprofilerTransformedResult$`P-value`),
      format = "e", digits = 3
    )
  )
  gprofilerTransformedResult$`P-value` <- formatC(
    gprofilerTransformedResult$`P-value`, format = "e", digits = 2
  )
  gprofilerTransformedResult$`Enrichment Score %` <- calculateEnrichmentScore(
    gprofilerTransformedResult$`Intersection Size`,
    gprofilerTransformedResult$`Term Size`
  )
  gprofilerTransformedResult <- gprofilerTransformedResult[, c(
    "Source", "Term_ID", "Function", "P-value", "-log10Pvalue",
    "Term Size", "Query size", "Intersection Size",
    "Enrichment Score %", "Positive Hits"
  )]
  return(gprofilerTransformedResult)
}

calculateEnrichmentScore <- function(hitGenesCount, databaseGenesCount) {
  enrichmentScore <- round((hitGenesCount / databaseGenesCount) * 100, 2)
  return(enrichmentScore)
}

findAndPrintNoHitGenes <- function(convertedInputs) {
  noHitGenes <- findNoHitGenes(convertedInputs)
  printNoHitGenes(noHitGenes)
}

findNoHitGenes <- function(convertedInputs) {
  allHitGenes <- paste(gprofilerTransformedResult$`Positive Hits`, collapse = ",")
  allHitGenes <- strsplit(allHitGenes, ",")[[1]]
  allHitGenes <- unique(allHitGenes)
  noHitGenes <- convertedInputs[!convertedInputs %in% allHitGenes]
  return(noHitGenes)
}

printNoHitGenes <- function(noHitGenes) {
  shinyOutputId <- "genesNotFound"
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
  datasources <- input$datasources
  shinyOutputId <- "table_all"
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "all")
  lapply(datasources, function(datasource) {
    partialId <- switch(
      datasource,
      "GO:MF" = "gomf",
      "GO:CC" = "gocc",
      "GO:BP" = "gobp",
      "KEGG" = "kegg",
      "REAC" = "reac",
      "WP" = "wp",
      "TF" = "tf",
      "MIRNA" = "mirna",
      "CORUM" = "corum",
      "HPA" = "hpa",
      "HP" = "hp"
    )
    shinyOutputId <- paste0("table_", partialId)
    tabPosition <- match(datasource, GPROFILER_DATASOURCES)
    printResultTable(shinyOutputId, tabPosition, datasource)
  })
}

formatResultTable <- function(organism) {
  gprofilerTransformedResult <<-
    gprofilerTransformedResult[order(-gprofilerTransformedResult$`Enrichment Score %`), ]
  # TODO remove after refactor plots.R
  all_gost <<- gprofilerTransformedResult
  gprofilerTransformedResult$Term_ID_noLinks <<-
    gprofilerTransformedResult$Term_ID
  attachDBLinks(organism)
}

printResultTable <- function(shinyOutputId, tabPosition, datasource) {
  if (datasource == "all")
    gprofilerTransformedResultPartial <- gprofilerTransformedResult
  else
    gprofilerTransformedResultPartial <-
      gprofilerTransformedResult[grepl(paste0("^", datasource, "$"),
                                       gprofilerTransformedResult$Source),]
  
  if (nrow(gprofilerTransformedResultPartial) > 0) {
    gprofilerTransformedResultPartial$`Positive Hits` <-
      gsub(",", ", ", gprofilerTransformedResultPartial$`Positive Hits`)
    session$sendCustomMessage("handler_enableSourceTab",
                              list(toolName = "gprofiler", tabPosition = tabPosition))
    caption = "Enrichment Results"
    fileName <- paste0(input$selectEnrichFile, "_all_enriched_gprofiler")
    mode <- "Positive Hits"
    hiddenColumns <- c(0, 11, 12)
    expandableColumn <- 11
    renderEnrichmentTable(shinyOutputId, gprofilerTransformedResultPartial,
                          caption, fileName, mode,
                          hiddenColumns, expandableColumn)
  }
}
