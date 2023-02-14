runWebgestalt <- function(userInputList) {
  datasources <- as.character(DATASOURCES_CODES[["WEBGESTALT"]][
    input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
  ])
  organism <- ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name
  # different short name between gprofiler and webgestalt
  if (organism == "clfamiliaris")
    organism <- "cfamiliaris"
  namespace <- 
    ifelse(input[[paste0(currentEnrichmentType, "_enrichment_namespace")]] == "USERINPUT",
           "genesymbol", "entrezgene")

  result <- suppressWarnings(WebGestaltR::WebGestaltR(
    organism = organism,
    enrichDatabase = datasources,
    interestGene = userInputList,
    interestGeneType = namespace,
    referenceSet = "genome",
    fdrMethod = currentSignificanceMetric,
    fdrThr = as.numeric(input$functional_enrichment_threshold),
    isOutput = F, # doesn't create extra load with folders
    hostName = "https://www.webgestalt.org/"
  ))

  if (isResultValid(result)) {
    webgestaltResult <- parseWebgestaltResult(result, length(userInputList))
    enrichmentResults[[currentType_Tool]] <<-
      transformEnrichmentResultTable(webgestaltResult)
    attachWebgestaltLinks(result$link)
  }
}

parseWebgestaltResult <- function(webgestaltResult, numInputs) {
  if (is.null(webgestaltResult$database))
    webgestaltResult$database <- as.character(DATASOURCES_CODES[["WEBGESTALT"]][
      input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
    ])
  webgestaltResult$database <- 
    unlistDatasourceCodes(webgestaltResult$database, DATASOURCES_CODES[["WEBGESTALT"]])
  if (is.null(webgestaltResult$userId)) # straight from entrezgene namespace
    webgestaltResult$userId <- webgestaltResult$overlapId
  webgestaltResult$userId <- gsub(";", ",", webgestaltResult$userId)
  webgestaltResult$querySize <- numInputs
  
  webgestaltResult <-
    webgestaltResult[, c(
      "database", "geneSet", "description", "pValue",
      "size", "querySize", "overlap", "userId")]
  colnames(webgestaltResult) <- ENRICHMENT_DF_COLNAMES
  webgestaltResult <- mapKEGGIds(webgestaltResult)
  return(webgestaltResult)
}