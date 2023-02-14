runAGoTool <- function(userInputList, taxid) {
  requestBody <- buildAGoToolRequestBody(userInputList, taxid)
  response <- sendAGoToolPOSTRequest(requestBody)
  if (isResponseValid(response)) {
    aGoToolParsedResult <- parseAGoToolResult(response, taxid)
    if (isResultValid(aGoToolParsedResult))
      enrichmentResults[[currentType_Tool]] <<-
        transformEnrichmentResultTable(aGoToolParsedResult)
  }
}

buildAGoToolRequestBody <- function(userInputList, taxid) {
  if (currentSignificanceMetric == "False discovery rate") {
    FDR_cutoff <- input[[paste0(currentEnrichmentType, "_enrichment_threshold")]]
    p_value_cutoff <- 1
  } else {
    FDR_cutoff <- 1
    p_value_cutoff <- input[[paste0(currentEnrichmentType, "_enrichment_threshold")]]
  }
  limit_2_entity_type <- 
    paste0(
      DATASOURCES_CODES[["AGOTOOL"]][
        input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
      ], collapse = ";"
    )
  foreground <- paste0(userInputList, collapse = "%0d")
  enrichment_method <- "genome"
  requestBody <- list(
    taxid = taxid,
    FDR_cutoff = FDR_cutoff,
    limit_2_entity_type = limit_2_entity_type,
    foreground = foreground,
    enrichment_method = enrichment_method,
    p_value_cutoff = p_value_cutoff
  )
  return(requestBody)
}

sendAGoToolPOSTRequest <- function(requestBody) {
  response <- httr::POST(AGOTOOL_API_LINK, body = requestBody, encode = "json")
  return(response)
}

isResponseValid <- function(response) {
  isValid <- T
  if (response$status_code != 200) {
    isValid <- F
    renderWarning("Connection to aGoTool could not be established.
                  Please try again later.")
  }
  return(isValid)
}

parseAGoToolResult <- function(response, taxid) {
  responseBody <- rawToChar(httr::content(response, "raw"))
  aGoToolResult <- read.delim(text = responseBody)
  significanceColumnName <- switch(
    currentSignificanceMetric,
    "False discovery rate" = "FDR",
    "P-value" = "p_value",
    DEFAULT_METRIC_TEXT = "FDR"
  )
  aGoToolResult <-
    aGoToolResult[, c(
      "category", "term", "description", significanceColumnName,
      "background_count", "foreground_n", "foreground_count", "foreground_ids")]
  colnames(aGoToolResult) <- ENRICHMENT_DF_COLNAMES
  aGoToolResult <- parseAGoToolPositiveHits(aGoToolResult, taxid)
  if (currentEnrichmentType == "functional") {
    aGoToolResult <- alterSourceKeywords(aGoToolResult)
    aGoToolResult <- updateReactomeTerms(aGoToolResult)
  } else
    aGoToolResult <- alterLiteratureKeyword(aGoToolResult)
  return(aGoToolResult)
}

parseAGoToolPositiveHits <- function(aGoToolResult, taxid) {
  aGoToolResult$`Positive Hits` <- 
    gsub(";", ",", aGoToolResult$`Positive Hits`)
  return(aGoToolResult)
}

alterSourceKeywords <- function(aGoToolResult) {
  aGoToolResult$Source <- gsub("PFAM \\(Protein FAMilies\\)",
                               "PFAM", aGoToolResult$Source)
  aGoToolResult$Source <- gsub("UniProt keywords",
                               "UNIPROT", aGoToolResult$Source)
  aGoToolResult$Source <- gsub("Disease Ontology",
                               "DO", aGoToolResult$Source)
  aGoToolResult$Source <- gsub("Brenda Tissue Ontology",
                               "BTO", aGoToolResult$Source)
  aGoToolResult$Source <- gsub("Gene Ontology biological process",
                               "GO:BP", aGoToolResult$Source)
  aGoToolResult$Source <- gsub("Gene Ontology cellular component",
                               "GO:CC", aGoToolResult$Source)
  aGoToolResult$Source <- gsub("Gene Ontology molecular function",
                               "GO:MF", aGoToolResult$Source)
  aGoToolResult$Source <- gsub("KEGG \\(Kyoto Encyclopedia of Genes and Genomes\\)",
                               "KEGG", aGoToolResult$Source)
  aGoToolResult$Source <- gsub("Reactome",
                               "REAC", aGoToolResult$Source)
  aGoToolResult$Source <- gsub("WikiPathways",
                               "WP", aGoToolResult$Source)
  return(aGoToolResult)
}

updateReactomeTerms <- function(aGoToolResult) {
  termsVector <-
    aGoToolResult[grepl("REAC", aGoToolResult$Source), ]$Term_ID
  if (length(termsVector) > 0) {
    aGoToolResult[grepl("REAC", aGoToolResult$Source), ]$Term_ID <-
      paste0("R-", termsVector)
  }
  return(aGoToolResult)
}

alterLiteratureKeyword <- function(aGoToolResult) {
  aGoToolResult$Source <- gsub("PMID \\(PubMed IDentifier\\)",
                               "PUBMED", aGoToolResult$Source)
  return(aGoToolResult)
}
