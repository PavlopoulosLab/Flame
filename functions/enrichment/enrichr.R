runEnrichr <- function(userInputList) {
  site <- setEnrichrOrganism()
  databases <- setEnrichrDatabases()
  
  result <- enrichR::enrichr(userInputList, databases)
  
  enrichrResult <- do.call(rbind, result) # extracting df from list of lists
  enrichrResult <- filterSignificance(enrichrResult)
  
  enrichmentBackgroundSizes[["FUNCTIONAL_ENRICHR"]] <<- getEnrichrBackgroundSize(site, databases)
  
  if (isResultValid(enrichrResult)) {
    enrichrResult <- parseEnirchrResult(enrichrResult, length(userInputList))
    enrichmentResults[[currentType_Tool]] <<-
      transformEnrichmentResultTable(enrichrResult)
  }
}

setEnrichrOrganism <- function() {
  site <- switch(
    ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name,
    "hsapiens" = "Enrichr",
    "mmusculus" = "Enrichr",
    "dmelanogaster" = "FlyEnrichr",
    "celegans" = "WormEnrichr",
    "scerevisiae" = "YeastEnrichr",
    "drerio" = "FishEnrichr",
    "btaurus" = "OxEnrichr"
  )
  enrichR::setEnrichrSite(site)
  return(site)
}

setEnrichrDatabases <- function() {
  toolSources <- getEnrichrOrganismSourceCodes()
  selectedDatasources <-
    input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
  databases <- as.character(toolSources[
    selectedDatasources[which(selectedDatasources %in% names(toolSources))]
  ])
  return(databases)
}

getEnrichrOrganismSourceCodes <- function() {
  prefix <- getEnrichrVariablePrefix()
  toolSources <- DATASOURCES_CODES[[paste0(prefix, "ENRICHR")]]
  return(toolSources)
}

getEnrichrVariablePrefix <- function() {
  return(
    switch(
      ORGANISMS[ORGANISMS$print_name ==
                  input[["functional_enrichment_organism"]], ]$short_name,
      "mmusculus" = "MOUSE_",
      "dmelanogaster" = "FLY_",
      "celegans" = "WORM_",
      "scerevisiae" = "YEAST_",
      "drerio" = "FISH_",
      "btaurus" = "OX_"
    )
  )
}

filterSignificance <- function(enrichrResult) {
  threshold <- as.numeric(input$functional_enrichment_threshold)
  enrichrResult <- enrichrResult[enrichrResult$Adjusted.P.value <= threshold, ]
  return(enrichrResult)
}

parseEnirchrResult <- function(enrichrResult, numInputs) {
  enrichrResult$database <- appendEnrichrDatabases(rownames(enrichrResult))
  rownames(enrichrResult) <- NULL
  toolSources <- getEnrichrOrganismSourceCodes()
  enrichrResult$database <- 
    unlistDatasourceCodes(enrichrResult$database, toolSources)
  enrichrResult$querySize <- numInputs
  enrichrResult$Genes <- gsub(";", ",", enrichrResult$Genes)
  enrichrResult <- enrichrResult %>%
    tidyr::separate(Overlap, c("overlap", "size"), sep="\\/")
  enrichrResult$overlap <- as.numeric(enrichrResult$overlap)
  enrichrResult$size <- as.numeric(enrichrResult$size)
  result <- splitEnrichrTermIds(enrichrResult)
  enrichrResult$Term <- result$terms
  enrichrResult$TermId <- result$ids
  
  enrichrResult <-
    enrichrResult[, c(
      "database", "TermId", "Term", "Adjusted.P.value",
      "size", "querySize", "overlap", "Genes")]
  colnames(enrichrResult) <- ENRICHMENT_DF_COLNAMES
  enrichrResult <- mapKEGGIds(enrichrResult)
  return(enrichrResult)
}

appendEnrichrDatabases <- function(rowNames) {
  return(sapply((strsplit(rowNames, "\\.")), "[[", 1))
}

splitEnrichrTermIds <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  result <- splitEnrichrGoTerms(enrichrResult)
  terms <- c(terms, result$terms)
  ids <- c(ids, result$ids)
  if (ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name == "hsapiens") {
    result <- splitEnrichrKEGGTerms(enrichrResult)
    terms <- c(terms, result$terms)
    ids <- c(ids, result$ids)
  } else { # scerevisiae
    result <- splitEnrichrKEGGTermsYeast(enrichrResult)
    terms <- c(terms, result$terms)
    ids <- c(ids, result$ids)
  }
  result <- splitEnrichrREACTerms(enrichrResult)
  terms <- c(terms, result$terms)
  ids <- c(ids, result$ids)
  result <- splitEnrichrWPTerms(enrichrResult)
  terms <- c(terms, result$terms)
  ids <- c(ids, result$ids)
  result <- splitEnrichrPantherTerms(enrichrResult)
  terms <- c(terms, result$terms)
  ids <- c(ids, result$ids)
  result <- splitEnrichrDOTerms(enrichrResult)
  terms <- c(terms, result$terms)
  ids <- c(ids, result$ids)
  result <- splitEnrichrWBPTerms(enrichrResult)
  terms <- c(terms, result$terms)
  ids <- c(ids, result$ids)
  result <- splitEnrichrWBBTTerms(enrichrResult)
  terms <- c(terms, result$terms)
  ids <- c(ids, result$ids)
  result <- splitEnrichrORPHATerms(enrichrResult)
  terms <- c(terms, result$terms)
  ids <- c(ids, result$ids)
  if (ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name == "mmusculus") {
    result <- splitEnrichrMGITerms(enrichrResult)
    terms <- c(terms, result$terms)
    ids <- c(ids, result$ids)
  } else { # Ox
    result <- splitEnrichrMGITermsOx(enrichrResult)
    terms <- c(terms, result$terms)
    ids <- c(ids, result$ids)
  }
  result <- splitEnrichrHPTerms(enrichrResult)
  terms <- c(terms, result$terms)
  ids <- c(ids, result$ids)
  return(list(terms = terms, ids = ids))
}

splitEnrichrGoTerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^GO:", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, " \\(GO:")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
    ids <- paste0("GO:", substring(ids, 1, nchar(ids) - 1))
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrKEGGTerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^KEGG$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, " Homo sapiens ")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrKEGGTermsYeast <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^KEGG$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    terms <- trimws(substr(items, 0, nchar(items) - 8))
    ids <- substr(items, nchar(items) - 7, nchar(items))
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrREACTerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^REAC$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, " R-")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
    ids <- paste0("R-", ids)
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrWPTerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^WP$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, "WP")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
    ids <- paste0("WP", ids)
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrPantherTerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^PANTHER$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, " Homo sapiens ")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrDOTerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^DO$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, "\\(DOID:")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
    ids <- paste0("DOID:", substring(ids, 1, nchar(ids) - 1))
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrWBPTerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^WBP$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, "_WBPhenotype:")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
    ids <- paste0("WBPhenotype:", ids)
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrWBBTTerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^WBBT$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, "\\(WBbt:")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
    ids <- paste0("WBbt:", substring(ids, 1, nchar(ids) - 1))
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrORPHATerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^ORPHA$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, " ORPHA:")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
    ids <- paste0("ORPHA:", ids)
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrMGITerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^MGI$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, " \\(MP:")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
    ids <- paste0("MP:", substring(ids, 1, nchar(ids) - 1))
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrMGITermsOx <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^MGI$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    terms <- substr(items, 12, nchar(items))
    ids <- substr(items, 0, 10)
  }
  return(list(terms = terms, ids = ids))
}

splitEnrichrHPTerms <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  items <- enrichrResult[grep("^HP$", enrichrResult$database), ]$Term
  if (!identical(items, character(0))) {
    splitList <- strsplit(items, " \\(HP:")
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, "[[", 2)
    ids <- paste0("HP:", substring(ids, 1, nchar(ids) - 1))
  }
  return(list(terms = terms, ids = ids))
}

getEnrichrBackgroundSize <- function(site, selected_dbs) {
  if(site == "Enrichr") {
    dbs_all <- listEnrichrDbs()
    size <- max(unlist(dbs_all[dbs_all$libraryName %in% selected_dbs,]$numTerms))
  }
  else
    size <- NULL
  return(size)
}