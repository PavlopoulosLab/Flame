runGprofiler <- function(query, user_reference = NULL) {
  gprofilerResult <<- list()
  sources <- DATASOURCES[["GPROFILER"]][
    DATASOURCES[["GPROFILER"]] %in% input$functional_enrichment_datasources
  ]
  if(is.null(user_reference)) {
    domain_scope <- "annotated"
    custom_bg <- NULL
    significant <- T
  }
  else {
    domain_scope <- "custom"
    custom_bg <- user_reference
    significant <- F
  }
  if (!identical(sources, character(0)))
    gprofilerResult <<- gprofiler2::gost(
      query = query,
      organism = ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name,
      significant = significant,
      evcodes = T, # gene hits and intersection
      user_threshold = input$functional_enrichment_threshold,
      correction_method = currentSignificanceMetric,
      sources = sources,
      domain_scope = domain_scope,
      custom_bg = custom_bg
    )
  
  if(is.null(user_reference))
    enrichmentBackgroundSizes[["FUNCTIONAL_GPROFILER"]] <<- getGprofilerBackgroundSize()
  else
    enrichmentBackgroundSizes[["FUNCTIONAL_GPROFILER"]] <<- length(user_reference)
  
  if (isGprofilerResultValid()) {
    gprofilerParsedResult <- parseGprofilerResult()
    enrichmentResults[[currentType_Tool]] <<-
      transformEnrichmentResultTable(gprofilerParsedResult)
  }
}

isGprofilerResultValid <- function() {
  isValid <- F
  if (length(gprofilerResult) > 0)
    isValid <- T
  return(isValid)
}

parseGprofilerResult <- function() {
  gprofilerParsedResult <- gprofilerResult$result[, c(
    "source", "term_id", "term_name", "p_value", "term_size",
    "query_size", "intersection_size", "intersection"
  )]
  colnames(gprofilerParsedResult) <- ENRICHMENT_DF_COLNAMES
  gprofilerParsedResult <- mapGProfilerIds(gprofilerParsedResult)
  return(gprofilerParsedResult)
}

mapGProfilerIds <- function(gprofilerParsedResult) {
  gprofilerParsedResult <- mapKEGGIds(gprofilerParsedResult)
  gprofilerParsedResult <- mapREACIds(gprofilerParsedResult)
  gprofilerParsedResult <- mapWPIds(gprofilerParsedResult)
  return(gprofilerParsedResult)
}

mapREACIds <- function(df) {
  if (length(df$Source[which(df$Source == "REAC")]) > 0) {
    df[df$Source == "REAC", ]$Term_ID <-
      gsub("REAC:", "", df[df$Source == "REAC", ]$Term_ID)
  }
  return(df)
}

mapWPIds <- function(df) {
  if (length(df$Source[which(df$Source == "WP")]) > 0) {
    df[df$Source == "WP", ]$Term_ID <-
      gsub("WP:", "", df[df$Source == "WP", ]$Term_ID)
  }
  return(df)
}

getGprofilerBackgroundSize <- function() {
  if(isGprofilerResultValid()) {
  metadata <- gprofilerResult$meta$result_metadata
  bsizes <- lapply(names(metadata), function(i) {
    return(metadata[[i]]$domain_size)
  })
  size <- max(unlist(bsizes))
  }
  else {
    size <- NULL
  }
  return(size)
}
