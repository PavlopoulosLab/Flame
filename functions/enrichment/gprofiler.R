runGprofiler <- function(query){
  gprofilerResult <<- gost(
    query = query,
    organism = currentOrganism,
    evcodes = T, # gene hits and intersection
    user_threshold = input$functional_enrichment_threshold,
    correction_method = currentSignificanceMetric,
    sources = input$functional_enrichment_datasources
  )
  if (validGprofilerResult()) {
    gprofilerParsedResult <- parseGprofilerResult()
    enrichmentResults[[currentType_Tool]] <<-
      transformEnrichmentResultTable(gprofilerParsedResult)
  }
}

parseGprofilerResult <- function() {
  gprofilerParsedResult <- gprofilerResult$result[, c(
    "source", "term_id", "term_name", "p_value", "term_size",
    "query_size", "intersection_size", "intersection"
  )]
  colnames(gprofilerParsedResult) <- c(
    "Source", "Term_ID", "Function", "P-value", "Term Size",
    "Query size", "Intersection Size", "Positive Hits"
  )
  return(gprofilerParsedResult)
}

validGprofilerResult <- function() {
  valid <- F
  if (length(gprofilerResult) > 0)
    valid <- T
  return(valid)
}
