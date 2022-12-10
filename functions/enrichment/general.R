transformEnrichmentResultTable <- function(enrichmentParsedResult) {
  enrichmentParsedResult$`-log10Pvalue` <- as.numeric(
    format(
      -log10(enrichmentParsedResult$`P-value`),
      format = "e", digits = 3
    )
  )
  enrichmentParsedResult$`P-value` <- formatC(
    enrichmentParsedResult$`P-value`, format = "e", digits = 2
  )
  enrichmentParsedResult$`Enrichment Score %` <- calculateEnrichmentScore(
    enrichmentParsedResult$`Intersection Size`,
    enrichmentParsedResult$`Term Size`
  )
  enrichmentParsedResult <- enrichmentParsedResult[, c(
    "Source", "Term_ID", "Function", "P-value", "-log10Pvalue",
    "Term Size", "Query size", "Intersection Size",
    "Enrichment Score %", "Positive Hits"
  )]
  return(enrichmentParsedResult)
}

calculateEnrichmentScore <- function(hitGenesCount, databaseGenesCount) {
  enrichmentScore <- round((hitGenesCount / databaseGenesCount) * 100, 2)
  return(enrichmentScore)
}

validResult <- function(result) {
  valid <- F
  if (nrow(result) > 0)
    valid <- T
  return(valid)
}
