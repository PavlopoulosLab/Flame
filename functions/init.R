initializeUIApp <- function() {
  initializeOrganismsData()
}

initializeOrganismsData <- function(filePath = "./organisms_with_kegg.tsv") {
  ORGANISMS_FROM_FILE <<- read.delim(
    filePath,
    header = T,
    stringsAsFactors = F
  )
  ORGANISMS_FROM_FILE$print_name <<- sprintf(
    "%s (%s) [NCBI Tax. ID: %s]",
    ORGANISMS_FROM_FILE$Species_Name,
    ORGANISMS_FROM_FILE$Common_Name,
    ORGANISMS_FROM_FILE$Taxonomy_ID
  )
}

initializeServerApp <- function() {
  lapply(ENRICHMENT_TOOLS, function(toolName) {
    session$sendCustomMessage("handler_hideSourceTabs",
                              paste0("functional_", toolName))
  })
  initializeOrganismsData()
  initializeEnrichmentResults()
  initializeArenaEdgelist()
  shinyjs::hide("functional_conversionBoxes")
  shinyjs::hide("literature_conversionBoxes")
  hideVisNetworks()
}

initializeEnrichmentResults <- function() {
  for (enrichmentTool in ENRICHMENT_TOOLS) {
    newItem <- list(data.frame())
    names(newItem) <- paste("functional", enrichmentTool, sep = "_")
    enrichmentResults <<- c(enrichmentResults, newItem)
  }
  newItem <- list(data.frame())
  names(newItem) <- paste("literature", "aGoTool", sep = "_")
  enrichmentResults <<- c(enrichmentResults, newItem)
}

initializeArenaEdgelist <- function() {
  for (enrichmentTool in ENRICHMENT_TOOLS) {
    for (networkId in NETWORK_IDS) {
      newItem <- list(data.frame())
      names(newItem) <- paste("functional", enrichmentTool, networkId, sep = "_")
      arenaEdgelist <<- c(arenaEdgelist, newItem)
    }
  }
  for (networkId in NETWORK_IDS) {
    newItem <- list(data.frame())
    names(newItem) <- paste("literature", "aGoTool", networkId, sep = "_")
    arenaEdgelist <<- c(arenaEdgelist, newItem)
  }
}

hideVisNetworks <- function() {
  lapply(ENRICHMENT_TOOLS, function(enrichmentTool) {
    lapply(NETWORK_IDS, function(networkId) {
      shinyjs::hide(paste("functional", enrichmentTool, networkId, sep = "_"))
    })
  })
  lapply(NETWORK_IDS, function(networkId) {
    shinyjs::hide(paste("literature", "aGoTool", networkId, sep = "_"))
  })
}
