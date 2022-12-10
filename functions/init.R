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
  session$sendCustomMessage("handler_hideSourceTabs", "functional")
  initializeOrganismsData()
  initializeArenaEdgelist()
  shinyjs::hide("functional_conversionBoxes")
  shinyjs::hide("literature_conversionBoxes")
  hideVisNetworks()
}

initializeArenaEdgelist <- function() {
  for (enrichmentType in ENRICHMENT_TYPES) {
    for (networkId in NETWORK_IDS) {
      newItem <- list(data.frame())
      names(newItem) <- paste(enrichmentType, networkId, sep = "_")
      arenaEdgelist <<- c(arenaEdgelist, newItem)
    }
  }
}

hideVisNetworks <- function() {
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(NETWORK_IDS, function(networkId) {
      shinyjs::hide(paste(enrichmentType, networkId, sep = "_"))
    })
  })
}
