initializeUIApp <- function() {
  initializeOrganismsData()
}

initializeOrganismsData <- function() {
  ORGANISMS_FROM_FILE <<- read.delim(
    "./organisms_with_kegg.tsv",
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
  session$sendCustomMessage("handler_disableSourcesTabs", "gprofiler")
  session$sendCustomMessage("handler_disableSourcesTabs", "aGoTool")
  initializeOrganismsData()
  initializeArenaEdgelist()
  shinyjs::hide("conversionBoxes")
  hideVisNetworks()
}

initializeArenaEdgelist <- function() {
  for (id in networkIds) {
    arenaEdgelist <<- c(arenaEdgelist, list(data.frame()))
  }
  names(arenaEdgelist) <<- networkIds
}

hideVisNetworks <- function() {
  lapply(networkIds, function(networkId) {
    shinyjs::hide(networkId)
  })
}
