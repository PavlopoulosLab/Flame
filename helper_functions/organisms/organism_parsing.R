# https://ftp.ncbi.nih.gov/pub/taxonomy/taxdmp.zip -> names.dmp # Organism name to NCBI taxid

parseGprofilerWebListOfOrganismsSingleColumnPasteToDF <- function() {
  gprofilerOrganismDF <- data.frame(
    display_name = character(),
    scientific_name = character(),
    id = character(),
    version = character(),
    datasources = character()
  )
  
  con <- file("helper_functions/gprofiler_browser_organisms_copypaste.txt", "r")
  while ( TRUE ) {
    line <- readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    display_name <- line
    line <- readLines(con, n = 1)
    scientific_name <- line
    line <- readLines(con, n = 1)
    id <- line
    line <- readLines(con, n = 1)
    version <- line
    line <- readLines(con, n = 1)
    datasources <- line
    
    gprofilerOrganismDF[nrow(gprofilerOrganismDF) + 1, ] <-
      c(display_name, scientific_name, id, version, datasources)
  }
  close(con)
  
  saveRDS(gprofilerOrganismDF, "organisms/gprofilerOrganismDF.RDS")
  write.table(gprofilerOrganismDF, "organisms/gprofiler.tsv",
              quote = F, row.names = F, sep = "\t")
  return(gprofilerOrganismDF)
}

convertAGoToolWebPasteToDF <- function() {
  aGoToolOrganismDF <- read.delim("organisms/agotool.tsv")
  colnames(aGoToolOrganismDF)[1] <- "taxon_id"
  saveRDS(aGoToolOrganismDF, "organisms/aGoToolOrganismDF.RDS")
  return(aGoToolOrganismDF)
}

createListOfToolsTaxidsLists <- function() {
  directory <- "helper_functions/organisms/"
  agotool_taxids <- as.list(read.table(paste0(directory, "agotool_taxids.txt"), header = F))
  gprofiler_taxids <- as.list(read.table(paste0(directory, "gprofiler_taxids.txt"), header = F))
  webgestalt_taxids <- as.list(read.table(paste0(directory, "webgestalt_taxids.txt"), header = F))
  enrichr_taxids <- as.list(read.table(paste0(directory, "enrichr_taxids.txt"), header = F))
  toolOrganismsList <- c(agotool_taxids, gprofiler_taxids,
                            webgestalt_taxids, enrichr_taxids)
  names(toolOrganismsList) <- c("aGOtool", "gProfiler", "WebGestalt", "enrichR")
  saveRDS(toolOrganismsList, "toolOrganismsList.RDS")
}

createOrganismsDF <- function() {
  # starting with gprofiler (better display_names)
  organismsDF <- read.delim(paste0(directory, "gprofiler_with_taxids.tsv"), header = T)
  organismsDF$print_name <-
    sprintf("%s (%s) [NCBI Tax. ID: %s]",
            organismsDF$scientific_name, organismsDF$display_name,
            organismsDF$ncbi)
  organismsDF <- organismsDF[, c("print_name", "ncbi", "id")]
  colnames(organismsDF) <- c("print_name", "taxid", "short_name")
  
  # adding aGoTool organism rows where they don't already exist
  agotoolDF <- read.delim(paste0(directory, "agotool_display_names.tsv"), header = T)
  # removing existing
  agotoolDF <- agotoolDF[!(agotoolDF$X.taxon_id %in% organismsDF$taxid), ]
  agotoolDF$print_name <-
    sprintf("%s (%s) [NCBI Tax. ID: %s]",
            agotoolDF$official_name_NCBI, agotoolDF$display_name,
            agotoolDF$X.taxon_id)
  agotoolDF <- agotoolDF[, c("print_name", "X.taxon_id")]
  colnames(agotoolDF) <- c("print_name", "taxid")
  agotoolDF$short_name <- NA
  
  organismsDF <- rbind(organismsDF, agotoolDF)
  
  # append KEGG name column
  keggDF <- read.delim(paste0(directory, "KEGG_codes.tsv"), header = T)
  organismsDF <- merge(organismsDF, keggDF, all.x = T)
  organismsDF$scientific_name <- NULL
  colnames(organismsDF)[4] <- "kegg_name"
  saveRDS(organismsDF, "organismsDF.RDS")
}

gprofilerOrganismDF <- parseGprofilerWebListOfOrganismsSingleColumnPasteToDF()
aGoToolOrganismDF <- convertAGoToolWebPasteToDF()
createListOfToolsTaxidsLists()
createOrganismsDF()
