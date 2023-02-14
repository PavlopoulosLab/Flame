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

gprofilerOrganismDF <- parseGprofilerWebListOfOrganismsSingleColumnPasteToDF()
aGoToolOrganismDF <- convertAGoToolWebPasteToDF()
