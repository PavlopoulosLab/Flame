#!/usr/bin/env Rscript
# This script adds PANTHER organism support to FLAME

library(httr)
library(jsonlite)

# Load existing data
cat("Loading existing organism data...\n")
organismsDF <- readRDS("organisms/organismsDF.RDS")
toolOrganisms <- readRDS("organisms/toolOrganismsList.RDS")

# Fetch PANTHER supported organisms
cat("Fetching PANTHER organisms from API...\n")
response <- GET("https://pantherdb.org/services/oai/pantherdb/supportedgenomes")
organism_data <- fromJSON(content(response, "text"))
genomes <- organism_data$search$output$genomes$genome
panther_taxids <- genomes$taxon_id

# Check for missing organisms in master list
missing_orgs <- setdiff(panther_taxids, organismsDF$taxid)
if (length(missing_orgs) > 0) {
  cat("Adding", length(missing_orgs), "new organisms to master organismsDF\n")

  # Add missing organisms to master list
  for (taxid in missing_orgs) {
    genome_info <- genomes[genomes$taxon_id == taxid, ]
    new_row <- data.frame(
      taxid = taxid,
      print_name = paste0(genome_info$long_name, " (", genome_info$long_name, ") [NCBI Tax. ID: ", taxid, "]"),
      short_name = NA,
      kegg_name = NA,
      stringsAsFactors = FALSE
    )
    organismsDF <- rbind(organismsDF, new_row)
  }

  # Save updated master organism list
  saveRDS(organismsDF, "organisms/organismsDF.RDS")
  cat("Updated organismsDF with", length(missing_orgs), "new organisms\n")
} else {
  cat("All PANTHER organisms already exist in organismsDF\n")
}

# Add PANTHER tool support for all organisms
toolOrganisms$PANTHER <- panther_taxids

# Save updated tool organism list
cat("Saving updated tool organism support...\n")
saveRDS(toolOrganisms, "organisms/toolOrganismsList.RDS")

cat("Successfully added PANTHER support for", length(panther_taxids), "organisms\n")
cat("Tool support summary:\n")
for (tool in names(toolOrganisms)) {
  cat("  ", tool, ": ", length(toolOrganisms[[tool]]), " organisms\n")
}