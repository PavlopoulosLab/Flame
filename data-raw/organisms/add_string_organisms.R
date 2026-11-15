#!/usr/bin/env Rscript
# This script adds STRING organism support to FLAME

# Load existing data
cat("Loading existing organism data...\n")
organismsDF <- readRDS("organisms/organismsDF.RDS")
toolOrganisms <- readRDS("organisms/toolOrganismsList.RDS")

# Load STRING taxids
cat("Reading STRING species data...\n")
string_data <- read.delim("organisms/automation/string_species.v12.0.txt",
                         header = TRUE, stringsAsFactors = FALSE)
string_taxids <- string_data$X.taxon_id

# Check for missing organisms in master list
missing_orgs <- setdiff(string_taxids, organismsDF$taxid)
if (length(missing_orgs) > 0) {
  cat("Adding", length(missing_orgs), "new organisms to master organismsDF\n")

  # Add missing organisms to master list
  for (taxid in missing_orgs) {
    # Find organism info from STRING data
    org_info <- string_data[string_data$X.taxon_id == taxid, ]
    new_row <- data.frame(
      taxid = taxid,
      print_name = paste0(org_info$official_name, " (", org_info$official_name, ") [NCBI Tax. ID: ", taxid, "]"),
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
  cat("All STRING organisms already exist in organismsDF\n")
}

# Add STRING tool support for all organisms
toolOrganisms$STRING <- string_taxids

# Save updated tool organism list
cat("Saving updated tool organism support...\n")
saveRDS(toolOrganisms, "organisms/toolOrganismsList.RDS")

cat("Successfully added STRING support for", length(string_taxids), "organisms\n")
cat("Tool support summary:\n")
for (tool in names(toolOrganisms)) {
  cat("  ", tool, ": ", length(toolOrganisms[[tool]]), " organisms\n")
}