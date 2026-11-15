#!/usr/bin/env Rscript
# This script adds GeneCodis organism support to FLAME

# Load existing data
cat("Loading existing organism data...\n")
organismsDF <- readRDS("organisms/organismsDF.RDS")
toolOrganisms <- readRDS("organisms/toolOrganismsList.RDS")

# GeneCodis supported organisms (verified via API testing)
genecodis_organisms <- data.frame(
  taxid = c(9606, 6239, 9615, 7955, 7227, 9031, 9913, 10090, 10116, 9823, 3702, 39947, 559292, 511145),
  scientific_name = c("Homo sapiens", "Caenorhabditis elegans", "Canis lupus familiaris",
                      "Danio rerio", "Drosophila melanogaster", "Gallus gallus",
                      "Bos taurus", "Mus musculus", "Rattus norvegicus", "Sus scrofa",
                      "Arabidopsis thaliana", "Oryza sativa", "Saccharomyces cerevisiae",
                      "Escherichia coli"),
  common_name = c("Human", "C. elegans", "Dog", "Zebrafish", "Fruit fly", "Chicken",
                  "Cow", "Mouse", "Rat", "Pig", "Thale cress", "Rice", "Yeast", "E. coli"),
  stringsAsFactors = FALSE
)
genecodis_taxids <- genecodis_organisms$taxid

# Check for missing organisms in master list
missing_orgs <- setdiff(genecodis_taxids, organismsDF$taxid)
if (length(missing_orgs) > 0) {
  cat("Adding", length(missing_orgs), "new organisms to master organismsDF\n")

  # Add missing organisms to master list
  for (taxid in missing_orgs) {
    org_info <- genecodis_organisms[genecodis_organisms$taxid == taxid, ]
    new_row <- data.frame(
      taxid = taxid,
      print_name = paste0(org_info$common_name, " (", org_info$scientific_name, ") [NCBI Tax. ID: ", taxid, "]"),
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
  cat("All GeneCodis organisms already exist in organismsDF\n")
}

# Add GeneCodis tool support for all organisms
toolOrganisms$GeneCodis <- genecodis_taxids

# Save updated tool organism list
cat("Saving updated tool organism support...\n")
saveRDS(toolOrganisms, "organisms/toolOrganismsList.RDS")

cat("Successfully added GeneCodis support for", length(genecodis_taxids), "organisms\n")
cat("Tool support summary:\n")
for (tool in names(toolOrganisms)) {
  cat("  ", tool, ": ", length(toolOrganisms[[tool]]), " organisms\n")
}
