# Enrichment ####
DEFAULT_TOOL <- "aGOtool"
DEFAULT_TOOL_UPPER <- toupper(DEFAULT_TOOL)
AGOTOOL_DATASOURCES_PRINT_LITERATURE <- list("PubMed Publications" = "PUBMED")

# Plots ####
PLOT_TABNAMES <- c("Network", "Heatmap", "Barchart", "Scatter Plot", "Manhattan")
# Network 
LEGEND_ITEMS <- list(
  list("GO:MF", "GO:BP", "GO:CC","UNIPROT"),
  list("KEGG", "REAC", "WP", "PANTHER"),
  list("DO", "DISGENET", "OMIM", "GLAD4U_DISEASE", "ORPHA"),
  list("DRUGBANK", "GLAD4U_DRUG", "INTERPRO", "PFAM", "PUBMED"),
  list("BTO", "WBBT", "TF", "MIRNA", "CORUM"),
  list("HPA", "HP", "WBP", "MGI", "GENE")
)
VIS_NET_HEIGHT <- "850px"

# About ####
YEAR <- substr(Sys.Date(), 1, 4)
