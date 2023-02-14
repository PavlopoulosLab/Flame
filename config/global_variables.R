# Input ####
# Volcano
DEFAULT_VOLCANO_LOG10PVALUE_THRESHOLD <- 1.30103
DEFAULT_VOLCANO_LOG10PVALUE_STEP <- 0.00001
DEFAULT_VOLCANO_LOG2FC_THRESHOLD <- 1
DEFAULT_VOLCANO_LOG2FC_STEP <- 0.001

# Enrichment ####
ENRICHMENT_TOOLS <- c("aGOtool", "gProfiler", "WebGestalt", "enrichR") # david
TAB_NAMES <- list(
  "ALL" = "all",
  "GO:MF" = "gomf", "GO:CC" = "gocc", "GO:BP" = "gobp",
  "KEGG" = "kegg", "REAC" = "reac", "WP" = "wp", "PANTHER" = "panther",
  "INTERPRO" = "interpro", "PFAM" = "pfam", "UNIPROT" = "uniprot",
  "DO" = "do", "DISGENET" = "disgenet", "OMIM" = "omim", "GLAD4U_DISEASE" = "glad4udisease", "ORPHA" = "orpha",
  "DRUGBANK" = "drugbank", "GLAD4U_DRUG" = "glad4udrug",
  "BTO" = "brenda", "WBBT" = "wbbt", "TF" = "tf",
  "MIRNA" = "mirna", "CORUM" = "corum",
  "HPA" = "hpa", "HP" = "hp", "WBP" = "wbp", "MGI" = "mgi"
)
TAB_NAMES_CODES <- as.character(TAB_NAMES)
ENRICHMENT_DATASOURCES <- names(TAB_NAMES[TAB_NAMES != "all"])
NAMESPACES <- list()
NAMESPACES[["CORE"]] <- list(
  "ENSEMBL Gene ID" = "ENSG",
  "ENSEMBL Protein ID" = "ENSP", "ENSEMBL Transcript ID" = "ENST",
  "Entrez Gene Name" = "ENTREZGENE", "Entrez Gene Accession" = "ENTREZGENE_ACC",
  "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME",
  "UniProt Gene Name" = "UNIPROT_GN", "UniProt Accession" = "UNIPROT_GN_ACC", 
  "UniProt Archive" = "UNIPARC", "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC",
  "RefSeq mRNA" = "REFSEQ_MRNA", "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
  "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC",
  "EMBL Accession" = "EMBL", "ChEMBL" = "CHEMBL", 
  "WIKIGENE ID" = "WIKIGENE" 
)
NAMESPACES[["SPECIAL"]] <- list()
NAMESPACES[["SPECIAL"]][["amellifera"]] <- c("BEEBASE" = "BEEBASE")
NAMESPACES[["SPECIAL"]][["dmelanogaster"]] <- c("FLYBASE_GENE_ID" = "FLYBASE_GENE_ID")
NAMESPACES[["AGOTOOL"]] <- list("ENSEMBL Protein ID" = "ENSP")
UI_TERM_KEYWORD <- list(
  "functional" = "functions", "literature" = "articles"
)

# Plots ####
# Network
NETWORK_IDS <- c("network1", "network2", "network3")
LAYOUT_CHOICES <- list(
  `layout_with_graphopt` = "Graph Opt", `layout_nicely`= "Fruchterman-Reingold",
  `layout_with_kk`= "Kamada-Kawai", `layout_with_mds` = "Multi-dimensional Scaling",
  `layout_as_tree` = "Tree", `layout_as_star`  =	"Star",
  `layout_in_circle`  =	"Circle", `layout_on_grid` = "Grid",
  `layout_randomly`  =	"Random"
)
# Heatmap
HEATMAP_IDS <- c("heatmap1", "heatmap2", "heatmap3")
# Colors
GENE_NODE_COLOR <- "#d1e1d9"
LITERATURE_NODE_COLOR <- "#cc9f9f"
DATASOURCE_COLORS <- c(
  "GO:MF" = "#dc3912", "GO:BP" = "#ff9900", "GO:CC" = "#109618",
  "KEGG" = "#dd4477", "REAC" = "#3366cc", "WP" = "#0099c6", "PANTHER" = "#634341",
  "INTERPRO" = "#8a5103", "PFAM" = "#b3b000", "UNIPROT" = "#55edeb",
  "DO" = "#f7c8fa", "DISGENET" = "#c0f0a1", "OMIM" = "#edebaf",
  "GLAD4U_DISEASE" = "#9f86d9", "ORPHA" = "#03fcc6", "DRUGBANK" = "#7d4a74", "GLAD4U_DRUG" = "#4a9091",
  "BTO" = "#f0d871", "WBBT" = "#9cb59c", "TF" = "#5574a6", "MIRNA" = "#22aa99",
  "CORUM" = "#66aa00", "HPA" = "#6633cc", "HP" = "#990099", "WBP" = "#fffd78", "MGI" = "#fc4503",
  "PUBMED" = LITERATURE_NODE_COLOR, "GENE" = GENE_NODE_COLOR
)
    