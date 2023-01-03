# Configuration variables ####
# Input
options(shiny.maxRequestSize = 1.0 * 1024^2) # uploaded files < 1MB each
LISTNAME_NCHAR_LIMIT <- 100
LIST_LIMIT <- 10
GENE_LIST_LIMIT <- 3000
OBJECT_SIZE_LIMIT <- 1000000 # bytes, 1MB
STRING_LIMIT <- 500
RANDOM_GENES_NUMBER <- 200 # currently 674 max
GENE_LIST_PREFIX <- "gene_list"
# Volcano
VOLCANO_COLORS <- c("default" = "#000000",
                    "overexpressed" = "#eb6e6c", "underexpressed" = "#64e4ed")
DEFAULT_VOLCANO_LOG10PVALUE_THRESHOLD <- 1.30103
DEFAULT_VOLCANO_LOG10PVALUE_STEP <- 0.00001
DEFAULT_VOLCANO_LOG2FC_THRESHOLD <- 1
DEFAULT_VOLCANO_LOG2FC_STEP <- 0.001
# Enrichment
ENRICHMENT_TYPES <- c("functional", "literature")
UI_TERM_KEYWORD <- list(
  "functional" = "functions", "literature" = "articles"
)
ENRICHMENT_TOOLS <- c("aGOtool", "gProfiler") # david, enrichr, webgestalt
DEFAULT_TOOL <- "aGOtool"
DEFAULT_TOOL_UPPER <- toupper(DEFAULT_TOOL)
ENRICHMENT_DATASOURCES <- c("INTERPRO", "PFAM", "UNIPROT",
                            "GO:MF", "GO:CC", "GO:BP",
                            "KEGG", "REAC", "WP",
                            "DO", "BTO",
                            "TF", "MIRNA", "CORUM", "HPA", "HP")
AGOTOOL_DATASOURCES <- c("INTERPRO", "PFAM", "UNIPROT",
                         "GO:MF", "GO:CC", "GO:BP",
                         "KEGG", "REAC", "WP",
                         "DO", "BTO", "PUBMED")
AGOTOOL_DATASOURCES_PRINT <- list(
  'Gene Ontology' = list(
    "Molecular Function (GO:MF)" = "GO:MF",
    "Cellular Component (GO:CC)" = "GO:CC",
    "Biological Process (GO:BP)" = "GO:BP"
  ),
  'Biological Pathways' = list(
    "KEGG" = "KEGG", "Reactome" = "REAC", "WikiPathways" = "WP"
  ),
  'Protein Families' = list(
    "Interpro" = "INTERPRO",
    "PFAM" = "PFAM"
  ),
  'Protein Keywords' = list(
    "UniProt keywords" = "UNIPROT"
  ),
  'Disease Ontology' = list(
    "Disease Ontology" = "DO"
  ),
  'Tissue Ontology' = list(
    "Brenda Tissue Ontology" = "BTO"
  )
)
AGOTOOL_DATASOURCES_PRINT_LITERATURE <- list("PubMed Publications" = "PUBMED")
AGOTOOL_DATASOURCES_DEFAULT_SELECTED <- list("GO:MF", "GO:CC", "GO:BP", "KEGG")
AGOTOOL_DATASOURCES_CODES <- list(
  "INTERPRO" = -54, "PFAM" = -55, "UNIPROT" = -51,
  "DO" = -26, "BTO" = -25,
  "GO:MF" = -23, "GO:CC" = -22, "GO:BP" = -21,
  "KEGG" = -52, "REAC" = -57, "WP" = -58,
  "PUBMED" = -56
)
AGOTOOL_NAMESPACES <- list(
  "ENSEMBL Protein ID" = "ENSP",
  "User Input" = "USERINPUT"
)
AGOTOOL_METRICS <- list("False discovery rate", "P-value")
AGOTOOL_API_LINK <- "https://agotool.org/api_orig"
GPROFILER_DATASOURCES <- c("GO:MF", "GO:CC", "GO:BP",
                           "KEGG", "REAC", "WP",
                           "TF", "MIRNA", "CORUM", "HPA", "HP")
GPROFILER_DATASOURCES_PRINT <- list(
  'Gene Ontology' = list(
    "Molecular Function (GO:MF)" = "GO:MF",
    "Cellular Component (GO:CC)" = "GO:CC",
    "Biological Process (GO:BP)" = "GO:BP"
  ),
  'Biological Pathways' = list(
    "KEGG" = "KEGG", "Reactome" = "REAC", "WikiPathways" = "WP"
  ),
  'Regulatory motifs in DNA' = list("TRANSFAC" = "TF","miRTarBase" = "MIRNA"),
  'Protein Databases' = list(
    "Human Protein Atlas (HPA)" = "HPA", "CORUM"= "CORUM"
  ),
  'Human Phenotype Ontology' = list("Human Phenotype Ontology" = "HP")
)
GPROFILER_DATASOURCES_DEFAULT_SELECTED <- list("GO:MF", "GO:CC", "GO:BP", "KEGG")
GPROFILER_NAMESPACES <- list(
  "User Input" = "USERINPUT", "ENSEMBL Gene ID" = "ENSG",
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
GPROFILER_METRICS <- list(
  "g:SCS threshold" = "gSCS",
  "False discovery rate" = "fdr",
  "Bonferroni correction" = "bonferroni"
)
DEFAULT_METRIC_TEXT <- "Default tool metrics"
# organisms initialized both from UI and server
ORGANISMS_FROM_FILE <- data.frame()
# Plots
DEFAULT_SLIDER_VALUE <- 50
SINGLE_BAR_HEIGHT_PX <- 18
MIN_BAR_HEIGHT_PX <- 200
# Network
VIS_NET_HEIGHT <- "850px"
EDGE_WIDTH_MIN <- 0.1
EDGE_WIDTH_MAX <- 3
LAYOUT_CHOICES <- list(
  `layout_with_graphopt` = "Graph Opt", `layout_nicely`= "Fruchterman-Reingold",
  `layout_with_kk`= "Kamada-Kawai", `layout_with_mds` = "Multi-dimensional Scaling",
  `layout_as_tree` = "Tree", `layout_as_star`  =	"Star",
  `layout_in_circle`  =	"Circle", `layout_on_grid` = "Grid",
  `layout_randomly`  =	"Random"
)
LEGEND_ITEMS <- list(
  list("GO:MF", "GO:BP", "GO:CC"),
  list("KEGG", "REAC", "WP"),
  list("INTERPRO", "PFAM", "UNIPROT"),
  list("DO", "BTO", "HP"),
  list("TF", "MIRNA", "HPA"),
  list("CORUM", "GENE", "PUBMED")
)
NETWORK_IDS <- c("network1", "network2", "network3")
# Heatmap
HEATMAP_IDS <- c("heatmap1", "heatmap2", "heatmap3")
PLOT_TABNAMES <- c("Network", "Heatmap", "Barchart", "Scatter Plot", "Manhattan")
# Interoperability
POST_REQUEST_PATH <- 'tmp/'
ARENA_API_LINK <- "https://bib.fleming.gr/bib/api/arena3dweb" #"http://127.0.0.1:8080/api/arena3dweb"
ARENA_LAYER_SPACING_PIXELS <- 300
ARENA_Y_Z_SAMPLING_LIMIT <- 410
# Colors
GENE_NODE_COLOR <- "#d1e1d9"
LITERATURE_NODE_COLOR <- "#cc9f9f"
DATASOURCE_COLORS <- c(
  "GO:MF" = "#dc3912", "GO:BP" = "#ff9900", "GO:CC" = "#109618",
  "KEGG" = "#dd4477", "REAC" = "#3366cc", "WP" = "#0099c6",
  "INTERPRO" = "#8a5103", "PFAM" = "#b3b000", "UNIPROT" = "#55edeb",
  "DO" = "#f7c8fa", "BTO" = "#f0d871",
  "TF" = "#5574a6", "MIRNA" = "#22aa99",
  "HPA" = "#6633cc", "CORUM" = "#66aa00", "HP" = "#990099",
  "PUBMED" = LITERATURE_NODE_COLOR, "GENE" = GENE_NODE_COLOR
)
# About
YEAR <- substr(Sys.Date(), 1, 4)

# User variables ####
userInputLists <- list()
checkedListNames <- list()
volcanoSelectedItems <- c()

gprofilerResult <- list() # for gprofiler ManhattanPlot only
enrichmentResults <- list()
arenaEdgelist <- list()

currentTextminingResult <- c()
currentUpsetMode <- ""
currentVolcano <- data.frame()
currentEnrichmentType <- ""
currentUserList <- c()
currentOrganism <- ""
currentEnrichmentTool <- ""
currentType_Tool <- ""
currentSignificanceMetric <- ""
