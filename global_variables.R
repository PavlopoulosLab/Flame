# Configuration variables ####
# options(bitmapType = "cairo") # TODO test if needed for server
# set the max size for each file
options(shiny.maxRequestSize = 1.0 * 1024^2) # uploaded files <1MB each

FILE_LIMIT <- 10
STRING_LIMIT <- 500
OBJECT_SIZE_LIMIT <- 1048576
RANDOM_GENES_NUMBER <- 100 # currently 322 max
POST_REQUEST_PATH <- 'tmp/'
ARENA_API_LINK <- "https://bib.fleming.gr/bib/api/arena3dweb" #"http://127.0.0.1:8080/api/arena3dweb"
# About
YEAR <- substr(Sys.Date(), 1, 4)
# Enrichment
# organisms for gprofiler and aGotool, initialized from UI
ORGANISMS_FROM_FILE <- data.frame()
# Plots
DEFAULT_SLIDER_VALUE <- 50
SINGLE_BAR_HEIGHT_PX <- 18
MIN_BAR_HEIGHT_PX <- 200
# Network
VIS_NET_HEIGHT <- "850px"
EDGE_WIDTH_MIN <- 0.1
EDGE_WIDTH_MAX <- 3
GPROFILER_DATASOURCES <- c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC",
                          "WP", "TF", "MIRNA", "CORUM", "HPA", "HP")
DATASOURCE_COLORS <- c(
  "GO:MF" = "#dc3912", "GO:BP" = "#ff9900", "GO:CC" = "#109618",
  "KEGG" = "#dd4477", "REAC" = "#3366cc", "WP" = "#0099c6",
  "TF" = "#5574a6", "MIRNA" = "#22aa99",
  "HPA" = "#6633cc", "CORUM" = "#66aa00",
  "HP" = "#990099"
)
GENE_NODE_COLOR <- "#d1e1d9"
LITERATURE_NODE_COLOR <- "#c95591"
AGOTOOL_COLORS <- c("PFAM" = "#22aa99", "INTERPRO" = "#6633cc",
                   "UniProt" = "#55edeb", "Disease Ontology" = "#990099")
LAYOUT_CHOICES <- list(
  `layout_with_graphopt` = "Graph Opt", `layout_nicely`= "Fruchterman-Reingold",
  `layout_with_kk`= "Kamada-Kawai", `layout_with_mds` = "Multi-dimensional Scaling",
  `layout_as_tree` = "Tree", `layout_as_star`  =	"Star",
  `layout_in_circle`  =	"Circle", `layout_on_grid` = "Grid",
  `layout_randomly`  =	"Random"
)

# User variables ####
file_names <- list()
global_positions <- list() # Saved positions for files to be renamed, needed for JS
upset_list <- "" # Saved files that are going to be handled for Upset plots (re-initialized when pushing the Submit Upset plot button)
inputGeneLists <- list()
gprofilerResult <- data.frame()
gprofilerTransformedResult <- data.frame()
heatmapIds <- c("heatmap1", "heatmap2", "heatmap3")
networkIds <- c("network1", "network2", "network3")
arenaEdgelist <- list()

# TODO remove after refactor plots
all_gost <- data.frame() # Variable to keep the running results of the functional enrichment analysis procedure (gprofiler2) in df format
aGotoolResults <- data.frame() # Variable to keep the initial results of the functional enrichment analysis procedure (aGotool) in df format without links
all_aGotool <- data.frame() # Variable to keep the running results of the functional enrichment analysis procedure (aGotool) in df format with links
LiteratureResults <- data.frame()
all_literature <- data.frame()
arena_ago_edgelist <- data.frame()
arena_lit_edgelist <- data.frame()
