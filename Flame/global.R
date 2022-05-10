# Initialization of global variables
file_names <- list() # File names of current existing files
file_data <- list() # Data (gene lists) of current existing files
global_positions <- list() # Saved positions for files to be renamed, needed for JS
upset_list <- "" # Saved files that are going to be handled for Upset plots (re-initialized when pushing the Submit Upset plot button)
all_gost <- data.frame()# Variable to keep the running results of the functional enrichment analysis procedure (gprofiler2) in df format
gostres <- data.frame()
aGotoolResults <- data.frame() # Variable to keep the initial results of the functional enrichment analysis procedure (aGotool) in df format without links
all_aGotool <- data.frame()# Variable to keep the running results of the functional enrichment analysis procedure (aGotool) in df format with links
LiteratureResults <- data.frame()
all_literature <- data.frame()
# FINAL
bar_colors <- c("GO:MF"= "#dc3912", "GO:BP"= "#ff9900", "GO:CC" = "#109618",
                "KEGG" = "#dd4477", "REAC" = "#3366cc", "WP" = "#0099c6", "TF" = "#5574a6",
                "MIRNA" = "#22aa99", "HPA" = "#6633cc", "CORUM" = "#66aa00", "HP" = "#990099")
aGoBar_colors <- c("PFAM" = "#22aa99", "INTERPRO" = "#6633cc", "UniProt" = "#55edeb", "Disease Ontology" = "#990099")

#read csv file with the organism for gprofiler and aGotool
organismsFromFile <- read.csv("./organisms_with_kegg.csv", header = T, sep="\t", stringsAsFactors = F)
organismsFromFile$print_name <- paste0(sprintf("%s (%s) [NCBI Tax. ID: %s]", organismsFromFile$Species_Name, organismsFromFile$Common_Name, organismsFromFile$Taxonomy_ID))

# CONFIGURATION variables
FILE_LIMIT <- 10
STRING_LIMIT <- 500
OBJECT_SIZE_LIMIT <- 1048576
POST_REQUEST_PATH <- 'tmp/'
