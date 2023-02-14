# Input ####
LISTNAME_NCHAR_LIMIT <- 100
LIST_LIMIT <- 10
GENE_LIST_LIMIT <- 3000
OBJECT_SIZE_LIMIT <- 1000000 # bytes, 1MB
STRING_LIMIT <- 500
RANDOM_GENES_NUMBER <- 200 # currently 674 max # enrichR::genes790 
GENE_LIST_PREFIX <- "gene_list"
# Volcano
VOLCANO_COLORS <- c("default" = "#000000",
                    "overexpressed" = "#eb6e6c", "underexpressed" = "#64e4ed")

# Organisms ####
ORGANISMS <- readRDS("./organisms/organismsDF.RDS")
TOOL_ORGANISMS <- readRDS("./organisms/toolOrganismsList.RDS")
SPECIAL_ORGANISMS <- c("amellifera", "dmelanogaster")
SPECIAL_PREFERRED_TOOL <- list()
SPECIAL_PREFERRED_TOOL[["amellifera"]] <- "gProfiler"
SPECIAL_PREFERRED_TOOL[["dmelanogaster"]] <- "enrichR"
SPECIAL_PREFERRED_NAMESPACE <- list()
SPECIAL_PREFERRED_NAMESPACE[["amellifera"]] <- "BEEBASE"
SPECIAL_PREFERRED_NAMESPACE[["dmelanogaster"]] <- "USERINPUT"

# Enrichment ####
ENRICHMENT_TYPES <- c("functional", "literature")
DATASOURCES_PRINT <- list(
  'Gene Ontology' = list(
    "Molecular Function (GO:MF)" = "GO:MF",
    "Cellular Component (GO:CC)" = "GO:CC",
    "Biological Process (GO:BP)" = "GO:BP"
  ),
  'Biological Pathways' = list(
    "KEGG" = "KEGG", "Reactome" = "REAC",
    "WikiPathways" = "WP", "PANTHER" = "PANTHER"
  ),
  'Diseases' = list(
    "Disease Ontology" = "DO", "DisGeNET" = "DISGENET",
    "OMIM" = "OMIM", "GLAD4U" = "GLAD4U_DISEASE", "Orphanet" = "ORPHA"
  ),
  'Proteins' = list(
    "Interpro" = "INTERPRO", "PFAM" = "PFAM",
    "UniProt keywords" = "UNIPROT",
    "CORUM"= "CORUM"
  ),
  'Phenotypes' = list(
    "Human Phenotype Ontology" = "HP",
    "MGI Mammalian Phenotype" = "MGI",
    "WormBase Phenotypes" = "WBP"
  ),
  'Tissues' = list(
    "Human Protein Atlas (HPA)" = "HPA",
    "Brenda Tissue Ontology" = "BTO",
    "WormBase Anatomic Associations Ontology" = "WBBT"
  ),
  'Drugs' = list(
    "DrugBank" = "DRUGBANK",
    "GLAD4U" = "GLAD4U_DRUG"
  ),
  'Regulatory motifs in DNA' = list(
    "TRANSFAC" = "TF", "miRTarBase" = "MIRNA"
  )
)
DATASOURCES_DEFAULT_SELECTED <- list("GO:MF", "GO:CC", "GO:BP", "KEGG")
DATASOURCES <- list() # gProfiler here, the rest through an init.R function
DATASOURCES[["GPROFILER"]] <- c("GO:MF", "GO:CC", "GO:BP",
                              "KEGG", "REAC", "WP",
                              "TF", "MIRNA", "CORUM", "HPA", "HP")
DATASOURCES_CODES <- list()
DATASOURCES_CODES[["AGOTOOL"]] <- list(
  "GO:MF" = -23, "GO:CC" = -22, "GO:BP" = -21,
  "KEGG" = -52, "REAC" = -57, "WP" = -58,
  "INTERPRO" = -54, "PFAM" = -55, "UNIPROT" = -51,
  "DO" = -26, "BTO" = -25,
  "PUBMED" = -56
)
DATASOURCES_CODES[["WEBGESTALT"]] <- list(
  "GO:MF" = "geneontology_Molecular_Function_noRedundant",
  "GO:CC" = "geneontology_Cellular_Component_noRedundant",
  "GO:BP" = "geneontology_Biological_Process_noRedundant",
  "KEGG" = "pathway_KEGG",
  "REAC" = "pathway_Reactome",
  "WP" = "pathway_Wikipathway",
  "PANTHER" = "pathway_Panther",
  "DISGENET" = "disease_Disgenet",
  "OMIM" = "disease_OMIM",
  "GLAD4U_DISEASE" = "disease_GLAD4U",
  "DRUGBANK" = "drug_DrugBank",
  "GLAD4U_DRUG" = "drug_GLAD4U",
  "HP" = "phenotype_Human_Phenotype_Ontology"
)
DATASOURCES_CODES[["ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2021",
  "GO:CC" = "GO_Cellular_Component_2021",
  "GO:BP" = "GO_Biological_Process_2021",
  "KEGG" = "KEGG_2016", # "KEGG_2021_Human" -> doesn't return Ids
  "REAC" = "Reactome_2022",
  "WP" = "WikiPathway_2021_Human",
  "PANTHER" = "Panther_2016",
  "HP" = "Human_Phenotype_Ontology"
)
DATASOURCES_CODES[["MOUSE_ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2021",
  "GO:CC" = "GO_Cellular_Component_2021",
  "GO:BP" = "GO_Biological_Process_2021",
  "WP" = "WikiPathways_2019_Mouse",
  "MGI" = "KOMP2_Mouse_Phenotypes_2022"
)
DATASOURCES_CODES[["FLY_ENRICHR"]] <-
  DATASOURCES_CODES[["FISH_ENRICHR"]] <- list(
    "GO:MF" = "GO_Molecular_Function_2018",
    "GO:CC" = "GO_Cellular_Component_2018",
    "GO:BP" = "GO_Biological_Process_2018",
    "WP" = "WikiPathways_2018"
  )
DATASOURCES_CODES[["YEAST_ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2018",
  "GO:CC" = "GO_Cellular_Component_2018",
  "GO:BP" = "GO_Biological_Process_2018",
  "WP" = "WikiPathways_2018",
  "KEGG" = "KEGG_2018"
)
DATASOURCES_CODES[["WORM_ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2018",
  "GO:CC" = "GO_Cellular_Component_2018",
  "GO:BP" = "GO_Biological_Process_2018",
  "WP" = "WikiPathways_2018",
  "DO" = "Human_Diseases_from_WormBase_2018",
  "WBP" = "Phenotypes_WormBase_2018",
  "WBBT" = "Anatomic_Associations_WormBase_2018"
)
DATASOURCES_CODES[["OX_ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2021",
  "GO:CC" = "GO_Cellular_Component_2021",
  "GO:BP" = "GO_Biological_Process_2021",
  "REAC" = "Reactome_2016",
  "WP" = "WikiPathway_2021_Human",
  "ORPHA" = "Orphanet_Augmented_2021",
  "HP" = "Human_Phenotype_Ontology",
  "MGI" = "MGI_Mammalian_Phenotype_Level_4_2019"
)
NAMESPACES[["GPROFILER"]] <- c("User Input" = "USERINPUT", NAMESPACES[["CORE"]])
NAMESPACES[["WEBGESTALT"]] <- list(
  "Entrez Gene Accession" = "ENTREZGENE_ACC",
  "User Input" = "USERINPUT"
)
NAMESPACES[["ENRICHR"]] <- list(
  "Entrez Gene Name" = "ENTREZGENE",
  "User Input" = "USERINPUT"
)
NAMESPACES[["FLY_ENRICHR"]] <-
  NAMESPACES[["YEAST_ENRICHR"]] <- list(
  "User Input" = "USERINPUT"
)
DEFAULT_NAMESPACE_TEXT <- "Default tool namespace conversions"
METRICS <- list()
METRICS[["AGOTOOL"]] <- list("False discovery rate", "P-value")
METRICS[["GPROFILER"]] <- list(
  "g:SCS threshold" = "gSCS",
  "False discovery rate" = "fdr",
  "Bonferroni correction" = "bonferroni"
)
METRICS[["WEBGESTALT"]] <- list(
  "Benjamini-Hochberg" = "BH",
  "Benjamini-Yekutieli" = "BY",
  "Holm" = "holm",
  "Hochberg" = "hochberg",
  "Hommel" = "hommel",
  "Bonferroni adjustment" = "bonferroni"
)
METRICS[["ENRICHR"]] <- list("Adjusted P-value" = "adjusted_pvalue")
DEFAULT_METRIC_TEXT <- "Default tool metrics"
ENRICHMENT_DF_COLNAMES <- c(
  "Source", "Term_ID", "Function", "P-value", "Term Size", 
  "Query size", "Intersection Size", "Positive Hits"
)
AGOTOOL_API_LINK <- "https://agotool.org/api_orig"

# Plots ####
ALL_PLOT_IDS <- c(NETWORK_IDS, HEATMAP_IDS, "barchart", "scatterPlot")
DEFAULT_SLIDER_VALUE <- 50
MAX_SLIDER_VALUE <- 200
SINGLE_BAR_HEIGHT_PX <- 18
MIN_BAR_HEIGHT_PX <- 200
EDGE_WIDTH_MIN <- 0.1
EDGE_WIDTH_MAX <- 3

# Interoperability ####
POST_REQUEST_PATH <- 'tmp/'
ARENA_API_LINK <- "https://bib.fleming.gr/bib/api/arena3dweb" #"http://127.0.0.1:8080/api/arena3dweb"
ARENA_LAYER_SPACING_PIXELS <- 300
ARENA_Y_Z_SAMPLING_LIMIT <- 410
