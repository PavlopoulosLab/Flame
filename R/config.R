# =============================================================================
# FLAME Configuration
# =============================================================================
#
# Single source of truth for all configuration in the FLAME app.
# Consolidates enums, tool configs, UI constants, and helper functions.
#
# Contents:
#   1. ENUMS - Type-safe identifiers (ToolId, ParadigmId, etc.)
#   2. PARADIGMS - Analysis paradigm configurations
#   3. TOOLS - Complete tool configurations (datasources, namespaces, metrics)
#   4. ORGANISMS - Organism data (loaded from RDS)
#   5. UI CONSTANTS - Colors, layouts, tab names, display structures
#   6. ENRICHMENT TYPES - Enrichment type configurations
#   7. HELPER FUNCTIONS - Config access functions
#
# =============================================================================

# =============================================================================
# 1. ENUMS - Centralized identifiers for type safety and autocomplete
# =============================================================================

#' Analyte Type
#' @description Valid types of biological analytes that can be analyzed
AnalyteType <- list(
  GENE = "GENE",
  METABOLITE = "METABOLITE",
  LIPID = "LIPID"
)

#' Workspace analyte metadata
#' @description Centralized analyte metadata for workspace dropdowns and filters.
ANALYTE_UI <- list(
  list(
    analyteType = AnalyteType$GENE,
    key = "genes",
    singularKey = "gene",
    label = "Genes",
    shortLabel = "Genes",
    uploadEnabled = TRUE
  ),
  list(
    analyteType = AnalyteType$METABOLITE,
    key = "metabolites",
    singularKey = "metabolite",
    label = "Metabolites",
    shortLabel = "Metab.",
    uploadEnabled = FALSE
  ),
  list(
    analyteType = AnalyteType$LIPID,
    key = "lipids",
    singularKey = "lipid",
    label = "Lipids",
    shortLabel = "Lipids",
    uploadEnabled = FALSE
  )
)

#' Score Direction
#' @description Indicates whether higher or lower scores are better for ranked analytes
ScoreDirection <- list(
  ASCENDING = "ASCENDING",
  DESCENDING = "DESCENDING"
)

#' Module IDs
#' @description Shiny module namespace IDs - must match between UI and server
#' Used by: ui.R, view-input.R (UI) and server.R (session constructors)
#'
#' Naming convention:
#'   - INPUT_X matches file input-session-X.R
#'   - LISTMGMT_X matches file listmgmt-session-*.R
#'   - UTILITIES_X matches file utilities-session-*.R
ModuleIds <- list(
  # Input sessions (input-session-*.R)
  INPUT_LIST = "input_list",
  INPUT_VOLCANO = "input_volcano",
  INPUT_REDUCTION = "input_reduction",
  INPUT_SNPS = "input_snps",
  INPUT_TEXTMINING = "input_textmining",
  INPUT_UPSET = "input_upset",
  # List management sessions (listmgmt-session-*.R)
  LISTMGMT_MANAGER = "listmgmt_manager",
  # Utility sessions (utilities-session-*.R)
  UTILITIES_CONVERSION = "gconvert",
  UTILITIES_ORTHOLOGY = "gorth",
  UTILITIES_NETWORK = "string_network",
  # Enrichment sessions (enrich-*.R)
  ENRICH_FORM = "enrich_form"
)

#' Paradigm Identifier
#' @description Types of enrichment analysis
ParadigmId <- list(
  ORA = "ORA",
  GSEA = "GSEA",
  TOPOLOGY = "TOPOLOGY"
)

#' Tool Identifier
#' @description Available enrichment tools
# Most IDs one list may hold: plain lists (a gene set, e.g. the up-regulated genes of an
# experiment) and ranked lists (IDs with scores, often every measured gene, for GSEA).
MAX_LIST_IDS <- as.integer(Sys.getenv("FLAME_MAX_IDS", "20000"))
MAX_RANKED_IDS <- as.integer(Sys.getenv("FLAME_MAX_RANKED_IDS", "60000"))

ToolId <- list(
  GPROFILER = "gProfiler",
  STRING = "STRING",
  ENRICHR = "enrichR",
  WEBGESTALT = "WebGestalt",
  PANTHER = "PANTHER",
  GENECODIS = "GeneCodis",
  CLUSTERPROFILER = "clusterProfiler",
  REACTOME = "Reactome"
)

#' Workspace tool metadata
#' @description Centralized tool metadata for workspace filters and chip pickers.
WORKSPACE_TOOL_UI <- list(
  list(
    key = "gprofiler",
    label = "gProfiler",
    toolId = ToolId$GPROFILER
  ),
  list(
    key = "string",
    label = "STRING",
    toolId = ToolId$STRING
  ),
  list(
    key = "enrichr",
    label = "EnrichR",
    toolId = ToolId$ENRICHR
  ),
  list(
    key = "webgestalt",
    label = "WebGestalt",
    toolId = ToolId$WEBGESTALT
  ),
  list(
    key = "panther",
    label = "PANTHER",
    toolId = ToolId$PANTHER
  ),
  list(
    key = "genecodis",
    label = "GeneCodis",
    toolId = ToolId$GENECODIS
  ),
  list(
    key = "clusterprofiler",
    label = "clusterProfiler",
    toolId = ToolId$CLUSTERPROFILER
  ),
  list(
    key = "reactome",
    label = "Reactome",
    toolId = ToolId$REACTOME
  )
)

#' Output Type
#' @description Available visualization outputs
#' Heatmaps and Networks share the same 3 relationship types:
#' - TERM_GENE: Terms connected to their genes
#' - TERM_TERM: Terms connected by similarity
#' - GENE_GENE: Genes connected by shared terms
OutputType <- list(
  BARCHART = "Barchart",
  SCATTER = "Scatter",
  DOTPLOT = "DotPlot",
  HEATMAP_TERM_GENE = "Heatmap_TermGene",
  HEATMAP_TERM_TERM = "Heatmap_TermTerm",
  HEATMAP_GENE_GENE = "Heatmap_GeneGene",
  NETWORK_TERM_GENE = "Network_TermGene",
  NETWORK_TERM_TERM = "Network_TermTerm",
  NETWORK_GENE_GENE = "Network_GeneGene"
)

#' Comparison Tab Identifier
#' @description Tab IDs for the comparison screen
ComparisonTabId <- list(
  OVERVIEW = "overview",
  BUBBLE   = "bubble",
  HEATMAP  = "heatmap"
)

# =============================================================================
# 2. PARADIGMS - What each analysis type needs and produces
# =============================================================================

PARADIGMS <- list()

PARADIGMS[[ParadigmId$ORA]] <- list(
  name = "Over-Representation Analysis",
  requiredInputClass = NULL,
  outputs = c(
    OutputType$BARCHART,
    OutputType$SCATTER,
    OutputType$DOTPLOT,
    OutputType$HEATMAP_TERM_GENE,
    OutputType$HEATMAP_TERM_TERM,
    OutputType$HEATMAP_GENE_GENE,
    OutputType$NETWORK_TERM_GENE,
    OutputType$NETWORK_TERM_TERM,
    OutputType$NETWORK_GENE_GENE
  ),
  requiresBackground = FALSE
)

PARADIGMS[[ParadigmId$GSEA]] <- list(
  name = "Gene Set Enrichment Analysis",
  requiredInputClass = "RankedAnalyteList",
  outputs = PARADIGMS[[ParadigmId$ORA]]$outputs,
  requiresBackground = FALSE
)

PARADIGMS[[ParadigmId$TOPOLOGY]] <- list(
  name = "Topology-Based Enrichment",
  requiredInputClass = NULL,
  outputs = NULL,
  requiresBackground = NULL
)

# =============================================================================
# 2b. OUTPUT_TYPES_CONFIG - Maps OutputType to implementation details
# =============================================================================
#
# Bridges semantic OutputType enum values to:
#   - class: R6 class name (string) for registry lookup
#   - containerId: Container div ID suffix
#   - key: Storage key in session's .outputSessions list
#   - tabTitle: Display title (for top-level tabs)
#   - group: Parent tab group (NULL for top-level, "Heatmap"/"Network" for grouped)
#   - subTabTitle: Title within group (%s replaced with uiTermKeyword)

OUTPUT_TYPES_CONFIG <- list()

# Top-level tabs
OUTPUT_TYPES_CONFIG[[OutputType$BARCHART]] <- list(
  class = "BarchartOutputSession",
  containerId = "barchart_container",
  key = "barchart",
  uiTabId = "barchart",
  tabTitle = "Barchart",
  group = NULL
)

OUTPUT_TYPES_CONFIG[[OutputType$SCATTER]] <- list(
  class = "ScatterOutputSession",
  containerId = "scatterPlot_container",
  key = "scatter",
  uiTabId = "scatter",
  tabTitle = "Scatter Plot",
  group = NULL
)

OUTPUT_TYPES_CONFIG[[OutputType$DOTPLOT]] <- list(
  class = "DotPlotOutputSession",
  containerId = "dotPlot_container",
  key = "dotplot",
  uiTabId = "dotplot",
  tabTitle = "Dot Plot",
  group = NULL
)

# Heatmap group (3 sub-tabs)
OUTPUT_TYPES_CONFIG[[OutputType$HEATMAP_TERM_GENE]] <- list(
  class = "Heatmap1OutputSession",
  containerId = "heatmap1_container",
  key = "heatmap1",
  uiTabId = "heatmap-tg",
  group = "Heatmap",
  subTabTitle = "%s Vs Genes"
)

OUTPUT_TYPES_CONFIG[[OutputType$HEATMAP_TERM_TERM]] <- list(
  class = "Heatmap2OutputSession",
  containerId = "heatmap2_container",
  key = "heatmap2",
  uiTabId = "heatmap-tt",
  group = "Heatmap",
  subTabTitle = "%s Vs %s"
)

OUTPUT_TYPES_CONFIG[[OutputType$HEATMAP_GENE_GENE]] <- list(
  class = "Heatmap3OutputSession",
  containerId = "heatmap3_container",
  key = "heatmap3",
  uiTabId = "heatmap-gg",
  group = "Heatmap",
  subTabTitle = "Genes Vs Genes"
)

# Network group (3 sub-tabs)
OUTPUT_TYPES_CONFIG[[OutputType$NETWORK_TERM_GENE]] <- list(
  class = "Network1OutputSession",
  containerId = "network1_container",
  key = "network1",
  uiTabId = "network-tg",
  group = "Network",
  subTabTitle = "%s Vs Genes"
)

OUTPUT_TYPES_CONFIG[[OutputType$NETWORK_TERM_TERM]] <- list(
  class = "Network2OutputSession",
  containerId = "network2_container",
  key = "network2",
  uiTabId = "network-tt",
  group = "Network",
  subTabTitle = "%s Vs %s"
)

OUTPUT_TYPES_CONFIG[[OutputType$NETWORK_GENE_GENE]] <- list(
  class = "Network3OutputSession",
  containerId = "network3_container",
  key = "network3",
  uiTabId = "network-gg",
  group = "Network",
  subTabTitle = "Genes Vs Genes"
)

# Results table routing config (not part of OutputType enum).
RESULTS_TABLE_CONFIG <- list(
  class = "TableOutputSession",
  key = "table",
  uiTabId = "table",
  group = NULL,
  tabTitle = "Table",
  outputType = "Table"
)

# Results tab UI overrides.
# Runtime identity/routing lives in OUTPUT_TYPES_CONFIG + RESULTS_TABLE_CONFIG.
# These overrides only define presentation defaults where needed.
RESULTS_TAB_UI_OVERRIDES <- list(
  table = list(label = "Table", icon = "&#9776;", defaultVisible = TRUE),
  dotplot = list(label = "Dotplot", icon = "&#9679;", defaultVisible = TRUE, tabType = "ggiraph"),
  barchart = list(label = "Barchart", icon = "&#9632;", defaultVisible = TRUE),
  scatter = list(label = "Scatter", icon = "&#9830;"),
  `heatmap-tg` = list(label = "Heatmap (T\u00D7G)", icon = "&#9638;"),
  `heatmap-tt` = list(label = "Heatmap (T\u00D7T)", icon = "&#9638;"),
  `heatmap-gg` = list(label = "Heatmap (G\u00D7G)", icon = "&#9638;"),
  `network-tg` = list(label = "Network (T\u00D7G)", icon = "&#9678;"),
  `network-tt` = list(label = "Network (T\u00D7T)", icon = "&#9678;"),
  `network-gg` = list(label = "Network (G\u00D7G)", icon = "&#9678;")
)

RESULTS_TAB_UI_ORDER <- c(
  "dotplot",
  "barchart",
  "scatter",
  "heatmap-tg",
  "heatmap-tt",
  "heatmap-gg",
  "network-tg",
  "network-tt",
  "network-gg",
  "table"
)

# =============================================================================
# 2c. COMPARISON_TABS_CONFIG - Comparison screen tab definitions
# =============================================================================
#
# Maps ComparisonTabId values to tab metadata.
# Used by the comparison screen to route between overview, bubble, and heatmap.
# Actual session class wiring will be added when comparison sessions are built.

COMPARISON_TABS_CONFIG <- list(
  overview = list(tabId = "overview", label = "Overview", outputKey = "overview"),
  bubble   = list(tabId = "bubble",   label = "Bubble",   outputKey = "bubble"),
  heatmap  = list(tabId = "heatmap",  label = "Heatmap",  outputKey = "heatmap")
)

# =============================================================================
# 3. TOOLS - Consolidated tool configuration
# =============================================================================
#
# Each tool has the following fields:
#
#   name                  - Display name shown in UI
#   paradigms             - Character vector of ParadigmId values this tool supports
#
#   datasources           - Character vector of datasource display names (e.g., "GO:MF", "KEGG")
#                           For tools with organism-specific datasources, use organismDatasources instead
#   datasourceCodes       - Named list mapping display names to API codes
#                           Example: list("GO:MF" = "geneontology_Molecular_Function")
#   organismDatasources   - Named list of datasources per organism short_name (for enrichR, GeneCodis)
#   organismDatasourceCodes - Named list of datasource codes per organism (for enrichR)
#
#   namespaces            - Named list of gene ID namespace options
#                           Example: list("Entrez Gene Name" = "ENTREZGENE")
#   namespacesSpecial     - Named list of namespaces per special organism (overrides namespaces)
#   defaultNamespace      - Default namespace for this tool
#
#   metrics               - Named list of significance metric options
#                           Example: list("False discovery rate" = "fdr")
#   defaultMetricGenome   - Default metric when using genome background
#   defaultMetricBackground - Default metric when using custom background
#
#   supportsBackground    - Whether tool supports custom background gene sets
#
# =============================================================================

TOOLS <- list()

# -----------------------------------------------------------------------------
# gProfiler
# -----------------------------------------------------------------------------
TOOLS[[ToolId$GPROFILER]] <- list(
  name = "g:Profiler",
  paradigms = c(ParadigmId$ORA),

  datasources = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP",
                  "TF", "MIRNA", "CORUM", "HPA", "HP"),

  datasourceCodes = NULL,

  namespaces = list(
    "User Input" = "USERINPUT",
    "ENSEMBL Gene ID" = "ENSG",
    "ENSEMBL Protein ID" = "ENSP",
    "ENSEMBL Transcript ID" = "ENST",
    "Entrez Gene Name" = "ENTREZGENE",
    "Entrez Gene Accession" = "ENTREZGENE_ACC",
    "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME",
    "UniProt Gene Name" = "UNIPROT_GN",
    "UniProt Accession" = "UNIPROT_GN_ACC",
    "UniProt Archive" = "UNIPARC",
    "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC",
    "RefSeq mRNA" = "REFSEQ_MRNA",
    "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
    "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC",
    "EMBL Accession" = "EMBL",
    "ChEMBL" = "CHEMBL",
    "WIKIGENE ID" = "WIKIGENE"
  ),

  defaultNamespace = "USERINPUT",

  metrics = list(
    "g:SCS threshold" = "gSCS",
    "False discovery rate" = "fdr",
    "Bonferroni correction" = "bonferroni"
  ),

  defaultMetricGenome = "fdr",
  defaultMetricBackground = "bonferroni",
  supportsBackground = TRUE
)

# -----------------------------------------------------------------------------
# STRING
# -----------------------------------------------------------------------------
TOOLS[[ToolId$STRING]] <- list(
  name = "STRING",
  paradigms = c(ParadigmId$ORA),

  datasources = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP",
                  "INTERPRO", "PFAM", "UNIPROT", "DO", "BTO", "HP", "PUBMED"),

  datasourceCodes = list(
    "GO:MF" = "Function",
    "GO:CC" = "Component",
    "GO:BP" = "Process",
    "KEGG" = "KEGG",
    "REAC" = "RCTM",
    "WP" = "WikiPathways",
    "INTERPRO" = "InterPro",
    "PFAM" = "Pfam",
    "UNIPROT" = "Keyword",
    "DO" = "DISEASES",
    "BTO" = "TISSUES",
    "HP" = "HPO",
    "PUBMED" = "PMID"
  ),

  namespaces = list("ENSEMBL Protein ID" = "ENSP"),

  defaultNamespace = "ENSP",

  metrics = list(
    "False discovery rate" = "fdr",
    "P-value" = "p_value"
  ),

  defaultMetricGenome = "fdr",
  defaultMetricBackground = "p_value",
  supportsBackground = TRUE
)

# -----------------------------------------------------------------------------
# enrichR - Has organism-specific datasources
# -----------------------------------------------------------------------------
TOOLS[[ToolId$ENRICHR]] <- list(
  name = "Enrichr",
  paradigms = c(ParadigmId$ORA),

  organismDatasources = list(
    hsapiens = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways", "HP", "PANGLAO", "CELLMARKER", "AZIMUTH", "TABULA", "GENESIGDB", "RUMMAGENE"),
    mmusculus = c("GO:MF", "GO:CC", "GO:BP", "WP", "MGI"),
    dmelanogaster = c("GO:MF", "GO:CC", "GO:BP", "WP"),
    drerio = c("GO:MF", "GO:CC", "GO:BP", "WP"),
    scerevisiae = c("GO:MF", "GO:CC", "GO:BP", "WP", "KEGG"),
    celegans = c("GO:MF", "GO:CC", "GO:BP", "WP", "DO", "WBP", "WBBT"),
    btaurus = c("GO:MF", "GO:CC", "GO:BP", "REAC", "WP", "ORPHA", "HP", "MGI")
  ),

  organismDatasourceCodes = list(
    hsapiens = list(
      "GO:MF" = "GO_Molecular_Function_2026",
      "GO:CC" = "GO_Cellular_Component_2026",
      "GO:BP" = "GO_Biological_Process_2026",
      "KEGG" = "KEGG_2026",
      "REAC" = "Reactome_Pathways_2024",
      "WP" = "WikiPathways_2024_Human",
      "PANTHER Pathways" = "Panther_2016",
      "HP" = "Human_Phenotype_Ontology",
      "PANGLAO" = "PanglaoDB_Augmented_2021",
      "CELLMARKER" = "CellMarker_2024",
      "AZIMUTH" = "Azimuth_2023",
      "TABULA" = "Tabula_Sapiens",
      "GENESIGDB" = "GeneSigDB",
      "RUMMAGENE" = "Rummagene_signatures"
    ),
    mmusculus = list(
      "GO:MF" = "GO_Molecular_Function_2026",
      "GO:CC" = "GO_Cellular_Component_2026",
      "GO:BP" = "GO_Biological_Process_2026",
      "WP" = "WikiPathways_2024_Mouse",
      "MGI" = "KOMP2_Mouse_Phenotypes_2022"
    ),
    dmelanogaster = list(
      "GO:MF" = "GO_Molecular_Function_2018",
      "GO:CC" = "GO_Cellular_Component_2018",
      "GO:BP" = "GO_Biological_Process_2018",
      "WP" = "WikiPathways_2018"
    ),
    drerio = list(
      "GO:MF" = "GO_Molecular_Function_2018",
      "GO:CC" = "GO_Cellular_Component_2018",
      "GO:BP" = "GO_Biological_Process_2018",
      "WP" = "WikiPathways_2018"
    ),
    scerevisiae = list(
      "GO:MF" = "GO_Molecular_Function_2018",
      "GO:CC" = "GO_Cellular_Component_2018",
      "GO:BP" = "GO_Biological_Process_2018",
      "WP" = "WikiPathways_2018",
      "KEGG" = "KEGG_2019"
    ),
    celegans = list(
      "GO:MF" = "GO_Molecular_Function_2018",
      "GO:CC" = "GO_Cellular_Component_2018",
      "GO:BP" = "GO_Biological_Process_2018",
      "WP" = "WikiPathways_2018",
      "DO" = "Human_Diseases_from_WormBase_2018",
      "WBP" = "Phenotypes_WormBase_2018",
      "WBBT" = "Anatomic_Associations_WormBase_2018"
    ),
    btaurus = list(
      "GO:MF" = "GO_Molecular_Function_2021",
      "GO:CC" = "GO_Cellular_Component_2021",
      "GO:BP" = "GO_Biological_Process_2021",
      "REAC" = "Reactome_2016",
      "WP" = "WikiPathway_2021_Human",
      "ORPHA" = "Orphanet_Augmented_2021",
      "HP" = "Human_Phenotype_Ontology",
      "MGI" = "MGI_Mammalian_Phenotype_Level_4_2021"
    )
  ),

  organismSites = list(
    hsapiens = "Enrichr",
    mmusculus = "Enrichr",
    dmelanogaster = "FlyEnrichr",
    drerio = "FishEnrichr",
    scerevisiae = "YeastEnrichr",
    celegans = "WormEnrichr",
    btaurus = "OxEnrichr"
  ),

  namespaces = list(
    "Entrez Gene Name" = "ENTREZGENE",
    "User Input" = "USERINPUT"
  ),

  namespacesSpecial = list(
    dmelanogaster = list("User Input" = "USERINPUT"),
    scerevisiae = list("User Input" = "USERINPUT")
  ),

  defaultNamespace = "ENTREZGENE",

  metrics = list("Adjusted P-value" = "adjusted_pvalue"),

  defaultMetricGenome = "adjusted_pvalue",
  defaultMetricBackground = NULL,
  supportsBackground = FALSE
)

# -----------------------------------------------------------------------------
# WebGestalt
# -----------------------------------------------------------------------------
TOOLS[[ToolId$WEBGESTALT]] <- list(
  name = "WebGestalt",
  # GSEA in Flame runs through clusterProfiler/fgsea (execute_gsea_run), not WebGestaltR.
  paradigms = c(ParadigmId$ORA),

  datasources = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP",
                  "PANTHER Pathways", "DISGENET", "OMIM", "GLAD4U_DISEASE",
                  "DRUGBANK", "GLAD4U_DRUG", "HP"),

  datasourceCodes = list(
    "GO:MF" = "geneontology_Molecular_Function_noRedundant",
    "GO:CC" = "geneontology_Cellular_Component_noRedundant",
    "GO:BP" = "geneontology_Biological_Process_noRedundant",
    "KEGG" = "pathway_KEGG",
    "REAC" = "pathway_Reactome",
    "WP" = "pathway_Wikipathway",
    "PANTHER Pathways" = "pathway_Panther",
    "DISGENET" = "disease_Disgenet",
    "OMIM" = "disease_OMIM",
    "GLAD4U_DISEASE" = "disease_GLAD4U",
    "DRUGBANK" = "drug_DrugBank",
    "GLAD4U_DRUG" = "drug_GLAD4U",
    "HP" = "phenotype_Human_Phenotype_Ontology"
  ),

  namespaces = list(
    "Entrez Gene Accession" = "ENTREZGENE_ACC",
    "User Input" = "USERINPUT"
  ),

  defaultNamespace = "ENTREZGENE_ACC",

  metrics = list(
    "Benjamini-Hochberg" = "BH",
    "Benjamini-Yekutieli" = "BY",
    "Holm" = "holm",
    "Hochberg" = "hochberg",
    "Hommel" = "hommel",
    "Bonferroni adjustment" = "bonferroni",
    "Top 100" = "top"
  ),

  defaultMetricGenome = "BH",
  defaultMetricBackground = "top",
  supportsBackground = TRUE
)

# -----------------------------------------------------------------------------
# PANTHER
# -----------------------------------------------------------------------------
TOOLS[[ToolId$PANTHER]] <- list(
  name = "PANTHER",
  paradigms = c(ParadigmId$ORA),

  datasources = c("GO:MF", "GO:CC", "GO:BP", "REAC",
                  "GOSLIM:MF", "GOSLIM:CC", "GOSLIM:BP",
                  "PANTHER Pathways", "PANTHERPC"),

  datasourceCodes = list(
    "GO:MF" = "GO:0003674",
    "GO:CC" = "GO:0005575",
    "GO:BP" = "GO:0008150",
    "GOSLIM:MF" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_MF",
    "GOSLIM:CC" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_CC",
    "GOSLIM:BP" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_BP",
    "REAC" = "ANNOT_TYPE_ID_REACTOME_PATHWAY",
    "PANTHER Pathways" = "ANNOT_TYPE_ID_PANTHER_PATHWAY",
    "PANTHERPC" = "ANNOT_TYPE_ID_PANTHER_PC"
  ),

  namespaces = list("PANTHER Accession" = "PANTHER_ACC"),

  defaultNamespace = "PANTHER_ACC",

  metrics = list(
    "False discovery rate" = "FDR",
    "P-value" = "NONE",
    "Bonferroni" = "BONFERRONI"
  ),

  defaultMetricGenome = "FDR",
  defaultMetricBackground = "NONE",
  supportsBackground = TRUE,
  # PANTHER's gene lookup refuses long lists: larger lists are not sent to PANTHER.
  maxIds = 3000L
)

# -----------------------------------------------------------------------------
# GeneCodis - Has organism-specific datasources
# -----------------------------------------------------------------------------
TOOLS[[ToolId$GENECODIS]] <- list(
  name = "GeneCodis",
  paradigms = c(ParadigmId$ORA),

  organismDatasources = list(
    hsapiens = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways",
                 "HP", "OMIM", "MGI", "BioPlanet", "PharmGKB", "LINCS",
                 "CollecTRI", "MIRNA"),
    mmusculus = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways",
                  "MGI", "CollecTRI", "MIRNA"),
    rnorvegicus = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways",
                    "MGI", "CollecTRI", "MIRNA"),
    celegans = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    dmelanogaster = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    drerio = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways", "MGI"),
    clfamiliaris = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    ggallus = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    btaurus = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    sscrofa = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC"),
    athaliana = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "WP", "PANTHER Pathways"),
    osativa = c("GO:MF", "GO:CC", "GO:BP", "KEGG"),
    scerevisiae = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    ecoli = c("GO:MF", "GO:CC", "GO:BP", "KEGG")
  ),

  datasourceCodes = list(
    "GO:MF" = "GO_MF",
    "GO:CC" = "GO_CC",
    "GO:BP" = "GO_BP",
    "KEGG" = "KEGG",
    "REAC" = "Reactome",
    "WP" = "WikiPathways",
    "PANTHER Pathways" = "Panther",
    "HP" = "HPO",
    "OMIM" = "OMIM",
    "MGI" = "MGI",
    "BioPlanet" = "BioPlanet",
    "PharmGKB" = "PharmGKB",
    "LINCS" = "LINCS",
    "CollecTRI" = "CollecTRI",
    "MIRNA" = "miRTarBase"
  ),

  namespaces = list("User Input" = "USERINPUT"),

  defaultNamespace = "USERINPUT",

  metrics = list("False discovery rate" = "fdr"),

  defaultMetricGenome = "fdr",
  defaultMetricBackground = "fdr",
  supportsBackground = TRUE
)

# =============================================================================
# 4. ORGANISMS - Loaded from RDS files at package load time
# =============================================================================

ORGANISMS <- tryCatch(
  readRDS(getOrganismsPath("organismsDF.RDS")),
  error = function(e) readRDS("./organisms/organismsDF.RDS")
)

TOOL_ORGANISMS <- tryCatch(
  readRDS(getOrganismsPath("toolOrganismsList.RDS")),
  error = function(e) readRDS("./organisms/toolOrganismsList.RDS")
)

# -----------------------------------------------------------------------------
# clusterProfiler: GO via enrichGO/gseGO (OrgDb), KEGG via enrichKEGG/gseKEGG (current KEGG),
# all other sources from MSigDB via msigdbr (enricher/GSEA); see enrich-annotations-clusterprofiler.R
# -----------------------------------------------------------------------------
TOOLS[[ToolId$CLUSTERPROFILER]] <- list(
  name = "clusterProfiler",
  paradigms = c(ParadigmId$ORA, ParadigmId$GSEA),
  datasources = c("GO:BP", "GO:MF", "GO:CC", "KEGG", "REAC", "WP", "HP", "TF", "MIRNA",
                  "HALLMARK", "BIOCARTA", "PID", "CELLTYPE", "IMMUNESIGDB", "ONCOGENIC",
                  "POSITIONAL", "CGP", "CP", "KEGG_MEDICUS", "MIR_LEGACY", "TFT_LEGACY",
                  "3CA", "CGN", "CM", "VAX", "CPS"),
  # FLAME source -> msigdbr collection / subcollection (KEGG is not MSigDB's frozen
  # KEGG_LEGACY: it is downloaded from KEGG itself, see kegg_gson())
  datasourceCodes = list(
    "GO:BP" = c("C5", "GO:BP"), "GO:MF" = c("C5", "GO:MF"), "GO:CC" = c("C5", "GO:CC"),
    "KEGG" = c("KEGG", ""), "REAC" = c("C2", "CP:REACTOME"), "WP" = c("C2", "CP:WIKIPATHWAYS"),
    "HP" = c("C5", "HPO"), "TF" = c("C3", "TFT:GTRD"), "MIRNA" = c("C3", "MIR:MIRDB"),
    "HALLMARK" = c("H", ""), "BIOCARTA" = c("C2", "CP:BIOCARTA"), "PID" = c("C2", "CP:PID"),
    "CELLTYPE" = c("C8", ""), "IMMUNESIGDB" = c("C7", "IMMUNESIGDB"), "ONCOGENIC" = c("C6", ""),
    "POSITIONAL" = c("C1", ""), "CGP" = c("C2", "CGP"), "CP" = c("C2", "CP"),
    "KEGG_MEDICUS" = c("C2", "CP:KEGG_MEDICUS"), "MIR_LEGACY" = c("C3", "MIR:MIR_LEGACY"),
    "TFT_LEGACY" = c("C3", "TFT:TFT_LEGACY"), "3CA" = c("C4", "3CA"), "CGN" = c("C4", "CGN"),
    "CM" = c("C4", "CM"), "VAX" = c("C7", "VAX"), "CPS" = c("C9", "")
  ),
  namespaces = list("Entrez Gene Accession" = "ENTREZGENE_ACC"),
  defaultNamespace = "ENTREZGENE_ACC",
  metrics = list("Benjamini-Hochberg FDR" = "BH", "Benjamini-Yekutieli" = "BY",
                 "Bonferroni" = "bonferroni", "Holm" = "holm", "None (p-value)" = "none"),
  defaultMetricGenome = "BH",
  defaultMetricBackground = "BH",
  supportsBackground = TRUE
)

# -----------------------------------------------------------------------------
# Reactome Analysis Service (Reactome pathways only; curated for human, inferred
# by orthology for other species; no custom background)
# -----------------------------------------------------------------------------
TOOLS[[ToolId$REACTOME]] <- list(
  name = "Reactome",
  paradigms = c(ParadigmId$ORA),
  datasources = c("REAC"),
  datasourceCodes = list("REAC" = "REAC"),
  namespaces = list("Ensembl gene" = "ENSG", "UniProt" = "UNIPROTSWISSPROT", "User Input" = "USERINPUT"),
  defaultNamespace = "ENSG",
  metrics = list("False discovery rate (Benjamini-Hochberg)" = "fdr", "Uncorrected p-value" = "p_value"),
  defaultMetricGenome = "fdr",
  defaultMetricBackground = "fdr",
  supportsBackground = FALSE
)

# msigdbr species (NCBI taxonomy id -> msigdbr species name)
MSIGDB_SPECIES <- c(
  "9606" = "Homo sapiens", "10090" = "Mus musculus", "10116" = "Rattus norvegicus",
  "7955" = "Danio rerio", "7227" = "Drosophila melanogaster", "6239" = "Caenorhabditis elegans",
  "4932" = "Saccharomyces cerevisiae", "559292" = "Saccharomyces cerevisiae",
  "9913" = "Bos taurus", "9615" = "Canis lupus familiaris", "9031" = "Gallus gallus",
  "9823" = "Sus scrofa", "9544" = "Macaca mulatta", "9598" = "Pan troglodytes",
  "9796" = "Equus caballus", "9685" = "Felis catus", "8364" = "Xenopus tropicalis",
  "13616" = "Monodelphis domestica", "9258" = "Ornithorhynchus anatinus",
  "28377" = "Anolis carolinensis", "284812" = "Schizosaccharomyces pombe 972h-"
)
TOOL_ORGANISMS[[ToolId$CLUSTERPROFILER]] <- as.integer(names(MSIGDB_SPECIES))
TOOL_ORGANISMS[[ToolId$REACTOME]] <- intersect(c(9606L, 10090L, 10116L, 9913L, 9823L, 9615L, 9031L, 8364L, 7955L, 7227L, 6239L,
                                                 4932L, 559292L, 4896L, 284812L, 44689L, 5833L, 1773L), ORGANISMS$taxid)

# Special organisms with non-standard tool/namespace preferences
SPECIAL_ORGANISMS <- c("amellifera", "dmelanogaster")

SPECIAL_PREFERRED_TOOL <- list(
  amellifera = ToolId$GPROFILER,
  dmelanogaster = ToolId$ENRICHR
)

SPECIAL_PREFERRED_NAMESPACE <- list(
  amellifera = "BEEBASE",
  dmelanogaster = "USERINPUT"
)

# Additional namespaces available for specific organisms (per-tool)
ADDITIONAL_NAMESPACES <- list(
  amellifera = list(
    gProfiler = list("BeeBase ID" = "BEEBASE")
  ),
  dmelanogaster = list(
    gProfiler = list("FlyBase Gene ID" = "FLYBASE_GENE_ID")
  )
)

# Core namespaces shared across tools (fallback when tool has no specific namespaces)
CORE_NAMESPACES <- list(
  "ENSEMBL Gene ID" = "ENSG",
  "ENSEMBL Protein ID" = "ENSP",
  "ENSEMBL Transcript ID" = "ENST",
  "Entrez Gene Name" = "ENTREZGENE",
  "Entrez Gene Accession" = "ENTREZGENE_ACC",
  "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME",
  "UniProt Gene Name" = "UNIPROT_GN",
  "UniProt Accession" = "UNIPROT_GN_ACC",
  "UniProt Archive" = "UNIPARC",
  "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC",
  "RefSeq mRNA" = "REFSEQ_MRNA",
  "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
  "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC",
  "EMBL Accession" = "EMBL",
  "ChEMBL" = "CHEMBL",
  "WIKIGENE ID" = "WIKIGENE"
)

# Organism-specific namespaces (flattened for conversion utility)
ORGANISM_NAMESPACES <- list(
  amellifera = c("BeeBase ID" = "BEEBASE"),
  dmelanogaster = c("FlyBase Gene ID" = "FLYBASE_GENE_ID")
)

# Workspace-specific organism shortcuts for initial dropdown view.
# Full organism list is still available through "More organisms...".
WORKSPACE_COMMON_ORGANISM_TAXIDS <- c(
  9606L,   # Homo sapiens
  10090L,  # Mus musculus
  10116L,  # Rattus norvegicus
  7227L,   # Drosophila melanogaster
  7955L,   # Danio rerio
  6239L,   # Caenorhabditis elegans
  4932L,   # Saccharomyces cerevisiae
  3702L,   # Arabidopsis thaliana
  9913L,   # Bos taurus
  9823L,   # Sus scrofa
  9031L,   # Gallus gallus
  9615L,   # Canis lupus familiaris
  7460L    # Apis mellifera
)

buildWorkspaceIdFormats <- function() {
  # Canonical namespace pool for conversion-capable IDs
  namespacePool <- c("Gene Symbol" = "USERINPUT", CORE_NAMESPACES, unlist(ORGANISM_NAMESPACES))
  namespacePool <- namespacePool[!duplicated(unname(namespacePool))]

  commonValues <- c("USERINPUT", "ENSG", "ENSP", "ENTREZGENE", "UNIPROT_GN_ACC", "REFSEQ_MRNA")

  labelOverrides <- c(
    USERINPUT = "Gene Symbol",
    ENSG = "Ensembl Gene",
    ENSP = "Ensembl Protein",
    ENST = "Ensembl Transcript",
    ENTREZGENE = "Entrez Gene",
    ENTREZGENE_ACC = "Entrez Accession",
    ENTREZGENE_TRANS_NAME = "Entrez Transcript",
    UNIPROT_GN = "UniProt Gene Name",
    UNIPROT_GN_ACC = "UniProt Accession",
    UNIPARC = "UniProt Archive",
    REFSEQ_PEPTIDE_ACC = "RefSeq Protein",
    REFSEQ_MRNA = "RefSeq mRNA",
    REFSEQ_MRNA_ACC = "RefSeq mRNA Accession",
    REFSEQ_NCRNA_ACC = "RefSeq ncRNA",
    EMBL = "EMBL Accession",
    CHEMBL = "ChEMBL",
    WIKIGENE = "WIKIGENE ID",
    BEEBASE = "BeeBase ID",
    FLYBASE_GENE_ID = "FlyBase Gene ID"
  )

  examples <- c(
    USERINPUT = "TP53",
    ENSG = "ENSG00000141510",
    ENSP = "ENSP00000269305",
    ENST = "ENST00000269305",
    ENTREZGENE = "7157",
    ENTREZGENE_ACC = "7157",
    ENTREZGENE_TRANS_NAME = "NM_000546",
    UNIPROT_GN = "TP53_HUMAN",
    UNIPROT_GN_ACC = "P04637",
    UNIPARC = "UPI0000000001",
    REFSEQ_PEPTIDE_ACC = "NP_000537",
    REFSEQ_MRNA = "NM_000546",
    REFSEQ_MRNA_ACC = "NM_000546.6",
    REFSEQ_NCRNA_ACC = "NR_003287",
    EMBL = "M14695",
    CHEMBL = "CHEMBL4303840",
    WIKIGENE = "WIKIGENE:5243",
    BEEBASE = "GB41089",
    FLYBASE_GENE_ID = "FBgn0001234"
  )

  entries <- lapply(seq_along(namespacePool), function(i) {
    label <- names(namespacePool)[i]
    value <- as.character(namespacePool[[i]])
    displayLabel <- if (value %in% names(labelOverrides)) labelOverrides[[value]] else label
    example <- if (value %in% names(examples)) examples[[value]] else ""
    list(
      value = value,
      label = displayLabel,
      example = example,
      group = if (value %in% commonValues) "common" else "other",
      common = value %in% commonValues
    )
  })

  entries
}

WORKSPACE_ID_FORMATS <- buildWorkspaceIdFormats()

# =============================================================================
# 5. UI CONSTANTS - Colors, layouts, display structures
# =============================================================================

# -----------------------------------------------------------------------------
# Tab identification
# -----------------------------------------------------------------------------
TAB_NAMES <- list(
  "ALL" = "all",
  "GO:MF" = "gomf", "GO:CC" = "gocc", "GO:BP" = "gobp",
  "GOSLIM:MF" = "goslimmf", "GOSLIM:CC" = "goslimcc", "GOSLIM:BP" = "goslimbp",
  "KEGG" = "kegg", "REAC" = "reac", "WP" = "wp", "PANTHER Pathways" = "panther",
  "INTERPRO" = "interpro", "PFAM" = "pfam", "UNIPROT" = "uniprot", "PANTHERPC" = "pantherpc",
  "DO" = "do", "DISGENET" = "disgenet", "OMIM" = "omim", "GLAD4U_DISEASE" = "glad4udisease", "ORPHA" = "orpha",
  "DRUGBANK" = "drugbank", "GLAD4U_DRUG" = "glad4udrug",
  "BTO" = "brenda", "WBBT" = "wbbt", "TF" = "tf", "CollecTRI" = "collectri",
  "MIRNA" = "mirna", "CORUM" = "corum",
  "HPA" = "hpa", "HP" = "hp", "WBP" = "wbp", "MGI" = "mgi",
  "BioPlanet" = "bioplanet", "PharmGKB" = "pharmgkb", "LINCS" = "lincs",
  "PUBMED" = "pubmed"
)
TAB_NAMES_CODES <- as.character(TAB_NAMES)
ENRICHMENT_DATASOURCES <- names(TAB_NAMES[TAB_NAMES != "all"])

# -----------------------------------------------------------------------------
# Datasource display structure for dropdowns (grouped)
# -----------------------------------------------------------------------------
DATASOURCES_PRINT <- list(
  'Gene Ontology' = list(
    "Molecular Function (GO:MF)" = "GO:MF",
    "Cellular Component (GO:CC)" = "GO:CC",
    "Biological Process (GO:BP)" = "GO:BP",
    "GO Slim - Molecular Function" = "GOSLIM:MF",
    "GO Slim - Cellular Component" = "GOSLIM:CC",
    "GO Slim - Biological Process" = "GOSLIM:BP"
  ),
  'Biological Pathways' = list(
    "KEGG" = "KEGG", "Reactome" = "REAC",
    "WikiPathways" = "WP", "PANTHER Pathways" = "PANTHER Pathways", "BioPlanet" = "BioPlanet"
  ),
  'Diseases' = list(
    "Disease Ontology" = "DO", "DisGeNET" = "DISGENET",
    "OMIM" = "OMIM", "GLAD4U" = "GLAD4U_DISEASE", "Orphanet" = "ORPHA"
  ),
  'Proteins' = list(
    "Interpro" = "INTERPRO", "PFAM" = "PFAM",
    "UniProt keywords" = "UNIPROT",
    "PANTHER Protein Class" = "PANTHERPC",
    "CORUM" = "CORUM"
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
    "GLAD4U" = "GLAD4U_DRUG",
    "PharmGKB" = "PharmGKB",
    "LINCS" = "LINCS"
  ),
  'Regulatory motifs in DNA' = list(
    "TRANSFAC" = "TF", "CollecTRI TFs" = "CollecTRI", "miRTarBase" = "MIRNA"
  ),
  'Literature' = list(
    "PubMed publications (STRING)" = "PUBMED", "GeneSigDB published signatures" = "GENESIGDB",
    "Rummagene: gene sets from PubMed Central papers" = "RUMMAGENE"
  ),
  'Cell types' = list(
    "PanglaoDB markers" = "PANGLAO", "CellMarker 2024" = "CELLMARKER", "Azimuth cell types" = "AZIMUTH",
    "Tabula Sapiens" = "TABULA", "MSigDB cell type signatures (C8)" = "CELLTYPE"
  ),
  'MSigDB collections' = list(
    "Hallmark gene sets (H)" = "HALLMARK", "Positional gene sets (C1)" = "POSITIONAL",
    "Chemical and genetic perturbations (C2 CGP)" = "CGP", "Canonical pathways, other (C2 CP)" = "CP",
    "BioCarta pathways" = "BIOCARTA", "KEGG MEDICUS pathways (C2)" = "KEGG_MEDICUS", "PID pathways" = "PID",
    "miRNA targets, legacy (C3)" = "MIR_LEGACY", "TF targets, legacy (C3)" = "TFT_LEGACY",
    "Curated Cancer Cell Atlas (C4 3CA)" = "3CA", "Cancer gene neighborhoods (C4 CGN)" = "CGN",
    "Cancer modules (C4 CM)" = "CM",
    "Oncogenic signatures (C6)" = "ONCOGENIC", "Immunologic signatures (C7 ImmuneSigDB)" = "IMMUNESIGDB",
    "Vaccine response (C7 VAX)" = "VAX", "Computational perturbation signatures (C9)" = "CPS"
  )
)

# MSigDB as the Molecular Signatures Database organises it (collections H, C1-C9), for the GSEA
# gene-set picker. `sources` are FLAME source IDs, all served by clusterProfiler via msigdbr;
# `codes` are the msigdbr collection/subcollection for the set counts.
MSIGDB_COLLECTIONS <- list(
  list(code = "H", name = "Hallmark", text = "Well-defined biological states and processes; the usual first choice.",
       sources = c(HALLMARK = "Hallmark gene sets")),
  list(code = "C1", name = "Positional", text = "Genes of each human chromosome cytogenetic band.",
       sources = c(POSITIONAL = "Chromosome bands")),
  list(code = "C2", name = "Curated", text = "Pathway databases and published expression signatures.",
       sources = c(CGP = "Chemical and genetic perturbations (CGP)", REAC = "Reactome", WP = "WikiPathways",
                   KEGG = "KEGG pathways (current, from KEGG)", KEGG_MEDICUS = "KEGG MEDICUS",
                   BIOCARTA = "BioCarta", PID = "PID", CP = "Other canonical pathways")),
  list(code = "C3", name = "Regulatory targets", text = "Targets of transcription factors and microRNAs.",
       sources = c(TF = "TF targets, GTRD", TFT_LEGACY = "TF targets, legacy motifs", MIRNA = "miRNA targets, miRDB",
                   MIR_LEGACY = "miRNA targets, legacy motifs")),
  list(code = "C4", name = "Computational", text = "Cancer-oriented sets from microarray and single-cell data.",
       sources = c(`3CA` = "Curated Cancer Cell Atlas (3CA)", CGN = "Cancer gene neighborhoods", CM = "Cancer modules")),
  list(code = "C5", name = "Ontology", text = "Gene Ontology and Human Phenotype Ontology terms.",
       sources = c(`GO:BP` = "GO biological process", `GO:MF` = "GO molecular function", `GO:CC` = "GO cellular component",
                   HP = "Human Phenotype Ontology")),
  list(code = "C6", name = "Oncogenic signatures", text = "Pathways often dysregulated in cancer.",
       sources = c(ONCOGENIC = "Oncogenic signatures")),
  list(code = "C7", name = "Immunologic signatures", text = "Immune cell states and perturbations.",
       sources = c(IMMUNESIGDB = "ImmuneSigDB", VAX = "Vaccine response (HIPC)")),
  list(code = "C8", name = "Cell type signatures", text = "Marker genes of cell types from single-cell studies.",
       sources = c(CELLTYPE = "Cell type signatures")),
  list(code = "C9", name = "Computational perturbations", text = "Signatures from perturbation screens.",
       sources = c(CPS = "Computational perturbation signatures"))
)
# Sources whose term IDs are MSigDB set names (linked to the MSigDB set page).
MSIGDB_CARD_SOURCES <- c("HALLMARK", "BIOCARTA", "PID", "CELLTYPE", "IMMUNESIGDB", "ONCOGENIC", "POSITIONAL",
                         "CGP", "CP", "KEGG_MEDICUS", "MIR_LEGACY", "TFT_LEGACY", "3CA", "CGN", "CM", "VAX", "CPS")
GSEA_DEFAULT_DATASOURCES <- c("HALLMARK", "GO:BP", "REAC", "KEGG")

#' Number of gene sets per FLAME MSigDB source (human release; NA when msigdbr cannot say).
msigdb_set_counts <- function() {
  codes <- TOOLS[[ToolId$CLUSTERPROFILER]]$datasourceCodes
  cols <- tryCatch(as.data.frame(msigdbr::msigdbr_collections(db_species = "HS")), error = function(e) NULL)
  vapply(names(codes), function(src) {
    if (is.null(cols)) return(NA_integer_)
    cs <- codes[[src]]
    sub <- ifelse(is.na(cols$gs_subcollection), "", cols$gs_subcollection)
    n <- cols$num_genesets[cols$gs_collection == cs[1] & sub == cs[2]]
    if (length(n)) as.integer(n[1]) else NA_integer_
  }, integer(1))
}

DATASOURCES_DEFAULT_SELECTED <- c("GO:MF", "GO:CC", "GO:BP", "KEGG")

# -----------------------------------------------------------------------------
# Plot configuration
# -----------------------------------------------------------------------------
NETWORK_IDS <- c("network1", "network2", "network3")
HEATMAP_IDS <- c("heatmap1", "heatmap2", "heatmap3")

LAYOUT_CHOICES <- list(
  `layout_with_graphopt` = "Graph Opt",
  `layout_nicely` = "Fruchterman-Reingold",
  `layout_with_kk` = "Kamada-Kawai",
  `layout_with_mds` = "Multi-dimensional Scaling",
  `layout_as_tree` = "Tree",
  `layout_as_star` = "Star",
  `layout_in_circle` = "Circle",
  `layout_on_grid` = "Grid",
  `layout_randomly` = "Random"
)

# -----------------------------------------------------------------------------
# Colors
# -----------------------------------------------------------------------------
GENE_NODE_COLOR <- "#d1e1d9"

DATASOURCE_COLORS <- c(
  "GO:MF" = "#dc3912", "GO:BP" = "#ff9900", "GO:CC" = "#109618",
  "GOSLIM:MF" = "#e85d4a", "GOSLIM:BP" = "#ffb84d", "GOSLIM:CC" = "#4db34d",
  "KEGG" = "#dd4477", "REAC" = "#3366cc", "WP" = "#0099c6", "PANTHER Pathways" = "#634341",
  "INTERPRO" = "#8a5103", "PFAM" = "#b3b000", "UNIPROT" = "#55edeb", "PANTHERPC" = "#8b6f47",
  "DO" = "#f7c8fa", "DISGENET" = "#c0f0a1", "OMIM" = "#edebaf",
  "GLAD4U_DISEASE" = "#9f86d9", "ORPHA" = "#03fcc6", "DRUGBANK" = "#7d4a74", "GLAD4U_DRUG" = "#4a9091",
  "BTO" = "#f0d871", "WBBT" = "#9cb59c", "TF" = "#5574a6", "CollecTRI" = "#A67C52", "MIRNA" = "#22aa99",
  "CORUM" = "#66aa00", "HPA" = "#6633cc", "HP" = "#990099", "WBP" = "#fffd78", "MGI" = "#fc4503",
  "BioPlanet" = "#9370DB", "PharmGKB" = "#FF6B6B", "LINCS" = "#4ECDC4",
  "PUBMED" = "#cc9f9f", "GENE" = GENE_NODE_COLOR,
  "HALLMARK" = "#e6550d", "BIOCARTA" = "#31a354", "PID" = "#756bb1",
  "CELLTYPE" = "#de2d26", "IMMUNESIGDB" = "#3182bd", "ONCOGENIC" = "#636363",
  "POSITIONAL" = "#8c6d31", "CGP" = "#e7298a", "CP" = "#1b9e77", "KEGG_MEDICUS" = "#b5367a",
  "MIR_LEGACY" = "#66c2a5", "TFT_LEGACY" = "#8da0cb", "3CA" = "#d95f02", "CGN" = "#7570b3",
  "CM" = "#a6761d", "VAX" = "#6baed6", "CPS" = "#525252",
  "PANGLAO" = "#0f766e", "CELLMARKER" = "#14b8a6", "AZIMUTH" = "#a16207", "TABULA" = "#be185d",
  "GENESIGDB" = "#9f7aea", "RUMMAGENE" = "#b45309"
)

# Short labels for external datasource links shown in the results selection sidebar.
# Keep this config-driven so datasource/link wording stays consistent across UI modules.
DATASOURCE_LINK_PROVIDERS <- c(
  "GO:MF" = "QuickGO",
  "GO:CC" = "QuickGO",
  "GO:BP" = "QuickGO",
  "GOSLIM:MF" = "QuickGO",
  "GOSLIM:CC" = "QuickGO",
  "GOSLIM:BP" = "QuickGO",
  "KEGG" = "KEGG",
  "REAC" = "Reactome",
  "WP" = "WikiPathways",
  "PANTHER Pathways" = "PANTHER",
  "PANTHERPC" = "PANTHER",
  "INTERPRO" = "InterPro",
  "PFAM" = "InterPro",
  "UNIPROT" = "UniProt",
  "DO" = "JAX",
  "DISGENET" = "DisGeNET",
  "OMIM" = "OMIM",
  "ORPHA" = "Orphanet",
  "HP" = "Monarch",
  "WBP" = "WormBase",
  "WBBT" = "WormBase",
  "MGI" = "MGI",
  "BTO" = "OLS",
  "TF" = "TRANSFAC",
  "CollecTRI" = "GeneCards",
  "MIRNA" = "miRBase",
  "PharmGKB" = "ClinPGx",
  "BioPlanet" = "BioPlanet",
  "PUBMED" = "PubMed",
  "GENESIGDB" = "PubMed",
  "RUMMAGENE" = "PubMed Central"
)

# Results plot styling tokens (design-system driven).
# Shared by multi-trace plotly outputs (e.g., barchart/scatter) so visual
# selection behavior stays consistent and centrally configurable.
RESULTS_SOURCE_COLOR_FALLBACK <- "#9ca3af"

RESULTS_SELECTION_STYLES <- list(
  barchart = list(
    defaultLineColor = "rgba(0,0,0,0.3)",
    selectedLineColor = "black",
    defaultLineWidth = 1,
    selectedLineWidth = 2
  ),
  scatter = list(
    defaultLineColor = "rgb(0, 0, 0)",
    selectedLineColor = "black",
    defaultLineWidth = 1,
    selectedLineWidth = 3
  )
)

# -----------------------------------------------------------------------------
# Table format
# -----------------------------------------------------------------------------
ENRICHMENT_DF_COLNAMES <- c(
  "Source", "Term_ID", "Function", "P-value", "Term Size",
  "Query size", "Intersection Size", "Positive Hits"
)

# -----------------------------------------------------------------------------
# UI fallback strings
# -----------------------------------------------------------------------------
DEFAULT_NAMESPACE_TEXT <- "Default tool namespace conversions"
DEFAULT_METRIC_TEXT <- "Default tool metrics"
UI_TERM_KEYWORD <- list(functional = "functions")

# =============================================================================
# 6. ENRICHMENT TYPES - Configuration for different enrichment paradigms
# =============================================================================

DEFAULT_TOOL <- ToolId$STRING

ENRICHMENT_TYPES_CONFIG <- list(
  functional = list(
    id = "functional",
    label = "Functional Enrichment",
    tabsetPanelId = "toolTabsPanel",
    resultsPanelId = "functionalEnrichmentResultsPanel",
    clearButtonId = "enrich_form-enrichment_all_clear",
    closeEvent = "closeRunTab",
    datasources = ENRICHMENT_DATASOURCES,
    tools = c(ToolId$GPROFILER, ToolId$WEBGESTALT, ToolId$ENRICHR,
              ToolId$PANTHER, ToolId$GENECODIS),
    supportsCombination = TRUE
  )
)

# =============================================================================
# 7. HELPER FUNCTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# Analyte helpers
# -----------------------------------------------------------------------------

#' Get analyte UI configuration
#' @return List of analyte metadata entries.
getAnalyteUI <- function() {
  ANALYTE_UI
}

#' Get analyte metadata by enum value
#' @param analyteType Character enum value from AnalyteType
#' @return List entry from ANALYTE_UI, defaults to first entry when unknown
getAnalyteUIByType <- function(analyteType) {
  hit <- Filter(function(entry) identical(entry$analyteType, analyteType), ANALYTE_UI)
  if (length(hit) > 0) {
    return(hit[[1]])
  }
  ANALYTE_UI[[1]]
}

#' Get analyte metadata by key
#' @param key Character key used in UI payloads (e.g. "genes")
#' @return List entry from ANALYTE_UI, defaults to first entry when unknown
getAnalyteUIByKey <- function(key) {
  if (is.null(key) || length(key) == 0) return(ANALYTE_UI[[1]])
  key <- tolower(trimws(as.character(key)[1]))
  hit <- Filter(function(entry) identical(entry$key, key), ANALYTE_UI)
  if (length(hit) > 0) {
    return(hit[[1]])
  }
  ANALYTE_UI[[1]]
}

#' Normalize analyte payload values to AnalyteType enum
#' @param value Character value from client payloads
#' @return Character enum value from AnalyteType
normalizeAnalyteType <- function(value) {
  if (is.null(value) || length(value) == 0) return(AnalyteType$GENE)
  key <- tolower(trimws(as.character(value)[1]))
  if (nchar(key) == 0) return(AnalyteType$GENE)

  for (entry in ANALYTE_UI) {
    if (key %in% c(entry$key, entry$singularKey, tolower(entry$analyteType))) {
      return(entry$analyteType)
    }
  }

  AnalyteType$GENE
}

#' Get filter tabs for list sidebar
#' @return List of tab descriptors with keys and labels
getListFilterTabs <- function() {
  c(
    list(list(key = "all", label = "All")),
    lapply(ANALYTE_UI, function(entry) list(key = entry$key, label = entry$shortLabel))
  )
}

#' Get common organism taxids for workspace dropdown shortcuts
#' @return Integer vector of taxids
getWorkspaceCommonOrganismTaxids <- function() {
  WORKSPACE_COMMON_ORGANISM_TAXIDS
}

#' Get workspace default organism taxid
#' @return Character taxid
getWorkspaceDefaultOrganismTaxid <- function() {
  defaultTaxid <- "9606"
  organismTaxids <- as.character(ORGANISMS$taxid[!is.na(ORGANISMS$taxid)])
  if (!defaultTaxid %in% organismTaxids && length(organismTaxids) > 0) {
    defaultTaxid <- organismTaxids[[1]]
  }
  defaultTaxid
}

#' Format organism rows for workspace dropdowns
#' @param organismDF Data frame with ORGANISMS columns.
#' @return List of dropdown option metadata.
formatWorkspaceOrganismChoices <- function(organismDF) {
  if (is.null(organismDF) || nrow(organismDF) == 0) return(list())

  lapply(seq_len(nrow(organismDF)), function(i) {
    taxid <- as.character(organismDF$taxid[[i]])
    label <- as.character(organismDF$print_name[[i]])
    shortName <- organismDF$short_name[[i]]
    keggName <- organismDF$kegg_name[[i]]
    shortName <- if (is.null(shortName) || is.na(shortName)) "" else as.character(shortName)
    keggName <- if (is.null(keggName) || is.na(keggName)) "" else as.character(keggName)

    list(
      value = taxid,
      label = label,
      searchText = paste(label, taxid, shortName, keggName),
      shortName = shortName,
      keggName = keggName
    )
  })
}

#' Get a workspace organism by taxid
#' @param taxid Character or integer taxid.
#' @return Dropdown option metadata for one organism.
getWorkspaceOrganismByTaxid <- function(taxid = getWorkspaceDefaultOrganismTaxid()) {
  if (is.null(taxid) || length(taxid) == 0 || is.na(taxid[[1]])) {
    taxid <- getWorkspaceDefaultOrganismTaxid()
  }
  taxid <- as.character(taxid[[1]])
  organismDF <- ORGANISMS[
    !is.na(ORGANISMS$taxid) & as.character(ORGANISMS$taxid) == taxid,
    , drop = FALSE
  ]

  if (nrow(organismDF) == 0) {
    fallbackTaxid <- getWorkspaceDefaultOrganismTaxid()
    organismDF <- ORGANISMS[
      !is.na(ORGANISMS$taxid) & as.character(ORGANISMS$taxid) == fallbackTaxid,
      , drop = FALSE
    ]
  }

  choices <- formatWorkspaceOrganismChoices(organismDF[seq_len(min(1L, nrow(organismDF))), , drop = FALSE])
  if (length(choices) > 0) {
    return(choices[[1]])
  }

  list(
    value = "9606",
    label = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
    searchText = "Homo sapiens (Human) [NCBI Tax. ID: 9606] 9606 hsapiens hsa",
    shortName = "hsapiens",
    keggName = "hsa"
  )
}

#' Get common workspace organism choices
#' @param selectedTaxid Taxid that must be included in the choices.
#' @return List of dropdown option metadata.
getWorkspaceCommonOrganismChoices <- function(selectedTaxid = getWorkspaceDefaultOrganismTaxid()) {
  if (is.null(selectedTaxid) || length(selectedTaxid) == 0 || is.na(selectedTaxid[[1]])) {
    selectedTaxid <- getWorkspaceDefaultOrganismTaxid()
  }
  selectedTaxid <- as.character(selectedTaxid[[1]])
  organismDF <- ORGANISMS[, c("taxid", "print_name", "short_name", "kegg_name"), drop = FALSE]
  organismDF <- organismDF[!is.na(organismDF$taxid) & !is.na(organismDF$print_name), , drop = FALSE]
  organismDF <- organismDF[order(organismDF$print_name), , drop = FALSE]

  commonTaxids <- unique(c(selectedTaxid, as.character(getWorkspaceCommonOrganismTaxids())))
  commonIdx <- match(commonTaxids, as.character(organismDF$taxid))
  commonIdx <- commonIdx[!is.na(commonIdx)]
  commonDF <- organismDF[commonIdx, , drop = FALSE]

  if (nrow(commonDF) > 0) {
    commonDF <- commonDF[!duplicated(as.character(commonDF$taxid)), , drop = FALSE]
  }

  formatWorkspaceOrganismChoices(commonDF)
}

#' Search workspace organisms for server-backed dropdowns
#' @param query Search query.
#' @param limit Maximum results to return.
#' @return List of dropdown option metadata.
searchWorkspaceOrganisms <- function(query, limit = 50L) {
  if (is.null(query) || length(query) == 0 || is.na(query[[1]])) query <- ""
  query <- tolower(trimws(as.character(query[[1]])))
  if (is.null(limit) || length(limit) == 0 || is.na(limit[[1]])) limit <- 50L
  limit <- suppressWarnings(as.integer(limit[[1]]))
  if (length(limit) == 0 || is.na(limit) || limit < 1L) limit <- 50L
  if (nchar(query) < 2L) return(list())

  organismDF <- ORGANISMS[, c("taxid", "print_name", "short_name", "kegg_name"), drop = FALSE]
  organismDF <- organismDF[!is.na(organismDF$taxid) & !is.na(organismDF$print_name), , drop = FALSE]

  taxid <- as.character(organismDF$taxid)
  printName <- as.character(organismDF$print_name)
  shortName <- ifelse(is.na(organismDF$short_name), "", as.character(organismDF$short_name))
  keggName <- ifelse(is.na(organismDF$kegg_name), "", as.character(organismDF$kegg_name))

  haystack <- tolower(paste(printName, taxid, shortName, keggName))
  hit <- grepl(query, haystack, fixed = TRUE)
  if (!any(hit)) return(list())

  matched <- organismDF[hit, , drop = FALSE]
  matchedTaxid <- taxid[hit]
  matchedPrintName <- printName[hit]
  matchedShortName <- shortName[hit]
  matchedKeggName <- keggName[hit]
  matchedPrintLower <- tolower(matchedPrintName)
  matchedShortLower <- tolower(matchedShortName)
  matchedKeggLower <- tolower(matchedKeggName)

  score <- rep(100L, nrow(matched))
  score[matchedTaxid == query] <- 0L
  score[matchedShortLower == query | matchedKeggLower == query] <- pmin(
    score[matchedShortLower == query | matchedKeggLower == query],
    1L
  )
  score[startsWith(matchedPrintLower, query)] <- pmin(score[startsWith(matchedPrintLower, query)], 2L)
  score[grepl(query, matchedPrintLower, fixed = TRUE)] <- pmin(
    score[grepl(query, matchedPrintLower, fixed = TRUE)],
    3L
  )

  orderIdx <- order(score, matchedPrintName, matchedTaxid)
  matched <- matched[orderIdx, , drop = FALSE]
  matched <- matched[seq_len(min(limit, nrow(matched))), , drop = FALSE]

  formatWorkspaceOrganismChoices(matched)
}

#' Get workspace ID format options
#' @return List of ID format metadata entries
getWorkspaceIdFormats <- function() {
  WORKSPACE_ID_FORMATS
}

#' Get workspace conversion target format options
#'
#' g:Convert target namespaces use the legacy conversion namespace pool. This
#' excludes app-internal formats such as USERINPUT while preserving workspace
#' labels/examples for the shared dropdown UI.
#'
#' @return List of ID format metadata entries valid as configured g:Convert targets.
getWorkspaceConversionTargetFormats <- function() {
  targetValues <- unique(as.character(unname(c(CORE_NAMESPACES, unlist(ORGANISM_NAMESPACES)))))
  Filter(function(entry) entry$value %in% targetValues, WORKSPACE_ID_FORMATS)
}

#' Normalize workspace ID format values
#' @param value Character value from client payloads
#' @return Canonical namespace code (e.g. "USERINPUT", "ENSG")
normalizeWorkspaceIdFormat <- function(value) {
  if (is.null(value) || length(value) == 0) return("USERINPUT")
  normalized <- toupper(trimws(as.character(value)[1]))
  if (nchar(normalized) == 0) return("USERINPUT")

  # Backward compatibility with legacy payload values
  legacyAliases <- c(
    SYMBOL = "USERINPUT",
    ENSEMBL = "ENSG",
    ENTREZ = "ENTREZGENE",
    UNIPROT = "UNIPROT_GN_ACC"
  )
  if (normalized %in% names(legacyAliases)) {
    return(legacyAliases[[normalized]])
  }

  knownValues <- vapply(WORKSPACE_ID_FORMATS, function(entry) entry$value, character(1))
  if (normalized %in% knownValues) {
    return(normalized)
  }

  "USERINPUT"
}

#' Get display label for a workspace ID format
#' @param value Namespace code
#' @return Human-readable label
getWorkspaceIdFormatLabel <- function(value) {
  canonical <- normalizeWorkspaceIdFormat(value)
  entry <- Filter(function(item) identical(item$value, canonical), WORKSPACE_ID_FORMATS)
  if (length(entry) > 0) {
    return(entry[[1]]$label)
  }
  canonical
}

#' Format analyte list metadata for workspace UI controls
#'
#' @param name List name.
#' @param analyteList An AnalyteList instance.
#' @param missingOrganismLabel Fallback text when list organism metadata is absent.
#' @return List metadata used by rail items and list-selector dropdowns.
formatWorkspaceAnalyteListSummary <- function(name, analyteList,
                                              missingOrganismLabel = "Human") {
  itemCount <- if (!is.null(analyteList)) analyteList$size() else 0L
  analyteType <- if (!is.null(analyteList)) analyteList$analyteType else AnalyteType$GENE
  analyteMeta <- getAnalyteUIByType(analyteType)
  organismTaxId <- if (!is.null(analyteList)) analyteList$organismTaxId else NULL
  organismLabel <- if (!is.null(analyteList) && !is.null(analyteList$organismName)) {
    analyteList$organismName
  } else {
    missingOrganismLabel
  }
  idFormat <- if (!is.null(analyteList)) analyteList$idFormat else NULL
  idFormatLabel <- if (!is.null(idFormat) && length(idFormat) > 0) {
    getWorkspaceIdFormatLabel(idFormat[1])
  } else {
    NULL
  }

  metaParts <- Filter(function(value) !is.null(value) && nzchar(as.character(value)), list(
    as.character(itemCount),
    analyteMeta$label,
    organismLabel,
    idFormatLabel
  ))

  list(
    value = name,
    label = name,
    name = name,
    meta = paste(metaParts, collapse = " · "),
    searchText = paste(c(name, metaParts), collapse = " "),
    size = itemCount,
    analyteType = analyteType,
    analyteLabel = analyteMeta$label,
    listTypeKey = analyteMeta$key,
    organismTaxId = if (is.null(organismTaxId)) "" else as.character(organismTaxId),
    organismLabel = organismLabel,
    idFormat = if (is.null(idFormat)) "" else as.character(idFormat[1]),
    idFormatLabel = if (is.null(idFormatLabel)) "" else idFormatLabel
  )
}

#' Get workspace tool UI configuration
#' @return List of workspace tool metadata entries
getWorkspaceToolUI <- function() {
  WORKSPACE_TOOL_UI
}

#' Get workspace tool label by UI key
#' @param key Tool key used in workspace DOM (e.g., "gprofiler") or canonical ToolId value
#' @return Human-readable label
getWorkspaceToolLabel <- function(key) {
  if (is.null(key) || length(key) == 0) return("")
  raw <- trimws(as.character(key)[1])
  if (!nzchar(raw)) return("")
  lowerKey <- tolower(raw)

  entry <- Filter(function(item) identical(item$key, lowerKey), WORKSPACE_TOOL_UI)
  if (length(entry) > 0) {
    return(entry[[1]]$label)
  }

  entry <- Filter(function(item) identical(item$toolId, raw), WORKSPACE_TOOL_UI)
  if (length(entry) > 0) {
    return(entry[[1]]$label)
  }
  raw
}

#' Get canonical ToolId value from workspace tool key
#' @param key Workspace tool key (e.g. "gprofiler")
#' @return Canonical ToolId value (e.g. "gProfiler"), or NULL when unknown
getWorkspaceToolIdByKey <- function(key) {
  if (is.null(key) || length(key) == 0) return(NULL)
  raw <- trimws(as.character(key)[1])
  if (!nzchar(raw)) return(NULL)
  lowerKey <- tolower(raw)

  entry <- Filter(function(item) identical(item$key, lowerKey), WORKSPACE_TOOL_UI)
  if (length(entry) > 0) {
    return(entry[[1]]$toolId)
  }

  if (raw %in% names(TOOLS)) {
    return(raw)
  }

  NULL
}

#' Get workspace tool key from canonical ToolId value
#' @param toolId Canonical ToolId value (e.g. "gProfiler")
#' @return Workspace key (e.g. "gprofiler"), or lowercased fallback
getWorkspaceToolKeyById <- function(toolId) {
  if (is.null(toolId) || length(toolId) == 0) return("")
  raw <- trimws(as.character(toolId)[1])
  if (!nzchar(raw)) return("")

  entry <- Filter(function(item) identical(item$toolId, raw), WORKSPACE_TOOL_UI)
  if (length(entry) > 0) {
    return(entry[[1]]$key)
  }

  tolower(raw)
}

# -----------------------------------------------------------------------------
# Tool helpers
# -----------------------------------------------------------------------------

#' Get tools that support a given paradigm
#' @param paradigmId A paradigm identifier from ParadigmId
#' @return Character vector of tool IDs
getToolsForParadigm <- function(paradigmId) {
  names(Filter(function(t) paradigmId %in% t$paradigms, TOOLS))
}

#' Get tools that support a given organism
#' @param taxid Organism taxid
#' @return Character vector of tool IDs
getToolsForOrganism <- function(taxid) {
  names(which(sapply(TOOL_ORGANISMS, function(tool) taxid %in% tool)))
}

#' Get all tool IDs
#' @return Character vector of tool IDs
getAllToolIds <- function() {
  names(TOOLS)
}

# -----------------------------------------------------------------------------
# Datasource helpers
# -----------------------------------------------------------------------------

#' Get datasources for a tool, optionally filtered by organism
#' @param toolId A tool identifier from ToolId
#' @param organism Optional organism short_name for organism-specific tools
#' @return Character vector of datasource identifiers
getDatasourcesForTool <- function(toolId, organism = NULL) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)

  if (!is.null(tool$organismDatasources) && !is.null(organism)) {
    return(tool$organismDatasources[[organism]])
  }

  return(tool$datasources)
}

#' Get datasource code for API call
#' @param toolId A tool identifier from ToolId
#' @param datasource A datasource display name
#' @param organism Optional organism for organism-specific tools
#' @return The API code for the datasource, or the datasource itself if no mapping
getDatasourceCode <- function(toolId, datasource, organism = NULL) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(datasource)

  if (!is.null(tool$organismDatasourceCodes) && !is.null(organism)) {
    codes <- tool$organismDatasourceCodes[[organism]]
    if (!is.null(codes) && !is.null(codes[[datasource]])) {
      return(codes[[datasource]])
    }
  }

  if (!is.null(tool$datasourceCodes) && !is.null(tool$datasourceCodes[[datasource]])) {
    return(tool$datasourceCodes[[datasource]])
  }

  return(datasource)
}

#' Get external provider label for datasource links
#'
#' @param datasource Datasource identifier (e.g. "GO:BP")
#' @return Short provider label (e.g. "QuickGO"), or NULL when none is configured
getDatasourceLinkProvider <- function(datasource) {
  if (is.null(datasource) || length(datasource) == 0) return(NULL)
  key <- as.character(datasource[[1]])
  if (!nzchar(key)) return(NULL)
  if (!key %in% names(DATASOURCE_LINK_PROVIDERS)) return(NULL)
  as.character(DATASOURCE_LINK_PROVIDERS[[key]])
}

# -----------------------------------------------------------------------------
# Namespace helpers
# -----------------------------------------------------------------------------

#' Get namespaces for a tool
#' @param toolId Tool identifier
#' @param organismShortName Optional organism short name for special namespaces
#' @return Named list of namespaces
getNamespacesForTool <- function(toolId, organismShortName = NULL) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)

  # Check for organism-specific overrides
  if (!is.null(tool$namespacesSpecial) && !is.null(organismShortName)) {
    special <- tool$namespacesSpecial[[organismShortName]]
    if (!is.null(special)) return(special)
  }

  # Get base namespaces
  namespaces <- tool$namespaces

  # Add additional namespaces for special organisms
  if (!is.null(organismShortName)) {
    additional <- getAdditionalNamespaces(organismShortName, toolId)
    if (!is.null(additional)) {
      namespaces <- c(additional, namespaces)
    }
  }

  return(namespaces)
}

#' Get default target namespace for a tool
#' @param toolId Tool identifier from ToolId
#' @param organism Optional organism taxid for organism-specific defaults
#' @return Namespace code string
getDefaultTargetNamespace <- function(toolId, organism = NULL) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)

  # Check for organism-specific override (enrichR special cases)
  if (!is.null(organism) && toolId == ToolId$ENRICHR) {
    shortName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name
    if (shortName %in% c("scerevisiae", "dmelanogaster")) {
      return("USERINPUT")
    }
  }

  return(tool$defaultNamespace)
}

#' Check if an organism has additional namespaces for a tool
#' @param organismShortName The organism short_name
#' @param toolId The tool identifier (optional)
#' @return Logical
hasAdditionalNamespaces <- function(organismShortName, toolId = NULL) {
  if (is.null(toolId)) {
    return(organismShortName %in% names(ADDITIONAL_NAMESPACES))
  }
  orgConfig <- ADDITIONAL_NAMESPACES[[organismShortName]]
  if (is.null(orgConfig)) return(FALSE)
  return(toolId %in% names(orgConfig))
}

#' Get additional namespaces for an organism and tool
#' @param organismShortName The organism short_name
#' @param toolId The tool identifier
#' @return Named list of additional namespaces, or NULL if none
getAdditionalNamespaces <- function(organismShortName, toolId) {
  orgConfig <- ADDITIONAL_NAMESPACES[[organismShortName]]
  if (is.null(orgConfig)) return(NULL)
  return(orgConfig[[toolId]])
}

# -----------------------------------------------------------------------------
# Metric helpers
# -----------------------------------------------------------------------------

#' Get metrics for a tool
#' @param toolId Tool identifier
#' @return Named list of metrics
getMetricsForTool <- function(toolId) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)
  return(tool$metrics)
}

#' Get default metric for a tool
#' @param toolId Tool identifier from ToolId
#' @param hasBackground Whether a custom background is being used
#' @return The default metric code
getDefaultMetric <- function(toolId, hasBackground = FALSE) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)

  if (hasBackground && !is.null(tool$defaultMetricBackground)) {
    return(tool$defaultMetricBackground)
  }

  return(tool$defaultMetricGenome)
}

#' Get display label for a tool metric code
#' @param toolId Tool identifier from ToolId
#' @param metricCode Metric code value (e.g., "fdr", "BH", "top")
#' @param abbreviate Whether to apply compact label normalization
#' @return Character metric label suitable for UI display
getMetricLabelForTool <- function(toolId, metricCode, abbreviate = TRUE) {
  code <- trimws(as.character(metricCode %||% ""))
  if (!nzchar(code)) return("")

  metricLabel <- code
  tool <- TOOLS[[toolId]]
  if (!is.null(tool) && !is.null(tool$metrics) && length(tool$metrics) > 0) {
    metricNames <- names(tool$metrics)
    metricValues <- as.character(unname(tool$metrics))
    metricIdx <- match(code, metricValues)
    if (!is.na(metricIdx) && metricIdx > 0 && metricIdx <= length(metricNames)) {
      metricLabel <- as.character(metricNames[[metricIdx]])
    }
  }

  if (!isTRUE(abbreviate)) return(metricLabel)

  metricLabel <- gsub(" threshold$", "", metricLabel, ignore.case = TRUE)
  metricLabel <- gsub("^False discovery rate$", "FDR", metricLabel, ignore.case = TRUE)
  metricLabel <- gsub("^Benjamini-Hochberg$", "BH", metricLabel, ignore.case = TRUE)
  metricLabel <- gsub("^Benjamini-Yekutieli$", "BY", metricLabel, ignore.case = TRUE)
  metricLabel
}

#' Format significance text for run metadata display
#' @param toolId Tool identifier from ToolId
#' @param metricCode Metric code value (e.g., "fdr", "BH", "top")
#' @param threshold Numeric or character threshold
#' @param abbreviate Whether to abbreviate metric labels
#' @return Character significance text (e.g., "FDR < 0.05")
formatSignificanceText <- function(toolId, metricCode, threshold, abbreviate = TRUE) {
  code <- trimws(as.character(metricCode %||% ""))
  if (identical(toolId, ToolId$WEBGESTALT) && identical(tolower(code), "top")) {
    return("Top 100 terms (rank-based)")
  }

  metricLabel <- getMetricLabelForTool(toolId, code, abbreviate = abbreviate)
  if (!nzchar(metricLabel)) metricLabel <- code

  thresholdText <- trimws(as.character(threshold %||% ""))
  if (!nzchar(thresholdText)) return(metricLabel)

  paste0(metricLabel, " < ", thresholdText)
}

# -----------------------------------------------------------------------------
# Organism helpers
# -----------------------------------------------------------------------------

#' Check if organism is a special organism
#' @param organismShortName Organism short name
#' @return Logical
isSpecialOrganism <- function(organismShortName) {
  organismShortName %in% SPECIAL_ORGANISMS
}

#' Get preferred tool for a special organism
#' @param organismShortName Organism short name
#' @return Tool ID or NULL
getPreferredToolForOrganism <- function(organismShortName) {
  SPECIAL_PREFERRED_TOOL[[organismShortName]]
}

#' Get preferred namespace for a special organism
#' @param organismShortName Organism short name
#' @return Namespace code or NULL
getPreferredNamespaceForOrganism <- function(organismShortName) {
  SPECIAL_PREFERRED_NAMESPACE[[organismShortName]]
}

# -----------------------------------------------------------------------------
# Paradigm/Output helpers
# -----------------------------------------------------------------------------

#' Get outputs available for a paradigm
#' @param paradigmId A paradigm identifier from ParadigmId
#' @return Character vector of output types
getOutputsForParadigm <- function(paradigmId) {
  paradigm <- PARADIGMS[[paradigmId]]
  if (is.null(paradigm)) return(NULL)
  return(paradigm$outputs)
}

inferResultsTabType <- function(tabId) {
  id <- trimws(as.character(tabId %||% ""))
  if (!nzchar(id)) return("plotly")
  if (identical(id, as.character(RESULTS_TABLE_CONFIG$uiTabId %||% ""))) return("table")
  if (startsWith(id, "network-")) return("network")
  "plotly"
}

inferResultsTabLabel <- function(tabId, cfg = NULL) {
  tabTitle <- as.character(cfg$tabTitle %||% "")
  if (nzchar(tabTitle)) return(tabTitle)

  id <- trimws(as.character(tabId %||% ""))
  if (!nzchar(id)) return("Plot")
  label <- gsub("[-_]+", " ", id)
  if (!nzchar(label)) return("Plot")
  tools::toTitleCase(label)
}

getDefaultResultsTabIcon <- function(tabType) {
  type <- trimws(as.character(tabType %||% "plotly"))
  if (identical(type, "network")) return("&#9678;")
  if (identical(type, "table")) return("&#9776;")
  "&#9679;"
}

buildResultsTabUiDescriptor <- function(cfg, tabId = NULL) {
  if (is.null(cfg) || !is.list(cfg)) return(NULL)

  resolvedTabId <- trimws(as.character(tabId %||% cfg$uiTabId %||% ""))
  if (!nzchar(resolvedTabId)) return(NULL)

  override <- RESULTS_TAB_UI_OVERRIDES[[resolvedTabId]] %||% list()
  tabType <- as.character(override$tabType %||% inferResultsTabType(resolvedTabId))
  defaultVisible <- isTRUE(override$defaultVisible)
  hasArena3D <- if (!is.null(override$hasArena3D)) {
    isTRUE(override$hasArena3D)
  } else {
    identical(tabType, "network")
  }

  list(
    tabId = resolvedTabId,
    outputKey = as.character(cfg$key %||% ""),
    label = as.character(override$label %||% inferResultsTabLabel(resolvedTabId, cfg)),
    icon = as.character(override$icon %||% getDefaultResultsTabIcon(tabType)),
    defaultVisible = defaultVisible,
    hasArena3D = hasArena3D,
    tabType = tabType
  )
}

#' Get canonical v4 results tab UI configuration
#' @return List of tab descriptor lists
getResultsTabUiConfig <- function() {
  tabs <- list()

  for (outputType in names(OUTPUT_TYPES_CONFIG)) {
    cfg <- OUTPUT_TYPES_CONFIG[[outputType]]
    descriptor <- buildResultsTabUiDescriptor(cfg)
    if (!is.null(descriptor)) tabs[[length(tabs) + 1L]] <- descriptor
  }

  tableDescriptor <- buildResultsTabUiDescriptor(
    RESULTS_TABLE_CONFIG,
    tabId = as.character(RESULTS_TABLE_CONFIG$uiTabId %||% "table")
  )
  if (!is.null(tableDescriptor)) tabs[[length(tabs) + 1L]] <- tableDescriptor

  deduped <- list()
  seen <- character(0)
  for (tab in tabs) {
    tabId <- as.character(tab$tabId %||% "")
    if (!nzchar(tabId) || tabId %in% seen) next
    deduped[[length(deduped) + 1L]] <- tab
    seen <- c(seen, tabId)
  }

  tabIds <- vapply(
    deduped,
    function(tab) as.character(tab$tabId %||% ""),
    character(1),
    USE.NAMES = FALSE
  )
  uiOrder <- match(tabIds, RESULTS_TAB_UI_ORDER)
  fallbackOrder <- seq_along(deduped)
  deduped[order(ifelse(is.na(uiOrder), Inf, uiOrder), fallbackOrder)]
}

#' Get v4 results tab UI descriptor by tab ID
#' @param tabId Results tab ID
#' @return Descriptor list or NULL
getResultsTabUiConfigById <- function(tabId) {
  target <- trimws(as.character(tabId %||% ""))
  if (!nzchar(target)) return(NULL)
  matches <- Filter(
    function(tab) identical(as.character(tab$tabId %||% ""), target),
    getResultsTabUiConfig()
  )
  if (length(matches) == 0) return(NULL)
  matches[[1]]
}

#' Get v4 results tab UI configuration for client-side bootstrap
#' @return List of scalar lists (JSON-ready)
getResultsTabUiConfigForClient <- function() {
  tabs <- getResultsTabUiConfig()
  lapply(tabs, function(tab) {
    list(
      tabId = as.character(tab$tabId %||% ""),
      outputKey = as.character(tab$outputKey %||% ""),
      label = as.character(tab$label %||% ""),
      icon = as.character(tab$icon %||% ""),
      defaultVisible = isTRUE(tab$defaultVisible),
      hasArena3D = isTRUE(tab$hasArena3D),
      tabType = as.character(tab$tabType %||% "plotly")
    )
  })
}

#' Get placeholder text copy for a results tab state
#' @param tabId Results tab ID
#' @param state Placeholder state (`needs_generate`, `no_terms_after_filters`, `ready`)
#' @return Named list with `title` and `hint`
getResultsTabPlaceholderCopy <- function(tabId, state = "needs_generate") {
  tab <- getResultsTabUiConfigById(tabId)
  label <- as.character(tab$label %||% "Plot")
  stateKey <- trimws(as.character(state %||% "needs_generate"))

  if (identical(stateKey, "no_terms_after_filters")) {
    return(list(
      title = "No terms match current filters",
      hint = "Broaden filters in the left panel"
    ))
  }

  if (identical(stateKey, "ready")) {
    return(list(
      title = "",
      hint = ""
    ))
  }

  list(
    title = paste0(label, " visualization"),
    hint = "Use Generate in the toolbar"
  )
}

#' Get output configuration by results tab ID
#' @param tabId Results tab ID (e.g. "dotplot", "heatmap-tg", "table")
#' @return Output config list or NULL
getOutputConfigByTabId <- function(tabId) {
  tabId <- as.character(tabId %||% "")
  if (!nzchar(tabId)) return(NULL)

  if (identical(tabId, RESULTS_TABLE_CONFIG$uiTabId)) {
    return(RESULTS_TABLE_CONFIG)
  }

  for (outputType in names(OUTPUT_TYPES_CONFIG)) {
    cfg <- OUTPUT_TYPES_CONFIG[[outputType]]
    cfgTabId <- as.character(cfg$uiTabId %||% "")
    if (identical(cfgTabId, tabId)) {
      cfg$outputType <- outputType
      return(cfg)
    }
  }

  NULL
}

#' Get output session key by results tab ID
#' @param tabId Results tab ID
#' @return Session key (e.g. "dotplot", "heatmap1", "table") or NULL
getOutputSessionKeyByTabId <- function(tabId) {
  cfg <- getOutputConfigByTabId(tabId)
  if (is.null(cfg)) return(NULL)
  as.character(cfg$key %||% NULL)
}

#' Get results tab ID by output session key
#' @param outputKey Output session key (e.g. "dotplot", "heatmap1")
#' @return Results tab ID (e.g. "dotplot", "heatmap-tg") or NULL
getResultsTabIdByOutputKey <- function(outputKey) {
  key <- as.character(outputKey %||% "")
  if (!nzchar(key)) return(NULL)

  for (cfg in OUTPUT_TYPES_CONFIG) {
    cfgKey <- as.character(cfg$key %||% "")
    if (identical(cfgKey, key)) {
      return(as.character(cfg$uiTabId %||% NULL))
    }
  }

  NULL
}

#' Get all supported results tab IDs
#' @return Character vector of results tab IDs
getAllResultsTabIds <- function() {
  ids <- vapply(
    getResultsTabUiConfig(),
    function(tab) as.character(tab$tabId %||% ""),
    character(1),
    USE.NAMES = FALSE
  )
  ids <- ids[nzchar(ids)]
  unique(ids)
}

# -----------------------------------------------------------------------------
# Comparison tab helpers
# -----------------------------------------------------------------------------

#' Get comparison tab configuration by tab ID
#' @param tabId Comparison tab ID (e.g. "overview", "bubble", "heatmap")
#' @return Config list from COMPARISON_TABS_CONFIG or NULL
getComparisonSessionByTabId <- function(tabId) {
  tabId <- as.character(tabId %||% "")
  if (!nzchar(tabId)) return(NULL)
  COMPARISON_TABS_CONFIG[[tabId]]
}

#' Get placeholder copy for a comparison tab
#' @param tabId Comparison tab ID (e.g. "overview", "bubble", "heatmap")
#' @param state Placeholder state: "needs_generate", "no_terms_after_filters", or "ready"
#' @return List with title and hint strings
getComparisonTabPlaceholderCopy <- function(tabId, state = "needs_generate") {
  tab <- getComparisonSessionByTabId(tabId)
  label <- as.character(tab$label %||% "Chart")
  stateKey <- trimws(as.character(state %||% "needs_generate"))

  if (identical(stateKey, "no_terms_after_filters")) {
    return(list(
      title = "No terms match current filters",
      hint = "Broaden filters in the left panel"
    ))
  }

  if (identical(stateKey, "ready")) {
    return(list(
      title = "",
      hint = ""
    ))
  }

  list(
    title = paste0(label, " visualization"),
    hint = "Use Generate in the toolbar"
  )
}

# -----------------------------------------------------------------------------
# Enrichment type helpers
# -----------------------------------------------------------------------------

#' Get Enrichment Type Configuration
#' @param type Character string: "functional", etc.
#' @return List containing all configuration properties for the type
getEnrichmentConfig <- function(type) {
  config <- ENRICHMENT_TYPES_CONFIG[[type]]
  if (is.null(config)) {
    stop(paste("Unknown enrichment type:", type))
  }
  return(config)
}

#' Get Valid Datasources for Enrichment Type
#' @param type Character string: "functional", etc.
#' @return Character vector of valid datasource names
getValidDatasources <- function(type) {
  return(getEnrichmentConfig(type)$datasources)
}

#' Check if Enrichment Type Supports Combination
#' @param type Character string: "functional", etc.
#' @return Logical TRUE if combination tab is supported
supportsCombination <- function(type) {
  config <- getEnrichmentConfig(type)
  return(isTRUE(config$supportsCombination))
}

#' Get All Registered Enrichment Types
#' @return Character vector of registered type IDs
getEnrichmentTypes <- function() {
  return(names(ENRICHMENT_TYPES_CONFIG))
}

# =============================================================================
# 8. WORKSPACE CONFIGURATION
# =============================================================================

#' Get maximum number of analyte lists allowed
#'
#' Reads from FLAME_MAX_LISTS environment variable or flame.max_lists option.
#' Default is 10. Set to 0 or Inf for unlimited.
#'
#' @return Integer or Inf
#' @export
getMaxLists <- function() {
  # Check environment variable first
  env_val <- Sys.getenv("FLAME_MAX_LISTS", unset = NA)
  if (!is.na(env_val)) {
    val <- as.numeric(env_val)
    if (!is.na(val)) {
      if (val <= 0) return(Inf)
      return(as.integer(val))
    }
  }

  # Check R option
  opt_val <- getOption("flame.max_lists", default = NULL)
  if (!is.null(opt_val)) {
    val <- as.numeric(opt_val)
    if (!is.na(val)) {
      if (val <= 0) return(Inf)
      return(as.integer(val))
    }
  }

  # Default
  return(10L)
}
