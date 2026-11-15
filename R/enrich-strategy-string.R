# =============================================================================
# STRING ORA Strategy
# =============================================================================
#
# Implements Over-Representation Analysis using the STRING API.
#
# Dependencies:
#   - config.R (for ToolId, ParadigmId, ENRICHMENT_DF_COLNAMES)
#   - enrich-strategy-base.R (for EnrichmentStrategy, strategyRegistry)
#   - aaa-utilities.R (for renderWarning)
#   - httr package
#
# =============================================================================

STRINGORAStrategy <- R6::R6Class("STRINGORAStrategy",

  inherit = EnrichmentStrategy,

  public = list(
    initialize = function() {
      super$initialize(ToolId$STRING, ParadigmId$ORA)
    },

    run = function(inputList, organism, backgroundList, params) {
      # Send enrichment request
      response <- private$sendRequest(inputList, organism, backgroundList)

      if (!private$isResponseValid(response)) {
        return(NULL)
      }

      # Parse API response
      result <- private$parseResponse(response, params$metric)

      if (!private$isResultValid(result)) {
        return(NULL)
      }

      # Filter by user's selected datasources
      if (!is.null(params$datasources) && length(params$datasources) > 0) {
        result <- result[result$Source %in% params$datasources, ]
      }

      # Filter by threshold
      threshold <- as.numeric(params$threshold)
      result <- result[result$`P-value` <= threshold, ]

      if (nrow(result) == 0) {
        return(NULL)
      }

      # Return structured result
      return(list(
        result = result,
        # TODO(background-size): Without custom background, returns NULL.
        # Two consequences:
        #   1. Results parameters box: shows "Genome-wide (tool default)"
        #      instead of the actual number (provenance gap).
        #   2. Comparison bubble/heatmap: FoldEnrichment = NA for all
        #      STRING terms (FE formula needs bgSize).
        # The STRING API does not return total background size directly.
        # It may be possible to infer it indirectly from other values
        # the API returns that are computed using the background size.
        # See also: identical TODOs in PANTHER, GeneCodis, Enrichr.
        backgroundSize = private$getSimpleBackgroundSize(backgroundList)
      ))
    },

    # STRING ID conversion is handled upstream by FLAME
    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    }
  ),

  private = list(
    sendRequest = function(inputList, taxid, backgroundList) {
      url <- "https://string-db.org/api/tsv/enrichment"
      identifiers <- paste0(inputList, collapse = "%0d")

      if (is.null(backgroundList)) {
        params <- list(identifiers = identifiers, species = taxid)
      } else {
        background <- paste0(backgroundList, collapse = "%0d")
        params <- list(
          identifiers = identifiers,
          background_string_identifiers = background,
          species = taxid
        )
      }

      return(httr::POST(url, body = params))
    },

    isResponseValid = function(response) {
      if (response$status_code != 200) {
        renderWarning("Connection to STRING could not be established. Please try again later.")
        return(FALSE)
      }
      return(TRUE)
    },

    parseResponse = function(response, metric) {
      responseBody <- rawToChar(httr::content(response, "raw"))
      result <- read.delim(text = responseBody, header = TRUE)

      if (nrow(result) == 0) {
        return(NULL)
      }

      # Select p-value column based on metric
      sigColumn <- switch(metric,
        "False discovery rate" = "fdr",
        "fdr" = "fdr",
        "P-value" = "p_value",
        "p_value" = "p_value",
        "fdr"  # default
      )

      result <- result[, c(
        "category", "term", "description", sigColumn,
        "number_of_genes_in_background", "number_of_genes", "inputGenes"
      )]

      allGenes <- unique(unlist(strsplit(paste(result$inputGenes, collapse = ","), ",")))
      result$query_size <- length(allGenes)

      result <- result[, c(
        "category", "term", "description", sigColumn,
        "number_of_genes_in_background", "query_size", "number_of_genes", "inputGenes"
      )]

      colnames(result) <- ENRICHMENT_DF_COLNAMES
      result <- private$parseStringPositiveHits(result)
      result <- private$alterStringSourceKeywords(result)
      result <- private$mapStringTermIds(result)

      return(result)
    },

    # -------------------------------------------------------------------------
    # STRING-specific helper methods
    # -------------------------------------------------------------------------

    parseStringPositiveHits = function(stringResult) {
      stringResult$`Positive Hits` <- gsub(",", ",", stringResult$`Positive Hits`)
      return(stringResult)
    },

    alterStringSourceKeywords = function(stringResult) {
      # Map STRING's data source names to FLAME's standard names
      stringResult$Source <- gsub("^Process$", "GO:BP", stringResult$Source)
      stringResult$Source <- gsub("^Function$", "GO:MF", stringResult$Source)
      stringResult$Source <- gsub("^Component$", "GO:CC", stringResult$Source)
      stringResult$Source <- gsub("^KEGG$", "KEGG", stringResult$Source)
      stringResult$Source <- gsub("^RCTM$", "REAC", stringResult$Source)
      stringResult$Source <- gsub("^WikiPathways$", "WP", stringResult$Source)
      stringResult$Source <- gsub("^InterPro$", "INTERPRO", stringResult$Source)
      stringResult$Source <- gsub("^Pfam$", "PFAM", stringResult$Source)
      stringResult$Source <- gsub("^Keyword$", "UNIPROT", stringResult$Source)
      stringResult$Source <- gsub("^PMID$", "PUBMED", stringResult$Source)
      stringResult$Source <- gsub("^DISEASES$", "DO", stringResult$Source)
      stringResult$Source <- gsub("^TISSUES$", "BTO", stringResult$Source)
      stringResult$Source <- gsub("^HPO$", "HP", stringResult$Source)
      return(stringResult)
    },

    mapStringTermIds = function(stringResult) {
      # Fix Reactome Pathway Term IDs
      # STRING returns "HSA-1643685" but FLAME expects "R-HSA-1643685"
      if (any(stringResult$Source == "REAC")) {
        reac_mask <- stringResult$Source == "REAC"
        hsa_mask <- reac_mask & grepl("^HSA-", stringResult$Term_ID)
        if (any(hsa_mask)) {
          stringResult[hsa_mask, "Term_ID"] <- paste0("R-", stringResult[hsa_mask, "Term_ID"])
        }
      }
      return(stringResult)
    }
  )
)

# Register the strategy
strategyRegistry$register(ToolId$STRING, ParadigmId$ORA, STRINGORAStrategy$new())
