# =============================================================================
# WebGestalt ORA Strategy
# =============================================================================
#
# Implements Over-Representation Analysis using the WebGestalt API.
#
# Dependencies:
#   - config.R (for ToolId, ParadigmId, ORGANISMS, TOOLS, ENRICHMENT_DF_COLNAMES)
#   - enrich-strategy-base.R (for EnrichmentStrategy, strategyRegistry)
#   - WebGestaltR package
#
# =============================================================================

WebGestaltORAStrategy <- R6::R6Class("WebGestaltORAStrategy",

  inherit = EnrichmentStrategy,

  public = list(
    initialize = function() {
      super$initialize(ToolId$WEBGESTALT, ParadigmId$ORA)
    },

    run = function(inputList, organism, backgroundList, params) {
      # Get datasource codes from tool config
      toolConfig <- TOOLS[[self$toolId]]
      datasourceCodes <- toolConfig$datasourceCodes
      datasources <- as.character(datasourceCodes[params$datasources])
      datasources <- datasources[!is.na(datasources)]

      if (length(datasources) == 0) {
        return(NULL)
      }

      # Get organism short name (WebGestalt uses different name for dog)
      organismName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name
      if (organismName == "clfamiliaris") {
        organismName <- "cfamiliaris"
      }

      # Determine namespace
      namespace <- if (!is.null(params$namespace) && params$namespace != "USERINPUT") {
        "entrezgene"
      } else {
        "genesymbol"
      }

      # Determine significance method
      metric <- params$metric
      if (metric == "top") {
        sigMethod <- "top"
        fdrMethod <- "BH"
      } else {
        sigMethod <- "fdr"
        fdrMethod <- metric
      }

      # Determine background
      if (is.null(backgroundList)) {
        referenceSet <- "genome"
        referenceGene <- NULL
        referenceGeneType <- NULL
      } else {
        referenceSet <- NULL
        referenceGene <- backgroundList
        referenceGeneType <- namespace
      }

      # Call WebGestalt API
      result <- suppressWarnings(WebGestaltR::WebGestaltR(
        organism = organismName,
        enrichDatabase = datasources,
        interestGene = inputList,
        interestGeneType = namespace,
        referenceGene = referenceGene,
        referenceGeneType = referenceGeneType,
        referenceSet = referenceSet,
        sigMethod = sigMethod,
        fdrMethod = fdrMethod,
        fdrThr = as.numeric(params$threshold),
        topThr = 100,
        isOutput = FALSE,
        hostName = "https://www.webgestalt.org/"
      ))

      # Calculate background size
      backgroundSize <- if (is.null(backgroundList)) {
        private$getWebgestaltBackgroundSize(organism = organismName)
      } else {
        length(backgroundList)
      }

      if (!private$isResultValid(result)) {
        return(NULL)
      }

      # Parse result
      result <- private$parseResult(result, length(inputList), params$datasources)

      # Filter by datasources
      if (!is.null(params$datasources) && length(params$datasources) > 0) {
        result <- result[result$Source %in% params$datasources, ]
      }

      if (nrow(result) == 0) {
        return(NULL)
      }

      # Return structured result
      return(list(
        result = result,
        backgroundSize = backgroundSize
      ))
    },

    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    }
  ),

  private = list(
    parseResult = function(result, numInputs, selectedDatasources) {
      toolConfig <- TOOLS[[self$toolId]]
      datasourceCodes <- toolConfig$datasourceCodes

      if (is.null(result$database)) {
        result$database <- as.character(datasourceCodes[selectedDatasources])
      }
      result$database <- private$unlistDatasourceCodes(result$database, datasourceCodes)

      if (is.null(result$userId)) {
        result$userId <- result$overlapId
      }
      result$userId <- gsub(";", ",", result$userId)
      result$querySize <- numInputs

      # Preserve original Term_ID before creating linked version
      termIdNoLinks <- result$geneSet

      # Create linked Term_IDs if links are available
      if (!is.null(result$link)) {
        # The link comes from the WebGestalt service: accept only http(s) and
        # encode it; escape the visible text.
        safeLinks <- ifelse(grepl("^https?://", result$link, ignore.case = TRUE),
                            flameUrlPart(result$link), "")
        linkedTermId <- ifelse(
          nzchar(safeLinks),
          paste0("<a href='", safeLinks, "' target='_blank' rel='noopener noreferrer'>",
                 htmltools::htmlEscape(result$geneSet), "</a>"),
          htmltools::htmlEscape(result$geneSet)
        )
        # Handle DISGENET special case
        if ("DISGENET" %in% result$database) {
          disgenetMask <- result$database == "DISGENET"
          linkedTermId[disgenetMask] <- paste0(
            "<a href='https://www.disgenet.org/search/0/",
            flameUrlPart(result$geneSet[disgenetMask]), "/' target='_blank' rel='noopener noreferrer'>",
            htmltools::htmlEscape(result$geneSet[disgenetMask]), "</a>"
          )
        }
        result$geneSet <- linkedTermId
      }

      result <- result[, c(
        "database", "geneSet", "description", "pValue",
        "size", "querySize", "overlap", "userId"
      )]
      colnames(result) <- ENRICHMENT_DF_COLNAMES
      result <- private$mapKEGGIds(result)

      # Add Term_ID_noLinks column (formatResultTable checks for this)
      result$Term_ID_noLinks <- termIdNoLinks

      return(result)
    },

    # -------------------------------------------------------------------------
    # WebGestalt-specific helper methods
    # -------------------------------------------------------------------------

    getWebgestaltBackgroundSize = function(organism = "hsapiens", referenceSet = "genome_protein-coding") {
      url <- sprintf("https://www.webgestalt.org/api/reference?organism=%s&referenceSet=%s", organism, referenceSet)
      x <- read.csv(url(url), header = FALSE)
      return(length(x$V1))
    }
  )
)

# Register the strategy
strategyRegistry$register(ToolId$WEBGESTALT, ParadigmId$ORA, WebGestaltORAStrategy$new())
