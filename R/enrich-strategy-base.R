# =============================================================================
# FLAME Enrichment Strategy Registry
# =============================================================================
#
# Manages enrichment tool strategies using the Strategy pattern.
# Organized by tool ID and paradigm (ORA, GSEA, etc.).
#
# Usage:
#   # Register a strategy (done in each strategy file)
#   strategyRegistry$register(ToolId$GPROFILER, ParadigmId$ORA, GProfilerORAStrategy$new())
#
#   # Get and use a strategy
#   strategy <- strategyRegistry$get(ToolId$GPROFILER, ParadigmId$ORA)
#   result <- strategy$run(inputList, organism, backgroundList, params)
#
# Dependencies:
#   - config.R (for ToolId, ParadigmId enums and helper functions)
#
# Note: The strategyRegistry is instantiated globally because:
#   1. Strategies are stateless (no session-specific data)
#   2. Registration happens once at startup
#   3. All sessions can share the same strategies
# This differs from AnalyteListRegistry/EnrichmentSessionRegistry which are per-session.
#
# =============================================================================

library(R6)

# =============================================================================
# EnrichmentStrategy - Abstract Interface for Enrichment Implementations
# =============================================================================
#
# All enrichment strategies must implement:
#   - run(): Execute the enrichment analysis
#   - convertIDs(): Convert gene IDs to the format expected by this tool
#
# Optional overrides:
#   - getValidDatasources(): Return available datasources for this tool
#   - getDefaultMetric(): Return the default significance metric

EnrichmentStrategy <- R6::R6Class("EnrichmentStrategy",
  public = list(
    toolId = NULL,
    paradigm = NULL,

    initialize = function(toolId, paradigm = ParadigmId$ORA) {
      self$toolId <- toolId
      self$paradigm <- paradigm
    },

    # === Core Methods (abstract - must override) ===

    #' Execute the enrichment analysis
    #' @param inputList Character vector of gene IDs (already converted)
    #' @param organism Organism taxid or identifier
    #' @param backgroundList Optional character vector of background genes
    #' @param params List of additional parameters (threshold, datasources, metric, etc.)
    #' @return List with 'result' (data frame) and 'backgroundSize' (integer)
    run = function(inputList, organism, backgroundList, params) {
      stop("Abstract: implement run() in subclass")
    },

    #' Convert gene IDs to the format expected by this tool
    #' @param geneList Character vector of gene IDs
    #' @param organism Organism taxid or identifier
    #' @param targetNamespace Target namespace (e.g., "ENTREZGENE_ACC", "ENSG")
    #' @return Character vector of converted IDs
    convertIDs = function(geneList, organism, targetNamespace) {
      stop("Abstract: implement convertIDs() in subclass")
    },

    # === Optional Methods (can override) ===

    #' Get valid datasources for this tool and organism
    #' @param organism Organism short_name or taxid
    #' @return Character vector of datasource names
    getValidDatasources = function(organism = NULL) {
      getDatasourcesForTool(self$toolId, organism)
    },

    #' Get the default significance metric for this tool
    #' @param hasBackground Whether a custom background is being used
    #' @return String identifier for the metric
    getDefaultMetric = function(hasBackground = FALSE) {
      getDefaultMetric(self$toolId, hasBackground)
    },

    #' Validate that the tool can run with the given parameters
    #' @param organism Organism to check
    #' @param datasources Datasources to check
    #' @return TRUE if valid, or throws an error with message
    validateParams = function(organism, datasources) {
      return(TRUE)
    }
  ),

  private = list(
    # =========================================================================
    # Protected Utility Methods (shared by multiple strategies)
    # =========================================================================

    #' Validate enrichment result data frame
    #' @param result Data frame to validate
    #' @return TRUE if valid, FALSE otherwise
    isResultValid = function(result) {
      if (is.null(result)) return(FALSE)
      if (!is.data.frame(result)) return(FALSE)
      if (nrow(result) == 0) return(FALSE)
      return(TRUE)
    },

    #' Get background size from user-provided reference list
    #' @param userReference Character vector of background genes
    #' @return Integer length or NULL
    getSimpleBackgroundSize = function(userReference) {
      if (is.null(userReference)) NULL else length(userReference)
    },

    #' Map KEGG IDs to standard format (map + numeric ID)
    #' @param df Data frame with Source and Term_ID columns
    #' @return Modified data frame
    mapKEGGIds = function(df) {
      if (length(df$Source[which(df$Source == "KEGG")]) > 0) {
        df[df$Source == "KEGG", ]$Term_ID <-
          paste0("map", gsub("[^0-9.-]", "", df[df$Source == "KEGG", ]$Term_ID))
      }
      return(df)
    },

    #' Unlist datasource codes to their display names
    #' @param sources Vector of source codes from API
    #' @param codes Named vector mapping codes to display names
    #' @return Vector of display names
    unlistDatasourceCodes = function(sources, codes) {
      return(
        unlist(lapply(sources, function(sourceName) {
          names(codes[codes == sourceName])
        }))
      )
    }
  )
)


# =============================================================================
# StrategyRegistry - Registry for Enrichment Strategies
# =============================================================================
#
# Manages registration and lookup of enrichment strategies.
# Organized by tool ID and paradigm.
#
# Structure: strategies[[toolId]][[paradigm]] = EnrichmentStrategy instance
#
# Note: Uses environments internally for reference semantics, avoiding
# R's copy-on-modify behavior with lists.

StrategyRegistry <- R6::R6Class("StrategyRegistry",
  private = list(
    # Structure: strategies[[toolId]][[paradigm]]
    # Using environment for reference semantics
    strategies = NULL
  ),

  public = list(
    initialize = function() {
      private$strategies <- new.env(parent = emptyenv())
    },

    #' Register a strategy for a given tool and paradigm
    #' @param toolId Tool ID from ToolId enum
    #' @param paradigm Paradigm ID from ParadigmId enum
    #' @param strategy EnrichmentStrategy instance
    #' @return Self (for chaining)
    register = function(toolId, paradigm, strategy) {
      if (is.null(private$strategies[[toolId]])) {
        private$strategies[[toolId]] <- new.env(parent = emptyenv())
      }
      private$strategies[[toolId]][[paradigm]] <- strategy
      invisible(self)
    },

    #' Get a registered strategy
    #' @param toolId Tool ID from ToolId enum
    #' @param paradigm Paradigm ID from ParadigmId enum (default: ORA)
    #' @return EnrichmentStrategy instance
    #' @throws Error if strategy not found
    get = function(toolId, paradigm = ParadigmId$ORA) {
      toolEnv <- private$strategies[[toolId]]
      if (is.null(toolEnv)) {
        stop(paste("No strategies registered for tool:", toolId))
      }

      strategy <- toolEnv[[paradigm]]
      if (is.null(strategy)) {
        stop(paste("No", paradigm, "strategy registered for", toolId))
      }

      return(strategy)
    },

    #' Check if a strategy is registered
    #' @param toolId Tool ID
    #' @param paradigm Paradigm ID
    #' @return TRUE if registered, FALSE otherwise
    hasStrategy = function(toolId, paradigm = ParadigmId$ORA) {
      toolEnv <- private$strategies[[toolId]]
      if (is.null(toolEnv)) return(FALSE)
      return(!is.null(toolEnv[[paradigm]]))
    },

    #' List all registered paradigms for a tool
    #' @param toolId Tool ID
    #' @return Character vector of paradigm IDs
    listParadigms = function(toolId) {
      toolEnv <- private$strategies[[toolId]]
      if (is.null(toolEnv)) return(character(0))
      return(names(as.list(toolEnv)))
    },

    #' List all registered tools
    #' @return Character vector of tool IDs
    listTools = function() {
      return(names(as.list(private$strategies)))
    },

    #' Get count of registered strategies
    #' @return Integer count
    getStrategyCount = function() {
      total <- 0
      for (toolId in names(as.list(private$strategies))) {
        toolEnv <- private$strategies[[toolId]]
        total <- total + length(as.list(toolEnv))
      }
      return(total)
    },

    #' Debug helper: print registry contents
    print = function() {
      cat("StrategyRegistry contents:\n")
      for (toolId in names(as.list(private$strategies))) {
        cat(paste0("  ", toolId, ":\n"))
        toolEnv <- private$strategies[[toolId]]
        for (paradigm in names(as.list(toolEnv))) {
          cat(paste0("    - ", paradigm, "\n"))
        }
      }
      invisible(self)
    }
  )
)


# =============================================================================
# Global Instance
# =============================================================================

strategyRegistry <- StrategyRegistry$new()
