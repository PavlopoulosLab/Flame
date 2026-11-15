# =============================================================================
# g:Profiler ORA Strategy
# =============================================================================
#
# Implements Over-Representation Analysis using the g:Profiler API (gprofiler2).
#
# Dependencies:
#   - config.R (for ToolId, ParadigmId, ORGANISMS, ENRICHMENT_DF_COLNAMES)
#   - enrich-strategy-base.R (for EnrichmentStrategy, strategyRegistry)
#   - gprofiler2 package
#
# =============================================================================

GProfilerORAStrategy <- R6::R6Class("GProfilerORAStrategy",

  inherit = EnrichmentStrategy,

  public = list(
    initialize = function() {
      super$initialize(ToolId$GPROFILER, ParadigmId$ORA)
    },

    run = function(inputList, organism, backgroundList, params) {
      # Filter datasources to only those supported by gProfiler
      toolDatasources <- getDatasourcesForTool(self$toolId)
      sources <- toolDatasources[toolDatasources %in% params$datasources]

      if (identical(sources, character(0))) {
        return(NULL)
      }

      # Determine background mode
      if (is.null(backgroundList)) {
        domain_scope <- "annotated"
        custom_bg <- NULL
        significant <- TRUE
      } else {
        domain_scope <- "custom"
        custom_bg <- backgroundList
        significant <- FALSE
      }

      # Get organism short name
      organismName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name

      # Call gProfiler API
      result <- gprofiler2::gost(
        query = inputList,
        organism = organismName,
        significant = significant,
        evcodes = TRUE,
        user_threshold = as.numeric(params$threshold),
        correction_method = params$metric,
        sources = sources,
        domain_scope = domain_scope,
        custom_bg = custom_bg
      )

      # Check if valid
      if (is.null(result) || length(result) == 0) {
        return(NULL)
      }

      # Calculate background size
      backgroundSize <- if (is.null(backgroundList)) {
        private$getBackgroundSize(result)
      } else {
        length(backgroundList)
      }

      # Return structured result
      return(list(
        result = private$parseResult(result),
        backgroundSize = backgroundSize
      ))
    },

    # gProfiler accepts most ID formats directly
    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    }
  ),

  private = list(
    parseResult = function(result) {
      parsed <- result$result[, c(
        "source", "term_id", "term_name", "p_value", "term_size",
        "query_size", "intersection_size", "intersection"
      )]
      colnames(parsed) <- ENRICHMENT_DF_COLNAMES
      parsed <- private$mapGProfilerIds(parsed)
      return(parsed)
    },

    getBackgroundSize = function(result) {
      if (is.null(result) || length(result) == 0) {
        return(NULL)
      }
      metadata <- result$meta$result_metadata
      bsizes <- lapply(names(metadata), function(i) {
        return(metadata[[i]]$domain_size)
      })
      return(max(unlist(bsizes)))
    },

    # -------------------------------------------------------------------------
    # gProfiler-specific ID mapping helpers
    # -------------------------------------------------------------------------

    mapGProfilerIds = function(df) {
      df <- private$mapKEGGIds(df)
      df <- private$mapREACIds(df)
      df <- private$mapWPIds(df)
      return(df)
    },

    mapREACIds = function(df) {
      if (length(df$Source[which(df$Source == "REAC")]) > 0) {
        df[df$Source == "REAC", ]$Term_ID <-
          gsub("REAC:", "", df[df$Source == "REAC", ]$Term_ID)
      }
      return(df)
    },

    mapWPIds = function(df) {
      if (length(df$Source[which(df$Source == "WP")]) > 0) {
        df[df$Source == "WP", ]$Term_ID <-
          gsub("WP:", "", df[df$Source == "WP", ]$Term_ID)
      }
      return(df)
    }
  )
)

# Register the strategy
strategyRegistry$register(ToolId$GPROFILER, ParadigmId$ORA, GProfilerORAStrategy$new())
