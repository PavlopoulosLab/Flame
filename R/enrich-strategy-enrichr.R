# =============================================================================
# Enrichr ORA Strategy
# =============================================================================
#
# Implements Over-Representation Analysis using the Enrichr API.
#
# Dependencies:
#   - config.R (for ToolId, ParadigmId, ORGANISMS, TOOLS, ENRICHMENT_DF_COLNAMES)
#   - enrich-strategy-base.R (for EnrichmentStrategy, strategyRegistry)
#   - enrichR package
#   - tidyr package
#
# =============================================================================

EnrichRORAStrategy <- R6::R6Class("EnrichRORAStrategy",

  inherit = EnrichmentStrategy,

  public = list(
    initialize = function() {
      super$initialize(ToolId$ENRICHR, ParadigmId$ORA)
    },

    run = function(inputList, organism, backgroundList, params) {
      phase <- "init"
      tryCatch({
        phase <- "init-options"
        private$ensureEnrichrOptions()

        # Normalize API inputs to strict character vectors to avoid enrichR type errors.
        phase <- "normalize-inputs"
        inputList <- as.character(unlist(inputList, use.names = FALSE))
        inputList <- trimws(inputList)
        inputList <- unique(inputList[!is.na(inputList) & nzchar(inputList)])
        if (length(inputList) == 0) {
          warning("No valid input IDs after normalization for enrichR")
          return(NULL)
        }

        # Get organism short name
        phase <- "resolve-organism"
        shortCandidates <- as.character(ORGANISMS[ORGANISMS$taxid == organism, ]$short_name)
        shortCandidates <- shortCandidates[!is.na(shortCandidates) & nzchar(shortCandidates)]
        shortName <- if (length(shortCandidates) > 0) shortCandidates[[1]] else "hsapiens"

        # Set up enrichR site based on organism
        phase <- "set-site"
        site <- private$getEnrichrSite(shortName)
        enrichR::setEnrichrSite(site)

        # Get database codes for selected datasources
        phase <- "resolve-databases"
        databases <- private$getDatabaseCodes(shortName, params$datasources)
        databases <- as.character(unlist(databases, use.names = FALSE))
        databases <- unique(databases[!is.na(databases) & nzchar(databases)])

        if (length(databases) == 0) {
          warning("No valid enrichR databases selected")
          return(NULL)
        }

        # Call enrichR API
        phase <- "api-call"
        result <- enrichR::enrichr(inputList, databases)

        # Combine results from all databases
        phase <- "bind-results"
        enrichrResult <- do.call(rbind, result)

        # Filter by significance threshold
        phase <- "threshold-filter"
        threshold <- as.numeric(params$threshold)
        enrichrResult <- enrichrResult[enrichrResult$Adjusted.P.value <= threshold, ]

        # Calculate background size
        # TODO(background-size): getEnrichrBackgroundSize() is broken:
        #   1. Only works for site=="Enrichr" (human). Returns NULL
        #      for FlyEnrichr, WormEnrichr, YeastEnrichr, etc.
        #   2. Returns max(numTerms) from listEnrichrDbs() — that's
        #      the number of terms/categories, NOT genes. Wrong metric.
        # Two consequences:
        #   1. Results parameters box: shows wrong number (human) or
        #      "Genome-wide (tool default)" (provenance gap).
        #   2. Comparison bubble/heatmap: FoldEnrichment is wrong
        #      (human) or NA (non-human), FE formula needs bgSize.
        # The Enrichr API does not expose total background gene count.
        # It may be possible to infer it indirectly from other values
        # the API returns that are computed using the background size.
        # See also: identical TODOs in STRING, PANTHER, GeneCodis.
        phase <- "background-size"
        backgroundSize <- private$getEnrichrBackgroundSize(site, databases)

        # Check if we have valid results
        phase <- "validate"
        if (!private$isResultValid(enrichrResult)) {
          return(NULL)
        }

        # Parse results into standard format
        phase <- "parse"
        enrichrResult <- private$parseResult(enrichrResult, length(inputList), shortName)

        # Filter by datasources
        phase <- "datasource-filter"
        if (!is.null(params$datasources) && length(params$datasources) > 0) {
          enrichrResult <- enrichrResult[enrichrResult$Source %in% params$datasources, ]
        }

        if (nrow(enrichrResult) == 0) {
          return(NULL)
        }

        # Return structured result
        return(list(
          result = enrichrResult,
          backgroundSize = backgroundSize
        ))
      }, error = function(e) {
        warning(
          paste0(
            "enrichR strategy error at phase '", phase, "': ",
            conditionMessage(e),
            " | organism=", as.character(organism),
            " | datasources=", paste(as.character(params$datasources %||% character(0)), collapse = ",")
          )
        )
        return(NULL)
      })
    },

    # enrichR uses gene symbols directly - no conversion needed
    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    }
  ),

  private = list(
    # -------------------------------------------------------------------------
    # Term parsing patterns (configuration)
    # -------------------------------------------------------------------------
    termPatterns = list(
      GO = list(dbPattern = "^GO:", separator = " \\(GO:", idPrefix = "GO:", trimSuffix = TRUE),
      KEGG_HUMAN = list(dbPattern = "^KEGG$", separator = " Homo sapiens ", idPrefix = "", trimSuffix = FALSE),
      KEGG_YEAST = list(dbPattern = "^KEGG$", parseMode = "fixed", termEnd = -8, idStart = -7, idEnd = 0),
      REAC = list(dbPattern = "^REAC$", separator = " R-", idPrefix = "R-", trimSuffix = FALSE),
      WP = list(dbPattern = "^WP$", separator = "WP", idPrefix = "WP", trimSuffix = FALSE),
      PANTHER = list(dbPattern = "^PANTHER Pathways$", separator = " Homo sapiens ", idPrefix = "", trimSuffix = FALSE),
      DO = list(dbPattern = "^DO$", separator = "\\(DOID:", idPrefix = "DOID:", trimSuffix = TRUE),
      WBP = list(dbPattern = "^WBP$", separator = "_WBPhenotype:", idPrefix = "WBPhenotype:", trimSuffix = FALSE),
      WBBT = list(dbPattern = "^WBBT$", separator = "\\(WBbt:", idPrefix = "WBbt:", trimSuffix = TRUE),
      ORPHA = list(dbPattern = "^ORPHA$", separator = " ORPHA:", idPrefix = "ORPHA:", trimSuffix = FALSE),
      MGI_MOUSE = list(dbPattern = "^MGI$", separator = " \\(MP:", idPrefix = "MP:", trimSuffix = TRUE),
      MGI_OX = list(dbPattern = "^MGI$", parseMode = "fixed", termStart = 12, idStart = 0, idEnd = 10),
      HP = list(dbPattern = "^HP$", separator = " \\(HP:", idPrefix = "HP:", trimSuffix = TRUE)
    ),

    # -------------------------------------------------------------------------
    # Core private methods
    # -------------------------------------------------------------------------

    getEnrichrSite = function(shortName) {
      toolConfig <- TOOLS[[self$toolId]]
      key <- if (is.null(shortName) || length(shortName) == 0) {
        "hsapiens"
      } else {
        as.character(shortName[[1]])
      }
      if (is.na(key) || !nzchar(key)) key <- "hsapiens"
      site <- toolConfig$organismSites[[key]]
      if (is.null(site)) site <- "Enrichr"
      site <- as.character(site[[1]])
      if (is.na(site) || !nzchar(site)) site <- "Enrichr"
      return(site)
    },

    ensureEnrichrOptions = function() {
      defaultBase <- "https://maayanlab.cloud/"
      defaultSites <- c("Enrichr", "FlyEnrichr", "WormEnrichr", "YeastEnrichr", "FishEnrichr", "OxEnrichr")

      sitesBase <- getOption("enrichR.sites.base.address")
      if (!is.character(sitesBase) || length(sitesBase) == 0 || is.na(sitesBase[1]) || !nzchar(sitesBase[1])) {
        options(enrichR.sites.base.address = defaultBase)
      } else {
        options(enrichR.sites.base.address = as.character(sitesBase[1]))
      }

      sites <- getOption("enrichR.sites")
      if (!is.character(sites) || length(sites) == 0) {
        options(enrichR.sites = defaultSites)
      } else {
        sites <- as.character(sites)
        sites <- sites[!is.na(sites) & nzchar(sites)]
        options(enrichR.sites = if (length(sites) > 0) unique(sites) else defaultSites)
      }

      quiet <- getOption("enrichR.quiet")
      if (!is.logical(quiet) || length(quiet) == 0 || is.na(quiet[1])) {
        options(enrichR.quiet = FALSE)
      } else {
        options(enrichR.quiet = isTRUE(quiet[1]))
      }

      live <- getOption("enrichR.live")
      if (!is.logical(live) || length(live) == 0 || is.na(live[1])) {
        options(enrichR.live = FALSE)
      } else {
        options(enrichR.live = isTRUE(live[1]))
      }
    },

    getDatabaseCodes = function(shortName, selectedDatasources) {
      toolConfig <- TOOLS[[self$toolId]]
      orgKey <- if (is.null(shortName) || length(shortName) == 0) {
        "hsapiens"
      } else {
        as.character(shortName[[1]])
      }
      if (is.na(orgKey) || !nzchar(orgKey)) orgKey <- "hsapiens"

      datasourceCodes <- toolConfig$organismDatasourceCodes[[orgKey]]
      if (is.null(datasourceCodes)) {
        datasourceCodes <- toolConfig$organismDatasourceCodes[["hsapiens"]]
      }
      if (is.null(datasourceCodes) || length(datasourceCodes) == 0) {
        return(character(0))
      }

      selected <- as.character(unlist(selectedDatasources, use.names = FALSE))
      selected <- selected[!is.na(selected) & nzchar(selected)]
      if (length(selected) == 0) {
        return(character(0))
      }

      selected <- intersect(selected, names(datasourceCodes))
      if (length(selected) == 0) {
        return(character(0))
      }

      databases <- as.character(unlist(datasourceCodes[selected], use.names = FALSE))
      databases <- unique(databases[!is.na(databases) & nzchar(databases)])
      return(databases)
    },

    parseResult = function(enrichrResult, numInputs, shortName) {
      # Add database column from rownames
      enrichrResult$database <- sapply(strsplit(rownames(enrichrResult), "\\."), "[[", 1)
      rownames(enrichrResult) <- NULL

      # Convert database codes back to display names
      toolConfig <- TOOLS[[self$toolId]]
      datasourceCodes <- toolConfig$organismDatasourceCodes[[shortName]]
      if (is.null(datasourceCodes)) {
        datasourceCodes <- toolConfig$organismDatasourceCodes[["hsapiens"]]
      }
      enrichrResult$database <- private$unlistDatasourceCodes(enrichrResult$database, datasourceCodes)

      enrichrResult$querySize <- numInputs
      enrichrResult$Genes <- gsub(";", ",", enrichrResult$Genes)

      # Split overlap column
      enrichrResult <- tidyr::separate(
        enrichrResult,
        Overlap,
        c("overlap", "size"),
        sep = "\\/"
      )
      enrichrResult$overlap <- as.numeric(enrichrResult$overlap)
      enrichrResult$size <- as.numeric(enrichrResult$size)

      # Parse term IDs
      result <- private$splitEnrichrTermIds(enrichrResult, shortName)
      enrichrResult$Term <- result$terms
      enrichrResult$TermId <- result$ids

      # Select and rename columns to standard format
      enrichrResult <- enrichrResult[, c(
        "database", "TermId", "Term", "Adjusted.P.value",
        "size", "querySize", "overlap", "Genes"
      )]
      colnames(enrichrResult) <- ENRICHMENT_DF_COLNAMES

      # Map KEGG IDs
      enrichrResult <- private$mapKEGGIds(enrichrResult)

      return(enrichrResult)
    },

    # -------------------------------------------------------------------------
    # enrichR-specific helper methods
    # -------------------------------------------------------------------------

    getEnrichrBackgroundSize = function(site, selected_dbs) {
      if (!identical(site, "Enrichr")) {
        return(NULL)
      }

      dbs_all <- tryCatch(
        enrichR::listEnrichrDbs(),
        error = function(e) {
          warning(
            paste0(
              "Unable to fetch EnrichR database statistics: ",
              conditionMessage(e)
            )
          )
          NULL
        }
      )

      if (is.null(dbs_all) || !is.data.frame(dbs_all) || nrow(dbs_all) == 0) {
        return(NULL)
      }
      if (!("libraryName" %in% names(dbs_all)) || !("numTerms" %in% names(dbs_all))) {
        return(NULL)
      }

      selected <- as.character(unlist(selected_dbs, use.names = FALSE))
      selected <- selected[!is.na(selected) & nzchar(selected)]
      if (length(selected) == 0) {
        return(NULL)
      }

      matched <- dbs_all$libraryName %in% selected
      if (!any(matched)) {
        return(NULL)
      }

      termSizes <- suppressWarnings(as.numeric(dbs_all$numTerms[matched]))
      termSizes <- termSizes[is.finite(termSizes)]
      if (length(termSizes) == 0) {
        return(NULL)
      }

      max(termSizes)
    },

    parseEnrichrTermsForDb = function(enrichrResult, config) {
      if (is.null(config$dbPattern) ||
          length(config$dbPattern) == 0 ||
          !is.character(config$dbPattern) ||
          is.na(config$dbPattern[1]) ||
          !nzchar(config$dbPattern[1])) {
        return(list(terms = c(), ids = c()))
      }

      patternHits <- tryCatch(
        grep(config$dbPattern, enrichrResult$database),
        error = function(e) {
          warning(
            paste0(
              "enrichR term parse pattern failure: pattern=",
              as.character(config$dbPattern[1]),
              " | message=", conditionMessage(e)
            )
          )
          integer(0)
        }
      )

      items <- enrichrResult[patternHits, ]$Term
      if (identical(items, character(0))) {
        return(list(terms = c(), ids = c()))
      }

      if (!is.null(config$parseMode) && config$parseMode == "fixed") {
        if (!is.null(config$termEnd)) {
          terms <- trimws(substr(items, 1, nchar(items) + config$termEnd))
        } else {
          terms <- substr(items, config$termStart, nchar(items))
        }
        ids <- substr(items, nchar(items) + config$idStart + 1, nchar(items) + config$idEnd)
      } else {
        splitList <- strsplit(items, config$separator)
        terms <- sapply(splitList, "[[", 1)
        ids <- sapply(splitList, function(x) if (length(x) > 1) x[[2]] else "")

        if (!is.null(config$trimSuffix) && config$trimSuffix) {
          ids <- substring(ids, 1, nchar(ids) - 1)
        }
        ids <- paste0(config$idPrefix, ids)
      }

      return(list(terms = terms, ids = ids))
    },

    splitEnrichrTermIds = function(enrichrResult, organismShortName) {
      if (is.null(organismShortName)) {
        warning("splitEnrichrTermIds: organismShortName is required")
        return(enrichrResult)
      }

      terms <- c()
      ids <- c()

      patternsToUse <- c("GO", "REAC", "WP", "DO", "WBP", "WBBT", "ORPHA", "HP")

      if (organismShortName == "hsapiens") {
        patternsToUse <- c(patternsToUse, "KEGG_HUMAN", "PANTHER")
      } else {
        patternsToUse <- c(patternsToUse, "KEGG_YEAST")
      }

      if (organismShortName == "mmusculus") {
        patternsToUse <- c(patternsToUse, "MGI_MOUSE")
      } else {
        patternsToUse <- c(patternsToUse, "MGI_OX")
      }

      for (patternKey in patternsToUse) {
        config <- private$termPatterns[[patternKey]]
        if (!is.null(config)) {
          result <- private$parseEnrichrTermsForDb(enrichrResult, config)
          terms <- c(terms, result$terms)
          ids <- c(ids, result$ids)
        }
      }

      return(list(terms = terms, ids = ids))
    }
  )
)

# Register the strategy
strategyRegistry$register(ToolId$ENRICHR, ParadigmId$ORA, EnrichRORAStrategy$new())
