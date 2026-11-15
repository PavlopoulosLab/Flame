# =============================================================================
# FLAME ORA Enrichment Session
# =============================================================================
#
# Concrete implementation of EnrichmentSession for Over-Representation Analysis.
# Each instance represents one ORA enrichment run and owns ALL its data.
#
# Lifecycle:
# 1. Created by EnrichmentController on submit
# 2. execute() runs full enrichment flow:
#    - Gene conversion
#    - Strategy execution via strategyRegistry
#    - Result transformation
#    - DB link attachment
# 3. EnrichmentController$insertTab() adds tab to results panel
# 4. server() sets up observers for plots/tables
# 5. cleanup() destroys observers when tab closes
#
# Data Ownership (Option B - no globals):
# - Results stored in private$.results (NOT enrichmentResults global)
# - Background size in private$.backgroundSize
# - Arena edgelists in private$.arenaEdgelists
#
# Dependencies:
# - enrich-session-base.R (for EnrichmentSession)
# - enrich-strategy-base.R (for strategyRegistry, ParadigmId)
# - func-tabGeneration.R (for UI generation, temporarily)
#
# =============================================================================

#' ORA Enrichment Session Class
#'
#' Handles Over-Representation Analysis (ORA) enrichment runs.
#' Owns all its data - results are stored in session, NOT globals.
#'
#' @section Results Storage:
#' Results are stored in private$.results (NOT globals).
#' Access via getResults(), hasResults().
#'
ORAEnrichmentSession <- R6::R6Class(
 "ORAEnrichmentSession",
  inherit = EnrichmentSession,

  public = list(
    #' @field background Optional background gene list (AnalyteList)
    background = NULL,

    #' Initialize an ORA Enrichment Session
    #' @param id Character. Unique session ID.
    #' @param runId Character. Run ID for tab panel.
    #' @param uniqueId Integer. Unique counter for Shiny IDs.
    #' @param displayNumber Integer. Display number for tab title.
    #' @param toolName Character. Tool name.
    #' @param organism Integer. Organism taxid.
    #' @param input AnalyteList. Input gene list.
    #' @param background AnalyteList. Optional background gene list.
    #' @param parameters List. Enrichment parameters (datasources, threshold, etc.).
    initialize = function(id, runId, uniqueId, displayNumber, toolName, organism,
                          input, paradigm = NULL, background = NULL, parameters = list()) {
      super$initialize(
        id = id,
        runId = runId,
        uniqueId = uniqueId,
        displayNumber = displayNumber,
        toolName = toolName,
        organism = organism,
        input = input,
        paradigm = paradigm,
        parameters = parameters
      )
      self$background <- background
      private$.arenaEdgelists <- list()
    },

    #' Execute the ORA enrichment
    #'
    #' Full enrichment flow:
    #' 1. Convert gene IDs to tool-specific format
    #' 2. Call strategyRegistry strategy
    #' 3. Transform results (add columns, format)
    #' 4. Sort by -log10Pvalue
    #' 5. Attach database links
    #' 6. Store everything in private fields
    #'
    #' @return Invisible self for chaining
    execute = function() {
      # Get parameters
      params <- private$.parameters
      namespace <- params$namespace

      # Step 1: Convert gene IDs to tool-specific format
      geneIds <- private$.input$getIds()
      flame_progress(0.12, sprintf("Mapping %d identifiers for %s", length(geneIds), self$toolName))
      inputConversionTable <- private$convertGeneIds(geneIds, namespace)

      if (is.null(inputConversionTable) || nrow(inputConversionTable) == 0) {
        warning(sprintf("Gene conversion failed for %s", self$toolName))
        return(invisible(self))
      }

      # Store conversion table and namespace
      private$.conversionTable <- inputConversionTable
      resolvedNamespace <- attr(inputConversionTable, "namespace")
      private$.parameters$namespace <- if (!is.null(resolvedNamespace)) resolvedNamespace else namespace

      # Convert background if provided
      backgroundConversionTable <- NULL
      convertedBackgroundIds <- NULL
      if (!is.null(self$background)) {
        backgroundIds <- self$background$getIds()
        backgroundConversionTable <- private$convertGeneIds(backgroundIds, namespace)
        if (!is.null(backgroundConversionTable)) {
          convertedBackgroundIds <- backgroundConversionTable$target
          private$.backgroundConversionTable <- backgroundConversionTable
        }
      }

      # Step 2: Call strategyRegistry strategy
      convertedInputIds <- inputConversionTable$target

      # Check if we have a strategy
      if (!strategyRegistry$hasStrategy(self$toolName, ParadigmId$ORA)) {
        warning(sprintf("No strategy registered for %s", self$toolName))
        return(invisible(self))
      }

      flame_progress(0.35, sprintf("Querying %s with %d mapped genes", self$toolName, length(convertedInputIds)))
      strategyResult <- tryCatch({
        strategy <- strategyRegistry$get(self$toolName, ParadigmId$ORA)
        strategy$run(
          convertedInputIds,
          self$organism,
          convertedBackgroundIds,
          params
        )
      }, error = function(e) {
        warning(sprintf("Enrichment failed for %s: %s", self$toolName, e$message))
        NULL
      })

      if (is.null(strategyResult) || is.null(strategyResult$result) ||
          nrow(strategyResult$result) == 0) {
        return(invisible(self))
      }

      private$.backgroundSize <- strategyResult$backgroundSize

      # Step 3: Transform results
      results <- private$transformResultTable(strategyResult$result)

      # Step 4: Sort by -log10Pvalue descending
      results <- results[order(-results$`-log10Pvalue`), ]

      # Store Term_ID_noLinks before link attachment
      if (is.null(results$Term_ID_noLinks)) {
        results$Term_ID_noLinks <- results$Term_ID
      }

      # Step 5: Attach database links
      results <- private$attachDBLinks(results)

      # Step 6: Store final results
      private$.results <- results

      invisible(self)
    },

    #' Rollback converted names to original input symbols
    #'
    #' Converts Positive Hits from tool-specific IDs back to original gene symbols.
    #' Called when user selects "Original input names" option.
    #'
    #' @return Character vector of IDs to use for no-hit calculation
    rollbackNames = function() {
      if (!self$hasResults()) return(character(0))

      conversionTable <- private$.conversionTable
      if (is.null(conversionTable)) return(character(0))

      results <- private$.results

      # Check if all Positive Hits are empty (safety check)
      allEmpty <- all(results$`Positive Hits` == "" | is.na(results$`Positive Hits`))
      if (allEmpty) {
        return(conversionTable$input)
      }

      # Rollback the names
      results <- tidyr::separate_rows(results, `Positive Hits`, sep = ",\\s*")
      results <- merge(results, conversionTable,
                       by.x = "Positive Hits", by.y = "target")
      results <- results[, !(names(results) %in% c("Positive Hits", "name"))]
      colnames(results)[match("input", colnames(results))] <- "Positive Hits"
      results <- results %>%
        dplyr::group_by(Term_ID) %>%
        dplyr::mutate(`Positive Hits` = paste(`Positive Hits`, collapse = ","))

      # Reorder columns to match expected structure (required for ALL tools)
      # After merge, column order changes - Positive Hits ends up at the end
      # Must restore to: Source, Term_ID, Function, P-value, -log10Pvalue,
      #                  Term Size, Query size, Intersection Size, Gene Ratio,
      #                  Term Coverage %, Fold Enrichment,
      #                  Positive Hits, Term_ID_noLinks
      expectedCols <- c("Source", "Term_ID", "Function", "P-value",
                        "-log10Pvalue", "Term Size", "Query size",
                        "Intersection Size", "Gene Ratio", "Term Coverage %",
                        "Fold Enrichment",
                        "Positive Hits", "Term_ID_noLinks")
      # Only include columns that exist
      existingCols <- expectedCols[expectedCols %in% colnames(results)]
      results <- results[, existingCols]
      results <- as.data.frame(dplyr::distinct(results))

      # Update stored results
      private$.results <- results

      # Return original input IDs for no-hit calculation
      return(conversionTable$input)
    },

    #' Get IDs for no-hit calculation without rollback
    #' @return Character vector of converted IDs
    getConvertedIds = function() {
      conversionTable <- private$.conversionTable
      if (is.null(conversionTable)) return(character(0))
      return(conversionTable$target)
    },

    #' Check if this session has a background list
    #' @return Logical
    hasBackground = function() {
      !is.null(self$background)
    },

    #' Get mapped input analytes with no significant term hits
    #'
    #' Returns mapped input analytes that do not appear in any
    #' significant term through direct or converted ID matching.
    #'
    #' @return Character vector of no-hit mapped input analytes
    getNoHitAnalytes = function() {
      normalizeValues <- function(values) {
        values <- trimws(as.character(values %||% character(0)))
        values <- values[nzchar(values) & !is.na(values)]
        unique(values)
      }

      inputObj <- private$.input
      inputIds <- if (!is.null(inputObj) && is.function(inputObj$getIds)) {
        normalizeValues(inputObj$getIds())
      } else {
        character(0)
      }
      if (length(inputIds) == 0) return(character(0))

      conversionTable <- private$.conversionTable
      if (is.null(conversionTable) || !"input" %in% names(conversionTable) ||
          !"target" %in% names(conversionTable)) {
        return(character(0))
      }

      conversionInputs <- normalizeValues(conversionTable$input)
      mappedInputs <- inputIds[inputIds %in% conversionInputs]
      if (length(mappedInputs) == 0) return(character(0))

      results <- private$.results
      if (is.null(results) || nrow(results) == 0 || !"Positive Hits" %in% names(results)) {
        return(mappedInputs)
      }

      hitTokens <- unlist(
        strsplit(paste(results$`Positive Hits`, collapse = ","), ",", fixed = TRUE),
        use.names = FALSE
      )
      hitIds <- normalizeValues(hitTokens)
      if (length(hitIds) == 0) return(mappedInputs)

      mapInput <- trimws(as.character(conversionTable$input))
      mapTarget <- trimws(as.character(conversionTable$target))
      validMap <- nzchar(mapInput) & !is.na(mapInput) & nzchar(mapTarget) & !is.na(mapTarget)
      mapInput <- mapInput[validMap]
      mapTarget <- mapTarget[validMap]

      hitFromTargets <- unique(mapInput[mapTarget %in% hitIds])
      hitFromInputs <- mappedInputs[mappedInputs %in% hitIds]
      hitInputs <- unique(c(hitFromTargets, hitFromInputs))

      mappedInputs[!mappedInputs %in% hitInputs]
    },

    #' Get run details payload for v4 UI
    #'
    #' Returns a list of metadata about the run for the Run Details panel.
    #'
    #' @return List with parameters, results, and conversion info
    getRunDetailsPayload = function() {
      params <- private$.parameters %||% list()
      inputObj <- private$.input
      conversionTable <- private$.conversionTable

      normalizeValues <- function(values) {
        values <- trimws(as.character(values %||% character(0)))
        values <- values[nzchar(values) & !is.na(values)]
        unique(values)
      }

      inputIds <- if (!is.null(inputObj) && is.function(inputObj$getIds)) {
        normalizeValues(inputObj$getIds())
      } else {
        character(0)
      }
      inputSize <- length(inputIds)

      mappedInputs <- if (!is.null(conversionTable) && "input" %in% names(conversionTable)) {
        conversionInputs <- normalizeValues(conversionTable$input)
        inputIds[inputIds %in% conversionInputs]
      } else {
        character(0)
      }
      convertedSize <- length(mappedInputs)
      convertedPct <- if (inputSize > 0) {
        (convertedSize / inputSize) * 100
      } else {
        0
      }

      noHitAnalytes <- as.character(self$getNoHitAnalytes())
      analytesWithHits <- max(convertedSize - length(noHitAnalytes), 0)
      analytesWithHitsPct <- if (convertedSize > 0) {
        (analytesWithHits / convertedSize) * 100
      } else {
        0
      }

      unconvertedInputs <- inputIds[!inputIds %in% mappedInputs]

      selectedSources <- sort(normalizeValues(params$datasources))

      hitSources <- if (self$hasResults() && "Source" %in% names(private$.results)) {
        normalizeValues(private$.results$Source)
      } else {
        character(0)
      }
      hitSources <- sort(hitSources)

      totalTerms <- if (self$hasResults()) nrow(private$.results) else 0

      organismLabel <- as.character(self$organism)
      orgRow <- ORGANISMS[ORGANISMS$taxid == self$organism, , drop = FALSE]
      if (nrow(orgRow) > 0) {
        organismLabel <- as.character(orgRow$print_name[[1]])
      }

      metricCode <- trimws(as.character(params$metric %||% ""))
      metricLabel <- getMetricLabelForTool(self$toolName, metricCode, abbreviate = TRUE)
      significanceText <- formatSignificanceText(
        toolId = self$toolName,
        metricCode = metricCode,
        threshold = params$threshold,
        abbreviate = TRUE
      )

      backgroundModeRaw <- trimws(as.character(params$backgroundMode %||% "genome"))
      isCustomBackground <- backgroundModeRaw %in% c("custom", "user_list")
      backgroundMode <- if (isCustomBackground) "custom" else "tool_default"

      backgroundListName <- trimws(as.character(params$backgroundList %||% ""))
      if (!nzchar(backgroundListName) && !is.null(self$background) && !is.null(self$background$name)) {
        backgroundListName <- trimws(as.character(self$background$name))
      }

      backgroundDisplay <- "Tool default background"
      if (isCustomBackground) {
        backgroundListSize <- NA_integer_
        if (!is.null(self$background) && is.function(self$background$size)) {
          backgroundListSize <- suppressWarnings(as.integer(self$background$size()))
        }
        if (nzchar(backgroundListName) && !is.na(backgroundListSize)) {
          backgroundDisplay <- paste0(
            "Custom background list: ", backgroundListName,
            " (", backgroundListSize, ")"
          )
        } else if (nzchar(backgroundListName)) {
          backgroundDisplay <- paste0("Custom background list: ", backgroundListName)
        } else if (!is.na(backgroundListSize)) {
          backgroundDisplay <- paste0("Custom background list (", backgroundListSize, ")")
        } else {
          backgroundDisplay <- "Custom background list"
        }
      }

      inputListName <- trimws(as.character(params$geneListName %||% ""))
      if (!nzchar(inputListName) && !is.null(inputObj) && !is.null(inputObj$name)) {
        inputListName <- trimws(as.character(inputObj$name))
      }

      analyteMeta <- if (!is.null(inputObj) && !is.null(inputObj$analyteType)) {
        getAnalyteUIByType(inputObj$analyteType)
      } else {
        getAnalyteUIByType(AnalyteType$GENE)
      }
      analyteLabel <- as.character(analyteMeta$label %||% "Analytes")
      analyteSingular <- tools::toTitleCase(as.character(analyteMeta$singularKey %||% "analyte"))

      list(
        runId = self$runId,
        parameters = list(
          tool = self$toolName,
          paradigm = as.character(self$paradigm %||% ParadigmId$ORA),
          organism = organismLabel,
          inputListName = inputListName,
          inputSize = inputSize,
          background = backgroundDisplay,
          backgroundMode = backgroundMode,
          backgroundListName = backgroundListName,
          backgroundSize = self$getBackgroundSize(),
          threshold = params$threshold,
          selectedSources = selectedSources,
          namespace = trimws(as.character(params$namespace %||% "")),
          metricCode = metricCode,
          metricLabel = metricLabel,
          significanceText = significanceText,
          analyteLabel = analyteLabel,
          analyteSingular = analyteSingular
        ),
        results = list(
          totalTerms = totalTerms,
          hitSources = hitSources,
          analytesWithHits = analytesWithHits,
          analytesWithHitsPct = analytesWithHitsPct,
          convertedSize = convertedSize
        ),
        conversion = list(
          inputSize = inputSize,
          convertedSize = convertedSize,
          convertedPct = convertedPct,
          noHitAnalytes = noHitAnalytes,
          unconvertedInputs = unconvertedInputs
        )
      )
    },

    #' Set results from worker process
    #'
    #' Called after async execution to copy results from worker's R6 copy
    #' back to the main process's session object. Required because
    #' future::multisession runs in separate R processes where R6 object
    #' mutations don't affect the main process's copy.
    #'
    #' @param results Data frame of enrichment results
    #' @param backgroundSize Integer background size used
    #' @param conversionTable Data frame of ID conversion results (optional)
    #' @param backgroundConversionTable Data frame of background ID conversions (optional)
    setResultsFromWorker = function(results, backgroundSize = NULL,
                                     conversionTable = NULL,
                                     backgroundConversionTable = NULL) {
      private$.results <- results
      if (!is.null(backgroundSize)) {
        private$.backgroundSize <- backgroundSize
      }
      if (!is.null(conversionTable)) {
        private$.conversionTable <- conversionTable
      }
      if (!is.null(backgroundConversionTable)) {
        private$.backgroundConversionTable <- backgroundConversionTable
      }
      message("[ORAEnrichmentSession] Results set from worker: ", nrow(results), " rows, ",
              if (!is.null(conversionTable)) nrow(conversionTable) else 0, " conversions")
    },

    #' Print summary
    print = function() {
      cat(sprintf("<ORAEnrichmentSession> %s\n", self$id))
      cat(sprintf("  Tool: %s\n", self$toolName))
      cat(sprintf("  Organism: %s\n", self$organism))
      cat(sprintf("  Input: %d genes\n", private$.input$size()))
      if (self$hasBackground()) {
        cat(sprintf("  Background: %d genes\n", self$background$size()))
      }
      cat(sprintf("  Has results: %s\n", self$hasResults()))
      if (self$hasResults()) {
        cat(sprintf("  Result rows: %d\n", nrow(private$.results)))
      }
      invisible(self)
    }
  ),

  private = list(
    # Module session reference
    .moduleSession = NULL,

    # Parent Shiny session (for UI operations from output sessions)
    .parentSession = NULL,

    # Output sessions (BarchartOutputSession, etc.)
    .outputSessions = list(),

    # Table output session (separate from plot-based OutputSessions)
    .tableSession = NULL,

    # Instance counter for unique OutputSession IDs (prevents moduleServer conflicts)
    # Incremented each time createOutputSessions is called
    .outputSessionInstance = 0,

    # Background conversion table (separate from input conversion table)
    .backgroundConversionTable = NULL,

    # Output IDs for cleanup tracking
    .outputIds = list(
      tables = character(),
      text = character(),
      conversion = character()
    ),

    #' Convert gene IDs to tool-specific format
    #'
    #' @param geneList Character vector of gene symbols
    #' @param namespace Target namespace (or NULL for default)
    #' @return Data frame with (input, target, name) columns, or NULL on failure
    convertGeneIds = function(geneList, namespace) {
      # Determine default namespace if not specified
      if (is.null(namespace) || namespace == "Default namespace") {
        namespace <- getDefaultTargetNamespace(self$toolName, self$organism)
      }

      conversionTable <- tryCatch({
        if (namespace == "USERINPUT") {
          # No conversion needed - pass through
          data.frame(
            input = geneList,
            target = geneList,
            name = geneList,
            stringsAsFactors = FALSE
          )
        } else if (self$toolName == "STRING") {
          # STRING: Use STRING's get_string_ids API
          private$stringConvert(geneList)
        } else if (self$toolName == "PANTHER") {
          # PANTHER: Use PANTHER's geneinfo API
          private$pantherConvert(geneList)
        } else if (self$toolName == "GeneCodis") {
          # GeneCodis: No conversion needed
          data.frame(
            input = geneList,
            target = geneList,
            name = geneList,
            stringsAsFactors = FALSE
          )
        } else {
          # gProfiler, WebGestalt, enrichR: Use g:Profiler conversion
          private$gprofilerConvert(geneList, namespace)
        }
      }, error = function(e) {
        warning(sprintf("Gene conversion error for %s: %s", self$toolName, e$message))
        NULL
      })

      # Attach namespace as attribute
      if (!is.null(conversionTable)) {
        attr(conversionTable, "namespace") <- namespace
      }

      conversionTable
    },

    # getDefaultTargetNamespace REMOVED - use global function from enrich-main.R
    # Called as: getDefaultTargetNamespace(self$toolName, self$organism)

    #' STRING-specific gene conversion via STRING API
    stringConvert = function(geneList) {
      url <- "https://string-db.org/api/json/get_string_ids"
      params <- list(
        "identifiers" = paste0(geneList, collapse = "%0d"),
        "species" = self$organism
      )
      request <- httr::POST(url, body = params)
      if (httr::status_code(request) == 200) {
        result <- jsonlite::fromJSON(rawToChar(httr::content(request, "raw")))
        if (is.data.frame(result) && nrow(result) > 0) {
          result <- result[, c("queryItem", "stringId", "preferredName")]
          colnames(result) <- c("input", "target", "name")
          return(result)
        }
      }
      NULL
    },

    #' PANTHER-specific gene conversion
    pantherConvert = function(geneList) {
      url <- "https://pantherdb.org/services/oai/pantherdb/geneinfo"
      params <- list(
        "geneInputList" = paste0(geneList, collapse = ","),
        "organism" = self$organism
      )
      request <- httr::POST(url, body = params, encode = "form")
      if (httr::status_code(request) == 200) {
        pantherResponse <- jsonlite::fromJSON(rawToChar(httr::content(request, "raw")))
        if ("search" %in% names(pantherResponse) && "mapped_genes" %in% names(pantherResponse$search)) {
          mappedGenes <- pantherResponse$search$mapped_genes$gene
          if (length(mappedGenes) > 0) {
            return(data.frame(
              input = mappedGenes$mapped_id_list,
              target = mappedGenes$accession,
              name = mappedGenes$sf_name,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      NULL
    },

    #' gProfiler-based gene conversion
    gprofilerConvert = function(geneList, namespace) {
      organismShortName <- ORGANISMS[ORGANISMS$taxid == self$organism, ]$short_name
      result <- gprofiler2::gconvert(
        geneList,
        organism = organismShortName,
        target = namespace,
        mthreshold = 1,
        filter_na = TRUE
      )
      if (!is.null(result) && nrow(result) > 0) {
        result <- result[, c("input", "target", "name")]
        return(result)
      }
      NULL
    },

    #' Attach database links to Term_ID column
    #'
    #' Modifies the Term_ID column to include hyperlinks to external databases.
    #' @param df Data frame with enrichment results
    #' @return Modified data frame with hyperlinked Term_IDs
    attachDBLinks = function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)

      # Gene Ontology - stopChar=":" prevents matching "GOSLIM:*"
      df <- private$attachLinksToDF(df, "GO", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":")
      df <- private$attachLinksToDF(df, "GOSLIM", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":")

      # Protein domains and classifications
      df <- private$attachLinksToDF(df, "INTERPRO", "https://www.ebi.ac.uk/interpro/entry/InterPro/")
      df <- private$attachLinksToDF(df, "PFAM", "https://www.ebi.ac.uk/interpro/entry/pfam/")
      df <- private$attachLinksToDF(df, "UNIPROT", "https://www.uniprot.org/keywords/")
      df <- private$attachLinksToDF(df, "PANTHERPC", "https://pantherdb.org/panther/category.do?categoryAcc=")

      # Pathways
      df <- private$attachLinksToDF(df, "PANTHER Pathways", "http://www.pantherdb.org/pathway/pathDetail.do?clsAccession=")
      df <- private$attachLinksToDF(df, "REAC", "https://reactome.org/content/detail/")
      df <- private$attachLinksToDF(df, "WP", "https://www.wikipathways.org/index.php/Pathway:")
      df <- private$attachLinksToDF(df, "BioPlanet", "https://tripod.nih.gov/bioplanet/detail.jsp?pid=", urlSuffix = "&target=pathway")

      # Disease and phenotype ontologies
      df <- private$attachLinksToDF(df, "DO", "http://www.informatics.jax.org/disease/")
      df <- private$attachLinksToDF(df, "HP", "https://monarchinitiative.org/")
      df <- private$attachLinksToDF(df, "OMIM", "https://www.omim.org/entry/")
      df <- private$attachLinksToDF(df, "ORPHA", "https://www.orpha.net/consor/cgi-bin/OC_Exp.php?Lng=GB&Expert=", gSub = "ORPHA:")
      df <- private$attachLinksToDF(df, "WBP", "https://wormbase.org/species/all/phenotype/")
      df <- private$attachLinksToDF(df, "WBBT", "https://wormbase.org/species/all/anatomy_term/")
      df <- private$attachLinksToDF(df, "MGI", "https://www.informatics.jax.org/vocab/mp_ontology/")

      # Tissue ontologies
      df <- private$attachLinksToDF(df, "BTO", "https://www.ebi.ac.uk/ols/ontologies/bto/terms?iri=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FBTO_", gSub = "BTO:")

      # Regulatory elements
      df <- private$attachLinksToDF(df, "TF", "http://gene-regulation.com/cgi-bin/pub/databases/transfac/search.cgi?species=Homo_sapiens&factor=")
      df <- private$attachLinksToDF(df, "CollecTRI", "https://www.genecards.org/cgi-bin/carddisp.pl?gene=")
      df <- private$attachLinksToDF(df, "MIRNA", "https://www.mirbase.org/textsearch.shtml?q=", gSub = "MIRNA:")

      # Pharmacogenomics
      df <- private$attachLinksToDF(df, "PharmGKB", "https://www.clinpgx.org/chemical/")

      # Literature (PUBMED from STRING)
      df <- private$attachLinksToDF(df, "PUBMED", "https://pubmed.ncbi.nlm.nih.gov/", gSub = "PMID:")

      # Published signatures from Enrichr: GeneSigDB terms start with the PMID,
      # Rummagene terms with the PubMed Central id of the paper.
      df <- private$attachLiteratureLinks(df)

      # KEGG (special handling)
      df <- private$attachKEGGLinksToDF(df)

      # DISGENET special handling
      df <- private$attachDISGENETLinks(df)

      df
    },

    #' Attach links to a specific datasource in the data frame
    attachLinksToDF = function(df, sourceId, url, stopChar = "$", gSub = NULL, urlSuffix = "") {
      pattern <- paste0("^", sourceId, stopChar)
      matches <- grepl(pattern, df$Source)

      if (any(matches)) {
        linksVector <- df$Term_ID[matches]
        gSubLinksVector <- if (!is.null(gSub)) gsub(gSub, "", linksVector) else linksVector

        df$Term_ID[matches] <- paste0(
          "<a href='", url, flameUrlPart(gSubLinksVector), urlSuffix,
          "' target='_blank' rel='noopener noreferrer'>",
          htmltools::htmlEscape(linksVector), "</a>"
        )
      }
      df
    },

    attachLiteratureLinks = function(df) {
      for (src in c("GENESIGDB", "RUMMAGENE")) {
        m <- which(df$Source == src)
        if (!length(m)) next
        ids <- df$Term_ID[m]
        name <- sub("^[A-Z]+:", "", ids)
        url <- if (src == "GENESIGDB") {
          ifelse(grepl("^[0-9]{5,9}", name), paste0("https://pubmed.ncbi.nlm.nih.gov/", sub("^([0-9]{5,9}).*", "\\1", name), "/"), NA)
        } else {
          { pmc <- regmatches(name, regexpr("PMC[0-9]+", name)); hit <- grepl("PMC[0-9]+", name); u <- rep(NA_character_, length(name)); u[hit] <- paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/", pmc, "/"); u }
        }
        ok <- !is.na(url)
        df$Term_ID[m[ok]] <- paste0("<a href='", url[ok], "' target='_blank' rel='noopener noreferrer'>", htmltools::htmlEscape(ids[ok]), "</a>")
      }
      df
    },

    #' Attach KEGG-specific links with optional gene highlighting
    attachKEGGLinksToDF = function(df) {
      keggMatches <- grepl("^KEGG$", df$Source)
      if (!any(keggMatches)) return(df)

      keggName <- ORGANISMS[ORGANISMS$taxid == self$organism, ]$kegg_name
      if (is.na(keggName)) return(df)  # No KEGG support for this organism

      linksVector <- df$Term_ID[keggMatches]

      if (self$toolName == "STRING") {
        # STRING provides organism-specific KEGG IDs already
        df$Term_ID[keggMatches] <- paste0(
          "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
          flameUrlPart(linksVector), "' target='_blank' rel='noopener noreferrer'>",
          htmltools::htmlEscape(linksVector), "</a>"
        )
      } else {
        # Traditional tools - need to add organism prefix
        df$Term_ID[keggMatches] <- paste0(
          "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
          flameUrlPart(gsub("KEGG:|map", keggName, linksVector)),
          "' target='_blank' rel='noopener noreferrer'>", htmltools::htmlEscape(linksVector), "</a>"
        )
      }
      df
    },

    #' Attach DISGENET-specific links
    attachDISGENETLinks = function(df) {
      disgenetMatches <- df$Source == "DISGENET"
      if (any(disgenetMatches)) {
        df$Term_ID[disgenetMatches] <- paste0(
          "<a href='https://www.disgenet.org/search/0/",
          flameUrlPart(df$Term_ID_noLinks[disgenetMatches]),
          "/' target='_blank' rel='noopener noreferrer'>",
          htmltools::htmlEscape(df$Term_ID_noLinks[disgenetMatches]),
          "</a>"
        )
      }
      df
    },

    # -------------------------------------------------------------------------
    # Result Transformation (encapsulated from enrich-general.R)
    # -------------------------------------------------------------------------

    #' Transform enrichment result table to standard display format
    #'
    #' Adds calculated metric columns and selects
    #' display columns in standard order.
    #'
    #' @param df Data frame from strategy with ENRICHMENT_DF_COLNAMES
    #' @return Transformed data frame ready for display
    transformResultTable = function(df) {
      df$`-log10Pvalue` <- as.numeric(
        format(
          -log10(df$`P-value`),
          format = "e", digits = 3
        )
      )
      # Keep P-value as numeric for downstream filtering; formatSignif() handles display formatting
      df$`P-value` <- as.numeric(df$`P-value`)
      df$`Gene Ratio` <- private$calculateGeneRatio(
        df$`Intersection Size`,
        df$`Query size`
      )
      df$`Term Coverage %` <- private$calculateTermCoverage(
        df$`Intersection Size`,
        df$`Term Size`
      )
      df$`Fold Enrichment` <- private$calculateFoldEnrichment(
        df$`Intersection Size`,
        df$`Query size`,
        df$`Term Size`,
        private$.backgroundSize
      )

      # Define base columns to keep
      baseColumns <- c(
        "Source", "Term_ID", "Function", "P-value", "-log10Pvalue",
        "Term Size", "Query size", "Intersection Size",
        "Gene Ratio", "Term Coverage %", "Fold Enrichment", "Positive Hits"
      )

      # Preserve Term_ID_noLinks if it exists (WebGestalt includes links pre-attached)
      if ("Term_ID_noLinks" %in% colnames(df)) {
        baseColumns <- c(baseColumns, "Term_ID_noLinks")
      }

      df[, baseColumns]
    },

    #' Calculate gene ratio
    #'
    #' @param hitGenesCount Integer. Number of genes in intersection.
    #' @param queryGenesCount Integer. Number of genes in query.
    #' @return Numeric. Fraction of query genes present in the term.
    calculateGeneRatio = function(hitGenesCount, queryGenesCount) {
      hitGenesCount <- suppressWarnings(as.numeric(hitGenesCount))
      queryGenesCount <- suppressWarnings(as.numeric(queryGenesCount))
      ratio <- hitGenesCount / queryGenesCount
      ratio[!is.finite(ratio) | queryGenesCount <= 0] <- NA_real_
      ratio
    },

    #' Calculate term coverage as percentage
    #'
    #' @param hitGenesCount Integer. Number of genes in intersection.
    #' @param databaseGenesCount Integer. Total genes in term.
    #' @return Numeric. Term coverage as percentage.
    calculateTermCoverage = function(hitGenesCount, databaseGenesCount) {
      hitGenesCount <- suppressWarnings(as.numeric(hitGenesCount))
      databaseGenesCount <- suppressWarnings(as.numeric(databaseGenesCount))
      coverage <- (hitGenesCount / databaseGenesCount) * 100
      coverage[!is.finite(coverage) | databaseGenesCount <= 0] <- NA_real_
      round(coverage, 2)
    },

    #' Calculate fold enrichment
    #'
    #' @param hitGenesCount Integer. Number of genes in intersection.
    #' @param queryGenesCount Integer. Number of genes in query.
    #' @param databaseGenesCount Integer. Total genes in term.
    #' @param backgroundSize Integer. Total background/universe size.
    #' @return Numeric. Fold enrichment, or NA when background is unavailable.
    calculateFoldEnrichment = function(hitGenesCount, queryGenesCount,
                                       databaseGenesCount, backgroundSize) {
      hitGenesCount <- suppressWarnings(as.numeric(hitGenesCount))
      queryGenesCount <- suppressWarnings(as.numeric(queryGenesCount))
      databaseGenesCount <- suppressWarnings(as.numeric(databaseGenesCount))
      backgroundSize <- suppressWarnings(as.numeric(backgroundSize))

      if (length(backgroundSize) == 0 || is.na(backgroundSize) ||
          !is.finite(backgroundSize) || backgroundSize <= 0) {
        return(rep(NA_real_, length(hitGenesCount)))
      }

      enrichment <- (hitGenesCount / queryGenesCount) / (databaseGenesCount / backgroundSize)
      enrichment[
        !is.finite(enrichment) |
          queryGenesCount <= 0 |
          databaseGenesCount <= 0
      ] <- NA_real_
      enrichment
    }
  )
)
