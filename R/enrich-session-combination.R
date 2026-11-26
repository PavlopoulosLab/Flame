# =============================================================================
# FLAME Combination Session
# =============================================================================
#
# Pure data/computation layer for comparing results across multiple enrichment
# runs. This R6 class owns the combination state and encapsulates all logic for
# computing, filtering, and querying combined results.
#
# Responsibilities:
# - Compute combined results from specified enrichment sessions
# - Calculate Fisher's combined p-values and hit statistics
# - Provide query methods for filtered results, datasources, tools, rank
#
# Pattern:
# - Instantiated once per user session in server.R
# - compare(sessionIds) called by event handlers to trigger computation
# - Query methods (getResults, applyComparisonFilters, etc.) called by renderers
#
# Dependencies:
# - enrich-session-registry.R (for EnrichmentSessionRegistry)
#
# =============================================================================

# =============================================================================
# R6 SESSION CLASS
# =============================================================================

CombinationSession <- R6::R6Class(
  "CombinationSession",

  public = list(
    #' Initialize a CombinationSession
    #'
    #' @param enrichmentRegistry EnrichmentSessionRegistry instance
    initialize = function(enrichmentRegistry) {
      if (!inherits(enrichmentRegistry, "EnrichmentSessionRegistry")) {
        stop("enrichmentRegistry must be an EnrichmentSessionRegistry")
      }
      private$.enrichmentRegistry <- enrichmentRegistry
      private$.results <- NULL
      private$.sessionIds <- NULL
      private$.runMetadata <- NULL
      private$.perRunData <- NULL
      private$.runMembership <- NULL
    },

    #' Compare specific enrichment sessions by ID
    #'
    #' Takes explicit session IDs, fetches each from the registry, filters to
    #' those with results, snapshots run metadata, and computes combined results.
    #' Guards for < 2 valid sessions (returns invisible self with NULL results).
    #'
    #' @param sessionIds Character vector of session IDs to compare
    #' @return Invisible self for chaining
    compare = function(sessionIds) {
      stopifnot(is.character(sessionIds), length(sessionIds) >= 1)

      # Fetch sessions from registry — resolve by runId since JS sends
      # card IDs (e.g. "string_1") not registry keys ("functional_string_1").
      # TODO: After the ID unification (see enrich-session-base.R TODO),
      # this should be a single registry$get(id) call per session.
      sessions <- lapply(sessionIds, function(id) {
        # Try direct registry key first
        direct <- private$.enrichmentRegistry$get(id)
        if (!is.null(direct)) return(direct)

        # Match by session$runId field
        allSessions <- private$.enrichmentRegistry$getAll()
        matches <- Filter(
          function(s) !is.null(s) && identical(as.character(s$runId %||% ""), id),
          allSessions
        )
        if (length(matches) > 0) return(matches[[1]])

        # Legacy fallback
        private$.enrichmentRegistry$get(paste0("functional_", id))
      })
      names(sessions) <- sessionIds

      # Remove NULLs (sessions not found in registry)
      sessions <- Filter(Negate(is.null), sessions)

      # Snapshot run metadata BEFORE filtering to those with results,
      # so we capture metadata even if a session finishes between now
      # and a later getRunMetadata() call
      debugComparison <- isTRUE(getOption("flame.debug.comparison", FALSE))
      private$.runMetadata <- lapply(names(sessions), function(id) {
        s <- sessions[[id]]
        params <- tryCatch(s$getParameters(), error = function(e) list())
        inputObj <- tryCatch(s$getInput(), error = function(e) NULL)

        # Resolve organism display name from taxid (s$organism is always set;
        # AnalyteList$organismName is NOT — controller creates lists without taxid)
        orgRow <- ORGANISMS[ORGANISMS$taxid == s$organism, , drop = FALSE]
        organismName <- if (nrow(orgRow) > 0) as.character(orgRow$print_name[1]) else NA_character_

        meta <- list(
          sessionId = id,
          toolName = s$toolName,
          displayNumber = s$displayNumber,
          paradigm = s$paradigm,
          organism = organismName,
          datasources = params$datasources,
          threshold = params$threshold,
          metric = params$metric,
          namespace = params$namespace,
          backgroundMode = params$backgroundMode,
          analyteType = if (!is.null(inputObj)) inputObj$analyteType else NULL,
          listName = if (!is.null(inputObj)) inputObj$name else params$geneListName,
          inputSize = private$inputSizes(s, inputObj)$input,
          mappedSize = private$inputSizes(s, inputObj)$mapped
        )

        if (debugComparison) {
          cat("[COMPARISON] run metadata snapshot: ", id, " -> tool=", s$toolName,
              " paradigm=", s$paradigm %||% "NA",
              " organism=", meta$organism %||% "NA",
              " datasources=", length(meta$datasources %||% character(0)),
              "\n", sep = "")
        }

        meta
      })
      names(private$.runMetadata) <- names(sessions)

      # Store the IDs we were asked to compare
      private$.sessionIds <- sessionIds

      # Filter to sessions that have results
      sessionsWithResults <- Filter(function(s) s$hasResults(), sessions)

      if (length(sessionsWithResults) < 2) {
        private$.results <- NULL
        private$.perRunData <- NULL
        private$.runMembership <- NULL
        return(invisible(self))
      }

      # Compute combined results
      private$.results <- private$computeCombinedResults(sessionsWithResults)
      invisible(self)
    },

    #' Get run metadata snapshot
    #'
    #' Returns structured metadata captured at compare-time. Never re-queries
    #' the registry, so results are stable even if sessions are later removed.
    #'
    #' @return Named list of lists, each with sessionId, toolName, displayNumber;
    #'   or NULL if compare() has not been called
    getRunMetadata = function() {
      private$.runMetadata
    },

    #' Get per-run detail data
    #'
    #' Returns structured per-run data captured at compute-time.
    #' Nested list: termId -> sessionId -> { pvalue, geneCount, genes, found }
    #'
    #' @return Named list of per-run data or NULL if compare() has not been called
    getPerRunData = function() {
      private$.perRunData
    },

    #' Get per-run fold enrichment values
    #'
    #' Returns per-run FE computed at combination time.
    #' Nested list: termId -> sessionId -> FE (numeric).
    #' Used by bubble/heatmap sessions to recompute mean FE
    #' over only the active (filtered) runs.
    #'
    #' @return Named list or NULL if compare() has not been called
    getPerRunFE = function() {
      private$.perRunFE
    },

    #' Get per-term run membership
    #'
    #' Returns which sessions contain each term. Named list:
    #' termId -> character vector of session IDs.
    #'
    #' @return Named list or NULL if compare() has not been called
    getRunMembership = function() {
      private$.runMembership
    },

    #' Check if combination is available (2+ sessions with results)
    #'
    #' @return Logical TRUE if combination can be shown
    isAvailable = function() {
      !is.null(private$.results) && nrow(private$.results) > 0
    },

    #' Get all results (unfiltered)
    #'
    #' @return DataFrame of all combined results or NULL
    getResults = function() {
      private$.results
    },

    #' Apply the 9-field comparison filter chain
    #'
    #' Shared filter method called by all 3 tab sessions. Applies the
    #' full filter chain in order and returns the filtered dataframe plus
    #' recomputed effective agreement. Callers always pass their stored
    #' originalData (raw from getResults()), never a previous filtered view.
    #'
    #' Filter chain order:
    #' 1. Runs filter (Any/All mode using structured run membership)
    #' 2. Effective agreement recomputation from active runs
    #' 3. Consensus min/max on effective agreement
    #' 4. Source filter
    #' 5. Search filter (case-insensitive on Function, "Term ID", Term_ID_noLinks)
    #' 6. P-value filter on CombPvalue
    #' 7. Term size filter on "Term Size"
    #'
    #' @param data DataFrame — raw combined results from getResults()
    #' @param filters List with 9 fields: runs, runsMode, sources, search,
    #'   pvalueMax, consensusMin, consensusMax, termSizeMin, termSizeMax
    #' @return List with: data (filtered DataFrame), effectiveAgreement
    #'   (named integer vector keyed by Term_ID_noLinks)
    applyComparisonFilters = function(data, filters) {
      debugComparison <- isTRUE(getOption("flame.debug.comparison", FALSE))

      if (is.null(data) || nrow(data) == 0) {
        if (debugComparison) {
          message("[COMPARISON FILTER] applyComparisonFilters: empty input data")
        }
        return(list(data = data, effectiveAgreement = integer(0)))
      }

      membership <- self$getRunMembership()
      # [[ ]] avoids R's partial matching (`filters$runs` would match `runsMode`).
      selectedRuns <- filters[["runs"]]
      runsMode <- filters[["runsMode"]] %||% "any"

      if (debugComparison) {
        message("[COMPARISON FILTER] chain start: ", nrow(data), " rows, ",
                length(selectedRuns), " selected runs, mode=", runsMode)
      }

      # --- Step 1: Runs filter ---
      if (is.null(selectedRuns) || length(selectedRuns) == 0) {
        # Zero runs selected = no terms pass
        if (debugComparison) {
          message("[COMPARISON FILTER] 0 runs selected -> empty result")
        }
        emptyDf <- data[integer(0), , drop = FALSE]
        return(list(data = emptyDf, effectiveAgreement = integer(0)))
      }

      if (!is.null(membership)) {
        termIds <- as.character(data$Term_ID_noLinks)

        if (runsMode == "all") {
          # ALL mode: keep term if ALL selected runs found it
          keep <- vapply(termIds, function(tid) {
            termRuns <- membership[[tid]]
            if (is.null(termRuns)) return(FALSE)
            all(selectedRuns %in% termRuns)
          }, logical(1))
        } else {
          # ANY mode (default): keep term if ANY selected run found it
          keep <- vapply(termIds, function(tid) {
            termRuns <- membership[[tid]]
            if (is.null(termRuns)) return(FALSE)
            length(intersect(termRuns, selectedRuns)) > 0
          }, logical(1))
        }

        data <- data[keep, , drop = FALSE]
      }

      if (debugComparison) {
        message("[COMPARISON FILTER] after runs filter (", runsMode, "): ", nrow(data), " rows")
      }

      if (nrow(data) == 0) {
        return(list(data = data, effectiveAgreement = integer(0)))
      }

      # --- Step 2: Effective agreement recomputation ---
      termIds <- as.character(data$Term_ID_noLinks)
      effectiveAgreement <- vapply(termIds, function(tid) {
        termRuns <- membership[[tid]]
        if (is.null(termRuns)) return(0L)
        as.integer(length(intersect(termRuns, selectedRuns)))
      }, integer(1))
      names(effectiveAgreement) <- termIds

      # --- Step 3: Consensus min/max on effective agreement ---
      consensusMin <- filters$consensusMin
      consensusMax <- filters$consensusMax

      if (!is.null(consensusMin) && is.finite(as.numeric(consensusMin))) {
        keep <- effectiveAgreement >= as.numeric(consensusMin)
        data <- data[keep, , drop = FALSE]
        effectiveAgreement <- effectiveAgreement[keep]
      }

      if (!is.null(consensusMax) && is.finite(as.numeric(consensusMax))) {
        keep <- effectiveAgreement <= as.numeric(consensusMax)
        data <- data[keep, , drop = FALSE]
        effectiveAgreement <- effectiveAgreement[keep]
      }

      if (debugComparison) {
        message("[COMPARISON FILTER] after consensus (min=",
                consensusMin %||% "NULL", ", max=", consensusMax %||% "NULL",
                "): ", nrow(data), " rows")
      }

      if (nrow(data) == 0) {
        return(list(data = data, effectiveAgreement = integer(0)))
      }

      # --- Step 4: Source filter ---
      if (!is.null(filters$sources) && length(filters$sources) > 0) {
        data <- data[data$Source %in% filters$sources, , drop = FALSE]
        effectiveAgreement <- effectiveAgreement[as.character(data$Term_ID_noLinks)]
      }

      # --- Step 5: Search filter ---
      if (!is.null(filters$search) && nzchar(filters$search)) {
        data <- filterRowsBySearchQuery(
          data,
          filters$search,
          c("Function", "Term ID", "Term_ID_noLinks")
        )
        effectiveAgreement <- effectiveAgreement[as.character(data$Term_ID_noLinks)]
      }

      # --- Step 6: P-value filter on CombPvalue ---
      # TODO: This filters rows using the static CombPvalue column (Fisher's
      # combined p-value across ALL runs). But bubble/heatmap sessions now
      # recompute CombPvalue at render time for only the active (filtered)
      # runs. This creates an inconsistency: a term might be discarded here
      # because its all-runs CombPvalue exceeds the threshold, even though
      # its active-runs CombPvalue would pass — or vice versa.
      # Fix: recompute CombPvalue for active runs here before filtering.
      # Requires access to perRunData + the active run IDs from filters$runs.
      if (!is.null(filters$pvalueMax) && is.finite(as.numeric(filters$pvalueMax))) {
        pvals <- suppressWarnings(as.numeric(as.character(data$CombPvalue)))
        keep <- !is.na(pvals) & pvals <= as.numeric(filters$pvalueMax)
        data <- data[keep, , drop = FALSE]
        effectiveAgreement <- effectiveAgreement[as.character(data$Term_ID_noLinks)]
      }

      # --- Step 7: Term size filter ---
      if ("Term Size" %in% names(data)) {
        termSizes <- suppressWarnings(as.numeric(as.character(data$`Term Size`)))

        if (!is.null(filters$termSizeMin) && is.finite(as.numeric(filters$termSizeMin))) {
          keep <- !is.na(termSizes) & termSizes >= as.numeric(filters$termSizeMin)
          data <- data[keep, , drop = FALSE]
          termSizes <- termSizes[keep]
          effectiveAgreement <- effectiveAgreement[as.character(data$Term_ID_noLinks)]
        }

        if (!is.null(filters$termSizeMax) && is.finite(as.numeric(filters$termSizeMax))) {
          keep <- !is.na(termSizes) & termSizes <= as.numeric(filters$termSizeMax)
          data <- data[keep, , drop = FALSE]
          effectiveAgreement <- effectiveAgreement[as.character(data$Term_ID_noLinks)]
        }
      }

      if (debugComparison) {
        message("[COMPARISON FILTER] chain complete: ", nrow(data), " rows remaining")
      }

      list(data = data, effectiveAgreement = effectiveAgreement)
    },
    #' Clear combination state (data only, no UI side effects)
    #'
    #' Resets all data fields to NULL.
    #'
    #' @return Invisible self for chaining
    clear = function() {
      private$.results <- NULL
      private$.sessionIds <- NULL
      private$.runMetadata <- NULL
      private$.perRunData <- NULL
      private$.perRunFE <- NULL
      private$.runMembership <- NULL
      invisible(self)
    },

    #' Clean up combination state
    #'
    #' Delegates to clear(), which resets all data.
    cleanup = function() {
      self$clear()
    }
  ),

  private = list(
    # Dependencies
    .enrichmentRegistry = NULL,

    # State
    .results = NULL,       # Combined DataFrame
    .sessionIds = NULL,    # Character vector of session IDs passed to compare()
    .runMetadata = NULL,   # Named list of metadata snapshots from compare()
    .perRunData = NULL,    # Nested list: termId -> sessionId -> {pvalue, geneCount, genes, found}
    .perRunFE = NULL,      # Nested list: termId -> sessionId -> FE (numeric)
    .runMembership = NULL, # Named list: termId -> c(sessionId1, sessionId2, ...)


    # =========================================================================
    # COMPUTATION METHODS
    # =========================================================================

    #' Compute combined results from sessions
    #'
    #' @param sessionsWithResults List of ORAEnrichmentSession objects
    #' @return DataFrame with combined results
    computeCombinedResults = function(sessionsWithResults) {
      # Build named list of results keyed by session ID
      functionalEnrichmentResults <- lapply(
        sessionsWithResults,
        function(s) s$getResults()
      )
      names(functionalEnrichmentResults) <- names(sessionsWithResults)

      # --- Normalize Positive Hits to original input IDs ---
      # Each tool may return gene hits in its own namespace (e.g., ENSP, Entrez).
      # Reverse-map to original input IDs using each session's conversion table
      # so that cross-tool intersection/union in calculateTermHitStats() works.
      for (sid in names(sessionsWithResults)) {
        s <- sessionsWithResults[[sid]]
        ct <- tryCatch(s$getConversionTable(), error = function(e) NULL)
        df <- functionalEnrichmentResults[[sid]]
        if (is.null(ct) || is.null(df) || nrow(df) == 0 ||
            !("Positive Hits" %in% colnames(df))) next

        reverseMap <- stats::setNames(as.character(ct$input), as.character(ct$target))

        functionalEnrichmentResults[[sid]]$`Positive Hits` <- vapply(
          df$`Positive Hits`,
          function(hits) {
            if (is.na(hits) || !nzchar(trimws(hits))) return("")
            genes <- trimws(strsplit(trimws(hits), ",\\s*")[[1]])
            mapped <- reverseMap[genes]
            mapped[is.na(mapped)] <- genes[is.na(mapped)]
            paste(unique(mapped), collapse = ", ")
          },
          character(1),
          USE.NAMES = FALSE
        )
      }

      # --- Per-run detail extraction (Phase 4A) ---
      # Build BEFORE sessionIdToDisplayName() so keys are raw session IDs.
      private$.perRunData <- private$buildPerRunData(
        functionalEnrichmentResults, names(sessionsWithResults)
      )

      # --- Run membership (Phase 4A) ---
      # Derive from perRunData: termId -> session IDs where found=TRUE.
      private$.runMembership <- private$buildRunMembership(
        private$.perRunData, names(sessionsWithResults)
      )

      # --- Term Size aggregation ---
      # For each term, take max Term Size across runs. If a session's results
      # lack a "Term Size" column, contribute NA_integer_.
      termSizeAgg <- private$aggregateTermSize(functionalEnrichmentResults)

      # --- Fold enrichment aggregation ---
      # Per-run FE values are stored in .perRunFE (termId -> sessionId -> FE)
      # so bubble/heatmap sessions can recompute mean FE over only active runs.
      # The static FoldEnrichment column below is the all-runs mean (used as
      # initial default before any run filtering).
      foldEnrichmentAgg <- private$aggregateFoldEnrichment(
        functionalEnrichmentResults, sessionsWithResults
      )

      # Combine all results
      combined <- dplyr::bind_rows(functionalEnrichmentResults, .id = "Tool")

      # Select relevant columns
      combined <- combined[, c("Source", "Term_ID", "Function",
                               "Term_ID_noLinks", "Tool", "P-value",
                               "Positive Hits")]

      # Convert session ID to display name
      combined$Tool <- sapply(combined$Tool, private$sessionIdToDisplayName)

      # Calculate hit statistics per term
      termHitStats <- private$calculateTermHitStats(combined)

      # Deduplicate (Term_ID, Tool) pairs before grouping
      termToolMatching <- combined %>%
        dplyr::distinct(Term_ID_noLinks, Tool, .keep_all = TRUE) %>%
        dplyr::group_by(Term_ID_noLinks) %>%
        dplyr::summarise(Tools = toString(Tool), .groups = "drop")

      # Join and calculate Fisher stats
      combined <- plyr::join(termToolMatching, combined, type = "left",
                             by = "Term_ID_noLinks")
      combined <- private$calculateFisherStats(combined)
      combined <- dplyr::distinct(combined, Term_ID_noLinks, Tools,
                                  .keep_all = TRUE)

      # Join hit stats
      combined <- plyr::join(combined, termHitStats, type = "left",
                             by = "Term_ID_noLinks")

      # Join Term Size aggregation
      combined <- plyr::join(combined, termSizeAgg, type = "left",
                             by = "Term_ID_noLinks")

      # Join Fold Enrichment aggregation
      combined <- plyr::join(combined, foldEnrichmentAgg, type = "left",
                             by = "Term_ID_noLinks")

      # Select final columns (machine-friendly names, no HTML)
      combined <- combined[, c("Source", "Term_ID", "Function",
                               "Term_ID_noLinks", "Tools",
                               "Chisq", "P_value_combined",
                               "Term Size",
                               "Intersection_Hits", "Union_Hits",
                               "Intersection_Count", "Union_Count",
                               "Hit_Summary", "FoldEnrichment")]
      names(combined) <- c("Source", "Term ID", "Function",
                           "Term_ID_noLinks", "Tools",
                           "Chisq", "CombPvalue",
                           "Term Size",
                           "Intersection_Hits", "Union_Hits",
                           "Intersection_Count", "Union_Count",
                           "Hit_Summary", "FoldEnrichment")

      # Calculate rank (number of tools)
      combined$Rank <- lengths(
        regmatches(combined$Tools, gregexpr(",", combined$Tools))
      ) + 1

      if (isTRUE(getOption("flame.debug.comparison", FALSE))) {
        cat("[COMPARISON] computeCombinedResults: ", nrow(combined), " terms, ",
            "columns=[", paste(colnames(combined), collapse = ", "), "], ",
            "rank range ", min(combined$Rank), "-", max(combined$Rank), "\n", sep = "")
      }

      return(combined)
    },

    #' Convert session ID to display name
    #'
    #' Single source of truth for the format: "toolName (displayNumber)"
    #'
    #' @param sessionId Full session ID (e.g., "functional_gProfiler_5")
    #' @return Display name (e.g., "gProfiler (1)")
    sessionIdToDisplayName = function(sessionId) {
      enrichSession <- private$.enrichmentRegistry$get(sessionId)
      if (!is.null(enrichSession)) {
        paste0(enrichSession$toolName, " (", enrichSession$displayNumber, ")")
      } else {
        # Fallback: parse from key
        parts <- strsplit(sessionId, "_")[[1]]
        paste(parts[-1], collapse = "_")
      }
    },

    #' Calculate term hit statistics
    #'
    #' @param comboData Combined DataFrame
    #' @return DataFrame with hit intersection/union per term
    calculateTermHitStats = function(comboData) {
      # Get unique hits per term per tool
      hitsPerTermTool <- comboData %>%
        dplyr::select(Term_ID_noLinks, Tool, `Positive Hits`) %>%
        dplyr::distinct(Term_ID_noLinks, Tool, .keep_all = TRUE)

      # Calculate intersection and union per term
      hitsPerTermTool %>%
        dplyr::group_by(Term_ID_noLinks) %>%
        dplyr::summarise(
          Intersection_Hits = {
            hitLists <- strsplit(trimws(`Positive Hits`), ",\\s*")
            if (length(hitLists) == 1) {
              paste(hitLists[[1]], collapse = ", ")
            } else {
              paste(Reduce(intersect, hitLists), collapse = ", ")
            }
          },
          Union_Hits = {
            hitLists <- strsplit(trimws(`Positive Hits`), ",\\s*")
            paste(Reduce(union, hitLists), collapse = ", ")
          },
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          Intersection_Count = sapply(
            strsplit(Intersection_Hits, ",\\s*"),
            function(x) sum(trimws(x) != "")
          ),
          Union_Count = sapply(
            strsplit(Union_Hits, ",\\s*"),
            function(x) sum(trimws(x) != "")
          ),
          Hit_Summary = paste0(Intersection_Count, " / ", Union_Count, " genes")
        ) %>%
        dplyr::select(Term_ID_noLinks, Intersection_Hits, Union_Hits,
                      Intersection_Count, Union_Count, Hit_Summary)
    },

    #' Aggregate Term Size across runs
    #'
    #' For each term (keyed by Term_ID_noLinks), take max(Term Size) across
    #' all runs. If a session's result table lacks a "Term Size" column,
    #' contribute NA_integer_ for that run. Uses na.rm = TRUE so a single
    #' valid value wins. If ALL runs return NA, the result is NA (not -Inf).
    #'
    #' @param resultsList Named list of result DataFrames (one per session)
    #' @return DataFrame with columns Term_ID_noLinks and `Term Size`
    aggregateTermSize = function(resultsList) {
      # Extract Term_ID_noLinks + Term Size from each session
      termSizeParts <- lapply(resultsList, function(df) {
        if (is.null(df) || nrow(df) == 0) return(NULL)
        if (!("Term_ID_noLinks" %in% colnames(df))) return(NULL)

        termIds <- df$Term_ID_noLinks

        if ("Term Size" %in% colnames(df)) {
          ts <- suppressWarnings(as.integer(df$`Term Size`))
        } else {
          ts <- rep(NA_integer_, nrow(df))
        }

        data.frame(
          Term_ID_noLinks = termIds,
          TermSizeValue = ts,
          stringsAsFactors = FALSE
        )
      })

      termSizeParts <- Filter(Negate(is.null), termSizeParts)

      if (length(termSizeParts) == 0) {
        return(data.frame(
          Term_ID_noLinks = character(0),
          `Term Size` = integer(0),
          stringsAsFactors = FALSE, check.names = FALSE
        ))
      }

      allTermSizes <- do.call(rbind, termSizeParts)

      # Aggregate: max per term, na.rm = TRUE.
      # na.action = na.pass ensures terms where ALL runs contribute NA are
      # still present in the result (default na.omit would drop them).
      agg <- stats::aggregate(
        TermSizeValue ~ Term_ID_noLinks,
        data = allTermSizes,
        na.action = na.pass,
        FUN = function(x) {
          val <- max(x, na.rm = TRUE)
          # max() on all-NA with na.rm = TRUE returns -Inf; replace with NA
          if (is.infinite(val)) NA_integer_ else as.integer(val)
        }
      )

      colnames(agg) <- c("Term_ID_noLinks", "Term Size")
      agg
    },

    #' Aggregate fold enrichment across runs
    #'
    #' For each term in each run, computes fold enrichment:
    #'   FE = (Intersection_Size / Query_Size) / (Term_Size / Background_Size)
    #' Then takes the arithmetic mean per term across runs.
    #'
    #' @param resultsList Named list of result DataFrames (one per session)
    #' @param sessionsWithResults Named list of ORA session objects
    #' @return DataFrame with columns Term_ID_noLinks and FoldEnrichment
    aggregateFoldEnrichment = function(resultsList, sessionsWithResults) {
      sessionIds <- names(resultsList)
      feParts <- lapply(sessionIds, function(sid) {
        df <- resultsList[[sid]]
        s <- sessionsWithResults[[sid]]
        if (is.null(df) || nrow(df) == 0) return(NULL)
        if (!("Term_ID_noLinks" %in% colnames(df))) return(NULL)

        bgSize <- tryCatch(s$getBackgroundSize(), error = function(e) NULL)
        if (is.null(bgSize) || !is.finite(bgSize) || bgSize <= 0) bgSize <- NA_real_

        intSize <- suppressWarnings(as.numeric(as.character(df$`Intersection Size`)))
        querySize <- suppressWarnings(as.numeric(as.character(df$`Query size`)))
        termSize <- suppressWarnings(as.numeric(as.character(df$`Term Size`)))

        fe <- ifelse(
          is.na(intSize) | is.na(querySize) | querySize <= 0 |
            is.na(termSize) | termSize <= 0 | is.na(bgSize),
          NA_real_,
          (intSize / querySize) / (termSize / bgSize)
        )

        data.frame(
          Term_ID_noLinks = df$Term_ID_noLinks,
          SessionId = sid,
          FE = fe,
          stringsAsFactors = FALSE
        )
      })

      feParts <- Filter(Negate(is.null), feParts)

      if (length(feParts) == 0) {
        private$.perRunFE <- list()
        return(data.frame(
          Term_ID_noLinks = character(0),
          FoldEnrichment = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      allFE <- do.call(rbind, feParts)

      # Store per-run FE for dynamic recomputation at render time.
      # Bubble/heatmap sessions use getPerRunFE() to average over
      # only the active (filtered) runs instead of all runs.
      perRunFE <- list()
      for (i in seq_len(nrow(allFE))) {
        tid <- allFE$Term_ID_noLinks[i]
        sid <- allFE$SessionId[i]
        if (is.null(perRunFE[[tid]])) perRunFE[[tid]] <- list()
        perRunFE[[tid]][[sid]] <- allFE$FE[i]
      }
      private$.perRunFE <- perRunFE

      agg <- stats::aggregate(
        FE ~ Term_ID_noLinks,
        data = allFE,
        na.action = na.pass,
        FUN = function(x) {
          valid <- x[!is.na(x)]
          if (length(valid) == 0) NA_real_ else mean(valid)
        }
      )

      colnames(agg) <- c("Term_ID_noLinks", "FoldEnrichment")
      agg
    },

    #' Build per-run detail data for all terms
    #'
    #' For each term (keyed by Term_ID_noLinks) in each session, extracts
    #' individual p-value, positive hit gene names, and gene count. For terms
    #' NOT found in a session, stores found=FALSE with NA/empty defaults.
    #'
    #' @param resultsList Named list of result DataFrames (one per session)
    #' @param allSessionIds Character vector of all session IDs with results
    #' @return Nested list: termId -> sessionId -> {pvalue, geneCount, genes, found}
    #' Input genes of a run, and how many of them the tool recognised (the
    #' conversion table's inputs with a target; all inputs when there is no table).
    #' The gene ratio of a term is its genes / mapped.
    inputSizes = function(s, inputObj) {
      ids <- tryCatch(unique(as.character(inputObj$getIds())), error = function(e) character(0))
      if (!length(ids)) ids <- tryCatch(unique(as.character(inputObj$ids)), error = function(e) character(0))
      ct <- tryCatch(s$getConversionTable(), error = function(e) NULL)
      mapped <- if (is.data.frame(ct) && nrow(ct) && all(c("input", "target") %in% names(ct))) {
        ok <- !is.na(ct$target) & nzchar(as.character(ct$target))
        m <- unique(as.character(ct$input[ok]))
        length(if (length(ids)) intersect(m, ids) else m)
      } else length(ids)
      list(input = length(ids), mapped = if (mapped > 0) mapped else length(ids))
    },

    buildPerRunData = function(resultsList, allSessionIds) {
      debugComparison <- isTRUE(getOption("flame.debug.comparison", FALSE))

      # Collect all unique term IDs across all sessions
      allTermIds <- unique(unlist(lapply(resultsList, function(df) {
        if (is.null(df) || nrow(df) == 0) return(character(0))
        if (!("Term_ID_noLinks" %in% colnames(df))) return(character(0))
        as.character(df$Term_ID_noLinks)
      })))

      if (debugComparison) {
        cat("[COMPARISON] buildPerRunData: ", length(allTermIds), " unique terms across ",
            length(allSessionIds), " sessions\n", sep = "")
      }

      # Build per-session lookup: sessionId -> { termId -> row data }
      sessionLookups <- lapply(allSessionIds, function(sid) {
        df <- resultsList[[sid]]
        if (is.null(df) || nrow(df) == 0) return(list())
        if (!("Term_ID_noLinks" %in% colnames(df))) return(list())

        lookup <- list()
        for (i in seq_len(nrow(df))) {
          termId <- as.character(df$Term_ID_noLinks[i])

          pval <- if ("P-value" %in% colnames(df)) {
            suppressWarnings(as.numeric(df$`P-value`[i]))
          } else {
            NA_real_
          }

          hitsRaw <- if ("Positive Hits" %in% colnames(df)) {
            as.character(df$`Positive Hits`[i])
          } else {
            ""
          }
          genes <- if (is.na(hitsRaw) || !nzchar(trimws(hitsRaw))) {
            character(0)
          } else {
            trimws(strsplit(trimws(hitsRaw), ",\\s*")[[1]])
          }
          genes <- genes[nzchar(genes)]

          # Keep first occurrence to match dplyr::distinct(.keep_all = TRUE)
          if (is.null(lookup[[termId]])) {
            lookup[[termId]] <- list(
              pvalue = pval,
              geneCount = length(genes),
              genes = genes,
              found = TRUE
            )
          }
        }
        lookup
      })
      names(sessionLookups) <- allSessionIds

      # Build the nested result: termId -> sessionId -> detail
      perRunData <- list()
      for (termId in allTermIds) {
        termEntry <- list()
        for (sid in allSessionIds) {
          sessionData <- sessionLookups[[sid]][[termId]]
          if (!is.null(sessionData)) {
            termEntry[[sid]] <- sessionData
          } else {
            termEntry[[sid]] <- list(
              pvalue = NA_real_,
              geneCount = 0L,
              genes = character(0),
              found = FALSE
            )
          }
        }
        perRunData[[termId]] <- termEntry
      }

      if (debugComparison) {
        foundCounts <- vapply(allTermIds, function(tid) {
          sum(vapply(allSessionIds, function(sid) {
            isTRUE(perRunData[[tid]][[sid]]$found)
          }, logical(1)))
        }, integer(1))
        cat("[COMPARISON] buildPerRunData complete: ",
            sum(foundCounts == length(allSessionIds)), " terms in all sessions, ",
            sum(foundCounts < length(allSessionIds)), " partial\n", sep = "")
      }

      perRunData
    },

    #' Build per-term run membership from perRunData
    #'
    #' For each term, collects the session IDs where found=TRUE.
    #'
    #' @param perRunData Nested list from buildPerRunData()
    #' @param allSessionIds Character vector of all session IDs
    #' @return Named list: termId -> character vector of session IDs
    buildRunMembership = function(perRunData, allSessionIds) {
      debugComparison <- isTRUE(getOption("flame.debug.comparison", FALSE))

      membership <- lapply(names(perRunData), function(termId) {
        termEntry <- perRunData[[termId]]
        foundIds <- vapply(allSessionIds, function(sid) {
          isTRUE(termEntry[[sid]]$found)
        }, logical(1))
        allSessionIds[foundIds]
      })
      names(membership) <- names(perRunData)

      if (debugComparison) {
        memberCounts <- vapply(membership, length, integer(1))
        if (length(memberCounts) > 0) {
          cat("[COMPARISON] buildRunMembership: ", length(membership), " terms, ",
              "membership range ", min(memberCounts), "-", max(memberCounts),
              " sessions\n", sep = "")
        } else {
          cat("[COMPARISON] buildRunMembership: 0 terms\n")
        }
      }

      membership
    },

    #' Calculate Fisher's combined p-values
    #'
    #' @param combinedData Combined DataFrame with P-value column
    #' @return DataFrame with Chisq and P_value_combined columns added
    calculateFisherStats = function(combinedData) {
      combinedData$`P-value` <- as.numeric(combinedData$`P-value`)

      combinedResults <- do.call(
        rbind,
        tapply(
          combinedData$`P-value`,
          combinedData$Term_ID_noLinks,
          FUN = private$combinePvalues
        )
      )

      combinedResults <- as.data.frame(combinedResults)
      colnames(combinedResults) <- c("P_value_combined", "Chisq")
      combinedResults$Term_ID_noLinks <- row.names(combinedResults)

      # Merge back
      combinedData <- merge(combinedData, combinedResults,
                            by = "Term_ID_noLinks")
      combinedData$P_value_combined <- as.numeric(
        combinedData$P_value_combined
      )
      combinedData$Chisq <- as.numeric(combinedData$Chisq)

      return(combinedData)
    },

    #' Combine p-values using Fisher's method
    #'
    #' @param pvalues Numeric vector of p-values
    #' @return Named vector with p and statistic
    combinePvalues = function(pvalues) {
      if (length(pvalues) < 2) {
        return(c(pvalues[1], -2 * log(pvalues[1])))
      }
      result <- poolr::fisher(pvalues)
      return(c(result$p, result$statistic))
    }
  )
)
