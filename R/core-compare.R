# =============================================================================
# FLAME core: comparison of runs as plain data (no Shiny)
# =============================================================================
#   cs   <- build_comparison(runs)           # runs = list of stored run records
#   view <- comparison_view(cs, "bubble", filters, top, sortBy, settings)
#
# Fisher combination: CombinationSession (enrich-session-combination.R).
# Per-tab maths lifted unchanged from the former Comparison*Session classes.
# A stored run record is a list with:
#   id, tool, displayNumber, organism (taxid), paradigm, listName, analyteType,
#   inputIds, params, results, conversionTable, backgroundSize
# =============================================================================

COMPARISON_VIEWS <- c("overview", "bubble", "heatmap")

.rebuild_session <- function(run) {
  input <- UnrankedAnalyteList$new(
    name = run$listName %||% "list",
    analyteType = normalizeAnalyteType(run$analyteType %||% AnalyteType$GENE),
    ids = as.character(unlist(run$inputIds %||% run$conversionTable$input %||% "x")),
    organismTaxId = run$organism
  )
  s <- ORAEnrichmentSession$new(
    id = run$id, runId = run$id, uniqueId = 1L,
    displayNumber = as.integer(run$displayNumber %||% 1L),
    toolName = run$tool, organism = as.integer(run$organism),
    input = input, paradigm = run$paradigm %||% ParadigmId$ORA,
    parameters = run$params %||% list()
  )
  s$setResultsFromWorker(
    results = run$results %||% data.frame(),
    backgroundSize = run$backgroundSize,
    conversionTable = run$conversionTable
  )
  s
}

#' Combine ≥ 2 completed runs (Fisher's method via CombinationSession)
build_comparison <- function(runs) {
  if (length(runs) < 2) stop("Select at least two runs to compare.")
  registry <- EnrichmentSessionRegistry$new()
  for (run in runs) registry$add(.rebuild_session(run))
  cs <- CombinationSession$new(registry)
  cs$compare(vapply(runs, function(r) as.character(r$id), character(1)))
  if (!cs$isAvailable()) stop("At least two of the selected runs need results to compare.")
  cs
}

# The filter chain treats "no runs selected" as "no terms pass"; a request that
# omits `runs` means "all runs".
.with_default_runs <- function(cs, filters) {
  # Use [[ ]]: `filters$runs` would partially match `runsMode`.
  filters <- filters %||% list()
  if (is.null(filters[["runs"]])) filters[["runs"]] <- names(cs$getRunMetadata())
  filters[["runs"]] <- as.character(unlist(filters[["runs"]]))
  filters
}

# ---- Per-term metrics over the active runs ----------------------------------

cmp_fold_enrichment <- function(cs, data, activeRunIds) {
  perRunFE <- cs$getPerRunFE()
  if (is.null(perRunFE) || length(perRunFE) == 0) {
    fe <- suppressWarnings(as.numeric(as.character(data$FoldEnrichment)))
    fe[is.na(fe) | !is.finite(fe)] <- 0
    return(fe)
  }
  vapply(as.character(data$Term_ID_noLinks), function(tid) {
    termFE <- perRunFE[[tid]]
    if (is.null(termFE)) return(0)
    keys <- intersect(names(termFE), activeRunIds)
    if (length(keys) == 0) return(0)
    vals <- unlist(termFE[keys], use.names = FALSE)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) 0 else mean(vals)
  }, numeric(1), USE.NAMES = FALSE)
}

cmp_agreement <- function(cs, data, activeRunIds) {
  membership <- cs$getRunMembership()
  if (is.null(membership)) return(rep(1L, nrow(data)))
  vapply(as.character(data$Term_ID_noLinks), function(tid) {
    runs <- membership[[tid]]
    if (is.null(runs)) 0L else as.integer(length(intersect(runs, activeRunIds)))
  }, integer(1), USE.NAMES = FALSE)
}

#' Fisher's combined p-value over the active runs only
cmp_comb_pvalue <- function(cs, data, activeRunIds) {
  perRunData <- cs$getPerRunData()
  if (is.null(perRunData) || length(perRunData) == 0) {
    return(suppressWarnings(as.numeric(as.character(data$CombPvalue))))
  }
  vapply(as.character(data$Term_ID_noLinks), function(tid) {
    termData <- perRunData[[tid]]
    if (is.null(termData)) return(NA_real_)
    pvals <- vapply(activeRunIds, function(sid) {
      e <- termData[[sid]]
      if (is.null(e) || !isTRUE(e$found)) NA_real_ else e$pvalue
    }, numeric(1))
    pvals <- pvals[!is.na(pvals) & pvals > 0]
    if (length(pvals) == 0) return(NA_real_)
    if (length(pvals) == 1) return(pvals[1])
    stats::pchisq(-2 * sum(log(pvals)), df = 2 * length(pvals), lower.tail = FALSE)
  }, numeric(1), USE.NAMES = FALSE)
}

cmp_order <- function(cs, data, activeRunIds, sortBy) {
  p <- cmp_comb_pvalue(cs, data, activeRunIds); p[is.na(p)] <- Inf
  switch(as.character(sortBy %||% "-log10Pvalue"),
    "Fold Enrichment" = order(-cmp_fold_enrichment(cs, data, activeRunIds), p, method = "radix"),
    "Agreement" = order(-cmp_agreement(cs, data, activeRunIds), p, method = "radix"),
    order(p, method = "radix"))
}

.top_n <- function(top, n) {
  t <- suppressWarnings(as.integer(unlist(top)[1]))
  if (is.na(t) || t < 1L) t <- 20L
  min(t, n)
}

# ---- Overview (D3 fan chart) --------------------------------------------------

.membership_matrix <- function(cs, data, activeRunIds) {
  membership <- cs$getRunMembership()
  termIds <- as.character(data$Term_ID_noLinks)
  m <- matrix(FALSE, nrow = length(termIds), ncol = length(activeRunIds))
  for (ri in seq_along(activeRunIds)) {
    m[, ri] <- vapply(termIds, function(t) activeRunIds[ri] %in% membership[[t]], logical(1))
  }
  m
}

cmp_overview_chart <- function(cs, data, activeRunIds) {
  M <- length(activeRunIds)
  meta <- cs$getRunMetadata()[activeRunIds]
  mat <- .membership_matrix(cs, data, activeRunIds)
  agreement <- rowSums(mat)
  pins <- list(uniqueTerms = nrow(data), consensus = sum(agreement == M), singleRun = sum(agreement == 1L))

  # Opacity steps within runs of the same tool + paradigm (first run opaque).
  groups <- vapply(meta, function(m) paste0(m$toolName, "|", m$paradigm %||% "ORA"), character(1))
  rank <- ave(seq_along(groups), groups, FUN = seq_along)
  size <- ave(seq_along(groups), groups, FUN = length)

  runs <- lapply(seq_along(activeRunIds), function(ri) {
    m <- meta[[ri]]; col <- mat[, ri]
    list(runId = activeRunIds[ri], tool = m$toolName, paradigm = m$paradigm %||% "ORA",
         configGroup = rank[ri], inputList = m$listName %||% "Unknown",
         runLabel = paste0(m$toolName, " (", m$displayNumber %||% 1, ")"),
         opacity = if (size[ri] <= 1) 1 else 1 - (rank[ri] - 1) * 0.7 / max(size[ri] - 1, 1),
         termsFound = sum(col), exclusiveTerms = sum(col & agreement == 1L),
         consensusBlocker = if (M <= 1) 0L else sum(!col & agreement == (M - 1L)))
  })
  list(runs = runs, pins = pins,
       metricLabels = list(termsFound = "Terms found", exclusiveTerms = "Exclusive terms",
                           consensusBlocker = "Consensus blocker"),
       metricDescriptions = list(
         termsFound = "Total significant terms for this run",
         exclusiveTerms = "Terms found only by this run",
         consensusBlocker = "Terms found by all other runs but not this run",
         uniqueTerms = "Distinct terms across all runs combined",
         consensus = "Terms found by all runs",
         singleRun = "Terms found by exactly one run"),
       pinLabels = list(uniqueTerms = "Unique terms", consensus = "Consensus", singleRun = "Single-run"))
}

# ---- Public API -------------------------------------------------------------

.comparison_terms <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(list())
  lapply(seq_len(nrow(data)), function(i) list(
    id = as.character(data$Term_ID_noLinks[i]),
    source = as.character(data$Source[i]),
    name = as.character(data$Function[i]),
    tools = as.character(data$Tools[i]),
    rank = suppressWarnings(as.integer(data$Rank[i])),
    combPvalue = suppressWarnings(as.numeric(data$CombPvalue[i])),
    chisq = suppressWarnings(as.numeric(data$Chisq[i])),
    termSize = suppressWarnings(as.numeric(as.character(data$`Term Size`[i]))),
    intersectionHits = as.character(data$Intersection_Hits[i]),
    unionHits = as.character(data$Union_Hits[i]),
    intersectionCount = suppressWarnings(as.integer(data$Intersection_Count[i])),
    unionCount = suppressWarnings(as.integer(data$Union_Count[i]))
  ))
}

#' Run metadata for the client (labels, tools, organisms)
comparison_runs_meta <- function(cs) {
  meta <- cs$getRunMetadata()
  lapply(names(meta), function(id) {
    m <- meta[[id]]
    list(id = id, tool = m$toolName, displayNumber = m$displayNumber,
         label = paste0(m$toolName, " (", m$displayNumber %||% 1, ")"),
         paradigm = m$paradigm, organism = m$organism, listName = m$listName,
         datasources = as.list(m$datasources), threshold = m$threshold, metric = m$metric)
  })
}

#' Build data for one comparison tab
comparison_view <- function(cs, view, filters = list(), top = 20L,
                            sortBy = "-log10Pvalue", settings = list()) {
  if (!view %in% COMPARISON_VIEWS) stop("Unknown comparison view: ", view)
  filters <- .with_default_runs(cs, filters)
  active <- filters[["runs"]]
  all <- cs$getResults()
  filtered <- cs$applyComparisonFilters(all, filters)$data
  meta <- list(view = view, totalCount = nrow(all),
               filteredCount = if (is.null(filtered)) 0L else nrow(filtered),
               availableSources = sort(unique(as.character(all$Source))),
               runs = comparison_runs_meta(cs), activeRunIds = as.list(active))
  if (is.null(filtered) || nrow(filtered) == 0) return(c(meta, list(terms = list())))

  if (view == "overview") {
    return(c(meta, list(chart = cmp_overview_chart(cs, filtered, active),
                        terms = .comparison_terms(filtered))))
  }

  shown <- filtered[cmp_order(cs, filtered, active, sortBy), , drop = FALSE]
  shown <- shown[seq_len(.top_n(top, nrow(shown))), , drop = FALSE]
  terms <- .comparison_terms(shown)
  fe <- cmp_fold_enrichment(cs, shown, active)
  ag <- cmp_agreement(cs, shown, active)
  cp <- cmp_comb_pvalue(cs, shown, active)
  cp[is.na(cp) | cp <= 0] <- .Machine$double.xmin
  for (i in seq_along(terms)) {
    terms[[i]]$foldEnrichment <- fe[i]
    terms[[i]]$agreement <- ag[i]
    terms[[i]]$activeCombPvalue <- cp[i]
    terms[[i]]$negLog10p <- -log10(cp[i])
  }
  if (view == "bubble") return(c(meta, list(shownCount = length(terms), terms = terms)))

  # Heatmap: −log10(p) per (term, run); NULL where the run did not find the term.
  perRun <- cs$getPerRunData()
  cells <- list()
  for (t in terms) for (rid in active) {
    e <- perRun[[t$id]][[rid]]
    v <- if (!is.null(e) && isTRUE(e$found) && is.finite(e$pvalue) && e$pvalue > 0) -log10(e$pvalue) else NULL
    cells[[length(cells) + 1]] <- list(term = t$id, run = rid, value = v)
  }
  c(meta, list(shownCount = length(terms), terms = terms, cells = cells))
}

#' Term IDs behind an overview bubble (per run) or pin (across runs) click
comparison_overview_terms <- function(cs, filters, kind = c("bubble", "pin"), metric, runId = NULL) {
  kind <- match.arg(kind)
  filters <- .with_default_runs(cs, filters)
  active <- filters[["runs"]]
  filtered <- cs$applyComparisonFilters(cs$getResults(), filters)$data
  if (is.null(filtered) || nrow(filtered) == 0) return(character(0))
  membership <- cs$getRunMembership()[as.character(filtered$Term_ID_noLinks)]
  inActive <- lapply(membership, function(runs) intersect(runs, active))
  keep <- switch(paste(kind, metric),
    "bubble termsFound" = vapply(membership, function(r) runId %in% r, logical(1)),
    "bubble exclusiveTerms" = vapply(inActive, function(r) length(r) == 1 && r[1] == runId, logical(1)),
    "bubble consensusBlocker" = {
      others <- setdiff(active, runId)
      if (length(others) == 0) rep(FALSE, length(inActive))
      else vapply(inActive, function(r) !(runId %in% r) && all(others %in% r), logical(1))
    },
    "pin uniqueTerms" = rep(TRUE, length(inActive)),
    "pin consensus" = vapply(inActive, function(r) length(r) == length(active), logical(1)),
    "pin singleRun" = vapply(inActive, function(r) length(r) == 1, logical(1)),
    rep(FALSE, length(inActive)))
  names(membership)[keep]
}
