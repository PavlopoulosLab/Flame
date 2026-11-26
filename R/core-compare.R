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

COMPARISON_VIEWS <- c("overview", "bubble", "heatmap", "top")
COMPARISON_SORTS <- c("Consensus", "Agreement", "Best rank", "-log10Pvalue", "Fold Enrichment")

#' Tools write the same KEGG pathway as "hsa05210", "map05210", "ko05210" or
#' "KEGG:05210". For matching terms across runs, KEGG rows use the
#' organism-neutral reference ID ("map05210"); other sources are unchanged.
canonical_kegg_ids <- function(results) {
  if (!is.data.frame(results) || nrow(results) == 0 || !"Source" %in% names(results)) return(results)
  kegg <- grepl("KEGG", as.character(results$Source), ignore.case = TRUE)
  if (!any(kegg)) return(results)
  canon <- function(x) sub("^(?:KEGG:|path:)?(?:[a-z]{2,4})?(\\d{5})$", "map\\1", trimws(as.character(x)), perl = TRUE)
  for (col in intersect(c("Term_ID_noLinks", "Term_ID"), names(results)))
    results[[col]][kegg] <- canon(results[[col]][kegg])
  results
}

.rebuild_session <- function(run) {
  run$results <- canonical_kegg_ids(run$results)
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

# ---- Consensus ranking (Robust Rank Aggregation) ----------------------------
# p-values are only comparable within one run: tools use different multiple-testing
# corrections, backgrounds and annotation versions. A term a run did not report is
# "above that run's threshold", not p = 1. So runs are combined by rank, not by p:
#   normalised rank r = (rank of the term in the run's list, by p; ties share the best rank) / N,
#   N = distinct terms under consideration (those passing the filters); not reported: r = 1.
# This is the partial-list convention of the RobustRankAggreg package. Dividing by each
# run's own list length instead would make a strict tool's few terms look weak.
# web/src/lib/rankAgg.ts computes the same score for the Table and Ranks views: keep both in step.
# RRA (Kolde et al. 2012, Bioinformatics 28:573) asks how unlikely the term's sorted
# ranks are if every run ranked at random: rho = min_k Beta(k, M-k+1) CDF at r_(k),
# Bonferroni-corrected over the M runs. It rewards terms ranked high by many runs and
# does not assume the runs are independent tests of the same thing (Fisher does).

#' Normalised ranks (terms x active runs) among the terms of `universe` (default: `ids`).
cmp_rank_matrix <- function(cs, ids, activeRunIds, universe = ids) {
  perRun <- cs$getPerRunData()
  all <- unique(as.character(universe))
  P <- vapply(activeRunIds, function(r) vapply(all, function(t) {
    e <- perRun[[t]][[r]]
    if (!is.null(e) && isTRUE(e$found) && is.finite(e$pvalue)) e$pvalue else NA_real_
  }, numeric(1)), numeric(length(all)))
  P <- matrix(P, nrow = length(all), dimnames = list(all, activeRunIds))
  N <- max(1L, sum(rowSums(!is.na(P)) > 0))
  R <- apply(P, 2, function(p) { r <- rep(NA_real_, length(p)); ok <- !is.na(p); r[ok] <- rank(p[ok], ties.method = "min"); r })
  R <- matrix(R, nrow = length(all), dimnames = list(all, activeRunIds))
  norm <- R / N
  norm[is.na(norm)] <- 1
  ids <- as.character(ids)
  list(rank = R[ids, , drop = FALSE], norm = norm[ids, , drop = FALSE], N = N)
}

#' RRA score of one term from its normalised ranks (one per run).
rra_score <- function(r) {
  r <- sort(pmin(pmax(as.numeric(r), 0), 1)); M <- length(r)
  if (!M) return(NA_real_)
  min(1, M * min(stats::pbeta(r, seq_len(M), M - seq_len(M) + 1L)))
}

#' Per-term consensus metrics over the active runs.
cmp_consensus <- function(cs, data, activeRunIds, universe = data$Term_ID_noLinks) {
  rm <- cmp_rank_matrix(cs, data$Term_ID_noLinks, activeRunIds, universe)
  list(score = apply(rm$norm, 1, rra_score), meanRank = rowMeans(rm$norm),
       bestRank = apply(rm$norm, 1, min), rank = rm$rank, norm = rm$norm, N = rm$N)
}

cmp_order <- function(cs, data, activeRunIds, sortBy, universe = data$Term_ID_noLinks) {
  key <- as.character(sortBy %||% "Consensus")
  p <- cmp_comb_pvalue(cs, data, activeRunIds); p[is.na(p)] <- Inf
  if (key %in% c("Consensus", "Agreement", "Best rank")) {
    cc <- cmp_consensus(cs, data, activeRunIds, universe)
    ag <- cmp_agreement(cs, data, activeRunIds)
    return(switch(key,
      "Consensus" = order(cc$score, cc$meanRank, method = "radix"),
      "Agreement" = order(-ag, cc$meanRank, method = "radix"),
      "Best rank" = order(cc$bestRank, -ag, cc$meanRank, method = "radix")))
  }
  switch(key,
    "Fold Enrichment" = order(-cmp_fold_enrichment(cs, data, activeRunIds), p, method = "radix"),
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

.comparison_terms <- function(data, membership = NULL) {
  if (is.null(data) || nrow(data) == 0) return(list())
  lapply(seq_len(nrow(data)), function(i) list(
    id = as.character(data$Term_ID_noLinks[i]),
    runIds = as.list(as.character(membership[[as.character(data$Term_ID_noLinks[i])]] %||% character(0))),
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
         datasources = as.list(m$datasources), threshold = m$threshold, metric = m$metric,
         inputSize = m$inputSize, mappedSize = m$mappedSize)
  })
}

#' Build data for one comparison tab
comparison_view <- function(cs, view, filters = list(), top = 20L,
                            sortBy = "Consensus", settings = list()) {
  if (!view %in% COMPARISON_VIEWS) stop("Unknown comparison view: ", view)
  filters <- .with_default_runs(cs, filters)
  active <- filters[["runs"]]
  all <- cs$getResults()
  filtered <- cs$applyComparisonFilters(all, filters)$data
  meta <- list(view = view, totalCount = nrow(all),
               filteredCount = if (is.null(filtered)) 0L else nrow(filtered),
               availableSources = as.list(sort(unique(as.character(all$Source)))),
               runs = comparison_runs_meta(cs), activeRunIds = as.list(active))
  if (is.null(filtered) || nrow(filtered) == 0) return(c(meta, list(terms = list())))

  if (view == "overview") {
    terms <- .comparison_terms(filtered, cs$getRunMembership())
    # Per-run detail for the comparison table: which run found the term, its p-value and gene count.
    perRun <- cs$getPerRunData()
    for (i in seq_along(terms)) {
      e <- perRun[[terms[[i]]$id]]
      terms[[i]]$byRun <- lapply(stats::setNames(active, active), function(r) {
        x <- e[[r]]
        if (is.null(x) || !isTRUE(x$found)) NULL else list(p = x$pvalue, genes = x$geneCount)
      })
    }
    return(c(meta, list(chart = cmp_overview_chart(cs, filtered, active), terms = terms)))
  }

  if (view == "top") return(c(meta, cmp_top_panels(cs, filtered, active, top, sortBy, settings)))

  shown <- filtered[cmp_order(cs, filtered, active, sortBy), , drop = FALSE]
  shown <- shown[seq_len(.top_n(top, nrow(shown))), , drop = FALSE]
  terms <- .comparison_terms(shown, cs$getRunMembership())
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

#' "Top terms" view, optionally one panel per annotation source (settings$splitBySource):
#' each panel ranks its own terms and keeps its own top `top`. `terms` lists all panels' terms.
cmp_top_panels <- function(cs, filtered, active, top, sortBy, settings = list()) {
  split <- isTRUE(as.logical(unlist(settings$splitBySource)[1]))
  if (!split) {
    p <- cmp_top_terms(cs, filtered, active, top, sortBy, settings)
    return(c(p, list(panels = list(c(list(source = NULL), p)))))
  }
  src <- as.character(filtered$Source)
  sources <- names(sort(table(src), decreasing = TRUE))
  panels <- lapply(sources, function(x) c(list(source = x), cmp_top_terms(cs, filtered[src == x, , drop = FALSE], active, top, sortBy, settings)))
  panels <- Filter(function(p) length(p$terms) > 0, panels)
  sum_of <- function(k) sum(vapply(panels, function(p) as.numeric(p[[k]] %||% 0), numeric(1)))
  list(minAgreement = if (length(panels)) panels[[1]]$minAgreement else NULL, sortBy = as.character(sortBy %||% "Consensus"),
       eligibleCount = sum_of("eligibleCount"), collapsed = sum_of("collapsed"), shownCount = sum_of("shownCount"),
       panels = panels, terms = unlist(lapply(panels, `[[`, "terms"), recursive = FALSE))
}

#' "Top terms" view: the best terms across runs for a dot plot and a bar plot.
#' settings: minAgreement (found by at least this many active runs; default: a majority),
#'           collapse (TRUE: merge terms of the same source sharing >= collapseCutoff of their genes).
cmp_top_terms <- function(cs, filtered, active, top, sortBy, settings = list()) {
  M <- length(active)
  minAg <- suppressWarnings(as.integer(unlist(settings$minAgreement)[1] %||% NA))
  if (is.na(minAg)) minAg <- ceiling(M / 2)
  minAg <- max(1L, min(minAg, M))
  ag <- cmp_agreement(cs, filtered, active)
  d <- filtered[ag >= minAg, , drop = FALSE]
  out <- list(minAgreement = minAg, sortBy = as.character(sortBy %||% "Consensus"), collapsed = 0L)
  if (nrow(d) == 0) return(c(out, list(shownCount = 0L, eligibleCount = 0L, terms = list())))
  universe <- filtered$Term_ID_noLinks           # ranks among all filtered terms, before the agreement cut
  d <- d[cmp_order(cs, d, active, sortBy, universe), , drop = FALSE]
  out$eligibleCount <- nrow(d)

  if (isTRUE(as.logical(unlist(settings$collapse)[1]))) {
    cut <- suppressWarnings(as.numeric(unlist(settings$collapseCutoff)[1] %||% NA))
    if (!is.finite(cut)) cut <- 0.5
    tmp <- data.frame(Source = d$Source, Term_ID_noLinks = d$Term_ID_noLinks, Function = d$Function,
                      `P-value` = seq_len(nrow(d)), `Positive Hits` = d$Union_Hits,
                      check.names = FALSE, stringsAsFactors = FALSE)
    red <- reduce_redundancy(tmp, cutoff = cut, method = "genes")
    keep <- match(red$Term_ID_noLinks, d$Term_ID_noLinks)
    out$collapsed <- nrow(d) - length(keep)
    d <- d[keep, , drop = FALSE]
    d$Represents <- red$Represents; d$Merged <- red$Merged
  }

  d <- d[seq_len(.top_n(top, nrow(d))), , drop = FALSE]
  cc <- cmp_consensus(cs, d, active, universe)
  ag <- cmp_agreement(cs, d, active)
  cp <- cmp_comb_pvalue(cs, d, active)
  perRun <- cs$getPerRunData()
  runMeta <- cs$getRunMetadata()
  terms <- .comparison_terms(d, cs$getRunMembership())
  for (i in seq_along(terms)) {
    tid <- terms[[i]]$id
    terms[[i]]$agreement <- ag[i]
    terms[[i]]$consensusScore <- max(cc$score[i], 1e-300)
    terms[[i]]$meanRank <- round(cc$meanRank[i], 4)
    terms[[i]]$bestRank <- round(cc$bestRank[i], 4)
    terms[[i]]$activeCombPvalue <- if (is.finite(cp[i])) cp[i] else NULL
    if (!is.null(d$Represents)) { terms[[i]]$represents <- d$Represents[i]; terms[[i]]$merged <- d$Merged[i] }
    terms[[i]]$cells <- lapply(stats::setNames(active, active), function(r) {
      e <- perRun[[tid]][[r]]
      if (is.null(e) || !isTRUE(e$found)) return(NULL)
      list(p = e$pvalue, negLog10p = if (is.finite(e$pvalue) && e$pvalue > 0) -log10(e$pvalue) else NULL,
           rank = cc$rank[i, r], normRank = round(cc$norm[i, r], 4), genes = e$geneCount,
           geneRatio = { m <- runMeta[[r]]$mappedSize %||% 0; if (m > 0) round(e$geneCount / m, 4) else NULL })
    })
  }
  c(out, list(shownCount = length(terms), rankedTerms = cc$N, terms = terms))
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

# ---- Statistics across runs (tools, lists or strategies) -------------------

.test_out <- function(t) {
  if (is.null(t) || inherits(t, "try-error")) return(NULL)
  p <- unname(t$p.value); s <- unname(t$statistic)
  if (!length(p) || !is.finite(p)) return(NULL)
  list(statistic = if (length(s)) signif(s, 4) else NULL, df = if (!is.null(t$parameter)) signif(unname(t$parameter)[1], 4) else NULL,
       p = p, method = t$method)
}
.safe_test <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NULL)

#' Cochran's Q test for k related binary samples (rows = terms, cols = runs)
cochran_q <- function(m) {
  m <- m[rowSums(m) > 0 & rowSums(m) < ncol(m), , drop = FALSE]
  k <- ncol(m)
  if (k < 3 || nrow(m) < 2) return(NULL)
  cj <- colSums(m); ri <- rowSums(m); n <- sum(ri)
  q <- (k - 1) * (k * sum(cj^2) - n^2) / (k * n - sum(ri^2))
  list(statistic = signif(q, 4), df = k - 1, p = stats::pchisq(q, k - 1, lower.tail = FALSE), method = "Cochran's Q test")
}

#' Statistics comparing the active runs on the filtered terms.
comparison_statistics <- function(cs, filters = list(), catMax = 100L) {
  filters <- .with_default_runs(cs, filters)
  active <- as.character(unlist(filters[["runs"]]))
  meta <- cs$getRunMetadata()[active]
  all <- cs$getResults()
  data <- cs$applyComparisonFilters(all, filters)$data
  if (is.null(data) || nrow(data) == 0) stop("No terms match the current filters.")
  perRun <- cs$getPerRunData()
  ids <- as.character(data$Term_ID_noLinks)
  P <- vapply(active, function(r) vapply(ids, function(t) {
    e <- perRun[[t]][[r]]
    if (!is.null(e) && isTRUE(e$found) && is.finite(e$pvalue)) max(e$pvalue, 1e-300) else NA_real_
  }, numeric(1)), numeric(length(ids)))
  P <- matrix(P, nrow = length(ids), dimnames = list(ids, active))
  found <- !is.na(P)
  L <- -log10(P)
  size <- suppressWarnings(as.numeric(as.character(data$`Term Size`)))
  label <- function(r) paste0(meta[[r]]$toolName, " (", meta[[r]]$displayNumber %||% 1, ")")

  runs <- lapply(active, function(r) {
    f <- found[, r]
    others <- if (length(active) > 1) rowSums(found[, setdiff(active, r), drop = FALSE]) else rep(0, length(f))
    list(id = r, label = label(r), tool = meta[[r]]$toolName, listName = meta[[r]]$listName, paradigm = meta[[r]]$paradigm %||% "ORA",
         terms = sum(f), unique = sum(f & others == 0),
         medianTermSize = if (any(f & is.finite(size))) stats::median(size[f], na.rm = TRUE) else NULL,
         medianLog10p = if (any(f)) round(stats::median(L[f, r]), 2) else NULL,
         bySource = as.list(table(as.character(data$Source[f]))))
  })

  pairs <- list(); cat <- list()
  if (length(active) >= 2) for (i in seq_along(active)) for (j in seq_along(active)) if (i < j) {
    a <- active[i]; b <- active[j]
    fa <- found[, a]; fb <- found[, b]; sh <- fa & fb
    nA <- sum(fa); nB <- sum(fb); nS <- sum(sh); nU <- sum(fa | fb)
    sp <- if (nS >= 3) .safe_test(stats::cor.test(L[sh, a], L[sh, b], method = "spearman", exact = FALSE)) else NULL
    # Rank agreement over all terms either run found; a term a run did not report is its
    # least significant (−log10 p = 0). Unlike `sp`, this does not condition on both finding it.
    un <- fa | fb
    spAll <- if (sum(un) >= 3) .safe_test(stats::cor.test(ifelse(fa[un], L[un, a], 0), ifelse(fb[un], L[un, b], 0), method = "spearman", exact = FALSE)) else NULL
    kd <- if (nS >= 3) .safe_test(stats::cor.test(L[sh, a], L[sh, b], method = "kendall", exact = FALSE)) else NULL
    wx <- if (nS >= 3) .safe_test(stats::wilcox.test(L[sh, a], L[sh, b], paired = TRUE, exact = FALSE)) else NULL
    mc <- if (nU >= 1 && (sum(fa & !fb) + sum(!fa & fb)) > 0) .safe_test(stats::mcnemar.test(table(factor(fa, c(TRUE, FALSE)), factor(fb, c(TRUE, FALSE))))) else NULL
    gj <- if (nS) {
      vals <- vapply(ids[sh], function(t) {
        ga <- perRun[[t]][[a]]$genes; gb <- perRun[[t]][[b]]$genes
        u <- length(union(ga, gb)); if (u) length(intersect(ga, gb)) / u else NA_real_
      }, numeric(1))
      if (any(is.finite(vals))) round(mean(vals, na.rm = TRUE), 3) else NULL
    } else NULL
    pairs[[length(pairs) + 1]] <- list(
      a = a, b = b, nA = nA, nB = nB, shared = nS, union = nU,
      jaccard = if (nU) round(nS / nU, 3) else 0, overlapCoef = if (min(nA, nB)) round(nS / min(nA, nB), 3) else 0,
      geneJaccard = gj,
      spearman = if (!is.null(sp)) round(unname(sp$estimate), 3) else NULL, spearmanP = sp$p.value,
      spearmanAll = if (!is.null(spAll) && is.finite(spAll$estimate)) round(unname(spAll$estimate), 3) else NULL, spearmanAllP = spAll$p.value,
      kendall = if (!is.null(kd)) round(unname(kd$estimate), 3) else NULL, kendallP = kd$p.value,
      medianDiff = if (nS) round(stats::median(L[sh, a] - L[sh, b]), 2) else NULL, wilcoxonP = wx$p.value,
      mcnemarP = mc$p.value)
    # Concordance at the top: share of the top-k terms of both runs that agree.
    ra <- ids[fa][order(P[fa, a])]; rb <- ids[fb][order(P[fb, b])]
    K <- min(length(ra), length(rb), catMax)
    if (K >= 1) {
      ks <- unique(round(seq(1, K, length.out = min(K, 50))))
      # as.list: always JSON arrays, also when a run has a single term.
      cat[[length(cat) + 1]] <- list(a = a, b = b, k = as.list(ks),
                                     overlap = as.list(vapply(ks, function(k) length(intersect(ra[seq_len(k)], rb[seq_len(k)])) / k, numeric(1))))
    }
  }
  # Benjamini–Hochberg across pairs for each test.
  for (key in c("spearmanP", "spearmanAllP", "kendallP", "wilcoxonP", "mcnemarP")) {
    p <- vapply(pairs, function(x) x[[key]] %||% NA_real_, numeric(1))
    adj <- stats::p.adjust(p, "BH")
    for (i in seq_along(pairs)) pairs[[i]][[paste0(key, "Adj")]] <- if (is.finite(adj[i])) adj[i] else NULL
  }

  tests <- list(
    detection = if (length(active) >= 3) cochran_q(found * 1) else if (length(pairs)) {
      if (!is.null(pairs[[1]]$mcnemarP)) list(p = pairs[[1]]$mcnemarP, method = "McNemar's test") else NULL
    } else NULL,
    termSize = {
      v <- unlist(lapply(active, function(r) size[found[, r]])); g <- unlist(lapply(active, function(r) rep(r, sum(found[, r]))))
      ok <- is.finite(v)
      if (length(unique(g[ok])) >= 2) .test_out(.safe_test(stats::kruskal.test(v[ok], factor(g[ok])))) else NULL
    },
    significance = {
      v <- unlist(lapply(active, function(r) L[found[, r], r])); g <- unlist(lapply(active, function(r) rep(r, sum(found[, r]))))
      if (length(unique(g)) >= 2) .test_out(.safe_test(stats::kruskal.test(v, factor(g)))) else NULL
    }
  )

  mds <- NULL
  if (length(active) >= 3) {
    D <- matrix(0, length(active), length(active), dimnames = list(active, active))
    for (p in pairs) { d <- 1 - p$jaccard; D[p$a, p$b] <- d; D[p$b, p$a] <- d }
    xy <- tryCatch(stats::cmdscale(stats::as.dist(D), k = 2), error = function(e) NULL)
    if (!is.null(xy) && ncol(xy) == 2) mds <- lapply(active, function(r) list(id = r, x = round(xy[r, 1], 4), y = round(xy[r, 2], 4)))
  }

  agreement <- lapply(seq_along(active), function(k) list(runs = k, terms = sum(rowSums(found) == k)))
  terms <- lapply(seq_along(ids), function(i) list(
    id = ids[i], name = as.character(data$Function[i]), source = as.character(data$Source[i]),
    termSize = if (is.finite(size[i])) size[i] else NULL,
    p = stats::setNames(lapply(active, function(r) if (found[i, r]) P[i, r] else NULL), active)))

  list(runs = runs, pairs = pairs, tests = tests, cat = cat, mds = mds, agreement = agreement, terms = terms,
       filteredCount = length(ids), activeRunIds = as.list(active),
       note = paste("Terms are matched by identifier across runs. Tests use only terms found by at least one compared run; pairwise p-values are Benjamini–Hochberg adjusted across pairs.",
                    "p-values are not on one scale across tools (different corrections, backgrounds and annotation versions): compare runs by which terms they find and how they rank them, not by the size of their p-values."))
}
