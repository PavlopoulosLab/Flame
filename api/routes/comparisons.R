# /api/comparisons — compare ≥ 2 finished runs (Fisher's combined p-value)

.cmp_cache <- new.env()

.comparison_for <- function(ws, runIds) {
  runIds <- sort(unique(as.character(unlist(runIds))))
  if (length(runIds) < 2) api_abort(400, "Select at least two runs to compare.")
  if (length(runIds) > 10) api_abort(400, "Compare at most ten runs at a time.")
  stamps <- vapply(runIds, function(id) {
    r <- get_run_row(ws, id)
    if (!identical(r$status, "complete")) api_abort(409, sprintf("Run %s has not finished.", id))
    as.character(r$finished)
  }, character(1))
  key <- paste(ws, paste(runIds, stamps, collapse = "|"))
  cs <- .cmp_cache[[key]]
  if (is.null(cs)) {
    runs <- lapply(runIds, function(id) run_record(ws, id))
    cs <- tryCatch(suppressMessages(build_comparison(runs)),
                   error = function(e) api_abort(422, conditionMessage(e)))
    if (length(ls(.cmp_cache)) > 50) rm(list = ls(.cmp_cache), envir = .cmp_cache)
    .cmp_cache[[key]] <- cs
  }
  cs
}

#* Data for a comparison tab.
#* Body: { runIds:[...], view: overview|bubble|heatmap,
#*        filters:{ runs, runsMode: any|all, consensusMin/Max, sources, search, pvalueMax, termSizeMin/Max },
#*        top, sortBy: -log10Pvalue|Fold Enrichment|Agreement, settings:{labels} }
#* @post /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    cs <- .comparison_for(ws, b$runIds)
    view <- as.character(b$view %||% "overview")
    if (!view %in% COMPARISON_VIEWS) api_abort(400, "Unknown comparison view.")
    top <- b$top %||% 20L
    if (identical(top, "all")) top <- .Machine$integer.max
    comparison_view(cs, view, b$filters %||% list(), top, b$sortBy %||% "-log10Pvalue", b$settings %||% list())
  })
}

#* Term IDs behind an overview click (for selection).
#* Body: { runIds, filters, kind: bubble|pin, metric, runId? }
#* @post /overview-terms
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    cs <- .comparison_for(ws, b$runIds)
    kind <- if (identical(b$kind, "pin")) "pin" else "bubble"
    as.list(comparison_overview_terms(cs, b$filters %||% list(), kind, as.character(b$metric %||% ""),
                                      as.character(b$runId %||% "")))
  })
}

#* Open the comparison in Arena3D: one layer per run (its terms), a Genes layer,
#* and links between copies of the same term in different runs.
#* Body: { runIds, filters, top, sortBy }
#* @post /arena3d
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    cs <- .comparison_for(ws, b$runIds)
    top <- b$top %||% 30L
    if (identical(top, "all")) top <- 200L
    body <- tryCatch(arena3d_payload_for_comparison(cs, b$filters %||% list(), min(as.integer(top), 200L), b$sortBy %||% "-log10Pvalue"),
                     error = function(e) api_abort(422, conditionMessage(e)))
    tryCatch(arena3d_open(body), error = function(e) api_abort(502, conditionMessage(e)))
  })
}

#* Statistics across the compared runs: overlap, rank agreement, tests, concordance at the top, MDS.
#* Body: { runIds, filters }
#* @post /statistics
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    cs <- .comparison_for(ws, b$runIds)
    tryCatch(comparison_statistics(cs, b$filters %||% list()), error = function(e) api_abort(422, conditionMessage(e)))
  })
}

#* Functional themes across the compared runs, with the runs that support each theme.
#* Body: { runIds, filters?, cutoff? }
#* @post /themes
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    cs <- .comparison_for(ws, b$runIds)
    cutoff <- suppressWarnings(as.numeric(b$cutoff %||% 0.35)); if (!is.finite(cutoff)) cutoff <- 0.35
    tryCatch(comparison_themes(cs, b$filters %||% list(), max(0.05, min(0.95, cutoff))), error = function(e) api_abort(422, conditionMessage(e)))
  })
}
