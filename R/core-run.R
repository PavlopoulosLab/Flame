# =============================================================================
# FLAME core: enrichment runs as plain data (no Shiny)
# =============================================================================
# Used by the REST API (api/routes/runs.R) and its job workers.
#
#   params <- build_run_params(tool, list, payload, backgroundList)
#   out    <- execute_ora_run(tool, list, params, backgroundList)
#
# `list` / `backgroundList` are plain lists:
#   list(name, ids, organismTaxId, analyteType, idFormat)
# Execution delegates to ORAEnrichmentSession$execute() so results are
# identical to the Shiny app (golden-tested in tests/testthat/test-core-run.R).
# =============================================================================

# Legacy datasource keys sent by older panels -> canonical FLAME codes
.DATASOURCE_ALIASES <- c(
  go_bp = "GO:BP", go_mf = "GO:MF", go_cc = "GO:CC", kegg = "KEGG",
  reactome = "REAC", wikipathways = "WP", hpo = "HP", mirna = "MIRNA"
)

#' Normalize requested datasources (aliases, de-duplication, blanks)
normalize_datasources <- function(datasources) {
  if (is.null(datasources) || length(datasources) == 0) return(character(0))
  values <- vapply(as.character(unlist(datasources)), function(raw) {
    if (is.na(raw) || !nzchar(raw)) return(NA_character_)
    lower <- tolower(raw)
    if (lower %in% names(.DATASOURCE_ALIASES)) .DATASOURCE_ALIASES[[lower]] else raw
  }, character(1), USE.NAMES = FALSE)
  unique(values[!is.na(values) & nzchar(values)])
}

#' Resolve a tool's significance metric from a UI value (aliases, case, fallbacks)
normalize_tool_metric <- function(toolId, metricCandidate = NULL, hasBackground = FALSE) {
  toolConfig <- TOOLS[[toolId]]
  supported <- unique(as.character(unname(toolConfig$metrics)))
  supported <- supported[!is.na(supported) & nzchar(supported)]
  defaultMetric <- getDefaultMetric(toolId, hasBackground = isTRUE(hasBackground))
  defaultMetric <- if (!is.null(defaultMetric)) as.character(defaultMetric) else NULL
  if (is.null(defaultMetric) || !nzchar(defaultMetric) || !(defaultMetric %in% supported)) {
    defaultMetric <- if (length(supported) > 0) supported[[1]] else "fdr"
  }

  candidate <- defaultMetric
  if (!is.null(metricCandidate) && length(metricCandidate) > 0) {
    raw <- trimws(as.character(metricCandidate[[1]]))
    if (nzchar(raw)) candidate <- raw
  }

  aliasMap <- c(gscs = "gSCS", fdr = "fdr", bh = "BH", by = "BY", holm = "holm",
                hochberg = "hochberg", hommel = "hommel", bonferroni = "bonferroni",
                none = "NONE", adjusted_pvalue = "adjusted_pvalue")
  lower <- tolower(candidate)
  if (lower %in% names(aliasMap)) candidate <- aliasMap[[lower]]

  if (!(candidate %in% supported)) {
    ci <- supported[tolower(supported) == tolower(candidate)]
    if (length(ci) > 0) candidate <- ci[[1]]
  }
  if (!(candidate %in% supported) && tolower(candidate) == "fdr") {
    if (any(tolower(supported) == "bh")) {
      candidate <- supported[tolower(supported) == "bh"][[1]]
    } else if (any(tolower(supported) == "adjusted_pvalue")) {
      candidate <- supported[tolower(supported) == "adjusted_pvalue"][[1]]
    }
  }
  if (!(candidate %in% supported)) candidate <- defaultMetric
  candidate
}

#' Intersect requested datasources with what the tool supports for an organism
resolve_tool_datasources <- function(toolId, taxid, requestedSources) {
  shortName <- NULL
  orgRow <- ORGANISMS[ORGANISMS$taxid == taxid, ]
  if (nrow(orgRow) > 0) shortName <- as.character(orgRow$short_name[[1]])

  supported <- unique(as.character(getDatasourcesForTool(toolId, shortName) %||% character(0)))
  supported <- supported[!is.na(supported) & nzchar(supported)]
  requested <- normalize_datasources(requestedSources)

  if (length(supported) == 0) return(requested)
  if (length(requested) == 0) requested <- DATASOURCES_DEFAULT_SELECTED

  resolved <- intersect(requested, supported)
  if (length(resolved) > 0) return(resolved)
  fallback <- intersect(DATASOURCES_DEFAULT_SELECTED, supported)
  if (length(fallback) > 0) return(fallback)
  supported
}

#' Does a tool support enrichment for this organism?
#' Uses TOOL_ORGANISMS (taxids per tool); unknown tools/organism tables => TRUE.
tool_supports_organism <- function(toolId, taxid) {
  if (is.null(TOOL_ORGANISMS) || is.null(TOOL_ORGANISMS[[toolId]])) return(TRUE)
  taxids <- suppressWarnings(as.integer(unlist(TOOL_ORGANISMS[[toolId]])))
  if (length(taxids) == 0 || all(is.na(taxids))) return(TRUE)
  as.integer(taxid) %in% taxids
}

#' Organism taxid for a list (falls back to human)
list_taxid <- function(lst) {
  taxid <- suppressWarnings(as.integer(lst$organismTaxId))
  if (length(taxid) == 0 || is.na(taxid[1])) 9606L else taxid[1]
}

#' Build the parameter list for one tool run (pure)
#'
#' @param toolId ToolId value (e.g. ToolId$GPROFILER)
#' @param lst Input list (plain list with name, ids, organismTaxId, analyteType)
#' @param datasources Requested FLAME datasource codes
#' @param threshold Significance threshold (default 0.05)
#' @param correction Metric/correction requested in the UI (optional)
#' @param backgroundList Optional plain list used as custom background
build_run_params <- function(toolId, lst, datasources = NULL, threshold = 0.05,
                             correction = NULL, backgroundList = NULL) {
  toolConfig <- TOOLS[[toolId]]
  if (is.null(toolConfig)) stop("Unknown tool: ", toolId)
  taxid <- list_taxid(lst)

  useBackground <- !is.null(backgroundList) &&
    isTRUE(toolConfig$supportsBackground) &&
    identical(normalizeAnalyteType(backgroundList$analyteType),
              normalizeAnalyteType(lst$analyteType))

  thr <- suppressWarnings(as.numeric(threshold))
  if (length(thr) == 0 || is.na(thr) || thr <= 0 || thr > 1) thr <- 0.05

  list(
    geneListName = lst$name,
    datasources = resolve_tool_datasources(toolId, taxid, datasources),
    threshold = thr,
    namespace = "Default namespace",
    metric = normalize_tool_metric(toolId, correction, hasBackground = useBackground),
    backgroundMode = if (useBackground) "custom" else "genome",
    backgroundList = if (useBackground) backgroundList$name else NULL
  )
}

#' Do two runs have identical parameters (for de-duplication)?
run_params_match <- function(a, b, organismA, organismB) {
  if (is.null(a) || is.null(b)) return(FALSE)
  identical(a$geneListName, b$geneListName) &&
    identical(as.integer(organismA), as.integer(organismB)) &&
    isTRUE(all.equal(as.numeric(a$threshold), as.numeric(b$threshold))) &&
    identical(sort(as.character(unlist(a$datasources))), sort(as.character(unlist(b$datasources)))) &&
    identical(a$namespace, b$namespace) &&
    identical(a$backgroundMode, b$backgroundMode) &&
    identical(a$backgroundList %||% NULL, b$backgroundList %||% NULL) &&
    identical(a$metric, b$metric)
}

.as_analyte_list <- function(lst) {
  UnrankedAnalyteList$new(
    name = lst$name,
    analyteType = normalizeAnalyteType(lst$analyteType %||% AnalyteType$GENE),
    ids = as.character(unlist(lst$ids)),
    organismTaxId = list_taxid(lst),
    idFormat = lst$idFormat %||% "USERINPUT"
  )
}

#' Allowed URL schemes for term links (defence against javascript: URLs)
safe_link <- function(href) {
  if (is.null(href) || is.na(href) || !nzchar(href)) return(NA_character_)
  if (grepl("^https?://", href, ignore.case = TRUE)) href else NA_character_
}

#' Turn the session's results (Term_ID holds an HTML anchor) into clean data:
#' Term_ID is the plain identifier and `link` is a validated http(s) URL.
clean_result_links <- function(results) {
  if (is.null(results) || nrow(results) == 0) return(results)
  raw <- as.character(results$Term_ID)
  results$link <- vapply(raw, function(x) safe_link(extractHref(x) %||% NA_character_),
                         character(1), USE.NAMES = FALSE)
  plain <- as.character(results$Term_ID_noLinks %||% raw)
  # Strip any remaining markup (e.g. WebGestalt anchors) from IDs and names.
  results$Term_ID <- gsub("<[^>]*>", "", plain)
  results$Term_ID_noLinks <- results$Term_ID
  results$Function <- gsub("<[^>]*>", "", as.character(results$Function))
  rownames(results) <- NULL
  results
}

#' Execute one ORA run and return plain data (safe to call in a worker process)
#'
#' @return list(status, results, conversionTable, backgroundConversionTable,
#'   backgroundSize, noHitAnalytes, details, messages)
execute_ora_run <- function(toolId, lst, params, backgroundList = NULL) {
  messages <- character(0)
  session <- ORAEnrichmentSession$new(
    id = paste0("run_", toolId), runId = paste0("run_", toolId),
    uniqueId = 1L, displayNumber = 1L,
    toolName = toolId, organism = list_taxid(lst),
    input = .as_analyte_list(lst), paradigm = ParadigmId$ORA,
    background = if (identical(params$backgroundMode, "custom") && !is.null(backgroundList))
      .as_analyte_list(backgroundList) else NULL,
    parameters = params
  )

  withCallingHandlers(
    session$execute(),
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  results <- if (session$hasResults()) clean_result_links(session$getResults()) else NULL
  details <- tryCatch(session$getRunDetailsPayload(), error = function(e) NULL)

  list(
    status = "complete",
    resultCount = if (is.null(results)) 0L else nrow(results),
    results = results,
    parameters = session$getParameters(),
    conversionTable = session$getConversionTable(),
    backgroundConversionTable = session$getBackgroundConversionTable(),
    backgroundSize = session$getBackgroundSize(),
    noHitAnalytes = tryCatch(session$getNoHitAnalytes(), error = function(e) character(0)),
    details = details,
    messages = unique(messages)
  )
}
