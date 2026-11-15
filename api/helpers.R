# =============================================================================
# API helpers: workspace store (SQLite), limits, errors, JSON.
# Sourced into the global environment by api/run.R before routers are built.
# All SQL is parameterised (never paste user input into SQL).
# =============================================================================

FLAME_LIMITS <- list(
  maxLists        = as.integer(Sys.getenv("FLAME_MAX_LISTS", "10")),
  maxIdsPerList   = 3000L,
  maxRuns         = as.integer(Sys.getenv("FLAME_MAX_RUNS", "60")),
  maxActiveJobs   = as.integer(Sys.getenv("FLAME_MAX_ACTIVE_JOBS", "6")),
  maxTextBytes    = 1024L * 1024L,          # 1 MB pasted/uploaded text
  maxNameChars    = 80L,
  retentionDays   = as.numeric(Sys.getenv("FLAME_RETENTION_DAYS", "30")),
  maxSources      = 5L,
  maxGmtTerms     = 20000L,
  maxSessionBytes = as.numeric(Sys.getenv("FLAME_MAX_SESSION_MB", "25")) * 1024^2
)

# ---- Errors -----------------------------------------------------------------

api_abort <- function(status, message) {
  cond <- structure(class = c("flame_api_error", "error", "condition"),
                    list(message = message, status = status, call = NULL))
  stop(cond)
}

#' Wrap a route body: converts flame_api_error to a JSON error with status;
#' other errors become 500 without leaking internals.
api_handle <- function(res, expr) {
  tryCatch(expr,
    flame_api_error = function(e) {
      res$status <- e$status
      list(error = e$message)
    },
    error = function(e) {
      message("[api] internal error: ", conditionMessage(e))
      res$status <- 500
      list(error = "Internal error. Please try again; if it persists, report an issue.")
    })
}

# ---- Small utilities --------------------------------------------------------

now_s <- function() as.numeric(Sys.time())
to_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
from_json <- function(x) if (is.null(x) || is.na(x) || !nzchar(x)) NULL else jsonlite::fromJSON(x, simplifyVector = TRUE)

new_workspace_id <- function() paste(as.character(sodium::random(16)), collapse = "")
valid_workspace_id <- function(x) is.character(x) && length(x) == 1 && grepl("^[0-9a-f]{32}$", x)
new_object_id <- function() paste(as.character(sodium::random(8)), collapse = "")

#' Clean a user-supplied display name (list/run/source names)
clean_name <- function(x, field = "name") {
  x <- as.character(x %||% "")[1]
  x <- trimws(gsub("[[:cntrl:]]", "", x))
  if (!nzchar(x)) api_abort(400, paste0("A ", field, " is required."))
  if (nchar(x) > FLAME_LIMITS$maxNameChars)
    api_abort(400, sprintf("The %s must be at most %d characters.", field, FLAME_LIMITS$maxNameChars))
  x
}

#' Clean analyte IDs: strip, de-duplicate (case-insensitive), validate charset
clean_ids <- function(ids) {
  ids <- trimws(as.character(unlist(ids)))
  ids <- ids[nzchar(ids) & !is.na(ids)]
  ids <- ids[grepl("^[A-Za-z0-9_.:\\-]+$", ids)]
  ids <- ids[!duplicated(tolower(ids))]
  if (length(ids) == 0) api_abort(400, "No valid IDs were found.")
  if (length(ids) > FLAME_LIMITS$maxIdsPerList)
    api_abort(400, sprintf("A list can contain at most %d IDs (got %d).", FLAME_LIMITS$maxIdsPerList, length(ids)))
  ids
}

check_text_size <- function(text) {
  if (!is.null(text) && nchar(text, type = "bytes") > FLAME_LIMITS$maxTextBytes)
    api_abort(413, "Input is larger than the 1 MB limit.")
  invisible(TRUE)
}

# ---- Database ---------------------------------------------------------------

.flame_db <- new.env()

db <- function() {
  con <- .flame_db$con
  if (is.null(con) || !DBI::dbIsValid(con)) {
    path <- Sys.getenv("FLAME_DB", file.path(FLAME_ROOT, "api", "data", "flame.sqlite"))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    schema <- readLines(file.path(FLAME_ROOT, "api", "schema.sql"), warn = FALSE)
    stmts <- strsplit(paste(schema[!grepl("^\\s*--", schema)], collapse = "\n"), ";")[[1]]
    for (s in stmts) if (nzchar(trimws(s))) DBI::dbExecute(con, s)
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
    # Migrations for databases created by earlier versions.
    if (!"scores_json" %in% DBI::dbListFields(con, "lists")) {
      DBI::dbExecute(con, "ALTER TABLE lists ADD COLUMN scores_json TEXT")
    }
    wsFields <- DBI::dbListFields(con, "workspaces")
    if (!"last_export" %in% wsFields) DBI::dbExecute(con, "ALTER TABLE workspaces ADD COLUMN last_export REAL")
    if (!"export_fingerprint" %in% wsFields) DBI::dbExecute(con, "ALTER TABLE workspaces ADD COLUMN export_fingerprint TEXT")
    .flame_db$con <- con
  }
  con
}

db_query <- function(sql, params = list()) DBI::dbGetQuery(db(), sql, params = params)
db_exec <- function(sql, params = list()) DBI::dbExecute(db(), sql, params = params)

# ---- Workspaces -------------------------------------------------------------

create_workspace <- function() {
  id <- new_workspace_id()
  db_exec("INSERT INTO workspaces (id, created, last_seen) VALUES (?, ?, ?)", list(id, now_s(), now_s()))
  id
}

#' Resolve the workspace from the X-Flame-Workspace header (404 if unknown/expired)
require_workspace <- function(req) {
  id <- req$HTTP_X_FLAME_WORKSPACE %||% ""
  if (!valid_workspace_id(id)) api_abort(401, "Missing or invalid workspace link.")
  row <- db_query("SELECT id FROM workspaces WHERE id = ?", list(id))
  if (nrow(row) == 0) api_abort(404, "This workspace does not exist or has expired.")
  db_exec("UPDATE workspaces SET last_seen = ? WHERE id = ?", list(now_s(), id))
  id
}

#' A short hash of what the workspace holds (lists, runs, sources, variant
#' analyses). Saved with each export, so the status can say whether anything
#' changed since the last save.
workspace_fingerprint <- function(ws) {
  parts <- c(
    with(db_query("SELECT id, name, ids_json, COALESCE(scores_json, '') AS sc FROM lists WHERE ws = ? ORDER BY id", list(ws)),
         paste("L", id, name, nchar(ids_json), nchar(sc))),
    with(db_query("SELECT id, status FROM runs WHERE ws = ? ORDER BY id", list(ws)), paste("R", id, status)),
    with(db_query("SELECT id, name FROM sources WHERE ws = ? ORDER BY id", list(ws)), paste("S", id, name)),
    with(db_query("SELECT id, name, status FROM variant_analyses WHERE ws = ? ORDER BY id", list(ws)), paste("V", id, name, status))
  )
  substr(sodium::bin2hex(sodium::sha256(charToRaw(enc2utf8(paste(parts, collapse = "\n"))))), 1, 16)
}

#' Session status for the workspace header and the Session dialog
workspace_status <- function(ws) {
  w <- db_query("SELECT created, last_seen, last_export, export_fingerprint FROM workspaces WHERE id = ?", list(ws))
  runs <- db_query("SELECT status, COUNT(*) AS n FROM runs WHERE ws = ? GROUP BY status", list(ws))
  va <- db_query("SELECT status, COUNT(*) AS n FROM variant_analyses WHERE ws = ? GROUP BY status", list(ws))
  cnt <- function(df, st) sum(df$n[df$status %in% st])
  bytes <- db_query("SELECT
      COALESCE((SELECT SUM(LENGTH(ids_json) + COALESCE(LENGTH(scores_json), 0)) FROM lists WHERE ws = ?1), 0) +
      COALESCE((SELECT SUM(COALESCE(LENGTH(result_blob), 0)) FROM runs WHERE ws = ?1), 0) +
      COALESCE((SELECT SUM(LENGTH(gmt_json)) FROM sources WHERE ws = ?1), 0) +
      COALESCE((SELECT SUM(COALESCE(LENGTH(result_blob), 0)) FROM variant_analyses WHERE ws = ?1), 0) AS b", list(ws))$b
  fp <- workspace_fingerprint(ws)
  lastExport <- if (is.na(w$last_export)) NULL else w$last_export
  nLists <- db_query("SELECT COUNT(*) AS n FROM lists WHERE ws = ?", list(ws))$n
  nSources <- db_query("SELECT COUNT(*) AS n FROM sources WHERE ws = ?", list(ws))$n
  empty <- nLists + sum(runs$n) + nSources + sum(va$n) == 0
  list(
    created = w$created, lastActive = w$last_seen, expires = w$last_seen + FLAME_LIMITS$retentionDays * 86400,
    retentionDays = FLAME_LIMITS$retentionDays, lastSaved = lastExport,
    state = if (empty) "empty" else if (is.null(lastExport)) "unsaved" else if (identical(fp, w$export_fingerprint)) "saved" else "changed",
    counts = list(lists = nLists, maxLists = FLAME_LIMITS$maxLists, sources = nSources,
                  runsComplete = cnt(runs, "complete"), runsActive = cnt(runs, c("queued", "running")), runsFailed = cnt(runs, "error"),
                  variantAnalyses = sum(va$n), variantActive = cnt(va, c("queued", "running"))),
    storageBytes = as.numeric(bytes), maxSessionBytes = FLAME_LIMITS$maxSessionBytes
  )
}

delete_workspace_data <- function(ws, keepWorkspace = TRUE) {
  db_exec("DELETE FROM runs WHERE ws = ?", list(ws))
  db_exec("DELETE FROM lists WHERE ws = ?", list(ws))
  db_exec("DELETE FROM sources WHERE ws = ?", list(ws))
  db_exec("DELETE FROM variant_analyses WHERE ws = ?", list(ws))
  if (!keepWorkspace) db_exec("DELETE FROM workspaces WHERE id = ?", list(ws))
  invisible(TRUE)
}

purge_expired_workspaces <- function() {
  cutoff <- now_s() - FLAME_LIMITS$retentionDays * 86400
  old <- db_query("SELECT id FROM workspaces WHERE last_seen < ?", list(cutoff))
  for (id in old$id) delete_workspace_data(id, keepWorkspace = FALSE)
  if (nrow(old) > 0) message("[api] purged ", nrow(old), " expired workspaces")
  invisible(nrow(old))
}

# ---- Lists ------------------------------------------------------------------

list_row_to_obj <- function(r, includeIds = FALSE) {
  ids <- from_json(r$ids_json)
  org <- ORGANISMS[ORGANISMS$taxid == r$taxid, , drop = FALSE]
  ranked <- !is.null(r$scores_json) && !is.na(r$scores_json) && nzchar(r$scores_json)
  out <- list(
    id = r$id, name = r$name, analyteType = r$analyte, organismTaxId = r$taxid, ranked = ranked,
    organismName = if (nrow(org) > 0) as.character(org$print_name[1]) else as.character(r$taxid),
    idFormat = r$id_format, size = length(ids), created = r$created,
    meta = from_json(r$meta_json) %||% list()
  )
  if (includeIds) {
    out$ids <- as.list(ids)
    if (ranked) out$scores <- as.list(from_json(r$scores_json))
  }
  out
}

get_list <- function(ws, idOrName) {
  r <- db_query("SELECT * FROM lists WHERE ws = ? AND (id = ? OR name = ?)", list(ws, idOrName, idOrName))
  if (nrow(r) == 0) api_abort(404, "List not found.")
  r[1, ]
}

#' Plain list record used by core functions (name, ids, organismTaxId, ...)
list_core <- function(r) {
  scores <- if (!is.null(r$scores_json) && !is.na(r$scores_json) && nzchar(r$scores_json)) from_json(r$scores_json) else NULL
  list(name = r$name, ids = from_json(r$ids_json), scores = scores, organismTaxId = r$taxid,
       analyteType = r$analyte, idFormat = r$id_format)
}

unique_list_name <- function(ws, base) {
  existing <- db_query("SELECT name FROM lists WHERE ws = ?", list(ws))$name
  if (!base %in% existing) return(base)
  i <- 2L
  while (paste0(base, " (", i, ")") %in% existing) i <- i + 1L
  paste0(base, " (", i, ")")
}

create_list <- function(ws, name, ids, taxid = 9606L, analyte = "GENE",
                        idFormat = "USERINPUT", meta = list(), uniquify = FALSE, scores = NULL) {
  name <- clean_name(name, "list name")
  if (!is.null(scores)) {
    # Ranked list: keep ID/score pairs aligned; duplicates keep the first score.
    ids <- trimws(as.character(unlist(ids)))
    scores <- suppressWarnings(as.numeric(unlist(scores)))
    if (length(scores) != length(ids)) api_abort(400, "A ranked list needs one score per ID.")
    ok <- nzchar(ids) & !is.na(ids) & grepl("^[A-Za-z0-9_.:\\-]+$", ids) & is.finite(scores)
    ids <- ids[ok]; scores <- scores[ok]
    keep <- !duplicated(tolower(ids))
    ids <- ids[keep]; scores <- scores[keep]
    if (length(ids) < 15) api_abort(400, "A ranked list needs at least 15 IDs with numeric scores.")
    if (length(ids) > 30000) api_abort(400, "A ranked list can contain at most 30,000 IDs.")
  } else {
    ids <- clean_ids(ids)
  }
  n <- db_query("SELECT COUNT(*) AS n FROM lists WHERE ws = ?", list(ws))$n
  if (FLAME_LIMITS$maxLists > 0 && n >= FLAME_LIMITS$maxLists)
    api_abort(409, sprintf("You can keep at most %d lists. Delete a list to add another.", FLAME_LIMITS$maxLists))
  if (uniquify) name <- unique_list_name(ws, name)
  if (nrow(db_query("SELECT 1 FROM lists WHERE ws = ? AND name = ?", list(ws, name))) > 0)
    api_abort(409, sprintf("A list named \"%s\" already exists.", name))
  taxid <- suppressWarnings(as.integer(taxid))
  if (is.na(taxid) || !(taxid %in% ORGANISMS$taxid)) api_abort(400, "Unknown organism.")
  idFormat <- normalizeWorkspaceIdFormat(idFormat %||% "USERINPUT")
  id <- new_object_id()
  db_exec("INSERT INTO lists (id, ws, name, analyte, taxid, id_format, ids_json, scores_json, meta_json, created)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    list(id, ws, name, normalizeAnalyteType(analyte), taxid, idFormat,
         to_json(ids), if (is.null(scores)) NA_character_ else to_json(scores), to_json(meta), now_s()))
  list_row_to_obj(db_query("SELECT * FROM lists WHERE id = ?", list(id))[1, ])
}

# ---- Runs -------------------------------------------------------------------

run_row_to_obj <- function(r) {
  org <- ORGANISMS[ORGANISMS$taxid == r$taxid, , drop = FALSE]
  list(
    id = r$id, tool = r$tool, toolKey = getWorkspaceToolKeyById(r$tool) %||% tolower(r$tool),
    displayNumber = r$display, paradigm = r$paradigm,
    title = paste0(r$tool, if (identical(r$paradigm, "GSEA")) " GSEA" else "", " (", r$display, ")"),
    listName = r$list_name, backgroundList = if (is.na(r$bg_list_name)) NULL else r$bg_list_name,
    organismTaxId = r$taxid,
    organismName = if (nrow(org) > 0) as.character(org$print_name[1]) else as.character(r$taxid),
    params = from_json(r$params_json),
    status = r$status, error = if (is.na(r$error)) NULL else r$error,
    resultCount = if (is.na(r$result_count)) NULL else r$result_count,
    messages = as.list(from_json(r$messages_json) %||% character(0)),
    created = r$created, finished = if (is.na(r$finished)) NULL else r$finished,
    progress = if (r$status %in% c("queued", "running") && exists("job_progress")) job_progress(r$ws, r$id) else NULL
  )
}

get_run_row <- function(ws, runId) {
  r <- db_query("SELECT * FROM runs WHERE ws = ? AND id = ?", list(ws, runId))
  if (nrow(r) == 0) api_abort(404, "Run not found.")
  r[1, ]
}

.run_cache <- new.env()

#' Deserialize a completed run's output (cached in memory by run id)
run_output <- function(ws, runId) {
  key <- paste(ws, runId)
  hit <- .run_cache[[key]]
  if (!is.null(hit)) return(hit)
  r <- get_run_row(ws, runId)
  if (!identical(r$status, "complete")) api_abort(409, "This run has not finished yet.")
  blob <- r$result_blob[[1]]
  out <- if (is.null(blob) || length(blob) == 0) NULL else unserialize(blob)
  # Runs stored before IDs were mapped back (e.g. STRING protein IDs) are fixed on read.
  if (!is.null(out$results)) out$results <- rollback_hit_ids(out$results, out$conversionTable)
  if (length(ls(.run_cache)) > 200) rm(list = ls(.run_cache), envir = .run_cache)
  .run_cache[[key]] <- out
  out
}

forget_run_cache <- function(ws, runId = NULL) {
  keys <- ls(.run_cache)
  drop <- if (is.null(runId)) keys[startsWith(keys, paste0(ws, " "))] else paste(ws, runId)
  rm(list = intersect(drop, keys), envir = .run_cache)
}

#' Stored run record in the shape build_comparison() expects
run_record <- function(ws, runId) {
  r <- get_run_row(ws, runId)
  out <- run_output(ws, runId)
  list(id = r$id, tool = r$tool, displayNumber = r$display, organism = r$taxid,
       paradigm = r$paradigm, listName = r$list_name, analyteType = "GENE",
       inputIds = out$conversionTable$input %||% character(0),
       params = out$parameters %||% from_json(r$params_json),
       results = out$results, conversionTable = out$conversionTable,
       backgroundSize = out$backgroundSize)
}

# ---- JSON serializer used by every route (#* @serializer flamejson) ---------
plumber::register_serializer("flamejson", function(...) {
  plumber::serializer_content_type("application/json; charset=utf-8", function(val) {
    jsonlite::toJSON(val, auto_unbox = TRUE, null = "null", na = "null",
                     digits = NA, force = TRUE, dataframe = "rows")
  })
}, verbose = FALSE)

#' Request body as a list (JSON), never NULL
body_of <- function(req) {
  b <- req$body
  if (is.null(b) || !is.list(b)) list() else b
}

# ---- Session export / import (plain JSON, validated) -------------------------

RESULT_COLUMNS <- c("Source", "Term_ID", "Function", "P-value", "-log10Pvalue", "Term Size",
                    "Query size", "Intersection Size", "Gene Ratio", "Term Coverage %",
                    "Fold Enrichment", "Positive Hits", "Term_ID_noLinks", "link")

run_output_to_json <- function(out) {
  if (is.null(out)) return(NULL)
  list(results = out$results, conversionTable = out$conversionTable,
       parameters = out$parameters, backgroundSize = out$backgroundSize,
       noHitAnalytes = as.list(out$noHitAnalytes %||% character(0)),
       messages = as.list(out$messages %||% character(0)))
}

#' Rebuild a run output from JSON, keeping only known columns with safe types
.records_to_df <- function(records) {
  if (is.null(records) || length(records) == 0) return(NULL)
  if (is.data.frame(records)) return(records)
  jsonlite::fromJSON(jsonlite::toJSON(records, auto_unbox = TRUE, null = "null", na = "null"),
                     simplifyVector = TRUE)
}

run_output_from_json <- function(x) {
  if (!is.list(x)) stop("bad run")
  res <- .records_to_df(x$results)
  if (!is.null(res)) {
    res <- as.data.frame(res, stringsAsFactors = FALSE, check.names = FALSE)
    if (!all(c("Source", "Term_ID", "Function", "P-value") %in% names(res))) stop("bad results")
    res <- res[, intersect(RESULT_COLUMNS, names(res)), drop = FALSE]
    for (col in c("P-value", "-log10Pvalue", "Term Size", "Query size", "Intersection Size",
                  "Gene Ratio", "Term Coverage %", "Fold Enrichment")) {
      if (col %in% names(res)) res[[col]] <- suppressWarnings(as.numeric(res[[col]]))
    }
    for (col in setdiff(names(res), c("P-value", "-log10Pvalue", "Term Size", "Query size",
                                      "Intersection Size", "Gene Ratio", "Term Coverage %", "Fold Enrichment"))) {
      res[[col]] <- as.character(res[[col]])
    }
    res$Function <- gsub("<[^>]*>", "", res$Function)
    res$Term_ID <- gsub("<[^>]*>", "", res$Term_ID)
    if ("link" %in% names(res)) res$link <- vapply(res$link, safe_link, character(1), USE.NAMES = FALSE)
    if (!"Term_ID_noLinks" %in% names(res)) res$Term_ID_noLinks <- res$Term_ID
  }
  ct <- .records_to_df(x$conversionTable)
  if (!is.null(ct)) {
    ct <- as.data.frame(ct, stringsAsFactors = FALSE)
    ct <- ct[, intersect(c("input", "target", "name"), names(ct)), drop = FALSE]
    for (col in names(ct)) ct[[col]] <- as.character(ct[[col]])
  }
  bg <- suppressWarnings(as.numeric(x$backgroundSize))
  list(status = "complete", resultCount = if (is.null(res)) 0L else nrow(res),
       results = res, parameters = x$parameters, conversionTable = ct,
       backgroundSize = if (length(bg) == 1 && is.finite(bg)) bg else NULL,
       noHitAnalytes = as.character(unlist(x$noHitAnalytes)),
       messages = as.character(unlist(x$messages)))
}

next_run_ids <- function(ws, toolKey, toolId) {
  seq <- (db_query("SELECT COALESCE(MAX(seq), 0) AS m FROM runs WHERE ws = ?", list(ws))$m) + 1L
  display <- (db_query("SELECT COALESCE(MAX(display), 0) AS m FROM runs WHERE ws = ? AND tool = ?", list(ws, toolId))$m) + 1L
  list(seq = as.integer(seq), display = as.integer(display), id = paste0(toolKey, "_", seq))
}

import_completed_run <- function(ws, r, out) {
  toolId <- as.character(r$tool %||% "Imported")[1]
  key <- gsub("[^a-z0-9]", "", tolower(as.character(r$toolKey %||% toolId)[1]))
  if (!nzchar(key)) key <- "run"
  ids <- next_run_ids(ws, key, toolId)
  taxid <- suppressWarnings(as.integer(r$organismTaxId)); if (is.na(taxid)) taxid <- 9606L
  db_exec("INSERT INTO runs (id, ws, seq, display, tool, paradigm, list_name, bg_list_name, taxid, params_json,
           status, result_count, result_blob, messages_json, created, finished)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'complete', ?, ?, ?, ?, ?)",
          list(ids$id, ws, ids$seq, ids$display, toolId, as.character(r$paradigm %||% "ORA")[1],
               substr(as.character(r$listName %||% "imported")[1], 1, 80),
               if (is.null(r$backgroundList)) NA else as.character(r$backgroundList)[1],
               taxid, to_json(out$parameters %||% list()), as.integer(out$resultCount),
               list(serialize(out, NULL)), to_json(as.list(out$messages)), now_s(), now_s()))
  ids$id
}

# ---- Custom GMT sources -----------------------------------------------------

save_source <- function(ws, name, sets, uniquify = FALSE) {
  name <- clean_name(name, "source name")
  n <- db_query("SELECT COUNT(*) AS n FROM sources WHERE ws = ?", list(ws))$n
  if (n >= FLAME_LIMITS$maxSources) api_abort(409, sprintf("You can keep at most %d custom sources.", FLAME_LIMITS$maxSources))
  existing <- db_query("SELECT name FROM sources WHERE ws = ?", list(ws))$name
  if (name %in% existing) {
    if (!uniquify) api_abort(409, sprintf("A source named \"%s\" already exists.", name))
    i <- 2L; while (paste0(name, " (", i, ")") %in% existing) i <- i + 1L
    name <- paste0(name, " (", i, ")")
  }
  sets <- lapply(sets, function(s) list(id = substr(as.character(s$id)[1], 1, 200),
                                        name = substr(as.character(s$name)[1], 1, 300),
                                        genes = as.character(unlist(s$genes))))
  id <- new_object_id()
  db_exec("INSERT INTO sources (id, ws, name, term_count, gmt_json, created) VALUES (?, ?, ?, ?, ?, ?)",
          list(id, ws, name, length(sets), to_json(sets), now_s()))
  list(id = id, name = name, termCount = length(sets))
}

get_source_sets <- function(ws, sourceId) {
  r <- db_query("SELECT name, gmt_json FROM sources WHERE ws = ? AND id = ?", list(ws, sourceId))
  if (nrow(r) == 0) api_abort(404, "Custom source not found.")
  sets <- jsonlite::fromJSON(r$gmt_json, simplifyVector = FALSE)
  list(name = r$name, sets = lapply(sets, function(s) list(id = s$id, name = s$name, genes = unlist(s$genes))))
}
