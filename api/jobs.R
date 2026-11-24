# =============================================================================
# Background enrichment jobs (mirai daemons)
# =============================================================================
# Runs execute in separate R processes so slow tools never block the API.
# The main process polls finished jobs (later::later loop, every second) and
# writes results to SQLite. Clients poll GET /api/runs for status.
# =============================================================================

.flame_jobs <- new.env()
.progress_dir <- file.path(tempdir(), "flame-progress")
dir.create(.progress_dir, showWarnings = FALSE)
.progress_file <- function(key) file.path(.progress_dir, paste0(gsub("[^A-Za-z0-9_]", "_", key), ".json"))

#' Progress of a running job: list(fraction, message, elapsed), or NULL
job_progress <- function(ws, runId, kind = NULL) {
  key <- if (identical(kind, "variants")) paste(ws, "variants", runId) else paste(ws, runId)
  job <- .flame_jobs[[key]]
  if (is.null(job)) return(NULL)
  p <- read_progress(job$progress) %||% list(fraction = 0.02, message = "Waiting for a free worker")
  c(p, list(elapsed = round(now_s() - job$started, 1)))
}

start_job_workers <- function(n = as.integer(Sys.getenv("FLAME_WORKERS", "4"))) {
  n <- max(1L, min(n, parallel::detectCores()))
  mirai::daemons(n)
  mirai::everywhere({
    suppressMessages(suppressWarnings(
      pkgload::load_all(root, export_all = TRUE, helpers = FALSE, quiet = TRUE)))
  }, root = FLAME_ROOT)
  message("[jobs] ", n, " enrichment workers ready")
  start_fetch_workers()
  later::later(poll_jobs, 1)
  invisible(n)
}

active_job_count <- function(ws) {
  ids <- ls(.flame_jobs)
  sum(vapply(ids, function(id) identical(.flame_jobs[[id]]$ws, ws), logical(1)))
}

#' Queue a run in a worker. `job` is list(kind = "ora", toolId, lst, params, bg)
#' or list(kind = "gmt", lst, sets, sourceName, threshold, correction, bgIds).
submit_run <- function(ws, runId, job) {
  pf <- .progress_file(paste(ws, runId))
  unlink(pf)
  m <- if (identical(job$kind, "gsea")) {
    mirai::mirai({ options(flame.progress_file = pf); execute_gsea_run(lst, params) }, lst = job$lst, params = job$params, pf = pf)
  } else if (identical(job$kind, "gmt")) {
    mirai::mirai(
      { options(flame.progress_file = pf); execute_gmt_run(lst, sets, sourceName, threshold, correction, bgIds) },
      lst = job$lst, sets = job$sets, sourceName = job$sourceName,
      threshold = job$threshold, correction = job$correction, bgIds = job$bgIds, pf = pf)
  } else {
    mirai::mirai(
      { options(flame.progress_file = pf); execute_ora_run(toolId, lst, params, bg) },
      toolId = job$toolId, lst = job$lst, params = job$params, bg = job$bg, pf = pf)
  }
  .flame_jobs[[paste(ws, runId)]] <- list(ws = ws, runId = runId, m = m, started = now_s(), progress = pf)
  db_exec("UPDATE runs SET status = 'running' WHERE id = ? AND ws = ?", list(runId, ws))
  invisible(runId)
}

#' Variant analysis job (Ensembl VEP + phenotypes); results go to variant_analyses
submit_variant_job <- function(ws, analysisId, text, taxid) {
  pf <- .progress_file(paste(ws, "variants", analysisId))
  unlink(pf)
  m <- mirai::mirai({ options(flame.progress_file = pf); analyse_variants(text, taxid) }, text = text, taxid = taxid, pf = pf)
  .flame_jobs[[paste(ws, "variants", analysisId)]] <- list(ws = ws, runId = analysisId, kind = "variants", m = m, started = now_s(), progress = pf)
  db_exec("UPDATE variant_analyses SET status = 'running' WHERE ws = ? AND id = ?", list(ws, analysisId))
  invisible(analysisId)
}

.finish_variant_job <- function(job) {
  value <- job$m$data
  if (mirai::is_mirai_error(value) || mirai::is_error_value(value) || !is.list(value)) {
    msg <- if (mirai::is_mirai_error(value)) conditionMessage(value) else "Ensembl did not respond."
    db_exec("UPDATE variant_analyses SET status = 'error', error = ?, finished = ? WHERE ws = ? AND id = ?",
            list(substr(msg, 1, 500), now_s(), job$ws, job$runId))
  } else {
    db_exec("UPDATE variant_analyses SET status = 'complete', result_blob = ?, finished = ? WHERE ws = ? AND id = ?",
            list(list(serialize(value, NULL)), now_s(), job$ws, job$runId))
  }
}

cancel_run <- function(ws, runId) {
  key <- paste(ws, runId)
  job <- .flame_jobs[[key]]
  if (!is.null(job)) {
    try(mirai::stop_mirai(job$m), silent = TRUE)
    rm(list = key, envir = .flame_jobs)
  }
}

.finish_run <- function(job) {
  runId <- job$runId
  value <- job$m$data
  stillExists <- nrow(db_query("SELECT 1 FROM runs WHERE id = ? AND ws = ?", list(runId, job$ws))) > 0
  if (!stillExists) return(invisible(NULL))   # deleted while running

  if (mirai::is_mirai_error(value) || mirai::is_error_value(value) || !is.list(value)) {
    msg <- if (mirai::is_mirai_error(value)) conditionMessage(value) else "The enrichment tool did not respond."
    db_exec("UPDATE runs SET status = 'error', error = ?, finished = ? WHERE ws = ? AND id = ?",
            list(substr(msg, 1, 500), now_s(), job$ws, runId))
  } else {
    db_exec("UPDATE runs SET status = 'complete', result_count = ?, result_blob = ?,
             messages_json = ?, params_json = ?, finished = ? WHERE ws = ? AND id = ?",
            list(as.integer(value$resultCount %||% 0L), list(serialize(value, NULL)),
                 to_json(as.list(value$messages %||% character(0))),
                 to_json(value$parameters), now_s(), job$ws, runId))
  }
  notify_by_email(job$ws, runId)
}

poll_jobs <- function() {
  for (key in ls(.flame_jobs)) {
    job <- .flame_jobs[[key]]
    if (!mirai::unresolved(job$m)) {
      tryCatch(if (identical(job$kind, "variants")) .finish_variant_job(job) else .finish_run(job),
               error = function(e) message("[jobs] finish failed: ", conditionMessage(e)))
      unlink(job$progress)
      rm(list = key, envir = .flame_jobs)
    }
  }
  later::later(poll_jobs, 1)
}

# Optional email notification. Only active when an SMTP server is configured
# (FLAME_SMTP_URL, FLAME_SMTP_FROM) and the user chose to leave an address.
notify_by_email <- function(ws, runId) {
  smtp <- Sys.getenv("FLAME_SMTP_URL", "")
  from <- Sys.getenv("FLAME_SMTP_FROM", "")
  if (!nzchar(smtp) || !nzchar(from)) return(invisible(FALSE))
  email <- db_query("SELECT email FROM workspaces WHERE id = ?", list(ws))$email
  if (length(email) == 0 || is.na(email) || !nzchar(email)) return(invisible(FALSE))
  pending <- db_query("SELECT COUNT(*) AS n FROM runs WHERE ws = ? AND status IN ('queued','running')", list(ws))$n
  if (pending > 0) return(invisible(FALSE))   # one mail when the batch is done
  base <- Sys.getenv("FLAME_PUBLIC_URL", "")
  body <- paste0("Subject: Your Flame results are ready\r\n\r\n",
                 "All enrichment runs in your Flame workspace have finished.\r\n",
                 if (nzchar(base)) paste0("Open your results: ", base, "/w/", ws, "\r\n") else "")
  try(curl::send_mail(from, email, body, smtp_server = smtp, use_ssl = "try"), silent = TRUE)
  invisible(TRUE)
}

# ---- Analysis tasks ----------------------------------------------------------
# Heavier one-off analyses (regulators, drugs, network modules, robustness,
# metabolites) run in the same workers. Each is a row of the `analyses` table
# (results stored as JSON, so they survive restarts and travel in session
# files); while it runs, its mirai handle and progress file are kept here.
.flame_tasks <- new.env()

#' `fn` is the name of a flame function (resolved in the worker), `args` its arguments.
submit_task <- function(ws, type, fn, args, label = type, listId = NULL, meta = list()) {
  id <- new_object_id()
  pf <- .progress_file(paste(ws, "task", id))
  unlink(pf)
  db_exec("INSERT INTO analyses (id, ws, type, label, list_id, status, meta_json, created) VALUES (?, ?, ?, ?, ?, 'running', ?, ?)",
          list(id, ws, type, substr(as.character(label), 1, 200), if (is.null(listId) || !nzchar(listId)) NA_character_ else listId,
               as.character(jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null")), now_s()))
  m <- mirai::mirai({ options(flame.progress_file = pf); do.call(fn, args) }, fn = fn, args = args, pf = pf)
  .flame_tasks[[id]] <- list(ws = ws, m = m, progress = pf)
  id
}

.task_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, force = TRUE, dataframe = "rows"))

#' Collect a finished worker result into the database.
.task_collect <- function(ws, id) {
  t <- .flame_tasks[[id]]
  if (is.null(t) || !identical(t$ws, ws) || mirai::unresolved(t$m)) return(invisible(FALSE))
  v <- t$m$data
  if (mirai::is_mirai_error(v) || mirai::is_error_value(v)) {
    db_exec("UPDATE analyses SET status = 'error', error = ?, finished = ? WHERE ws = ? AND id = ?",
            list(substr(conditionMessage(v), 1, 400), now_s(), ws, id))
  } else {
    db_exec("UPDATE analyses SET status = 'complete', result_json = ?, finished = ? WHERE ws = ? AND id = ?",
            list(.task_json(v), now_s(), ws, id))
  }
  unlink(t$progress)
  rm(list = id, envir = .flame_tasks)
  invisible(TRUE)
}

#' One analysis: status, progress and (withResult) its result; NULL if unknown.
task_state <- function(ws, id, withResult = TRUE) {
  .task_collect(ws, id)
  r <- db_query(paste0("SELECT id, type, label, list_id, status, error, run_id, meta_json, created, finished",
                       if (withResult) ", result_json" else "", " FROM analyses WHERE ws = ? AND id = ?"), list(ws, id))
  if (!nrow(r)) return(NULL)
  # A row still "running" without a worker handle was interrupted by a restart.
  if (identical(r$status, "running") && is.null(.flame_tasks[[id]])) {
    msg <- "Interrupted by a server restart. Please run the analysis again."
    db_exec("UPDATE analyses SET status = 'error', error = ?, finished = ? WHERE ws = ? AND id = ?", list(msg, now_s(), ws, id))
    r$status <- "error"; r$error <- msg
  }
  t <- .flame_tasks[[id]]
  list(id = r$id, type = r$type, label = r$label, status = r$status, error = if (is.na(r$error)) NULL else r$error,
       listId = if (is.na(r$list_id)) NULL else r$list_id, runId = if (is.na(r$run_id)) NULL else r$run_id,
       started = r$created, finished = if (is.na(r$finished)) NULL else r$finished,
       meta = from_json(r$meta_json) %||% list(),
       progress = if (identical(r$status, "running"))
         c(read_progress(t$progress) %||% list(fraction = 0.03, message = "Waiting for a free worker"), list(elapsed = round(now_s() - r$created, 1))) else NULL,
       result = if (withResult && identical(r$status, "complete") && !is.na(r$result_json))
         jsonlite::fromJSON(r$result_json, simplifyVector = FALSE) else NULL)
}

active_task_count <- function(ws) sum(vapply(ls(.flame_tasks), function(k) identical(.flame_tasks[[k]]$ws, ws), logical(1)))

# ---- Requests to external services (TCGA, nutrients, diseases, EXTRACT, REVIGO, ...) -----
# They run on a small pool of their own, so a slow service neither blocks the API (plumber
# answers on one R thread) nor waits behind enrichment runs; too many at once are refused.
FETCH_WORKERS <- max(1L, as.integer(Sys.getenv("FLAME_FETCH_WORKERS", "2")))
FETCH_MAX_PENDING <- FETCH_WORKERS * 4L
FETCH_TIMEOUT_MS <- 10 * 60 * 1000
.fetch_state <- new.env()
.fetch_state$pending <- 0L
.fetch_state$revigo <- list()        # REVIGO key -> promise, so identical requests share one job

start_fetch_workers <- function(n = FETCH_WORKERS) {
  mirai::daemons(n, .compute = "fetch")
  mirai::everywhere({
    suppressMessages(suppressWarnings(
      pkgload::load_all(root, export_all = TRUE, helpers = FALSE, quiet = TRUE)))
    api_abort <<- function(status, message)
      stop(structure(class = c("flame_api_error", "error", "condition"), list(message = message, status = status, call = NULL)))
  }, root = FLAME_ROOT, .compute = "fetch")
  .fetch_state$ready <- TRUE
  message("[jobs] ", n, " workers for external services ready")
}

# Messages of network failures name hosts and proxies: tell the user something useful instead.
public_error_message <- function(e) {
  msg <- conditionMessage(e)
  if (inherits(e, c("curl_error", "httr2_failure", "httr2_http")) ||
      grepl("^(Timeout was reached|Could not resolve|Failed to connect|SSL|OpenSSL|Recv failure|Send failure|HTTP [0-9]{3})", msg))
    "The external service did not respond. Please try again later." else msg
}

#' Evaluate `expr` (a language object) with `args` on the external-service pool and answer
#' with its value; errors become 4xx/5xx JSON as with api_handle. Without the pool (tests,
#' FLAME_NO_WORKERS=1) it runs here.
fetch_async <- function(res, expr, args = list()) {
  wrapped <- bquote(tryCatch(list(ok = TRUE, value = .(expr)),
    flame_api_error = function(e) list(ok = FALSE, status = e$status, error = conditionMessage(e)),
    error = function(e) list(ok = FALSE, status = 422L, error = .(public_error_message)(e))))   # the function itself travels with the job
  answer <- function(r) {
    if (isTRUE(r$ok)) return(r$value)
    res$status <- as.integer(r$status %||% 422L)
    list(error = r$error)
  }
  if (!isTRUE(.fetch_state$ready)) return(answer(eval(wrapped, list2env(args, parent = environment()))))
  if (.fetch_state$pending >= FETCH_MAX_PENDING) {
    res$status <- 503L
    return(list(error = "The server is busy with other requests to external services. Please try again in a minute."))
  }
  .fetch_state$pending <- .fetch_state$pending + 1L
  m <- mirai::mirai(.expr = wrapped, .args = args, .timeout = FETCH_TIMEOUT_MS, .compute = "fetch")
  p <- promises::then(promises::as.promise(m), onFulfilled = answer, onRejected = function(e) {
    res$status <- 504L
    list(error = "The external service took too long to answer. Please try again later.")
  })
  promises::finally(p, function() .fetch_state$pending <- .fetch_state$pending - 1L)
}

#' Ask REVIGO from the external-service pool; the view request is answered when it arrives
#' (plumber waits on the promise without blocking other requests). Identical requests in
#' flight share one REVIGO job.
revigo_async <- function(pending, build, res) {
  failed <- "REVIGO did not respond. Try again later, or use another method."
  job <- .fetch_state$revigo[[pending$key]]
  if (is.null(job)) {
    if (!isTRUE(.fetch_state$ready)) {
      tab <- tryCatch(revigo_fetch(pending$ids, pending$pvalues, pending$taxid, pending$cutoff, pending$measure), error = function(e) NULL)
      if (!is.data.frame(tab)) { res$status <- 502L; return(list(error = failed)) }
      revigo_cache_set(pending$key, tab)
      return(api_handle(res, build()))
    }
    if (.fetch_state$pending >= FETCH_MAX_PENDING) {
      res$status <- 503L
      return(list(error = "The server is busy with other REVIGO requests. Please try again in a minute, or use another method."))
    }
    .fetch_state$pending <- .fetch_state$pending + 1L
    m <- mirai::mirai(
      tryCatch(revigo_fetch(ids, pvalues, taxid, cutoff, measure),
               error = function(e) structure(list(message = conditionMessage(e)), class = "revigo_failure")),
      ids = pending$ids, pvalues = pending$pvalues, taxid = pending$taxid,
      cutoff = pending$cutoff, measure = pending$measure, .timeout = FETCH_TIMEOUT_MS, .compute = "fetch")
    job <- promises::then(promises::as.promise(m), onFulfilled = function(tab) {
      if (is.data.frame(tab)) revigo_cache_set(pending$key, tab)
      tab
    })
    job <- promises::finally(job, function() {
      .fetch_state$pending <- .fetch_state$pending - 1L
      .fetch_state$revigo[[pending$key]] <- NULL
    })
    .fetch_state$revigo[[pending$key]] <- job
  }
  promises::then(job,
    onFulfilled = function(tab) {
      if (!is.data.frame(tab)) {
        res$status <- 502L
        return(list(error = if (inherits(tab, "revigo_failure")) public_error_message(simpleError(tab$message)) else failed))
      }
      api_handle(res, build())
    },
    onRejected = function(e) { res$status <- 502L; list(error = failed) })
}
