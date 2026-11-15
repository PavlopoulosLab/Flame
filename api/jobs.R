# =============================================================================
# Background enrichment jobs (mirai daemons)
# =============================================================================
# Runs execute in separate R processes so slow tools never block the API.
# The main process polls finished jobs (later::later loop, every second) and
# writes results to SQLite. Clients poll GET /api/runs for status.
# =============================================================================

.flame_jobs <- new.env()

start_job_workers <- function(n = as.integer(Sys.getenv("FLAME_WORKERS", "4"))) {
  n <- max(1L, min(n, parallel::detectCores()))
  mirai::daemons(n)
  mirai::everywhere({
    suppressMessages(suppressWarnings(
      pkgload::load_all(root, export_all = TRUE, helpers = FALSE, quiet = TRUE)))
  }, root = FLAME_ROOT)
  message("[jobs] ", n, " enrichment workers ready")
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
  m <- if (identical(job$kind, "gsea")) {
    mirai::mirai(execute_gsea_run(lst, params), lst = job$lst, params = job$params)
  } else if (identical(job$kind, "gmt")) {
    mirai::mirai(
      execute_gmt_run(lst, sets, sourceName, threshold, correction, bgIds),
      lst = job$lst, sets = job$sets, sourceName = job$sourceName,
      threshold = job$threshold, correction = job$correction, bgIds = job$bgIds)
  } else {
    mirai::mirai(
      execute_ora_run(toolId, lst, params, bg),
      toolId = job$toolId, lst = job$lst, params = job$params, bg = job$bg)
  }
  .flame_jobs[[paste(ws, runId)]] <- list(ws = ws, runId = runId, m = m, started = now_s())
  db_exec("UPDATE runs SET status = 'running' WHERE id = ? AND ws = ?", list(runId, ws))
  invisible(runId)
}

#' Variant analysis job (Ensembl VEP + phenotypes); results go to variant_analyses
submit_variant_job <- function(ws, analysisId, text, taxid) {
  m <- mirai::mirai(analyse_variants(text, taxid), text = text, taxid = taxid)
  .flame_jobs[[paste(ws, "variants", analysisId)]] <- list(ws = ws, runId = analysisId, kind = "variants", m = m, started = now_s())
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
