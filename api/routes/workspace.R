# /api/workspace — anonymous workspaces (no login). The id is the only key:
# keep it private; anyone holding the link can open the workspace.

.ws_creations <- new.env()

#* Create a new workspace. Returns its id; send it as X-Flame-Workspace.
#* @post /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    # At most 30 new workspaces per hour per client address.
    ip <- trimws(strsplit(req$HTTP_X_FORWARDED_FOR %||% req$REMOTE_ADDR %||% "local", ",")[[1]][1])
    hour <- floor(now_s() / 3600)
    key <- paste(ip, hour)
    n <- (.ws_creations[[key]] %||% 0) + 1
    .ws_creations[[key]] <- n
    if (length(ls(.ws_creations)) > 5000) rm(list = ls(.ws_creations), envir = .ws_creations)
    if (n > 30) api_abort(429, "Too many new workspaces from this address. Please try again later.")
    res$status <- 201
    list(id = create_workspace(), retentionDays = FLAME_LIMITS$retentionDays)
  })
}

#* Workspace summary: lists, runs and custom sources
#* @get /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    w <- db_query("SELECT created, last_seen, email FROM workspaces WHERE id = ?", list(ws))
    lists <- db_query("SELECT * FROM lists WHERE ws = ? ORDER BY created DESC", list(ws))
    runs <- db_query("SELECT id, ws, seq, display, tool, paradigm, list_id, list_name, bg_list_name, taxid,
                             params_json, status, error, result_count, messages_json, created, finished
                      FROM runs WHERE ws = ? ORDER BY seq", list(ws))
    sources <- db_query("SELECT id, name, term_count, created FROM sources WHERE ws = ? ORDER BY created", list(ws))
    list(
      id = ws, created = w$created, expires = w$last_seen + FLAME_LIMITS$retentionDays * 86400,
      hasEmail = !is.na(w$email) && nzchar(w$email),
      lists = lapply(seq_len(nrow(lists)), function(i) list_row_to_obj(lists[i, ])),
      runs = lapply(seq_len(nrow(runs)), function(i) run_row_to_obj(runs[i, ])),
      sources = lapply(seq_len(nrow(sources)), function(i) as.list(sources[i, ])),
      status = workspace_status(ws)
    )
  })
}

#* Session status only (cheap; polled by the workspace header)
#* @get /status
#* @serializer flamejson
function(req, res) {
  api_handle(res, workspace_status(require_workspace(req)))
}

#* Set or clear the optional notification email (used only to say runs finished)
#* @put /email
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    email <- trimws(as.character(body_of(req)$email %||% "")[1])
    if (nzchar(email) && !grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", email, perl = TRUE))
      api_abort(400, "That does not look like an email address.")
    db_exec("UPDATE workspaces SET email = ? WHERE id = ?", list(if (nzchar(email)) email else NA, ws))
    list(ok = TRUE, hasEmail = nzchar(email))
  })
}

#* Clear all lists, runs and sources (the workspace link stays valid)
#* @delete /data
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    for (id in db_query("SELECT id FROM runs WHERE ws = ?", list(ws))$id) cancel_run(ws, id)
    delete_workspace_data(ws, keepWorkspace = TRUE)
    forget_run_cache(ws)
    list(ok = TRUE)
  })
}

#* Delete the workspace and everything in it, permanently
#* @delete /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    for (id in db_query("SELECT id FROM runs WHERE ws = ?", list(ws))$id) cancel_run(ws, id)
    delete_workspace_data(ws, keepWorkspace = FALSE)
    forget_run_cache(ws)
    list(ok = TRUE)
  })
}

.split_arg <- function(x) {
  v <- trimws(unlist(strsplit(paste(as.character(unlist(x %||% "")), collapse = ","), ",")))
  v[nzchar(v)]
}

#* Export the workspace as a session file (JSON).
#* Query: parts=lists,sources,runs,variants (default all); runIds=a,b (default all finished runs);
#*        listIds=..., variantIds=... (default all); prefs (client display settings are added by the web app).
#* @get /export
#* @serializer flamejson
function(req, res, parts = "", runIds = "", listIds = "", variantIds = "") {
  api_handle(res, {
    ws <- require_workspace(req)
    parts <- .split_arg(parts); if (!length(parts)) parts <- c("lists", "sources", "runs", "variants")
    want <- function(p) p %in% parts
    pick <- function(df, ids) { ids <- .split_arg(ids); if (length(ids)) df[df$id %in% ids, , drop = FALSE] else df }
    lists <- if (want("lists")) pick(db_query("SELECT * FROM lists WHERE ws = ? ORDER BY created", list(ws)), listIds) else NULL
    sources <- if (want("sources")) db_query("SELECT name, gmt_json FROM sources WHERE ws = ?", list(ws)) else NULL
    runs <- if (want("runs")) pick(db_query("SELECT * FROM runs WHERE ws = ? AND status = 'complete' ORDER BY seq", list(ws)), runIds) else NULL
    vas <- if (want("variants")) pick(db_query("SELECT * FROM variant_analyses WHERE ws = ? AND status = 'complete' ORDER BY created", list(ws)), variantIds) else NULL
    # A full export marks the workspace as saved.
    full <- all(c("lists", "sources", "runs", "variants") %in% parts) && !length(.split_arg(runIds)) &&
      !length(.split_arg(listIds)) && !length(.split_arg(variantIds))
    if (full) db_exec("UPDATE workspaces SET last_export = ?, export_fingerprint = ? WHERE id = ?", list(now_s(), workspace_fingerprint(ws), ws))
    res$setHeader("Content-Disposition", "attachment; filename=\"flame-session.json\"")
    n <- function(df) if (is.null(df)) 0L else nrow(df)
    list(
      format = "flame-session", version = 2L, flameVersion = as.character(utils::packageVersion("flame")),
      exported = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), complete = full,
      contents = list(lists = n(lists), sources = n(sources), runs = n(runs), variants = n(vas)),
      lists = if (n(lists)) lapply(seq_len(nrow(lists)), function(i) list_row_to_obj(lists[i, ], includeIds = TRUE)) else list(),
      sources = if (n(sources)) lapply(seq_len(nrow(sources)), function(i) list(name = sources$name[i], sets = from_json(sources$gmt_json[i]))) else list(),
      runs = if (n(runs)) lapply(seq_len(nrow(runs)), function(i) {
        r <- runs[i, ]
        c(run_row_to_obj(r), list(output = run_output_to_json(run_output(ws, r$id))))
      }) else list(),
      variants = if (n(vas)) lapply(seq_len(nrow(vas)), function(i) {
        v <- vas[i, ]
        result <- unserialize(v$result_blob[[1]])
        result$variants$impact <- as.character(result$variants$impact)
        list(name = v$name, organismTaxId = v$taxid, inputCount = v$input_count, created = v$created, result = result)
      }) else list()
    )
  })
}

#* Load a session file into this workspace.
#* Query: mode=add (default) | replace (clears this workspace first);
#*        parts=lists,sources,runs,variants (default all).
#* @post /import
#* @serializer flamejson
function(req, res, mode = "add", parts = "") {
  api_handle(res, {
    ws <- require_workspace(req)
    # Re-parse without simplification so nested lists keep their shape.
    b <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
                  error = function(e) api_abort(400, "The session file is not valid JSON."))
    if (!identical(b$format, "flame-session")) api_abort(400, "This is not a Flame session file.")
    parts <- .split_arg(parts); if (!length(parts)) parts <- c("lists", "sources", "runs", "variants")
    if (identical(mode, "replace")) {
      for (id in db_query("SELECT id FROM runs WHERE ws = ?", list(ws))$id) cancel_run(ws, id)
      delete_workspace_data(ws, keepWorkspace = TRUE)
      forget_run_cache(ws)
    }
    added <- list(lists = 0L, runs = 0L, sources = 0L, variants = 0L, skipped = list())
    skip <- function(what, why) added$skipped[[length(added$skipped) + 1]] <<- paste0(what, ": ", why)
    if ("lists" %in% parts) for (l in b$lists %||% list()) {
      ok <- tryCatch({
        create_list(ws, l$name, unlist(l$ids), l$organismTaxId %||% 9606L, l$analyteType %||% "GENE", l$idFormat %||% "USERINPUT",
                    l$meta %||% list(), uniquify = TRUE, scores = if (length(l$scores)) unlist(l$scores) else NULL)
        TRUE
      }, flame_api_error = function(e) { skip(paste("List", substr(as.character(l$name %||% "?"), 1, 60)), conditionMessage(e)); FALSE })
      if (ok) added$lists <- added$lists + 1L
    }
    if ("sources" %in% parts) for (s in b$sources %||% list()) {
      sets <- lapply(s$sets, function(x) list(id = x$id, name = x$name, genes = unlist(x$genes)))
      ok <- tryCatch({ save_source(ws, s$name, sets, uniquify = TRUE); TRUE },
                     flame_api_error = function(e) { skip(paste("Source", substr(as.character(s$name %||% "?"), 1, 60)), conditionMessage(e)); FALSE })
      if (ok) added$sources <- added$sources + 1L
    }
    # Session files carry plain JSON only (never R-serialized data, which is
    # unsafe to deserialize from untrusted input).
    if ("runs" %in% parts) for (r in b$runs %||% list()) {
      out <- tryCatch(run_output_from_json(r$output), error = function(e) NULL)
      if (is.null(out)) { skip(paste("Run", substr(as.character(r$title %||% r$id %||% "?"), 1, 60)), "results could not be read"); next }
      import_completed_run(ws, r, out)
      added$runs <- added$runs + 1L
    }
    if ("variants" %in% parts) for (v in b$variants %||% list()) {
      result <- tryCatch(variant_result_from_json(v$result), error = function(e) NULL)
      if (is.null(result)) { skip(paste("Variant analysis", substr(as.character(v$name %||% "?"), 1, 60)), "could not be read"); next }
      db_exec("INSERT INTO variant_analyses (id, ws, name, taxid, status, input_count, result_blob, created, finished)
               VALUES (?, ?, ?, ?, 'complete', ?, ?, ?, ?)",
              list(new_object_id(), ws, clean_name(v$name %||% "Imported analysis", "analysis name"), result$organismTaxId,
                   result$inputCount, list(serialize(result, NULL)), now_s(), now_s()))
      added$variants <- added$variants + 1L
    }
    added
  })
}
