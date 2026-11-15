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
      sources = lapply(seq_len(nrow(sources)), function(i) as.list(sources[i, ]))
    )
  })
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

#* Export the workspace (lists, sources, finished runs with results) as JSON
#* @get /export
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    lists <- db_query("SELECT * FROM lists WHERE ws = ?", list(ws))
    sources <- db_query("SELECT name, gmt_json FROM sources WHERE ws = ?", list(ws))
    runs <- db_query("SELECT * FROM runs WHERE ws = ? AND status = 'complete' ORDER BY seq", list(ws))
    res$setHeader("Content-Disposition", "attachment; filename=\"flame-session.json\"")
    list(
      format = "flame-session", version = 1L, exported = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      lists = lapply(seq_len(nrow(lists)), function(i) list_row_to_obj(lists[i, ], includeIds = TRUE)),
      sources = lapply(seq_len(nrow(sources)), function(i) list(name = sources$name[i], sets = from_json(sources$gmt_json[i]))),
      runs = lapply(seq_len(nrow(runs)), function(i) {
        r <- runs[i, ]
        c(run_row_to_obj(r), list(output = run_output_to_json(run_output(ws, r$id))))
      })
    )
  })
}

#* Import a previously exported session into this workspace (adds to it)
#* @post /import
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    # Re-parse without simplification so nested lists keep their shape.
    b <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
                  error = function(e) api_abort(400, "The session file is not valid JSON."))
    if (!identical(b$format, "flame-session")) api_abort(400, "This is not a Flame session file.")
    added <- list(lists = 0L, runs = 0L, sources = 0L)
    for (l in b$lists %||% list()) {
      create_list(ws, l$name, unlist(l$ids), l$organismTaxId, l$analyteType, l$idFormat, l$meta %||% list(), uniquify = TRUE)
      added$lists <- added$lists + 1L
    }
    for (s in b$sources %||% list()) {
      sets <- lapply(s$sets, function(x) list(id = x$id, name = x$name, genes = unlist(x$genes)))
      save_source(ws, s$name, sets, uniquify = TRUE)
      added$sources <- added$sources + 1L
    }
    # Session files carry plain JSON only (never R-serialized data, which is
    # unsafe to deserialize from untrusted input).
    for (r in b$runs %||% list()) {
      out <- tryCatch(run_output_from_json(r$output), error = function(e) NULL)
      if (is.null(out)) next
      import_completed_run(ws, r, out)
      added$runs <- added$runs + 1L
    }
    added
  })
}
