# /api/runs — enrichment runs (async), results views, details, exports

#* All runs with status (poll this while any run is queued or running)
#* @get /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    rows <- db_query("SELECT id, ws, seq, display, tool, paradigm, list_id, list_name, bg_list_name, taxid,
                             params_json, status, error, result_count, messages_json, created, finished
                      FROM runs WHERE ws = ? ORDER BY seq", list(ws))
    lapply(seq_len(nrow(rows)), function(i) run_row_to_obj(rows[i, ]))
  })
}

#* Start runs: one per selected tool and per selected custom source.
#* Body: { listId, tools:[toolKey], sourceIds:[customSourceId], datasources:[code],
#*        threshold, perTool:{ toolKey: { threshold, correction, backgroundListId } } }
#* @post /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    lstRow <- get_list(ws, as.character(b$listId %||% ""))
    lst <- list_core(lstRow)
    toolKeys <- unique(tolower(as.character(unlist(b$tools))))
    sourceIds <- unique(as.character(unlist(b$sourceIds)))
    if (length(toolKeys) + length(sourceIds) == 0) api_abort(400, "Select at least one tool or custom source.")

    total <- db_query("SELECT COUNT(*) AS n FROM runs WHERE ws = ?", list(ws))$n
    if (total + length(toolKeys) + length(sourceIds) > FLAME_LIMITS$maxRuns)
      api_abort(409, sprintf("A workspace can hold at most %d runs. Remove some runs first.", FLAME_LIMITS$maxRuns))
    if (active_job_count(ws) + length(toolKeys) + length(sourceIds) > FLAME_LIMITS$maxActiveJobs)
      api_abort(429, "Too many runs are still in progress. Wait for them to finish and try again.")

    perTool <- b$perTool %||% list()
    created <- list(); reused <- list(); skipped <- list()

    insert_run <- function(toolKey, toolId, params, bgName) {
      ids <- next_run_ids(ws, toolKey, toolId)
      db_exec("INSERT INTO runs (id, ws, seq, display, tool, paradigm, list_id, list_name, bg_list_name,
               taxid, params_json, status, created) VALUES (?, ?, ?, ?, ?, 'ORA', ?, ?, ?, ?, ?, 'queued', ?)",
              list(ids$id, ws, ids$seq, ids$display, toolId, lstRow$id, lstRow$name,
                   if (is.null(bgName)) NA else bgName, lstRow$taxid, to_json(params), now_s()))
      ids$id
    }

    for (key in toolKeys) {
      toolId <- getWorkspaceToolIdByKey(key)
      if (is.null(toolId)) { skipped[[length(skipped) + 1]] <- list(tool = key, reason = "Unknown tool."); next }
      if (!tool_supports_organism(toolId, lstRow$taxid)) {
        skipped[[length(skipped) + 1]] <- list(tool = key, reason = sprintf("%s does not support %s.", toolId, .organism_label(lstRow$taxid)))
        next
      }
      cfg <- perTool[[key]] %||% list()
      bgRow <- NULL
      if (nzchar(as.character(cfg$backgroundListId %||% ""))) bgRow <- get_list(ws, as.character(cfg$backgroundListId))
      bg <- if (!is.null(bgRow)) list_core(bgRow) else NULL
      params <- build_run_params(toolId, lst, b$datasources, cfg$threshold %||% b$threshold %||% 0.05,
                                 cfg$correction, bg)

      existing <- db_query("SELECT id, taxid, params_json, status FROM runs WHERE ws = ? AND tool = ? AND status != 'error'",
                           list(ws, toolId))
      match <- Filter(function(i) run_params_match(from_json(existing$params_json[i]), params, existing$taxid[i], lstRow$taxid),
                      seq_len(nrow(existing)))
      if (length(match) > 0) { reused[[length(reused) + 1]] <- existing$id[match[[1]]]; next }

      runId <- insert_run(key, toolId, params, params$backgroundList)
      submit_run(ws, runId, list(kind = "ora", toolId = toolId, lst = lst, params = params,
                                 bg = if (identical(params$backgroundMode, "custom")) bg else NULL))
      created[[length(created) + 1]] <- runId
    }

    for (sid in sourceIds) {
      src <- get_source_sets(ws, sid)
      cfg <- perTool[["custom"]] %||% list()
      thr <- suppressWarnings(as.numeric(cfg$threshold %||% b$threshold %||% 0.05)); if (is.na(thr) || thr <= 0 || thr > 1) thr <- 0.05
      corr <- as.character(cfg$correction %||% "BH"); if (!corr %in% c("BH", "bonferroni", "none")) corr <- "BH"
      bgRow <- if (nzchar(as.character(cfg$backgroundListId %||% ""))) get_list(ws, as.character(cfg$backgroundListId)) else NULL
      params <- list(geneListName = lstRow$name, datasources = src$name, threshold = thr, namespace = "USERINPUT",
                     metric = corr, backgroundMode = if (is.null(bgRow)) "genome" else "custom",
                     backgroundList = if (is.null(bgRow)) NULL else bgRow$name, customSourceId = sid)
      runId <- insert_run("custom", paste0("Custom: ", src$name), params, params$backgroundList)
      submit_run(ws, runId, list(kind = "gmt", lst = lst, sets = src$sets, sourceName = src$name,
                                 threshold = thr, correction = corr,
                                 bgIds = if (is.null(bgRow)) NULL else from_json(bgRow$ids_json)))
      created[[length(created) + 1]] <- runId
    }

    res$status <- if (length(created) > 0) 202 else 200
    list(created = created, reused = reused, skipped = skipped)
  })
}

#* One run with status
#* @get /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, { ws <- require_workspace(req); run_row_to_obj(get_run_row(ws, id)) })
}

#* Remove a run (cancels it if still running)
#* @delete /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    get_run_row(ws, id)
    cancel_run(ws, id)
    db_exec("DELETE FROM runs WHERE ws = ? AND id = ?", list(ws, id))
    forget_run_cache(ws, id)
    list(ok = TRUE)
  })
}

#* Run details: parameters, sources, input mapping, inputs without hits, messages
#* @get /<id>/details
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    r <- get_run_row(ws, id)
    obj <- run_row_to_obj(r)
    if (!identical(r$status, "complete")) return(c(obj, list(details = NULL)))
    out <- run_output(ws, id)
    ct <- out$conversionTable
    inputs <- if (!is.null(ct)) unique(as.character(ct$input)) else character(0)
    lstIds <- tryCatch(from_json(get_list(ws, r$list_name)$ids_json), flame_api_error = function(e) inputs)
    mapped <- intersect(lstIds, inputs)
    c(obj, list(
      details = out$details,
      backgroundSize = out$backgroundSize,
      sourcesWithHits = as.list(if (!is.null(out$results)) sort(unique(as.character(out$results$Source))) else character(0)),
      inputSize = length(lstIds),
      mappedCount = length(mapped),
      unmapped = as.list(setdiff(lstIds, inputs)),
      noHitAnalytes = as.list(out$noHitAnalytes %||% character(0)),
      significantTerms = out$resultCount %||% 0L
    ))
  })
}

#* Data for a result view.
#* Body: { view, filters:{sources, search, pvalueMax, termSizeMin/Max, intersectionMin/Max,
#*        termCoverageMin/Max}, top, sortBy, settings:{layout, similarityCutoff, minShared} }
#* @post /<id>/view
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    out <- run_output(ws, id)
    view <- as.character(b$view %||% "table")
    if (!view %in% VIEW_IDS) api_abort(400, "Unknown view.")
    top <- b$top %||% 20L
    if (identical(top, "all")) top <- .Machine$integer.max
    build_view(out$results, view, b$filters %||% list(), top, b$sortBy %||% "-log10Pvalue", b$settings %||% list())
  })
}

#* Export the current network view to Arena3Dweb (3D). Same body as /view.
#* @post /<id>/arena3d
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    view <- as.character(b$view %||% "network-tg")
    if (!view %in% c("network-tg", "network-tt", "network-gg")) api_abort(400, "Arena3Dweb export is available for network views.")
    out <- run_output(ws, id)
    edgelist <- arena_edgelist_for_view(out$results, view, b$filters %||% list(), b$top %||% 20L,
                                        b$sortBy %||% "-log10Pvalue", b$settings %||% list())
    if (is.null(edgelist) || nrow(edgelist) == 0) api_abort(422, "The current view has no edges to export.")
    result <- arena_export(edgelist)
    if (!isTRUE(result$success) || !grepl("^https://", result$url %||% ""))
      api_abort(502, "Arena3Dweb did not accept the network. Please try again later.")
    list(url = result$url)
  })
}
