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
    paradigm <- if (identical(toupper(as.character(b$paradigm %||% "ORA")), "GSEA")) ParadigmId$GSEA else ParadigmId$ORA
    if (paradigm == ParadigmId$GSEA && is.null(lst$scores))
      api_abort(400, "GSEA needs a ranked list (IDs with scores). Create one from a volcano table or upload IDs with scores.")
    toolKeys <- unique(tolower(as.character(unlist(b$tools))))
    sourceIds <- unique(as.character(unlist(b$sourceIds)))
    if (length(toolKeys) + length(sourceIds) == 0) api_abort(400, "Select at least one tool or custom source.")

    total <- db_query("SELECT COUNT(*) AS n FROM runs WHERE ws = ?", list(ws))$n
    if (total + length(toolKeys) + length(sourceIds) > FLAME_LIMITS$maxRuns)
      api_abort(409, sprintf("A workspace can hold at most %d runs. Remove some runs first.", FLAME_LIMITS$maxRuns))
    if (active_job_count(ws) + length(toolKeys) + length(sourceIds) > FLAME_LIMITS$maxActiveJobs)
      api_abort(429, sprintf("At most %d runs can be queued or running at once in a workspace. Wait for some to finish and try again.", FLAME_LIMITS$maxActiveJobs))

    perTool <- b$perTool %||% list()
    created <- list(); reused <- list(); skipped <- list()

    insert_run <- function(toolKey, toolId, params, bgName, runParadigm = ParadigmId$ORA) {
      ids <- next_run_ids(ws, toolKey, toolId)
      db_exec("INSERT INTO runs (id, ws, seq, display, tool, paradigm, list_id, list_name, bg_list_name,
               taxid, params_json, status, created) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'queued', ?)",
              list(ids$id, ws, ids$seq, ids$display, toolId, runParadigm, lstRow$id, lstRow$name,
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
      if (!paradigm %in% (TOOLS[[toolId]]$paradigms %||% ParadigmId$ORA)) {
        skipped[[length(skipped) + 1]] <- list(tool = key, reason = sprintf("%s does not offer %s.", toolId, paradigm))
        next
      }
      cfg <- perTool[[key]] %||% list()
      if (paradigm == ParadigmId$GSEA) {
        params <- build_run_params(toolId, lst, b$datasources, cfg$threshold %||% b$threshold %||% 0.05, cfg$correction, NULL)
        params$paradigm <- ParadigmId$GSEA
        existing <- db_query("SELECT id, taxid, params_json FROM runs WHERE ws = ? AND tool = ? AND paradigm = 'GSEA' AND status != 'error'", list(ws, toolId))
        match <- Filter(function(i) run_params_match(from_json(existing$params_json[i]), params, existing$taxid[i], lstRow$taxid), seq_len(nrow(existing)))
        if (length(match) > 0) { reused[[length(reused) + 1]] <- existing$id[match[[1]]]; next }
        runId <- insert_run(paste0(key, "-gsea"), toolId, params, NULL, ParadigmId$GSEA)
        submit_run(ws, runId, list(kind = "gsea", lst = lst, params = params))
        created[[length(created) + 1]] <- runId
        next
      }
      bgRow <- NULL
      if (nzchar(as.character(cfg$backgroundListId %||% ""))) bgRow <- get_list(ws, as.character(cfg$backgroundListId))
      bg <- if (!is.null(bgRow)) list_core(bgRow) else NULL
      params <- build_run_params(toolId, lst, b$datasources, cfg$threshold %||% b$threshold %||% 0.05,
                                 cfg$correction, bg)

      existing <- db_query("SELECT id, taxid, params_json, status FROM runs WHERE ws = ? AND tool = ? AND paradigm = 'ORA' AND status != 'error'",
                           list(ws, toolId))
      match <- Filter(function(i) run_params_match(from_json(existing$params_json[i]), params, existing$taxid[i], lstRow$taxid),
                      seq_len(nrow(existing)))
      if (length(match) > 0) { reused[[length(reused) + 1]] <- existing$id[match[[1]]]; next }

      runId <- insert_run(key, toolId, params, params$backgroundList)
      submit_run(ws, runId, list(kind = "ora", toolId = toolId, lst = lst, params = params,
                                 bg = if (identical(params$backgroundMode, "custom")) bg else NULL))
      created[[length(created) + 1]] <- runId
    }

    # Custom gene sets with a ranked list: GSEA (clusterProfiler/fgsea) on the user's own signatures.
    if (paradigm == ParadigmId$GSEA) {
      for (sid in sourceIds) {
        src <- get_source_sets(ws, sid)
        cfg <- perTool[["custom"]] %||% list()
        corr <- as.character(cfg$correction %||% "BH"); if (!corr %in% c("BH", "bonferroni", "none")) corr <- "BH"
        params <- list(geneListName = lstRow$name, datasources = src$name, threshold = 1, namespace = "ENTREZGENE_ACC",
                       metric = corr, backgroundMode = "genome", customSourceId = sid, paradigm = ParadigmId$GSEA)
        runId <- insert_run("custom-gsea", paste0("Custom: ", src$name), params, NULL, ParadigmId$GSEA)
        submit_run(ws, runId, list(kind = "gsea", lst = lst, params = params, sets = src$sets))
        created[[length(created) + 1]] <- runId
      }
      sourceIds <- character(0)
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

#* GSEA enrichment plot data for up to 12 terms of a GSEA run. Body: { terms: [termId, ...] }
#* Returns the ranking (thinned for the ranked-score panel) and, per term, the running
#* enrichment score curve, hit positions, peak and leading edge.
#* @post /<id>/gsea-plot
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    r <- get_run_row(ws, id)
    if (!identical(r$paradigm, "GSEA")) api_abort(400, "Enrichment plots are for GSEA runs.")
    out <- run_output(ws, id)
    rk <- out$gseaRanking
    if (is.null(rk)) api_abort(409, "This GSEA run was made before Flame kept the ranking. Run it again to draw enrichment plots.")
    terms <- utils::head(unique(as.character(unlist(body_of(req)$terms))), 12)
    if (!length(terms)) api_abort(400, "Choose at least one term.")
    resRows <- out$results
    n <- length(rk$scores)
    keep <- unique(round(seq(1, n, length.out = min(n, 1500))))
    zero <- which(diff(sign(rk$scores)) != 0)[1]
    list(
      n = n, scoreType = rk$scoreType,
      metric = list(x = keep, y = round(rk$scores[keep], 4)),
      zeroCross = if (is.na(zero)) NULL else zero,
      terms = Filter(Negate(is.null), lapply(terms, function(t) {
        row <- resRows[resRows$Term_ID == t, , drop = FALSE]
        set <- rk$sets[[t]]
        if (!nrow(row) || !length(set)) return(NULL)
        rs <- gsea_running_score(rk$scores, rk$genes %in% set)
        list(id = t, name = row$Function[1], source = row$Source[1], nes = row$NES[1], es = rs$es,
             padj = row$`P-value`[1], setSize = length(set), x = rs$x, y = rs$y, hits = rs$hits, peak = rs$peak,
             hitGenes = as.list(rk$symbols[rs$hits]),
             leadingEdge = as.list(rk$symbols[rs$leadingEdge]))
      })))
  })
}

#* Data for a result view.
#* Body: { view, filters:{sources, search, pvalueMax, termSizeMin/Max, intersectionMin/Max,
#*        termCoverageMin/Max, reduce, reduceMethod ("genes" | "semantic" | "revigo"),
#*        reduceCutoff, reduceMeasure (REVIGO: SIMREL | LIN | RESNIK | JIANG)},
#*        top, sortBy, settings:{layout, similarityCutoff, minShared} }
#* @post /<id>/view
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    out <- run_output(ws, id)
    taxid <- get_run_row(ws, id)$taxid
    view <- as.character(b$view %||% "table")
    if (!view %in% VIEW_IDS) api_abort(400, "Unknown view.")
    top <- b$top %||% 20L
    if (identical(top, "all")) top <- .Machine$integer.max
    filters <- b$filters %||% list()
    build <- function() build_view(out$results, view, filters, top, b$sortBy %||% "-log10Pvalue", b$settings %||% list(), taxid = taxid)
    # REVIGO is a web service: ask it from a background worker so the server keeps answering others.
    pending <- revigo_pending(out$results, filters, taxid)
    if (is.null(pending)) return(build())
    revigo_async(pending, build, res)
  })
}

# Exports reuse REVIGO's answer for the view on screen; they never ask REVIGO themselves.
.needs_revigo_view <- function(results, filters, taxid) {
  if (!is.null(revigo_pending(results, filters %||% list(), taxid)))
    api_abort(409, "Show this view with REVIGO first, then export it.")
}

#* Open the current view in Arena3D (3D multilayer network, arena3d.org). Same body as /view.
#* Works for network views and for the term–gene heatmap / Sankey (terms by source as layers + genes).
#* @post /<id>/arena3d
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    view <- as.character(b$view %||% "network-tg")
    if (!view %in% c("network-tg", "network-tt", "network-gg", "heatmap-tg")) api_abort(400, "Arena3D export is available for network views.")
    out <- run_output(ws, id)
    taxid <- get_run_row(ws, id)$taxid
    .needs_revigo_view(out$results, b$filters, taxid)
    body <- tryCatch(arena3d_payload_for_view(out$results, view, b$filters %||% list(), b$top %||% 20L,
                                              b$sortBy %||% "-log10Pvalue", b$settings %||% list(), taxid = taxid),
                     error = function(e) api_abort(422, conditionMessage(e)))
    # Arena3D is a web service: send the network from the external-service workers.
    fetch_async(res, quote(arena3d_open(body)), list(body = body))
  })
}

#* Open the current network view in NORMA (network annotation viewer). Same body as /view.
#* @post /<id>/norma
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    view <- as.character(b$view %||% "network-tg")
    if (!view %in% c("network-tg", "network-tt", "network-gg")) api_abort(400, "NORMA export is available for network views.")
    r <- get_run_row(ws, id)
    out <- run_output(ws, id)
    .needs_revigo_view(out$results, b$filters, r$taxid)
    payload <- tryCatch(norma_payload_for_view(out$results, view, b$filters %||% list(), b$top %||% 20L,
                                               b$sortBy %||% "-log10Pvalue", b$settings %||% list(),
                                               name = paste(r$tool, r$list_name), taxid = r$taxid),
                        error = function(e) api_abort(422, conditionMessage(e)))
    fetch_async(res, quote(list(url = norma_export(payload))), list(payload = payload))
  })
}

#* Links from one enriched term to pathway databases (KEGG map with the genes
#* coloured, Reactome with genes flagged, WikiPathways, GO, HPO, searches).
#* Body: { termId }
#* @post /<id>/pathway-links
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    termId <- as.character(body_of(req)$termId %||% "")[1]
    r <- get_run_row(ws, id)
    out <- run_output(ws, id)
    df <- out$results
    row <- df[as.character(df$Term_ID_noLinks %||% df$Term_ID) == termId, , drop = FALSE]
    if (!nrow(row)) api_abort(404, "Term not found in this run.")
    genes <- .split_hits(row$`Positive Hits`[1])
    # Some tools report their own IDs (e.g. STRING 9606.ENSP...): map back to the user's IDs.
    ct <- out$conversionTable
    if (!is.null(ct) && nrow(ct) && all(c("input", "target") %in% names(ct))) {
      back <- ct$input[match(genes, ct$target)]
      byName <- if ("name" %in% names(ct)) ct$input[match(genes, ct$name)] else NA
      genes <- unique(ifelse(!is.na(back), back, ifelse(!is.na(byName), byName, sub("^[0-9]+\\.", "", genes))))
    }
    scores <- NULL
    if (!is.na(r$list_id %||% NA)) {
      l <- db_query("SELECT ids_json, scores_json FROM lists WHERE ws = ? AND id = ?", list(ws, r$list_id))
      if (nrow(l) && !is.na(l$scores_json) && nzchar(l$scores_json)) {
        ids <- as.character(unlist(from_json(l$ids_json))); sc <- as.numeric(unlist(from_json(l$scores_json)))
        if (length(ids) == length(sc)) scores <- as.list(stats::setNames(sc, ids)[intersect(genes, ids)])
      }
    }
    pathway_links(list(id = termId, source = row$Source[1], name = row$Function[1]), genes, r$taxid, scores)
  })
}

#* Functional themes of a run: overlapping terms grouped by shared genes, with keywords.
#* Body: { filters?, cutoff? }
#* @post /<id>/themes
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    out <- run_output(ws, id)
    cutoff <- suppressWarnings(as.numeric(b$cutoff %||% 0.35)); if (!is.finite(cutoff)) cutoff <- 0.35
    tryCatch(run_themes(out$results, b$filters %||% list(), max(0.05, min(0.95, cutoff))), error = function(e) api_abort(422, conditionMessage(e)))
  })
}
