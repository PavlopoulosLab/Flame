# /api/analyses — background analyses on a list or input (regulators, drugs,
# network modules, robustness, pathway activity). Poll GET /api/analyses/<id>.

.analysis_genes <- function(ws, b) {
  r <- get_list(ws, as.character(b$listId %||% ""))
  list(ids = as.character(unlist(from_json(r$ids_json))), taxid = r$taxid, name = r$name, row = r)
}

# Metabolite enrichment becomes an ordinary run, so every result view and comparison works.
.finish_metabolites <- function(ws, st) {
  if (!identical(st$type, "metabolites") || !identical(st$status, "complete") || !is.null(st$runId)) return(st)
  out <- run_output_from_json(st$result)
  if (!is.null(out$results)) out$results <- clean_result_links(out$results)
  runId <- import_completed_run(ws, list(tool = "KEGG compounds", toolKey = "metabolites", paradigm = "ORA",
                                         listName = st$meta$name %||% "Metabolites", organismTaxId = st$meta$taxid %||% 9606L), out)
  db_exec("UPDATE analyses SET run_id = ? WHERE ws = ? AND id = ?", list(runId, ws, st$id))
  st$runId <- runId
  st
}

#* Start an analysis. Body: { type, listId?, ... }
#* type: tf | kinase (regulators of a list)
#* @post /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    type <- as.character(b$type %||% "")
    if (active_job_count(ws) + active_task_count(ws) >= FLAME_LIMITS$maxActiveJobs) api_abort(429, "Too many analyses are running. Please wait.")
    id <- switch(type,
      tf = , kinase = {
        g <- .analysis_genes(ws, b)
        if (!identical(as.integer(g$taxid), 9606L)) api_abort(400, "Regulator analysis uses human gene symbols; convert or map orthologs to human first.")
        label <- paste(if (type == "tf") "Transcription factors" else "Kinases", "of", g$name)
        submit_task(ws, type, "upstream_regulators", list(genes = g$ids, kind = type), label, b$listId)
      },
      drugs = {
        up <- if (nzchar(as.character(b$upListId %||% ""))) .analysis_genes(ws, list(listId = b$upListId)) else NULL
        down <- if (nzchar(as.character(b$downListId %||% ""))) .analysis_genes(ws, list(listId = b$downListId)) else NULL
        if (is.null(up) && is.null(down)) api_abort(400, "Choose a list of up- and/or down-regulated genes.")
        if (any(vapply(list(up, down), function(x) !is.null(x) && !identical(as.integer(x$taxid), 9606L), logical(1))))
          api_abort(400, "Drug signatures (LINCS L1000) are human; map orthologs to human first.")
        label <- paste0(if (identical(b$kind, "crispr")) "CRISPR knock-outs" else "Drugs", " for ",
                        paste(c(if (!is.null(up)) paste(up$name, "(up)"), if (!is.null(down)) paste(down$name, "(down)")), collapse = " / "))
        submit_task(ws, type, "drug_connectivity", list(up = up$ids %||% character(0), down = down$ids %||% character(0),
                                                         kind = if (identical(b$kind, "crispr")) "crispr" else "chemical"),
                    label, as.character(b$upListId %||% b$downListId %||% ""))
      },
      modules = {
        g <- .analysis_genes(ws, b)
        if (length(g$ids) > 500) api_abort(400, "STRING networks are limited to 500 proteins.")
        score <- suppressWarnings(as.integer(b$score %||% 700L)); if (is.na(score)) score <- 700L
        label <- paste("Network modules of", g$name)
        submit_task(ws, type, "ppi_modules", list(ids = g$ids, taxid = g$taxid, score = score), label, b$listId, list(score = score))
      },
      robustness = {
        g <- .analysis_genes(ws, b)
        if (is.null(MSIGDB_SPECIES[[as.character(g$taxid)]])) api_abort(400, "Robustness checks use MSigDB gene sets, available for the main model organisms.")
        src <- intersect(as.character(unlist(b$sources %||% c("GO:BP", "KEGG", "REAC"))), names(TOOLS[[ToolId$CLUSTERPROFILER]]$datasourceCodes))
        if (!length(src)) src <- c("GO:BP", "KEGG", "REAC")
        label <- paste0("Robustness of ", g$name, " (", paste(src, collapse = ", "), ")")
        submit_task(ws, type, "enrichment_robustness", list(ids = g$ids, taxid = g$taxid, sources = src), label, b$listId, list(sources = src))
      },
      metabolites = {
        text <- paste(as.character(unlist(b$text %||% "")), collapse = "\n")
        check_text_size(text)
        ids <- unique(trimws(unlist(strsplit(text, "[\r\n,;\t]+"))))
        ids <- ids[nzchar(ids)]
        if (length(ids) < 3) api_abort(400, "Give at least 3 metabolites.")
        if (length(ids) > 3000) api_abort(400, "At most 3,000 metabolites.")
        taxid <- suppressWarnings(as.integer(b$organismTaxId %||% 9606L)); if (is.na(taxid)) taxid <- 9606L
        label <- paste("Metabolite pathways of", clean_name(b$name %||% "Metabolites", "list name"))
        submit_task(ws, type, "metabolite_enrichment", list(ids = ids, taxid = taxid,
                                                             threshold = suppressWarnings(as.numeric(b$threshold %||% 0.05))),
                    label, NULL, list(name = clean_name(b$name %||% "Metabolites", "list name"), taxid = taxid))
      },
      api_abort(400, "Unknown analysis type."))
    res$status <- 202
    list(id = id, type = type, label = task_state(ws, id, withResult = FALSE)$label)
  })
}

#* Analyses of this workspace, newest first (without results)
#* @get /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    ids <- db_query("SELECT id FROM analyses WHERE ws = ? ORDER BY created DESC", list(ws))$id
    lapply(ids, function(k) {
      st <- task_state(ws, k, withResult = FALSE)
      if (identical(st$type, "metabolites") && identical(st$status, "complete") && is.null(st$runId))
        st <- .finish_metabolites(ws, task_state(ws, k))
      st$result <- NULL
      st
    })
  })
}

#* Status and result of an analysis
#* @get /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    st <- task_state(ws, id)
    if (is.null(st)) api_abort(404, "Analysis not found.")
    st <- .finish_metabolites(ws, st)
    if (!is.null(st$runId)) st$result <- NULL
    st
  })
}

#* Delete an analysis (stops it if it is still running)
#* @delete /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    t <- .flame_tasks[[id]]
    if (!is.null(t) && identical(t$ws, ws)) { try(mirai::stop_mirai(t$m), silent = TRUE); unlink(t$progress); rm(list = id, envir = .flame_tasks) }
    db_exec("DELETE FROM analyses WHERE ws = ? AND id = ?", list(ws, id))
    list(ok = TRUE)
  })
}
