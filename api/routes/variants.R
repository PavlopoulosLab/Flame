# /api/variants — variant analysis (SNPs, small variants, GWAS summary statistics)
# Annotation runs in a background job (Ensembl VEP + trait associations).

.va_row <- function(r, ws = NULL) list(
  id = r$id, name = r$name, organismTaxId = r$taxid, status = r$status,
  error = if (is.na(r$error)) NULL else r$error, inputCount = r$input_count,
  created = r$created, finished = if (is.na(r$finished)) NULL else r$finished,
  progress = if (!is.null(ws) && r$status %in% c("queued", "running")) job_progress(ws, r$id, "variants") else NULL)

#* Species available for variant annotation
#* @get /species
#* @serializer flamejson
function(res) {
  api_handle(res, lapply(names(VARIANT_SPECIES), function(t) list(taxid = as.integer(t), name = .organism_label(t))))
}

.variant_example <- function(kind) {
  if (kind == "variants") {
      list(text = paste(c(
        "rs429358", "rs7412", "rs1801133", "rs6025", "rs1800562", "rs334", "rs113993960", "rs80357906",
        "rs121913529", "rs28934578", "rs1042522", "rs1799853", "rs1057910", "rs4149056", "rs12248560",
        "7 140753336 . A T", "17:7674220:C:T", "ENST00000366667:c.803C>T"), collapse = "\n"),
        source = "Well-studied clinical variants (e.g. APOE e4, Factor V Leiden, HFE C282Y, sickle-cell HBB, BRAF V600E) in rsID, VCF, chr:pos:ref:alt and HGVS forms.")
  } else if (kind == "gwas") {
      gw <- .example_file("ad_gwas_bellenguez2022.tsv")
      list(text = .table_text(gw[, c("SNP", "CHR", "BP", "P")]), source = EXAMPLE_SOURCES$gwas)
  } else NULL
}

#* Example inputs: variants (rsIDs, VCF lines, HGVS) or GWAS summary statistics
#* @get /examples/<kind>
#* @serializer flamejson
function(res, kind) {
  api_handle(res, {
    ex <- .variant_example(kind)
    if (is.null(ex)) api_abort(404, "Unknown example.")
    ex
  })
}

#* Example variant / GWAS input as a downloadable file
#* @get /examples/<kind>/file
#* @serializer contentType list(type="text/plain; charset=utf-8")
function(res, kind) {
  ex <- .variant_example(kind)
  if (is.null(ex)) { res$status <- 404; return("Unknown example") }
  res$setHeader("Content-Disposition", sprintf("attachment; filename=\"%s\"", if (kind == "gwas") "flame_example_gwas.tsv" else "flame_example_variants.txt"))
  ex$text
}

#* Variant analyses in this workspace
#* @get /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    rows <- db_query("SELECT id, name, taxid, status, error, input_count, created, finished FROM variant_analyses WHERE ws = ? ORDER BY created DESC", list(ws))
    lapply(seq_len(nrow(rows)), function(i) .va_row(rows[i, ], ws))
  })
}

#* Start a variant analysis. Body: { name?, text, organismTaxId }
#* @post /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    text <- paste(as.character(unlist(b$text %||% "")), collapse = "\n")
    if (nchar(text, type = "bytes") > 2 * 1024^2) api_abort(413, "The input is larger than the 2 MB limit.")
    taxid <- suppressWarnings(as.integer(b$organismTaxId %||% 9606L))
    if (is.na(taxid) || is.null(VARIANT_SPECIES[as.character(taxid)]) || is.na(VARIANT_SPECIES[as.character(taxid)]))
      api_abort(400, "Variant annotation is available for human and a few model organisms only.")
    parsed <- tryCatch(parse_variant_input(text), error = function(e) api_abort(400, conditionMessage(e)))
    if (active_job_count(ws) >= FLAME_LIMITS$maxActiveJobs) api_abort(429, "Too many analyses are still running. Please wait.")
    id <- new_object_id()
    name <- clean_name(b$name %||% if (identical(attr(parsed, "mode"), "gwas")) "GWAS summary statistics" else "Variants", "analysis name")
    db_exec("INSERT INTO variant_analyses (id, ws, name, taxid, status, input_count, created) VALUES (?, ?, ?, ?, 'queued', ?, ?)",
            list(id, ws, name, taxid, nrow(parsed), now_s()))
    submit_variant_job(ws, id, text, taxid)
    res$status <- 202
    list(id = id, mode = attr(parsed, "mode"), inputCount = nrow(parsed), skipped = attr(parsed, "skipped") %||% 0L,
         truncated = nrow(parsed) > VARIANT_MAX)
  })
}

#* A variant analysis: status, and when complete the annotated variants, traits, genes and summaries
#* @get /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    r <- db_query("SELECT * FROM variant_analyses WHERE ws = ? AND id = ?", list(ws, id))
    if (!nrow(r)) api_abort(404, "Analysis not found.")
    out <- .va_row(r[1, ], ws)
    if (identical(r$status, "complete")) {
      out$result <- unserialize(r$result_blob[[1]])
      out$result$variants$impact <- as.character(out$result$variants$impact)
    }
    out
  })
}

#* Open a variant analysis in Arena3D: layers Variants → Genes → Traits
#* @post /<id>/arena3d
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    r <- db_query("SELECT status, result_blob FROM variant_analyses WHERE ws = ? AND id = ?", list(ws, id))
    if (!nrow(r)) api_abort(404, "Analysis not found.")
    if (!identical(r$status, "complete")) api_abort(409, "The analysis has not finished.")
    result <- unserialize(r$result_blob[[1]])
    body <- tryCatch(arena3d_payload_for_variants(result), error = function(e) api_abort(422, conditionMessage(e)))
    fetch_async(res, quote(arena3d_open(body)), list(body = body))   # web service: from the external-service workers
  })
}

#* Delete a variant analysis
#* @delete /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    cancel_run(ws, paste("variants", id))
    db_exec("DELETE FROM variant_analyses WHERE ws = ? AND id = ?", list(ws, id))
    list(ok = TRUE)
  })
}
