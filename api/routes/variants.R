# /api/variants — variant analysis (SNPs, small variants, GWAS summary statistics)
# Annotation runs in a background job (Ensembl VEP + trait associations).

.va_row <- function(r) list(
  id = r$id, name = r$name, organismTaxId = r$taxid, status = r$status,
  error = if (is.na(r$error)) NULL else r$error, inputCount = r$input_count,
  created = r$created, finished = if (is.na(r$finished)) NULL else r$finished)

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
        "7 140753336 . A T", "17:7674220:C:T", "ENST00000366667:c.803C>T"), collapse = "\n"))
  } else if (kind == "gwas") {
      list(text = paste(c("SNP\tCHR\tBP\tP",
        "rs429358\t19\t44908684\t1.2e-120", "rs7412\t19\t44908822\t3.4e-45", "rs6857\t19\t44888997\t2.1e-60",
        "rs11136000\t8\t27607002\t4.5e-12", "rs3851179\t11\t86157598\t6.0e-11", "rs744373\t2\t127137039\t2.3e-10",
        "rs3764650\t19\t1046521\t8.1e-10", "rs610932\t11\t60171834\t1.7e-09", "rs3865444\t19\t51224706\t3.0e-08",
        "rs9349407\t6\t47485953\t5.2e-08", "rs11218343\t11\t121564878\t9.9e-08", "rs10498633\t14\t92460608\t2.2e-06",
        "rs1476679\t7\t100406823\t4.6e-06", "rs10948363\t6\t41034000\t7.0e-06", "rs17125944\t14\t52924962\t1.1e-05",
        "rs7274581\t20\t56423488\t3.3e-05", "rs2718058\t7\t37841534\t5.0e-05", "rs983392\t11\t60156035\t8.8e-05"), collapse = "\n"))
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
    lapply(seq_len(nrow(rows)), function(i) .va_row(rows[i, ]))
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
    out <- .va_row(r[1, ])
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
    tryCatch(arena3d_open(body), error = function(e) api_abort(502, conditionMessage(e)))
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
