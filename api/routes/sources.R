# /api/sources — custom gene-set sources (GMT) for this workspace

#* Custom sources
#* @get /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    rows <- db_query("SELECT id, name, term_count, created FROM sources WHERE ws = ? ORDER BY created", list(ws))
    lapply(seq_len(nrow(rows)), function(i) list(id = rows$id[i], name = rows$name[i],
                                                termCount = rows$term_count[i], created = rows$created[i]))
  })
}

#* Upload a GMT file, or one signature. Body: { name, text } (GMT) or { name, genes } (one gene set:
#* IDs one per line or separated by commas, tabs or spaces)
#* @post /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    sets <- if (!is.null(b$genes)) {
      genesText <- paste(as.character(unlist(b$genes)), collapse = "\n")
      check_text_size(genesText)
      ids <- parse_id_text(genesText)$ids
      if (length(ids) < 5) api_abort(400, "A signature needs at least 5 genes.")
      if (length(ids) > 5000) api_abort(400, "A signature can have at most 5,000 genes.")
      name <- clean_name(b$name %||% "My signature", "signature name")
      list(list(id = gsub("[^A-Za-z0-9_.:-]+", "_", name), name = name, genes = ids))
    } else {
      text <- paste(as.character(unlist(b$text %||% "")), collapse = "\n")
      tryCatch(parse_gmt(text, FLAME_LIMITS$maxGmtTerms), error = function(e) api_abort(400, conditionMessage(e)))
    }
    res$status <- 201
    save_source(ws, b$name %||% "Custom source", sets)
  })
}

#* Delete a custom source
#* @delete /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    db_exec("DELETE FROM sources WHERE ws = ? AND id = ?", list(ws, id))
    list(ok = TRUE)
  })
}
