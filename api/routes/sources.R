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

#* Upload a GMT file. Body: { name, text }
#* @post /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    text <- paste(as.character(unlist(b$text %||% "")), collapse = "\n")
    sets <- tryCatch(parse_gmt(text, FLAME_LIMITS$maxGmtTerms), error = function(e) api_abort(400, conditionMessage(e)))
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
