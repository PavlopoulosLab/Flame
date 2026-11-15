# /api/lists — analyte lists in the workspace (X-Flame-Workspace header)

#* All lists (newest first)
#* @get /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    rows <- db_query("SELECT * FROM lists WHERE ws = ? ORDER BY created DESC", list(ws))
    lapply(seq_len(nrow(rows)), function(i) list_row_to_obj(rows[i, ]))
  })
}

#* Create a list from IDs (array) or text (pasted / file contents)
#* Body: { name, ids? | text?, skipFirstRow?, organismTaxId, analyteType, idFormat, meta?, uniqueName? }
#* @post /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    ids <- if (!is.null(b$ids)) unlist(b$ids) else {
      text <- as.character(b$text %||% "")[1]
      check_text_size(text)
      parse_id_text(text, isTRUE(b$skipFirstRow))$ids
    }
    meta <- b$meta %||% list()
    if (!is.list(meta)) meta <- list()
    meta <- meta[intersect(names(meta), c("source", "sourceLists", "mode", "derivedFrom", "method"))]
    res$status <- 201
    create_list(ws, b$name, ids, b$organismTaxId %||% 9606L, b$analyteType %||% "GENE",
                b$idFormat %||% "USERINPUT", meta, uniquify = isTRUE(b$uniqueName))
  })
}

#* A list with its IDs
#* @get /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    list_row_to_obj(get_list(ws, id), includeIds = TRUE)
  })
}

#* Download a list as CSV
#* @get /<id>/csv
#* @serializer contentType list(type="text/csv; charset=utf-8")
function(req, res, id) {
  tryCatch({
    ws <- require_workspace(req)
    r <- get_list(ws, id)
    ids <- from_json(r$ids_json)
    fname <- paste0(gsub("[^A-Za-z0-9_.-]+", "_", r$name), ".csv")
    res$setHeader("Content-Disposition", sprintf("attachment; filename=\"%s\"", fname))
    # Prefix cells that a spreadsheet could interpret as formulas.
    safe <- ifelse(grepl("^[=+@-]", ids), paste0("'", ids), ids)
    paste0("id\n", paste(safe, collapse = "\n"), "\n")
  }, flame_api_error = function(e) { res$status <- e$status; e$message })
}

#* Rename a list. Body: { name }
#* @patch /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    r <- get_list(ws, id)
    name <- clean_name(body_of(req)$name, "list name")
    if (!identical(name, r$name) &&
        nrow(db_query("SELECT 1 FROM lists WHERE ws = ? AND name = ?", list(ws, name))) > 0)
      api_abort(409, sprintf("A list named \"%s\" already exists.", name))
    db_exec("UPDATE lists SET name = ? WHERE id = ?", list(name, r$id))
    db_exec("UPDATE runs SET list_name = ? WHERE ws = ? AND list_name = ?", list(name, ws, r$name))
    db_exec("UPDATE runs SET bg_list_name = ? WHERE ws = ? AND bg_list_name = ?", list(name, ws, r$name))
    list_row_to_obj(db_query("SELECT * FROM lists WHERE id = ?", list(r$id))[1, ])
  })
}

#* Delete a list (runs made from it are kept)
#* @delete /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    r <- get_list(ws, id)
    db_exec("DELETE FROM lists WHERE id = ?", list(r$id))
    list(ok = TRUE)
  })
}
