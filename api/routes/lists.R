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
    scores <- if (!is.null(b$scores)) unlist(b$scores) else NULL
    ids <- if (!is.null(b$ids)) unlist(b$ids) else {
      text <- as.character(b$text %||% "")[1]
      check_text_size(text)
      if (isTRUE(b$ranked)) {
        parsed <- parse_ranked_text(text, isTRUE(b$skipFirstRow))
        scores <- parsed$scores
        parsed$ids
      } else parse_id_text(text, isTRUE(b$skipFirstRow))$ids
    }
    meta <- b$meta %||% list()
    if (!is.list(meta)) meta <- list()
    meta <- meta[intersect(names(meta), c("source", "sourceLists", "mode", "derivedFrom", "method", "direction", "pairId", "cutoffs"))]
    # Up/down lists from one volcano table share a pairId, so their runs can be plotted together.
    if (!is.null(meta$direction) && !identical(as.character(meta$direction)[1], "up") && !identical(as.character(meta$direction)[1], "down")) meta$direction <- NULL
    if (!is.null(meta$pairId)) meta$pairId <- substr(gsub("[^A-Za-z0-9_-]", "", as.character(meta$pairId)[1]), 1, 40)
    if (!is.null(meta$cutoffs)) meta$cutoffs <- list(log2fc = suppressWarnings(as.numeric(meta$cutoffs$log2fc)[1]),
                                                     pvalue = suppressWarnings(as.numeric(meta$cutoffs$pvalue)[1]))
    res$status <- 201
    create_list(ws, b$name, ids, b$organismTaxId %||% 9606L, b$analyteType %||% "GENE",
                b$idFormat %||% "USERINPUT", meta, uniquify = isTRUE(b$uniqueName), scores = scores)
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
    if (!is.null(r$scores_json) && !is.na(r$scores_json) && nzchar(r$scores_json)) {
      sc <- from_json(r$scores_json)
      return(paste0("id,score\n", paste(safe, sc, sep = ",", collapse = "\n"), "\n"))
    }
    paste0("id\n", paste(safe, collapse = "\n"), "\n")
  }, flame_api_error = function(e) { res$status <- e$status; e$message })
}

#* Rename a list and/or replace its contents.
#* Body: { name?, text? | ids? (+ scores? for a ranked list) }
#* `text` is parsed like pasted input: one ID per line (or "ID score" pairs for a ranked list).
#* Runs already made from the list keep their results.
#* @patch /<id>
#* @serializer flamejson
function(req, res, id) {
  api_handle(res, {
    ws <- require_workspace(req)
    r <- get_list(ws, id)
    b <- body_of(req)
    if (is.null(b$name) && is.null(b$text) && is.null(b$ids)) api_abort(400, "Nothing to change: send a name, text or ids.")
    # Validate everything before writing anything.
    name <- if (!is.null(b$name)) clean_name(b$name, "list name") else r$name
    if (!identical(name, r$name) &&
        nrow(db_query("SELECT 1 FROM lists WHERE ws = ? AND name = ?", list(ws, name))) > 0)
      api_abort(409, sprintf("A list named \"%s\" already exists.", name))
    newIds <- NULL; newScores <- NULL
    if (!is.null(b$text) || !is.null(b$ids)) {
      ranked <- !is.null(r$scores_json) && !is.na(r$scores_json) && nzchar(r$scores_json)
      text <- if (!is.null(b$text)) as.character(b$text)[1] else NULL
      if (!is.null(text)) check_text_size(text)
      if (ranked) {
        if (!is.null(text)) {
          parsed <- parse_ranked_text(text)
          newIds <- parsed$ids; newScores <- parsed$scores
        } else {
          newIds <- trimws(as.character(unlist(b$ids)))
          newScores <- suppressWarnings(as.numeric(unlist(b$scores)))
          if (length(newScores) != length(newIds)) api_abort(400, "A ranked list needs one score per ID.")
          ok <- nzchar(newIds) & !is.na(newIds) & grepl("^[A-Za-z0-9_.:\\-]+$", newIds) & is.finite(newScores)
          newIds <- newIds[ok]; newScores <- newScores[ok]
          keep <- !duplicated(tolower(newIds))
          newIds <- newIds[keep]; newScores <- newScores[keep]
        }
        if (length(newIds) < 15) api_abort(400, "A ranked list needs at least 15 IDs with numeric scores.")
        if (length(newIds) > FLAME_LIMITS$maxRankedIds)
          api_abort(400, sprintf("A ranked list can contain at most %s IDs.", format(FLAME_LIMITS$maxRankedIds, big.mark = ",")))
      } else {
        newIds <- clean_ids(if (!is.null(text)) parse_id_text(text)$ids else b$ids)
      }
    }
    if (!identical(name, r$name)) {
      db_exec("UPDATE lists SET name = ? WHERE id = ?", list(name, r$id))
      db_exec("UPDATE runs SET list_name = ? WHERE ws = ? AND list_name = ?", list(name, ws, r$name))
      db_exec("UPDATE runs SET bg_list_name = ? WHERE ws = ? AND bg_list_name = ?", list(name, ws, r$name))
    }
    if (!is.null(newIds))
      db_exec("UPDATE lists SET ids_json = ?, scores_json = ? WHERE id = ?",
              list(to_json(newIds), if (is.null(newScores)) NA_character_ else to_json(newScores), r$id))
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
