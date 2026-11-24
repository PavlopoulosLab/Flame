# /api/utilities — quick actions on a list: convert IDs, find orthologs, PPI network

.list_ids <- function(r) as.character(from_json(r$ids_json))

#* Convert a list's IDs (g:Convert). Body: { listId, targetFormat, mappingMode: all|best }
#* @post /convert
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    r <- get_list(ws, as.character(b$listId %||% ""))
    out <- tryCatch(convert_list_ids(.list_ids(r), r$taxid, as.character(b$targetFormat %||% ""),
                                     if (identical(b$mappingMode, "best")) "best" else "all"),
                    error = function(e) api_abort(422, conditionMessage(e)))
    out$source <- list_row_to_obj(r)
    out
  })
}

#* Annotate a list's genes (MyGene.info): current symbol, name, gene type, location, Ensembl and Entrez IDs,
#* with a status per ID (ok, alias, notfound) and duplicates after mapping. Body: { listId }
#* @post /annotate
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    r <- get_list(ws, as.character(b$listId %||% ""))
    out <- tryCatch(annotate_genes(.list_ids(r), r$taxid), error = function(e) api_abort(422, conditionMessage(e)))
    out$source_list <- list_row_to_obj(r)
    out
  })
}

#* Find orthologs (g:Orth). Body: { listId, targetTaxId, mappingMode: all|best }
#* @post /orthologs
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    r <- get_list(ws, as.character(b$listId %||% ""))
    target <- suppressWarnings(as.integer(b$targetTaxId))
    if (is.na(target) || !target %in% ORGANISMS$taxid) api_abort(400, "Choose a target organism.")
    out <- tryCatch(find_orthologs(.list_ids(r), r$taxid, target, r$id_format,
                                   if (identical(b$mappingMode, "best")) "best" else "all"),
                    error = function(e) api_abort(422, conditionMessage(e)))
    out$source <- list_row_to_obj(r)
    out$targetTaxId <- target
    out
  })
}

#* STRING protein–protein interaction network.
#* Body: { listId, networkType: functional|physical, networkEdges: evidence|confidence, score: 150|400|700|900 }
#* @post /ppi
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    r <- get_list(ws, as.character(b$listId %||% ""))
    out <- tryCatch(ppi_network(.list_ids(r), r$taxid, b$networkType %||% "functional",
                                b$networkEdges %||% "evidence", b$score %||% 400),
                    error = function(e) api_abort(422, conditionMessage(e)))
    out$source <- list_row_to_obj(r)
    out
  })
}

#* Open the STRING network in Arena3D (evidence types as edge channels). Same body as /ppi.
#* @post /ppi/arena3d
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    r <- get_list(ws, as.character(b$listId %||% ""))
    net <- tryCatch(ppi_network(.list_ids(r), r$taxid, b$networkType %||% "functional",
                                b$networkEdges %||% "evidence", b$score %||% 400),
                    error = function(e) api_abort(422, conditionMessage(e)))
    body <- tryCatch(arena3d_payload_for_ppi(net), error = function(e) api_abort(422, conditionMessage(e)))
    fetch_async(res, quote(arena3d_open(body)), list(body = body))   # web service: from the external-service workers
  })
}
