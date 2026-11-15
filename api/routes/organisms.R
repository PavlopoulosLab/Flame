# /api/organisms — organism search (no workspace needed)

#* Search organisms by name, short name or NCBI taxonomy id
#* @param q Search text (2+ characters)
#* @param limit Max results (default 50)
#* @param tool Optional tool key: only organisms this tool supports
#* @get /
#* @serializer flamejson
function(res, q = "", limit = 50, tool = "") {
  api_handle(res, {
    q <- substr(trimws(as.character(q)), 1, 100)
    limit <- max(1L, min(100L, suppressWarnings(as.integer(limit)) %||% 50L))
    hits <- if (nchar(q) >= 2) searchWorkspaceOrganisms(q, limit = limit * 3) else getWorkspaceCommonOrganismChoices()
    if (nzchar(tool)) {
      toolId <- getWorkspaceToolIdByKey(tool)
      if (!is.null(toolId)) hits <- Filter(function(h) tool_supports_organism(toolId, as.integer(h$value)), hits)
    }
    utils::head(hits, limit)
  })
}

#* Which tools support an organism
#* @get /<taxid:int>/tools
#* @serializer flamejson
function(res, taxid) {
  api_handle(res, {
    if (!taxid %in% ORGANISMS$taxid) api_abort(404, "Unknown organism.")
    tools <- getWorkspaceToolUI()
    list(taxid = taxid, name = .organism_label(taxid),
         tools = lapply(tools, function(t) list(key = t$key, supported = tool_supports_organism(t$toolId, taxid))))
  })
}
