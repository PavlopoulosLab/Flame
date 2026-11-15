# /api/legacy — Flame v2-compatible programmatic entry points.
# Both create a fresh workspace, add the gene lists, and return its link.
#   GET  /api/legacy?url_genes=TP53,BRCA1;EGFR,KRAS[&organism=9606]
#   POST /api/legacy   { "gene_list1": ["TP53","BRCA1"], "gene_list2": [...], "organism": 9606 }
# The web app also accepts /?url_genes=... and forwards it here.

.legacy_import <- function(res, lists, organism) {
  taxid <- suppressWarnings(as.integer(organism %||% 9606L))
  if (is.na(taxid) || !taxid %in% ORGANISMS$taxid) api_abort(400, "Unknown organism.")
  lists <- Filter(function(x) length(x) > 0, lists)
  if (length(lists) == 0) api_abort(400, "No gene lists were given.")
  if (length(lists) > FLAME_LIMITS$maxLists) api_abort(400, sprintf("At most %d lists can be imported.", FLAME_LIMITS$maxLists))
  ws <- create_workspace()
  created <- lapply(seq_along(lists), function(i) {
    nm <- names(lists)[i]
    if (is.null(nm) || !nzchar(nm)) nm <- paste0("List ", i)
    create_list(ws, nm, lists[[i]], taxid, "GENE", "USERINPUT", list(source = "api"), uniquify = TRUE)
  })
  base <- Sys.getenv("FLAME_PUBLIC_URL", "")
  res$status <- 201
  list(workspace = ws, url = paste0(base, "/w/", ws), lists = created)
}

#* Create a workspace from gene lists in the URL (v2 syntax)
#* @param url_genes Lists separated by ";", genes by ","
#* @param organism NCBI taxonomy id (default 9606)
#* @get /
#* @serializer flamejson
function(res, url_genes = "", organism = 9606) {
  api_handle(res, {
    raw <- substr(as.character(url_genes), 1, 200000)
    parts <- strsplit(raw, ";", fixed = TRUE)[[1]]
    lists <- lapply(parts, function(p) trimws(strsplit(p, ",", fixed = TRUE)[[1]]))
    names(lists) <- paste0("List ", seq_along(lists))
    .legacy_import(res, lists, organism)
  })
}

#* Create a workspace from named gene lists in a JSON body (v2 syntax)
#* @post /
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    organism <- b$organism %||% 9606L
    b$organism <- NULL
    lists <- lapply(b, function(x) as.character(unlist(x)))
    .legacy_import(res, lists, organism)
  })
}
