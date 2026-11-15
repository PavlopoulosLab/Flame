# =============================================================================
# FLAME core: upstream regulators of a gene list
# =============================================================================
# Transcription factors: ChEA3 (Keenan et al. 2019, Nucleic Acids Res 47:W212)
# integrates TF-target evidence from ChIP-seq (ENCODE, ReMap, literature),
# co-expression (ARCHS4, GTEx) and Enrichr co-occurrence; TFs are ranked by
# their mean rank across these libraries.
# Kinases: KEA3 (Kuleshov et al. 2021, Nucleic Acids Res 49:W304) ranks kinases
# whose substrates or interactors are over-represented in the list.
# Both are human gene-symbol resources (https://maayanlab.cloud).
# =============================================================================

REGULATOR_APIS <- list(
  tf = list(url = "https://maayanlab.cloud/chea3/api/enrich/", name = "ChEA3",
            cite = "Keenan AB et al. ChEA3: transcription factor enrichment analysis by orthogonal omics integration. Nucleic Acids Res 2019;47:W212-W224."),
  kinase = list(url = "https://maayanlab.cloud/kea3/api/enrich/", name = "KEA3",
                cite = "Kuleshov MV et al. KEA3: improved kinase enrichment analysis via data integration. Nucleic Acids Res 2021;49:W304-W316.")
)

#' Rank upstream regulators (kind = "tf" or "kinase") of a human gene list.
upstream_regulators <- function(genes, kind = c("tf", "kinase"), top = 60L) {
  kind <- match.arg(kind)
  api <- REGULATOR_APIS[[kind]]
  genes <- unique(toupper(trimws(as.character(unlist(genes)))))
  genes <- genes[grepl("^[A-Z0-9][A-Z0-9.-]*$", genes)]
  if (length(genes) < 5) stop("At least 5 gene symbols are needed.")
  if (length(genes) > 3000) genes <- genes[seq_len(3000)]
  flame_progress(0.1, sprintf("Sending %d genes to %s", length(genes), api$name))
  resp <- httr::POST(api$url, body = jsonlite::toJSON(list(query_name = "flame", gene_set = genes), auto_unbox = TRUE),
                     httr::content_type_json(), httr::timeout(240))
  if (httr::status_code(resp) != 200) stop(api$name, " did not respond (HTTP ", httr::status_code(resp), "). Please try again later.")
  flame_progress(0.8, "Ranking regulators")
  d <- jsonlite::fromJSON(rawToChar(httr::content(resp, "raw")), simplifyVector = FALSE)
  integ <- d[["Integrated--meanRank"]]
  if (!length(integ)) stop(api$name, " returned no regulators for this list.")
  libs <- setdiff(names(d), c("Integrated--meanRank", "Integrated--topRank"))
  rows <- lapply(utils::head(integ, top), function(r) {
    libRanks <- lapply(strsplit(strsplit(r$Library %||% "", ";")[[1]], ","), function(x) list(library = trimws(x[1]), rank = suppressWarnings(as.integer(x[2]))))
    overlap <- unique(strsplit(r$Overlapping_Genes %||% "", ",")[[1]])
    list(rank = as.integer(r$Rank), name = r$TF, score = suppressWarnings(as.numeric(r$Score)),
         libraries = libRanks, overlap = as.list(overlap[nzchar(overlap)]))
  })
  list(kind = kind, tool = api$name, citation = api$cite, queried = length(genes), libraries = as.list(libs),
       regulators = rows,
       note = paste0(api$name, " ranks ", if (kind == "tf") "transcription factors" else "kinases",
                     " by their mean rank across ", length(libs), " evidence libraries (lower score = stronger support)."))
}
