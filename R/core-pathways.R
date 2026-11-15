# =============================================================================
# FLAME core: links from enriched terms to pathway databases
# =============================================================================
# KEGG: the organism-specific map with the user's genes coloured, built with
#   KEGG's Map Coloring URL (show_pathway?map=...&multi_query=...). Gene IDs are
#   mapped symbol -> NCBI Gene (g:Convert) -> KEGG gene ID (KEGG REST /conv).
#   Ranked lists colour genes by the sign of their score.
# Reactome: Pathway Browser with the user's genes flagged (FLG).
# WikiPathways, Gene Ontology (QuickGO/AmiGO), HPO: term pages.
# Every term also gets searches in the other pathway databases by name.
# =============================================================================

KEGG_REST <- "https://rest.kegg.jp"
KEGG_ORG <- c(
  "9606" = "hsa", "10090" = "mmu", "10116" = "rno", "7955" = "dre", "7227" = "dme", "6239" = "cel",
  "4932" = "sce", "559292" = "sce", "9031" = "gga", "9913" = "bta", "9823" = "ssc", "9615" = "cfa",
  "3702" = "ath", "9544" = "mcc", "9598" = "ptr", "8364" = "xtr", "9796" = "ecb", "9685" = "fca",
  "9940" = "oas", "511145" = "eco", "83333" = "eco", "284812" = "spo", "83332" = "mtu", "39947" = "osa",
  "4577" = "zma", "7460" = "ame", "13616" = "mdo", "9258" = "oaa"
)
KEGG_MAX_GENES <- 400L
.kegg_cache <- new.env()

.pl <- function(label, url, group) list(label = label, url = url, group = group)
.enc <- function(x) utils::URLencode(as.character(x), reserved = TRUE)

#' KEGG pathway number (5 digits) from any tool's term ID, or NA
kegg_map_number <- function(termId) {
  m <- regmatches(termId, regexpr("(?<![0-9])[0-9]{5}(?![0-9])", termId, perl = TRUE))
  if (length(m)) m else NA_character_
}

#' Reactome stable ID (R-HSA-123) from a term ID, or NA
reactome_stable_id <- function(termId) {
  m <- regmatches(termId, regexpr("R-[A-Z]{3}-[0-9]+", termId))
  if (length(m)) return(m)
  m <- regmatches(termId, regexpr("(?<![A-Z])[A-Z]{3}-[0-9]{3,}", termId, perl = TRUE))
  if (length(m)) paste0("R-", m) else NA_character_
}

wikipathways_id <- function(termId) {
  m <- regmatches(termId, regexpr("WP[0-9]+", termId))
  if (length(m)) m else NA_character_
}

#' Map gene symbols / IDs to KEGG gene IDs (org:id). Returns a named character
#' vector (names = input IDs). Network calls are cached per organism and gene set.
kegg_gene_ids <- function(genes, taxid) {
  org <- KEGG_ORG[[as.character(taxid)]] %||% NA_character_
  genes <- unique(genes[nzchar(genes)])
  if (is.na(org) || !length(genes)) return(character(0))
  key <- paste(org, paste(sort(genes), collapse = ","))
  if (!is.null(.kegg_cache[[key]])) return(.kegg_cache[[key]])
  short <- ORGANISMS$short_name[ORGANISMS$taxid == taxid][1]
  conv <- tryCatch(gprofiler2::gconvert(genes, organism = short, target = "ENTREZGENE_ACC", mthreshold = 1, filter_na = TRUE),
                   error = function(e) NULL)
  if (is.null(conv) || !nrow(conv)) return(character(0))
  entrez <- stats::setNames(as.character(conv$target), conv$input)
  entrez <- entrez[grepl("^[0-9]+$", entrez)]
  out <- character(0)
  ids <- unique(entrez)
  for (b in split(ids, ceiling(seq_along(ids) / 100))) {
    resp <- tryCatch(httr::GET(paste0(KEGG_REST, "/conv/", org, "/", paste0("ncbi-geneid:", b, collapse = "+")), httr::timeout(30)),
                     error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) next
    lines <- strsplit(rawToChar(httr::content(resp, "raw")), "\n")[[1]]
    parts <- strsplit(lines[nzchar(lines)], "\t")
    from <- sub("^ncbi-geneid:", "", vapply(parts, `[`, "", 1))
    to <- vapply(parts, function(p) p[2] %||% NA_character_, "")
    out <- c(out, stats::setNames(to, from))
  }
  res <- stats::setNames(out[entrez], names(entrez))
  res <- res[!is.na(res) & grepl("^[a-z]{2,5}:[A-Za-z0-9_.-]+$", res)]
  if (length(ls(.kegg_cache)) > 200) rm(list = ls(.kegg_cache), envir = .kegg_cache)
  .kegg_cache[[key]] <- res
  res
}

#' KEGG map URL with genes coloured. `scores` (named by gene) colours by sign.
kegg_colored_url <- function(mapNumber, taxid, keggIds, scores = NULL) {
  org <- KEGG_ORG[[as.character(taxid)]]
  keggIds <- utils::head(keggIds, KEGG_MAX_GENES)
  colour <- function(g) {
    s <- if (!is.null(scores)) scores[[g]] %||% NA else NA
    if (is.na(s)) "#ff0000,black" else if (s > 0) "#ff0000,black" else if (s < 0) "#2563eb,white" else "#d4d4d8,black"
  }
  lines <- vapply(names(keggIds), function(g) paste(keggIds[[g]], colour(g)), "")
  paste0("https://www.kegg.jp/kegg-bin/show_pathway?map=", org, mapNumber,
         "&multi_query=", .enc(paste(unique(lines), collapse = "\r\n")))
}

#' All pathway / ontology links for one enriched term.
#' term: list(id, source, name); genes: the user's genes in the term;
#' scores: optional named numeric (ranked lists).
pathway_links <- function(term, genes, taxid, scores = NULL) {
  id <- as.character(term$id); src <- toupper(as.character(term$source)); name <- as.character(term$name)
  genes <- utils::head(unique(genes[grepl("^[A-Za-z0-9._:-]+$", genes)]), KEGG_MAX_GENES)
  links <- list(); notes <- character(0)
  org <- KEGG_ORG[[as.character(taxid)]] %||% NA_character_
  num <- if (grepl("KEGG", src) || grepl("^(KEGG:|path:|map|[a-z]{2,4})[0-9]{5}$", id)) kegg_map_number(id) else NA_character_
  if (!is.na(num)) {
    if (!is.na(org)) {
      kid <- kegg_gene_ids(genes, taxid)
      if (length(kid)) {
        links[[length(links) + 1]] <- .pl(sprintf("KEGG map with your %d genes highlighted in red", length(kid)),
                                           kegg_colored_url(num, taxid, kid, scores), "Pathway maps")
        notes <- c(notes, if (!is.null(scores)) "KEGG colours: red = positive score, blue = negative score." else "Your genes are highlighted in red on the KEGG map.")
        if (length(kid) < length(genes)) notes <- c(notes, sprintf("%d of %d genes have no KEGG entry.", length(genes) - length(kid), length(genes)))
      } else notes <- c(notes, "None of the genes could be mapped to KEGG; the map opens uncoloured.")
      links[[length(links) + 1]] <- .pl("KEGG map (this organism)", paste0("https://www.kegg.jp/pathway/", org, num), "Pathway maps")
    }
    links[[length(links) + 1]] <- .pl("KEGG reference map and entry", paste0("https://www.kegg.jp/entry/map", num), "Pathway maps")
  }
  rid <- if (grepl("REAC", src) || grepl("R-[A-Z]{3}-[0-9]+", id)) reactome_stable_id(id) else NA_character_
  if (!is.na(rid)) {
    flg <- if (length(genes)) paste0("&FLG=", .enc(paste(utils::head(genes, 100), collapse = ","))) else ""
    links[[length(links) + 1]] <- .pl(sprintf("Reactome pathway browser%s", if (length(genes)) " with your genes flagged" else ""),
                                       paste0("https://reactome.org/PathwayBrowser/#/", rid, flg), "Pathway maps")
    links[[length(links) + 1]] <- .pl("Reactome pathway page", paste0("https://reactome.org/content/detail/", rid), "Pathway maps")
  }
  wp <- if (grepl("WP", src) || grepl("^WP:?WP[0-9]+$|^WP[0-9]+$", id)) wikipathways_id(id) else NA_character_
  if (!is.na(wp)) links[[length(links) + 1]] <- .pl("WikiPathways diagram", paste0("https://www.wikipathways.org/pathways/", wp), "Pathway maps")
  go <- regmatches(id, regexpr("GO:[0-9]{7}", id))
  if (length(go)) {
    links[[length(links) + 1]] <- .pl("QuickGO term", paste0("https://www.ebi.ac.uk/QuickGO/term/", go), "Ontologies")
    links[[length(links) + 1]] <- .pl("AmiGO term", paste0("https://amigo.geneontology.org/amigo/term/", go), "Ontologies")
  }
  hp <- regmatches(id, regexpr("HP:[0-9]{7}", id))
  if (length(hp)) links[[length(links) + 1]] <- .pl("Human Phenotype Ontology", paste0("https://hpo.jax.org/browse/term/", hp), "Ontologies")
  # Find the same biology elsewhere.
  q <- .enc(gsub("\\s+", " ", substr(name, 1, 120)))
  links <- c(links, list(
    .pl("Search KEGG pathways", paste0("https://www.kegg.jp/kegg-bin/search_pathway_text?map=", if (is.na(org)) "map" else org, "&keyword=", q, "&mode=1"), "Search other databases"),
    .pl("Search Reactome", paste0("https://reactome.org/content/query?q=", q), "Search other databases"),
    .pl("Search WikiPathways", paste0("https://www.wikipathways.org/search.html?query=", q), "Search other databases"),
    .pl("Search Pathway Commons", paste0("https://apps.pathwaycommons.org/search?type=Pathway&q=", q), "Search other databases")
  ))
  list(links = links, notes = as.list(notes))
}
