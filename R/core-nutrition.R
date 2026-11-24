# =============================================================================
# FLAME core: nutrients, food compounds and foods as a source of gene lists
# =============================================================================
# Live web APIs only (nothing is downloaded or bundled):
#   * CTD (Comparative Toxicogenomics Database): manually curated chemical-gene
#     interactions, including nutrients, phytochemicals, foods (Tea, Coffee, Olive
#     Oil...) and dietary fats; each interaction says what the compound does to the
#     gene (e.g. increases its expression) and in which publications.
#   * ChEMBL: measured bioactivity of the compound on human protein targets.
#   * UniProt (via ChEBI): enzymes, transporters and binding proteins whose
#     annotations name the compound (substrate, product, cofactor, ligand).
# Diet and nutrient traits from the GWAS Catalog are reached through
# disease_search() / disease_gwas_variants() (core-fetch.R).
# =============================================================================

CTD_BATCH_URL <- "https://ctdbase.org/tools/batchQuery.go"
CHEMBL_API <- "https://www.ebi.ac.uk/chembl/api/data"
UNIPROT_API <- "https://rest.uniprot.org/uniprotkb/search"
OLS_API <- "https://www.ebi.ac.uk/ols4/api/search"
NUTRITION_SOURCES <- c("ctd", "chembl", "uniprot")

.nutri_cache <- new.env()
.nutri_cached <- function(key, expr) {
  hit <- .nutri_cache[[key]]
  if (!is.null(hit)) return(hit)
  val <- expr
  size <- as.numeric(utils::object.size(val))
  if (size > 20e6) return(val)                                  # very large answers are not kept
  used <- .nutri_cache$.bytes %||% 0
  if (length(ls(.nutri_cache)) >= 100 || used + size > 200e6) { rm(list = ls(.nutri_cache, all.names = TRUE), envir = .nutri_cache); used <- 0 }
  .nutri_cache[[key]] <- val
  .nutri_cache$.bytes <- used + size
  val
}

.nutri_term <- function(term) {
  t <- trimws(gsub("[\r\n\t|]+", " ", as.character(term %||% "")[1]))
  if (nchar(t) < 2) stop("Type a nutrient, food compound or food (e.g. curcumin, vitamin D, green tea).")
  if (nchar(t) > 120) stop("The name is too long.")
  t
}

.http_text <- function(url, query = NULL, timeout = 120) {
  resp <- tryCatch(httr::GET(url, query = query, httr::user_agent("Flame/3.0 (functional enrichment web server)"), httr::timeout(timeout)),
                   error = function(e) stop(sprintf("%s could not be reached (%s). Try again later.", .service_host(url), conditionMessage(e))))
  code <- httr::status_code(resp)
  if (code >= 500) stop(sprintf("%s is not available right now (HTTP %d). Please try again later.", .service_host(url), code))
  if (code >= 400) stop(sprintf("%s rejected the request (HTTP %d).", .service_host(url), code))
  httr::content(resp, as = "text", encoding = "UTF-8")
}

# ---- CTD ----------------------------------------------------------------------

.ctd_interactions <- function(term) {
  .nutri_cached(paste("ctd", tolower(term)), {
    txt <- .http_text(CTD_BATCH_URL, list(inputType = "chem", inputTerms = term, report = "cgixns", format = "tsv", actionTypes = "ANY"))
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    if (length(lines) < 2 || grepl("Object not found", lines[2], fixed = TRUE)) NULL
    else {
      lines[1] <- sub("^#\\s*", "", lines[1])
      utils::read.delim(text = paste(lines, collapse = "\n"), quote = "", comment.char = "", colClasses = "character", check.names = FALSE)
    }
  })
}

#' MeSH headings for a name (CTD indexes chemicals by MeSH), e.g. "omega-3" -> "Fatty Acids, Omega-3".
.mesh_headings <- function(term, max = 3L) {
  ids <- tryCatch(.eutils("esearch", db = "mesh", term = term, retmax = max)$esearchresult$idlist, error = function(e) list())
  if (!length(ids)) return(character(0))
  s <- tryCatch(.eutils("esummary", db = "mesh", id = paste(unlist(ids), collapse = ","))$result, error = function(e) NULL)
  if (is.null(s)) return(character(0))
  unique(unlist(lapply(unlist(ids), function(i) s[[i]]$ds_meshterms[[1]] %||% character(0))))
}

#' Genes whose expression, activity or binding a compound or food changes (CTD curated interactions).
#' `organisms`: "human" (default) or "all" (adds mouse and rat genes under their human-style symbol).
nutrition_ctd <- function(term, organisms = "human") {
  term <- .nutri_term(term)
  x <- .ctd_interactions(term)
  tried <- term
  if (is.null(x)) {                                          # try the MeSH heading of the name
    for (h in setdiff(.mesh_headings(term), term)) {
      tried <- c(tried, h)
      x <- .ctd_interactions(h)
      if (!is.null(x)) break
    }
  }
  if (is.null(x) || !nrow(x)) stop(sprintf("CTD has no curated gene interactions for '%s'. Try another name (e.g. its MeSH name, or the main compound of the food).", term))
  orgs <- if (identical(organisms, "all")) c("9606", "10090", "10116") else "9606"
  x <- x[x$OrganismID %in% orgs & nzchar(x$GeneSymbol), , drop = FALSE]
  if (!nrow(x)) stop(sprintf("CTD has interactions for '%s', but none in human genes. Include mouse and rat to see them.", term))
  x$gene <- if (identical(organisms, "all")) toupper(x$GeneSymbol) else x$GeneSymbol
  by <- split(x, x$gene)
  rows <- lapply(names(by), function(g) {
    d <- by[[g]]
    acts <- unlist(strsplit(d$InteractionActions, "|", fixed = TRUE))
    pm <- unique(unlist(strsplit(d$PubMedIDs, "|", fixed = TRUE))); pm <- pm[nzchar(pm)]
    up <- any(acts == "increases^expression"); down <- any(acts == "decreases^expression")
    dir <- if (up && down) "both" else if (up) "up" else if (down) "down" else "other"
    kinds <- unique(sub("^[a-z]+\\^", "", acts))
    list(gene = g, value = length(pm), direction = dir,
         note = paste0(c(up = "↑ expression", down = "↓ expression", both = "↑↓ expression", other = "")[[dir]],
                       if (dir != "other" && length(setdiff(kinds, "expression"))) " · " else "",
                       paste(utils::head(setdiff(kinds, "expression"), 3), collapse = ", "),
                       sprintf(" · %d interaction%s", nrow(d), if (nrow(d) == 1) "" else "s"),
                       if (identical(organisms, "all")) paste0(" · ", paste(sort(unique(c(`9606` = "human", `10090` = "mouse", `10116` = "rat")[d$OrganismID])), collapse = "/")) else ""))
  })
  rows <- rows[order(-vapply(rows, `[[`, 0, "value"), names(by))]
  chem <- x$ChemicalName[1]; id <- x$ChemicalID[1]
  list(source = "CTD", term = chem, matched = if (!identical(tolower(chem), tolower(term))) chem else NULL,
       genes = rows, valueLabel = "publications (CTD)", ranked = TRUE, organismTaxId = 9606L, total = length(rows),
       link = paste0("https://ctdbase.org/detail.go?type=chem&acc=", utils::URLencode(id, reserved = TRUE)),
       citation = "Curated chemical-gene interactions from the Comparative Toxicogenomics Database (Davis et al. 2025, Nucleic Acids Res 53:D1328); genes ranked by the number of publications.")
}

# ---- ChEMBL -------------------------------------------------------------------

# ChEMBL's API answers some requests with HTTP 500 and the same request moments later: retry.
.chembl <- function(path, query, timeout = 90) {
  url <- paste0(CHEMBL_API, path)
  for (attempt in 1:4) {
    resp <- tryCatch(httr::GET(url, query = c(query, list(format = "json")), httr::user_agent("Flame/3.0 (functional enrichment web server)"), httr::timeout(timeout)),
                     error = function(e) NULL)
    if (!is.null(resp) && httr::status_code(resp) < 500) break
    Sys.sleep(attempt)
  }
  if (is.null(resp)) stop("ChEMBL could not be reached. Try again later.")
  if (httr::status_code(resp) >= 400) stop(sprintf("ChEMBL is not answering right now (HTTP %d). Try again later.", httr::status_code(resp)))
  jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

.chembl_molecule <- function(term) {
  for (q in list(list(pref_name__iexact = term), list(molecule_synonyms__molecule_synonym__iexact = term))) {
    m <- .chembl("/molecule", c(q, list(only = "molecule_chembl_id,pref_name", limit = 5)))
    if (length(m$molecules)) return(m$molecules[[1]])
  }
  NULL
}

#' Human protein targets with measured activity of the compound (ChEMBL, pChEMBL >= minPchembl).
nutrition_chembl <- function(term, minPchembl = 5) {
  term <- .nutri_term(term)
  minPchembl <- suppressWarnings(as.numeric(minPchembl)); if (!is.finite(minPchembl)) minPchembl <- 5
  mol <- .nutri_cached(paste("chembl-mol", tolower(term)), .chembl_molecule(term))
  if (is.null(mol)) stop(sprintf("ChEMBL has no compound named '%s'. ChEMBL lists single compounds (e.g. curcumin, quercetin, genistein), not foods.", term))
  acts <- .nutri_cached(paste("chembl-act", mol$molecule_chembl_id, minPchembl), {
    out <- list(); offset <- 0L
    repeat {
      a <- .chembl("/activity", list(molecule_chembl_id = mol$molecule_chembl_id, target_organism = "Homo sapiens", pchembl_value__gte = minPchembl,
                                     only = "target_chembl_id,pchembl_value", limit = 1000L, offset = offset))
      out <- c(out, a$activities)
      offset <- offset + 1000L
      if (is.null(a$page_meta$`next`) || offset >= 5000L) break
    }
    out
  })
  if (!length(acts)) stop(sprintf("ChEMBL has no human targets of %s with pChEMBL ≥ %s.", mol$pref_name %||% term, minPchembl))
  tid <- vapply(acts, function(a) a$target_chembl_id %||% "", ""); pv <- suppressWarnings(as.numeric(vapply(acts, function(a) as.character(a$pchembl_value %||% NA), "")))
  ids <- unique(tid[nzchar(tid)])
  targets <- list()
  for (chunk in split(ids, ceiling(seq_along(ids) / 50))) {
    t <- .nutri_cached(paste("chembl-tg", paste(chunk, collapse = ",")), .chembl("/target", list(target_chembl_id__in = paste(chunk, collapse = ","), target_type = "SINGLE PROTEIN",
                                                                                                  only = "target_chembl_id,pref_name,target_components", limit = 100L))$targets)
    targets <- c(targets, t)
  }
  sym <- list()
  for (t in targets) {
    g <- unlist(lapply(t$target_components, function(c) vapply(Filter(function(s) identical(s$syn_type, "GENE_SYMBOL"), c$target_component_synonyms %||% list()), `[[`, "", "component_synonym")))
    if (length(g)) sym[[t$target_chembl_id]] <- list(gene = g[1], name = t$pref_name %||% "")
  }
  keep <- tid %in% names(sym)
  if (!any(keep)) stop(sprintf("ChEMBL's activities of %s are on cells or protein complexes, not single human proteins.", mol$pref_name %||% term))
  genes <- vapply(sym[tid[keep]], `[[`, "", "gene")
  best <- tapply(pv[keep], genes, max, na.rm = TRUE); n <- table(genes)
  nm <- vapply(names(best), function(g) sym[[which(vapply(sym, `[[`, "", "gene") == g)[1]]]$name, "")
  o <- order(-best, names(best))
  list(source = "ChEMBL", term = mol$pref_name %||% term, organismTaxId = 9606L, ranked = TRUE, valueLabel = "best pChEMBL",
       genes = lapply(o, function(i) list(gene = names(best)[i], name = nm[[i]], value = round(unname(best[i]), 2),
                                          note = sprintf("%d activit%s", n[[names(best)[i]]], if (n[[names(best)[i]]] == 1) "y" else "ies"))),
       total = length(best), link = paste0("https://www.ebi.ac.uk/chembl/explore/compound/", mol$molecule_chembl_id),
       citation = sprintf("Human single-protein targets with measured activity (pChEMBL ≥ %s) in ChEMBL (Zdrazil et al. 2024, Nucleic Acids Res 52:D1180); pChEMBL = −log10 of the molar IC50, EC50, Ki or Kd.", minPchembl))
}

# ---- UniProt via ChEBI ------------------------------------------------------------

#' ChEBI entity for a name (EBI OLS): exact label or synonym match first.
.chebi_of <- function(term) {
  d <- .http_json(OLS_API, query = list(q = term, ontology = "chebi", rows = 10, queryFields = "label,synonym", fieldList = "obo_id,label,synonym"))
  docs <- d$response$docs
  if (!length(docs)) return(NULL)
  exact <- Filter(function(x) tolower(term) %in% tolower(c(x$label, unlist(x$synonym))), docs)
  (if (length(exact)) exact else docs)[[1]]
}

#' Reviewed proteins whose UniProt annotations name the compound (catalytic activity, cofactor, binding site).
nutrition_uniprot <- function(term, taxid = 9606L) {
  term <- .nutri_term(term)
  taxid <- as.integer(taxid %||% 9606L)
  chebi <- .nutri_cached(paste("chebi", tolower(term)), .chebi_of(term))
  if (is.null(chebi$obo_id)) stop(sprintf("No ChEBI entry is named '%s'.", term))
  num <- sub("^CHEBI:", "", chebi$obo_id)
  txt <- .nutri_cached(paste("uniprot", num, taxid), .http_text(UNIPROT_API, list(
    query = sprintf("(chebi:%s) AND (organism_id:%d) AND (reviewed:true)", num, taxid),
    fields = "gene_primary,protein_name", format = "tsv", size = 500)))
  df <- utils::read.delim(text = txt, quote = "", colClasses = "character", check.names = FALSE)
  df <- df[nzchar(df[[1]]), , drop = FALSE]
  if (!nrow(df)) stop(sprintf("No reviewed proteins of this organism are annotated with %s (%s) in UniProt.", chebi$label, chebi$obo_id))
  df <- df[!duplicated(df[[1]]), , drop = FALSE]
  list(source = "UniProt", term = chebi$label, chebi = chebi$obo_id, organismTaxId = taxid, ranked = FALSE,
       genes = lapply(seq_len(nrow(df)), function(i) list(gene = df[[1]][i], name = sub(" \\(.*$", "", df[[2]][i]))),
       total = nrow(df), link = sprintf("https://www.uniprot.org/uniprotkb?query=chebi:%s+AND+organism_id:%d+AND+reviewed:true", num, taxid),
       citation = sprintf("Reviewed UniProtKB proteins (UniProt Consortium 2025, Nucleic Acids Res 53:D609) whose catalytic activity, cofactor or binding-site annotations name %s (%s, ChEBI).", chebi$label, chebi$obo_id))
}

#' One entry point for the API.
nutrition_genes <- function(term, source, ...) {
  a <- list(...)
  switch(as.character(source %||% ""),
    ctd = nutrition_ctd(term, a$organisms %||% "human"),
    chembl = nutrition_chembl(term, a$minPchembl %||% 5),
    uniprot = nutrition_uniprot(term, a$taxid %||% 9606L),
    stop("Unknown source."))
}

# ---- Diet and nutrient traits (GWAS Catalog via Open Targets) ------------------------

#' Diet and nutrient traits for a term: Open Targets searched for the term and for its intake,
#' level, measurement and consumption traits; measurement-like traits first.
nutrition_traits <- function(term, max = 25L) {
  term <- .nutri_term(term)
  phrasings <- unique(c(term, paste(term, c("level", "measurement", "intake", "consumption"))))
  hits <- list()
  for (q in phrasings) for (h in tryCatch(disease_search(q, 10L), error = function(e) list())) if (is.null(hits[[h$id]])) hits[[h$id]] <- h
  if (!length(hits)) return(list())
  nm <- tolower(vapply(hits, `[[`, "", "name"))
  words <- strsplit(tolower(term), "\\s+")[[1]]
  about <- vapply(nm, function(n) all(vapply(words, function(w) grepl(w, n, fixed = TRUE), logical(1))), logical(1))
  trait <- grepl("level|measurement|intake|consumption|concentration|amount|diet|supplement", nm)
  if (any(about)) { hits <- hits[about]; trait <- trait[about] }   # drop matches of the added words only ("level of ... protein")
  unname(utils::head(hits[order(-trait)], max))
}
