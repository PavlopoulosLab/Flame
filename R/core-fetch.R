# =============================================================================
# FLAME core: fetch gene lists from public resources
# =============================================================================
#   * Disease genes   Open Targets Platform (scored associations from GWAS,
#                     ClinVar, literature, animal models...), DisGeNET, OMIM,
#                     Orphanet, ClinVar, Jensen DISEASES, GWAS Catalog gene
#                     libraries (Enrichr), and GWAS Catalog variants.
#   * PubMed          genes linked to articles (NCBI gene2pubmed) for PMIDs or
#                     a PubMed search, ranked by the number of articles.
#   * GEO             differential-expression signatures of GEO series
#                     (RummaGEO, from uniformly processed ARCHS4 data).
#   * dbSNP           rsIDs, or a dbSNP search, to variants and their genes.
# No API keys are needed. Requests carry only the user's query.
# =============================================================================

EUTILS <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
OPENTARGETS_API <- "https://api.platform.opentargets.org/api/v4/graphql"
RUMMAGEO_API <- "https://rummageo.com/graphql"
GWAS_API_V2 <- "https://www.ebi.ac.uk/gwas/rest/api/v2"

DISEASE_LIBRARIES <- list(
  DisGeNET = list(library = "DisGeNET", label = "DisGeNET (curated and text-mined)"),
  OMIM = list(library = "OMIM_Expanded", label = "OMIM (expanded)"),
  Orphanet = list(library = "Orphanet_Augmented_2021", label = "Orphanet rare diseases"),
  ClinVar = list(library = "ClinVar_2025", label = "ClinVar 2025"),
  Jensen = list(library = "Jensen_DISEASES_Curated_2025", label = "Jensen DISEASES (curated)"),
  GWAS = list(library = "GWAS_Catalog_2025", label = "GWAS Catalog 2025 (reported genes)")
)

.http_json <- function(url, query = NULL, body = NULL, timeout = 60) {
  # NCBI allows 3 requests per second without a key: wait and retry on HTTP 429.
  for (attempt in 1:4) {
    resp <- tryCatch(
      if (is.null(body)) httr::GET(url, query = query, httr::timeout(timeout))
      else httr::POST(url, body = jsonlite::toJSON(body, auto_unbox = TRUE), httr::content_type_json(), httr::timeout(timeout)),
      error = function(e) stop("The service could not be reached: ", conditionMessage(e)))
    if (httr::status_code(resp) != 429) break
    Sys.sleep(0.6 * attempt)
  }
  if (httr::status_code(resp) >= 400) stop(sprintf("The service answered with HTTP %d.", httr::status_code(resp)))
  jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}
.gql <- function(url, query, variables = list()) {
  out <- .http_json(url, body = list(query = query, variables = variables))
  if (!is.null(out$errors)) stop(out$errors[[1]]$message %||% "Query failed.")
  out$data
}
# NCBI E-utilities: 3 requests/second without a key, 10 with FLAME_NCBI_API_KEY
# (free, from an NCBI account: https://www.ncbi.nlm.nih.gov/account/settings/).
.ncbi_query <- function(q) {
  key <- Sys.getenv("FLAME_NCBI_API_KEY", "")
  c(list(retmode = "json", tool = "flame"), if (nzchar(key)) list(api_key = key), q)
}
.eutils <- function(tool, ...) .http_json(paste0(EUTILS, "/", tool, ".fcgi"), query = .ncbi_query(list(...)))

# ---- Diseases -----------------------------------------------------------------

#' Search diseases (Open Targets: EFO / MONDO / Orphanet ids)
disease_search <- function(query, size = 15L) {
  q <- trimws(as.character(query))
  if (nchar(q) < 2) stop("Type at least 2 characters.")
  d <- .gql(OPENTARGETS_API, "query($q:String!,$n:Int!){ search(queryString:$q, entityNames:[\"disease\"], page:{index:0,size:$n}){ hits { id name description } } }",
            list(q = q, n = as.integer(size)))
  lapply(d$search$hits, function(h) list(id = h$id, name = h$name, description = substr(h$description %||% "", 1, 240)))
}

#' Genes associated with a disease, ranked by the Open Targets association score
disease_genes_opentargets <- function(id, size = 500L, minScore = 0) {
  d <- .gql(OPENTARGETS_API, "query($id:String!,$n:Int!){ disease(efoId:$id){ id name associatedTargets(page:{index:0,size:$n}){ count rows { score target { approvedSymbol approvedName } datatypeScores { id score } } } } }",
            list(id = id, n = as.integer(min(size, 3000))))
  if (is.null(d$disease)) stop("Disease not found in Open Targets.")
  rows <- d$disease$associatedTargets$rows
  genes <- lapply(rows, function(r) list(gene = r$target$approvedSymbol, name = r$target$approvedName, score = round(r$score, 4),
                                         evidence = stats::setNames(lapply(r$datatypeScores, function(x) round(x$score, 3)), vapply(r$datatypeScores, `[[`, "", "id"))))
  genes <- Filter(function(g) g$score >= minScore, genes)
  list(source = "Open Targets Platform", disease = d$disease$name, id = d$disease$id, total = d$disease$associatedTargets$count,
       organismTaxId = 9606L, genes = genes, ranked = TRUE,
       citation = "Open Targets Platform (Buniello et al. 2025, Nucleic Acids Res 53:D1467); score = overall association from genetics, somatic mutations, known drugs, expression, literature and animal models.",
       link = paste0("https://platform.opentargets.org/disease/", id, "/associations"))
}

.enrichr_libs <- new.env()
.enrichr_library <- function(library) {
  if (!is.null(.enrichr_libs[[library]])) return(.enrichr_libs[[library]])
  resp <- httr::GET("https://maayanlab.cloud/Enrichr/geneSetLibrary", query = list(mode = "text", libraryName = library), httr::timeout(120))
  if (httr::status_code(resp) >= 400) stop("The gene-set library could not be downloaded.")
  lines <- strsplit(httr::content(resp, as = "text", encoding = "UTF-8"), "\n")[[1]]
  parts <- strsplit(lines[nzchar(lines)], "\t")
  # Some libraries carry a lower-case type word (DisGeNET: "disease", "group", "phenotype") before the genes.
  sets <- lapply(parts, function(p) { g <- sub(",.*$", "", p[-(1:2)]); unique(g[nzchar(g) & !grepl("^[a-z ]+$", g)]) })
  names(sets) <- vapply(parts, `[[`, "", 1)
  .enrichr_libs[[library]] <- sets
  sets
}

#' Search the disease gene-set libraries (DisGeNET, OMIM, Orphanet, ClinVar...)
disease_library_search <- function(query, sources = names(DISEASE_LIBRARIES), max = 40L) {
  q <- tolower(trimws(query))
  if (nchar(q) < 2) stop("Type at least 2 characters.")
  words <- strsplit(q, "\\s+")[[1]]
  out <- list()
  for (s in intersect(sources, names(DISEASE_LIBRARIES))) {
    sets <- tryCatch(.enrichr_library(DISEASE_LIBRARIES[[s]]$library), error = function(e) NULL)
    if (is.null(sets)) next
    nm <- tolower(names(sets))
    hit <- which(Reduce(`&`, lapply(words, function(w) grepl(w, nm, fixed = TRUE))))
    hit <- hit[order(nchar(names(sets)[hit]))]
    for (i in utils::head(hit, max)) out[[length(out) + 1]] <- list(source = s, label = DISEASE_LIBRARIES[[s]]$label, term = names(sets)[i], size = length(sets[[i]]))
  }
  out
}

disease_library_genes <- function(source, term) {
  lib <- DISEASE_LIBRARIES[[source]]
  if (is.null(lib)) stop("Unknown disease library.")
  sets <- .enrichr_library(lib$library)
  g <- sets[[term]]
  if (is.null(g)) stop("Term not found in ", lib$label, ".")
  list(source = lib$label, disease = term, organismTaxId = 9606L, ranked = FALSE,
       genes = lapply(g, function(x) list(gene = x)),
       citation = paste0(lib$label, " gene set, via the Enrichr library ", lib$library, " (Kuleshov et al. 2016, Nucleic Acids Res 44:W90)."))
}

#' GWAS Catalog associations of a disease (EFO / MONDO id): variants and mapped genes
disease_gwas_variants <- function(id, max = 500L, pmax = 5e-8) {
  out <- list(); page <- 0L
  repeat {
    d <- .http_json(paste0(GWAS_API_V2, "/associations"), query = list(efo_id = id, size = 200, page = page, sort = "p_value", direction = "asc"))
    a <- d$`_embedded`$associations
    if (!length(a)) break
    for (x in a) {
      p <- suppressWarnings(as.numeric(x$p_value))
      if (!is.finite(p) || p > pmax) next
      rs <- vapply(x$snp_allele %||% list(), function(s) s$rs_id %||% "", "")
      for (r in rs[grepl("^rs[0-9]+$", rs)]) out[[length(out) + 1]] <- list(rsid = r, p = p, genes = unlist(x$mapped_genes), study = x$accession_id, pmid = x$pubmed_id, trait = paste(unlist(x$reported_trait), collapse = "; "))
    }
    page <- page + 1L
    if (length(out) >= max || page >= (d$page$totalPages %||% 1L) || page >= 10) break
  }
  if (!length(out)) return(list(variants = list(), genes = list(), total = 0L))
  best <- out[!duplicated(vapply(out, `[[`, "", "rsid"))]
  best <- utils::head(best, max)
  gcount <- table(unlist(lapply(best, function(v) unique(unlist(strsplit(v$genes %||% character(0), ",\\s*"))))))
  gcount <- sort(gcount[nzchar(names(gcount))], decreasing = TRUE)
  list(variants = best, total = length(best),
       genes = lapply(names(gcount), function(g) list(gene = g, variants = as.integer(gcount[[g]]))),
       citation = "GWAS Catalog associations (Cerezo et al. 2025, Nucleic Acids Res 53:D998) with p ≤ 5e-8; genes = genes mapped by the Catalog.")
}

# ---- PubMed -------------------------------------------------------------------

#' Genes linked to PubMed articles (NCBI gene2pubmed), ranked by number of articles
pubmed_genes <- function(query, taxid = 9606L, max = 200L) {
  q <- trimws(as.character(query))
  if (!nzchar(q)) stop("Give PMIDs or a PubMed search.")
  toks <- unlist(strsplit(q, "[\\s,;]+", perl = TRUE))
  if (all(grepl("^(PMID:?)?[0-9]{1,9}$", toks, ignore.case = TRUE))) {
    pmids <- unique(sub("^PMID:?", "", toks, ignore.case = TRUE))
    searched <- NULL
  } else {
    s <- .eutils("esearch", db = "pubmed", term = q, retmax = as.integer(min(max, 1000)), sort = "relevance")
    pmids <- unlist(s$esearchresult$idlist)
    searched <- list(query = q, count = as.integer(s$esearchresult$count %||% 0))
    if (!length(pmids)) stop("No PubMed articles match this search.")
  }
  pmids <- utils::head(pmids, 1000)
  flame_progress(0.3, "Linking articles to genes")
  links <- .http_json(paste0(EUTILS, "/elink.fcgi"), query = .ncbi_query(c(list(dbfrom = "pubmed", db = "gene"),
                                                               stats::setNames(as.list(pmids), rep("id", length(pmids))))), timeout = 120)
  per <- list()
  for (ls in links$linksets) {
    pm <- unlist(ls$ids)[1]
    g <- unlist(lapply(ls$linksetdbs, function(x) if (identical(x$linkname, "pubmed_gene")) unlist(x$links)))
    for (x in g) per[[x]] <- c(per[[x]], pm)
  }
  if (!length(per)) stop("None of these articles is linked to genes in NCBI Gene.")
  ids <- names(per)
  info <- list()
  for (chunk in split(ids, ceiling(seq_along(ids) / 300))) {
    sm <- .http_json(paste0(EUTILS, "/esummary.fcgi"), query = .ncbi_query(list(db = "gene", id = paste(chunk, collapse = ","))), timeout = 120)$result
    for (k in chunk) info[[k]] <- sm[[k]]
  }
  genes <- lapply(ids, function(k) {
    x <- info[[k]]
    list(gene = x$name %||% k, geneId = k, name = x$description %||% "", taxid = as.integer(x$organism$taxid %||% NA),
         articles = length(unique(per[[k]])), pmids = as.list(utils::head(unique(per[[k]]), 20)))
  })
  all <- length(genes)
  genes <- Filter(function(g) identical(g$taxid, as.integer(taxid)), genes)
  genes <- genes[order(-vapply(genes, `[[`, 0L, "articles"))]
  titles <- tryCatch({
    sm <- .eutils("esummary", db = "pubmed", id = paste(utils::head(pmids, 30), collapse = ","))$result
    lapply(utils::head(pmids, 30), function(p) list(pmid = p, title = sm[[p]]$title %||% "", year = substr(sm[[p]]$pubdate %||% "", 1, 4), journal = sm[[p]]$source %||% ""))
  }, error = function(e) list())
  list(articles = length(pmids), searched = searched, genes = genes, otherOrganisms = all - length(genes), organismTaxId = as.integer(taxid),
       titles = titles, ranked = TRUE,
       citation = "Genes linked to the articles in NCBI Gene (gene2pubmed: curated links and GeneRIFs). Ranked by the number of articles that mention each gene.")
}

# ---- GEO (RummaGEO) -------------------------------------------------------------

#' Search GEO signatures by series accession (GSE…) or keywords
geo_search <- function(query, size = 40L) {
  q <- trimws(as.character(query))
  if (nchar(q) < 3) stop("Type a GSE accession or at least 3 characters.")
  terms <- as.list(strsplit(q, "\\s+")[[1]])
  d <- .gql(RUMMAGEO_API, "query($t:[String]!,$n:Int!){ geneSetTermSearch(terms:$t, first:$n){ totalCount nodes { id term gse title sampleGroups geneSetById { nGeneIds species } } } }",
            list(t = terms, n = as.integer(size)))
  r <- d$geneSetTermSearch
  list(total = r$totalCount %||% 0L, sets = lapply(r$nodes, function(x) {
    groups <- if (is.character(x$sampleGroups)) tryCatch(jsonlite::fromJSON(x$sampleGroups, simplifyVector = FALSE), error = function(e) NULL) else x$sampleGroups
    m <- regmatches(x$term, regexec("-([0-9]+)-vs-([0-9]+)-", x$term))[[1]]
    ttl <- function(k) { t <- groups$titles[[k]] %||% paste("group", k); n <- length(groups$samples[[k]] %||% list()); if (n) sprintf("%s (n = %d)", t, n) else t }
    contrast <- if (length(m) == 3) list(a = ttl(m[2]), b = ttl(m[3])) else NULL
    list(id = x$id, term = x$term, gse = x$gse, title = x$title, size = x$geneSetById$nGeneIds, species = x$geneSetById$species,
         direction = if (grepl(" up$", x$term)) "up" else if (grepl(" dn$", x$term)) "down" else "",
         contrast = contrast)
  }))
}

geo_genes <- function(id) {
  d <- .gql(RUMMAGEO_API, "query($id:UUID!){ geneSet(id:$id){ term nGeneIds geneSetGsesById { nodes { gse } } genes(first:5000){ nodes { symbol } } } }", list(id = id))
  if (is.null(d$geneSet)) stop("Gene set not found.")
  sp <- if (grepl("mouse", d$geneSet$term)) 10090L else 9606L
  list(source = "RummaGEO", term = d$geneSet$term, organismTaxId = sp, ranked = FALSE,
       genes = lapply(d$geneSet$genes$nodes, function(x) list(gene = x$symbol)),
       citation = "RummaGEO (Marino et al. 2024, Patterns 5:101072): differential-expression signatures of GEO series computed from uniformly aligned ARCHS4 data (limma-voom).")
}

# ---- dbSNP ---------------------------------------------------------------------

#' rsIDs (or a dbSNP search) to variants and their genes, from NCBI dbSNP
dbsnp_lookup <- function(query, max = 300L) {
  q <- trimws(as.character(query))
  toks <- unlist(strsplit(q, "[\\s,;]+", perl = TRUE))
  if (length(toks) && all(grepl("^rs[0-9]+$", toks, ignore.case = TRUE))) {
    ids <- unique(sub("^rs", "", toks, ignore.case = TRUE)); searched <- NULL
  } else {
    s <- .eutils("esearch", db = "snp", term = q, retmax = as.integer(min(max, 1000)))
    ids <- unlist(s$esearchresult$idlist)
    searched <- list(query = q, count = as.integer(s$esearchresult$count %||% 0))
    if (!length(ids)) stop("No dbSNP records match this search. Examples: APOE[gene] AND pathogenic[clinical_significance], or rs429358 rs7412.")
  }
  ids <- utils::head(ids, max)
  vars <- list()
  for (chunk in split(ids, ceiling(seq_along(ids) / 200))) {
    sm <- .http_json(paste0(EUTILS, "/esummary.fcgi"), query = .ncbi_query(list(db = "snp", id = paste(chunk, collapse = ","))), timeout = 120)$result
    for (k in chunk) {
      x <- sm[[k]]
      if (is.null(x) || !is.null(x$error)) next
      mafs <- x$global_mafs %||% list()
      study <- vapply(mafs, function(m) m$study %||% "", "")
      pick <- c(which(study == "GnomAD_genomes"), seq_along(mafs))[1]
      gn <- if (length(mafs) && !is.na(pick)) paste0(mafs[[pick]]$freq %||% "", " (", study[pick], ")") else ""
      vars[[length(vars) + 1]] <- list(rsid = paste0("rs", k), chrpos = x$chrpos %||% "", genes = vapply(x$genes %||% list(), function(g) g$name %||% "", ""),
                                       consequence = gsub(",", ", ", x$fxn_class %||% ""), clinical = gsub(",", ", ", x$clinical_significance %||% ""),
                                       frequency = gn)
    }
  }
  gcount <- sort(table(unlist(lapply(vars, function(v) unique(v$genes)))), decreasing = TRUE)
  list(searched = searched, variants = vars, organismTaxId = 9606L, ranked = TRUE,
       genes = lapply(names(gcount), function(g) list(gene = g, variants = as.integer(gcount[[g]]))),
       citation = "NCBI dbSNP (Sherry et al. 2001, Nucleic Acids Res 29:308): variant locations, genes, functional classes, ClinVar significance and allele frequencies.")
}
