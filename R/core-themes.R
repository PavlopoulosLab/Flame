# =============================================================================
# FLAME core: functional themes and word clouds
# =============================================================================
# Enrichment tools report many overlapping terms. Terms that share most of the
# user's genes describe the same biology, so they are grouped into themes:
#   similarity = Jaccard index of the genes each term found (the "kappa"-like
#   overlap used by DAVID / EnrichmentMap), edges for similarity >= cutoff,
#   themes = Louvain communities of that graph (igraph).
# Each theme is named after its most significant term and summarised by
# keywords: words of its term names weighted by significance (also used for the
# word clouds).
# =============================================================================

THEME_STOPWORDS <- c(
  "of", "the", "and", "in", "to", "by", "via", "or", "a", "an", "on", "with", "from", "into", "for", "as", "at",
  "process", "processes", "regulation", "positive", "negative", "cellular", "cell", "cells", "pathway", "pathways",
  "involved", "activity", "response", "signaling", "signalling", "protein", "proteins", "gene", "genes", "binding",
  "complex", "organization", "metabolic", "biosynthetic", "catabolic", "system", "mediated", "dependent", "type",
  "mitotic", "human", "mouse", "rat", "hsa", "mus", "musculus", "homo", "sapiens", "other", "related", "level", "levels", "term",
  "up", "down", "dn", "v1", "v2", "kegg", "reactome", "wikipathways", "hallmark", "go", "class", "family", "member",
  "establishment", "maintenance", "development", "developmental", "structure", "formation")

#' Words of term names weighted by significance (for word clouds)
term_keywords <- function(names, weights, max = 60L) {
  if (!length(names)) return(list())
  toks <- lapply(tolower(names), function(n) {
    w <- unlist(strsplit(gsub("[^a-z0-9\\-]+", " ", n), "\\s+"))
    w <- gsub("^-+|-+$", "", w)
    unique(w[nchar(w) >= 3 & !w %in% THEME_STOPWORDS & !grepl("^[0-9-]+$", w)])
  })
  score <- tapply(rep(weights, lengths(toks)), unlist(toks), sum)
  if (!length(score)) return(list())
  score <- sort(score, decreasing = TRUE)[seq_len(min(max, length(score)))]
  lapply(names(score), function(w) list(word = w, weight = round(unname(score[[w]]), 3)))
}

#' Group terms into themes.
#' terms: data.frame with id, name, source, pvalue, genes (comma-separated); optional runs (list column).
term_themes <- function(terms, cutoff = 0.35, maxTerms = 800L) {
  terms <- terms[order(terms$pvalue), , drop = FALSE]
  terms <- utils::head(terms, maxTerms)
  n <- nrow(terms)
  if (!n) return(list(themes = list(), words = list(), termCount = 0L))
  gl <- lapply(terms$genes, function(g) unique(trimws(unlist(strsplit(g %||% "", ",")))))
  w <- -log10(pmax(terms$pvalue, 1e-300))
  # Pairwise Jaccard similarity via a sparse term x gene incidence matrix.
  allGenes <- unique(unlist(gl))
  inc <- Matrix::sparseMatrix(i = rep(seq_len(n), lengths(gl)), j = match(unlist(gl), allGenes), x = 1, dims = c(n, length(allGenes)))
  inter <- as.matrix(Matrix::tcrossprod(inc))
  sz <- lengths(gl)
  jac <- inter / (outer(sz, sz, "+") - inter)
  jac[!is.finite(jac)] <- 0
  diag(jac) <- 0
  edges <- which(jac >= cutoff & upper.tri(jac), arr.ind = TRUE)
  g <- igraph::make_empty_graph(n, directed = FALSE)
  if (nrow(edges)) g <- igraph::add_edges(g, as.vector(t(edges)), weight = jac[edges])
  memb <- if (igraph::ecount(g)) as.integer(igraph::membership(igraph::cluster_louvain(g, weights = igraph::E(g)$weight))) else seq_len(n)
  groups <- split(seq_len(n), memb)
  groups <- groups[order(vapply(groups, function(ix) min(terms$pvalue[ix]), 0))]
  themes <- lapply(seq_along(groups), function(k) {
    ix <- groups[[k]]
    best <- ix[which.min(terms$pvalue[ix])]
    genes <- unique(unlist(gl[ix]))
    th <- list(
      id = paste0("theme", k), label = terms$name[best], leadTerm = terms$id[best],
      size = length(ix), bestP = terms$pvalue[best], score = round(sum(w[ix]), 2),
      terms = as.list(terms$id[ix]), genes = as.list(genes),
      sources = as.list(table(terms$source[ix])),
      keywords = term_keywords(terms$name[ix], w[ix], 25L),
      cohesion = if (length(ix) > 1) round(mean(jac[ix, ix][upper.tri(jac[ix, ix])]), 3) else 1)
    if ("runs" %in% names(terms)) {
      rr <- unlist(terms$runs[ix])
      th$runs <- lapply(split(seq_along(rr), rr), function(z) list(terms = length(z)))
    }
    th
  })
  list(themes = themes, words = term_keywords(terms$name, w, 80L), termCount = n, cutoff = cutoff,
       method = "Terms linked when they share genes (Jaccard >= cutoff), grouped by Louvain community detection.")
}

#' Reduce redundancy: keep one representative term per group of overlapping terms.
#'
#' Terms are taken from the most to the least significant. Each term not yet covered
#' becomes a representative and absorbs every less significant term of the same
#' annotation source whose genes overlap its own by at least `cutoff` (Jaccard index of
#' the genes of the list found in each term). This is the greedy scheme of
#' clusterProfiler::simplify() and REVIGO, with gene overlap instead of semantic
#' similarity, so it works for every source (GO, KEGG, Reactome, custom sets...).
#' Jaccard (not containment) is used on purpose: a small specific term is not swallowed
#' by a broad one that merely contains it.
#'
#' Returns the representative rows of `df` with two extra columns: `Represents`
#' (how many terms the row stands for, itself included) and `Merged` (the absorbed
#' terms as "name (ID)", separated by " | ").
reduce_redundancy <- function(df, cutoff = 0.5, maxTerms = 3000L) {
  if (is.null(df) || nrow(df) == 0) return(df)
  cutoff <- suppressWarnings(as.numeric(cutoff)[1]); if (!is.finite(cutoff)) cutoff <- 0.5
  cutoff <- min(max(cutoff, 0.1), 1)
  df$Represents <- 1L; df$Merged <- ""
  n <- nrow(df)
  if (n < 2) return(df)
  ord <- order(.num(df$`P-value`), na.last = TRUE)
  work <- utils::head(ord, maxTerms)                      # the rest (least significant) stay as they are
  m <- length(work)
  gl <- lapply(as.character(df$`Positive Hits`[work]), function(g) unique(.split_hits(g)))
  allGenes <- unique(unlist(gl))
  if (!length(allGenes)) return(df)
  inc <- Matrix::sparseMatrix(i = rep(seq_len(m), lengths(gl)), j = match(unlist(gl), allGenes), x = 1, dims = c(m, length(allGenes)))
  inter <- as.matrix(Matrix::tcrossprod(inc))
  sz <- lengths(gl)
  jac <- inter / (outer(sz, sz, "+") - inter)
  jac[!is.finite(jac)] <- 0
  src <- as.character(df$Source[work])
  ids <- as.character(df$Term_ID_noLinks %||% df$Term_ID)[work]
  nms <- as.character(df$Function[work])
  rep <- integer(m)
  for (i in seq_len(m)) {
    if (rep[i] != 0L) next
    rep[i] <- i
    absorbed <- which(rep == 0L & jac[i, ] >= cutoff & src == src[i])
    if (length(absorbed)) {
      rep[absorbed] <- i
      df$Represents[work[i]] <- 1L + length(absorbed)
      df$Merged[work[i]] <- paste(sprintf("%s (%s)", nms[absorbed], ids[absorbed]), collapse = " | ")
    }
  }
  keep <- rep(TRUE, n)
  keep[work[rep != seq_len(m)]] <- FALSE
  df[keep, , drop = FALSE]
}

#' Themes for one run (filtered results)
run_themes <- function(results, filters = list(), cutoff = 0.35) {
  df <- apply_result_filters(results, filters)
  if (is.null(df) || !nrow(df)) stop("No terms match the current filters.")
  terms <- data.frame(id = as.character(df$Term_ID_noLinks %||% df$Term_ID), name = as.character(df$Function),
                      source = as.character(df$Source), pvalue = as.numeric(df$`P-value`),
                      genes = as.character(df$`Positive Hits`), stringsAsFactors = FALSE)
  term_themes(terms, cutoff)
}

#' Themes across compared runs: which runs support each theme
comparison_themes <- function(cs, filters = list(), cutoff = 0.35) {
  filters <- .with_default_runs(cs, filters)
  active <- as.character(unlist(filters[["runs"]]))
  data <- cs$applyComparisonFilters(cs$getResults(), filters)$data
  if (is.null(data) || !nrow(data)) stop("No terms match the current filters.")
  membership <- cs$getRunMembership()
  ids <- as.character(data$Term_ID_noLinks)
  terms <- data.frame(id = ids, name = as.character(data$Function), source = as.character(data$Source),
                      pvalue = suppressWarnings(as.numeric(data$CombPvalue)),
                      genes = as.character(data$Union_Hits), stringsAsFactors = FALSE)
  terms$pvalue[!is.finite(terms$pvalue)] <- 1
  terms$runs <- lapply(ids, function(t) intersect(membership[[t]] %||% character(0), active))
  out <- term_themes(terms, cutoff)
  out$runs <- lapply(comparison_runs_meta(cs), function(r) r[c("id", "label", "tool", "listName")])
  out$activeRunIds <- as.list(active)
  out
}
