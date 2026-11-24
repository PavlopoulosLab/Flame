# Redundancy reduction of enrichment results.
#
# Three ways to decide that two terms say the same thing:
#   "genes"    - they share most of the list's genes (Jaccard index); works for every source.
#   "semantic" - they are close in the Gene Ontology graph (Wang similarity, the default
#                measure of clusterProfiler::simplify()); GO terms only.
#   "revigo"   - REVIGO (Supek et al. 2011) decides, with its semantic measures
#                (SimRel, Lin, Resnik, Jiang) and GO annotation frequencies; GO terms only.
# In every case terms are taken from the most to the least significant: each term not yet
# covered becomes a representative and absorbs the less significant similar terms of the
# same annotation source (REVIGO makes its own choice of representatives).

REDUNDANCY_METHODS <- c("genes", "semantic", "revigo")
REVIGO_MEASURES <- c("SIMREL", "LIN", "RESNIK", "JIANG")
REVIGO_CUTOFFS <- c(0.4, 0.5, 0.7, 0.9)
REVIGO_URLS <- c("https://revigo.irb.hr", "http://revigo.irb.hr")

.is_go_source <- function(src) grepl("^GO", src)
.ontology_of <- function(src) {
  o <- toupper(sub("^GO[:_ ]?", "", src))
  ifelse(o %in% c("BP", "MF", "CC"), o, NA_character_)
}

#' Reduce redundancy: keep one representative term per group of similar terms.
#'
#' @param df Filtered results (one row per term).
#' @param cutoff Similarity at or above which a term is merged into a representative:
#'   share of genes for "genes" (0.1-1, default 0.5), Wang similarity for "semantic"
#'   (default 0.7), REVIGO's allowed similarity for "revigo" (0.4, 0.5, 0.7 or 0.9).
#' @param maxTerms Only the most significant terms are compared; the rest stay as they are.
#' @param method "genes", "semantic" or "revigo".
#' @param taxid Organism (REVIGO uses it for GO term frequencies; unknown -> whole UniProt).
#' @param measure REVIGO measure: SIMREL, LIN, RESNIK or JIANG.
#' @return The representative rows of `df` with `Represents` (how many terms the row stands
#'   for, itself included) and `Merged` (absorbed terms as "name (ID)", " | "-separated).
reduce_redundancy <- function(df, cutoff = NULL, maxTerms = 3000L, method = "genes",
                              taxid = 9606L, measure = "SIMREL") {
  if (is.null(df) || nrow(df) == 0) return(df)
  method <- if (length(method) && method[1] %in% REDUNDANCY_METHODS) method[1] else "genes"
  cutoff <- suppressWarnings(as.numeric(cutoff)[1])
  if (!length(cutoff) || !is.finite(cutoff)) cutoff <- if (method == "genes") 0.5 else 0.7
  cutoff <- min(max(cutoff, 0.1), 1)
  df$Represents <- 1L; df$Merged <- ""
  if (nrow(df) < 2) return(df)
  ord <- order(.num(df$`P-value`), na.last = TRUE)
  work <- utils::head(ord, maxTerms)                      # the rest (least significant) stay as they are
  src <- as.character(df$Source)
  ids <- as.character(df$Term_ID_noLinks %||% df$Term_ID)
  nms <- as.character(df$Function)
  rep <- rep(NA_integer_, nrow(df))                       # row index of each term's representative

  greedy <- function(rows, sim) {                         # rows sorted by significance; sim[i, j] in the same order
    r <- integer(length(rows))
    for (i in seq_along(rows)) {
      if (r[i] != 0L) next
      r[i] <- i
      r[which(r == 0L & sim[i, ] >= cutoff)] <- i
    }
    rep[rows] <<- rows[r]
  }

  if (method == "genes") {
    gl <- lapply(as.character(df$`Positive Hits`[work]), function(g) unique(.split_hits(g)))
    allGenes <- unique(unlist(gl))
    if (!length(allGenes)) return(df)
    m <- length(work)
    inc <- Matrix::sparseMatrix(i = rep(seq_len(m), lengths(gl)), j = match(unlist(gl), allGenes), x = 1, dims = c(m, length(allGenes)))
    inter <- as.matrix(Matrix::tcrossprod(inc))
    sz <- lengths(gl)
    jac <- inter / (outer(sz, sz, "+") - inter)
    jac[!is.finite(jac)] <- 0
    jac[outer(src[work], src[work], "!=")] <- 0           # only terms of the same source are merged
    greedy(work, jac)
  } else if (method == "semantic") {
    for (s in unique(src[work])) {
      ont <- .ontology_of(s)
      if (is.na(ont)) next                                # KEGG, Reactome, ... stay as they are
      rows <- work[src[work] == s & grepl("^GO:\\d{7}$", ids[work])]
      if (length(rows) < 2) next
      greedy(rows, go_wang_similarity(ids[rows], ont))
    }
  } else {
    inp <- revigo_input(df, maxTerms)
    if (length(inp$ids) >= 2) {
      tab <- revigo_table(inp$ids, inp$pvalues, taxid, cutoff, measure)
      tab <- tab[tab$id %in% ids, , drop = FALSE]
      kept <- is.na(tab$representative)
      row <- match(tab$id, ids)
      repRow <- match(tab$representative, ids)
      rep[row[kept]] <- row[kept]
      ok <- !kept & !is.na(repRow) & repRow %in% row[kept]   # eliminated, with a representative we show
      rep[row[ok]] <- repRow[ok]
      rep[row[!kept & !ok]] <- row[!kept & !ok]
    }
  }

  absorbed <- which(!is.na(rep) & rep != seq_along(rep))
  if (length(absorbed)) {
    for (r in unique(rep[absorbed])) {
      a <- absorbed[rep[absorbed] == r]
      a <- a[order(.num(df$`P-value`[a]))]
      df$Represents[r] <- 1L + length(a)
      df$Merged[r] <- paste(sprintf("%s (%s)", nms[a], ids[a]), collapse = " | ")
    }
    df <- df[-absorbed, , drop = FALSE]
  }
  df
}

# ---- Wang semantic similarity (as GOSemSim / clusterProfiler::simplify) -----------------

.go_sem <- new.env()

#' GO parent relations: the table GOSemSim uses (from GO.db), so results match it exactly.
.go_relations <- function() {
  if (!is.null(.go_sem$rel)) return(.go_sem$rel)
  if (!requireNamespace("GOSemSim", quietly = TRUE) || !requireNamespace("GO.db", quietly = TRUE))
    stop("Semantic similarity needs the Bioconductor packages GOSemSim and GO.db.")
  tb <- tryCatch(utils::getFromNamespace("ensure_gotbl_cached", "GOSemSim")(), error = function(e) NULL)
  if (is.null(tb)) {
    tb <- do.call(rbind, lapply(c(BP = "GOBPPARENTS", MF = "GOMFPARENTS", CC = "GOCCPARENTS"), function(map) {
      p <- AnnotationDbi::toTable(getExportedValue("GO.db", map))
      data.frame(go_id = p[[1]], parent = p[[2]], relationship = p[[3]],
                 Ontology = sub("GO(..)PARENTS", "\\1", map), stringsAsFactors = FALSE)
    }))
  }
  .go_sem$rel <- tb[, c("go_id", "parent", "relationship", "Ontology")]
  .go_sem$rel
}

.wang_index <- function(ont) {
  key <- paste0("idx_", ont)
  if (!is.null(.go_sem[[key]])) return(.go_sem[[key]])
  tb <- .go_relations()
  tb <- tb[tb$Ontology == ont, , drop = FALSE]
  rel <- tb$relationship
  w <- ifelse(rel %in% c("isa", "is a", "is_a"), 0.8, ifelse(rel %in% c("part of", "part_of"), 0.6, 0.7))
  idx <- list(child = tb$go_id, parent = tb$parent, w = unname(w),
              rows = split(seq_len(nrow(tb)), tb$go_id), sv = new.env())
  .go_sem[[key]] <- idx
  idx
}

# Semantic values of a term and its ancestors, walked level by level exactly like
# GOSemSim:::getSV (each ancestor keeps the value of the first path that reaches it).
.wang_sv <- function(id, idx) {
  hit <- idx$sv[[id]]
  if (!is.null(hit)) return(hit)
  sv <- c(1); names(sv) <- id
  frontier <- id
  repeat {
    rows <- sort(unique(unlist(idx$rows[unique(frontier)], use.names = FALSE)))
    if (!length(rows)) break
    vals <- idx$w[rows] * sv[match(idx$child[rows], names(sv))]
    names(vals) <- idx$parent[rows]
    sv <- c(sv, vals)
    frontier <- idx$parent[rows]
  }
  sv <- sv[!is.na(names(sv)) & !duplicated(names(sv))]
  sv["all"] <- 0
  idx$sv[[id]] <- sv
  sv
}

#' Wang semantic similarity between GO terms of one ontology (matrix, diagonal 1).
#' Same values as GOSemSim::mgoSim(ids, ids, measure = "Wang", combine = NULL).
go_wang_similarity <- function(ids, ont = "BP") {
  ids <- as.character(ids)
  n <- length(ids)
  if (!n) return(matrix(numeric(0), 0, 0))
  idx <- .wang_index(ont)
  svs <- lapply(ids, .wang_sv, idx = idx)
  anc <- unique(unlist(lapply(svs, names), use.names = FALSE))
  i <- rep(seq_len(n), lengths(svs))
  j <- match(unlist(lapply(svs, names), use.names = FALSE), anc)
  S <- Matrix::sparseMatrix(i = i, j = j, x = unlist(svs, use.names = FALSE), dims = c(n, length(anc)))
  I <- Matrix::sparseMatrix(i = i, j = j, x = 1, dims = c(n, length(anc)))
  num <- as.matrix(Matrix::tcrossprod(S, I) + Matrix::tcrossprod(I, S))   # sum over shared ancestors of sv_a + sv_b
  tot <- vapply(svs, sum, 0)
  sim <- num / outer(tot, tot, "+")
  sim[!is.finite(sim)] <- 0
  diag(sim) <- 1
  dimnames(sim) <- list(ids, ids)
  sim
}

# ---- REVIGO ---------------------------------------------------------------------------

.revigo_cache <- new.env()

#' The GO terms (most significant first, at most `maxTerms`) REVIGO is asked about.
revigo_input <- function(df, maxTerms = 3000L) {
  if (is.null(df) || !nrow(df)) return(list(ids = character(0), pvalues = numeric(0)))
  ids <- as.character(df$Term_ID_noLinks %||% df$Term_ID)
  p <- .num(df$`P-value`)
  keep <- .is_go_source(as.character(df$Source)) & grepl("^GO:\\d{7}$", ids)
  ids <- ids[keep]; p <- p[keep]
  o <- order(p, na.last = TRUE)
  ids <- ids[o]; p <- p[o]
  d <- !duplicated(ids)
  ids <- utils::head(ids[d], maxTerms); p <- utils::head(p[d], maxTerms)
  p[!is.finite(p) | p <= 0] <- 1e-300
  list(ids = ids, pvalues = p)
}

.revigo_params <- function(taxid, cutoff, measure) {
  cutoff <- REVIGO_CUTOFFS[which.min(abs(REVIGO_CUTOFFS - (suppressWarnings(as.numeric(cutoff))[1] %||% 0.7)))]
  measure <- toupper(as.character(measure %||% "SIMREL"))[1]
  if (!measure %in% REVIGO_MEASURES) measure <- "SIMREL"
  taxid <- suppressWarnings(as.integer(taxid))[1]
  if (is.na(taxid)) taxid <- 0L
  list(taxid = taxid, cutoff = cutoff, measure = measure)
}

revigo_cache_key <- function(ids, pvalues, taxid, cutoff, measure) {
  p <- .revigo_params(taxid, cutoff, measure)
  key <- paste(p$taxid, p$cutoff, p$measure, paste(ids, signif(pvalues, 4), collapse = ";"), sep = "|")
  # R environment names are limited to 10,000 bytes: long term lists are stored under a hash.
  if (requireNamespace("sodium", quietly = TRUE)) sodium::bin2hex(sodium::sha256(charToRaw(key))) else digest_key(key)
}

#' REVIGO's verdict for each GO term: data frame (id, representative); representative is
#' NA for the terms REVIGO keeps. Cached in memory; `fetch = FALSE` only reads the cache.
revigo_table <- function(ids, pvalues, taxid = 9606L, cutoff = 0.7, measure = "SIMREL", fetch = TRUE) {
  key <- revigo_cache_key(ids, pvalues, taxid, cutoff, measure)
  hit <- .revigo_cache[[key]]
  if (!is.null(hit) || !fetch) return(hit)
  tab <- revigo_fetch(ids, pvalues, taxid, cutoff, measure)
  revigo_cache_set(key, tab)
  tab
}

revigo_cache_set <- function(key, tab) {
  if (length(ls(.revigo_cache)) > 200) rm(list = ls(.revigo_cache), envir = .revigo_cache)
  .revigo_cache[[key]] <- tab
  invisible(tab)
}

#' Ask the REVIGO web service (revigo.irb.hr, job API) to reduce a GO term list.
#' Only GO IDs and p-values are sent. Tries HTTPS first, then HTTP.
revigo_fetch <- function(ids, pvalues, taxid = 9606L, cutoff = 0.7, measure = "SIMREL", timeout = 120) {
  if (!requireNamespace("httr2", quietly = TRUE)) stop("REVIGO needs the R package httr2.")
  p <- .revigo_params(taxid, cutoff, measure)
  goList <- paste(ids, format(pvalues, digits = 4, scientific = TRUE, trim = TRUE), collapse = "\n")
  start <- function(base, species) {
    httr2::request(paste0(base, "/StartJob")) |>
      httr2::req_user_agent("Flame/3.0 (functional enrichment web server)") |>
      httr2::req_body_form(cutoff = as.character(p$cutoff), valueType = "PValue", speciesTaxon = as.character(species),
                           measure = p$measure, goList = goList) |>
      httr2::req_timeout(60) |> httr2::req_perform()
  }
  base <- NULL; resp <- NULL
  for (b in REVIGO_URLS) {
    resp <- tryCatch(start(b, p$taxid), error = function(e) NULL)
    if (!is.null(resp)) { base <- b; break }
  }
  if (is.null(resp)) stop("REVIGO (revigo.irb.hr) did not respond. Try again later, or use another method.")
  job <- tryCatch(httr2::resp_body_json(resp), error = function(e) list())
  if (is.null(job$jobid) && p$taxid != 0L) {             # organism unknown to REVIGO: use the whole UniProt
    job <- tryCatch(httr2::resp_body_json(start(base, 0L)), error = function(e) list())
  }
  if (is.null(job$jobid)) stop("REVIGO did not accept the term list", if (nzchar(job$message %||% "")) paste0(": ", job$message) else "", ".")
  q <- function(...) httr2::request(paste0(base, "/QueryJob")) |> httr2::req_url_query(jobid = job$jobid, ...) |>
    httr2::req_timeout(60) |> httr2::req_perform()
  t0 <- Sys.time()
  repeat {
    st <- tryCatch(httr2::resp_body_json(q(type = "jstatus")), error = function(e) list(running = 1))
    if (identical(as.integer(st$running), 0L)) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) stop("REVIGO took too long. Try again later.")
    Sys.sleep(1)
  }
  out <- lapply(1:3, function(ns) {
    txt <- tryCatch(httr2::resp_body_string(q(namespace = ns, type = "Table")), error = function(e) "")
    if (!nzchar(trimws(txt))) return(NULL)
    t <- utils::read.delim(text = txt, colClasses = "character", quote = "\"", check.names = FALSE)
    if (!all(c("TermID", "Representative") %in% names(t))) return(NULL)
    r <- suppressWarnings(as.integer(t$Representative))
    data.frame(id = t$TermID, representative = ifelse(is.na(r), NA_character_, sprintf("GO:%07d", r)),
               stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, out)
  if (is.null(tab)) tab <- data.frame(id = character(0), representative = character(0), stringsAsFactors = FALSE)
  # Terms REVIGO does not know (obsolete IDs) are kept.
  missing <- setdiff(ids, tab$id)
  if (length(missing)) tab <- rbind(tab, data.frame(id = missing, representative = NA_character_, stringsAsFactors = FALSE))
  tab
}
