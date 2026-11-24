# =============================================================================
# FLAME core: gene expression matrices as an input
# =============================================================================
# A table with one row per gene (first column = gene ID) and one numeric column
# per sample. Flame offers three ways to pick genes from it:
#   * clusters of co-expressed genes (hierarchical clustering of the most
#     variable genes, shown as a heatmap of z-scores),
#   * a gene map (PCA of the genes' z-scored profiles) for lasso selection,
#   * a two-group comparison (Welch t-test, log2 fold change, BH) that opens
#     in the volcano selector,
#   * a gene of interest: its correlation with every other gene (Pearson,
#     Spearman, Kendall) and boxplots of genes by the two groups.
# Base R only (stats::prcomp, hclust, t.test, cor).
# =============================================================================

EXPRESSION_MAX_BYTES <- as.numeric(Sys.getenv("FLAME_MAX_EXPRESSION_MB", "20")) * 1024^2   # survival cohorts need room
EXPRESSION_MAX_GENES <- 30000L
EXPRESSION_MAX_SAMPLES <- 5000L
KENDALL_MAX_WORK <- 3e8        # genes x samples^2: Kendall's tau is quadratic in the samples (about a second at this size)

#' Parse an expression matrix. Values that look like counts or intensities
#' (max > 50) are log2(x + 1) transformed unless `log` is FALSE.
parse_expression_matrix <- function(text, fileName = "", log = NA) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  if (!nzchar(trimws(text))) stop("Paste an expression table or upload a .txt, .csv or .tsv file.")
  if (nchar(text, type = "bytes") > EXPRESSION_MAX_BYTES)
    stop(sprintf("The expression table is larger than the %d MB limit.", round(EXPRESSION_MAX_BYTES / 1024^2)))
  # Each view of the dialog sends the table again: reuse the parsed matrix.
  key <- paste(if (requireNamespace("sodium", quietly = TRUE)) sodium::bin2hex(sodium::sha256(charToRaw(text))) else nchar(text),
               fileName, log)
  hit <- .expr_cache[[key]]
  if (!is.null(hit)) return(hit)
  out <- .parse_expression_matrix(text, fileName, log)
  if (length(ls(.expr_cache)) >= 4) rm(list = ls(.expr_cache), envir = .expr_cache)
  .expr_cache[[key]] <- out
  out
}
.expr_cache <- new.env()

.parse_expression_matrix <- function(text, fileName, log) {
  df <- parse_delimited_table(text, fileName, maxBytes = EXPRESSION_MAX_BYTES)
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(df) < 3) stop("Expected a gene ID column and at least two sample columns.")
  ids <- trimws(as.character(df[[1]]))
  num <- lapply(df[-1], function(x) suppressWarnings(as.numeric(as.character(x))))
  isNum <- vapply(num, function(x) mean(is.finite(x)) > 0.8, logical(1))
  if (sum(isNum) < 2) stop("Expected at least two numeric sample columns after the gene ID column.")
  if (sum(isNum) > EXPRESSION_MAX_SAMPLES) stop(sprintf("At most %d samples are supported.", EXPRESSION_MAX_SAMPLES))
  m <- do.call(cbind, num[isNum])
  colnames(m) <- make.unique(substr(names(df)[-1][isNum], 1, 60))
  ok <- nzchar(ids) & !is.na(ids) & grepl("^[A-Za-z0-9_.:-]+$", ids) & rowSums(is.finite(m)) == ncol(m)
  m <- m[ok, , drop = FALSE]; ids <- ids[ok]
  keep <- !duplicated(toupper(ids))
  m <- m[keep, , drop = FALSE]; ids <- ids[keep]
  if (nrow(m) < 10) stop("Fewer than 10 genes with complete numeric values were found.")
  if (nrow(m) > EXPRESSION_MAX_GENES) stop(sprintf("At most %d genes are supported.", EXPRESSION_MAX_GENES))
  doLog <- if (is.na(log)) max(m) > 50 && min(m) >= 0 else isTRUE(log)
  if (doLog) m <- log2(pmax(m, 0) + 1)
  rownames(m) <- ids
  list(matrix = m, logged = doLog, samples = colnames(m), dropped = sum(!ok) + sum(!keep))
}

#' Clusters, heatmap and gene PCA of the most variable genes
expression_overview <- function(x, topN = 1000L, k = 6L) {
  m <- x$matrix
  topN <- max(20L, min(as.integer(topN), nrow(m), 3000L))
  k <- max(2L, min(as.integer(k), 20L))
  v <- apply(m, 1, stats::var)
  sel <- m[order(-v)[seq_len(topN)], , drop = FALSE]
  sel <- sel[apply(sel, 1, stats::sd) > 0, , drop = FALSE]
  z <- t(scale(t(sel)))
  hc <- stats::hclust(stats::as.dist(1 - stats::cor(t(z))), method = "average")
  cl <- stats::cutree(hc, k = min(k, nrow(z) - 1))
  # Number clusters by their order along the dendrogram, so colours run top to bottom.
  ord <- hc$order
  first <- tapply(seq_along(ord), cl[ord], min)
  relabel <- stats::setNames(rank(first), names(first))
  cl <- as.integer(relabel[as.character(cl)])
  pc <- stats::prcomp(z, center = TRUE, scale. = FALSE)
  var <- round(100 * pc$sdev^2 / sum(pc$sdev^2), 1)
  sampleOrder <- if (ncol(z) > 2) stats::hclust(stats::as.dist(1 - stats::cor(z)), method = "average")$order else seq_len(ncol(z))
  genes <- rownames(z)
  list(
    samples = as.list(colnames(z)[sampleOrder]), logged = x$logged, totalGenes = nrow(m), shownGenes = nrow(z), dropped = x$dropped,
    genes = lapply(seq_along(genes), function(i) list(id = genes[i], cluster = cl[i], x = round(pc$x[i, 1], 3), y = round(pc$x[i, min(2, ncol(pc$x))], 3),
                                                      sd = round(stats::sd(sel[i, ]), 3))),
    heatmap = list(genes = as.list(genes[ord]), clusters = as.list(cl[ord]), z = unname(lapply(ord, function(i) as.list(unname(round(z[i, sampleOrder], 2)))))),
    clusterSizes = as.list(table(factor(cl, levels = seq_len(max(cl))))),
    variance = as.list(var[seq_len(min(2, length(var)))])
  )
}

#' Two-group comparison (Welch t-test per gene). Returns a table for the volcano selector.
expression_de <- function(x, groupA, groupB) {
  m <- x$matrix
  a <- intersect(as.character(unlist(groupA)), colnames(m)); b <- intersect(as.character(unlist(groupB)), colnames(m))
  if (length(a) < 2 || length(b) < 2) stop("Choose at least two samples in each group.")
  if (length(intersect(a, b))) stop("A sample cannot be in both groups.")
  A <- m[, a, drop = FALSE]; B <- m[, b, drop = FALSE]
  lfc <- rowMeans(A) - rowMeans(B)
  p <- vapply(seq_len(nrow(m)), function(i) {
    if (stats::sd(A[i, ]) == 0 && stats::sd(B[i, ]) == 0) return(if (lfc[i] == 0) 1 else NA_real_)
    tryCatch(stats::t.test(A[i, ], B[i, ])$p.value, error = function(e) NA_real_)
  }, numeric(1))
  ok <- is.finite(p)
  df <- data.frame(gene = rownames(m)[ok], log2FC = round(lfc[ok], 4), pvalue = signif(p[ok], 4),
                   padj = signif(stats::p.adjust(p[ok], "BH"), 4), stringsAsFactors = FALSE)
  df <- df[order(df$pvalue), , drop = FALSE]
  list(text = .table_text(df), fileName = "flame_expression_de.tsv", n = nrow(df),
       significant = sum(df$padj <= 0.05), groupA = as.list(a), groupB = as.list(b),
       note = if (x$logged) "Values were log2(x + 1) transformed, so log2FC is a difference of means on the log scale." else "Values were used as given (assumed already on a log scale).")
}

# ---- Gene of interest: correlation and group boxplots ---------------------------

CORRELATION_METHODS <- c("pearson", "spearman", "kendall")

.find_gene <- function(m, gene) {
  g <- trimws(as.character(gene %||% ""))
  if (!nzchar(g)) stop("Type the gene of interest.")
  hit <- match(toupper(g), toupper(rownames(m)))
  if (is.na(hit)) {
    near <- utils::head(rownames(m)[startsWith(toupper(rownames(m)), toupper(g))], 5)
    stop(sprintf("'%s' is not in the table.%s", g, if (length(near)) paste0(" Did you mean: ", paste(near, collapse = ", "), "?") else ""))
  }
  hit
}

#' Two-sided p-values of Kendall's tau for n samples: exact null distribution of
#' the number of discordant pairs (Mahonian numbers) when n < 50, else normal.
.kendall_p <- function(tau, n) {
  if (n >= 50) return(2 * stats::pnorm(-abs(3 * tau * sqrt(n * (n - 1)) / sqrt(2 * (2 * n + 5)))))
  d <- 1
  for (k in 2:n) { cs <- cumsum(c(d, rep(0, k - 1))); d <- (cs - c(rep(0, k), utils::head(cs, -k))) / k }
  lower <- cumsum(d); upper <- rev(cumsum(rev(d)))
  pairs <- n * (n - 1) / 2
  disc <- pmin(pairs, pmax(0, round((1 - tau) * pairs / 2)))
  pmin(1, 2 * pmin(lower[disc + 1], upper[disc + 1]))
}

.row_sd <- function(m) sqrt(pmax(0, (rowSums(m^2) - rowSums(m)^2 / ncol(m)) / (ncol(m) - 1)))

#' Correlate one gene with every other gene of the matrix.
#' Pearson and Spearman p-values use the t distribution (n - 2 df); Kendall's
#' is exact for fewer than 50 samples (as in stats::cor.test, ignoring ties) and
#' a normal approximation above. BH adjustment per method.
#' `samples` optionally restricts the samples used (at least 4).
expression_correlate <- function(x, gene, methods = CORRELATION_METHODS, samples = NULL) {
  m <- x$matrix
  methods <- intersect(tolower(as.character(unlist(methods))), CORRELATION_METHODS)
  if (!length(methods)) stop("Choose at least one correlation method.")
  s <- intersect(as.character(unlist(samples)), colnames(m))
  if (length(s)) m <- m[, s, drop = FALSE]
  n <- ncol(m)
  if (n < 4) stop("Correlation needs at least 4 samples.")
  hit <- .find_gene(m, gene)
  goi <- rownames(m)[hit]
  g <- m[hit, ]
  if (stats::sd(g) == 0) stop(sprintf("%s has the same value in every sample, so it cannot be correlated.", goi))
  keep <- .row_sd(m) > 1e-12
  keep[hit] <- FALSE
  M <- m[keep, , drop = FALSE]
  if (nrow(M) < 2) stop("No other genes vary across these samples.")
  tp <- function(r) { r <- pmin(pmax(r, -1), 1); t <- r * sqrt((n - 2) / pmax(1e-300, 1 - r^2)); 2 * stats::pt(-abs(t), n - 2) }
  skipped <- ""
  if ("kendall" %in% methods && as.numeric(nrow(M)) * n^2 > KENDALL_MAX_WORK) {
    methods <- setdiff(methods, "kendall")
    if (!length(methods)) methods <- "spearman"
    skipped <- sprintf("Kendall's tau was left out: with %d samples and %s genes it would take too long; Spearman's rho ranks in the same way. ",
                       n, format(nrow(M), big.mark = ","))
  }
  out <- list()
  for (meth in methods) {
    r <- switch(meth,
      pearson = as.numeric(stats::cor(t(M), g)),
      spearman = as.numeric(stats::cor(apply(M, 1, rank), rank(g))),
      kendall = as.numeric(stats::cor(t(M), g, method = "kendall")))
    r[!is.finite(r)] <- 0
    p <- switch(meth,
      pearson = tp(r),
      spearman = pmax(tp(r), 2 / factorial(min(n, 170))),   # never below the exact minimum for n samples
      kendall = .kendall_p(r, n))
    out[[meth]] <- list(r = round(r, 4), p = signif(p, 4), padj = signif(stats::p.adjust(p, "BH"), 4))
  }
  list(gene = goi, n = n, samples = as.list(colnames(m)), methods = as.list(methods), genes = as.list(rownames(M)),
       tested = nrow(M), constant = sum(!keep) - 1L, logged = x$logged, values = as.list(unname(round(g, 4))),
       results = out,
       note = paste0(skipped, if (n < 8) sprintf("Only %d samples: correlations are unstable, and with thousands of genes few can reach a low FDR. ", n) else "",
                     "Pearson measures linear association; Spearman and Kendall use ranks and resist outliers. P-values are Benjamini-Hochberg adjusted per method."))
}

#' Correlation of two genes across the samples, by each method (stats::cor.test:
#' exact p-values for Spearman and Kendall in small samples without ties), with
#' their values per sample for a scatter plot.
expression_pair <- function(x, geneA, geneB, methods = CORRELATION_METHODS) {
  m <- x$matrix
  methods <- intersect(tolower(as.character(unlist(methods))), CORRELATION_METHODS)
  if (!length(methods)) methods <- CORRELATION_METHODS
  ia <- .find_gene(m, geneA); ib <- .find_gene(m, geneB)
  if (ia == ib) stop("Choose two different genes.")
  if (ncol(m) < 3) stop("Correlation needs at least 3 samples.")
  va <- m[ia, ]; vb <- m[ib, ]
  for (k in c(ia, ib)) if (stats::sd(m[k, ]) == 0) stop(sprintf("%s has the same value in every sample, so it cannot be correlated.", rownames(m)[k]))
  stats <- lapply(methods, function(meth) {
    ct <- suppressWarnings(stats::cor.test(va, vb, method = meth))
    ci <- if (!is.null(ct$conf.int)) round(as.numeric(ct$conf.int), 4) else c(NA_real_, NA_real_)
    list(method = meth, r = round(unname(ct$estimate), 4), p = signif(ct$p.value, 4), ciLow = ci[1], ciHigh = ci[2])
  })
  list(geneA = rownames(m)[ia], geneB = rownames(m)[ib], n = ncol(m), samples = as.list(colnames(m)), logged = x$logged,
       a = as.list(unname(round(va, 4))), b = as.list(unname(round(vb, 4))), stats = stats,
       note = if (ncol(m) < 8) sprintf("Only %d samples: one sample can change the correlation a lot; check the plot for outliers.", ncol(m)) else "")
}

#' Values of a few genes per sample, split into 2 or 3 sample groups, with tests.
#' `groups`: named list of sample vectors (2-3 groups, at least 2 samples each).
#' `test`: "parametric" (Welch t-test; Welch one-way ANOVA for 3 groups) or
#' "rank" (Wilcoxon rank-sum; Kruskal-Wallis for 3 groups). Pairwise tests
#' between groups are Holm-adjusted within each gene.
expression_gene_values <- function(x, genes, groups = NULL, test = "parametric") {
  m <- x$matrix
  genes <- unique(trimws(as.character(unlist(genes))))
  genes <- genes[nzchar(genes)]
  if (!length(genes)) stop("Choose at least one gene.")
  if (length(genes) > 24) genes <- genes[1:24]
  test <- if (identical(test, "rank")) "rank" else "parametric"
  grp <- list(); used <- character(0)
  for (nm in names(groups %||% list())) {
    ss <- setdiff(intersect(as.character(unlist(groups[[nm]])), colnames(m)), used)
    used <- c(used, ss)
    if (length(ss)) grp[[substr(nm, 1, 40)]] <- ss
  }
  if (length(grp) > 3) stop("Use at most 3 groups.")
  testable <- length(grp) >= 2 && all(lengths(grp) >= 2)
  idx <- vapply(genes, function(g) match(toupper(g), toupper(rownames(m))), integer(1))
  missing <- genes[is.na(idx)]
  idx <- idx[!is.na(idx)]
  if (!length(idx)) stop(sprintf("None of these genes is in the table: %s.", paste(genes, collapse = ", ")))
  sp <- function(p) if (is.finite(p)) signif(p, 4) else NA_real_
  two <- function(a, b) suppressWarnings(tryCatch(
    if (test == "rank") stats::wilcox.test(a, b, exact = length(a) + length(b) < 50 && !anyDuplicated(c(a, b)))$p.value else stats::t.test(a, b)$p.value,
    error = function(e) NA_real_))
  pairs <- if (length(grp) >= 2) utils::combn(names(grp), 2, simplify = FALSE) else list()
  list(samples = as.list(colnames(m)), groups = lapply(grp, as.list), logged = x$logged, missing = as.list(missing), test = test,
       testName = if (!testable) "" else if (length(grp) == 2) (if (test == "rank") "Wilcoxon rank-sum test" else "Welch t-test")
                  else if (test == "rank") "Kruskal-Wallis test; pairwise Wilcoxon rank-sum tests (Holm-adjusted)" else "Welch one-way ANOVA; pairwise Welch t-tests (Holm-adjusted)",
       genes = lapply(unname(idx), function(i) {
         v <- m[i, ]
         pw <- if (testable) vapply(pairs, function(pr) two(v[grp[[pr[1]]]], v[grp[[pr[2]]]]), numeric(1)) else numeric(0)
         overall <- if (!testable) NA_real_ else if (length(grp) == 2) pw[1] else suppressWarnings(tryCatch({
           y <- unlist(lapply(grp, function(ss) v[ss])); f <- factor(rep(names(grp), lengths(grp)), levels = names(grp))
           if (test == "rank") stats::kruskal.test(y, f)$p.value else stats::oneway.test(y ~ f, var.equal = FALSE)$p.value
         }, error = function(e) NA_real_))
         adj <- if (length(pw) > 1) stats::p.adjust(pw, "holm") else pw
         list(gene = rownames(m)[i], values = as.list(unname(round(v, 4))), pvalue = sp(overall),
              means = lapply(grp, function(ss) round(mean(v[ss]), 4)),
              pairwise = lapply(seq_along(pairs), function(k) list(a = pairs[[k]][1], b = pairs[[k]][2], log2FC = round(mean(v[grp[[pairs[[k]][1]]]]) - mean(v[grp[[pairs[[k]][2]]]]), 4),
                                                                   pvalue = sp(pw[k]), padj = sp(adj[k]))))
       }))
}
