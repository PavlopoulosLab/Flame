# =============================================================================
# FLAME core: gene expression matrices as an input
# =============================================================================
# A table with one row per gene (first column = gene ID) and one numeric column
# per sample. Flame offers three ways to pick genes from it:
#   * clusters of co-expressed genes (hierarchical clustering of the most
#     variable genes, shown as a heatmap of z-scores),
#   * a gene map (PCA of the genes' z-scored profiles) for lasso selection,
#   * a two-group comparison (Welch t-test, log2 fold change, BH) that opens
#     in the volcano selector.
# Base R only (stats::prcomp, hclust, t.test).
# =============================================================================

EXPRESSION_MAX_BYTES <- 2 * 1024^2
EXPRESSION_MAX_GENES <- 30000L

#' Parse an expression matrix. Values that look like counts or intensities
#' (max > 50) are log2(x + 1) transformed unless `log` is FALSE.
parse_expression_matrix <- function(text, fileName = "", log = NA) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  if (!nzchar(trimws(text))) stop("Paste an expression table or upload a .txt, .csv or .tsv file.")
  if (nchar(text, type = "bytes") > EXPRESSION_MAX_BYTES) stop("The expression table is larger than the 2 MB limit.")
  df <- parse_delimited_table(text, fileName)
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(df) < 3) stop("Expected a gene ID column and at least two sample columns.")
  ids <- trimws(as.character(df[[1]]))
  num <- lapply(df[-1], function(x) suppressWarnings(as.numeric(as.character(x))))
  isNum <- vapply(num, function(x) mean(is.finite(x)) > 0.8, logical(1))
  if (sum(isNum) < 2) stop("Expected at least two numeric sample columns after the gene ID column.")
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
    samples = colnames(z)[sampleOrder], logged = x$logged, totalGenes = nrow(m), shownGenes = nrow(z), dropped = x$dropped,
    genes = lapply(seq_along(genes), function(i) list(id = genes[i], cluster = cl[i], x = round(pc$x[i, 1], 3), y = round(pc$x[i, min(2, ncol(pc$x))], 3),
                                                      sd = round(stats::sd(sel[i, ]), 3))),
    heatmap = list(genes = genes[ord], clusters = cl[ord], z = unname(lapply(ord, function(i) round(z[i, sampleOrder], 2)))),
    clusterSizes = as.list(table(factor(cl, levels = seq_len(max(cl))))),
    variance = var[seq_len(min(2, length(var)))]
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

#' A small simulated expression matrix (6 samples, 2 groups) whose fold changes
#' follow the example differential-expression table, for trying the input out.
example_expression <- function() {
  df <- as.data.frame(readRDS(getExamplesPath("volcano_example.RDS")), stringsAsFactors = FALSE)
  df <- df[named_gene(df$symbol) & is.finite(df$logFC), , drop = FALSE]
  df <- df[!duplicated(df$symbol), , drop = FALSE]
  df <- df[order(df$pvalue), , drop = FALSE][seq_len(min(3000, nrow(df))), ]
  set.seed(42)
  base <- stats::rnorm(nrow(df), 7, 1.5)
  module <- sample(c(0, 0, 0, 1, -1), nrow(df), replace = TRUE)   # co-regulated modules across samples
  noise <- function() stats::rnorm(nrow(df), 0, 0.15)
  ctrl <- sapply(1:3, function(j) base + noise() + 0.25 * module * (j - 2))
  trt <- sapply(1:3, function(j) base + df$logFC + noise() + 0.25 * module * (j - 2))
  m <- round(2^cbind(ctrl, trt) - 1, 1)
  out <- data.frame(gene = df$symbol, m, check.names = FALSE)
  names(out) <- c("gene", "control_1", "control_2", "control_3", "treated_1", "treated_2", "treated_3")
  list(text = .table_text(out), fileName = "flame_example_expression.tsv",
       note = "Simulated counts for 3,000 genes in 3 control and 3 treated samples; fold changes follow the example differential-expression table.")
}
