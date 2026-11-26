# Builds the TCGA "tumour vs normal" table: for every TCGA cancer type with enough
# adjacent normal tissue samples, each gene's log2 fold change (primary tumour vs normal)
# and Benjamini-Hochberg q-value.
#   inst/extdata/tcga_tumor_normal.rds   list(meta = ..., cancers = list(<code> = list(n, genes data.frame)))
#
# Data: TCGA STAR gene counts (GDC data release, GENCODE v36) from the UCSC Xena GDC hub,
#   https://gdc.xenahubs.net (Goldman et al. 2020, Nat Biotechnol 38:675). Xena stores log2(count + 1).
# Samples: primary solid tumours (TCGA sample type 01) vs solid tissue normal (11); one aliquot
#   per patient and type. Cancer types with fewer than MIN_NORMALS normals are skipped.
# Method: edgeR filterByExpr + TMM, limma-voom, unpaired tumour-vs-normal design
#   (Law et al. 2014, Genome Biol 15:R29). Genes kept: those with an Entrez ID (org.Hs.eg.db);
#   several Ensembl IDs for one symbol are summed.
# Codes follow the cBioPortal PanCancer Atlas studies Flame uses (COADREAD = COAD + READ).
#
#   Rscript data-raw/build_tcga_tumor_normal.R          (needs limma, edgeR, data.table, org.Hs.eg.db)
# Downloads (~2 GB in all) are kept in $FLAME_TCGA_CACHE (default ~/.cache/flame-tcga).
suppressPackageStartupMessages({ library(data.table); library(limma); library(edgeR) })

MIN_NORMALS <- 9
HUB <- "https://gdc.xenahubs.net/download"
PROJECTS <- list(BLCA = "BLCA", BRCA = "BRCA", CHOL = "CHOL", COADREAD = c("COAD", "READ"), ESCA = "ESCA",
                 HNSC = "HNSC", KICH = "KICH", KIRC = "KIRC", KIRP = "KIRP", LIHC = "LIHC", LUAD = "LUAD",
                 LUSC = "LUSC", PRAD = "PRAD", STAD = "STAD", THCA = "THCA", UCEC = "UCEC",
                 # checked too, but too few normals in TCGA (0-5): listed so a rerun notices new data
                 CESC = "CESC", GBM = "GBM", PAAD = "PAAD", PCPG = "PCPG", SARC = "SARC", SKCM = "SKCM", THYM = "THYM")
cache <- Sys.getenv("FLAME_TCGA_CACHE", path.expand("~/.cache/flame-tcga"))
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
options(timeout = 3600)

fetch <- function(file) {
  dest <- file.path(cache, file)
  if (!file.exists(dest)) {
    message("  downloading ", file)
    utils::download.file(file.path(HUB, file), paste0(dest, ".part"), mode = "wb", quiet = TRUE)
    file.rename(paste0(dest, ".part"), dest)
  }
  dest
}

# Ensembl gene -> symbol -> Entrez
pm <- fread(fetch("gencode.v36.annotation.gtf.gene.probemap"), select = 1:2)
setnames(pm, c("id", "symbol"))
entrez_of <- suppressMessages(AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db, unique(pm$symbol), "ENTREZID", "SYMBOL"))
pm[, entrez := unname(entrez_of[symbol])]
pm <- pm[!is.na(entrez)]

read_counts <- function(project) {
  f <- fetch(sprintf("TCGA-%s.star_counts.tsv.gz", project))
  x <- fread(cmd = paste("zcat", shQuote(f)), showProgress = FALSE)
  setnames(x, 1, "id")
  x <- x[id %in% pm$id]
  type <- substr(names(x)[-1], 14, 15)
  keep <- names(x)[-1][type %in% c("01", "11")]
  keep <- keep[!duplicated(substr(keep, 1, 15))]                    # one aliquot per patient and type
  x[, c("id", keep), with = FALSE]
}

out <- list()
for (code in names(PROJECTS)) {
  message(code)
  parts <- lapply(PROJECTS[[code]], read_counts)
  x <- Reduce(function(a, b) merge(a, b, by = "id"), parts)
  smp <- names(x)[-1]
  type <- ifelse(substr(smp, 14, 15) == "11", "normal", "tumour")
  n <- c(tumour = sum(type == "tumour"), normal = sum(type == "normal"))
  message(sprintf("  %d tumours, %d normals", n[["tumour"]], n[["normal"]]))
  if (n[["normal"]] < MIN_NORMALS) { message("  skipped: too few normals"); next }

  m <- round(2^as.matrix(x[, -1]) - 1)                                # Xena: log2(count + 1)
  sym <- pm$symbol[match(x$id, pm$id)]
  m <- rowsum(m, sym)                                                 # sum Ensembl IDs per symbol
  group <- factor(type, levels = c("normal", "tumour"))
  y <- DGEList(m, group = group)
  y <- y[filterByExpr(y, group = group), , keep.lib.sizes = FALSE]
  y <- calcNormFactors(y)
  design <- model.matrix(~ group)
  fit <- eBayes(lmFit(voom(y, design), design))
  tt <- topTable(fit, coef = "grouptumour", number = Inf, sort.by = "none")
  out[[code]] <- list(
    n = as.list(n),
    projects = PROJECTS[[code]],
    genes = data.frame(gene = rownames(tt), entrez = as.integer(entrez_of[rownames(tt)]),
                       log2FC = round(tt$logFC, 3), q = signif(tt$adj.P.Val, 3), stringsAsFactors = FALSE))
  message(sprintf("  %d genes tested, %d with q <= 0.05 and |log2FC| >= 1", nrow(tt), sum(tt$adj.P.Val <= 0.05 & abs(tt$logFC) >= 1)))
  rm(x, parts, m, y, fit); gc(verbose = FALSE)
}

res <- list(
  meta = list(built = format(Sys.Date()), source = "TCGA STAR counts, GDC data release (GENCODE v36), via the UCSC Xena GDC hub",
              method = "edgeR filterByExpr + TMM, limma-voom; primary tumour (01) vs solid tissue normal (11), unpaired; BH q-values",
              minNormals = MIN_NORMALS),
  cancers = out)
saveRDS(res, "inst/extdata/tcga_tumor_normal.rds", compress = "xz")
cat(length(out), "cancer types:", paste(names(out), collapse = " "), "\n")
cat(sprintf("%.1f MB\n", file.size("inst/extdata/tcga_tumor_normal.rds") / 1e6))
