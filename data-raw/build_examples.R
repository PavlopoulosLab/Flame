# =============================================================================
# Build Flame's example files from published datasets
# =============================================================================
# Writes processed, small example files to inst/extdata/examples/. Run once;
# the server only reads the results. Build-time packages: uwot, irlba, limma,
# airway (Bioconductor), igraph, Matrix, jsonlite.
#
#   Rscript data-raw/build_examples.R <pbmc3k_dir> <gwas_associations.json>   (from the project root)
#
# Sources
#  * PBMC 3k: 10x Genomics, "3k PBMCs from a Healthy Donor" (Zheng et al. 2017,
#    Nat Commun 8:14049), CC BY 4.0.
#    https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz
#  * airway: Himes et al. 2014, PLoS One 9:e99625 (GEO GSE52778), Bioconductor
#    package "airway": airway smooth-muscle cells from 4 donors, untreated or
#    treated with dexamethasone (1 uM, 18 h).
#  * Alzheimer's disease GWAS: Bellenguez et al. 2022, Nat Genet 54:412-436
#    (GWAS Catalog GCST90027158), lead associations via the GWAS Catalog API v2:
#    https://www.ebi.ac.uk/gwas/rest/api/v2/associations?accession_id=GCST90027158&size=500
#  * microRNAs: Volinia et al. 2006, PNAS 103:2257-2261, Table 2 (miRNAs
#    up-regulated in at least 3 of 6 solid cancers vs normal tissue).
# =============================================================================

suppressMessages({ library(Matrix); pkgload::load_all(".", quiet = TRUE) })   # named_gene(), %||%
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2)
pbmcDir <- args[1]; gwasJson <- args[2]
out <- file.path("inst", "extdata", "examples")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
write_tsv <- function(df, name) {
  utils::write.table(df, file.path(out, name), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat(sprintf("wrote %s (%d rows, %.0f kB)\n", name, nrow(df), file.size(file.path(out, name)) / 1024))
}
set.seed(1)

# ---- 1. PBMC 3k: a real UMAP of genes ---------------------------------------
# Genes are embedded by their expression across cells: genes expressed in the
# same cells fall together, so islands are cell-type programs.
m <- Matrix::readMM(file.path(pbmcDir, "matrix.mtx"))
genes <- utils::read.delim(file.path(pbmcDir, "genes.tsv"), header = FALSE, stringsAsFactors = FALSE)
m <- methods::as(m, "CsparseMatrix")
rownames(m) <- make.unique(genes$V2)
# Standard QC (as in the Seurat PBMC tutorial): 200-2500 genes, < 5 % mitochondrial reads.
nGene <- Matrix::colSums(m > 0); mito <- Matrix::colSums(m[grepl("^MT-", rownames(m)), ]) / Matrix::colSums(m)
m <- m[, nGene >= 200 & nGene <= 2500 & mito < 0.05]
lognorm <- log1p(t(t(m) / Matrix::colSums(m)) * 1e4)
cat("PBMC cells after QC:", ncol(m), "\n")

# Cell clusters and their labels (canonical PBMC markers).
mu <- Matrix::rowMeans(lognorm); v <- apply(lognorm, 1, stats::var)
hvg <- names(sort(v / pmax(mu, 1e-3), decreasing = TRUE))[seq_len(2000)]
z <- t(scale(t(as.matrix(lognorm[hvg, ])))); z[!is.finite(z)] <- 0; z <- pmin(pmax(z, -10), 10)
cellPc <- irlba::prcomp_irlba(t(z), n = 10)$x
nn <- uwot::umap(cellPc, n_neighbors = 20, ret_nn = TRUE, n_epochs = 1)$nn$euclidean$idx
edges <- cbind(rep(seq_len(nrow(nn)), ncol(nn) - 1), as.vector(nn[, -1]))
g <- igraph::simplify(igraph::graph_from_edgelist(edges, directed = FALSE))
cl <- igraph::membership(igraph::cluster_louvain(g, resolution = 1.6))
markers <- list(
  "Naive CD4 T cells" = c("CCR7", "LEF1"), "Memory CD4 T cells" = c("IL7R", "S100A4"), "CD8 T cells" = c("CD8A", "CD8B"),
  "B cells" = c("MS4A1", "CD79A"), "NK cells" = c("GNLY", "NKG7"), "CD14 monocytes" = c("CD14", "LYZ"),
  "FCGR3A monocytes" = c("FCGR3A", "MS4A7"), "Dendritic cells" = c("FCER1A", "CST3"), "Platelets" = c("PPBP", "PF4"))
# Annotate clusters with the usual PBMC marker rules (mean log-expression per cluster).
mk <- unique(unlist(markers))
avg <- t(sapply(split(seq_len(ncol(lognorm)), cl), function(ix) Matrix::rowMeans(lognorm[mk, ix, drop = FALSE])))
clusterLabel <- apply(avg, 1, function(x) {
  if (x[["PPBP"]] > 2) "Platelets"
  else if (x[["FCER1A"]] > 1.5) "Dendritic cells"
  else if (x[["LYZ"]] > 3 && x[["CD14"]] > 1) "CD14 monocytes"
  else if (x[["LYZ"]] > 2 && (x[["FCGR3A"]] > 1 || x[["MS4A7"]] > 1)) "FCGR3A monocytes"
  else if (x[["MS4A1"]] > 1 || x[["CD79A"]] > 1.5) "B cells"
  else if (x[["GNLY"]] > 2 && x[["CD8A"]] < 0.5) "NK cells"
  else if (x[["CD8A"]] > 0.6) "CD8 T cells"
  else if (x[["CCR7"]] > 0.8 && x[["S100A4"]] < 2) "Naive CD4 T cells"
  else "Memory CD4 T cells"
})
cellType <- clusterLabel[cl]
cat("cell types:", paste(names(table(cellType)), table(cellType), collapse = ", "), "\n")

# Gene embedding. Single cells are sparse, so similar cells are first averaged
# into ~200 metacells (k-means on the cell PCs); genes detected in >= 2 % of
# cells and variable across metacells are embedded by their z-scored metacell
# profiles (PCA, then UMAP with cosine distance).
meta <- stats::kmeans(cellPc, centers = 200, iter.max = 50, nstart = 3)$cluster
metaExpr <- sapply(split(seq_len(ncol(lognorm)), meta), function(ix) Matrix::rowMeans(lognorm[, ix, drop = FALSE]))
det <- Matrix::rowMeans(m > 0)
cand <- names(det)[det >= 0.02 & named_gene(names(det))]
mv <- apply(metaExpr[cand, ], 1, stats::var) / pmax(rowMeans(metaExpr[cand, ]), 0.05)
keep <- union(names(sort(mv, decreasing = TRUE))[seq_len(min(2500, length(mv)))], intersect(unlist(markers), cand))
gz <- t(scale(t(metaExpr[keep, ]))); gz[!is.finite(gz)] <- 0
genePc <- irlba::prcomp_irlba(gz, n = 25)$x
um <- uwot::umap(genePc, n_neighbors = 12, min_dist = 0.05, metric = "cosine", seed = 42, n_threads = 1)
# Colour each gene by the cell type where it is highest, if clearly specific.
byType <- sapply(split(seq_len(ncol(lognorm)), cellType), function(ix) Matrix::rowMeans(lognorm[keep, ix, drop = FALSE]))
top <- colnames(byType)[apply(byType, 1, which.max)]
spec <- apply(byType, 1, function(x) if (max(x) <= 0) 0 else (max(x) - mean(x)) / max(x))
# A gene is labelled with a cell type only if it is detected in >= 20 % of those cells.
detType <- sapply(split(seq_len(ncol(m)), cellType), function(ix) Matrix::rowMeans(m[keep, ix, drop = FALSE] > 0))
detTop <- detType[cbind(seq_along(keep), match(top, colnames(detType)))]
spec[detTop < 0.2] <- 0
geneUmap <- data.frame(gene = keep, UMAP1 = round(um[, 1], 3), UMAP2 = round(um[, 2], 3),
                       cell_type = ifelse(spec >= 0.45, top, "Broadly expressed"),
                       mean_expression = round(Matrix::rowMeans(lognorm[keep, ]), 3), stringsAsFactors = FALSE)
geneUmap <- geneUmap[sample(nrow(geneUmap)), ]
write_tsv(geneUmap, "pbmc3k_gene_umap.tsv")

# ---- 2. airway: dexamethasone response (limma-voom, paired by donor) --------
suppressMessages(library(SummarizedExperiment))
data("airway", package = "airway")
cnt <- assay(airway, "counts")
sym <- rowData(airway)$gene_name
info <- as.data.frame(colData(airway))
info$dex <- stats::relevel(factor(info$dex), "untrt")
lib <- colSums(cnt)
expressed <- rowSums(t(t(cnt) / lib) * 1e6 >= 1) >= 4
cnt <- cnt[expressed, ]; sym <- sym[expressed]
design <- stats::model.matrix(~ cell + dex, data = info)
vm <- limma::voom(cnt, design, lib.size = lib)
fit <- limma::eBayes(limma::lmFit(vm, design))
tt <- limma::topTable(fit, coef = "dextrt", number = Inf, sort.by = "none")
de <- data.frame(gene = sym, log2FC = round(tt$logFC, 4), pvalue = signif(tt$P.Value, 4), padj = signif(tt$adj.P.Val, 4),
                 mean_log2CPM = round(tt$AveExpr, 3), stringsAsFactors = FALSE)
ok <- named_gene(de$gene) & !duplicated(de$gene)
de <- de[ok, ]; cntSym <- cnt[ok, ]; rownames(cntSym) <- de$gene
de <- de[order(de$pvalue), ]
write_tsv(de, "airway_dex_de.tsv")
cat("airway: significant (BH 5 %, |log2FC| >= 1):", sum(de$padj <= 0.05 & abs(de$log2FC) >= 1),
    "; top up:", paste(utils::head(de$gene[de$log2FC > 0], 8), collapse = ", "), "\n")

# Expression matrix for the expression input: raw counts, 4 donors x 2 conditions,
# the 4,000 most variable expressed genes.
ord <- order(info$dex, info$cell)
counts <- cntSym[, ord]
colnames(counts) <- paste0(ifelse(info$dex[ord] == "trt", "dexamethasone_", "untreated_"), info$cell[ord])
lc <- log2(t(t(counts) / colSums(counts)) * 1e6 + 1)
counts <- counts[order(-apply(lc, 1, stats::var))[seq_len(4000)], ]
write_tsv(data.frame(gene = rownames(counts), counts, check.names = FALSE), "airway_counts.tsv")

# ---- 3. Alzheimer's disease GWAS (Bellenguez et al. 2022) -------------------
a <- jsonlite::fromJSON(gwasJson, simplifyVector = FALSE)$`_embedded`$associations
gw <- do.call(rbind, lapply(a, function(x) {
  rs <- x$snp_allele[[1]]$rs_id; loc <- strsplit(unlist(x$locations)[1] %||% ":", ":")[[1]]
  data.frame(SNP = rs, CHR = loc[1], BP = suppressWarnings(as.integer(loc[2])), P = x$p_value,
             GENE = paste(unlist(x$mapped_genes), collapse = ","), stringsAsFactors = FALSE)
}))
gw <- gw[grepl("^rs[0-9]+$", gw$SNP) & !is.na(gw$BP) & is.finite(gw$P), ]
gw <- gw[order(gw$P), ]; gw <- gw[!duplicated(gw$SNP), ]
write_tsv(gw, "ad_gwas_bellenguez2022.tsv")

# ---- 4. microRNAs (Volinia et al. 2006, Table 2) -----------------------------
# 2006 precursor names mapped to current miRBase mature names (dominant arm).
mirnas <- data.frame(
  paper = c("miR-21", "miR-17-5p", "miR-191", "miR-29b-2", "miR-223", "miR-128b", "miR-199a-1", "miR-24-1 / miR-24-2",
            "miR-146", "miR-155", "miR-181b-1", "miR-20a", "miR-107", "miR-32", "miR-92-2", "miR-214", "miR-30c", "miR-25",
            "miR-221", "miR-106a"),
  mirbase = c("hsa-miR-21-5p", "hsa-miR-17-5p", "hsa-miR-191-5p", "hsa-miR-29b-3p", "hsa-miR-223-3p", "hsa-miR-128-3p",
              "hsa-miR-199a-5p", "hsa-miR-24-3p", "hsa-miR-146a-5p", "hsa-miR-155-5p", "hsa-miR-181b-5p", "hsa-miR-20a-5p",
              "hsa-miR-107", "hsa-miR-32-5p", "hsa-miR-92a-3p", "hsa-miR-214-3p", "hsa-miR-30c-5p", "hsa-miR-25-3p",
              "hsa-miR-221-3p", "hsa-miR-106a-5p"), stringsAsFactors = FALSE)
write_tsv(mirnas, "mirna_volinia2006.tsv")
cat("done\n")
