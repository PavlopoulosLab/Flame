# =============================================================================
# FLAME core: input methods as plain functions (no Shiny)
# =============================================================================
# Upload/paste parsing, volcano and 2D-reduction tables, UpSet combinations,
# example data. Lifted from the former input-session-*.R classes; the client
# keeps the selection state and sends the chosen IDs to POST /api/lists.
# =============================================================================

MAX_INPUT_BYTES <- 1024L * 1024L

# ---- Upload / paste ---------------------------------------------------------

#' Split pasted/uploaded text into IDs with counts (unique, duplicates, invalid)
parse_id_text <- function(text, skipFirstRow = FALSE) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  if (nchar(text, type = "bytes") > MAX_INPUT_BYTES) stop("The input is larger than the 1 MB limit.")
  lines <- strsplit(text, "\r\n|\n|\r", perl = TRUE)[[1]]
  if (isTRUE(skipFirstRow) && length(lines) > 0) lines <- lines[-1]
  tokens <- trimws(unlist(strsplit(lines, "[\t,;\\s]+", perl = TRUE)))
  tokens <- tokens[nzchar(tokens)]
  valid <- grepl("^[A-Za-z0-9_.:-]+$", tokens)
  okTokens <- tokens[valid]
  dup <- duplicated(tolower(okTokens))
  list(ids = okTokens[!dup], total = length(tokens), unique = sum(!dup),
       duplicates = sum(dup), invalid = sum(!valid),
       invalidExamples = utils::head(unique(tokens[!valid]), 5))
}

#' Parse a ranked list: one "ID <tab/comma/space> score" pair per line.
#' Lines whose second field is not numeric (e.g. a header) are skipped and counted.
parse_ranked_text <- function(text, skipFirstRow = FALSE) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  if (nchar(text, type = "bytes") > MAX_INPUT_BYTES) stop("The input is larger than the 1 MB limit.")
  lines <- strsplit(text, "\r\n|\n|\r", perl = TRUE)[[1]]
  if (isTRUE(skipFirstRow) && length(lines) > 0) lines <- lines[-1]
  lines <- trimws(lines[nzchar(trimws(lines))])
  parts <- strsplit(lines, "[\t,; ]+", perl = TRUE)
  id <- vapply(parts, function(p) p[1] %||% "", character(1))
  score <- suppressWarnings(vapply(parts, function(p) as.numeric(p[2] %||% NA), numeric(1)))
  valid <- nzchar(id) & grepl("^[A-Za-z0-9_.:-]+$", id) & is.finite(score)
  dup <- duplicated(tolower(id)) & valid
  keep <- valid & !dup
  list(ids = id[keep], scores = score[keep], total = length(lines), unique = sum(keep),
       duplicates = sum(dup), invalid = sum(!valid),
       invalidExamples = utils::head(unique(lines[!valid]), 5))
}

# ---- Delimited tables (volcano, reduction) ----------------------------------

clean_column_names <- function(namesIn) {
  cleaned <- trimws(as.character(namesIn))
  missing <- !nzchar(cleaned) | is.na(cleaned)
  cleaned[missing] <- paste0("Column_", which(missing))
  make.unique(cleaned, sep = "_")
}

#' Parse a CSV / TSV / whitespace table (delimiter from extension or first line)
parse_delimited_table <- function(text, fileName = "", maxBytes = MAX_INPUT_BYTES) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  if (!nzchar(trimws(text))) stop("Paste a table or upload a .txt, .csv or .tsv file.")
  if (nchar(text, type = "bytes") > maxBytes) stop(sprintf("The input is larger than the %s MB limit.", format(round(maxBytes / 1024^2, 1))))
  ext <- tolower(tools::file_ext(fileName %||% ""))
  firstLine <- strsplit(text, "\r\n|\n|\r", perl = TRUE)[[1]][1]
  mode <- if (ext %in% c("csv", "tsv")) ext
          else if (grepl("\t", firstLine, fixed = TRUE)) "tsv"
          else if (grepl(",", firstLine, fixed = TRUE)) "csv"
          else "txt"
  args <- list(text = text, header = TRUE, na.strings = c("", "NA"),
               stringsAsFactors = FALSE, check.names = FALSE, comment.char = "")
  df <- tryCatch(switch(mode,
    csv = do.call(utils::read.csv, args),
    tsv = do.call(utils::read.delim, args),
    txt = do.call(utils::read.table, c(args, list(sep = "")))),
    error = function(e) stop("Could not parse the table. Check the delimiter, header row and file encoding."))
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- clean_column_names(names(df))
  if (nrow(df) == 0) stop("No rows were detected in the table.")
  df
}

table_columns <- function(df) {
  all <- names(df)
  list(all = as.list(all), numeric = as.list(all[vapply(df, is.numeric, logical(1))]))
}

.require_numeric_cols <- function(df, what) {
  if (ncol(df) < 3) stop(sprintf("Only %d column(s) detected. %s needs at least three columns.", ncol(df), what))
  if (sum(vapply(df, is.numeric, logical(1))) < 2) stop("At least two numeric columns are required.")
}

volcano_default_mapping <- function(df) {
  allCols <- names(df); numericCols <- allCols[vapply(df, is.numeric, logical(1))]
  idCandidates <- allCols[!allCols %in% numericCols]
  id <- if (length(idCandidates) > 0) idCandidates[1] else allCols[1]
  nl <- tolower(numericCols)
  lfc <- numericCols[grepl("log.?2?fc|log.?fold|fold.?change|logfc|log_fc", nl, perl = TRUE)]
  pv <- numericCols[grepl("p.?value|pval|padj|adj.?p|fdr", nl, perl = TRUE)]
  logfc <- if (length(lfc)) lfc[1] else if (length(numericCols)) numericCols[1] else ""
  pvalue <- if (length(pv)) pv[1] else if (length(setdiff(numericCols, logfc))) setdiff(numericCols, logfc)[1] else ""
  if (id %in% c(logfc, pvalue)) { rest <- setdiff(allCols, c(logfc, pvalue)); if (length(rest)) id <- rest[1] }
  list(id = id, logfc = logfc, pvalue = pvalue)
}

reduction_default_mapping <- function(df) {
  allCols <- names(df); numericCols <- allCols[vapply(df, is.numeric, logical(1))]
  idCandidates <- allCols[!allCols %in% numericCols]
  id <- if (length(idCandidates) > 0) idCandidates[1] else allCols[1]
  x <- if (length(numericCols) >= 1) numericCols[1] else ""
  y <- if (length(numericCols) >= 2) numericCols[2] else ""
  colorCand <- setdiff(allCols, c(id, x, y))
  cat <- setdiff(colorCand, numericCols)
  list(id = id, x = x, y = y,
       color = if (length(cat)) cat[1] else if (length(colorCand)) colorCand[1] else "", size = "")
}

#' Prepare step: parse, validate, and suggest a column mapping
prepare_table_input <- function(text, fileName = "", kind = c("volcano", "reduction")) {
  kind <- match.arg(kind)
  df <- parse_delimited_table(text, fileName)
  .require_numeric_cols(df, if (kind == "volcano") "A volcano plot" else "A 2D plot")
  list(rowCount = nrow(df), columns = table_columns(df),
       defaults = if (kind == "volcano") volcano_default_mapping(df) else reduction_default_mapping(df),
       preview = .records_head(df, 5))
}

.records_head <- function(df, n) {
  df <- utils::head(df, n)
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

.check_col <- function(df, col, label, numeric = FALSE) {
  if (!nzchar(col %||% "") || !col %in% names(df)) stop(sprintf("Select a valid %s column.", label))
  if (numeric && !is.numeric(df[[col]])) stop(sprintf("The %s column must be numeric.", label))
}

#' Volcano points: one per valid row, with significance class
volcano_points <- function(text, fileName = "", mapping, pThreshold = 0.05, lfcThreshold = 1) {
  df <- parse_delimited_table(text, fileName)
  .check_col(df, mapping$id, "ID"); .check_col(df, mapping$logfc, "log2FC", TRUE)
  .check_col(df, mapping$pvalue, "p-value", TRUE)
  if (length(unique(c(mapping$id, mapping$logfc, mapping$pvalue))) < 3) stop("ID, log2FC and p-value columns must be different.")
  p <- suppressWarnings(as.numeric(pThreshold)); l <- suppressWarnings(as.numeric(lfcThreshold))
  if (!is.finite(p) || p <= 0 || p > 1) stop("The p-value threshold must be greater than 0 and at most 1.")
  if (!is.finite(l) || l < 0) stop("The log2FC threshold must be a non-negative number.")

  ids <- trimws(as.character(df[[mapping$id]]))
  lfc <- suppressWarnings(as.numeric(df[[mapping$logfc]]))
  pv <- suppressWarnings(as.numeric(df[[mapping$pvalue]]))
  missingId <- is.na(ids) | !nzchar(ids); missingLfc <- !is.finite(lfc)
  badP <- !is.finite(pv) | pv <= 0 | pv > 1
  ok <- !(missingId | missingLfc | badP)
  if (!any(ok)) stop("No rows have IDs, finite log2FC values and p-values between 0 and 1.")

  status <- ifelse(pv[ok] < p & lfc[ok] > l, "up", ifelse(pv[ok] < p & lfc[ok] < -l, "down", "ns"))
  points <- data.frame(pointId = paste0("row_", which(ok)), id = ids[ok], logfc = lfc[ok],
                       pvalue = pv[ok], negLog10p = -log10(pv[ok]), status = status,
                       stringsAsFactors = FALSE)
  excluded <- c(missingIds = sum(missingId), missingLog2FC = sum(missingLfc), invalidPValues = sum(badP))
  list(points = points, total = nrow(df), plotted = sum(ok), excluded = as.list(excluded),
       counts = list(significant = sum(status != "ns"), up = sum(status == "up"), down = sum(status == "down")),
       thresholds = list(pvalue = p, log2fc = l))
}

#' 2D reduction points (optional colour and size channels)
reduction_points <- function(text, fileName = "", mapping) {
  df <- parse_delimited_table(text, fileName)
  .check_col(df, mapping$id, "ID"); .check_col(df, mapping$x, "X", TRUE); .check_col(df, mapping$y, "Y", TRUE)
  if (identical(mapping$x, mapping$y)) stop("X and Y columns must be different.")
  if (nzchar(mapping$color %||% "")) .check_col(df, mapping$color, "colour")
  if (nzchar(mapping$size %||% "")) .check_col(df, mapping$size, "size", TRUE)

  ids <- trimws(as.character(df[[mapping$id]]))
  x <- suppressWarnings(as.numeric(df[[mapping$x]])); y <- suppressWarnings(as.numeric(df[[mapping$y]]))
  missingId <- is.na(ids) | !nzchar(ids); missingXY <- !is.finite(x) | !is.finite(y)
  ok <- !(missingId | missingXY)
  if (!any(ok)) stop("No rows have IDs and finite coordinates.")
  points <- data.frame(pointId = paste0("row_", which(ok)), id = ids[ok], x = x[ok], y = y[ok],
                       stringsAsFactors = FALSE)
  colorType <- NULL
  if (nzchar(mapping$color %||% "")) {
    cv <- df[[mapping$color]][ok]
    colorType <- if (is.numeric(cv)) "continuous" else "categorical"
    points$color <- if (is.numeric(cv)) cv else as.character(cv)
  }
  if (nzchar(mapping$size %||% "")) points$size <- suppressWarnings(as.numeric(df[[mapping$size]][ok]))
  list(points = points, total = nrow(df), plotted = sum(ok), colorType = colorType,
       excluded = list(missingIds = sum(missingId), missingCoordinates = sum(missingXY)))
}

# ---- UpSet ------------------------------------------------------------------

UpsetMode <- list(INTERSECTION = "intersection", DISTINCT = "distinct", UNION = "union")

flameNormalizeUpsetMode <- function(mode) {
  mode <- tolower(trimws(as.character(mode %||% UpsetMode$INTERSECTION)[1]))
  if (mode %in% unlist(UpsetMode)) mode else UpsetMode$INTERSECTION
}

flameUpsetModeLabel <- function(mode) {
  switch(as.character(mode), intersection = "Intersection",
         distinct = "Distinct Combinations", union = "Union", "Intersection")
}

#' All 2^n - 1 combinations of named sets for a mode, largest first
flameComputeUpsetCombinations <- function(namedSets, mode = UpsetMode$INTERSECTION) {
  mode <- flameNormalizeUpsetMode(mode)
  if (is.null(names(namedSets)) || any(!nzchar(names(namedSets)))) stop("All source sets must be named.")
  setNames <- names(namedSets)
  namedSets <- lapply(namedSets, function(ids) { ids <- unique(trimws(as.character(ids))); ids[nzchar(ids)] })
  allIds <- unique(unlist(namedSets, use.names = FALSE))
  if (length(setNames) < 1 || length(allIds) == 0) return(list())

  combinations <- list()
  for (mask in seq_len(2^length(setNames) - 1)) {
    idx <- which(as.logical(intToBits(mask)[seq_along(setNames)]))
    sel <- namedSets[idx]
    ids <- switch(mode,
      intersection = Reduce(intersect, sel),
      distinct = {
        inSel <- Reduce(intersect, sel)
        other <- setdiff(seq_along(setNames), idx)
        if (length(other) == 0) inSel else setdiff(inSel, unique(unlist(namedSets[other], use.names = FALSE)))
      },
      union = unique(unlist(sel, use.names = FALSE)))
    ids <- unique(as.character(ids)); ids <- ids[nzchar(ids)]
    if (length(ids) == 0) next
    names_ <- setNames[idx]
    label <- if (mode == "distinct") paste0(paste(names_, collapse = " intersect "), " only")
             else paste(names_, collapse = if (mode == "union") " union " else " intersect ")
    key <- paste0("combo_", mask)
    combinations[[key]] <- list(id = key, mode = mode, label = label, sourceNames = names_,
                                sourceIndexes = idx, degree = length(idx), count = length(ids), ids = ids)
  }
  ord <- order(vapply(combinations, function(i) -i$count, numeric(1)),
               vapply(combinations, function(i) i$degree, numeric(1)),
               vapply(combinations, function(i) i$label, character(1)))
  combinations[names(combinations)[ord]]
}

# ---- Example data -----------------------------------------------------------

#' Example inputs used by every "Load example" button and the guided demo
EXAMPLE_KINDS <- c("upload", "snps", "textmining", "volcano", "volcano_airway", "reduction", "ranked", "de_lists", "gmt", "expression", "mirna", "metabolites", "survival", "bench_qpcr", "bench_western", "bench_facs", "bench_column",
                   "bench_viability", "bench_elisa", "bench_luciferase", "bench_qpcr_fold")

#' TRUE for recognisable gene names; FALSE for uncharacterised ORFs and
#' placeholder symbols (C11orf24, LOC647225, KIAA0100, FAM100A, FLJ..., LINC...,
#' antisense / readthrough transcripts, clone names), so examples read well.
named_gene <- function(x) {
  x <- as.character(x)
  !is.na(x) & nzchar(x) & grepl("^[A-Za-z][A-Za-z0-9-]*$", x) &
    !grepl("^(C[0-9XY]+orf[0-9]+|LOC[0-9]+|KIAA[0-9]+|FAM[0-9]+[A-Z]?[0-9]*|FLJ[0-9]+|DKFZ|LINC[0-9]+|MGC[0-9]+|ORF[0-9]+|RP[0-9]+-|CT[DC]-|AC[0-9]{6}|AL[0-9]{6}|AP[0-9]{6}|SNOR[AD][0-9]+|MIR[0-9]+HG)", x, ignore.case = TRUE) &
    !grepl("-(AS|IT|OT|DT)[0-9]*$|orf", x, ignore.case = TRUE)
}

# Published datasets behind the examples (built by data-raw/build_examples.R).
EXAMPLE_SOURCES <- list(
  airway = "Airway smooth-muscle cells from 4 donors, untreated vs dexamethasone (Himes et al. 2014, PLoS One 9:e99625; GEO GSE52778; Bioconductor 'airway'). Differential expression: limma-voom, paired by donor.",
  lihc_de = "TCGA liver hepatocellular carcinoma: 371 primary tumours vs 50 adjacent normal liver samples (Cancer Genome Atlas Research Network 2017, Cell 169:1327; STAR counts from the GDC via the UCSC Xena hub). Differential expression: edgeR TMM + limma-voom, unpaired; padj = Benjamini-Hochberg. Up- and down-regulated genes point to different biology (cell division up; immune and liver functions down), which suits analysing the two directions separately.",
  pbmc = "Genes of 2,638 peripheral blood mononuclear cells (10x Genomics PBMC 3k; Zheng et al. 2017, Nat Commun 8:14049; CC BY 4.0), embedded by their expression across cells (metacells, PCA, UMAP). Colour = the cell type where each gene is specifically expressed.",
  gwas = "Lead associations for Alzheimer's disease from Bellenguez et al. 2022, Nat Genet 54:412-436 (GWAS Catalog GCST90027158; GRCh38).",
  metabolites = "Metabolites raised in metastatic prostate cancer and validated in the study (Sreekumar et al. 2009, Nature 457:910-914): sarcosine, uracil, kynurenine, glycerol 3-phosphate, leucine and proline.",
  mirna = "microRNAs up-regulated in at least 3 of 6 solid cancers vs normal tissue (Volinia et al. 2006, PNAS 103:2257-2261, Table 2), with 2006 names mapped to current miRBase names.",
  hallmark = "The MSigDB Hallmark collection (Liberzon et al. 2015, Cell Systems 1:417-425).",
  lihc = "TCGA liver hepatocellular carcinoma (PanCancer Atlas; Cancer Genome Atlas Research Network 2017, Cell 169:1327) via cBioPortal: RNA-seq (log2 RSEM + 1) of 365 tumours for the genes of ten MSigDB Hallmark sets, with overall and progression-free survival and stage (built by data-raw/build_survival_example.R).")

.example_file <- function(name) {
  f <- getExamplesPath(name)
  if (!file.exists(f)) stop("Example file missing: ", name, " (run data-raw/build_examples.R).")
  utils::read.delim(f, stringsAsFactors = FALSE, check.names = FALSE)
}

# The gene-list example is a random draw of EXAMPLE_GENES_N from this pool.
EXAMPLE_POOL_N <- 500L
EXAMPLE_GENES_N <- 200L

#' The 500 human genes most significantly changed by dexamethasone (airway
#' dataset, BH-adjusted p <= 0.05, |log2FC| >= 1), most significant first.
example_gene_pool <- function() {
  de <- .example_file("airway_dex_de.tsv")
  utils::head(de$gene[de$padj <= 0.05 & abs(de$log2FC) >= 1], EXAMPLE_POOL_N)
}

example_input <- function(kind = EXAMPLE_KINDS) {
  kind <- match.arg(kind)
  airway <- function() .example_file("airway_dex_de.tsv")
  switch(kind,
    # Gene list: a fresh random draw of 200 from the 500 genes most affected by
    # dexamethasone (both directions), so each "Load example" gives a different list.
    upload = {
      pool <- example_gene_pool()
      list(text = paste(sample(pool, min(EXAMPLE_GENES_N, length(pool))), collapse = "\n"),
           source = paste(sprintf("%d genes drawn at random from the %d most significantly changed by dexamethasone.",
                                  min(EXAMPLE_GENES_N, length(pool)), length(pool)), EXAMPLE_SOURCES$airway))
    },
    # Every expressed gene, scored by sign(log2FC) x -log10 p: input for GSEA.
    ranked = {
      de <- airway()
      score <- round(sign(de$log2FC) * -log10(de$pvalue), 4)
      o <- order(-score)
      list(text = paste(c("gene\tscore", paste(de$gene[o], score[o], sep = "\t")), collapse = "\n"),
           fileName = "flame_example_ranked.tsv", skipFirstRow = TRUE,
           source = paste("All expressed genes ranked by sign(log2FC) x -log10 p.", EXAMPLE_SOURCES$airway))
    },
    # Up- and down-regulated genes (BH-adjusted p <= 0.05, |log2FC| >= 1).
    de_lists = {
      de <- airway()
      sig <- de$padj <= 0.05
      list(up = as.list(de$gene[sig & de$log2FC >= 1]), down = as.list(de$gene[sig & de$log2FC <= -1]),
           thresholds = list(log2FC = 1, padj = 0.05), source = EXAMPLE_SOURCES$airway)
    },
    # Experimental analysis: small illustrative tables (made-up numbers in realistic ranges, not measured data).
    bench_qpcr = , bench_western = , bench_facs = , bench_column = , bench_viability = , bench_elisa = , bench_luciferase = , bench_qpcr_fold = {
      f <- paste0(kind, ".tsv")
      list(text = paste(readLines(getExamplesPath(f), warn = FALSE), collapse = "\n"), fileName = paste0("flame_example_", f),
           source = c(bench_qpcr = "Illustrative qPCR Ct values (made-up numbers): airway cells, control vs dexamethasone vs dexamethasone + RU486, 3 experiments; GAPDH and ACTB as reference genes.",
                      bench_western = "Illustrative western blot band intensities (made-up numbers, as from ImageJ): 3 blots; ACTB as loading control.",
                      bench_facs = "Illustrative cell-cycle percentages (made-up numbers): control vs nocodazole, 3 experiments.",
                      bench_column = "Illustrative cell viability (% of control, made-up numbers): 4 replicates per group.",
                      bench_viability = "Illustrative MTT viability (% of vehicle, made-up numbers): cancer cells treated with increasing cisplatin doses for 48 h, 4 wells per dose.",
                      bench_elisa = "Illustrative ELISA (pg/mL, made-up numbers): IL-6, TNF and IL-1\u03b2 in macrophage supernatants, unstimulated vs LPS vs LPS + dexamethasone, 3 experiments.",
                      bench_qpcr_fold = "Illustrative qPCR fold changes (2^-\u0394\u0394Ct already calculated, e.g. in Excel; made-up numbers): airway cells, control vs dexamethasone vs dexamethasone + RU486, 3 experiments, relative to the control mean.",
                      bench_luciferase = "Illustrative dual-luciferase assay (relative light units, made-up numbers): NF-\u03baB firefly reporter and Renilla control, unstimulated vs TNF vs TNF + IKK inhibitor, 4 wells.")[[kind]])
    },
    # Expression of 365 liver tumours with their survival (and stage), for the survival analysis.
    survival = {
      rd <- function(f) paste(readLines(getExamplesPath(f), warn = FALSE), collapse = "\n")
      list(text = rd("tcga_lihc_expression.tsv"), fileName = "flame_example_survival_expression.tsv",
           clinical = rd("tcga_lihc_clinical.tsv"), clinicalFileName = "flame_example_survival_clinical.tsv",
           source = EXAMPLE_SOURCES$lihc)
    },
    # Tumour vs normal liver (TCGA LIHC): up- and down-regulated genes have clearly different
    # functions, so the up & down GO analysis shows distinct terms. Built from the shipped
    # tumour-vs-normal table (data-raw/build_tcga_tumor_normal.R).
    volcano = {
      g <- .tcga_tn()$cancers$LIHC$genes
      if (is.null(g)) stop("The TCGA tumour-vs-normal table is missing (run data-raw/build_tcga_tumor_normal.R).")
      g <- g[named_gene(g$gene) & !is.na(g$q) & !duplicated(g$gene), , drop = FALSE]
      g <- g[order(g$q), , drop = FALSE]
      de <- data.frame(gene = g$gene, log2FC = round(g$log2FC, 4), padj = signif(pmax(g$q, 1e-300), 4), stringsAsFactors = FALSE)
      list(text = .table_text(de), fileName = "flame_example_tcga_liver_tumour_vs_normal.tsv", source = EXAMPLE_SOURCES$lihc_de)
    },
    volcano_airway = {
      de <- airway()
      list(text = .table_text(de[, c("gene", "log2FC", "pvalue", "padj")]), fileName = "flame_example_airway_dex.tsv", source = EXAMPLE_SOURCES$airway)
    },
    expression = list(text = .table_text(.example_file("airway_counts.tsv")), fileName = "flame_example_airway_counts.tsv",
                      source = paste("Raw counts of the 4,000 most variable expressed genes;", EXAMPLE_SOURCES$airway)),
    reduction = list(text = .table_text(.example_file("pbmc3k_gene_umap.tsv")), fileName = "flame_example_pbmc3k_gene_umap.tsv", source = EXAMPLE_SOURCES$pbmc),
    metabolites = {
      me <- .example_file("metabolites_sreekumar2009.tsv")
      list(text = paste(me$metabolite, collapse = "\n"), fileName = "flame_example_metabolites.txt", source = EXAMPLE_SOURCES$metabolites)
    },
    mirna = {
      mi <- .example_file("mirna_volinia2006.tsv")
      list(text = paste(mi$mirbase, collapse = "\n"), fileName = "flame_example_mirnas.txt", source = EXAMPLE_SOURCES$mirna)
    },
    # The MSigDB Hallmark collection (human) as a GMT file: an example of custom gene sets.
    gmt = {
      sets <- msigdb_sets(9606, "HALLMARK")
      by <- split(sets$symbol, sets$term)
      names <- sets$name[match(names(by), sets$term)]
      list(text = paste(vapply(seq_along(by), function(i) paste(c(names(by)[i], names[i], unique(by[[i]])), collapse = "\t"), ""), collapse = "\n"),
           fileName = "flame_example_hallmark.gmt", name = "Example: MSigDB Hallmark", source = EXAMPLE_SOURCES$hallmark)
    },
    snps = list(text = paste(as.character(unlist(readRDS(getExamplesPath("variants.RDS")))), collapse = "\n"),
                source = "Well-studied human variants (dbSNP rsIDs)."),
    textmining = list(text = paste(as.character(unlist(readRDS(getExamplesPath("textmining_example.RDS")))), collapse = "\n"),
                      source = "Text from a published abstract on aspirin and cisplatin in cancer cells.")
  )
}

.table_text <- function(df) {
  paste(utils::capture.output(utils::write.table(df, sep = "\t", row.names = FALSE,
                                                 quote = FALSE, na = "")), collapse = "\n")
}
