# =============================================================================
# clusterProfiler strategy: ORA (enricher) and GSEA (GSEA / fgsea) on MSigDB
# =============================================================================
# Gene sets come from MSigDB via msigdbr (20 species). Term IDs use each set's
# original database ID where MSigDB records one (GO:..., R-HSA-..., WP...,
# HP:..., KEGG map...), so results line up with the other tools in comparisons.
# Genes are matched on NCBI Gene IDs; hits are reported as gene symbols.
# =============================================================================

.msigdb_cache <- new.env()

#' Gene sets for an organism and FLAME sources: data frame
#' term, gene (NCBI ID), symbol, source, name
msigdb_sets <- function(taxid, sources) {
  species <- MSIGDB_SPECIES[[as.character(taxid)]]
  if (is.null(species)) stop("clusterProfiler/MSigDB does not support this organism.")
  codes <- TOOLS[[ToolId$CLUSTERPROFILER]]$datasourceCodes
  sources <- intersect(sources, names(codes))
  if (!length(sources)) stop("None of the selected sources is available in MSigDB.")
  parts <- lapply(sources, function(src) {
    key <- paste(species, src)
    if (is.null(.msigdb_cache[[key]])) {
      cs <- codes[[src]]
      args <- list(species = species, collection = cs[1])
      if (nzchar(cs[2])) args$subcollection <- cs[2]
      raw <- suppressMessages(do.call(msigdbr::msigdbr, args))
      raw <- as.data.frame(raw)
      if (!nrow(raw)) { .msigdb_cache[[key]] <- data.frame(); return(NULL) }
      exact <- as.character(raw$gs_exact_source)
      useExact <- !is.na(exact) & nzchar(exact) & src %in% c("GO:BP", "GO:MF", "GO:CC", "REAC", "WP", "HP", "KEGG")
      term <- ifelse(useExact, exact, raw$gs_name)
      if (src == "KEGG") term <- ifelse(useExact, paste0("map", gsub("[^0-9]", "", exact)), term)
      .msigdb_cache[[key]] <- data.frame(
        term = term, gene = as.character(raw$ncbi_gene), symbol = as.character(raw$gene_symbol),
        source = src, name = .pretty_set_name(raw$gs_name, src), stringsAsFactors = FALSE)
    }
    .msigdb_cache[[key]]
  })
  sets <- do.call(rbind, parts)
  sets[!is.na(sets$gene) & nzchar(sets$gene), , drop = FALSE]
}

#' "GOBP_REGULATION_OF_APOPTOTIC_PROCESS" -> "regulation of apoptotic process"
.pretty_set_name <- function(x, src) {
  x <- sub("^(GOBP|GOMF|GOCC|REACTOME|WP|KEGG_MEDICUS|KEGG|HP|HALLMARK|BIOCARTA|PID)_", "", x)
  x <- gsub("_", " ", x)
  # Author-named signatures, gene symbols and chromosome bands keep their case.
  if (src %in% c("TF", "MIRNA", "CELLTYPE", "IMMUNESIGDB", "ONCOGENIC", "POSITIONAL", "CGP", "CP",
                 "MIR_LEGACY", "TFT_LEGACY", "3CA", "CGN", "CM", "VAX", "CPS")) return(x)
  tolower(x)
}

.cp_result_frame <- function(df, sets, querySize, hitsCol, sep = "/") {
  first <- !duplicated(sets$term)
  info <- sets[first, c("term", "source", "name")]
  sym <- stats::setNames(sets$symbol, sets$gene)
  hits <- vapply(strsplit(as.character(df[[hitsCol]]), sep, fixed = TRUE),
                 function(g) paste(unique(stats::na.omit(sym[g])), collapse = ","), character(1))
  idx <- match(df$ID, info$term)
  data.frame(Source = info$source[idx], Term_ID = df$ID, Function = info$name[idx],
             `P-value` = df$p.adjust, `Term Size` = as.numeric(df$setSize %||% NA),
             `Query size` = querySize, `Intersection Size` = lengths(strsplit(as.character(df[[hitsCol]]), sep, fixed = TRUE)),
             `Positive Hits` = hits, check.names = FALSE, stringsAsFactors = FALSE)
}

ClusterProfilerORAStrategy <- R6::R6Class("ClusterProfilerORAStrategy",
  inherit = EnrichmentStrategy,
  public = list(
    initialize = function() super$initialize(ToolId$CLUSTERPROFILER, ParadigmId$ORA),

    run = function(inputList, organism, backgroundList, params) {
      sets <- msigdb_sets(organism, params$datasources)
      universe <- if (!is.null(backgroundList)) unique(as.character(backgroundList)) else unique(sets$gene)
      genes <- intersect(unique(as.character(inputList)), universe)
      if (length(genes) == 0) {
        renderWarning("None of the list's genes are annotated in the selected MSigDB collections.")
        return(NULL)
      }
      method <- if (params$metric %in% c("BH", "BY", "bonferroni", "holm", "none")) params$metric else "BH"
      res <- suppressMessages(clusterProfiler::enricher(
        gene = genes, TERM2GENE = unique(sets[, c("term", "gene")]), universe = universe,
        pvalueCutoff = as.numeric(params$threshold), pAdjustMethod = method, qvalueCutoff = 1,
        minGSSize = 5, maxGSSize = 2000))
      if (is.null(res)) return(NULL)
      df <- as.data.frame(res)
      df <- df[df$p.adjust <= as.numeric(params$threshold), , drop = FALSE]
      if (!nrow(df)) return(NULL)
      df$setSize <- as.numeric(sub("/.*", "", df$BgRatio))
      list(result = .cp_result_frame(df, sets, length(genes), "geneID"), backgroundSize = length(universe))
    },

    convertIDs = function(geneList, organism, targetNamespace) geneList
  )
)

#' GSEA on a ranked list (IDs + scores) with clusterProfiler::GSEA (fgsea)
#' Returns the same structure as execute_ora_run(), plus NES/ES columns.
execute_gsea_run <- function(lst, params, customSets = NULL) {
  messages <- character(0)
  taxid <- list_taxid(lst)
  shortName <- ORGANISMS$short_name[ORGANISMS$taxid == taxid][1]
  ids <- as.character(unlist(lst$ids)); scores <- as.numeric(unlist(lst$scores))
  if (length(ids) != length(scores) || length(ids) < 15) stop("GSEA needs a ranked list (IDs with scores).")

  flame_progress(0.08, sprintf("Mapping %d ranked genes to NCBI Gene IDs", length(ids)))
  conv <- gprofiler2::gconvert(ids, organism = shortName, target = "ENTREZGENE_ACC", mthreshold = 1, filter_na = TRUE)
  if (is.null(conv) || !nrow(conv)) stop("The list's IDs could not be mapped to NCBI Gene IDs.")
  ct <- data.frame(input = conv$input, target = as.character(conv$target), name = conv$name, stringsAsFactors = FALSE)
  sc <- scores[match(ct$input, ids)]
  keep <- !is.na(sc) & nzchar(ct$target)
  # One score per gene: keep the most extreme when several inputs map to it.
  byGene <- tapply(sc[keep], ct$target[keep], function(v) v[which.max(abs(v))])
  geneList <- sort(stats::setNames(as.numeric(byGene), names(byGene)), decreasing = TRUE)
  if (length(geneList) < 15) stop("Fewer than 15 genes could be mapped; GSEA needs more.")

  custom <- !is.null(customSets)
  if (custom) {
    flame_progress(0.25, "Matching your gene sets to the ranking")
    sets <- custom_gsea_sets(customSets, ct, as.character(params$datasources)[1])
    if (!nrow(sets)) stop("None of the genes of your gene sets are in the ranked list. Check that both use the same organism and gene IDs.")
    inRank <- tapply(sets$gene %in% names(geneList), sets$term, sum)
    total <- vapply(customSets, function(x) length(unique(x$genes)), integer(1))
    names(total) <- vapply(customSets, function(x) as.character(x$id), "")
    setName <- stats::setNames(sets$name, sets$term)
    messages <- c(messages, sprintf("%s: %d of %d genes are in your ranking.", setName[names(inRank)], inRank, total[names(inRank)]))
  } else {
    flame_progress(0.25, "Loading MSigDB gene sets")
    sets <- msigdb_sets(taxid, params$datasources)
  }
  method <- if (params$metric %in% c("BH", "BY", "bonferroni", "holm", "none")) params$metric else "BH"
  scoreType <- if (all(geneList >= 0)) "pos" else if (all(geneList <= 0)) "neg" else "std"
  flame_progress(0.4, sprintf("Running GSEA (fgsea) on %d genes and %d gene sets", length(geneList), length(unique(sets$term))))
  res <- withCallingHandlers(
    suppressMessages(clusterProfiler::GSEA(
      # Your own signatures are always reported (significant or not) and may be small or large.
      geneList = geneList, TERM2GENE = unique(sets[, c("term", "gene")]),
      minGSSize = if (custom) 5 else 10, maxGSSize = if (custom) 5000 else 500,
      pvalueCutoff = if (custom) 1 else as.numeric(params$threshold), pAdjustMethod = method, eps = 0, seed = TRUE,
      verbose = FALSE, scoreType = scoreType)),
    warning = function(w) { messages <<- c(messages, conditionMessage(w)); invokeRestart("muffleWarning") })

  df <- as.data.frame(res)
  results <- NULL
  if (nrow(df)) {
    results <- .cp_result_frame(df, sets, length(geneList), "core_enrichment")
    results$`-log10Pvalue` <- -log10(pmax(results$`P-value`, .Machine$double.xmin))
    results$`Gene Ratio` <- results$`Intersection Size` / results$`Term Size`
    results$`Term Coverage %` <- round(100 * results$`Intersection Size` / results$`Term Size`, 2)
    results$`Fold Enrichment` <- NA_real_
    results$NES <- df$NES
    results$ES <- df$enrichmentScore
    results$Term_ID_noLinks <- results$Term_ID
    results <- results[order(results$`P-value`, -abs(results$NES)), , drop = FALSE]
    rownames(results) <- NULL
    # Same database links as ORA results (QuickGO, Reactome, KEGG, ...).
    linker <- ORAEnrichmentSession$new(id = "gsea", runId = "gsea", uniqueId = 1L, displayNumber = 1L,
      toolName = ToolId$CLUSTERPROFILER, organism = taxid,
      input = UnrankedAnalyteList$new(name = "gsea", analyteType = AnalyteType$GENE, ids = names(geneList)[1], organismTaxId = taxid))
    results <- clean_result_links(linker$.__enclos_env__$private$attachDBLinks(results))
  }
  # Turn fgsea's technical warnings into one plain sentence.
  skipped <- suppressWarnings(as.integer(sub(".*?([0-9]+) of [0-9]+ gene set.*", "\\1", grep("no estimable p-value", messages, value = TRUE)[1])))
  kept <- messages[!grepl("no estimable p-value|fgsea|nperm|ties|eps", messages)]
  messages <- c(if (custom) kept else character(0),
                if (!is.na(skipped)) sprintf("%d gene set(s) were skipped because their significance could not be estimated (unbalanced scores).", skipped))
  if (custom && is.null(results)) messages <- c(messages, "Your gene sets have fewer than 5 genes in the ranking, so GSEA could not test them.")
  # What the enrichment plot needs: the ranking and each reported set's genes in it.
  ranking <- NULL
  if (!is.null(results)) {
    t2g <- unique(sets[sets$term %in% results$Term_ID, c("term", "gene")])
    t2g <- t2g[t2g$gene %in% names(geneList), , drop = FALSE]
    symbol <- ct$name[match(names(geneList), ct$target)]
    ranking <- list(genes = names(geneList), scores = unname(geneList),
                    symbols = ifelse(is.na(symbol) | !nzchar(symbol), names(geneList), symbol),
                    sets = split(t2g$gene, t2g$term), scoreType = scoreType)
  }
  list(
    status = "complete", resultCount = if (is.null(results)) 0L else nrow(results), results = results,
    gseaRanking = ranking,
    parameters = c(params, list(namespace = "ENTREZGENE_ACC", paradigm = "GSEA", rankedGenes = length(geneList),
                                scoreType = scoreType)),
    conversionTable = ct, backgroundConversionTable = NULL, backgroundSize = NULL,
    noHitAnalytes = character(0), details = NULL, messages = unique(messages)
  )
}

#' The user's own gene sets (GMT or a pasted signature) as GSEA sets on NCBI Gene IDs. Their genes
#' are matched to the ranked list's mapping (`ct`: input ID, NCBI Gene ID, symbol) by input ID or
#' symbol, case-insensitively; genes outside the ranking play no part in GSEA anyway.
custom_gsea_sets <- function(customSets, ct, sourceName) {
  key <- toupper(c(ct$input, ct$name)); target <- c(ct$target, ct$target)
  rows <- lapply(customSets, function(x) {
    genes <- unique(toupper(trimws(as.character(unlist(x$genes)))))
    hit <- unique(target[key %in% genes & !is.na(target) & nzchar(target)])
    if (!length(hit)) return(NULL)
    data.frame(term = as.character(x$id), gene = hit, symbol = ct$name[match(hit, ct$target)],
               source = sourceName, name = as.character(x$name %||% x$id), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) data.frame(term = character(0), gene = character(0), symbol = character(0), source = character(0), name = character(0)) else out
}

#' Running enrichment score of one gene set along a ranking (GSEA, weight p = 1), as drawn in
#' the classic enrichment plot. `scores` are sorted decreasingly; `inSet` marks the set's genes.
#' Between two hits the score falls linearly, so the curve is exact from the points just before
#' and at every hit. Returns the curve, the hit positions, the peak and the leading edge.
gsea_running_score <- function(scores, inSet) {
  n <- length(scores); hits <- which(inSet); nh <- length(hits)
  if (!nh || nh == n) stop("The set has no genes (or only genes) in the ranking.")
  w <- abs(scores[hits]); nr <- sum(w)
  step <- numeric(n)
  step[!inSet] <- -1 / (n - nh)
  step[hits] <- if (nr > 0) w / nr else 1 / nh
  res <- cumsum(step)
  peak <- if (max(res) >= -min(res)) which.max(res) else which.min(res)
  es <- res[peak]
  x <- c(0L, as.vector(rbind(hits - 1L, hits)), n)
  y <- c(0, ifelse(x[-c(1, length(x))] == 0, 0, res[pmax(1L, x[-c(1, length(x))])]), 0)
  leading <- if (es >= 0) hits[hits <= peak] else hits[hits >= peak]
  list(x = x, y = round(y, 5), hits = hits, peak = peak, es = es, leadingEdge = leading)
}

strategyRegistry$register(ToolId$CLUSTERPROFILER, ParadigmId$ORA, ClusterProfilerORAStrategy$new())
