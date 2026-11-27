# =============================================================================
# clusterProfiler strategy: ORA (enricher) and GSEA (GSEA / fgsea) on MSigDB
# =============================================================================
# GO uses enrichGO / gseGO on the organism's OrgDb, KEGG uses enrichKEGG / gseKEGG on
# current KEGG pathways, and every other source comes from MSigDB via msigdbr (see
# enrich-annotations-clusterprofiler.R). Term IDs use each set's original database ID
# (GO:..., R-HSA-..., WP..., HP:..., KEGG map...), so results line up with the other
# tools in comparisons. Genes are matched on NCBI Gene IDs; hits are reported as symbols.
# =============================================================================

.msigdb_cache <- new.env()

#' Gene sets for an organism and FLAME sources: data frame
#' term, gene (NCBI ID), symbol, source, name
msigdb_sets <- function(taxid, sources) {
  species <- MSIGDB_SPECIES[[as.character(taxid)]]
  if (is.null(species)) stop("clusterProfiler/MSigDB does not support this organism.")
  codes <- TOOLS[[ToolId$CLUSTERPROFILER]]$datasourceCodes
  sources <- setdiff(intersect(sources, names(codes)), "KEGG")   # KEGG comes from KEGG itself
  if (!length(sources)) stop("None of the selected sources is available in MSigDB.")
  parts <- lapply(sources, function(src) {
    key <- paste(species, src)
    if (is.null(.msigdb_cache[[key]])) {
      cs <- codes[[src]]
      args <- list(species = species, collection = cs[1])
      # Mouse: MSigDB's own mouse collections where they exist (else human sets via orthologs).
      if (identical(species, "Mus musculus") && !is.null(MSIGDB_MOUSE_CODES[[src]])) {
        cs <- MSIGDB_MOUSE_CODES[[src]]
        args <- list(db_species = "MM", species = species, collection = cs[1])
      }
      if (nzchar(cs[2])) args$subcollection <- cs[2]
      raw <- suppressMessages(do.call(msigdbr::msigdbr, args))
      raw <- as.data.frame(raw)
      if (!nrow(raw)) { .msigdb_cache[[key]] <- data.frame(); return(NULL) }
      exact <- as.character(raw$gs_exact_source)
      useExact <- !is.na(exact) & nzchar(exact) & src %in% c("GO:BP", "GO:MF", "GO:CC", "REAC", "WP", "HP")
      term <- ifelse(useExact, exact, raw$gs_name)
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

#' One standard frame from a clusterProfiler result (enrichResult or gseaResult) of one source:
#' Source, ID, Function, p.adjust, setSize, hits ("/"-separated NCBI IDs), NES, ES.
.cp_std <- function(res, source, threshold, names = NULL) {
  if (is.null(res)) return(NULL)
  df <- as.data.frame(res)
  if (!nrow(df)) return(NULL)
  df <- df[!is.na(df$p.adjust) & df$p.adjust <= threshold, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  gsea <- "core_enrichment" %in% names(df)
  id <- as.character(df$ID)
  fn <- if (!is.null(names)) unname(names[id]) else as.character(df$Description)
  fn <- sub(" - [A-Z][a-z]+ [a-z]+.*\\)$", "", fn)   # KEGG: "Cell cycle - Homo sapiens (human)"
  if (source == "KEGG") id <- paste0("map", sub("^[a-z]+", "", id))   # organism-neutral, as the other tools
  data.frame(Source = source, ID = id, OrigID = as.character(df$ID), Function = fn, p.adjust = df$p.adjust,
             setSize = if (gsea) as.numeric(df$setSize) else as.numeric(sub("/.*", "", df$BgRatio)),
             hits = as.character(if (gsea) df$core_enrichment else df$geneID),
             NES = if (gsea) df$NES else NA_real_, ES = if (gsea) df$enrichmentScore else NA_real_,
             stringsAsFactors = FALSE)
}

#' Run clusterProfiler on every requested source with the engine that serves it (enrichGO /
#' gseGO, enrichKEGG / gseKEGG, or enricher / GSEA on MSigDB).
#' mode "ORA": `genes` (NCBI IDs), optional `universe`; mode "GSEA": named sorted `geneList`.
#' Returns list(df = standard frame or NULL, sets = term -> NCBI IDs (reported terms),
#' symbols = NCBI ID -> symbol, universeSize, notes).
cp_enrich <- function(mode, taxid, sources, genes = NULL, geneList = NULL, universe = NULL,
                      method = "BH", threshold = 0.05, scoreType = "std") {
  plan <- cp_source_plan(taxid, sources)
  threshold <- as.numeric(threshold)
  ora <- identical(mode, "ORA")
  common <- if (ora) list(pvalueCutoff = threshold, pAdjustMethod = method, qvalueCutoff = 1, minGSSize = 5, maxGSSize = 2000)
            else list(pvalueCutoff = threshold, pAdjustMethod = method, minGSSize = 10, maxGSSize = 500,
                      eps = 0, seed = TRUE, verbose = FALSE, scoreType = scoreType)
  if (ora && !is.null(universe)) common$universe <- universe
  parts <- list(); sets <- list(); notes <- character(0); univ <- character(0); symbols <- character(0)
  keep <- function(res, src, names = NULL) {
    d <- .cp_std(res, src, threshold, names)
    if (ora && !is.null(res)) univ <<- union(univ, as.character(res@universe))
    if (!is.null(d)) {
      parts[[length(parts) + 1]] <<- d
      gs <- res@geneSets[d$OrigID]; names(gs) <- d$ID
      sets <<- c(sets, gs)
    }
  }
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))

  if (length(plan$go)) {
    db <- cp_orgdb(taxid)
    flame_progress(if (ora) 0.45 else 0.3, "GO enrichment (enrichGO)")
    for (src in plan$go) {
      res <- quiet(if (ora) do.call(clusterProfiler::enrichGO, c(list(gene = genes, OrgDb = db, keyType = "ENTREZID", ont = CP_GO_ONT[[src]]), common))
                   else do.call(clusterProfiler::gseGO, c(list(geneList = geneList, OrgDb = db, keyType = "ENTREZID", ont = CP_GO_ONT[[src]]), common)))
      keep(res, src)
    }
    notes <- c(notes, go_note(taxid))
  }
  if (plan$kegg) {
    flame_progress(if (ora) 0.6 else 0.5, "KEGG pathways (enrichKEGG)")
    k <- kegg_gson(taxid)
    res <- quiet(if (ora) do.call(clusterProfiler::enrichKEGG, c(list(gene = genes, organism = k$gson, keyType = "ncbi-geneid"), common))
                 else do.call(clusterProfiler::gseKEGG, c(list(geneList = geneList, organism = k$gson, keyType = "ncbi-geneid"), common)))
    keep(res, "KEGG")
    notes <- c(notes, kegg_note(k))
  }
  if (length(plan$msigdb)) {
    flame_progress(if (ora) 0.7 else 0.6, "MSigDB gene sets")
    ms <- msigdb_sets(taxid, plan$msigdb)
    t2g <- unique(ms[, c("term", "gene")])
    nm <- stats::setNames(ms$name, ms$term)
    msCommon <- common
    if (ora && is.null(universe)) msCommon$universe <- unique(ms$gene)
    res <- quiet(if (ora) do.call(clusterProfiler::enricher, c(list(gene = genes, TERM2GENE = t2g), msCommon))
                 else do.call(clusterProfiler::GSEA, c(list(geneList = geneList, TERM2GENE = t2g), msCommon)))
    if (!is.null(res)) {
      d <- .cp_std(res, "MSIGDB", threshold, nm[!duplicated(names(nm))])
      if (ora) univ <- union(univ, as.character(res@universe))
      if (!is.null(d)) {
        d$Source <- ms$source[match(d$ID, ms$term)]
        parts[[length(parts) + 1]] <- d
        gs <- res@geneSets[d$OrigID]; names(gs) <- d$ID
        sets <- c(sets, gs)
      }
    }
    symbols <- stats::setNames(ms$symbol, ms$gene)
    rel <- unique(stats::na.omit(as.character(suppressMessages(msigdbr::msigdbr_collections(db_species = if (identical(MSIGDB_SPECIES[[as.character(taxid)]], "Mus musculus")) "MM" else "HS"))$db_version)))
    notes <- c(notes, paste0("Other gene sets from MSigDB ", paste(rel, collapse = ", "), "."))
  }
  df <- if (length(parts)) do.call(rbind, parts) else NULL
  # Gene symbols for the reported genes: MSigDB tables where they have them, else OrgDb / g:Profiler.
  need <- unique(unlist(strsplit(df$hits %||% character(0), "/", fixed = TRUE)))
  if (!ora) need <- union(need, unlist(sets, use.names = FALSE))
  miss <- setdiff(need, names(symbols)[!is.na(symbols) & nzchar(symbols)])
  if (length(miss)) symbols <- c(symbols, cp_symbols(taxid, miss))
  symbols <- symbols[!duplicated(names(symbols))]
  list(df = df, sets = sets, symbols = symbols, universeSize = length(univ), notes = notes)
}

#' Standard clusterProfiler frame -> FLAME result table
.cp_flame_frame <- function(df, symbols, querySize) {
  hitIds <- strsplit(df$hits, "/", fixed = TRUE)
  hits <- vapply(hitIds, function(g) { s <- unname(symbols[g]); s[is.na(s)] <- g[is.na(s)]; paste(unique(s), collapse = ",") }, character(1))
  data.frame(Source = df$Source, Term_ID = df$ID, Function = df$Function,
             `P-value` = df$p.adjust, `Term Size` = df$setSize,
             `Query size` = querySize, `Intersection Size` = lengths(hitIds),
             `Positive Hits` = hits, check.names = FALSE, stringsAsFactors = FALSE)
}

ClusterProfilerORAStrategy <- R6::R6Class("ClusterProfilerORAStrategy",
  inherit = EnrichmentStrategy,
  public = list(
    initialize = function() super$initialize(ToolId$CLUSTERPROFILER, ParadigmId$ORA),

    run = function(inputList, organism, backgroundList, params) {
      genes <- unique(as.character(inputList))
      universe <- if (!is.null(backgroundList)) unique(as.character(backgroundList)) else NULL
      if (!is.null(universe)) genes <- intersect(genes, universe)
      if (!length(genes)) {
        renderWarning("None of the list's genes are in the background list.")
        return(NULL)
      }
      method <- if (params$metric %in% c("BH", "BY", "bonferroni", "holm", "none")) params$metric else "BH"
      out <- cp_enrich("ORA", organism, params$datasources, genes = genes, universe = universe,
                       method = method, threshold = params$threshold)
      for (n in out$notes) renderWarning(n)
      if (is.null(out$df)) return(NULL)
      list(result = .cp_flame_frame(out$df, out$symbols, length(genes)), backgroundSize = out$universeSize)
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
  }
  method <- if (params$metric %in% c("BH", "BY", "bonferroni", "holm", "none")) params$metric else "BH"
  scoreType <- if (all(geneList >= 0)) "pos" else if (all(geneList <= 0)) "neg" else "std"
  dataNotes <- character(0)
  if (!custom) {
    # GO (gseGO), KEGG (gseKEGG) and MSigDB (GSEA), each with its own engine.
    flame_progress(0.25, sprintf("Running GSEA (fgsea) on %d genes", length(geneList)))
    out <- withCallingHandlers(
      cp_enrich("GSEA", taxid, params$datasources, geneList = geneList, method = method,
                threshold = params$threshold, scoreType = scoreType),
      warning = function(w) { messages <<- c(messages, conditionMessage(w)); invokeRestart("muffleWarning") })
    dataNotes <- out$notes
    df <- out$df
    sets <- data.frame(term = rep(names(out$sets), lengths(out$sets)), gene = as.character(unlist(out$sets, use.names = FALSE)),
                       stringsAsFactors = FALSE)
    # Symbols: the ranked list's own mapping first, then the annotation sources'.
    sym <- c(stats::setNames(ct$name, ct$target)[!is.na(ct$name) & nzchar(ct$name)], out$symbols)
    sym <- sym[!duplicated(names(sym))]
    results <- if (!is.null(df)) .cp_flame_frame(df, sym, length(geneList)) else NULL
  } else {
    # Your own signatures are always reported (significant or not) and may be small or large.
    flame_progress(0.4, sprintf("Running GSEA (fgsea) on %d genes and %d gene sets", length(geneList), length(unique(sets$term))))
    res <- withCallingHandlers(
      suppressMessages(clusterProfiler::GSEA(
        geneList = geneList, TERM2GENE = unique(sets[, c("term", "gene")]), minGSSize = 5, maxGSSize = 5000,
        pvalueCutoff = 1, pAdjustMethod = method, eps = 0, seed = TRUE, verbose = FALSE, scoreType = scoreType)),
      warning = function(w) { messages <<- c(messages, conditionMessage(w)); invokeRestart("muffleWarning") })
    df <- as.data.frame(res)
    results <- NULL
    if (nrow(df)) {
      results <- .cp_result_frame(df, sets, length(geneList), "core_enrichment")
      df$ES <- df$enrichmentScore
    }
  }
  if (!is.null(results) && nrow(results)) {
    results$`-log10Pvalue` <- -log10(pmax(results$`P-value`, .Machine$double.xmin))
    results$`Gene Ratio` <- results$`Intersection Size` / results$`Term Size`
    results$`Term Coverage %` <- round(100 * results$`Intersection Size` / results$`Term Size`, 2)
    results$`Fold Enrichment` <- NA_real_
    results$NES <- df$NES
    results$ES <- df$ES
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
  messages <- c(if (custom) kept else character(0), dataNotes,
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
