# =============================================================================
# FLAME core: further analyses of gene lists, networks and expression data
# =============================================================================
#  * drug_connectivity()     drugs whose signatures mimic or reverse a response
#  * ppi_modules()           STRING network split into modules, each enriched
#  * enrichment_robustness() bootstrap stability and random-list control
#  * pathway_activity()      per-sample pathway scores (ssGSEA, z-score, PROGENy)
#  * metabolite_enrichment() KEGG pathway enrichment of metabolites
# =============================================================================

ENRICHR_API <- "https://maayanlab.cloud/Enrichr"

.enrichr_enrich <- function(genes, library) {
  r <- httr::POST(paste0(ENRICHR_API, "/addList"), body = list(list = paste(genes, collapse = "\n"), description = "flame"),
                  encode = "multipart", httr::timeout(120))
  if (httr::status_code(r) != 200) stop("Enrichr did not accept the gene list (HTTP ", httr::status_code(r), ").")
  id <- jsonlite::fromJSON(rawToChar(httr::content(r, "raw")))$userListId
  e <- httr::GET(paste0(ENRICHR_API, "/enrich"), query = list(userListId = id, backgroundType = library), httr::timeout(180))
  if (httr::status_code(e) != 200) stop("Enrichr did not return results (HTTP ", httr::status_code(e), ").")
  rows <- jsonlite::fromJSON(rawToChar(httr::content(e, "raw")), simplifyVector = FALSE)[[library]]
  if (!length(rows)) return(data.frame(term = character(0), p = numeric(0), padj = numeric(0), combined = numeric(0), genes = character(0)))
  data.frame(term = vapply(rows, function(x) x[[2]], ""), p = vapply(rows, function(x) as.numeric(x[[3]]), 0),
             combined = vapply(rows, function(x) as.numeric(x[[5]]), 0), padj = vapply(rows, function(x) as.numeric(x[[7]]), 0),
             genes = vapply(rows, function(x) paste(unlist(x[[6]]), collapse = ","), ""), stringsAsFactors = FALSE)
}

DRUG_LIBRARIES <- c(chemical = "LINCS_L1000_Chem_Pert_Consensus_Sigs", crispr = "LINCS_L1000_CRISPR_KO_Consensus_Sigs")

#' Connectivity: which perturbations mimic or reverse an up/down gene response.
#' Mimic: up genes in the perturbation's Up signature + down genes in its Down signature.
#' Reverse: up genes in its Down signature + down genes in its Up signature.
drug_connectivity <- function(up, down = character(0), kind = c("chemical", "crispr"), top = 50L) {
  kind <- match.arg(kind)
  lib <- DRUG_LIBRARIES[[kind]]
  up <- unique(toupper(up)); down <- unique(toupper(down))
  if (length(up) < 5 && length(down) < 5) stop("Give at least 5 up- or down-regulated genes.")
  parts <- list()
  if (length(up) >= 5) { flame_progress(0.15, sprintf("Comparing %d up-regulated genes with LINCS L1000 signatures", length(up))); parts$up <- .enrichr_enrich(up, lib) }
  if (length(down) >= 5) { flame_progress(0.55, sprintf("Comparing %d down-regulated genes with LINCS L1000 signatures", length(down))); parts$down <- .enrichr_enrich(down, lib) }
  flame_progress(0.9, "Scoring perturbations")
  split_dir <- function(df) {
    if (is.null(df) || !nrow(df)) return(list(Up = df, Down = df))
    dir <- sub(".*\\s(Up|Down)$", "\\1", df$term)
    df$agent <- sub("\\s(Up|Down)$", "", df$term)
    list(Up = df[dir == "Up", ], Down = df[dir == "Down", ])
  }
  u <- split_dir(parts$up); d <- split_dir(parts$down)
  agents <- unique(c(u$Up$agent, u$Down$agent, d$Up$agent, d$Down$agent))
  pick <- function(df, a) { if (is.null(df) || !nrow(df)) return(NULL); r <- df[df$agent == a, ]; if (nrow(r)) r[1, ] else NULL }
  score <- function(r) if (is.null(r)) 0 else -log10(max(r$padj, 1e-300))
  rows <- lapply(agents, function(a) {
    uu <- pick(u$Up, a); ud <- pick(u$Down, a); du <- pick(d$Up, a); dd <- pick(d$Down, a)
    list(agent = a, mimic = round(score(uu) + score(dd), 2), reverse = round(score(ud) + score(du), 2),
         upInUp = if (!is.null(uu)) uu$padj else NULL, downInDown = if (!is.null(dd)) dd$padj else NULL,
         upInDown = if (!is.null(ud)) ud$padj else NULL, downInUp = if (!is.null(du)) du$padj else NULL,
         genesMimic = unique(c(strsplit(uu$genes %||% "", ",")[[1]], strsplit(dd$genes %||% "", ",")[[1]])),
         genesReverse = unique(c(strsplit(ud$genes %||% "", ",")[[1]], strsplit(du$genes %||% "", ",")[[1]])))
  })
  ord <- function(key) utils::head(rows[order(-vapply(rows, `[[`, 0, key))], top)
  list(kind = kind, library = lib, upCount = length(up), downCount = length(down),
       mimic = Filter(function(r) r$mimic > 0, ord("mimic")), reverse = Filter(function(r) r$reverse > 0, ord("reverse")),
       citation = "LINCS L1000 consensus signatures (Subramanian et al. 2017, Cell 171:1437) via Enrichr (Kuleshov et al. 2016, Nucleic Acids Res 44:W90).",
       note = "Score = −log10 adjusted p of the up genes plus −log10 adjusted p of the down genes in the matching signature direction. Reverse = candidates that may counteract the response; mimic = perturbations that produce a similar response.")
}

#' Split the STRING network of a list into modules (Louvain) and enrich each module.
ppi_modules <- function(ids, taxid, score = 700L, sources = c("GO:BP", "KEGG", "REAC"), minSize = 4L) {
  flame_progress(0.1, "Fetching the STRING network")
  net <- ppi_network(ids, taxid, "functional", "confidence", score)
  e <- net$edges
  if (is.null(e) || nrow(e) < 3) stop("The STRING network has too few interactions at this confidence; try a lower score.")
  g <- igraph::graph_from_data_frame(e[, c("from", "to")], directed = FALSE)
  igraph::E(g)$weight <- e$score
  flame_progress(0.3, "Finding network modules (Louvain)")
  cl <- igraph::cluster_louvain(g, weights = igraph::E(g)$weight)
  memb <- igraph::membership(cl)
  groups <- split(names(memb), memb)
  groups <- groups[lengths(groups) >= minSize]
  groups <- groups[order(-lengths(groups))]
  if (!length(groups)) stop("No modules with at least ", minSize, " proteins were found.")
  short <- ORGANISMS$short_name[ORGANISMS$taxid == taxid][1]
  modules <- lapply(seq_along(groups), function(k) {
    flame_progress(0.35 + 0.6 * k / length(groups), sprintf("Enriching module %d of %d (%d proteins)", k, length(groups), length(groups[[k]])))
    res <- tryCatch(gprofiler2::gost(groups[[k]], organism = short, sources = sources, correction_method = "fdr", user_threshold = 0.05, evcodes = TRUE)$result,
                    error = function(e) NULL)
    top <- if (!is.null(res) && nrow(res)) utils::head(res[order(res$p_value), ], 5) else NULL
    list(id = k, size = length(groups[[k]]), genes = as.list(groups[[k]]),
         label = if (!is.null(top)) top$term_name[1] else sprintf("Module %d", k),
         terms = if (!is.null(top)) lapply(seq_len(nrow(top)), function(i) list(id = top$term_id[i], name = top$term_name[i], source = top$source[i], p = top$p_value[i], size = top$intersection_size[i],
                                                                         genes = if ("intersection" %in% names(top)) gsub(",", ", ", top$intersection[i]) else "")) else list(),
         density = round(igraph::edge_density(igraph::induced_subgraph(g, groups[[k]])), 3))
  })
  list(modules = modules, membership = as.list(stats::setNames(as.integer(memb), names(memb))), score = score,
       modularity = round(igraph::modularity(cl), 3), nodes = igraph::vcount(g), edges = igraph::ecount(g),
       links = lapply(seq_len(nrow(e)), function(i) list(from = e$from[i], to = e$to[i], score = e$score[i])),
       method = "STRING functional network (confidence >= score/1000) split with Louvain community detection; each module enriched with g:Profiler (FDR < 0.05).")
}

.fast_ora <- function(query, sets, universe) {
  q <- intersect(query, universe); n <- length(q); N <- length(universe)
  k <- vapply(sets, function(s) sum(s %in% q), 0L); K <- lengths(sets)
  p <- stats::phyper(k - 1, K, N - K, n, lower.tail = FALSE)
  p[k == 0] <- 1
  stats::p.adjust(p, "BH")
}

#' Robustness of an enrichment: bootstrap stability of each significant term and
#' a random-list control, using MSigDB gene sets locally (fast hypergeometric test).
enrichment_robustness <- function(ids, taxid, sources = c("GO:BP", "KEGG", "REAC"), nBoot = 50L, nRandom = 50L, fraction = 0.8, fdr = 0.05) {
  flame_progress(0.05, "Loading gene sets")
  sets <- msigdb_sets(taxid, sources)
  byTerm <- lapply(split(toupper(sets$symbol), sets$term), unique)
  byTerm <- byTerm[lengths(byTerm) >= 5 & lengths(byTerm) <= 2000]   # same limits as Flame's clusterProfiler ORA
  names_ <- sets$name[match(names(byTerm), sets$term)]; src <- sets$source[match(names(byTerm), sets$term)]
  universe <- unique(unlist(byTerm))
  query <- intersect(unique(toupper(ids)), universe)
  if (length(query) < 10) stop("Fewer than 10 genes of the list are in the gene-set universe.")
  obs <- .fast_ora(query, byTerm, universe)
  sig <- which(obs <= fdr)
  set.seed(1)
  boot <- matrix(FALSE, length(byTerm), nBoot)
  for (b in seq_len(nBoot)) {
    if (b %% 10 == 1) flame_progress(0.1 + 0.5 * b / nBoot, sprintf("Bootstrap %d of %d (%.0f%% of the genes each time)", b, nBoot, fraction * 100))
    boot[, b] <- .fast_ora(sample(query, ceiling(fraction * length(query))), byTerm, universe) <= fdr
  }
  rnd <- matrix(FALSE, length(byTerm), nRandom)
  for (r in seq_len(nRandom)) {
    if (r %% 10 == 1) flame_progress(0.6 + 0.35 * r / nRandom, sprintf("Random list %d of %d", r, nRandom))
    rnd[, r] <- .fast_ora(sample(universe, length(query)), byTerm, universe) <= fdr
  }
  stab <- rowMeans(boot); fp <- rowMeans(rnd)
  terms <- lapply(sig[order(obs[sig])], function(i) list(id = names(byTerm)[i], name = names_[i], source = src[i], padj = obs[i],
                                                            genes = paste(sort(intersect(byTerm[[i]], query)), collapse = ", "),
                                                            stability = round(stab[i], 3), randomRate = round(fp[i], 3)))
  thresholds <- lapply(c(0.001, 0.01, 0.05, 0.1, 0.25), function(t) list(fdr = t, terms = sum(obs <= t)))
  list(query = length(query), universe = length(universe), sets = length(byTerm), significant = length(sig),
       randomSignificant = as.list(colSums(rnd)), medianRandom = stats::median(colSums(rnd)),
       stable = sum(stab[sig] >= 0.8), terms = unname(utils::head(terms, 300)), thresholds = thresholds, nBoot = nBoot, nRandom = nRandom,
       method = sprintf("Hypergeometric test with Benjamini–Hochberg (FDR < %.2f) on MSigDB sets of %s. Stability = share of %d subsamples (%.0f%% of the genes) in which a term stays significant. Random rate = share of %d random lists of the same size in which it is significant.",
                        fdr, paste(sources, collapse = ", "), nBoot, fraction * 100, nRandom))
}

#' ssGSEA (Barbie et al. 2009): per-sample enrichment score of each gene set.
.ssgsea <- function(m, sets, alpha = 0.25) {
  sapply(seq_len(ncol(m)), function(j) {
    x <- m[, j]; o <- order(x, decreasing = TRUE); g <- rownames(m)[o]; r <- abs(rank(x)[o])^alpha
    vapply(sets, function(s) {
      hit <- g %in% s; nh <- sum(hit)
      if (nh < 3 || nh == length(g)) return(NA_real_)
      ph <- cumsum(hit * r) / sum(r[hit]); pm <- cumsum(!hit) / (length(g) - nh)
      sum(ph - pm)
    }, 0)
  })
}

#' Pathway activity per sample from an expression matrix (list from parse_expression_matrix).
#' method: "ssgsea" | "zscore" (Lee et al. 2008) on MSigDB sets, or "progeny" (14 signalling pathways).
pathway_activity <- function(x, method = c("ssgsea", "zscore", "progeny"), sources = "HALLMARK", taxid = 9606L, top = 40L) {
  method <- match.arg(method)
  m <- x$matrix
  rownames(m) <- toupper(rownames(m))
  if (method == "progeny") {
    flame_progress(0.3, "Scoring 14 signalling pathways with PROGENy")
    org <- if (identical(as.integer(taxid), 10090L)) "Mouse" else "Human"
    sc <- t(progeny::progeny(m, scale = TRUE, organism = org, top = 100))
    scores <- sc
    cite <- "PROGENy: Schubert et al. 2018, Nat Commun 9:20 (footprint genes of 14 signalling pathways, top 100 per pathway)."
  } else {
    flame_progress(0.2, "Loading gene sets")
    sets <- msigdb_sets(taxid, sources)
    by <- lapply(split(toupper(sets$symbol), sets$term), unique)
    by <- by[vapply(by, function(s) sum(s %in% rownames(m)) >= 5, logical(1))]
    if (!length(by)) stop("Too few genes of the matrix are in the selected gene sets.")
    flame_progress(0.5, sprintf("Scoring %d gene sets in %d samples (%s)", length(by), ncol(m), method))
    if (method == "ssgsea") {
      scores <- .ssgsea(m, by)
      cite <- "ssGSEA: Barbie et al. 2009, Nature 462:108 (rank-based single-sample enrichment, weight 0.25)."
    } else {
      z <- t(scale(t(m))); z[!is.finite(z)] <- 0
      scores <- t(vapply(by, function(s) { ii <- rownames(z) %in% s; colSums(z[ii, , drop = FALSE]) / sqrt(sum(ii)) }, numeric(ncol(z))))
      cite <- "Combined z-score: Lee et al. 2008, PLoS Comput Biol 4:e1000217."
    }
    if (is.null(dim(scores))) scores <- matrix(scores, ncol = ncol(m))
    rownames(scores) <- sets$name[match(names(by), sets$term)]
    colnames(scores) <- colnames(m)
  }
  scores <- scores[apply(scores, 1, function(r) all(is.finite(r))), , drop = FALSE]
  v <- apply(scores, 1, stats::var)
  keep <- order(-v)[seq_len(min(top, nrow(scores)))]
  s <- scores[keep, , drop = FALSE]
  ord <- if (nrow(s) > 2) stats::hclust(stats::dist(s))$order else seq_len(nrow(s))
  list(method = method, citation = cite, samples = as.list(colnames(s)), pathways = as.list(rownames(s)[ord]),
       scores = unname(lapply(ord, function(i) round(s[i, ], 3))), totalPathways = nrow(scores))
}

# ---- Metabolites: KEGG pathway enrichment -----------------------------------
.kegg_tables <- new.env()
.kegg_table <- function(path) {
  if (!is.null(.kegg_tables[[path]])) return(.kegg_tables[[path]])
  r <- httr::GET(paste0(KEGG_REST, "/", path), httr::timeout(120))
  if (httr::status_code(r) != 200) stop("KEGG did not respond (", path, ").")
  lines <- strsplit(rawToChar(httr::content(r, "raw")), "\n")[[1]]
  df <- do.call(rbind, lapply(strsplit(lines[nzchar(lines)], "\t"), function(x) data.frame(a = x[1], b = x[2], stringsAsFactors = FALSE)))
  .kegg_tables[[path]] <- df
  df
}

#' Map metabolite identifiers (KEGG C IDs, ChEBI, HMDB-free names) to KEGG compounds.
metabolite_ids <- function(ids) {
  ids <- unique(trimws(ids[nzchar(trimws(ids))]))
  kegg <- toupper(sub("^(cpd:)", "", ids[grepl("^(cpd:)?C[0-9]{5}$", ids, ignore.case = TRUE)]))
  chebi <- ids[grepl("^(CHEBI:)?[0-9]+$", ids, ignore.case = TRUE)]
  mapped <- stats::setNames(kegg, kegg)
  if (length(chebi)) {
    conv <- .kegg_table("conv/compound/chebi")   # a = cpd:Cxxxxx, b = chebi:NNN
    key <- paste0("chebi:", sub("^CHEBI:", "", chebi, ignore.case = TRUE))
    hit <- conv$a[match(key, conv$b)]
    mapped <- c(mapped, stats::setNames(sub("^cpd:", "", hit[!is.na(hit)]), chebi[!is.na(hit)]))
  }
  rest <- setdiff(ids, c(names(mapped), kegg))
  if (length(rest)) {
    names <- .kegg_table("list/compound")          # a = cpd:Cxxxxx or Cxxxxx, b = "name1; name2"
    syn <- strsplit(tolower(names$b), ";\\s*")
    idx <- rep(seq_along(syn), lengths(syn)); flat <- unlist(syn)
    hit <- names$a[idx[match(tolower(rest), flat)]]
    mapped <- c(mapped, stats::setNames(sub("^cpd:", "", hit[!is.na(hit)]), rest[!is.na(hit)]))
  }
  list(mapped = mapped, unmapped = setdiff(ids, names(mapped)))
}

#' KEGG pathway over-representation of metabolites; returns a run output so all
#' result views and comparisons work.
metabolite_enrichment <- function(ids, taxid = 9606L, threshold = 0.05) {
  flame_progress(0.1, "Mapping metabolites to KEGG compounds")
  m <- metabolite_ids(ids)
  cpds <- unique(m$mapped)
  if (length(cpds) < 3) stop("Fewer than 3 metabolites could be mapped to KEGG compounds (use KEGG IDs such as C00031, ChEBI IDs or names).")
  flame_progress(0.35, "Loading KEGG compound–pathway links")
  link <- .kegg_table("link/pathway/compound")  # a = cpd:Cxxxxx, b = path:map00010
  link$cpd <- sub("^cpd:", "", link$a); link$map <- sub("^path:", "", link$b)
  link <- link[grepl("^map", link$map), ]
  org <- KEGG_ORG[[as.character(taxid)]] %||% NA_character_
  pnames <- .kegg_table(if (!is.na(org)) paste0("list/pathway/", org) else "list/pathway")
  num <- sub("^[a-z]+", "", sub("^path:", "", pnames$a))
  link <- link[sub("^map", "", link$map) %in% num, ]            # pathways present in the organism
  sets <- split(link$cpd, link$map)
  sets <- sets[lengths(sets) >= 3]
  universe <- unique(unlist(sets))
  q <- intersect(cpds, universe)
  flame_progress(0.7, sprintf("Testing %d pathways", length(sets)))
  k <- vapply(sets, function(s) sum(s %in% q), 0L); K <- lengths(sets); N <- length(universe); n <- length(q)
  p <- stats::phyper(k - 1, K, N - K, n, lower.tail = FALSE); p[k == 0] <- 1
  padj <- stats::p.adjust(p, "BH")
  keep <- which(padj <= threshold & k > 0)
  back <- stats::setNames(names(m$mapped), m$mapped)   # KEGG ID -> user's name
  res <- if (length(keep)) {
    mp <- names(sets)[keep]
    data.frame(Source = "KEGG", Term_ID = mp, Function = sub(" - .*$", "", pnames$b[match(sub("^map", "", mp), num)]),
               `P-value` = padj[keep], `-log10Pvalue` = -log10(padj[keep]), `Term Size` = K[keep], `Query size` = n,
               `Intersection Size` = k[keep], `Gene Ratio` = k[keep] / n, `Term Coverage %` = round(100 * k[keep] / K[keep], 2),
               `Fold Enrichment` = round((k[keep] / n) / (K[keep] / N), 3),
               `Positive Hits` = vapply(sets[keep], function(s) paste(unique(back[intersect(s, q)]), collapse = ","), ""),
               Term_ID_noLinks = mp, link = paste0("https://www.kegg.jp/pathway/", mp, "+", vapply(sets[keep], function(s) paste(intersect(s, q), collapse = "+"), "")),
               check.names = FALSE, stringsAsFactors = FALSE)
  } else NULL
  if (!is.null(res)) res <- res[order(res$`P-value`), ]
  list(status = "complete", resultCount = if (is.null(res)) 0L else nrow(res), results = res,
       parameters = list(datasources = "KEGG", threshold = threshold, metric = "BH", namespace = "KEGG compound", organism = taxid,
                         tool = "KEGG compounds"),
       conversionTable = data.frame(input = names(m$mapped), target = unname(m$mapped), name = unname(m$mapped), stringsAsFactors = FALSE),
       backgroundSize = N, noHitAnalytes = m$unmapped,
       messages = c(sprintf("%d of %d metabolites mapped to KEGG compounds; %d are in %s pathways.", length(cpds), length(unique(ids)), n, if (is.na(org)) "KEGG" else org)))
}
