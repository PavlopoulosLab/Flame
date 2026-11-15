# =============================================================================
# FLAME core: result views as plain data (no Shiny, no htmlwidgets)
# =============================================================================
# The web client renders charts; R only filters, ranks and shapes data.
# Semantics are lifted from the Shiny OutputSession classes:
#   filters       -> OutputSession$applyFiltersFromRequest (output-session-base.R)
#   top/sort      -> OutputSession$applyTopSortLimit
#   heatmap T×G/T×T/G×G and network T×G/T×T/G×G
#                 -> output-session-heatmap.R / output-session-network.R
# =============================================================================

VIEW_IDS <- c("table", "dotplot", "barchart", "scatter",
              "heatmap-tg", "heatmap-tt", "heatmap-gg",
              "network-tg", "network-tt", "network-gg")

NETWORK_LAYOUTS <- c(
  force = "layout_nicely", graphopt = "layout_with_graphopt",
  kk = "layout_with_kk", mds = "layout_with_mds", tree = "layout_as_tree",
  star = "layout_as_star", circle = "layout_in_circle", grid = "layout_on_grid",
  random = "layout_randomly", hierarchical = "layout_as_tree"
)

.num <- function(x) suppressWarnings(as.numeric(as.character(x)))

.split_hits <- function(x) {
  out <- trimws(unlist(strsplit(as.character(x %||% ""), ",\\s*"), use.names = FALSE))
  out[nzchar(out)]
}

#' Apply sidebar filters (sources, search, ranges) to a results data frame
apply_result_filters <- function(data, filters = list()) {
  if (is.null(data) || nrow(data) == 0) return(data)
  filters <- filters %||% list()

  applyRange <- function(df, column, minVal = NULL, maxVal = NULL) {
    if (!column %in% names(df)) return(df)
    vals <- .num(df[[column]])
    keep <- rep(TRUE, length(vals))
    minNum <- .num(minVal %||% NA)
    maxNum <- .num(maxVal %||% NA)
    if (length(minNum) == 1 && is.finite(minNum)) keep <- keep & !is.na(vals) & vals >= minNum
    if (length(maxNum) == 1 && is.finite(maxNum)) keep <- keep & !is.na(vals) & vals <= maxNum
    df[keep, , drop = FALSE]
  }

  sources <- unlist(filters$sources)
  if (!is.null(filters$sources)) data <- data[data$Source %in% sources, , drop = FALSE]

  search <- as.character(filters$search %||% "")
  if (length(search) == 1 && nzchar(search)) {
    # Bound query length to keep the parser cheap on hostile input.
    data <- filterRowsBySearchQuery(data, substr(search, 1, 500),
                                    c("Function", "Term_ID", "Term_ID_noLinks"))
  }

  data <- applyRange(data, "P-value", maxVal = filters$pvalueMax)
  data <- applyRange(data, "Term Size", filters$termSizeMin, filters$termSizeMax)
  data <- applyRange(data, "Intersection Size", filters$intersectionMin, filters$intersectionMax)
  data <- applyRange(data, "Term Coverage %", filters$termCoverageMin, filters$termCoverageMax)
  data
}

#' Sort (descending by metric, ties by term ID) and keep the top N rows
apply_top_sort <- function(data, top = 20L, sortBy = "-log10Pvalue") {
  if (is.null(data) || nrow(data) == 0) return(data)
  orderCol <- switch(as.character(sortBy %||% "-log10Pvalue"),
                     "Term Coverage" = "Term Coverage %",
                     "Gene Ratio" = "Gene Ratio",
                     "Fold Enrichment" = "Fold Enrichment",
                     "NES" = "NES",
                     "-log10Pvalue")
  if (orderCol %in% names(data)) {
    values <- .num(data[[orderCol]])
    if (orderCol == "NES") values <- abs(values)   # strongest enrichment in either direction
    tieBreak <- if ("Term_ID_noLinks" %in% names(data)) as.character(data$Term_ID_noLinks) else seq_len(nrow(data))
    data <- data[order(-values, tieBreak, na.last = TRUE), , drop = FALSE]
  }
  topN <- suppressWarnings(as.integer(unlist(top)[1]))
  if (is.na(topN) || topN < 1L) topN <- 20L
  data[seq_len(min(topN, nrow(data))), , drop = FALSE]
}

#' Rows as a list of named records for JSON (stable column names)
.records <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  rownames(df) <- NULL
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

.term_rows <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  out <- data.frame(
    id = as.character(df$Term_ID_noLinks %||% df$Term_ID),
    source = as.character(df$Source),
    name = as.character(df$Function),
    pvalue = .num(df$`P-value`),
    log10p = .num(df$`-log10Pvalue`),
    termSize = .num(df$`Term Size`),
    querySize = .num(df$`Query size`),
    intersection = .num(df$`Intersection Size`),
    geneRatio = .num(df$`Gene Ratio`),
    coverage = .num(df$`Term Coverage %`),
    foldEnrichment = .num(df$`Fold Enrichment`),
    genes = as.character(df$`Positive Hits`),
    link = as.character(df$link %||% NA_character_),
    nes = .num(df$NES %||% NA),
    stringsAsFactors = FALSE
  )
  .records(out)
}

# ---- Heatmaps ---------------------------------------------------------------

.term_pairs <- function(data) {
  # Long table: one row per (term, gene)
  pairs <- do.call(rbind, lapply(seq_len(nrow(data)), function(i) {
    g <- .split_hits(data$`Positive Hits`[i])
    if (length(g) == 0) return(NULL)
    data.frame(term = as.character(data$Term_ID_noLinks[i]), gene = g, stringsAsFactors = FALSE)
  }))
  if (is.null(pairs)) data.frame(term = character(0), gene = character(0)) else unique(pairs)
}

#' Term × term similarity: common genes / union of genes × 100 (rounded to 2 dp)
term_similarity_edges <- function(data) {
  pairs <- .term_pairs(data)
  terms <- unique(as.character(data$Term_ID_noLinks))
  genesBy <- split(pairs$gene, pairs$term)
  grid <- expand.grid(a = terms, b = terms, stringsAsFactors = FALSE)
  common <- mapply(function(a, b) length(intersect(genesBy[[a]], genesBy[[b]])), grid$a, grid$b)
  total <- mapply(function(a, b) length(union(genesBy[[a]], genesBy[[b]])), grid$a, grid$b)
  keep <- common > 0
  data.frame(source = grid$a[keep], target = grid$b[keep],
             common = common[keep], total = total[keep],
             similarity = round(common[keep] / total[keep] * 100, 2),
             stringsAsFactors = FALSE)
}

#' Gene × gene shared functions: number of terms both genes hit
gene_cooccurrence_edges <- function(data) {
  pairs <- .term_pairs(data)
  if (nrow(pairs) == 0) return(data.frame(source = character(0), target = character(0), shared = numeric(0)))
  m <- merge(pairs, pairs, by = "term")
  agg <- stats::aggregate(term ~ gene.x + gene.y, data = m, FUN = length)
  data.frame(source = agg$gene.x, target = agg$gene.y, shared = agg$term, stringsAsFactors = FALSE)
}

# ---- Networks ---------------------------------------------------------------

.layout_coords <- function(graph, layout = "force") {
  fn <- NETWORK_LAYOUTS[[layout %||% "force"]] %||% "layout_nicely"
  set.seed(123)
  coords <- tryCatch(do.call(getExportedValue("igraph", fn), list(graph)),
                     error = function(e) igraph::layout_nicely(graph))
  if (nrow(coords) == 0) return(coords)
  # Scale to a stable canvas size for the client.
  scale01 <- function(v) if (diff(range(v)) == 0) rep(0.5, length(v)) else (v - min(v)) / diff(range(v))
  cbind(scale01(coords[, 1]) * 1000, scale01(coords[, 2]) * 1000)
}

.edge_width <- function(w, newMin = 0.1, newMax = 3) {
  if (length(w) == 0) return(numeric(0))
  r <- max(w) - min(w)
  if (r == 0) rep(1, length(w)) else ((w - min(w)) * (newMax - newMin) / r) + newMin
}

.network_payload <- function(nodes, edges, layout) {
  if (nrow(edges) == 0) return(list(nodes = list(), edges = list(), layout = layout))
  graph <- igraph::graph_from_data_frame(edges[, c("from", "to")], directed = FALSE,
                                         vertices = data.frame(name = nodes$id))
  xy <- .layout_coords(graph, layout)
  nodes$x <- xy[, 1]
  nodes$y <- xy[, 2]
  edges$id <- seq_len(nrow(edges))
  edges$width <- .edge_width(edges$weight)
  list(nodes = .records(nodes), edges = .records(edges), layout = layout)
}

#' Build the data for one result view
#'
#' @param results Full results data frame for the run (from execute_ora_run)
#' @param view One of VIEW_IDS
#' @param filters Sidebar filters (list)
#' @param top Top N
#' @param sortBy Sort key ("-log10Pvalue", "Term Coverage", "Gene Ratio", "Fold Enrichment")
#' @param settings View settings (labels, layout, similarityCutoff, minShared, orientation)
build_view <- function(results, view, filters = list(), top = 20L,
                       sortBy = "-log10Pvalue", settings = list()) {
  if (!view %in% VIEW_IDS) stop("Unknown view: ", view)
  settings <- settings %||% list()
  total <- if (is.null(results)) 0L else nrow(results)
  filtered <- apply_result_filters(results, filters)
  filteredCount <- if (is.null(filtered)) 0L else nrow(filtered)
  data <- apply_top_sort(filtered, top, sortBy)

  meta <- list(view = view, totalCount = total, filteredCount = filteredCount,
               shownCount = if (is.null(data)) 0L else nrow(data),
               availableSources = if (total > 0) as.list(sort(unique(as.character(results$Source)))) else list(),
               sortBy = sortBy, top = top)

  if (is.null(data) || nrow(data) == 0) return(c(meta, list(terms = list())))
  terms <- .term_rows(data)

  if (view %in% c("table", "dotplot", "barchart", "scatter")) {
    return(c(meta, list(terms = terms)))
  }

  if (view == "heatmap-tg") {
    pairs <- .term_pairs(data)
    return(c(meta, list(terms = terms, cells = .records(pairs))))
  }
  if (view == "heatmap-tt") {
    return(c(meta, list(terms = terms, cells = .records(term_similarity_edges(data)))))
  }
  if (view == "heatmap-gg") {
    return(c(meta, list(terms = terms, cells = .records(gene_cooccurrence_edges(data)))))
  }

  layout <- as.character(settings$layout %||% "force")
  if (!layout %in% names(NETWORK_LAYOUTS)) layout <- "force"

  if (view == "network-tg") {
    pairs <- .term_pairs(data)
    termIds <- unique(pairs$term)
    genes <- unique(pairs$gene)
    nodes <- data.frame(
      id = c(termIds, genes),
      label = c(termIds, genes),
      kind = c(rep("term", length(termIds)), rep("gene", length(genes))),
      group = c(as.character(data$Source[match(termIds, data$Term_ID_noLinks)]), rep("Gene", length(genes))),
      name = c(as.character(data$Function[match(termIds, data$Term_ID_noLinks)]), genes),
      stringsAsFactors = FALSE
    )
    edges <- data.frame(from = pairs$term, to = pairs$gene, weight = 1, stringsAsFactors = FALSE)
    return(c(meta, list(terms = terms, network = .network_payload(nodes, edges, layout))))
  }

  if (view == "network-tt") {
    cutoff <- .num(settings$similarityCutoff %||% 10)
    if (is.na(cutoff)) cutoff <- 10
    e <- term_similarity_edges(data)
    e <- e[e$source < e$target & e$similarity >= cutoff, , drop = FALSE]
    if (nrow(e) == 0) return(c(meta, list(terms = terms, network = list(nodes = list(), edges = list(), layout = layout))))
    ids <- unique(c(e$source, e$target))
    nodes <- data.frame(id = ids, label = ids, kind = "term",
                        group = as.character(data$Source[match(ids, data$Term_ID_noLinks)]),
                        name = as.character(data$Function[match(ids, data$Term_ID_noLinks)]),
                        stringsAsFactors = FALSE)
    edges <- data.frame(from = e$source, to = e$target, weight = e$similarity,
                        common = e$common, total = e$total, stringsAsFactors = FALSE)
    return(c(meta, list(terms = terms, network = .network_payload(nodes, edges, layout))))
  }

  # network-gg
  minShared <- .num(settings$minShared %||% 3)
  if (is.na(minShared)) minShared <- 3
  e <- gene_cooccurrence_edges(data)
  e <- e[e$source < e$target & e$shared >= minShared, , drop = FALSE]
  if (nrow(e) == 0) return(c(meta, list(terms = terms, network = list(nodes = list(), edges = list(), layout = layout))))
  ids <- unique(c(e$source, e$target))
  nodes <- data.frame(id = ids, label = ids, kind = "gene", group = "Gene", name = ids,
                      stringsAsFactors = FALSE)
  edges <- data.frame(from = e$source, to = e$target, weight = e$shared, stringsAsFactors = FALSE)
  c(meta, list(terms = terms, network = .network_payload(nodes, edges, layout)))
}

# ---- NORMA (network annotation viewer) export ------------------------------

NORMA_API_URL <- "https://normaviz.org/api/external"

#' Build a NORMA payload for a network view: edges, groups and node values.
#' Groups: term–gene network -> each term with its genes; gene–gene -> genes
#' grouped by the shown terms; term–term -> terms grouped by annotation source.
norma_payload_for_view <- function(results, view, filters = list(), top = 20L,
                                   sortBy = "-log10Pvalue", settings = list(), name = "Flame network") {
  v <- build_view(results, view, filters, top, sortBy, settings)
  net <- v$network
  if (is.null(net) || length(net$edges) == 0) stop("The current view has no edges to send.")
  terms <- v$terms
  label <- function(id) {
    t <- Filter(function(x) identical(x$id, id), terms)
    if (length(t)) paste0(t[[1]]$name, " (", id, ")") else id
  }
  edges <- lapply(net$edges, function(e) list(source = label(e$from), target = label(e$to), weight = e$weight))
  groups <- list()
  if (view == "network-tt") {
    for (t in terms) groups[[t$source]] <- c(groups[[t$source]], label(t$id))
  } else {
    for (t in terms) {
      genes <- .split_hits(t$genes)
      members <- if (view == "network-tg") c(label(t$id), genes) else genes
      groups[[substr(t$name, 1, 60)]] <- unique(members)
    }
  }
  expression <- list()
  for (t in terms) expression[[label(t$id)]] <- round(t$log10p, 3)
  list(name = substr(name, 1, 80), edges = edges, groups = groups,
       expression = if (view == "network-gg") NULL else expression,
       settings = list(layout = "fr", showGroupHulls = TRUE))
}

#' Send a payload to NORMA; returns the link that opens it (valid 24 h).
norma_export <- function(payload) {
  resp <- httr::POST(NORMA_API_URL, body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
                     httr::content_type_json(), httr::accept_json(), httr::timeout(60))
  if (httr::status_code(resp) != 200) stop("NORMA did not accept the network (HTTP ", httr::status_code(resp), ").")
  out <- jsonlite::fromJSON(rawToChar(httr::content(resp, "raw")))
  if (!is.character(out$url) || !grepl("^https://", out$url)) stop("NORMA returned an unexpected answer.")
  out$url
}
