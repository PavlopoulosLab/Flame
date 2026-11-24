# =============================================================================
# FLAME core: Arena3D 3.0 export (https://arena3d.org/api/open)
# =============================================================================
# Arena3D stacks networks as layers in 3D. POST /api/open takes edges (with
# source/target layers, optional weight and channel) and node styling, lays the
# network out and returns a link that opens it (valid for one hour).
#   arena3d_open(edges, nodes)                    low-level call
#   arena3d_payload_for_view(results, view, ...)  result networks
#   arena3d_payload_for_comparison(cs, ...)       a Tools layer + one layer per term category
#   arena3d_payload_for_variants(result, ...)     variants → genes → traits
#   arena3d_payload_for_ppi(net, ...)             STRING evidence as channels
# =============================================================================

ARENA3D_OPEN_URL <- "https://arena3d.org/api/open"
ARENA3D_MAX_EDGES <- 10000L

.clip200 <- function(x) substr(gsub("[[:cntrl:]]", " ", as.character(x)), 1, 200)
.hex <- function(x) ifelse(grepl("^#[0-9a-fA-F]{6}$", x %||% ""), x, NA_character_)

#' Build the request body. `edges`: data frame with source, target, source_layer,
#' target_layer, optional weight / channel. `nodes`: data frame with name, layer,
#' optional color, size, url, description.
arena3d_body <- function(edges, nodes = NULL, layout = "Fruchterman-Reingold") {
  if (is.null(edges) || !nrow(edges)) stop("There are no edges to send to Arena3D.")
  if (nrow(edges) > ARENA3D_MAX_EDGES) edges <- edges[seq_len(ARENA3D_MAX_EDGES), , drop = FALSE]
  hasChannel <- "channel" %in% names(edges) && all(!is.na(edges$channel) & nzchar(edges$channel))
  e <- lapply(seq_len(nrow(edges)), function(i) {
    out <- list(source = .clip200(edges$source[i]), target = .clip200(edges$target[i]),
                source_layer = .clip200(edges$source_layer[i]), target_layer = .clip200(edges$target_layer[i]))
    if ("weight" %in% names(edges) && is.finite(edges$weight[i])) out$weight <- edges$weight[i]
    if (hasChannel) out$channel <- .clip200(edges$channel[i])
    out
  })
  n <- NULL
  if (!is.null(nodes) && nrow(nodes)) {
    nodes <- nodes[!duplicated(paste(nodes$name, nodes$layer)), , drop = FALSE]
    # Arena3D only accepts styling for nodes that take part in an edge.
    inNet <- unique(c(paste(.clip200(edges$source), .clip200(edges$source_layer)), paste(.clip200(edges$target), .clip200(edges$target_layer))))
    nodes <- nodes[paste(.clip200(nodes$name), .clip200(nodes$layer)) %in% inNet, , drop = FALSE]
    n <- lapply(seq_len(min(nrow(nodes), 20000L)), function(i) {
      out <- list(name = .clip200(nodes$name[i]), layer = .clip200(nodes$layer[i]))
      col <- .hex(nodes$color[i] %||% NA)
      if (!is.na(col)) out$color <- col
      if (!is.null(nodes$size) && is.finite(nodes$size[i])) out$size <- max(0.2, min(5, nodes$size[i]))
      if (!is.null(nodes$url) && !is.na(nodes$url[i]) && grepl("^https?://", nodes$url[i])) out$url <- substr(nodes$url[i], 1, 2000)
      if (!is.null(nodes$description) && !is.na(nodes$description[i])) out$description <- substr(nodes$description[i], 1, 2000)
      out
    })
  }
  list(edges = e, nodes = n, layout = layout, directed = FALSE)
}

#' Send to Arena3D; returns list(url, layers, nodes, edges, warnings)
arena3d_open <- function(body) {
  resp <- httr::POST(ARENA3D_OPEN_URL, body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
                     httr::content_type_json(), httr::accept_json(), httr::timeout(90))
  code <- httr::status_code(resp)
  txt <- rawToChar(httr::content(resp, "raw"))
  if (code != 200) {
    detail <- tryCatch(as.character(jsonlite::fromJSON(txt)$detail)[1], error = function(e) NA_character_)
    stop("Arena3D did not accept the network (HTTP ", code, ")",
         if (!is.na(detail)) paste0(": ", substr(gsub("[^[:print:]]", " ", detail), 1, 200)) else "", ".")
  }
  out <- jsonlite::fromJSON(txt)
  if (!is.character(out$url) || !grepl("^https://(www\\.)?arena3d\\.org/", out$url)) stop("Arena3D returned an unexpected answer.")
  list(url = out$url, layers = if (is.character(out$layers)) as.list(out$layers) else out$layers, nodes = out$nodes, edges = out$edges, warnings = as.list(out$warnings %||% list()))
}

.term_label <- function(t) substr(paste0(t$name, " (", t$id, ")"), 1, 200)

#' Result network views as layers: annotation sources (+ a Genes layer)
arena3d_payload_for_view <- function(results, view, filters = list(), top = 20L,
                                     sortBy = "-log10Pvalue", settings = list(), taxid = 9606L) {
  v <- build_view(results, view, filters, top, sortBy, settings, taxid = taxid)
  terms <- v$terms
  if (!length(terms)) stop("The current view has no terms.")
  byId <- stats::setNames(terms, vapply(terms, `[[`, "", "id"))
  col <- function(src) unname(DATASOURCE_COLORS[src] %||% NA)
  if (view == "network-gg") {
    net <- v$network
    if (!length(net$edges)) stop("The current view has no edges.")
    edges <- data.frame(source = vapply(net$edges, `[[`, "", "from"), target = vapply(net$edges, `[[`, "", "to"),
                        source_layer = "Genes", target_layer = "Genes", weight = vapply(net$edges, function(e) as.numeric(e$weight), 0),
                        stringsAsFactors = FALSE)
    nodes <- data.frame(name = unique(c(edges$source, edges$target)), layer = "Genes", color = GENE_NODE_COLOR, stringsAsFactors = FALSE)
    return(arena3d_body(edges, nodes))
  }
  if (view == "network-tt") {
    net <- v$network
    if (!length(net$edges)) stop("The current view has no edges.")
    from <- vapply(net$edges, `[[`, "", "from"); to <- vapply(net$edges, `[[`, "", "to")
    edges <- data.frame(source = vapply(from, function(i) .term_label(byId[[i]]), ""), target = vapply(to, function(i) .term_label(byId[[i]]), ""),
                        source_layer = vapply(from, function(i) byId[[i]]$source, ""), target_layer = vapply(to, function(i) byId[[i]]$source, ""),
                        weight = vapply(net$edges, function(e) as.numeric(e$weight), 0), stringsAsFactors = FALSE)
  } else {
    pairs <- do.call(rbind, lapply(terms, function(t) {
      g <- .split_hits(t$genes)
      if (!length(g)) return(NULL)
      data.frame(source = .term_label(t), target = g, source_layer = t$source, target_layer = "Genes", stringsAsFactors = FALSE)
    }))
    edges <- pairs
  }
  nodes <- do.call(rbind, lapply(terms, function(t) data.frame(
    name = .term_label(t), layer = t$source, color = col(t$source), size = min(3, 0.6 + t$log10p / 10),
    url = t$link %||% NA, description = sprintf("%s · p = %s · %d genes", t$source, signif(t$pvalue, 3), as.integer(t$intersection)),
    stringsAsFactors = FALSE)))
  if (view == "network-tg") {
    genes <- unique(edges$target)
    nodes <- rbind(nodes, data.frame(name = genes, layer = "Genes", color = GENE_NODE_COLOR, size = 1, url = NA, description = NA, stringsAsFactors = FALSE))
  }
  arena3d_body(edges, nodes)
}

#' Comparison: one layer per run (its terms) + a Genes layer; copies of the same
#' term in different runs are linked across layers ("same term" channel).
#' Terms of a comparison, the top `perSource` of each chosen annotation source.
#' Returns list(terms, runs (active only), sources).
comparison_terms_by_source <- function(cs, filters = list(), sources = NULL, perSource = 30L, sortBy = "-log10Pvalue") {
  filters <- filters %||% list()
  base <- comparison_view(cs, "bubble", filters, 1L, sortBy)
  available <- unlist(base$availableSources)
  shown <- unlist(filters$sources); if (!length(shown)) shown <- available
  sources <- as.character(unlist(sources)); sources <- sources[nzchar(sources)]
  sources <- if (length(sources)) intersect(sources, available) else intersect(shown, available)
  if (!length(sources)) stop("Choose at least one term category.")
  perSource <- max(1L, min(as.integer(perSource), 200L))
  terms <- list()
  for (src in sources) {
    f <- filters; f$sources <- list(src)
    terms <- c(terms, comparison_view(cs, "bubble", f, perSource, sortBy)$terms)
  }
  if (!length(terms)) stop("No terms match the current filters.")
  active <- unlist(base$activeRunIds)
  list(terms = terms, runs = Filter(function(r) r$id %in% active, base$runs), sources = sources)
}

#' A comparison as Arena3D layers: a "Tools" layer with the compared runs, one layer per
#' term category (GO:BP, KEGG...) with its terms, and optionally a "Genes" layer.
#' Links: run - term (the run found the term) and term - gene.
#' `layers`: list(tools = TRUE/FALSE, genes = TRUE/FALSE, sources = c("GO:BP", ...)).
arena3d_payload_for_comparison <- function(cs, filters = list(), top = 30L, sortBy = "-log10Pvalue", layers = list()) {
  withTools <- !identical(layers$tools, FALSE)
  withGenes <- isTRUE(layers$genes)
  if (!withTools && !withGenes) stop("Include the Tools layer or the Genes layer: the term layers need something to link to.")
  x <- comparison_terms_by_source(cs, filters, layers$sources, top, sortBy)
  runLabel <- function(r) substr(gsub("[^A-Za-z0-9 ()_.-]", "", r$label), 1, 60)
  runIds <- vapply(x$runs, `[[`, "", "id")
  labels <- stats::setNames(vapply(x$runs, runLabel, ""), runIds)
  edges <- list(); nodes <- list()
  if (withTools) nodes[[1]] <- data.frame(name = unname(labels), layer = "Tools", color = "#f97316", size = 2, url = NA,
                                          description = vapply(x$runs, function(r) paste(r$tool, "-", r$listName), ""), stringsAsFactors = FALSE)
  for (t in x$terms) {
    rs <- intersect(unlist(t$runIds), runIds)
    nodes[[length(nodes) + 1]] <- data.frame(name = .term_label(t), layer = t$source, color = unname(DATASOURCE_COLORS[t$source] %||% NA),
                                             size = 0.8 + 0.4 * length(rs), url = NA,
                                             description = sprintf("%s - combined p = %s - found by %d of %d runs: %s", t$source, signif(t$combPvalue, 3), length(rs), length(runIds), paste(labels[rs], collapse = ", ")),
                                             stringsAsFactors = FALSE)
    if (withTools && length(rs)) edges[[length(edges) + 1]] <- data.frame(source = unname(labels[rs]), target = .term_label(t), source_layer = "Tools",
                                                                          target_layer = t$source, channel = "found by", stringsAsFactors = FALSE)
    if (withGenes) {
      genes <- unique(.split_hits(t$unionHits))
      if (length(genes)) edges[[length(edges) + 1]] <- data.frame(source = .term_label(t), target = genes, source_layer = t$source,
                                                                  target_layer = "Genes", channel = "term-gene", stringsAsFactors = FALSE)
    }
  }
  if (!length(edges)) stop("There are no links to send to Arena3D for this choice of layers.")
  edges <- do.call(rbind, edges)
  nodes <- do.call(rbind, nodes)
  if (withGenes) {
    genes <- unique(edges$target[edges$target_layer == "Genes"])
    nodes <- rbind(nodes, data.frame(name = genes, layer = "Genes", color = GENE_NODE_COLOR, size = 0.8, url = NA, description = NA, stringsAsFactors = FALSE))
  }
  arena3d_body(edges, nodes)
}

#' Variant report: Variants → Genes → Traits (top traits by number of variants)
arena3d_payload_for_variants <- function(result, topTraits = 25L) {
  ann <- result$variants
  traits <- utils::head(vapply(result$summary$traits, `[[`, "", "trait"), topTraits)
  e1 <- do.call(rbind, lapply(seq_len(nrow(ann)), function(i) {
    g <- unique(strsplit(ann$allGenes[i] %||% "", ",")[[1]]); g <- g[nzchar(g) & !is.na(g)]
    if (!length(g)) return(NULL)
    data.frame(source = ann$id[i], target = g, source_layer = "Variants", target_layer = "Genes", channel = "affects gene", stringsAsFactors = FALSE)
  }))
  ph <- result$phenotypes
  e2 <- if (nrow(ph)) {
    ph <- ph[tolower(ph$trait) %in% tolower(traits), , drop = FALSE]
    unique(data.frame(source = ph$variant, target = substr(ph$trait, 1, 200), source_layer = "Variants", target_layer = "Traits",
                      channel = "associated trait", stringsAsFactors = FALSE))
  } else NULL
  edges <- rbind(e1, e2)
  impactCol <- c(HIGH = "#dc2626", MODERATE = "#f97316", LOW = "#eab308", MODIFIER = "#a1a1aa")
  nodes <- rbind(
    data.frame(name = ann$id, layer = "Variants", color = unname(impactCol[as.character(ann$impact)]), size = 1,
               url = ifelse(grepl("^rs[0-9]+$", ann$id), paste0("https://www.ensembl.org/Homo_sapiens/Variation/Explore?v=", ann$id), NA),
               description = paste0(ann$consequence, " · ", ann$impact, ifelse(is.na(ann$clinicalSignificance), "", paste0(" · ClinVar: ", ann$clinicalSignificance))),
               stringsAsFactors = FALSE),
    data.frame(name = unique(e1$target), layer = "Genes", color = "#f97316", size = 1, url = NA, description = NA, stringsAsFactors = FALSE),
    if (!is.null(e2) && nrow(e2)) data.frame(name = unique(e2$target), layer = "Traits", color = "#e11d48", size = 1.2, url = NA, description = NA, stringsAsFactors = FALSE)
  )
  arena3d_body(edges, nodes)
}

#' STRING network: one layer, evidence types as edge channels
arena3d_payload_for_ppi <- function(net) {
  e <- net$edges
  if (is.null(e) || !nrow(e)) stop("The network has no interactions.")
  ch <- c(escore = "Experiments", dscore = "Databases", tscore = "Text mining", ascore = "Co-expression",
          nscore = "Neighbourhood", fscore = "Gene fusion", pscore = "Co-occurrence")
  rows <- list()
  for (k in intersect(names(ch), names(e))) {
    keep <- as.numeric(e[[k]]) > 0
    if (any(keep)) rows[[k]] <- data.frame(source = e$from[keep], target = e$to[keep], source_layer = "Proteins", target_layer = "Proteins",
                                          weight = as.numeric(e[[k]][keep]), channel = ch[[k]], stringsAsFactors = FALSE)
  }
  edges <- if (length(rows)) do.call(rbind, rows) else data.frame(source = e$from, target = e$to, source_layer = "Proteins",
                                                                target_layer = "Proteins", weight = e$score, stringsAsFactors = FALSE)
  nodes <- data.frame(name = net$nodes$id, layer = "Proteins", color = "#f97316", size = pmin(3, 0.6 + net$nodes$degree / 10),
                      url = NA, description = paste("Query:", net$nodes$query), stringsAsFactors = FALSE)
  arena3d_body(edges, nodes)
}
