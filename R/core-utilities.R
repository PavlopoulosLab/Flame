# =============================================================================
# FLAME core: quick actions and custom sources as plain functions (no Shiny)
# =============================================================================
# Convert IDs (g:Convert), Find orthologs (g:Orth), PPI network (STRING),
# custom GMT sources (parse + hypergeometric ORA).
# =============================================================================

.organism_short_name <- function(taxid) {
  row <- ORGANISMS[ORGANISMS$taxid == as.integer(taxid), , drop = FALSE]
  if (nrow(row) == 0 || is.na(row$short_name[1]) || !nzchar(row$short_name[1])) return(NULL)
  as.character(row$short_name[1])
}

.organism_label <- function(taxid) {
  row <- ORGANISMS[ORGANISMS$taxid == as.integer(taxid), , drop = FALSE]
  if (nrow(row) == 0) as.character(taxid) else as.character(row$print_name[1])
}

.clean_chr <- function(x) {
  x <- trimws(as.character(x %||% character(0)))
  x[is.na(x) | x %in% c("nan", "None", "NA")] <- ""
  x
}

.mthreshold <- function(mode) if (identical(mode, "best")) 1 else Inf

#' Convert a list's IDs to another namespace (g:Convert)
convert_list_ids <- function(ids, taxid, targetFormat, mappingMode = "all") {
  shortName <- .organism_short_name(taxid)
  if (is.null(shortName)) stop("ID conversion is not available for this organism.")
  allowed <- vapply(getWorkspaceConversionTargetFormats(), function(f) f$value, character(1))
  if (!targetFormat %in% allowed) stop("This target ID format is not available for conversion.")
  res <- gconvert_ids(ids, organism = shortName, target_namespace = targetFormat,
                      mthreshold = .mthreshold(mappingMode), filter_na = FALSE)
  if (is.null(res) || nrow(res) == 0) stop("No conversion results were found. Try another target format.")
  target <- .clean_chr(res$target)
  rows <- data.frame(rowId = paste0("r", seq_len(nrow(res))), original = .clean_chr(res$input),
                     converted = target, symbol = .clean_chr(res$name),
                     description = .clean_chr(res$description), stringsAsFactors = FALSE)
  mapped <- unique(rows$original[nzchar(rows$converted)])
  if (length(mapped) == 0) stop("No IDs were converted for this organism and target format.")
  list(rows = rows,
       stats = list(inputCount = length(unique(ids)), mappedInputs = length(mapped),
                    unmappedInputs = length(setdiff(unique(ids), mapped)),
                    convertedIds = length(unique(target[nzchar(target)])),
                    ambiguousInputs = sum(table(rows$original[nzchar(rows$converted)]) > 1),
                    targetFormat = targetFormat, targetLabel = getWorkspaceIdFormatLabel(targetFormat),
                    mappingMode = mappingMode))
}

#' Find orthologs in another organism (g:Orth)
find_orthologs <- function(ids, sourceTaxid, targetTaxid, idFormat = "USERINPUT", mappingMode = "all") {
  src <- .organism_short_name(sourceTaxid); tgt <- .organism_short_name(targetTaxid)
  if (is.null(src)) stop("Ortholog search is not available for the list's organism.")
  if (is.null(tgt)) stop("Ortholog search is not available for the target organism.")
  if (identical(as.integer(sourceTaxid), as.integer(targetTaxid))) stop("Choose a target organism different from the list's organism.")
  res <- gorth_ids(ids, source_organism = src, target_organism = tgt,
                   numeric_ns = if (identical(idFormat, "ENTREZGENE_ACC")) "ENTREZGENE_ACC" else "",
                   mthreshold = .mthreshold(mappingMode), filter_na = FALSE)
  if (is.null(res) || nrow(res) == 0) stop("No orthologs were found.")
  rows <- data.frame(rowId = paste0("r", seq_len(nrow(res))), original = .clean_chr(res$input),
                     originalEnsg = .clean_chr(res$input_ensg), ortholog = .clean_chr(res$ortholog_name),
                     orthologEnsg = .clean_chr(res$ortholog_ensg), description = .clean_chr(res$description),
                     stringsAsFactors = FALSE)
  found <- nzchar(rows$ortholog) | nzchar(rows$orthologEnsg)
  if (!any(found)) stop("No orthologs were found in the target organism.")
  list(rows = rows,
       stats = list(inputCount = length(unique(ids)), mappedInputs = length(unique(rows$original[found])),
                    orthologs = length(unique(c(rows$ortholog[nzchar(rows$ortholog)]))),
                    sourceOrganism = .organism_label(sourceTaxid), targetOrganism = .organism_label(targetTaxid),
                    mappingMode = mappingMode))
}

#' STRING protein–protein interaction network for a list (≤ 500 IDs)
ppi_network <- function(ids, taxid, networkType = "functional", networkEdges = "evidence", score = 400) {
  if (!tool_supports_organism(ToolId$STRING, taxid)) stop("STRING does not support this organism.")
  if (length(ids) > 500) stop("STRING networks are limited to 500 IDs.")
  networkType <- if (identical(networkType, "physical")) "physical" else "functional"
  networkEdges <- if (identical(networkEdges, "confidence")) "confidence" else "evidence"
  score <- suppressWarnings(as.integer(score)); if (is.na(score) || !score %in% c(150L, 400L, 700L, 900L)) score <- 400L

  req <- httr::POST("https://string-db.org/api/json/get_string_ids",
                    body = list(identifiers = paste0(ids, collapse = "%0d"), species = taxid,
                                caller_identity = "Flame@bib.fleming"))
  if (httr::status_code(req) != 200) stop("STRING did not respond. Please try again later.")
  map <- jsonlite::fromJSON(rawToChar(httr::content(req, "raw")))
  if (!is.data.frame(map) || nrow(map) == 0) stop("None of the IDs could be mapped to STRING.")
  stringIds <- unique(map$stringId)
  idsPost <- paste(stringIds, collapse = "%0d")

  tsv <- string_network_get_data(idsPost, taxid, networkType, networkEdges, score)
  link <- tryCatch(string_network_get_link(idsPost, taxid, networkType, networkEdges, score), error = function(e) "")
  df <- tryCatch(utils::read.delim(text = tsv, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  channels <- intersect(c("nscore", "fscore", "pscore", "ascore", "escore", "dscore", "tscore"), names(df))
  edges <- if (nrow(df) > 0) {
    e <- data.frame(from = df$preferredName_A, to = df$preferredName_B, score = df$score, stringsAsFactors = FALSE)
    for (ch in channels) e[[ch]] <- df[[ch]]
    unique(e[e$from != e$to, , drop = FALSE])
  } else data.frame(from = character(0), to = character(0), score = numeric(0))
  nodeNames <- unique(c(edges$from, edges$to, map$preferredName))
  nodes <- data.frame(id = nodeNames, label = nodeNames,
                      query = vapply(nodeNames, function(n) paste(map$queryItem[map$preferredName == n], collapse = ", "), character(1)),
                      stringsAsFactors = FALSE)
  nodes$degree <- vapply(nodes$id, function(n) sum(edges$from == n | edges$to == n), numeric(1))
  list(nodes = nodes, edges = edges, link = if (grepl("^https://", link)) link else NULL,
       tsv = tsv, settings = list(networkType = networkType, networkEdges = networkEdges, score = score),
       stats = list(nodes = nrow(nodes), edges = nrow(edges),
                    averageDegree = if (nrow(nodes) > 0) round(2 * nrow(edges) / nrow(nodes), 1) else 0,
                    unmapped = length(setdiff(ids, map$queryItem))))
}

# ---- Custom GMT sources -----------------------------------------------------

#' Parse GMT text: one gene set per line, `id <tab> description <tab> gene...`
parse_gmt <- function(text, maxTerms = 20000L) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  if (nchar(text, type = "bytes") > 2 * MAX_INPUT_BYTES) stop("GMT files are limited to 2 MB.")
  lines <- strsplit(text, "\r\n|\n|\r", perl = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  sets <- lapply(lines, function(l) {
    parts <- strsplit(l, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 3) return(NULL)
    genes <- unique(trimws(parts[-(1:2)])); genes <- genes[nzchar(genes)]
    if (length(genes) == 0) return(NULL)
    list(id = trimws(parts[1]), name = trimws(if (nzchar(trimws(parts[2]))) parts[2] else parts[1]), genes = genes)
  })
  sets <- Filter(Negate(is.null), sets)
  if (length(sets) == 0) stop("No gene sets found. GMT lines must be: ID <tab> description <tab> genes...")
  if (length(sets) > maxTerms) stop(sprintf("At most %d gene sets are allowed.", maxTerms))
  sets
}

#' Hypergeometric over-representation against custom gene sets (BH adjusted)
#' Returns results in the same column layout as execute_ora_run().
execute_gmt_run <- function(lst, sets, sourceName, threshold = 0.05, correction = "BH",
                            backgroundIds = NULL) {
  flame_progress(0.3, sprintf("Testing %d custom gene sets", length(sets)))
  q <- unique(toupper(as.character(unlist(lst$ids))))
  universe <- unique(toupper(if (!is.null(backgroundIds)) unlist(backgroundIds) else unlist(lapply(sets, `[[`, "genes"))))
  q <- intersect(q, universe)
  N <- length(universe); n <- length(q)
  out <- list(status = "complete", resultCount = 0L, results = NULL,
              parameters = list(geneListName = lst$name, datasources = sourceName, threshold = threshold,
                                namespace = "USERINPUT", metric = correction,
                                backgroundMode = if (is.null(backgroundIds)) "genome" else "custom"),
              conversionTable = data.frame(input = as.character(unlist(lst$ids)), target = toupper(as.character(unlist(lst$ids))),
                                           name = as.character(unlist(lst$ids)), stringsAsFactors = FALSE),
              backgroundSize = N, noHitAnalytes = character(0), messages = character(0))
  if (n == 0) { out$messages <- "None of the list's IDs occur in this custom source."; return(out) }

  rows <- lapply(sets, function(s) {
    g <- intersect(unique(toupper(s$genes)), universe)
    hits <- intersect(q, g); k <- length(hits); K <- length(g)
    if (k == 0) return(NULL)
    data.frame(Source = sourceName, Term_ID = s$id, Function = s$name,
               p = stats::phyper(k - 1, K, N - K, n, lower.tail = FALSE),
               `Term Size` = K, `Query size` = n, `Intersection Size` = k,
               `Positive Hits` = paste(hits, collapse = ","), check.names = FALSE, stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(df) || nrow(df) == 0) return(out)
  method <- if (correction %in% c("BH", "bonferroni", "none")) correction else "BH"
  df$`P-value` <- stats::p.adjust(df$p, method = method)
  df <- df[df$`P-value` <= threshold, , drop = FALSE]
  if (nrow(df) == 0) return(out)
  df$`-log10Pvalue` <- -log10(pmax(df$`P-value`, .Machine$double.xmin))
  df$`Gene Ratio` <- df$`Intersection Size` / df$`Query size`
  df$`Term Coverage %` <- round(df$`Intersection Size` / df$`Term Size` * 100, 2)
  df$`Fold Enrichment` <- (df$`Intersection Size` / n) / (df$`Term Size` / N)
  df$Term_ID_noLinks <- df$Term_ID
  df$link <- NA_character_
  df <- df[order(-df$`-log10Pvalue`, df$Term_ID), c("Source", "Term_ID", "Function", "P-value", "-log10Pvalue",
          "Term Size", "Query size", "Intersection Size", "Gene Ratio", "Term Coverage %",
          "Fold Enrichment", "Positive Hits", "Term_ID_noLinks", "link")]
  rownames(df) <- NULL
  hitGenes <- unique(unlist(strsplit(df$`Positive Hits`, ",")))
  out$results <- df; out$resultCount <- nrow(df)
  out$noHitAnalytes <- setdiff(q, hitGenes)
  out
}
