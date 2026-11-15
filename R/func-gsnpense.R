# =============================================================================
# FLAME gSNPense Functions
# =============================================================================
#
# API wrapper function for the gSNPense API (gprofiler2 package).
# Used by the workspace SNPs to Genes input method for converting SNP IDs
# to gene information.
#
# API Documentation: https://biit.cs.ut.ee/gprofiler/snpense
#
# Dependencies:
# - gprofiler2 package
#
# =============================================================================

#' Convert SNP IDs to gene information using gSNPense
#'
#' Wraps gprofiler2::gsnpense() to convert dbSNP IDs to gene mappings.
#'
#' @param snp_ids Character vector. dbSNP IDs (e.g., "rs123", "rs456").
#' @param filter_na Logical. Whether to filter out NA results. Default TRUE.
#' @return Data frame with SNP-to-gene mappings, or NULL on error.
#'
#' @examples
#' \dontrun{
#' result <- gsnpense_convert(c("rs11734132", "rs7961894", "rs4430796"))
#' }
gsnpense_convert <- function(snp_ids, filter_na = TRUE) {
  tryCatch({
    if (length(snp_ids) == 0) {
      cat("[gsnpense_convert] No SNP IDs provided\n")
      return(NULL)
    }

    result <- gprofiler2::gsnpense(snp_ids, filter_na = filter_na)

    if (is.null(result) || nrow(result) == 0) {
      cat("[gsnpense_convert] No results returned from gsnpense\n")
      return(NULL)
    }

    return(result)
  }, error = function(e) {
    cat(paste("[gsnpense_convert] gsnpense_convert error:", conditionMessage(e), "\n"))
    return(NULL)
  })
}

#' Parse SNP rsIDs from text or uploaded file content
#'
#' @param value Character vector, scalar text, data frame, or list-like input.
#' @return List with unique rsIDs and parsing stats.
parse_snp_ids <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(list(
      ids = character(0),
      stats = list(total = 0L, unique = 0L, duplicates = 0L, invalid = 0L)
    ))
  }

  if (is.data.frame(value)) {
    raw <- as.character(unlist(value, use.names = FALSE))
  } else {
    raw <- unlist(strsplit(paste(as.character(value), collapse = "\n"),
                           "[\\s,;]+", perl = TRUE), use.names = FALSE)
  }

  raw <- trimws(raw)
  raw <- raw[nzchar(raw)]

  if (length(raw) == 0) {
    return(list(
      ids = character(0),
      stats = list(total = 0L, unique = 0L, duplicates = 0L, invalid = 0L)
    ))
  }

  validMask <- grepl("^rs[0-9]+$", raw, ignore.case = TRUE)
  valid <- tolower(raw[validMask])
  invalid <- sum(!validMask)
  duplicateMask <- duplicated(valid)
  ids <- valid[!duplicateMask]

  list(
    ids = ids,
    stats = list(
      total = length(raw),
      unique = length(ids),
      duplicates = sum(duplicateMask),
      invalid = invalid
    )
  )
}

gsnpense_format_effects <- function(result) {
  variantCols <- grep("^variants\\.", colnames(result), value = TRUE)
  if (length(variantCols) == 0) {
    return(rep("", nrow(result)))
  }

  vapply(seq_len(nrow(result)), function(i) {
    active <- variantCols[vapply(variantCols, function(col) {
      value <- result[[col]][i]
      if (is.na(value)) return(FALSE)
      if (is.logical(value)) return(isTRUE(value))
      numericValue <- suppressWarnings(as.numeric(value))
      !is.na(numericValue) && numericValue != 0
    }, logical(1))]

    if (length(active) == 0) return("")

    labels <- gsub("^variants\\.", "", active)
    labels <- gsub("_variant$", "", labels)
    labels <- gsub("_", " ", labels)
    paste(labels, collapse = ", ")
  }, character(1))
}

gsnpense_column <- function(result, column) {
  if (!column %in% colnames(result)) {
    return(rep("", nrow(result)))
  }
  values <- result[[column]]
  if (is.list(values) && !is.data.frame(values)) {
    return(vapply(values, function(item) {
      item <- as.character(unlist(item, use.names = FALSE))
      item <- item[!is.na(item) & nzchar(trimws(item))]
      paste(item, collapse = ", ")
    }, character(1)))
  }
  as.character(values)
}

#' Format g:SNPense results for workspace preview rendering
#'
#' @param result Data frame returned from gprofiler2::gsnpense().
#' @return Data frame with display columns and external-link URLs.
format_gsnpense_mapping_rows <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(data.frame())
  }

  snps <- gsnpense_column(result, "rs_id")
  ensgs <- gsnpense_column(result, "ensgs")

  data.frame(
    snp = snps,
    gene = gsnpense_column(result, "gene_names"),
    ensembl = ensgs,
    chromosome = gsnpense_column(result, "chromosome"),
    start = gsnpense_column(result, "start"),
    end = gsnpense_column(result, "end"),
    strand = gsnpense_column(result, "strand"),
    effect = gsnpense_format_effects(result),
    snpUrl = ifelse(grepl("^rs[0-9]+$", snps, ignore.case = TRUE),
                    sprintf("https://www.ncbi.nlm.nih.gov/snp/%s", snps), ""),
    ensemblUrl = ifelse(grepl("^ENS", ensgs),
                        sprintf("https://www.ensembl.org/id/%s", ensgs), ""),
    stringsAsFactors = FALSE
  )
}

#' Extract final gene IDs from a g:SNPense result
#'
#' @param result Data frame returned from gprofiler2::gsnpense().
#' @param idFormat Workspace ID format. USERINPUT uses gene symbols, ENSG uses Ensembl IDs.
#' @return Unique non-empty IDs.
extract_gsnpense_list_ids <- function(result, idFormat = "USERINPUT") {
  if (is.null(result) || nrow(result) == 0) {
    return(character(0))
  }

  canonical <- normalizeWorkspaceIdFormat(idFormat)
  column <- if (identical(canonical, "ENSG")) "ensgs" else "gene_names"
  if (!column %in% colnames(result)) {
    return(character(0))
  }
  ids <- as.character(unlist(result[[column]], use.names = FALSE))
  ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
  unique(ids)
}

#' Build summary stats for a g:SNPense conversion result
#'
#' @param parsed Parsed SNP input from parse_snp_ids().
#' @param result Data frame returned from gprofiler2::gsnpense().
#' @return Named list of counts for client rendering.
summarize_gsnpense_mapping <- function(parsed, result) {
  genes <- extract_gsnpense_list_ids(result, "USERINPUT")
  ensgs <- extract_gsnpense_list_ids(result, "ENSG")
  inputStats <- parsed$stats
  statValue <- function(name) {
    value <- inputStats[[name]]
    if (is.null(value) || length(value) == 0 || is.na(value[1])) return(0L)
    value[[1]]
  }

  list(
    totalInput = statValue("total"),
    uniqueInput = statValue("unique"),
    duplicates = statValue("duplicates"),
    invalid = statValue("invalid"),
    mappedSnps = if (is.null(result)) 0L else nrow(result),
    uniqueGenes = length(genes),
    uniqueEnsembl = length(ensgs)
  )
}
