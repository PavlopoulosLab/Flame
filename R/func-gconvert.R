# =============================================================================
# FLAME g:Convert Functions
# =============================================================================
#
# API wrapper function for the g:Convert API (gprofiler2 package).
# Used by ConversionSession for converting gene/protein identifiers.
#
# API Documentation: https://biit.cs.ut.ee/gprofiler/convert
#
# Dependencies:
# - gprofiler2 package
#
# =============================================================================

#' Convert gene/protein identifiers using g:Convert
#'
#' Wraps gprofiler2::gconvert() to convert identifiers between different
#' namespaces (e.g., ENSEMBL to gene symbols).
#'
#' @param ids Character vector. Gene/protein identifiers to convert.
#' @param organism Character. Organism short name (e.g., "hsapiens", "mmusculus").
#' @param target_namespace Character. Target namespace (e.g., "ENTREZGENE_ACC", "ENSG").
#' @param mthreshold Numeric. Maximum mappings per input. Use Inf for all mappings.
#' @param filter_na Logical. Whether to filter out NA results. Default TRUE.
#' @return Data frame with conversion results, or NULL on error.
#'
#' @examples
#' \dontrun{
#' result <- gconvert_ids(c("TP53", "BRCA1"), "hsapiens", "ENTREZGENE_ACC")
#' }
gconvert_ids <- function(ids, organism, target_namespace, mthreshold = Inf, filter_na = TRUE) {
  tryCatch({
    if (length(ids) == 0) {
      cat("[gconvert_ids] No IDs provided\n")
      return(NULL)
    }

    result <- gprofiler2::gconvert(
      ids,
      organism = organism,
      target = target_namespace,
      mthreshold = mthreshold,
      filter_na = filter_na
    )

    if (is.null(result) || nrow(result) == 0) {
      cat("[gconvert_ids] No results returned from gconvert\n")
      return(NULL)
    }

    return(result)
  }, error = function(e) {
    cat(paste("[gconvert_ids] error:", conditionMessage(e), "\n"))
    return(NULL)
  })
}
