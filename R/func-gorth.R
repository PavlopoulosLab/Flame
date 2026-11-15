# =============================================================================
# FLAME g:Orth Functions
# =============================================================================
#
# API wrapper function for the g:Orth API (gprofiler2 package).
# Used by OrthologySession for finding orthologous genes across species.
#
# API Documentation: https://biit.cs.ut.ee/gprofiler/orth
#
# Dependencies:
# - gprofiler2 package
#
# =============================================================================

#' Find orthologous genes using g:Orth
#'
#' Wraps gprofiler2::gorth() to find orthologous genes between different
#' organisms.
#'
#' @param ids Character vector. Gene/protein identifiers to find orthologs for.
#' @param source_organism Character. Source organism short name (e.g., "hsapiens").
#' @param target_organism Character. Target organism short name (e.g., "mmusculus").
#' @param numeric_ns Character. Namespace for numeric query IDs.
#' @param mthreshold Numeric. Maximum mappings per input. Use Inf for all mappings.
#' @param filter_na Logical. Whether to filter out NA results. Default TRUE.
#' @return Data frame with orthology results, or NULL on error.
#'
#' @examples
#' \dontrun{
#' result <- gorth_ids(c("TP53", "BRCA1"), "hsapiens", "mmusculus")
#' }
gorth_ids <- function(ids, source_organism, target_organism, numeric_ns = "",
                      mthreshold = Inf, filter_na = TRUE) {
  tryCatch({
    if (length(ids) == 0) {
      cat("[gorth_ids] No IDs provided\n")
      return(NULL)
    }

    result <- gprofiler2::gorth(
      ids,
      source_organism = source_organism,
      target_organism = target_organism,
      numeric_ns = numeric_ns,
      mthreshold = mthreshold,
      filter_na = filter_na
    )

    if (is.null(result) || nrow(result) == 0) {
      cat("[gorth_ids] No results returned from gorth\n")
      return(NULL)
    }

    return(result)
  }, error = function(e) {
    cat(paste("[gorth_ids] error:", conditionMessage(e), "\n"))
    return(NULL)
  })
}
