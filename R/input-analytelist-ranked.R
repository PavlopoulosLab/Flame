# =============================================================================
# FLAME Ranked AnalyteList Class
# =============================================================================
#
# Represents a list of identifiers with associated scores and ranks.
# Used for GSEA and other ranked enrichment methods.
#
# Note: This is a placeholder implementation.
# Full GSEA support will be added when WebGestalt GSEA is integrated.
#
# Dependencies:
# - input-analytelist.R (for AnalyteList base class)
# - config.R (for ScoreDirection)
#
# =============================================================================

#' Ranked AnalyteList Class
#'
#' Represents a list of identifiers with associated scores and ranks.
#' Used for GSEA and other ranked enrichment methods.
#'
#' Note: This is a placeholder implementation.
#' Full GSEA support will be added when WebGestalt GSEA is integrated.
#'
#' @examples
#' \dontrun{
#' genes <- RankedAnalyteList$new(
#'   "DE Genes",
#'   AnalyteType$GENE,
#'   ids = c("TP53", "BRCA1", "EGFR"),
#'   scores = c(2.5, -1.8, 0.5),
#'   direction = ScoreDirection$DESCENDING
#' )
#' genes$getIds()     # c("TP53", "BRCA1", "EGFR")
#' genes$getScores()  # c(2.5, -1.8, 0.5)
#' genes$getRanks()   # c(1, 3, 2) - auto-computed from scores
#' }
#'
RankedAnalyteList <- R6::R6Class(
  "RankedAnalyteList",
  inherit = AnalyteList,

  public = list(
    #' @field direction Score direction from ScoreDirection enum
    direction = NULL,

    #' Initialize a RankedAnalyteList
    #' @param name Character. Display name.
    #' @param analyteType Character. Type from AnalyteType.
    #' @param ids Character vector. The identifiers.
    #' @param scores Numeric vector. Scores for each identifier.
    #' @param direction Character. From ScoreDirection enum.
    #' @param organismTaxId Integer. Optional organism taxid for this list.
    #' @param idFormat Character. Optional selected identifier format.
    #' @note Throws error if ids exceeds maximum allowed.
    initialize = function(name, analyteType, ids, scores, direction, organismTaxId = NULL, idFormat = NULL) {
      super$initialize(name, analyteType, organismTaxId = organismTaxId, idFormat = idFormat)

      if (length(ids) != length(scores)) {
        stop("ids and scores must have the same length")
      }

      if (length(ids) > private$.maxIds) {
        stop(sprintf("Too many items (%d). Maximum allowed is %d.",
                     length(ids), private$.maxIds))
      }

      private$.ids <- as.character(ids)
      private$.scores <- as.numeric(scores)
      self$direction <- direction

      private$computeRanks()
    },

    #' Get all identifiers
    #' @return Character vector of identifiers
    getIds = function() {
      private$.ids
    },

    #' Get scores for all identifiers
    #' @return Numeric vector of scores
    getScores = function() {
      private$.scores
    },

    #' Get ranks for all identifiers
    #' @return Integer vector of ranks
    getRanks = function() {
      private$.ranks
    },

    #' Convert to data frame for display and export
    #'
    #' Used by AnalyteListManagerSession to render list contents in a DataTable
    #' and enable export to CSV/Excel/PDF.
    #'
    #' @return Data frame with ID, score, and rank columns
    toDataFrame = function() {
      data.frame(
        id = self$getIds(),
        score = self$getScores(),
        rank = self$getRanks(),
        stringsAsFactors = FALSE
      )
    }
  ),

  private = list(
    .ids = NULL,
    .scores = NULL,
    .ranks = NULL,

    computeRanks = function() {
      if (self$direction == "DESCENDING") {
        private$.ranks <- rank(-private$.scores, ties.method = "first")
      } else {
        private$.ranks <- rank(private$.scores, ties.method = "first")
      }
    }
  )
)
