# =============================================================================
# FLAME AnalyteList Base Class
# =============================================================================
#
# Abstract base class for all analyte list types. An AnalyteList represents a
# named collection of biological identifiers (genes, metabolites, etc.) that
# can be used as input for enrichment analysis.
#
# Dependencies:
# - config.R (for AnalyteType)
#
# Subclasses (in separate files):
# - input-analytelist-unranked.R (UnrankedAnalyteList)
# - input-analytelist-ranked.R (RankedAnalyteList)
#
# Registry (in separate file):
# - input-analytelist-registry.R (AnalyteListRegistry)
#
# =============================================================================

#' AnalyteList Base Class
#'
#' Abstract base class for all analyte list types. An AnalyteList represents a
#' named collection of biological identifiers (genes, metabolites, etc.) that
#' can be used as input for enrichment analysis.
#'
#' @field name Character. Display name for this analyte list.
#' @field analyteType Character. Type from AnalyteType enum.
#' @field organismTaxId Integer. NCBI taxid associated with this list.
#' @field organismName Character. Display name resolved from taxid at creation.
#' @field idFormat Character. Identifier namespace/format selected at creation.
#' @field createdAt POSIXct. Timestamp when list was created.
#'
AnalyteList <- R6::R6Class(
  "AnalyteList",
  public = list(
    #' @field name Display name for this analyte list
    name = NULL,

    #' @field analyteType Type of analytes in this list (from AnalyteType)
    analyteType = NULL,

    #' @field organismTaxId Organism taxid selected for this list
    organismTaxId = NULL,

    #' @field idFormat Identifier format selected for this list
    idFormat = NULL,

    #' @field organismName Display name for the organism (resolved from taxid at creation)
    organismName = NULL,

    #' @field createdAt Timestamp when list was created
    createdAt = NULL,

    #' Initialize an AnalyteList
    #' @param name Character. Display name.
    #' @param analyteType Character. Type from AnalyteType.
    #' @param organismTaxId Integer. Optional organism taxid for this list.
    #' @param idFormat Character. Optional selected identifier format.
    initialize = function(name, analyteType, organismTaxId = NULL, idFormat = NULL) {
      self$name <- name
      self$analyteType <- analyteType
      self$organismTaxId <- organismTaxId
      self$idFormat <- idFormat
      self$createdAt <- Sys.time()
      if (!is.null(organismTaxId) && length(organismTaxId) > 0 && !is.na(organismTaxId)) {
        orgRow <- ORGANISMS[ORGANISMS$taxid == organismTaxId, , drop = FALSE]
        if (nrow(orgRow) == 0) {
          self$organismName <- as.character(organismTaxId)
        } else {
          printName <- as.character(orgRow$print_name[1])
          m <- regexpr("\\([^)]*\\)", printName)
          if (!is.na(m) && m[1] > 0) {
            label <- gsub("^\\(|\\)$", "", regmatches(printName, m))
            if (nchar(label) > 0) {
              self$organismName <- label
            } else {
              self$organismName <- printName
            }
          } else {
            shortName <- as.character(orgRow$short_name[1])
            if (!is.na(shortName) && nchar(shortName) > 0) {
              self$organismName <- shortName
            } else {
              self$organismName <- printName
            }
          }
        }
      }
    },

    #' Get all identifiers in this list
    #' @return Character vector of identifiers
    getIds = function() {
      stop("Abstract method: must be implemented by subclass")
    },

    #' Get the number of identifiers
    #' @return Integer count
    size = function() {
      length(self$getIds())
    },

    #' Create a printable representation
    #' @return Character string
    print = function() {
      cat(sprintf("<%s> '%s' (%d items)\n",
                  class(self)[1], self$name, self$size()))
      invisible(self)
    }
  ),

  private = list(
    # Maximum identifiers allowed per analyte list
    .maxIds = 3000
  )
)
