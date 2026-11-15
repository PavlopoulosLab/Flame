# Package initialization and path helpers
# This file is loaded first due to its name (zzz.R sorts last alphabetically,
# but we need it loaded first for path helpers)

# Package imports (processed by roxygen2 into NAMESPACE)
#' @importFrom R6 R6Class
#' @importFrom httr GET POST content status_code
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom enrichR setEnrichrSite enrichr
#' @importFrom gprofiler2 gost gconvert gorth
#' @importFrom stringr str_extract str_replace str_detect str_split str_trim
#' @importFrom plyr rbind.fill
#' @importFrom poolr fisher
#' @importFrom curl curl
#' @importFrom stats setNames
#' @importFrom utils head tail read.csv write.csv
#' @import dplyr
#' @importFrom data.table fread setDT
#' @importFrom tools file_ext
NULL

# Package root path - computed once at load time
.pkgRoot <- NULL

#' Get the package root directory
#'
#' Returns the root directory of the flame package.
#' Works both during development and after installation.
#'
#' @return Character string with the path to package root
#' @keywords internal
getFlameRoot <- function() {
  if (is.null(.pkgRoot)) {
    # Try to find package installation
    pkgPath <- system.file(package = "flame")
    if (pkgPath != "") {
      # Installed package
      .pkgRoot <<- pkgPath
    } else {
      # Development mode - try to find from current working directory
      # Check if we're in inst/app/
      if (file.exists("../../DESCRIPTION")) {
        .pkgRoot <<- normalizePath("../..")
      } else if (file.exists("DESCRIPTION")) {
        .pkgRoot <<- getwd()
      } else {
        # Fall back to trying to find package
        .pkgRoot <<- find.package("flame", quiet = TRUE)
        if (length(.pkgRoot) == 0) {
          .pkgRoot <<- getwd()
        }
      }
    }
  }
  return(.pkgRoot)
}

#' Get path to a data file in extdata
#'
#' @param ... Path components relative to extdata/
#' @return Full path to the file
#' @keywords internal
getExtdataPath <- function(...) {
  root <- getFlameRoot()

  # Check if running from installed package
  instPath <- file.path(root, "extdata", ...)
  if (file.exists(instPath)) {
    return(instPath)
  }

  # Check inst/extdata/ (development mode)
  devPath <- file.path(root, "inst", "extdata", ...)
  if (file.exists(devPath)) {
    return(devPath)
  }

  # Fall back to original locations (legacy support)
  legacyPath <- file.path(root, ...)
  if (file.exists(legacyPath)) {
    return(legacyPath)
  }

  # Return the expected installed path even if not found
  return(instPath)
}

#' Get path to organisms data
#'
#' @param filename The organism data filename
#' @return Full path to the file
#' @keywords internal
getOrganismsPath <- function(filename) {
  getExtdataPath("organisms", filename)
}

#' Get path to examples data
#'
#' @param filename The example data filename
#' @return Full path to the file
#' @keywords internal
getExamplesPath <- function(filename) {
  getExtdataPath("examples", filename)
}

.onLoad <- function(libname, pkgname) {
  # Initialize package root on load
  getFlameRoot()
}
