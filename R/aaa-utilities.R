# =============================================================================
# SHARED UTILITY FUNCTIONS
# =============================================================================
# Functions used across the codebase: link parsing, search queries, warnings
# and HTTP response checks.
#
# NOTE: This file is named aaa-* to ensure it loads early in R's alphabetical
# loading order, as these utilities are used by many other files.
# =============================================================================

# --- HTML Parsing -----------------------------------------------------------

#' Extract href URL from an HTML anchor tag
#'
#' Parses an HTML string containing an <a> tag and extracts the href attribute
#' value. Used by enrichment result tables where the "Term ID" column contains
#' HTML anchor tags linking to external datasource pages.
#'
#' @param value Character. HTML string potentially containing an anchor tag.
#' @return Character string with the URL, or NULL if no valid href found.
extractHref <- function(value) {
  x <- as.character(value %||% "")
  if (!nzchar(x)) return(NULL)
  m <- regexec("href=['\"]([^'\"]+)['\"]", x, perl = TRUE)
  parts <- regmatches(x, m)[[1]]
  if (length(parts) < 2) return(NULL)
  href <- trimws(parts[[2]])
  if (!nzchar(href)) return(NULL)
  if (!grepl("^https?://", href, ignore.case = TRUE)) return(NULL)
  href
}

#' Make an ID safe to place inside a quoted href (quotes, brackets, spaces,
#' backslashes are percent-encoded; ':' '/' etc. are kept so database URLs work).
flameUrlPart <- function(value) {
  x <- as.character(value)
  for (ch in c("%", "'", "\"", "<", ">", " ", "\\", "`")) {
    x <- gsub(ch, sprintf("%%%02X", utf8ToInt(ch)), x, fixed = TRUE)
  }
  x
}

# --- Search Query Filtering --------------------------------------------------

#' Filter rows by a literal search query
#'
#' Supports a small search-box grammar:
#' - whitespace or AND: required terms
#' - quoted text: exact phrase
#' - OR: alternate groups
#' - -term or NOT term: excluded terms
#'
#' Matching is case-insensitive and literal, not regex-based.
#'
#' @param data Data frame to filter.
#' @param query Search query string.
#' @param fields Character vector of fields to search.
#' @return Filtered data frame.
filterRowsBySearchQuery <- function(data, query, fields) {
  if (is.null(data) || nrow(data) == 0) return(data)
  if (is.null(query) || !nzchar(trimws(as.character(query)))) return(data)

  searchFields <- intersect(fields, names(data))
  if (length(searchFields) == 0) return(data)

  groups <- parseSearchQuery(query)
  if (length(groups) == 0) return(data)

  fieldValues <- lapply(searchFields, function(field) {
    values <- as.character(data[[field]])
    values[is.na(values)] <- ""
    values
  })

  rowText <- Reduce(function(left, right) paste(left, right, sep = " "), fieldValues)
  rowText <- tolower(rowText)

  keep <- rep(FALSE, length(rowText))
  for (group in groups) {
    includeTerms <- group$include
    excludeTerms <- group$exclude

    groupKeep <- rep(TRUE, length(rowText))
    for (term in includeTerms) {
      groupKeep <- groupKeep & grepl(term, rowText, fixed = TRUE)
    }
    for (term in excludeTerms) {
      groupKeep <- groupKeep & !grepl(term, rowText, fixed = TRUE)
    }

    keep <- keep | groupKeep
  }

  data[keep, , drop = FALSE]
}

parseSearchQuery <- function(query) {
  tokens <- tokenizeSearchQuery(query)
  groups <- list()
  current <- list(include = character(0), exclude = character(0))
  pendingExclude <- FALSE

  hasTerms <- function(group) {
    length(group$include) > 0 || length(group$exclude) > 0
  }

  addGroup <- function(group) {
    if (hasTerms(group)) {
      groups[[length(groups) + 1]] <<- group
    }
  }

  for (token in tokens) {
    value <- token$value
    if (!nzchar(value)) next

    upperValue <- toupper(value)
    if (!isTRUE(token$quoted) && upperValue == "OR") {
      addGroup(current)
      current <- list(include = character(0), exclude = character(0))
      pendingExclude <- FALSE
      next
    }

    if (!isTRUE(token$quoted) && upperValue == "AND") {
      next
    }

    if (!isTRUE(token$quoted) && upperValue == "NOT") {
      pendingExclude <- TRUE
      next
    }

    exclude <- pendingExclude
    pendingExclude <- FALSE

    if (startsWith(value, "-") && nchar(value) > 1) {
      exclude <- TRUE
      value <- substring(value, 2)
    }

    value <- tolower(trimws(value))
    if (!nzchar(value)) next

    if (exclude) {
      current$exclude <- c(current$exclude, value)
    } else {
      current$include <- c(current$include, value)
    }
  }

  addGroup(current)
  groups
}

tokenizeSearchQuery <- function(query) {
  text <- trimws(as.character(query %||% ""))
  if (!nzchar(text)) return(list())

  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  tokens <- list()
  i <- 1
  n <- length(chars)

  addToken <- function(value, quoted = FALSE) {
    value <- trimws(value)
    if (nzchar(value)) {
      tokens[[length(tokens) + 1]] <<- list(value = value, quoted = quoted)
    }
  }

  while (i <= n) {
    while (i <= n && grepl("\\s", chars[[i]])) {
      i <- i + 1
    }
    if (i > n) break

    tokenPrefix <- ""
    if (chars[[i]] == "-" && i < n && chars[[i + 1]] == '"') {
      tokenPrefix <- "-"
      i <- i + 1
    }

    if (chars[[i]] == '"') {
      i <- i + 1
      start <- i
      while (i <= n && chars[[i]] != '"') {
        i <- i + 1
      }
      value <- if (i > start) paste(chars[start:(i - 1)], collapse = "") else ""
      addToken(paste0(tokenPrefix, value), quoted = TRUE)
      if (i <= n && chars[[i]] == '"') {
        i <- i + 1
      }
    } else {
      start <- i
      while (i <= n && !grepl("\\s", chars[[i]])) {
        i <- i + 1
      }
      value <- paste(chars[start:(i - 1)], collapse = "")
      addToken(value, quoted = FALSE)
    }
  }

  tokens
}

# --- Warnings ----------------------------------------------------------------
# Strategies report recoverable problems through these helpers; they raise R
# warnings that execute_ora_run() collects and returns to the client.

renderError <- function(prompt) {
  warning(trimws(gsub("\\s+", " ", prompt)), call. = FALSE)
}

renderWarning <- function(prompt) {
  warning(trimws(gsub("\\s+", " ", prompt)), call. = FALSE)
}

# --- API Utilities -----------------------------------------------------------

#' Check if a POST response is valid
#'
#' Validates HTTP response from external APIs. Returns FALSE and shows warning
#' if status code is not 200 or if content is empty.
#'
#' @param request httr response object
#' @return TRUE if response is valid, FALSE otherwise
isPOSTResponseValid <- function(request) {
  isValid <- TRUE
  if (request$status_code != 200) {
    isValid <- FALSE
    renderWarning("Invalid response from the called API.
                  Please try again in a while.")
  } else if (identical(request$content, raw(0))) {
    isValid <- FALSE
  }
  return(isValid)
}
