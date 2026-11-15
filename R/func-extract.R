# =============================================================================
# FLAME Extract Functions
# =============================================================================
#
# API wrapper functions for the EXTRACT API (Jensen Lab tagger).
# Used by the workspace Text Mining input method for extracting gene/protein
# names from text.
#
# API Documentation: https://extract.jensenlab.org/
#
# Dependencies:
# - httr package (for HTTP requests)
# - httpuv package (for URL encoding)
# - data.table package (for parsing TSV responses)
# - aaa-utilities.R (for isPOSTResponseValid)
#
# =============================================================================

# API base URL
EXTRACT_API_URL <- "https://tagger.jensenlab.org"
EXTRACT_TEXT_MAX_WORDS <- 100000L
EXTRACT_TEXT_MAX_CHARS <- 500000L

#' Extract gene/protein entities from text using EXTRACT API
#'
#' @param text Character. The text to analyze.
#' @param species Integer. NCBI taxonomy ID (e.g., 9606 for human).
#' @return Data frame with columns (Name, Type, ID) or NULL if no entities found.
#'
#' @examples
#' \dontrun{
#' result <- extract_entities("TP53 and BRCA1 are tumor suppressors", 9606)
#' }
extract_entities <- function(text, species) {
  tryCatch({
    encoded <- encode_for_extract_api(text, species)
    url <- paste0(EXTRACT_API_URL, "/GetEntities")
    params <- sprintf("document=%s&entity_types=%s&auto_detect=0&format=tsv",
                      encoded$text, encoded$entityTypes)

    request <- httr::POST(url, body = params)

    if (isPOSTResponseValid(request)) {
      result <- data.table::fread(
        sprintf("%s\n", rawToChar(httr::content(request, "raw"))),
        header = FALSE
      )
      names(result) <- c("Name", "Type", "ID")
      # Filter out the species type marker (-3)
      result <- result[result$Type != -3, ]

      if (nrow(result) == 0) {
        return(NULL)
      }
      return(result)
    } else {
      cat("[extract_entities] GetEntities request failed\n")
      return(NULL)
    }
  }, error = function(e) {
    cat(paste("[extract_entities] error:", conditionMessage(e), "\n"))
    return(NULL)
  })
}

#' Get HTML with entities annotated/highlighted using EXTRACT API
#'
#' @param text Character. The text to analyze.
#' @param species Integer. NCBI taxonomy ID (e.g., 9606 for human).
#' @return Character string of HTML or NULL on failure.
#'
#' @examples
#' \dontrun{
#' html <- extract_annotated_html("TP53 and BRCA1 are tumor suppressors", 9606)
#' }
extract_annotated_html <- function(text, species) {
  tryCatch({
    encoded <- encode_for_extract_api(text, species)
    url <- paste0(EXTRACT_API_URL, "/GetHTML")
    params <- sprintf("document=%s&entity_types=%s&auto_detect=0",
                      encoded$text, encoded$entityTypes)

    request <- httr::POST(url, body = params)

    if (isPOSTResponseValid(request)) {
      return(rawToChar(httr::content(request, "raw")))
    } else {
      cat("[extract_annotated_html] GetHTML request failed\n")
      return(NULL)
    }
  }, error = function(e) {
    cat(paste("[extract_annotated_html] error:", conditionMessage(e), "\n"))
    return(NULL)
  })
}

#' Encode text and build entity types string for EXTRACT API requests
#'
#' @param text Character. Raw text input.
#' @param species Integer. NCBI taxonomy ID.
#' @return List with 'text' (URL-encoded) and 'entityTypes' (formatted string).
encode_for_extract_api <- function(text, species) {
  # Clean up text (remove newlines/carriage returns)
  text <- gsub("\n", " ", text)
  text <- gsub("\r", "", text)

  # URL encode the text
  textEncoded <- httpuv::encodeURIComponent(text)

  # Build entity types string
  # -3 is the type code for genes/proteins
  entityTypes <- c(-3)
  entityTypesStr <- paste(entityTypes, sep = "+", collapse = "+")
  entityTypesStr <- sprintf("%s+%s", entityTypesStr, species)

  return(list(
    text = textEncoded,
    entityTypes = entityTypesStr
  ))
}

#' Validate text before submitting it to EXTRACT
#'
#' @param text Character scalar.
#' @return List with validation status and text stats.
validate_extract_text <- function(text) {
  text <- if (is.null(text) || length(text) == 0) "" else as.character(text[1])
  if (is.na(text)) text <- ""
  trimmed <- trimws(text)
  charCount <- nchar(text, type = "chars", allowNA = FALSE, keepNA = FALSE)
  words <- unlist(strsplit(trimmed, "\\s+", perl = TRUE), use.names = FALSE)
  words <- words[nzchar(words)]
  wordCount <- length(words)

  if (nchar(trimmed) < 3L) {
    return(list(
      ok = FALSE,
      message = "Enter at least 3 characters of text.",
      charCount = charCount,
      wordCount = wordCount
    ))
  }

  if (wordCount > EXTRACT_TEXT_MAX_WORDS || charCount > EXTRACT_TEXT_MAX_CHARS) {
    return(list(
      ok = FALSE,
      message = sprintf(
        "Text is too long. Maximum is %s words or %s characters.",
        format(EXTRACT_TEXT_MAX_WORDS, big.mark = ",", scientific = FALSE),
        format(EXTRACT_TEXT_MAX_CHARS, big.mark = ",", scientific = FALSE)
      ),
      charCount = charCount,
      wordCount = wordCount
    ))
  }

  list(
    ok = TRUE,
    message = "",
    charCount = charCount,
    wordCount = wordCount
  )
}

extract_db_link <- function(id) {
  id <- if (is.null(id) || length(id) == 0 || is.na(id[1])) "" else as.character(id[1])
  if (!nzchar(id)) return("")

  if (grepl("^ENS", id)) {
    return(sprintf("https://www.ensembl.org/id/%s", utils::URLencode(id, reserved = TRUE)))
  }

  if (grepl("^hsa-", id, ignore.case = TRUE)) {
    return(sprintf("https://www.mirbase.org/results/?query=%s", utils::URLencode(id, reserved = TRUE)))
  }

  ""
}

#' Format EXTRACT entity rows for workspace preview rendering
#'
#' @param result Data frame returned by extract_entities().
#' @return Data frame with display rows, links, ambiguity flags, and defaults.
format_extract_entity_rows <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(data.frame())
  }

  required <- c("Name", "Type", "ID")
  missing <- setdiff(required, colnames(result))
  if (length(missing) > 0) {
    stop(sprintf("EXTRACT result is missing required columns: %s", paste(missing, collapse = ", ")))
  }

  rows <- data.frame(
    name = as.character(result$Name),
    type = as.character(result$Type),
    id = as.character(result$ID),
    stringsAsFactors = FALSE
  )
  rows <- rows[!is.na(rows$id) & nzchar(trimws(rows$id)), , drop = FALSE]
  rows$name <- trimws(rows$name)
  rows$type <- trimws(rows$type)
  rows$id <- trimws(rows$id)
  rows <- unique(rows)

  if (nrow(rows) == 0) {
    return(data.frame())
  }

  groupKey <- paste(tolower(rows$name), rows$type, sep = "\r")
  idCountsByGroup <- tapply(rows$id, groupKey, function(ids) length(unique(ids)))
  rows$ambiguous <- unname(idCountsByGroup[groupKey] > 1L)
  hasAmbiguity <- any(rows$ambiguous)
  rows$selected <- if (hasAmbiguity) !rows$ambiguous else rep(TRUE, nrow(rows))
  rows$rowId <- paste0("extract-row-", seq_len(nrow(rows)))
  rows$idUrl <- vapply(rows$id, extract_db_link, character(1))
  rows <- rows[, c("rowId", "name", "type", "id", "idUrl", "ambiguous", "selected"), drop = FALSE]
  rownames(rows) <- NULL
  rows
}

#' Summarize formatted EXTRACT rows for client rendering
#'
#' @param rows Data frame returned by format_extract_entity_rows().
#' @param validation Validation list from validate_extract_text().
#' @param rawResult Optional raw EXTRACT result before row de-duplication.
#' @return Named list of counts.
summarize_extract_entity_rows <- function(rows, validation = NULL, rawResult = NULL) {
  rowCount <- if (is.null(rows)) 0L else nrow(rows)
  ids <- if (rowCount == 0L) character(0) else unique(rows$id[nzchar(rows$id)])
  rawRows <- if (is.null(rawResult)) rowCount else nrow(rawResult)

  list(
    uniqueIds = length(ids),
    rows = rowCount,
    duplicatesRemoved = max(0L, rawRows - rowCount),
    ambiguousRows = if (rowCount == 0L) 0L else sum(rows$ambiguous %in% TRUE),
    selectedRows = if (rowCount == 0L) 0L else sum(rows$selected %in% TRUE),
    charCount = if (!is.null(validation$charCount)) validation$charCount else NA_integer_,
    wordCount = if (!is.null(validation$wordCount)) validation$wordCount else NA_integer_
  )
}

#' Resolve selected EXTRACT IDs against the stored server-side preview rows
#'
#' @param rows Data frame returned by format_extract_entity_rows().
#' @param selectedRowIds Character vector of selected row IDs.
#' @return Unique selected EXTRACT IDs.
extract_textmining_selected_ids <- function(rows, selectedRowIds) {
  if (is.null(rows) || nrow(rows) == 0 || is.null(selectedRowIds) || length(selectedRowIds) == 0) {
    return(character(0))
  }

  selectedRowIds <- as.character(unlist(selectedRowIds, use.names = FALSE))
  selectedRowIds <- selectedRowIds[nzchar(trimws(selectedRowIds))]
  selected <- rows[rows$rowId %in% selectedRowIds, , drop = FALSE]
  ids <- selected$id
  ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
  unique(ids)
}
