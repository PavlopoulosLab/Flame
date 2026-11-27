# =============================================================================
# g:Profiler ORA Strategy
# =============================================================================
#
# Implements Over-Representation Analysis using the g:Profiler API (gprofiler2).
#
# Dependencies:
#   - config.R (for ToolId, ParadigmId, ORGANISMS, ENRICHMENT_DF_COLNAMES)
#   - enrich-strategy-base.R (for EnrichmentStrategy, strategyRegistry)
#   - gprofiler2 package
#
# =============================================================================

GProfilerORAStrategy <- R6::R6Class("GProfilerORAStrategy",

  inherit = EnrichmentStrategy,

  public = list(
    initialize = function() {
      super$initialize(ToolId$GPROFILER, ParadigmId$ORA)
    },

    run = function(inputList, organism, backgroundList, params) {
      # Filter datasources to only those supported by gProfiler
      toolDatasources <- getDatasourcesForTool(self$toolId)
      sources <- toolDatasources[toolDatasources %in% params$datasources]

      if (identical(sources, character(0))) {
        return(NULL)
      }

      # Determine background mode
      if (is.null(backgroundList)) {
        domain_scope <- "annotated"
        custom_bg <- NULL
        significant <- TRUE
      } else {
        domain_scope <- "custom"
        custom_bg <- backgroundList
        significant <- FALSE
      }

      # Get organism short name
      organismName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name

      # Call gProfiler API (with the hit genes of every term; see gprofiler_gost below)
      result <- gprofiler_gost(
        query = inputList,
        organism = organismName,
        significant = significant,
        user_threshold = as.numeric(params$threshold),
        correction_method = params$metric,
        sources = sources,
        domain_scope = domain_scope,
        custom_bg = custom_bg
      )

      # Check if valid
      if (is.null(result) || length(result) == 0) {
        return(NULL)
      }

      # Calculate background size
      backgroundSize <- if (is.null(backgroundList)) {
        private$getBackgroundSize(result)
      } else {
        length(backgroundList)
      }

      # Return structured result
      return(list(
        result = private$parseResult(result),
        backgroundSize = backgroundSize
      ))
    },

    # gProfiler accepts most ID formats directly
    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    }
  ),

  private = list(
    parseResult = function(result) {
      parsed <- result$result[, c(
        "source", "term_id", "term_name", "p_value", "term_size",
        "query_size", "intersection_size", "intersection"
      )]
      colnames(parsed) <- ENRICHMENT_DF_COLNAMES
      parsed <- private$mapGProfilerIds(parsed)
      return(parsed)
    },

    getBackgroundSize = function(result) {
      if (is.null(result) || length(result) == 0) {
        return(NULL)
      }
      metadata <- result$meta$result_metadata
      bsizes <- lapply(names(metadata), function(i) {
        return(metadata[[i]]$domain_size)
      })
      return(max(unlist(bsizes)))
    },

    # -------------------------------------------------------------------------
    # gProfiler-specific ID mapping helpers
    # -------------------------------------------------------------------------

    mapGProfilerIds = function(df) {
      df <- private$mapKEGGIds(df)
      df <- private$mapREACIds(df)
      df <- private$mapWPIds(df)
      return(df)
    },

    mapREACIds = function(df) {
      if (length(df$Source[which(df$Source == "REAC")]) > 0) {
        df[df$Source == "REAC", ]$Term_ID <-
          gsub("REAC:", "", df[df$Source == "REAC", ]$Term_ID)
      }
      return(df)
    },

    mapWPIds = function(df) {
      if (length(df$Source[which(df$Source == "WP")]) > 0) {
        df[df$Source == "WP", ]$Term_ID <-
          gsub("WP:", "", df[df$Source == "WP", ]$Term_ID)
      }
      return(df)
    }
  )
)

# Register the strategy
strategyRegistry$register(ToolId$GPROFILER, ParadigmId$ORA, GProfilerORAStrategy$new())

#' g:Profiler g:GOSt with each term's hit genes, as gprofiler2::gost(evcodes = TRUE) returns
#' them (result$result: source, term_id, term_name, p_value, term_size, query_size,
#' intersection_size, intersection; result$meta). The reply lists evidence codes for every
#' query gene of every term, and gost() simplifies all of that into data frames, which takes
#' minutes for lists of several thousand genes; reading it unsimplified takes seconds.
gprofiler_gost <- function(query, organism, sources, user_threshold = 0.05, correction_method = "g_SCS",
                           significant = TRUE, domain_scope = "annotated", custom_bg = NULL) {
  payload <- list(organism = jsonlite::unbox(organism), query = as.character(query), sources = as.character(sources),
                  user_threshold = jsonlite::unbox(user_threshold), all_results = jsonlite::unbox(!significant),
                  ordered = jsonlite::unbox(FALSE), no_evidences = jsonlite::unbox(FALSE), combined = jsonlite::unbox(FALSE),
                  measure_underrepresentation = jsonlite::unbox(FALSE), no_iea = jsonlite::unbox(FALSE),
                  domain_scope = jsonlite::unbox(domain_scope), numeric_ns = jsonlite::unbox(""),
                  significance_threshold_method = jsonlite::unbox(correction_method),
                  highlight = jsonlite::unbox(FALSE), output = jsonlite::unbox("json"))
  if (!is.null(custom_bg)) payload$background <- as.character(custom_bg)
  url <- paste0(sub("/+$", "", gprofiler2::get_base_url()), "/api/gost/profile/")
  resp <- httr::POST(url, body = jsonlite::toJSON(payload, auto_unbox = FALSE, null = "null"),
                     httr::content_type_json(), httr::accept_json(), httr::timeout(600))
  if (httr::status_code(resp) != 200) stop("g:Profiler returned an error (HTTP ", httr::status_code(resp), ").")
  res <- jsonlite::parse_json(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  terms <- res$result
  if (!length(terms)) return(NULL)
  # Hit genes: the query's Ensembl IDs with evidence, named back to the input ID that mapped to them.
  qmeta <- res$meta$genes_metadata$query[[1]]
  ensgs <- vapply(qmeta$ensgs, as.character, "")
  map <- qmeta$mapping
  backName <- stats::setNames(rep(names(map), lengths(map)), as.character(unlist(map)))
  backName <- backName[!duplicated(names(backName))]
  hitsOf <- function(ev) {
    g <- ensgs[lengths(ev) > 0]
    nm <- unname(backName[g]); nm[is.na(nm)] <- g[is.na(nm)]
    paste(nm, collapse = ",")
  }
  num <- function(k) vapply(terms, function(t) as.numeric(t[[k]] %||% NA), 0)
  chr <- function(k) vapply(terms, function(t) as.character(t[[k]] %||% NA), "")
  df <- data.frame(source = chr("source"), term_id = chr("native"), term_name = chr("name"), p_value = num("p_value"),
                   term_size = num("term_size"), query_size = num("query_size"), intersection_size = num("intersection_size"),
                   significant = vapply(terms, function(t) isTRUE(t$significant), TRUE),
                   intersection = vapply(terms, function(t) hitsOf(t$intersections), ""), stringsAsFactors = FALSE)
  df <- df[order(df$source, df$p_value), , drop = FALSE]
  rownames(df) <- NULL
  list(result = df, meta = res$meta)
}
