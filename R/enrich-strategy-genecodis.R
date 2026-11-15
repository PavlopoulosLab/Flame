# =============================================================================
# GeneCodis ORA Strategy
# =============================================================================
#
# Implements Over-Representation Analysis using the GeneCodis API.
#
# Dependencies:
#   - config.R (for ToolId, ParadigmId, ORGANISMS, TOOLS, ENRICHMENT_DF_COLNAMES)
#   - enrich-strategy-base.R (for EnrichmentStrategy, strategyRegistry)
#   - aaa-utilities.R (for renderWarning)
#   - httr package
#   - jsonlite package
#
# =============================================================================

GeneCodisORAStrategy <- R6::R6Class("GeneCodisORAStrategy",

  inherit = EnrichmentStrategy,

  public = list(
    initialize = function() {
      super$initialize(ToolId$GENECODIS, ParadigmId$ORA)
    },

    run = function(inputList, organism, backgroundList, params) {
      # Get organism short name for datasource lookup
      shortName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name

      # Map datasources to GeneCodis annotations
      genecodisAnnotations <- private$mapDatasourcesToAnnotations(params$datasources, shortName)

      if (length(genecodisAnnotations) == 0) {
        renderWarning("No valid GeneCodis annotations for selected data sources.")
        return(NULL)
      }

      # Send job request
      jobInfo <- private$sendRequest(inputList, organism, backgroundList, genecodisAnnotations)

      if (is.null(jobInfo$job_id)) {
        return(NULL)
      }

      # Poll for completion
      jobReady <- private$pollGeneCodisJob(jobInfo$job_id, length(inputList))

      if (!jobReady) {
        renderWarning("GeneCodis job timed out.")
        return(NULL)
      }

      # Retrieve results
      genecodisResult <- private$retrieveGeneCodisResults(
        jobInfo$job_id, jobInfo$job_name, jobInfo$annotations
      )

      if (!private$isGeneCodisResponseValid(genecodisResult)) {
        return(NULL)
      }

      # Parse results
      result <- private$parseGeneCodisResult(genecodisResult, params$threshold)

      if (!private$isGeneCodisResultValid(result)) {
        return(NULL)
      }

      # Filter by datasources
      if (!is.null(params$datasources) && length(params$datasources) > 0) {
        filteredResult <- result[result$Source %in% params$datasources, ]
        if (nrow(filteredResult) == 0 && nrow(result) > 0) {
          renderWarning(paste0(
            "Data source filtering removed all results. ",
            "Results had sources: ", paste(unique(result$Source), collapse = ", "),
            " but you selected: ", paste(params$datasources, collapse = ", ")
          ))
        }
        result <- filteredResult
      }

      if (nrow(result) == 0) {
        return(NULL)
      }

      # Return structured result
      return(list(
        result = result,
        # TODO(background-size): Without custom background, returns NULL.
        # Two consequences:
        #   1. Results parameters box: shows "Genome-wide (tool default)"
        #      instead of the actual number (provenance gap).
        #   2. Comparison bubble/heatmap: FoldEnrichment = NA for all
        #      GeneCodis terms (FE formula needs bgSize).
        # The GeneCodis API does not return total background size
        # directly. Note: GeneCodis uses scope="annotated", so its
        # background may be annotated genes, not the full genome.
        # It may be possible to infer it indirectly from other values
        # the API returns that are computed using the background size.
        # See also: identical TODOs in STRING, PANTHER, Enrichr.
        backgroundSize = private$getSimpleBackgroundSize(backgroundList)
      ))
    },

    # GeneCodis accepts multiple ID formats
    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    }
  ),

  private = list(
    mapDatasourcesToAnnotations = function(selectedDatasources, shortName) {
      toolConfig <- TOOLS[[self$toolId]]

      # Get organism-specific datasources
      organismDatasources <- toolConfig$organismDatasources[[shortName]]
      if (is.null(organismDatasources)) {
        organismDatasources <- toolConfig$organismDatasources[["hsapiens"]]
      }

      # Filter selected datasources to only those GeneCodis supports for this organism
      selectedDatasources <- selectedDatasources[selectedDatasources %in% organismDatasources]

      # Map to API codes using tool config
      datasourceCodes <- toolConfig$datasourceCodes
      genecodisAnnotations <- c()

      for (source in selectedDatasources) {
        if (source %in% names(datasourceCodes)) {
          genecodisAnnotations <- c(genecodisAnnotations, datasourceCodes[[source]])
        }
      }

      return(genecodisAnnotations)
    },

    sendRequest = function(inputList, taxid, backgroundList, annotations) {
      url <- "https://genecodis.genyo.es/gc4/analysis"
      jobName <- paste0("FLAME_", format(Sys.time(), "%Y%m%d_%H%M%S"))

      body <- list(
        organism = as.integer(taxid),
        inputtype = "genes",
        input = list(input = inputList),
        annotations = I(annotations),
        stat = "hypergeom",
        scope = "annotated",
        coannotation = "no",
        inputmode = "on",
        universe = if (is.null(backgroundList)) list() else backgroundList,
        email = "",
        jobName = jobName,
        algorithm = "fpgrowth",
        inputSupport = 0,
        inputNames = list(input1unique = jobName),
        gc4uid = ""
      )

      json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)

      tryCatch({
        response <- httr::POST(url,
          body = json_body,
          encode = "raw",
          httr::content_type("application/json"),
          httr::timeout(30)
        )

        if (response$status_code == 200) {
          response_text <- rawToChar(httr::content(response, "raw"))

          if (grepl("error:", response_text, ignore.case = TRUE)) {
            renderWarning(paste("GeneCodis API error:", response_text))
            return(list(job_id = NULL, job_name = NULL, annotations = NULL))
          }

          if (grepl("jobID:", response_text)) {
            job_id <- gsub(".*jobID:\\s*", "", response_text)
            job_id <- trimws(job_id)
            return(list(job_id = job_id, job_name = jobName, annotations = annotations))
          }
        }

        return(list(job_id = NULL, job_name = NULL, annotations = NULL))
      }, error = function(e) {
        renderWarning(paste("Connection to GeneCodis failed:", e$message))
        return(list(job_id = NULL, job_name = NULL, annotations = NULL))
      })
    },

    # -------------------------------------------------------------------------
    # GeneCodis-specific helper methods
    # -------------------------------------------------------------------------

    pollGeneCodisJob = function(job_id, gene_count, max_timeout = 60) {
      initial_wait <- if (gene_count < 20) {
        8
      } else if (gene_count < 100) {
        12
      } else {
        18
      }

      Sys.sleep(initial_wait)

      qc_url <- sprintf("https://genecodis.genyo.es/gc4/qc?job=%s", job_id)
      start_time <- Sys.time()
      poll_interval <- 2

      while (difftime(Sys.time(), start_time, units = "secs") < max_timeout) {
        tryCatch({
          qc_response <- httr::GET(qc_url, httr::timeout(10))

          if (qc_response$status_code == 200) {
            return(TRUE)
          } else if (qc_response$status_code >= 400) {
            stop("GeneCodis encountered an error processing your request. Please verify your gene list contains valid identifiers.")
          }

          Sys.sleep(poll_interval)

        }, error = function(e) {
          if (grepl("Please verify your gene list", e$message)) {
            stop(e$message)
          } else {
            Sys.sleep(poll_interval)
          }
        })
      }

      return(FALSE)
    },

    retrieveGeneCodisResults = function(job_id, job_name, annotations) {
      allResults <- list()

      for (annotation in annotations) {
        annotation_key <- paste0(job_name, "-", annotation)
        url <- sprintf("https://genecodis.genyo.es/gc4/results?job=%s&annotation=%s",
                       job_id, annotation_key)

        tryCatch({
          response <- httr::GET(url, httr::timeout(30))

          if (response$status_code == 200) {
            allResults[[annotation]] <- response
          } else {
            allResults[[annotation]] <- NULL
          }
        }, error = function(e) {
          renderWarning(paste("Failed to retrieve GeneCodis results for", annotation, ":", e$message))
          allResults[[annotation]] <- NULL
        })
      }

      return(allResults)
    },

    isGeneCodisResponseValid = function(responseList) {
      valid_responses <- sum(sapply(responseList, function(r) !is.null(r)))

      if (valid_responses == 0) {
        renderWarning("No valid results returned from GeneCodis. Try adjusting your gene list or selected data sources.")
        return(FALSE)
      }

      return(TRUE)
    },

    parseGeneCodisResult = function(responseList, threshold = NULL) {
      allResults <- data.frame()

      for (annotation in names(responseList)) {
        response <- responseList[[annotation]]

        if (is.null(response)) {
          next
        }

        responseBody <- rawToChar(httr::content(response, "raw"))

        if (nchar(responseBody) == 0) {
          next
        }

        if (grepl("error:", responseBody, ignore.case = TRUE)) {
          next
        }

        tryCatch({
          results <- read.delim(text = responseBody, header = TRUE,
                               stringsAsFactors = FALSE, sep = "\t")

          if (nrow(results) == 0) {
            next
          }

          results$flame_source <- private$mapGeneCodisToFlameSource(annotation)

          threshold <- as.numeric(threshold)

          if ("pval_adj" %in% colnames(results)) {
            significant_mask <- results$pval_adj < threshold
            results <- results[significant_mask, ]
          }

          if (nrow(results) == 0) {
            next
          }

          genecodisSelected <- data.frame(
            Source = trimws(results$flame_source),
            Term_ID = results$annotation_id,
            Function = results$description,
            Pvalue = results$pval_adj,
            Term_Size = results$term_genes,
            Query_size = results$input_size,
            Intersection_Size = results$genes_found,
            Positive_Hits = results$genes,
            stringsAsFactors = FALSE
          )

          colnames(genecodisSelected) <- ENRICHMENT_DF_COLNAMES

          allResults <- rbind(allResults, genecodisSelected)

        }, error = function(e) {
          renderWarning(paste("Failed to parse GeneCodis response for", annotation, ":", e$message))
        })
      }

      return(allResults)
    },

    isGeneCodisResultValid = function(parsedResult) {
      if (is.null(parsedResult)) {
        renderWarning("No significant enrichment results found in GeneCodis analysis")
        return(FALSE)
      }

      if (!is.data.frame(parsedResult)) {
        renderWarning("No significant enrichment results found in GeneCodis analysis")
        return(FALSE)
      }

      if (nrow(parsedResult) == 0) {
        renderWarning("No significant enrichment results found in GeneCodis analysis")
        return(FALSE)
      }

      return(TRUE)
    },

    mapGeneCodisToFlameSource = function(annotation) {
      mapping <- list(
        "GO_MF" = "GO:MF",
        "GO_BP" = "GO:BP",
        "GO_CC" = "GO:CC",
        "KEGG" = "KEGG",
        "Reactome" = "REAC",
        "WikiPathways" = "WP",
        "Panther" = "PANTHER Pathways",
        "HPO" = "HP",
        "OMIM" = "OMIM",
        "MGI" = "MGI",
        "BioPlanet" = "BioPlanet",
        "PharmGKB" = "PharmGKB",
        "LINCS" = "LINCS",
        "CollecTRI" = "CollecTRI",
        "miRTarBase" = "MIRNA"
      )

      if (annotation %in% names(mapping)) {
        return(mapping[[annotation]])
      }

      return(annotation)
    }
  )
)

# Register the strategy
strategyRegistry$register(ToolId$GENECODIS, ParadigmId$ORA, GeneCodisORAStrategy$new())
