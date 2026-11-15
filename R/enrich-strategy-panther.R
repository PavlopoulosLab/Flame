# =============================================================================
# PANTHER ORA Strategy
# =============================================================================
#
# Implements Over-Representation Analysis using the PANTHER API.
#
# Dependencies:
#   - config.R (for ToolId, ParadigmId, ENRICHMENT_DF_COLNAMES)
#   - enrich-strategy-base.R (for EnrichmentStrategy, strategyRegistry)
#   - aaa-utilities.R (for renderWarning)
#   - httr package
#   - jsonlite package
#
# =============================================================================

PANTHERORAStrategy <- R6::R6Class("PANTHERORAStrategy",

  inherit = EnrichmentStrategy,

  public = list(
    initialize = function() {
      super$initialize(ToolId$PANTHER, ParadigmId$ORA)
    },

    run = function(inputList, organism, backgroundList, params) {
      # Map datasources to PANTHER dataset IDs
      pantherDatasets <- private$mapDataSourcesToPantherDatasets(params$datasources)

      # Map metric to PANTHER correction parameter
      correction <- switch(params$metric,
        "False discovery rate" = "FDR",
        "FDR" = "FDR",
        "P-value" = "NONE",
        "NONE" = "NONE",
        "Bonferroni" = "BONFERRONI",
        "BONFERRONI" = "BONFERRONI",
        "FDR"  # default
      )

      # Send requests for each dataset
      allResponses <- private$sendRequests(
        inputList, organism, backgroundList, pantherDatasets, correction
      )

      if (!private$areResponsesValid(allResponses)) {
        return(NULL)
      }

      # Parse and combine results
      result <- private$parseResponses(allResponses, params$metric, params$threshold)

      if (!private$isResultValid(result)) {
        return(NULL)
      }

      # Filter by datasources
      if (!is.null(params$datasources) && length(params$datasources) > 0) {
        originalRowCount <- nrow(result)
        result <- result[result$Source %in% params$datasources, ]
        if (nrow(result) == 0 && originalRowCount > 0) {
          renderWarning(paste0(
            "Data source filtering removed all results. ",
            "Check that your selected datasources match PANTHER's available sources."
          ))
        }
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
        #      PANTHER terms (FE formula needs bgSize).
        # The PANTHER API does not return total background size directly.
        # It may be possible to infer it indirectly from other values
        # the API returns that are computed using the background size.
        # See also: identical TODOs in STRING, GeneCodis, Enrichr.
        backgroundSize = private$getSimpleBackgroundSize(backgroundList)
      ))
    },

    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    }
  ),

  private = list(
    sendRequests = function(inputList, taxid, backgroundList, datasets, correction) {
      url <- "https://pantherdb.org/services/oai/pantherdb/enrich/overrep"
      identifiers <- paste0(inputList, collapse = ",")
      allResults <- list()

      for (dataset in datasets) {
        if (is.null(backgroundList)) {
          params <- list(
            geneInputList = identifiers,
            organism = taxid,
            annotDataSet = dataset,
            correction = correction,
            enrichmentTestType = "FISHER",
            mappedInfo = "COMP_LIST"
          )
        } else {
          background <- paste0(backgroundList, collapse = ",")
          params <- list(
            geneInputList = identifiers,
            organism = taxid,
            refInputList = background,
            refOrganism = taxid,
            annotDataSet = dataset,
            correction = correction,
            enrichmentTestType = "FISHER",
            mappedInfo = "COMP_LIST"
          )
        }

        response <- httr::POST(url, body = params, encode = "form")
        allResults[[dataset]] <- response
      }

      return(allResults)
    },

    areResponsesValid = function(responseList) {
      for (dataset in names(responseList)) {
        if (responseList[[dataset]]$status_code != 200) {
          renderWarning(paste("Connection to PANTHER failed for", dataset))
          return(FALSE)
        }
      }
      return(TRUE)
    },

    parseResponses = function(responseList, metric, threshold) {
      allResults <- data.frame()
      threshold <- as.numeric(threshold)

      sigColumn <- switch(metric,
        "False discovery rate" = "fdr",
        "FDR" = "fdr",
        "P-value" = "pValue",
        "NONE" = "pValue",
        "Bonferroni" = "pValue",
        "BONFERRONI" = "pValue",
        "fdr"
      )

      for (dataset in names(responseList)) {
        response <- responseList[[dataset]]
        responseBody <- rawToChar(httr::content(response, "raw"))

        if (nchar(responseBody) == 0) next

        tryCatch({
          pantherResult <- jsonlite::fromJSON(responseBody)
        }, error = function(e) {
          next
        })

        if (!"results" %in% names(pantherResult)) next
        if (!"result" %in% names(pantherResult$results)) next

        results <- pantherResult$results$result
        if (length(results) == 0 || !is.data.frame(results)) next

        results$annotation_dataset <- dataset
        results$flame_source <- private$mapPantherDatasetToFlameSource(dataset)
        results$term_id <- results$term$id
        results$term_label <- results$term$label

        query_size <- if (nrow(allResults) == 0) {
          max(results$number_in_list, na.rm = TRUE)
        } else {
          max(c(allResults$query_size[1], max(results$number_in_list, na.rm = TRUE)), na.rm = TRUE)
        }
        results$query_size <- query_size

        # Filter by threshold
        if (metric %in% c("False discovery rate", "FDR")) {
          significant_mask <- results[[sigColumn]] < threshold
        } else {
          significant_mask <- results$pValue < threshold
        }
        results <- results[significant_mask, ]

        if (nrow(results) == 0) next

        # Filter unclassified
        unclassified_mask <- is.na(results$term_id) |
          is.na(results$term_label) |
          results$term_label == "" |
          toupper(results$term_label) == "UNCLASSIFIED"
        results <- results[!unclassified_mask, ]

        if (nrow(results) == 0) next

        positive_hits <- if ("input_list" %in% names(results)) {
          private$extractPantherGeneList(results$input_list)
        } else {
          rep("", nrow(results))
        }

        pantherSelected <- data.frame(
          Source = results$flame_source,
          Term_ID = results$term_id,
          Function = results$term_label,
          Pvalue = results[[sigColumn]],
          Term_Size = results$number_in_reference,
          Query_size = results$query_size,
          Intersection_Size = results$number_in_list,
          Positive_Hits = positive_hits,
          stringsAsFactors = FALSE
        )

        colnames(pantherSelected) <- ENRICHMENT_DF_COLNAMES
        allResults <- rbind(allResults, pantherSelected)
      }

      if (nrow(allResults) > 0) {
        allResults <- private$alterPantherSourceKeywords(allResults)
        allResults <- private$mapPantherTermIds(allResults)
      }

      return(allResults)
    },

    # -------------------------------------------------------------------------
    # PANTHER-specific helper methods
    # -------------------------------------------------------------------------

    extractPantherGeneList = function(input_list_column) {
      if (is.null(input_list_column) || length(input_list_column) == 0) {
        return("")
      }

      if (is.data.frame(input_list_column)) {
        n <- nrow(input_list_column)
      } else {
        n <- length(input_list_column)
      }

      if (is.null(n) || n == 0) {
        return("")
      }

      gene_lists <- sapply(seq_len(n), function(i) {
        if (is.data.frame(input_list_column)) {
          row_data <- input_list_column[i, ]
        } else {
          row_data <- input_list_column[[i]]
        }
        if (!is.null(row_data$mapped_panther_ids) && !is.na(row_data$mapped_panther_ids)) {
          return(as.character(row_data$mapped_panther_ids))
        } else {
          return("")
        }
      })

      return(gene_lists)
    },

    alterPantherSourceKeywords = function(pantherResult) {
      pantherResult$Source <- gsub("^GO:0003674$", "GO:MF", pantherResult$Source)
      pantherResult$Source <- gsub("^GO:0008150$", "GO:BP", pantherResult$Source)
      pantherResult$Source <- gsub("^GO:0005575$", "GO:CC", pantherResult$Source)
      pantherResult$Source <- gsub("^ANNOT_TYPE_ID_REACTOME_PATHWAY$", "REAC", pantherResult$Source)
      return(pantherResult)
    },

    mapPantherTermIds = function(pantherResult) {
      if (any(pantherResult$Source == "REAC")) {
        reac_mask <- pantherResult$Source == "REAC"
        missing_prefix <- reac_mask & !grepl("^R-", pantherResult$Term_ID)
        if (any(missing_prefix)) {
          pantherResult[missing_prefix, "Term_ID"] <- paste0("R-", pantherResult[missing_prefix, "Term_ID"])
        }
      }
      return(pantherResult)
    },

    mapDataSourcesToPantherDatasets = function(selectedDataSources) {
      datasetMapping <- list(
        "GO:MF" = "GO:0003674",
        "GO:BP" = "GO:0008150",
        "GO:CC" = "GO:0005575",
        "GOSLIM:MF" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_MF",
        "GOSLIM:BP" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_BP",
        "GOSLIM:CC" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_CC",
        "REAC" = "ANNOT_TYPE_ID_REACTOME_PATHWAY",
        "PANTHER Pathways" = "ANNOT_TYPE_ID_PANTHER_PATHWAY",
        "PANTHERPC" = "ANNOT_TYPE_ID_PANTHER_PC"
      )

      pantherDatasets <- c()
      for (source in selectedDataSources) {
        if (source %in% names(datasetMapping)) {
          pantherDatasets <- c(pantherDatasets, datasetMapping[[source]])
        }
      }

      if (length(pantherDatasets) == 0) {
        pantherDatasets <- unlist(datasetMapping)
      }

      return(pantherDatasets)
    },

    mapPantherDatasetToFlameSource = function(dataset) {
      mapping <- list(
        "GO:0003674" = "GO:MF",
        "GO:0008150" = "GO:BP",
        "GO:0005575" = "GO:CC",
        "ANNOT_TYPE_ID_PANTHER_GO_SLIM_MF" = "GOSLIM:MF",
        "ANNOT_TYPE_ID_PANTHER_GO_SLIM_BP" = "GOSLIM:BP",
        "ANNOT_TYPE_ID_PANTHER_GO_SLIM_CC" = "GOSLIM:CC",
        "ANNOT_TYPE_ID_REACTOME_PATHWAY" = "REAC",
        "ANNOT_TYPE_ID_PANTHER_PATHWAY" = "PANTHER Pathways",
        "ANNOT_TYPE_ID_PANTHER_PC" = "PANTHERPC"
      )

      if (dataset %in% names(mapping)) {
        return(mapping[[dataset]])
      } else {
        return(dataset)
      }
    }
  )
)

# Register the strategy
strategyRegistry$register(ToolId$PANTHER, ParadigmId$ORA, PANTHERORAStrategy$new())
