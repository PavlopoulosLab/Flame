# =============================================================================
# Reactome ORA Strategy
# =============================================================================
#
# Over-representation analysis with Reactome's own Analysis Service
# (https://reactome.org/AnalysisService; Fabregat et al. 2017, BMC Bioinformatics
# 18:142). Reactome tests every pathway of the species against the genes
# it can map (hypergeometric test, Benjamini–Hochberg FDR) and returns, per
# pathway, the submitted identifiers it found. Human pathways are curated;
# the other species use Reactome's computationally inferred pathways.
#
# Reactome has no custom background: the universe is every entity it covers
# for the species.
# =============================================================================

REACTOME_ANALYSIS <- "https://reactome.org/AnalysisService"
ReactomeORAStrategy <- R6::R6Class("ReactomeORAStrategy",

  inherit = EnrichmentStrategy,

  public = list(
    initialize = function() {
      super$initialize(ToolId$REACTOME, ParadigmId$ORA)
    },

    run = function(inputList, organism, backgroundList, params) {
      if (!"REAC" %in% params$datasources) return(NULL)
      ids <- unique(as.character(inputList))
      if (!length(ids)) return(NULL)
      flame_progress(0.4, "Reactome is testing its pathways")
      human <- identical(as.integer(organism), 9606L)
      url <- paste0(REACTOME_ANALYSIS, "/identifiers/", if (human) "projection" else "")
      query <- list(interactors = "false", pageSize = 5000, page = 1, sortBy = "ENTITIES_FDR", order = "ASC",
                    resource = "TOTAL", pValue = 1, includeDisease = if (isFALSE(params$includeDisease)) "false" else "true")
      if (!human) query$species <- as.character(organism)
      resp <- httr::POST(url, query = query, body = paste(ids, collapse = "\n"), httr::content_type("text/plain"), httr::timeout(300))
      if (httr::status_code(resp) == 404) return(NULL)   # nothing could be mapped
      if (httr::status_code(resp) >= 400) stop(sprintf("Reactome Analysis Service answered with HTTP %d.", httr::status_code(resp)))
      d <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
      pw <- d$pathways
      if (!length(pw)) return(NULL)
      token <- utils::URLdecode(d$summary$token)

      pvals <- vapply(pw, function(p) as.numeric(p$entities$pValue), 0)
      fdrs <- vapply(pw, function(p) as.numeric(p$entities$fdr), 0)
      metricValue <- if (identical(params$metric, "p_value")) pvals else fdrs
      keep <- which(metricValue <= as.numeric(params$threshold))
      if (!length(keep)) return(NULL)
      pw <- pw[keep]; metricValue <- metricValue[keep]
      # Species filter (non-human requests can include human disease pathways).
      sp <- vapply(pw, function(p) as.character(p$species$taxId %||% ""), "")
      ok <- sp == as.character(organism) | !nzchar(sp)
      pw <- pw[ok]; metricValue <- metricValue[ok]
      if (!length(pw)) return(NULL)

      flame_progress(0.7, "Fetching the genes found in each pathway")
      stIds <- vapply(pw, `[[`, "", "stId")
      found <- list()
      for (chunk in split(stIds, ceiling(seq_along(stIds) / 200))) {
        fr <- httr::POST(paste0(REACTOME_ANALYSIS, "/token/", utils::URLencode(token, reserved = TRUE), "/found/all"),
                         query = list(resource = "TOTAL"), body = paste(chunk, collapse = ","), httr::content_type("text/plain"), httr::timeout(300))
        if (httr::status_code(fr) >= 400) next
        for (x in jsonlite::fromJSON(httr::content(fr, as = "text", encoding = "UTF-8"), simplifyVector = FALSE))
          found[[x$pathway]] <- unique(vapply(x$entities, function(e) as.character(e$id), ""))
      }
      querySize <- length(ids) - length(unlist(d$identifiersNotFound %||% list()))
      if (!is.null(d$identifiersNotFound) && is.numeric(d$identifiersNotFound)) querySize <- length(ids) - d$identifiersNotFound
      res <- data.frame(
        Source = "REAC", Term_ID = stIds, Function = vapply(pw, `[[`, "", "name"),
        `P-value` = metricValue,
        `Term Size` = vapply(pw, function(p) as.numeric(p$entities$total), 0),
        `Query size` = querySize,
        `Intersection Size` = vapply(stIds, function(s) length(found[[s]] %||% character(0)), 0),
        `Positive Hits` = vapply(stIds, function(s) paste(found[[s]] %||% character(0), collapse = ","), ""),
        check.names = FALSE, stringsAsFactors = FALSE)
      res$`Intersection Size`[res$`Intersection Size` == 0] <- vapply(pw[res$`Intersection Size` == 0], function(p) as.numeric(p$entities$found), 0)
      colnames(res) <- ENRICHMENT_DF_COLNAMES
      # Reactome reports ratio = pathway size / all entities it covers: recover that universe for fold enrichment.
      universe <- stats::median(vapply(pw, function(p) as.numeric(p$entities$total) / as.numeric(p$entities$ratio), 0), na.rm = TRUE)
      list(result = res, backgroundSize = if (is.finite(universe)) round(universe) else NULL, token = token)
    },

    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    }
  )
)

strategyRegistry$register(ToolId$REACTOME, ParadigmId$ORA, ReactomeORAStrategy$new())
