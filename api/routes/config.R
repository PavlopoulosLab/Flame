# /api/config — static configuration for the web client (no workspace needed)

.config_cache <- NULL

.build_config <- function() {
  tools <- lapply(getWorkspaceToolUI(), function(t) {
    cfg <- TOOLS[[t$toolId]]
    metrics <- cfg$metrics %||% list()
    list(
      key = t$key, id = t$toolId, label = t$label, name = cfg$name %||% t$label,
      paradigms = as.list(cfg$paradigms),
      # Tools with organism-specific sources (Enrichr, GeneCodis) have no plain list: offer the union.
      datasources = as.list(cfg$datasources %||% unique(unlist(cfg$organismDatasources)) %||% character(0)),
      metrics = lapply(names(metrics), function(n) list(label = n, value = as.character(metrics[[n]]))),
      defaultMetricGenome = cfg$defaultMetricGenome,
      defaultMetricBackground = cfg$defaultMetricBackground,
      supportsBackground = isTRUE(cfg$supportsBackground),
      organismCount = length(unlist(TOOL_ORGANISMS[[t$toolId]] %||% list())),
      color = paste0("var(--color-", t$key, ")")
    )
  })
  categories <- lapply(names(DATASOURCES_PRINT), function(cat) {
    entries <- DATASOURCES_PRINT[[cat]]
    list(category = cat,
         sources = lapply(names(entries), function(label) list(label = label, id = entries[[label]])))
  })
  tabs <- getResultsTabUiConfigForClient()
  tabs <- lapply(tabs, function(t) { t$label <- enc2utf8(t$label); t })
  list(
    version = as.character(utils::packageVersion("flame")),
    tools = tools,
    datasourceCategories = categories,
    defaultDatasources = as.list(DATASOURCES_DEFAULT_SELECTED),
    gseaDefaultDatasources = as.list(GSEA_DEFAULT_DATASOURCES),
    msigdbCollections = local({
      counts <- msigdb_set_counts()
      lapply(MSIGDB_COLLECTIONS, function(cl) list(
        code = cl$code, name = cl$name, text = cl$text,
        sources = lapply(names(cl$sources), function(id) list(id = id, label = cl$sources[[id]],
                                                               sets = if (is.na(counts[id])) NULL else counts[[id]]))))
    }),
    datasourceColors = as.list(DATASOURCE_COLORS),
    linkProviders = as.list(DATASOURCE_LINK_PROVIDERS),
    geneNodeColor = GENE_NODE_COLOR,
    idFormats = getWorkspaceIdFormats(),
    commonOrganisms = getWorkspaceCommonOrganismChoices(),
    analytes = getAnalyteUI(),
    resultTabs = tabs,
    networkLayouts = as.list(names(NETWORK_LAYOUTS)),
    customSourceTool = list(key = "custom", label = "Custom GMT", metrics = list(
      list(label = "Benjamini-Hochberg FDR", value = "BH"),
      list(label = "Bonferroni", value = "bonferroni"),
      list(label = "None (p-value)", value = "none"))),
    limits = list(maxLists = FLAME_LIMITS$maxLists, maxIdsPerList = FLAME_LIMITS$maxIdsPerList,
                  maxTextBytes = FLAME_LIMITS$maxTextBytes, retentionDays = FLAME_LIMITS$retentionDays,
                  stringMaxIds = 500L),
    emailNotifications = nzchar(Sys.getenv("FLAME_SMTP_URL", ""))
  )
}

#* Client configuration: tools, metrics, datasources, ID formats, organisms, limits
#* @get /
#* @serializer flamejson
function(res) {
  api_handle(res, {
    if (is.null(.config_cache)) .config_cache <<- .build_config()
    .config_cache
  })
}
