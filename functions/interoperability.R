arenaHandler <- function(enrichmentType, networkId) {
  tryCatch({
    showModal(modalDialog(HTML("<h2>Please wait.</h2>
                                 <p>Building network for Arena3Dweb</p>"), footer = NULL))
    currentArenaEdgelist <- arenaEdgelist[[paste(enrichmentType, networkId, sep = "_")]]
    if (existsNetwork(currentArenaEdgelist)){
      jsonBody <- constructArenaJSONBody(currentArenaEdgelist)
      sendToAPI(ARENA_API_LINK, jsonBody)
    }
  }, error = function(e) {
    print(paste("Error while preparing JSON for Arena: ", e))
    renderWarning("Cannot open Arena3Dweb network at this time. Try again in a while.")
  }, finally = removeModal())
}

existsNetwork <- function(currentArenaEdgelist) {
  exist <- F
  if (nrow(currentArenaEdgelist) > 0)
    exist <- T
  else
    renderWarning("Make sure a visible network exists.")
  return(exist)
}

constructArenaJSONBody <- function(currentArenaEdgelist) {
  scene <- buildScene()
  layers <- buildLayers(currentArenaEdgelist)
  nodes <- buildNodes(currentArenaEdgelist)
  edges <- buildEdges(currentArenaEdgelist)
  universalLabelColor <- "#FFFFFF"
  
  jsonBody <- list(scene = scene, layers = layers,
                   nodes = nodes, edges = edges,
                   universalLabelColor = universalLabelColor)
  return(jsonBody)
}

buildScene <- function() {
  scene <- list(position_x = "0", position_y = "0", scale = "0.9", color = "#000000",
                rotation_x = "0.2618", rotation_y = "0.2618",  rotation_z = "0.0873")
  return(scene)
}

buildLayers <- function(currentArenaEdgelist) {
  layers <- data.frame()
  layerNames <- getLayerNames(currentArenaEdgelist)
  layerPositionMultiplier <- calculateLayerPositionMultiplier(layerNames)
  
  for (i in 1:length(layerNames)){
    layers <- rbind(
      layers,
      c(layerNames[i],
        as.character((i - layerPositionMultiplier) * ARENA_LAYER_SPACING_PIXELS),
        "0", "0", "1", "0", "0", "0", "#777777", "820")
    )
  }
  colnames(layers) <- c("name", "position_x","position_y", "position_z",
                        "last_layer_scale","rotation_x","rotation_y","rotation_z",
                        "floor_current_color","geometry_parameters_width")
  return(layers)
}

getLayerNames <- function(currentArenaEdgelist) {
  if (isFunctionsVsGenesMode(currentArenaEdgelist))
    layerNames <- c(unique(currentArenaEdgelist$`Source Database`), "Gene")
  else if (isGenesVsGenesMode(currentArenaEdgelist))
    layerNames <- c("Gene")
  else { # Functions Vs Functions mode
    layerNames <- unique(c(
      currentArenaEdgelist$`Source Database`,
      currentArenaEdgelist$`Target Database`
    ))
  }
  return(layerNames)
}

isFunctionsVsGenesMode <- function(currentArenaEdgelist) {
  return("Target Gene" %in% colnames(currentArenaEdgelist))
}

isGenesVsGenesMode <- function(currentArenaEdgelist) {
  return("Common Functions" %in% colnames(currentArenaEdgelist))
}

calculateLayerPositionMultiplier <- function(layerNames) {
  # remainder 1 for odd number of layerNames returns TRUE
  if (length(layerNames) %% 2)
    layerPositionMultiplier <- length(layerNames) - floor(length(layerNames) / 2)
  else
    layerPositionMultiplier <- length(layerNames) / 2 + 0.5
}

buildNodes <- function(currentArenaEdgelist) {
  # setting seed for random y, x coordinate sampling
  set.seed(123)
  currentArenaEdgelist <- appendNodeColors(currentArenaEdgelist)

  if (isFunctionsVsGenesMode(currentArenaEdgelist)) {
    nodes <- appendFunctionVsGeneNodes(currentArenaEdgelist)
  } else {
    nodes <- appendSameVsSameModeNodes(currentArenaEdgelist)
  }
  colnames(nodes) <- c("name", "layer",
                       "position_x", "position_y", "position_z",
                       "scale", "color", "url", "descr")
  return(nodes)
}

appendNodeColors <- function(currentArenaEdgelist) {
  if (isGenesVsGenesMode(currentArenaEdgelist)) {
    currentArenaEdgelist$`Source Database` <- "Gene"
    currentArenaEdgelist$`Target Database` <- "Gene"
    currentArenaEdgelist$`Source Color` <- GENE_NODE_COLOR
    currentArenaEdgelist$`Target Color` <- GENE_NODE_COLOR
  } else { # Functions Vs Genes and Functions Vs Functions modes
    currentArenaEdgelist$`Source Color` <-
      appendDatasourceColorColumn(currentArenaEdgelist,
                                  colorSourceColumn = "Source Database")
    if (isFunctionsVsGenesMode(currentArenaEdgelist)) {
      currentArenaEdgelist$`Target Color` <- GENE_NODE_COLOR
    } else { # Functions Vs Functions mode
      currentArenaEdgelist$`Target Color` <-
        appendDatasourceColorColumn(currentArenaEdgelist,
                                    colorSourceColumn = "Target Database")
    }
  }
  return(currentArenaEdgelist)
}

appendDatasourceColorColumn <- function(currentArenaEdgelist, colorSourceColumn) {
  if (currentArenaEdgelist[[colorSourceColumn]][1] %in% names(DATASOURCE_COLORS)){
    colorList <- as.character(DATASOURCE_COLORS[
      currentArenaEdgelist[[colorSourceColumn]]
    ])
  } else
    colorList <- LITERATURE_NODE_COLOR
  return(colorList)
}

appendFunctionVsGeneNodes <- function(currentArenaEdgelist) {
  nodes <- data.frame()
  nodes <- appendFunctionNodes(nodes, currentArenaEdgelist)
  nodes <- appendGeneNodes(nodes, currentArenaEdgelist)
  return(nodes)
}

appendFunctionNodes <- function(nodes, currentArenaEdgelist) {
  uniqueFunctionNodeRows <- dplyr::distinct(currentArenaEdgelist,
                                     `Source Database`, `Source Id`,
                                     `Source Name`, `Source Color`)
  for (i in 1:nrow(uniqueFunctionNodeRows)) {
    nodes <- rbind(
      nodes,
      c(
        uniqueFunctionNodeRows$`Source Name`[i],
        uniqueFunctionNodeRows$`Source Database`[i],
        "0",
        as.character(sample(-ARENA_Y_Z_SAMPLING_LIMIT:ARENA_Y_Z_SAMPLING_LIMIT, 1)),
        as.character(sample(-ARENA_Y_Z_SAMPLING_LIMIT:ARENA_Y_Z_SAMPLING_LIMIT, 1)),
        "1", uniqueFunctionNodeRows$`Source Color`[i], "", ""
      )
    )
  }
  return(nodes)
}

appendGeneNodes <- function(nodes, currentArenaEdgelist) {
  uniqueGeneNodeRows <- dplyr::distinct(currentArenaEdgelist, `Target Gene`)
  for (i in 1:nrow(uniqueGeneNodeRows)) {
    nodes <- rbind(
      nodes,
      c(uniqueGeneNodeRows$`Target Gene`[i], "Gene", "0",
        as.character(sample(-ARENA_Y_Z_SAMPLING_LIMIT:ARENA_Y_Z_SAMPLING_LIMIT, 1)),
        as.character(sample(-ARENA_Y_Z_SAMPLING_LIMIT:ARENA_Y_Z_SAMPLING_LIMIT, 1)),
        "1", GENE_NODE_COLOR, "", "")
    )
  }
  return(nodes)
}

appendSameVsSameModeNodes <- function(currentArenaEdgelist) {
  nodes <- data.frame()
  uniqueNodeRows <- combineUniqueNodeRows(currentArenaEdgelist)
  
  for (i in 1:nrow(uniqueNodeRows)) {
    nodes <- rbind(
      nodes,
      c(uniqueNodeRows$Name[i], uniqueNodeRows$Database[i], "0",
        as.character(sample(-ARENA_Y_Z_SAMPLING_LIMIT:ARENA_Y_Z_SAMPLING_LIMIT, 1)),
        as.character(sample(-ARENA_Y_Z_SAMPLING_LIMIT:ARENA_Y_Z_SAMPLING_LIMIT, 1)),
        "1", uniqueNodeRows$Color[i], "", "")
    )
  }
  return(nodes)
}

combineUniqueNodeRows <- function(currentArenaEdgelist) {
  tempEdgelist1 <-
    currentArenaEdgelist[, c("Source Name", "Source Database", "Source Color")]
  tempEdgelist2 <-
    currentArenaEdgelist[, c("Target Name", "Target Database", "Target Color")]
  colnames(tempEdgelist1) <- c("Name", "Database", "Color")
  colnames(tempEdgelist2) <- colnames(tempEdgelist1)
  
  uniqueNodeRows <- rbind(tempEdgelist1, tempEdgelist2)
  uniqueNodeRows <- dplyr::distinct(uniqueNodeRows)
  return(uniqueNodeRows)
}

buildEdges <- function(currentArenaEdgelist) {
  edges <- data.frame()
  currentArenaEdgelist <- appendEdgeNames(currentArenaEdgelist)

  for (i in 1:nrow(currentArenaEdgelist)){
    edges <- rbind(edges,
                   c(currentArenaEdgelist$src[i],
                     currentArenaEdgelist$trg[i],
                     "1", "#CFCFCF", "" ))
  }
  colnames(edges) <- c("src", "trg", "opacity", "color", "channel")
  return(edges)
}

appendEdgeNames <- function(currentArenaEdgelist) {
  if (isGenesVsGenesMode(currentArenaEdgelist)) {
    currentArenaEdgelist$src <- paste0(currentArenaEdgelist$`Source Name`, "_Gene")
    currentArenaEdgelist$trg <- paste0(currentArenaEdgelist$`Target Name`, "_Gene")
  } else {
    currentArenaEdgelist$src <- 
      paste0(currentArenaEdgelist$`Source Name`,
             "_", currentArenaEdgelist$`Source Database`)
    if (isFunctionsVsGenesMode(currentArenaEdgelist))
      currentArenaEdgelist$trg <- paste0(currentArenaEdgelist$`Target Gene`, "_Gene")
    else
      currentArenaEdgelist$trg <- 
        paste0(currentArenaEdgelist$`Target Name`,
               "_", currentArenaEdgelist$`Target Database`)
  }
  return(currentArenaEdgelist)
}

sendToAPI <- function(apiLink, jsonBody) {
  # ,verbose() to print info
  response <- httr::POST(apiLink, body = jsonBody, encode = "json")
  urlFromResponse <- httr::content(response, as = "parsed")$url
  session$sendCustomMessage("handler_browseUrl", urlFromResponse)
}
