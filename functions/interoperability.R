arenaHandler <- function(networkId) {
  tryCatch({
    showModal(modalDialog(HTML("<h2>Please wait.</h2>
                                 <p>Building network for Arena3Dweb</p>"), footer = NULL))
    currentArenaEdgelist <- arenaEdgelist[[networkId]]
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
  if ("Target Gene" %in% colnames(currentArenaEdgelist))
    layerNames <- c(unique(currentArenaEdgelist$`Source Database`), "Gene")
  else if ("Common Functions" %in% colnames(currentArenaEdgelist))
    layerNames <- c("Gene")
  else {
    layerNames <- unique(c(
      currentArenaEdgelist$`Source Database`,
      currentArenaEdgelist$`Target Database`
    ))
  }
    
  # remainder 1 for odd number of layerNames returns TRUE
  if (length(layerNames) %% 2)
    layerPositionMultiplier <- length(layerNames) - floor(length(layerNames) / 2)
  else
    layerPositionMultiplier <- length(layerNames) / 2 + 0.5
  
  layerSpacingPixels <- 300
  for (i in 1:length(layerNames)){
    layers <- rbind(layers,
                    c(layerNames[i],
                      as.character((i - layerPositionMultiplier) * layerSpacingPixels), "0", "0",
                      "1", "0", "0", "0",
                      "#777777", "820"))
  }
  
  colnames(layers) <- c("name", "position_x","position_y", "position_z",
                        "last_layer_scale","rotation_x","rotation_y","rotation_z",
                        "floor_current_color","geometry_parameters_width")
  return(layers)
}

buildNodes <- function(currentArenaEdgelist) {
  # setting seed for random y, x coordinate sampling
  set.seed(123)
  if ("Common Functions" %in% colnames(currentArenaEdgelist)) {
    currentArenaEdgelist$`Source Database` <- "Gene"
    currentArenaEdgelist$`Target Database` <- "Gene"
    currentArenaEdgelist$`Source Color` <- GENE_NODE_COLOR
    currentArenaEdgelist$`Target Color` <- GENE_NODE_COLOR
    nodes <- appendFunctionVsFunctionNodes(currentArenaEdgelist)
  } else {
    currentArenaEdgelist$`Source Color` <-
      appendColorColumn(currentArenaEdgelist,
                        colorSourceColumn = "Source Database")
    
    if ("Target Gene" %in% colnames(currentArenaEdgelist)) {
      currentArenaEdgelist$`Target Color` <- GENE_NODE_COLOR
      nodes <- appendFunctionVsGeneNodes(currentArenaEdgelist)
    } else {
      currentArenaEdgelist$`Target Color` <-
        appendColorColumn(currentArenaEdgelist,
                          colorSourceColumn = "Target Database")
      nodes <- appendFunctionVsFunctionNodes(currentArenaEdgelist)
    }
  }
  
  colnames(nodes) <- c("name", "layer",
                       "position_x", "position_y", "position_z",
                       "scale", "color", "url", "descr")
  return(nodes)
}

appendColorColumn <- function(currentArenaEdgelist, colorSourceColumn) {
  if (currentArenaEdgelist[[colorSourceColumn]][1] %in% names(DATASOURCE_COLORS)){
    colorList <- as.character(DATASOURCE_COLORS[
      currentArenaEdgelist[[colorSourceColumn]]
    ])
  } else if (currentArenaEdgelist[[colorSourceColumn]][1] %in% names(AGOTOOL_COLORS)){
    colorList <- as.character(AGOTOOL_COLORS[
      currentArenaEdgelist[[colorSourceColumn]]
    ])
  } else
    colorList <- LITERATURE_NODE_COLOR
  return(colorList)
}

appendFunctionVsGeneNodes <- function(currentArenaEdgelist) {
  nodes <- data.frame()
  
  uniqueNodeRows <- distinct(
    currentArenaEdgelist,
    `Source Database`, `Source Id`, `Source Name`, `Source Color`
  )
  for (i in 1:nrow(uniqueNodeRows)) {
    nodes <- rbind(
      nodes,
      c(uniqueNodeRows$`Source Name`[i], uniqueNodeRows$`Source Database`[i],
        "0", as.character(sample(-410:410, 1)), as.character(sample(-410:410, 1)),
        "1", uniqueNodeRows$`Source Color`[i], "", "")
    )
  }
  
  uniqueNodeRows <- distinct(
    currentArenaEdgelist, `Target Gene`
  )
  for (i in 1:nrow(uniqueNodeRows)) {
    nodes <- rbind(
      nodes,
      c(uniqueNodeRows$`Target Gene`[i], "Gene",
        "0", as.character(sample(-410:410, 1)), as.character(sample(-410:410, 1)),
        "1", GENE_NODE_COLOR, "", "")
    )
  }
  return(nodes)
}

appendFunctionVsFunctionNodes <- function(currentArenaEdgelist) {
  nodes <- data.frame()
  
  tempEdgelist1 <-
    currentArenaEdgelist[, c("Source Name", "Source Database", "Source Color")]
  tempEdgelist2 <-
    currentArenaEdgelist[, c("Target Name", "Target Database", "Target Color")]
  colnames(tempEdgelist1) <- c("Name", "Database", "Color")
  colnames(tempEdgelist2) <- colnames(tempEdgelist1)
    
  uniqueNodeRows <- rbind(tempEdgelist1, tempEdgelist2)
  uniqueNodeRows <- distinct(uniqueNodeRows)
  for (i in 1:nrow(uniqueNodeRows)) {
    nodes <- rbind(
      nodes,
      c(uniqueNodeRows$Name[i], uniqueNodeRows$Database[i],
        "0", as.character(sample(-410:410, 1)), as.character(sample(-410:410, 1)),
        "1", uniqueNodeRows$Color[i], "", "")
    )
  }
  return(nodes)
}

buildEdges <- function(currentArenaEdgelist) {
  edges <- data.frame()
  
  for (i in 1:nrow(currentArenaEdgelist)){
    if ("Common Functions" %in% colnames(currentArenaEdgelist)) {
      src <- paste0(currentArenaEdgelist$`Source Name`[i], "_Gene")
      trg <- paste0(currentArenaEdgelist$`Target Name`[i], "_Gene")
    } else {
      src <- paste0(currentArenaEdgelist$`Source Name`[i],
                    "_", currentArenaEdgelist$`Source Database`[i])
      if ("Target Gene" %in% colnames(currentArenaEdgelist))
        trg <- paste0(currentArenaEdgelist$`Target Gene`[i], "_Gene")
      else
        trg <- paste0(currentArenaEdgelist$`Target Name`[i],
                      "_", currentArenaEdgelist$`Target Database`[i])
    }
    edges <- rbind(edges, c(src, trg, "1", "#CFCFCF", "" ))
  }
  
  colnames(edges) <- c("src", "trg", "opacity", "color", "channel")
  return(edges)
}

sendToAPI <- function(apiLink, jsonBody) {
  # ,verbose() to print info
  response <- httr::POST(apiLink, body = jsonBody, encode = "json")
  urlFromResponse <- httr::content(response, as = "parsed")$url
  session$sendCustomMessage("handler_browseUrl", urlFromResponse)
}
