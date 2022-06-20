# Interoperability functions ####

# Parse current network into Arena3Dweb format
# @params edgelist (dataframe): source and target
# @params networkSelect (string): category name for layer
# @return void
arenaHandler <- function (edgelist, networkSelect) {
  if ( nrow(edgelist) > 0 ){
    scene_pan <- list(position_x = "0", position_y = "0",  scale_x = "0.9", color = "#000000")
    scene_sphere <- list(rotation_x = "0.2618", rotation_y = "0.2618",  rotation_z = "0.0873")
    
    layer_df <- data.frame()
    layers <- c(networkSelect, "Gene")
    
    # # remainder 1, odd number of layers
    # if (length(layers) %% 2) pos <- length(layers) - floor( length(layers)/2 )
    # else 
    pos <- length(layers)/2 + 0.5 # even number of layers

    for (i in 1:length(layers)){
      layer_df <- rbind(layer_df, c(layers[i], as.character( (i-pos) * 300), "0", "0",
                                    "1", "0", "0", "0",
                                    "#777777", "820"))
    }
    colnames(layer_df) <- c("name", "position_x","position_y", "position_z",
                            "last_layer_scale","rotation_x","rotation_y","rotation_z",
                            "floor_current_color","geometry_parameters_width")
    
    nodes_df <- data.frame()
    nodes <- unique(edgelist[, 1]) #1st column, category
    for (i in 1:length(nodes)){
      if (networkSelect %in% names(bar_colors)) color <- as.character(bar_colors[networkSelect])
      else if (networkSelect %in% names(aGoBar_colors)) color <- as.character(aGoBar_colors[networkSelect])
      else color <- "#c95591" # Literature
      nodes_df <- rbind(nodes_df, c(nodes[i], networkSelect,
                                    "0", as.character(sample(-410:410, 1)), as.character(sample(-410:410, 1)), "1",
                                    color, "", ""))
    }
    nodes <- unique(edgelist[, 2]) #2nd column, genes
    for (i in 1:length(nodes)){
      nodes_df <- rbind(nodes_df, c(nodes[i], "Gene",
                                    "0", as.character(sample(-410:410, 1)), as.character(sample(-410:410, 1)), "1",
                                    "#b6b9bf", "", ""))
    }
    colnames(nodes_df) <- c("name","layer", "position_x", "position_y", "position_z", "scale_x", "color", "url", "descr")
    
    edges_df <- data.frame()
    for (i in 1:nrow(edgelist)){
      src <- paste0( edgelist[i, 1], "_", networkSelect)
      trg <- paste0( edgelist[i, 2], "_Gene")
      edges_df <- rbind(edges_df, c(src, trg, "1", "#CFCFCF", "" ))
    }
    colnames(edges_df) <- c("src", "trg", "opacity", "color", "channel")
    
    matrix <- list(scene_pan = scene_pan, scene_sphere = scene_sphere,
                   layers = layer_df, nodes = nodes_df, edges = edges_df,
                   universal_label_color = "#FFFFFF")
    
    res <- POST(ARENA_API_LINK, body = matrix, encode = "json") # , verbose() to print info
    print(res)
    js$BrowseURL(content(res, as="parsed")$url)
  } else shinyalert("Warning!", "There is currently no visible network to send to Arena3Dweb.", type = "error")
}
