# =============================================================================
# Arena3D Network Export API Functions
# =============================================================================
# Stateless functions for building Arena3D JSON and calling the API.
# Used by arenaHandlerForRun() in func-observers.R.
#
# API Documentation: https://arena3d.org/
# =============================================================================

# --- Constants ---
ARENA_API_URL <- "https://pavlopoulos-lab-services.org/bib/api/arena3dweb"
ARENA_LAYER_SPACING <- 300
ARENA_COORDINATE_RANGE <- 410

# --- Main API Function ---

#' Export edgelist to Arena3D
#'
#' Builds JSON from edgelist and POSTs to Arena3D API.
#'
#' @param edgelist Data frame with network data. Expected columns depend on mode:
#'   - Functions vs Genes: Source Name, Source Database, Source Id, Target Gene
#'   - Genes vs Genes: Source Name, Target Name, Common Functions
#'   - Functions vs Functions: Source Name, Source Database, Target Name, Target Database
#' @param api_url Character. Arena3D API endpoint URL.
#' @return List with either:
#'   - success=TRUE, url=<arena3d_url>
#'   - success=FALSE, error=<error_message>
#' @examples
#' \dontrun{
#' result <- arena_export(my_edgelist)
#' if (result$success) browseURL(result$url)
#' }
arena_export <- function(edgelist, api_url = ARENA_API_URL) {
  tryCatch({
    json_body <- arena_build_json(edgelist)

    response <- httr::POST(
      api_url,
      body = json_body,
      encode = "json",
      httr::content_type_json()
    )

    if (httr::status_code(response) != 200) {
      warning(paste("[arena_export] API error:", httr::status_code(response)))
      return(list(
        success = FALSE,
        error = "Arena3D service is temporarily unavailable. Please try again later."
      ))
    }

    parsed_response <- httr::content(response, as = "parsed")
    url <- parsed_response$url

    if (is.null(url)) {
      warning("[arena_export] API returned no URL")
      return(list(
        success = FALSE,
        error = "Arena3D returned an invalid response. Please try again."
      ))
    }

    return(list(success = TRUE, url = url))

  }, error = function(e) {
    warning(paste("[arena_export] Error:", conditionMessage(e)))
    return(list(
      success = FALSE,
      error = "Failed to export to Arena3D. Please try again."
    ))
  })
}

# --- JSON Building Functions ---

#' Build complete Arena3D JSON body from edgelist
#' @param edgelist Data frame with network data
#' @return List suitable for JSON serialization
arena_build_json <- function(edgelist) {
  scene <- arena_build_scene()
  layers <- arena_build_layers(edgelist)
  nodes <- arena_build_nodes(edgelist)
  edges <- arena_build_edges(edgelist)

  json_body <- list(
    scene = scene,
    layers = layers,
    nodes = nodes,
    edges = edges,
    universalLabelColor = "#FFFFFF",
    direction = FALSE,
    edgeOpacityByWeight = TRUE
  )
  return(json_body)
}

#' Build scene configuration
#' @return List with position, scale, rotation, color
arena_build_scene <- function() {
  list(
    position_x = "0",
    position_y = "0",
    scale = "0.9",
    color = "#000000",
    rotation_x = "0.2618",
    rotation_y = "0.2618",
    rotation_z = "0.0873"
  )
}

#' Build layer definitions from edgelist
#' @param edgelist Data frame with network data
#' @return Data frame with layer configuration
arena_build_layers <- function(edgelist) {
  layers <- data.frame()
  layer_names <- arena_get_layer_names(edgelist)
  layer_offset <- arena_calculate_layer_offset(layer_names)

  for (i in seq_along(layer_names)) {
    layers <- rbind(
      layers,
      c(
        layer_names[i],
        as.character((i - layer_offset) * ARENA_LAYER_SPACING),
        "0", "0", "1", "0", "0", "0", "#777777", "820"
      )
    )
  }
  colnames(layers) <- c(
    "name", "position_x", "position_y", "position_z",
    "last_layer_scale", "rotation_x", "rotation_y", "rotation_z",
    "floor_current_color", "geometry_parameters_width"
  )
  return(layers)
}

#' Build node definitions from edgelist
#' @param edgelist Data frame with network data
#' @return Data frame with node configuration
arena_build_nodes <- function(edgelist) {
  set.seed(123)  # For reproducible random coordinates
  edgelist <- arena_append_node_colors(edgelist)

  if (arena_is_functions_vs_genes(edgelist)) {
    nodes <- arena_build_function_vs_gene_nodes(edgelist)
  } else {
    nodes <- arena_build_same_mode_nodes(edgelist)
  }

  colnames(nodes) <- c(
    "name", "layer", "position_x", "position_y", "position_z",
    "scale", "color", "url", "descr"
  )
  return(nodes)
}

#' Build edge definitions from edgelist
#' @param edgelist Data frame with network data
#' @return Data frame with edge configuration
arena_build_edges <- function(edgelist) {
  edges <- data.frame()
  edgelist <- arena_append_edge_names(edgelist)

  for (i in seq_len(nrow(edgelist))) {
    edges <- rbind(
      edges,
      c(edgelist$src[i], edgelist$trg[i], "1", "#CFCFCF", "")
    )
  }
  colnames(edges) <- c("src", "trg", "opacity", "color", "channel")
  return(edges)
}

# --- Helper Functions (internal) ---

#' Get layer names based on network mode
arena_get_layer_names <- function(edgelist) {
  if (arena_is_functions_vs_genes(edgelist)) {
    c(unique(edgelist$`Source Database`), "Gene")
  } else if (arena_is_genes_vs_genes(edgelist)) {
    c("Gene")
  } else {
    # Functions vs Functions mode
    unique(c(edgelist$`Source Database`, edgelist$`Target Database`))
  }
}

#' Check if edgelist is Functions vs Genes mode
arena_is_functions_vs_genes <- function(edgelist) {
  "Target Gene" %in% colnames(edgelist)
}

#' Check if edgelist is Genes vs Genes mode
arena_is_genes_vs_genes <- function(edgelist) {
  "Common Functions" %in% colnames(edgelist)
}

#' Calculate layer position offset for centering
arena_calculate_layer_offset <- function(layer_names) {
  n <- length(layer_names)
  if (n %% 2 == 1) {
    n - floor(n / 2)
  } else {
    n / 2 + 0.5
  }
}

#' Append node colors to edgelist based on mode
arena_append_node_colors <- function(edgelist) {
  if (arena_is_genes_vs_genes(edgelist)) {
    edgelist$`Source Database` <- "Gene"
    edgelist$`Target Database` <- "Gene"
    edgelist$`Source Color` <- GENE_NODE_COLOR
    edgelist$`Target Color` <- GENE_NODE_COLOR
  } else {
    edgelist$`Source Color` <- as.character(
      DATASOURCE_COLORS[edgelist$`Source Database`]
    )
    if (arena_is_functions_vs_genes(edgelist)) {
      edgelist$`Target Color` <- GENE_NODE_COLOR
    } else {
      edgelist$`Target Color` <- as.character(
        DATASOURCE_COLORS[edgelist$`Target Database`]
      )
    }
  }
  return(edgelist)
}

#' Build nodes for Functions vs Genes mode
arena_build_function_vs_gene_nodes <- function(edgelist) {
  nodes <- data.frame()
  nodes <- arena_build_function_nodes(nodes, edgelist)
  nodes <- arena_build_gene_nodes(nodes, edgelist)
  return(nodes)
}

#' Build function nodes
arena_build_function_nodes <- function(nodes, edgelist) {
  unique_rows <- dplyr::distinct(
    edgelist,
    `Source Database`, `Source Id`, `Source Name`, `Source Color`
  )
  for (i in seq_len(nrow(unique_rows))) {
    nodes <- rbind(
      nodes,
      c(
        unique_rows$`Source Name`[i],
        unique_rows$`Source Database`[i],
        "0",
        as.character(sample(-ARENA_COORDINATE_RANGE:ARENA_COORDINATE_RANGE, 1)),
        as.character(sample(-ARENA_COORDINATE_RANGE:ARENA_COORDINATE_RANGE, 1)),
        "1",
        unique_rows$`Source Color`[i],
        "", ""
      )
    )
  }
  return(nodes)
}

#' Build gene nodes
arena_build_gene_nodes <- function(nodes, edgelist) {
  unique_rows <- dplyr::distinct(edgelist, `Target Gene`)
  for (i in seq_len(nrow(unique_rows))) {
    nodes <- rbind(
      nodes,
      c(
        unique_rows$`Target Gene`[i],
        "Gene",
        "0",
        as.character(sample(-ARENA_COORDINATE_RANGE:ARENA_COORDINATE_RANGE, 1)),
        as.character(sample(-ARENA_COORDINATE_RANGE:ARENA_COORDINATE_RANGE, 1)),
        "1",
        GENE_NODE_COLOR,
        "", ""
      )
    )
  }
  return(nodes)
}

#' Build nodes for same-type modes (Genes vs Genes, Functions vs Functions)
arena_build_same_mode_nodes <- function(edgelist) {
  nodes <- data.frame()

  # Combine source and target into unified format
  temp1 <- edgelist[, c("Source Name", "Source Database", "Source Color")]
  temp2 <- edgelist[, c("Target Name", "Target Database", "Target Color")]
  colnames(temp1) <- c("Name", "Database", "Color")
  colnames(temp2) <- c("Name", "Database", "Color")

  unique_rows <- dplyr::distinct(rbind(temp1, temp2))

  for (i in seq_len(nrow(unique_rows))) {
    nodes <- rbind(
      nodes,
      c(
        unique_rows$Name[i],
        unique_rows$Database[i],
        "0",
        as.character(sample(-ARENA_COORDINATE_RANGE:ARENA_COORDINATE_RANGE, 1)),
        as.character(sample(-ARENA_COORDINATE_RANGE:ARENA_COORDINATE_RANGE, 1)),
        "1",
        unique_rows$Color[i],
        "", ""
      )
    )
  }
  return(nodes)
}

#' Append edge source/target names for Arena3D format
arena_append_edge_names <- function(edgelist) {
  if (arena_is_genes_vs_genes(edgelist)) {
    edgelist$src <- paste0(edgelist$`Source Name`, "_Gene")
    edgelist$trg <- paste0(edgelist$`Target Name`, "_Gene")
  } else {
    edgelist$src <- paste0(
      edgelist$`Source Name`, "_", edgelist$`Source Database`
    )
    if (arena_is_functions_vs_genes(edgelist)) {
      edgelist$trg <- paste0(edgelist$`Target Gene`, "_Gene")
    } else {
      edgelist$trg <- paste0(
        edgelist$`Target Name`, "_", edgelist$`Target Database`
      )
    }
  }
  return(edgelist)
}
