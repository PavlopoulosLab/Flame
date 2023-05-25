handleStringOrganismSelection <- function() {
  tryCatch({
    updateAvailableStringNamespaces()
  }, error = function(e) {
    print(paste("String organism error: ", e))
    renderError("Organism selection error. Please try again in a while.")
  })
}

handleStringNetwork <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Generating STRING network.</p>")
    if (existInputGeneLists())
      createSTRINGNetwork()
  }, error = function(e) {
    print(paste("Error: ", e))
    renderError("Problem with STRING network.")
  }, finally = {
    removeModal()
  })
}

handleStringSubmitForEnrichment <- function(category) {
  tryCatch({
    sendSTRINGNetworkForEnrichment(category)
    updateTabItems(session, "sideBarId", selected = sprintf("%s_enrichment", category))
  }, error = function(e) {
    print(paste("Error: ", e))
    renderError("Problem with STRING network.")    
  })
}

createSTRINGNetwork <- function() {
  dataset <- input$STRINGnetworkSelect
  organism <- input$string_network_organism
  type <- input$STRINGnetworkType
  edges <- input$STRINGnetworkEdges
  score <- input$STRINGnetworkScore
  
  printSTRINGParameters(organism, type, edges, score)
  string_taxid <- ORGANISMS[ORGANISMS$print_name == organism, ]$taxid
  selectedListItems <- userInputLists[[dataset]][[1]]
  result_ids <- parseInputsForSTRINGRequest(selectedListItems, string_taxid)
  ids_interactive <- result_ids$ids_interactive
  ids_post <- result_ids$ids_post
  output$string_legend <- createSTRINGNetworkLegend(edges)
  STRINGNetworkData <<-
    createSTRINGNetworkData(ids_post, string_taxid, type, edges, score)
  createSTRINGActionButtons(STRINGNetworkData)
  createSTRINGInteractiveViewer(ids_interactive, string_taxid, type,
                                edges, score)
}

printSTRINGParameters <- function(organism, type, edges, score) {
  stringParameters <- paste0(
    "Organism: ", organism,
    "\nNetwork Type: ", type,
    "\nMeaning of network edges: ", edges,
    "\nInteraction score cut-off: ", as.numeric(score) / 1000
  )
  renderShinyText("networkParameters", stringParameters)
}

parseInputsForSTRINGRequest <- function(selectedListItems, string_taxid) {
  selectedListItems <- stringPOSTConvertENSP(selectedListItems,
                                             string_taxid)$target
  # STRING's API request have a limit at 500.
  if (length(selectedListItems) > STRING_LIMIT)
    selectedListItems <- selectedListItems[1:STRING_LIMIT]
  ids_interactive <- paste(selectedListItems, collapse = "','")
  ids_post <- paste(selectedListItems, collapse = "%0d") 
  return(list(ids_interactive = ids_interactive, ids_post = ids_post))
}

createSTRINGNetworkLegend <- function(edge_meaning) {
  table_nodes <- "<table>
    <tr><td><img src='string_icons/node_known_structure.png' /></td><td>Proteins with known 3D structure (experimental or predicted)</td></tr>
    <tr><td><img src='string_icons/node_unknown_structure_string.png' /></td><td>Proteins with unknown 3D structure</td></tr>
    </table>
    "
  
  if(tolower(edge_meaning) == "evidence") {
    table_edges <- "<table style='width:70%'>
    <th colspan=6>Known Interactions</th>
    <tr>
    <td style='width:5%'><img src='string_icons/edge_experiment.png' ></td><td style='width:20%'>Experimentally determined</td>
    <td style='width:5%'><img src='string_icons/edge_curated_database.png' ></td><td style='width:20%'>From curated databases</td>
    <td></td>
    </tr>
    <th colspan=6>Computationally inferred from gene analysis</th>
    <tr>
    <td style='width:5%'><img src='string_icons/edge_gene_neighborhood.png' ></td><td style='width:20%'>Gene neighborhood</td>
    <td style='width:5%'><img src='string_icons/edge_gene_fusions.png' ></td><td style='width:20%'>Gene fusions</td>
    <td style='width:5%'><img src='string_icons/edge_gene_coocurrence.png' ></td><td style='width:20%'>Gene co-occurrence</td>
    </tr>
    <th colspan=6>Computationally inferred from other sources</th>
    <tr>
    <td style='width:5%'><img src='string_icons/edge_textmining.png' ></td><td style='width:20%'>Text mining</td>
    <td style='width:5%'><img src='string_icons/edge_coexpression.png' ></td><td style='width:20%'>Co-expression</td>
    <td style='width:5%'><img src='string_icons/edge_homology.png' ></td><td style='width:20%'>Protein homology</td>
    </tr>
    </table>
    "
  } else {
    table_edges <- "<table style='width:60%'>
    <th colspan=4>Confidence levels</th>
    <tr>
    <td style='width:8%'><img src='string_icons/edge_confidence_low.png' ></td><td>Low confidence (>=0.150) </td>
    <td style='width:8%'><img src='string_icons/edge_confidence_medium.png' ></td><td>Medium confidence (>=0.400) </td>
    </tr>
    <tr>
    <td style='width:8%'><img src='string_icons/edge_confidence_high.png' ></td><td>High confidence (>=0.700) </td>
    <td style='width:8%'><img src='string_icons/edge_confidence_highest.png' ></td><td>Highest confidence (>=0.900) </td>
    </tr>
    </table>
    "
  }
  
  return(
    renderUI({
      fluidRow(
        column(
          4,
          div(
            h5(strong("Nodes:")),
            HTML(table_nodes)
          )
        ),
        column(
          8,
          div(
            h5(strong("Edges:")),
            HTML(table_edges)
          )
        )
      )
    })
  )
}

createSTRINGNetworkData <- function(ids_post, string_taxid,
                                               type, edges, score) {
  string_link <- createSTRINGLink(ids_post, string_taxid,
                                  type, edges, score)
  string_tsv <- createSTRINGFile(ids_post, string_taxid,
                                 type, edges, score)
  if (strsplit(string_tsv, "\t")[[1]][1] == "Error")
    renderWarning("STRING did not return any valid results.
                  Make sure the input is in ENSP namespace.")
  svg_to_png_js_code <- createSTRINGPNGCode()
  return(list(string_link = string_link, string_tsv = string_tsv,
              svg_to_png_js_code = svg_to_png_js_code))
}

createSTRINGLink <- function(ids_post, string_taxid, type, edges, score) {
  h_open <- curl::new_handle(url="https://string-db.org/api/tsv-no-header/get_link")
  curl::handle_setform(
    h_open,
    identifiers = ids_post,
    species = sprintf("%s", string_taxid),
    network_type = type,
    network_flavor = edges,
    required_score = score,
    caller_identity = "Flame@bib.fleming"
  )
  h_open_curl <- curl::curl_fetch_memory("https://string-db.org/api/tsv-no-header/get_link", h_open)
  string_link <- rawToChar(h_open_curl$content)
  string_link <- trimws(string_link)
  return(string_link)
}

createSTRINGFile <- function(ids_post, string_taxid, type, edges, score) {
  h_tsv <- curl::new_handle(url="https://string-db.org/api/tsv/network")
  curl::handle_setform(
    h_tsv,
    identifiers = ids_post,
    species = sprintf("%s", string_taxid),
    network_type = type,
    network_flavor = edges,
    required_score = score,
    caller_identity = "Flame@bib.fleming"
  )
  h_tsv_curl <- curl::curl_fetch_memory("https://string-db.org/api/tsv/network", h_tsv)
  string_tsv <- rawToChar(h_tsv_curl$content)
  return(string_tsv)
}

createSTRINGPNGCode <- function() {
  return(
    "var svg = document.getElementById('svg_network_image');
    var svgData = new XMLSerializer().serializeToString( svg );
  
    var canvas = document.createElement( 'canvas' );
    var ctx = canvas.getContext( '2d' );
  
    var img = document.createElement( 'img' );
    img.setAttribute( 'src', 'data:image/svg+xml;base64,' + btoa( svgData ) );
  
    img.onload = function() {
      var canvas = document.createElement('canvas');
      canvas.width = img.width;
      canvas.height = img.height;
      var context = canvas.getContext('2d');
      context.drawImage(img, 0, 0);
  
      var a = document.createElement('a');
      a.download = 'network.png';
      a.href = canvas.toDataURL('image/png');
      document.body.appendChild(a);
      a.click();
    }"
  )
}

createSTRINGActionButtons <- function(buttonParameters) {
  string_link <- buttonParameters$string_link
  string_tsv <- buttonParameters$string_tsv
  svg_to_png_js_code <- buttonParameters$svg_to_png_js_code
  
  output$string_export_buttons <- renderUI({
    div(style="margin:10px !important", fluidRow(
      
      actionButton(inputId = 'string_link', label = 'Open in STRING',
                   icon = icon('link'), style='md-flat',
                   onclick = sprintf("window.open('%s', '_blank')", string_link)),
      downloadButton(outputId = 'dnl_tsv_string', label = 'Download Network',
                     icon = icon('download'), style = 'md-flat'),
      actionButton(inputId = 'dnl_png_string', label = 'Export Image',
                   icon = icon('image'),  style = 'md-flat',
                   onclick = svg_to_png_js_code),
    ),
    h5("Perform Enrichment on Network:"),
    fluidRow(
      actionButton(inputId = "string_submit_functional", label="Functional Enrichment", icon=icon("table"), style="md-flat" ),
      actionButton(inputId = "string_submit_literature", label="Literature Enrichment", icon=icon("table"), style="book-open" ),
    ))
  })
  
  output$dnl_tsv_string <- downloadHandler(
    filename = "network.tsv",
    content = function(file) {
      write(string_tsv, file)
    }
  )
}

createSTRINGInteractiveViewer <- function(ids_interactive, string_taxid,
                                          type, edges, score) {
  output$string_viewer <- renderUI({
    tags$div(
      tags$script(sprintf(
        "var proteins = ['%s']; 
        var species = ['%s'];
        var type = ['%s'];
        var edges = ['%s'];
        var score = ['%s'];
        getSTRING('https://string-db.org', {
        'species':species, 
        'identifiers':proteins,
        'network_type':type,
        'network_flavor':edges,
        'required_score':score});",
        ids_interactive, string_taxid, type, edges, score
      )),
      tags$div(id = 'stringEmbedded')
    )
  })
}


getGenesFromSTRINGNetworkTSV <- function(tsv_str_obj) {
  df <- read.table(text = tsv_str_obj, header=T)
  unique_proteins <- unlist(unique(c(
    unlist(df$preferredName_A),
    unlist(df$preferredName_B)
  )))
  return(paste0(unique_proteins, paste="\n", collapse="\n"))
}

sendSTRINGNetworkForEnrichment <- function(category) {
  stringGenes <- getGenesFromSTRINGNetworkTSV(STRINGNetworkData$string_tsv)
  stringList <- buildUserListFromText(paste(
    stringGenes, sep = "\n", collapse = "\n"
  ), sprintf("string_%s", category))
  updateSelectInput(session, sprintf("%s_enrichment_file", category), choices = stringList)
}

