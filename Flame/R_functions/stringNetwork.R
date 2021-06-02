
create_string_network <- function(dataset, organism, type, edges, score, output) {
  
  #1. Get input IDs from dataset
  input_ids_initial<-unlist(file_data[file_names==dataset][[1]])
  
  #2. Convert them to ENSEMBL protein IDs for STRING
  #2.1 get the gprofiler ID from the organismsFromFile global
  gconvert_organism <- organismsFromFile[organismsFromFile$print_name == organism,]$gprofiler_ID
  #2.2 Run gconvert
  gconv <- gconvert(input_ids_initial, organism = gconvert_organism, target = "ENSP", numeric_ns = "", mthreshold = Inf, filter_na = T)
  #2.3 create a vector of converted ids and remove 'nan', if they exist
  
  
  converted_ids<-c()
  for (i in 1:length(input_ids_initial))
  {
    inputGenes <- gconv[grepl(input_ids_initial[i], gconv$input),]
    converted_ids[i] <- inputGenes$target[1] # in case of more than one matches to target namespace
  }
  converted_ids<-converted_ids[grep('nan', converted_ids, invert=T)] #remove 'nan' fields, otherwise the STRING call WILL crash!
  
  #STRING's API request have a limit at 500.  For more than 500 input IDs, the export/download API calls WILL NOT WORK
  if (length(converted_ids)>STRING_LIMIT)
  {
    converted_ids<-converted_ids[1:STRING_LIMIT]
  }

  #3. Create STRING input IDs: 
  #3.1 get the tax ID from the organismsFromFile global
  string_taxid <- organismsFromFile[organismsFromFile$print_name == organism,]$Taxonomy_ID
  #3.2 create vector of STRING-DB IDs, formatted as taxid.ENSP00XXXXX
  string_ids<-paste(string_taxid, ".", converted_ids, sep="")
  #3.3 create strings for input in the string API
  #3.3.1 input for interactive viewer
  ids_interactive <- paste(unlist(string_ids), sep = "','", collapse = "','")
  #3.3.2 input for GET & POST requests 
  ids_post <- paste(unlist(string_ids), sep = "%0d", collapse = "%0d") 
  
  #4 Perform all POST requests to create network objects

  #4.1 Create and render Interactive Network Viewer, using STRING's 'getSTRING' javascript POST API 
  #this uses the getSTRING function, which builds and interactive window viewer
  output$string_viewer <- renderUI ({
    
    
    tags$div(style="border: 2px solid black;",
             tags$script(sprintf("
        var proteins = ['%s']; 
        var species = ['%s'];
        var type = ['%s'];
        var edges = ['%s'];
        var score = ['%s'];
        getSTRING('https://string-db.org', {
        'species':species, 
        'identifiers':proteins,
        'network_type':type,
        'network_flavor':edges,
        'required_score':score});", ids_interactive, string_taxid, type, edges, score) 
             ),
             tags$div(id = 'stringEmbedded')
    )
  })
  
  
  #4.2 create API POST calls for the export/download buttons
  #these use the CURL library, because for some reason, the built-in httr POST request wouldn't work
  
  #4.2.1 POST Call to get the url for the Open-in-String button
  # 'get_link' returns a file (json, tsv or XML) containing the URL to the string website
  #we call it with a curl POST request and decode it
  h_open <- new_handle(url="https://string-db.org/api/tsv-no-header/get_link")
  handle_setform(h_open,
                 identifiers=ids_post,
                 species=sprintf("%s",string_taxid),
                 network_type=type,
                 network_flavor=edges,
                 required_score=score,
                 caller_identity = "Flame@bib.fleming"
  )
  h_open_curl <- curl_fetch_memory("https://string-db.org/api/tsv-no-header/get_link", h_open)
  string_link <- rawToChar(h_open_curl$content)
  string_link <- trimws(string_link)
  
  
  
  #4.2.2 POST request to create a TSV file for network download
  # 'tsv/network' returns the network in tab-delimited format
  #we call it with a curl POST request and decode it.
  #this will be saved as output through a downloadButton
  h_tsv <- new_handle(url="https://string-db.org/api/tsv/network")
  handle_setform(h_tsv,
                 identifiers=ids_post,
                 species=sprintf("%s",string_taxid),
                 network_type=type,
                 network_flavor=edges,
                 required_score=score,
                 caller_identity = "Flame@bib.fleming"
  )
  h_tsv_curl <- curl_fetch_memory("https://string-db.org/api/tsv/network", h_tsv)
  string_tsv <- rawToChar(h_tsv_curl$content)
  
  #4.2.3 Create an image of the network from the canvas
  #This is pure javascript. It converts the SVG element of the network viewer
  #to a canvas object and then opens as save file dialog to save
  #this canvas as an image
  #this is saved to 'svg_to_png_js_code', which is then used as an onclick
  #function in the 'Download image' actionButton
  
  
  svg_to_png_js_code="var svg = document.getElementById('svg_network_image');
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
  
  
  
  #5. Create Network Legend and download/export buttons
  
  #5.1 Create and render box with submitted parameters
  
  # output$networkParameters <- renderUI({
  #   p(strong("Organism: "), organism, " | ", strong("Network Type: "), type, " | ", strong("Meaning of network edges:"), edges, " | ", strong("Interaction score cut-off: "), as.numeric(score)/1000)
  # })
  param_string <- "" # String variable for execution parameters to be printed
  param_string <- paste( "Organism: ", organism, "\nNetwork Type: ", type, "\nMeaning of network edges:", edges, "\nInteraction score cut-off: ", as.numeric(score)/1000, sep ="")
  output$networkParameters <- renderText(param_string)
  
  #5.2 create and render figure legend
  #this uses the create_network_legend function defined below
  output$string_legend<- create_network_legend(edges)
  
  
  #5.3 render download/export buttons
  output$string_export_buttons<-renderUI({
    fluidRow(
      actionButton(inputId = 'string_link', label = 'Open in STRING', icon = icon('link'), style='md-flat', onclick = sprintf("window.open('%s', '_blank')", string_link)),
      downloadButton(outputId = 'dnl_tsv_string', label = 'Download Network', icon = icon('download'),  style='md-flat'),
      actionButton(inputId = 'dnl_png_string', label = 'Export Image', icon = icon('image'),  style='md-flat', onclick=svg_to_png_js_code)
    )
  })
  
  #5.5 code for the download buttons
  
  #download file dialog for saving a tsv text
  output$dnl_tsv_string <-downloadHandler(
    filename = "network.tsv",
    content = function(file) {
      write(string_tsv, file)
    }
  )


  
  # # The old GET API urls are shown below.  These DO NOT work for large (>400 IDs) datasets
  # download_tsv <- sprintf("https://string-db.org/api/tsv/network?identifiers=%s&species=%s&network_type=%s&network_flavor=%s&required_score=%s", ids_post,  string_taxid, type, edges, score)
  # download_png <- sprintf("https://string-db.org/api/image/network?identifiers=%s&species=%s&network_type=%s&network_flavor=%s&required_score=%s", ids_post,  string_taxid, type, edges, score)
  # # A small GET API call to get the open in STRING URL:
  # # 'get_link' returns a file (json, tsv or XML) containing the URL to the string website
  # # we call it with a GET request and decode it
  # string_link_raw <- GET(sprintf("https://string-db.org/api/tsv-no-header/get_link?identifiers=%s&species=%s&network_type=%s&network_flavor=%s&required_score=%s", ids_post,  string_taxid, type, edges, score))
  # string_link <- trimws(rawToChar(content(string_link_raw, "raw")))
  #
  # output$string_export_buttons<-renderUI({
  #   fluidRow(
  #     actionButton(inputId = 'string_link', label = 'Open in STRING', icon = icon('link'), style='md-flat', onclick = sprintf("window.open('%s', '_blank')", string_link)),
  #     actionButton(inputId = 'dnl_tsv_string', label = 'Download Network', icon = icon('download'),  style='md-flat', onclick = sprintf("window.open('%s', '_blank')", download_tsv)),
  #     actionButton(inputId = 'dnl_png_string', label = 'Export Image', icon = icon('image'),  style='md-flat', onclick = sprintf("window.open('%s', '_blank')", download_png))
  #   )
  # })
  
  
}






##Method to create legend for the network viewer-####

create_network_legend <- function(edge_meaning) {
  
  table_nodes="<table>
    <tr><td><img src='string_icons/node_known_structure.png' /></td><td>Proteins with known 3D structure (experimental or predicted)</td></tr>
    <tr><td><img src='string_icons/node_unknown_structure_string.png' /></td><td>Proteins with unknown 3D structure</td></tr>
    </table>
    "
  
  if(tolower(edge_meaning) == "evidence")
  {
    table_edges="<table style='width:70%'>
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
    
  }
  else
  {
    table_edges="<table style='width:60%'>
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
  legend <-renderUI({
    
    fluidRow(
      column(4,
             div(
               h5(strong("Nodes:")),
               HTML(table_nodes)
             )
      ),
      column(8,
             div(
               h5(strong("Edges:")),
               HTML(table_edges)
             )
      )
    )
  })
  
  
  return(legend)
  
}
