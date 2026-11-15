# =============================================================================
# STRING Network Analysis API Functions
# =============================================================================
#
# Stateless wrappers for STRING-db.org network API calls.
# Used by NetworkAnalysisSession.
#
# Note: Gene ID conversion is now a private method in NetworkAnalysisSession
# (convertToStringIds) - no longer shared with enrichment.
#
# =============================================================================

#' Get STRING interactive network link
#'
#' Calls STRING API to get a shareable link to the network viewer.
#'
#' @param ids_post Character. URL-encoded IDs (%0d separated)
#' @param taxid Numeric. NCBI taxonomy ID
#' @param network_type Character. "functional" or "physical"
#' @param network_flavor Character. "evidence" or "confidence"
#' @param required_score Numeric. Score threshold (150/400/700/900)
#' @return Character. URL to STRING network
string_network_get_link <- function(ids_post, taxid, network_type, network_flavor, required_score) {
  h <- curl::new_handle(url = "https://string-db.org/api/tsv-no-header/get_link")
  curl::handle_setform(
    h,
    identifiers = ids_post,
    species = sprintf("%s", taxid),
    network_type = network_type,
    network_flavor = network_flavor,
    required_score = as.character(required_score),
    caller_identity = "Flame@bib.fleming"
  )
  response <- curl::curl_fetch_memory("https://string-db.org/api/tsv-no-header/get_link", h)
  trimws(rawToChar(response$content))
}

#' Get STRING network data as TSV
#'
#' Calls STRING API to get network interaction data in TSV format.
#'
#' @param ids_post Character. URL-encoded IDs (%0d separated)
#' @param taxid Numeric. NCBI taxonomy ID
#' @param network_type Character. "functional" or "physical"
#' @param network_flavor Character. "evidence" or "confidence"
#' @param required_score Numeric. Score threshold (150/400/700/900)
#' @return Character. TSV-formatted network data
string_network_get_data <- function(ids_post, taxid, network_type, network_flavor, required_score) {
  h <- curl::new_handle(url = "https://string-db.org/api/tsv/network")
  curl::handle_setform(
    h,
    identifiers = ids_post,
    species = sprintf("%s", taxid),
    network_type = network_type,
    network_flavor = network_flavor,
    required_score = as.character(required_score),
    caller_identity = "Flame@bib.fleming"
  )
  response <- curl::curl_fetch_memory("https://string-db.org/api/tsv/network", h)
  rawToChar(response$content)
}

#' Parse genes from STRING network TSV
#'
#' Extracts unique gene names from STRING network TSV data.
#'
#' @param tsv_string Character. Raw TSV data from STRING API
#' @return Character vector. Unique gene names from network
string_network_parse_genes <- function(tsv_string) {
  df <- read.table(text = tsv_string, header = TRUE)
  unique_proteins <- unique(c(
    as.character(df$preferredName_A),
    as.character(df$preferredName_B)
  ))
  unique_proteins[!is.na(unique_proteins) & unique_proteins != ""]
}

#' Generate JavaScript code for PNG export
#'
#' Creates JavaScript code to convert the STRING SVG network to PNG.
#'
#' @return Character. JavaScript code for SVG-to-PNG conversion
string_network_png_export_js <- function() {
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
}
