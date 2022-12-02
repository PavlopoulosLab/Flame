attachDBLinks <- function(organism) {
  linkOrganism <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$gprofiler_ID == organism, ]$KEGG
  attachGOLinks()
  attachKEGGLinks(organism, linkOrganism)
  attachReactomeLinks()
  attachWikiPathsLinks()
  # attachTransfacLinks()
  attachMirBaseLinks()
  # attachHPALinks()
  # attachCORUMLinks()
  # attachHPOLinks()
}

attachGOLinks <- function() {
  linksVector <-
    gprofilerTransformedResult[grepl("^GO:", gprofilerTransformedResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    gprofilerTransformedResult[grepl("^GO:", gprofilerTransformedResult$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.ebi.ac.uk/QuickGO/term/",
        linksVector,
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachKEGGLinks <- function(organism, linkOrganism) {
  linksVector <-
    gprofilerTransformedResult[grepl("KEGG$", gprofilerTransformedResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    gprofilerTransformedResult[grepl("KEGG$", gprofilerTransformedResult$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.genome.jp/kegg-bin/show_pathway?",
        gsub("KEGG:", linkOrganism, linksVector),
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachReactomeLinks <- function() {
  linksVector <-
    gprofilerTransformedResult[grepl("REAC$", gprofilerTransformedResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    gprofilerTransformedResult[grepl("REAC$", gprofilerTransformedResult$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://reactome.org/content/detail/",
        gsub("REAC:", "", linksVector),
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachWikiPathsLinks <- function() {
  linksVector <-
    gprofilerTransformedResult[grepl("WP$", gprofilerTransformedResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    gprofilerTransformedResult[grepl("WP$", gprofilerTransformedResult$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.wikipathways.org/index.php/Pathway:",
        gsub("WP:", "", linksVector),
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachTransfacLinks <- function() {
  
}

attachMirBaseLinks <- function() {
  linksVector <-
    gprofilerTransformedResult[grepl("MIRNA$", gprofilerTransformedResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    gprofilerTransformedResult[grepl("MIRNA$", gprofilerTransformedResult$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.mirbase.org/textsearch.shtml?q=",
        gsub("MIRNA:", "", linksVector),
        "&submit=submit' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachHPALinks <- function() {
  
}

attachCORUMLinks <- function() {
  
}

attachHPOLinks <- function() {
  
}
