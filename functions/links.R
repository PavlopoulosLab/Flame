attachDBLinks <- function() { # Transfac HPA CORUMLinks, unavailable
  attachInterProLinks()
  attachPFAMLinks()
  attachUniProtLinks()
  attachGOLinks()
  attachKEGGLinks()
  attachReactomeLinks()
  attachWikiPathsLinks()
  attachDOLinks()
  attachBTOLinks()
  attachMirBaseLinks()
  attachHPOLinks()
}

attachInterProLinks <- function() {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^INTERPRO$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^INTERPRO$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.ebi.ac.uk/interpro/entry/InterPro/",
        linksVector,
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachPFAMLinks <- function() {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^PFAM$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^PFAM$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.ebi.ac.uk/interpro/entry/pfam/",
        linksVector,
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachUniProtLinks <- function() {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^UNIPROT$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^UNIPROT$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.uniprot.org/keywords/",
        linksVector,
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachGOLinks <- function() {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^GO:", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^GO:", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.ebi.ac.uk/QuickGO/term/",
        linksVector,
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachKEGGLinks <- function() {
  linkOrganism <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$gprofiler_ID == currentOrganism, ]$KEGG
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^KEGG$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^KEGG$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
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
    enrichmentResults[[currentType_Tool]][grepl(
      "^REAC$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^REAC$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
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
    enrichmentResults[[currentType_Tool]][grepl(
      "^WP$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^WP$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.wikipathways.org/index.php/Pathway:",
        gsub("WP:", "", linksVector),
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachDOLinks <- function() {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^DO$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^DO$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='http://www.informatics.jax.org/disease/",
        linksVector,
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachBTOLinks <- function() {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^BTO$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^BTO$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.ebi.ac.uk/ols/ontologies/bto/terms?iri=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FBTO_",
        gsub("BTO:", "", linksVector),
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachMirBaseLinks <- function() {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^MIRNA$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^MIRNA$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://www.mirbase.org/textsearch.shtml?q=",
        gsub("MIRNA:", "", linksVector),
        "&submit=submit' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachHPOLinks <- function() {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^HP$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^HP$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://mseqdr.org/hpo_browser.php?",
        gsub("HP:", "", linksVector),
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}

attachLiteratureDBLinks <- function() {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^PUBMED$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    enrichmentResults[[currentType_Tool]][grepl(
      "^PUBMED$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://pubmed.ncbi.nlm.nih.gov/",
        gsub("PMID:", "", linksVector),
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}
