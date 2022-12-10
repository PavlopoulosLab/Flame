attachDBLinks <- function(organism) { # Transfac HPA CORUMLinks, unavailable
  linkOrganism <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$gprofiler_ID == organism, ]$KEGG
  attachInterProLinks()
  attachPFAMLinks()
  attachUniProtLinks()
  attachGOLinks()
  attachKEGGLinks(organism, linkOrganism)
  attachReactomeLinks()
  attachWikiPathsLinks()
  attachDOLinks()
  attachBTOLinks()
  attachMirBaseLinks()
  attachHPOLinks()
}

attachInterProLinks <- function() {
  linksVector <-
    functionalEnrichmentResult[grepl("^INTERPRO$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^INTERPRO$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^PFAM$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^PFAM$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^UNIPROT$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^UNIPROT$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^GO:", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^GO:", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^KEGG$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^KEGG$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^REAC$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^REAC$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^WP$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^WP$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^DO$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^DO$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^BTO$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^BTO$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^MIRNA$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^MIRNA$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    functionalEnrichmentResult[grepl("^HP$", functionalEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    functionalEnrichmentResult[grepl("^HP$", functionalEnrichmentResult$Source), ]$Term_ID <<-
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
    literatureEnrichmentResult[grepl("^PUBMED$", literatureEnrichmentResult$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    literatureEnrichmentResult[grepl("^PUBMED$", literatureEnrichmentResult$Source), ]$Term_ID <<-
      paste0(
        "<a href='https://pubmed.ncbi.nlm.nih.gov/",
        gsub("PMID:", "", linksVector),
        "' target='_blank'>",
        linksVector,
        "</a>"
      )
  }
}
