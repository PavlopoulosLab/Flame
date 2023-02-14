attachTextMiningDBLinks <- function(df) {
  df$ID_noLINKS <- df$ID 
  df$ID[grep("^ENS", df$ID_noLINKS)] <- 
    paste0(
      "<a href='",
      sprintf('https://www.ensembl.org/id/%s',
              df$ID_noLINKS[grep("^ENS", df$ID_noLINKS)]),
      "' target = '_blank'>",
      df$ID_noLINKS[grep("^ENS", df$ID_noLINKS)],
      "</a>"
    )
  df$ID[grep("^hsa-", df$ID_noLINKS)] <-
    paste0(
      "<a href='",
      sprintf('https://www.mirbase.org/textsearch.shtml?q=%s',
              df$ID_noLINKS[grep("^hsa-", df$ID_noLINKS)]),
      "' target = '_blank'>",
      df$ID_noLINKS[grep("^hsa-", df$ID_noLINKS)],
      "</a>"
    )
  return(df)
}

attachVariantTableLinks <- function(df) {
  df$rs_id_noLinks <- df$rs_id
  df$ensgs_noLinks <- df$ensgs
  df$ensgs[grep("^ENS", df$ensgs_noLinks)] <- 
    paste0(
      "<a href='",
      sprintf('https://www.ensembl.org/id/%s',
              df$ensgs_noLinks[grep("^ENS", df$ensgs_noLinks)]),
      "' target = '_blank'>",
      df$ensgs_noLinks[grep("^ENS", df$ensgs_noLinks)],
      "</a>"
    )
  df$rs_id[grep("^rs", df$rs_id_noLinks)] <- 
    paste0(
      "<a href='",
      sprintf('https://www.ncbi.nlm.nih.gov/snp/%s',
              df$rs_id_noLinks[grep("^rs", df$rs_id_noLinks)]),
      "' target = '_blank'>",
      df$rs_id_noLinks[grep("^rs", df$rs_id_noLinks)],
      "</a>"
    )
  df <- subset(df, select = -c(rs_id_noLinks, ensgs_noLinks))
  return(df)
  
}

attachDBLinks <- function() { # Transfac HPA CORUMLinks, unavailable
  attachLinks("GO", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = NULL)
  attachLinks("INTERPRO", "https://www.ebi.ac.uk/interpro/entry/InterPro/")
  attachLinks("PFAM", "https://www.ebi.ac.uk/interpro/entry/pfam/")
  attachLinks("UNIPROT", "https://www.uniprot.org/keywords/")
  attachLinks("PANTHER", "http://www.pantherdb.org/pathway/pathDetail.do?clsAccession=")
  attachLinks("DO", "http://www.informatics.jax.org/disease/")
  attachLinks("WBP", "https://wormbase.org/species/all/phenotype/")
  attachLinks("WBBT", "https://wormbase.org/species/all/anatomy_term/")
  attachLinks("MGI", "https://www.informatics.jax.org/vocab/mp_ontology/")
  attachLinks("REAC", "https://reactome.org/content/detail/")
  attachLinks("WP", "https://www.wikipathways.org/index.php/Pathway:")
  attachLinks("BTO", "https://www.ebi.ac.uk/ols/ontologies/bto/terms?iri=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FBTO_", gSub = "BTO:")
  attachLinks("MIRNA", "https://www.mirbase.org/textsearch.shtml?q=", gSub = "MIRNA:")
  attachLinks("HP", "https://mseqdr.org/hpo_browser.php?", gSub = "HP:")
  attachLinks("ORPHA", "https://www.orpha.net/consor/cgi-bin/OC_Exp.php?Lng=GB&Expert=", gSub = "ORPHA:")
  
  attachKEGGLinks()
}

attachLinks <- function(sourceId, url, stopChar = "$", gSub = NULL) {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      paste0("^", sourceId, stopChar), enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    gSubLinksVector <- linksVector
    if (!is.null(gSub))
      gSubLinksVector <- gsub(gSub, "", linksVector)
    enrichmentResults[[currentType_Tool]][grepl(
      paste0("^", sourceId, stopChar), enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='", url, gSubLinksVector, "' target='_blank'>",
        linksVector, "</a>"
      )
  }
}

attachKEGGLinks <- function() {
  tempEnrichmentDF <- 
    enrichmentResults[[currentType_Tool]][grepl(
      "^KEGG$", enrichmentResults[[currentType_Tool]]$Source
    ), ][, c("Source", "Positive Hits", "Term_ID")]
  
  if (nrow(tempEnrichmentDF) > 0) {
    shortName <- ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name
    conversionTable <- createEntrezAccConversionTable(tempEnrichmentDF, shortName)
    if (!is.null(conversionTable)) {
      tempEnrichmentDFWithEntrezAcc <-
        convertPositiveHitsToEntrezAcc(tempEnrichmentDF, conversionTable)
      
      linkOrganism <- ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$kegg_name
      linksVector <- tempEnrichmentDFWithEntrezAcc$Term_ID
      enrichmentResults[[currentType_Tool]][grepl(
        "^KEGG$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
        paste0(
          "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
          gsub("KEGG:|map", linkOrganism, linksVector), 
          "+", gsub(",", "+", tempEnrichmentDFWithEntrezAcc$`Positive Hits EntrezAcc`),
          "' target='_blank'>",
          linksVector,
          "</a>"
        )
    }
  }
}

createEntrezAccConversionTable <- function(tempEnrichmentDF, shortName) {
  inputToConvert <- unique(unlist(strsplit(paste(
    tempEnrichmentDF[grepl("^KEGG$", tempEnrichmentDF$Source), ]$`Positive Hits`,
    collapse = ","), ",")))
  if (isUnconvertedENSPNamespace())
    inputToConvert <- sapply(strsplit(inputToConvert , "\\."), "[[", 2)
  
  conversionTable <- calculateConversionTable(inputToConvert, shortName)
  
  if (isUnconvertedENSPNamespace())
    conversionTable$input <- paste0(currentOrganism, ".", conversionTable$input)
  
  if (!is.null(conversionTable)) {
    conversionTable <- dplyr::distinct(conversionTable[, c("input", "target")])
    colnames(conversionTable)[1] <- "Positive Hits"
  }
  return(conversionTable)
}

isUnconvertedENSPNamespace <- function() {
  return(
    currentEnrichmentTool == "aGOtool" &&
      input$functional_enrichment_inputConversion == "Converted input names"
  )
}

calculateConversionTable <- function(inputToConvert, shortName) {
  if (currentNamespace == "ENTREZGENE_ACC" &&
      input$functional_enrichment_inputConversion == "Converted input names")
    conversionTable <- data.frame(
      "input" = inputToConvert,
      "target" = inputToConvert
    )
  else
    conversionTable <- gprofiler2::gconvert(inputToConvert,
                                            organism = shortName,
                                            target = "ENTREZGENE_ACC")
  return(conversionTable)
}

convertPositiveHitsToEntrezAcc <- function(tempEnrichmentDF, conversionTable) {
  tempEnrichmentDF <- tidyr::separate_rows(tempEnrichmentDF,
                                           `Positive Hits`, sep = ",")
  tempEnrichmentDF <- plyr::join(tempEnrichmentDF, conversionTable,
                                  type = "left", by = "Positive Hits")
  tempEnrichmentDF <-
    tempEnrichmentDF[, !(names(tempEnrichmentDF) %in% c("Positive Hits", "Source"))]
  colnames(tempEnrichmentDF)[match("target", colnames(tempEnrichmentDF))] <-
    "Positive Hits EntrezAcc"
  tempEnrichmentDF <- tempEnrichmentDF %>%
    dplyr::group_by(Term_ID) %>%
    dplyr::mutate(`Positive Hits EntrezAcc` = paste(`Positive Hits EntrezAcc`,
                                                    collapse = ","))
  tempEnrichmentDF <- dplyr::distinct(tempEnrichmentDF)
  tempEnrichmentDF <- plyr::join(enrichmentResults[[currentType_Tool]][grepl(
    "^KEGG$", enrichmentResults[[currentType_Tool]]$Source), ],
    tempEnrichmentDF, type = "left", by = "Term_ID")
  return(tempEnrichmentDF)
}

attachWebgestaltLinks <- function(links) {
  enrichmentResults[[currentType_Tool]]$Term_ID_noLinks <<- 
    enrichmentResults[[currentType_Tool]]$Term_ID
  enrichmentResults[[currentType_Tool]]$Term_ID <<-
    paste0(
      "<a href='",
      links,
      "' target='_blank'>",
      enrichmentResults[[currentType_Tool]]$Term_ID,
      "</a>"
    )
}
