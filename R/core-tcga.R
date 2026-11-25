# TCGA as a source of gene lists, through the cBioPortal web API (TCGA PanCancer Atlas studies).
#
#   tcga_studies()                       the 32 cancer types
#   tcga_groups(study)                   clinical attributes that split the tumours into groups
#   tcga_genes(study, query, ...)        genes: "mutated", "cna", "coexpression", "de" or "normal"
#
# "normal" (tumour vs adjacent normal tissue) is not on cBioPortal: it reads a table precomputed
# from the GDC STAR counts by data-raw/build_tcga_tumor_normal.R (inst/extdata/tcga_tumor_normal.rds).
#
# Results are lists in the shape the gene-list dialogs use: genes (gene, name, value, note),
# valueLabel, source, term, link, citation, organismTaxId (always 9606).

CBIOPORTAL_API <- "https://www.cbioportal.org/api"
TCGA_SUFFIX <- "_tcga_pan_can_atlas_2018"
TCGA_CITATION <- paste(
  "TCGA PanCancer Atlas data (Hoadley et al. 2018, Cell 173:291) through the cBioPortal API",
  "(Cerami et al. 2012, Cancer Discov 2:401; de Bruijn et al. 2023, Cancer Res 83:3861).")

.tcga_cache <- new.env()
.tcga_cached <- function(key, expr) {
  hit <- .tcga_cache[[key]]
  if (!is.null(hit)) return(hit)
  val <- expr
  if (length(ls(.tcga_cache)) > 60) rm(list = ls(.tcga_cache), envir = .tcga_cache)
  .tcga_cache[[key]] <- val
  val
}

#' cBioPortal API call (JSON in, simplified JSON out).
.cbio <- function(path, body = NULL, query = NULL, timeout = 180) {
  url <- paste0(CBIOPORTAL_API, path)
  json <- if (!is.null(body)) jsonlite::toJSON(body, auto_unbox = TRUE)
  # cBioPortal's gateway sometimes answers 502/503/504 to heavy requests that succeed when repeated.
  for (attempt in 1:3) {
    resp <- tryCatch(
      if (is.null(body)) httr::GET(url, query = query, httr::accept_json(), httr::timeout(timeout))
      else httr::POST(url, query = query, body = json, httr::content_type_json(), httr::accept_json(), httr::timeout(timeout)),
      error = function(e) stop("cBioPortal could not be reached (", conditionMessage(e), "). Try again later."))
    if (!httr::status_code(resp) %in% c(502L, 503L, 504L)) break
    Sys.sleep(1.5 * attempt)
  }
  code <- httr::status_code(resp)
  if (code >= 500) stop(sprintf("cBioPortal is not available right now (HTTP %d). This is a problem at cBioPortal, not with your input; please try again later.", code))
  if (code >= 400) stop(sprintf("cBioPortal rejected the request (HTTP %d).", code))
  jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
}

#' The TCGA PanCancer Atlas studies on cBioPortal (one per cancer type).
tcga_studies <- function() {
  .tcga_cached("studies", {
    s <- .cbio("/studies", query = list(keyword = sub("^_", "", TCGA_SUFFIX), projection = "SUMMARY"))
    s <- s[endsWith(s$studyId, TCGA_SUFFIX), , drop = FALSE]
    s <- s[order(s$name), , drop = FALSE]
    lapply(seq_len(nrow(s)), function(i) list(
      id = s$studyId[i], code = toupper(sub(TCGA_SUFFIX, "", s$studyId[i], fixed = TRUE)),
      name = sub("\\s*\\(TCGA, PanCancer Atlas\\)\\s*$", "", s$name[i]), samples = s$allSampleCount[i],
      normals = .tcga_tn()$cancers[[toupper(sub(TCGA_SUFFIX, "", s$studyId[i], fixed = TRUE))]]$n$normal %||% 0L))
  })
}

.tcga_study <- function(study) {
  study <- as.character(study %||% "")[1]
  st <- Filter(function(s) identical(s$id, study), tcga_studies())
  if (!length(st)) stop("Choose one of the TCGA cancer types.")
  st[[1]]
}

.gene_names <- function(entrez) {
  out <- rep("", length(entrez))
  if (!length(entrez) || !requireNamespace("org.Hs.eg.db", quietly = TRUE)) return(out)
  nm <- tryCatch(suppressMessages(AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db, as.character(entrez), "GENENAME", "ENTREZID")),
                 error = function(e) NULL)
  if (is.null(nm)) return(out)
  out <- unname(as.character(nm)); out[is.na(out)] <- ""
  out
}

.entrez_of <- function(symbol) {
  symbol <- toupper(trimws(as.character(symbol %||% "")))[1]
  if (!grepl("^[A-Z0-9][A-Z0-9.\\-]{0,30}$", symbol)) stop("Type a gene symbol, e.g. ESR1.")
  g <- tryCatch(.cbio(paste0("/genes/", utils::URLencode(symbol, reserved = TRUE))), error = function(e) NULL)
  if (is.null(g$entrezGeneId)) stop("cBioPortal does not know the gene ", symbol, ".")
  list(entrez = as.integer(g$entrezGeneId), symbol = g$hugoGeneSymbol %||% symbol)
}

.symbols_of <- function(entrez) {
  entrez <- unique(as.integer(entrez))
  if (!length(entrez)) return(data.frame(entrezGeneId = integer(0), hugoGeneSymbol = character(0)))
  g <- .cbio("/genes/fetch", body = as.list(as.character(entrez)), query = list(geneIdType = "ENTREZ_GENE_ID", projection = "SUMMARY"))
  g[, c("entrezGeneId", "hugoGeneSymbol")]
}

.sample_list <- function(id) {
  tryCatch(as.character(.cbio(paste0("/sample-lists/", id))$sampleIds), error = function(e) character(0))
}

.tcga_result <- function(st, genes, valueLabel, what, extra = list(), citation = TCGA_CITATION,
                         link = paste0("https://www.cbioportal.org/study/summary?id=", st$id)) {
  c(list(genes = genes, valueLabel = valueLabel, organismTaxId = 9606L, ranked = TRUE,
         source = paste0("TCGA ", st$code, " \u00b7 ", what), term = paste0("TCGA ", st$code, " ", what),
         link = link, citation = citation, total = length(genes)), extra)
}

.gene_records <- function(gene, name, value, note) {
  lapply(seq_along(gene), function(i) list(gene = gene[i], name = name[i], value = value[i], note = note[i]))
}

#' Genes with somatic mutations, by share of the sequenced tumours.
tcga_mutated_genes <- function(study, max = 3000L) {
  st <- .tcga_study(study)
  m <- .tcga_cached(paste("mut", st$id), .cbio("/mutated-genes/fetch", body = list(studyIds = list(st$id))))
  if (!NROW(m)) stop("No mutation data for this cancer type.")
  pct <- 100 * m$numberOfAlteredCases / pmax(m$numberOfProfiledCases, 1)
  o <- order(-pct, m$hugoGeneSymbol)[seq_len(min(nrow(m), max))]
  m <- m[o, , drop = FALSE]; pct <- pct[o]
  .tcga_result(st, .gene_records(m$hugoGeneSymbol, .gene_names(m$entrezGeneId), round(pct, 2),
                                 sprintf("%d of %d tumours", m$numberOfAlteredCases, m$numberOfProfiledCases)),
               "% of tumours mutated", "mutated genes",
               list(note = "Long genes (TTN, MUC16, OBSCN…) are often mutated by chance; consider filtering them or using a driver list."))
}

#' Genes amplified or deeply deleted (GISTIC), by share of the profiled tumours.
tcga_cna_genes <- function(study, direction = "amp", max = 3000L) {
  st <- .tcga_study(study)
  direction <- if (identical(direction, "del")) "del" else "amp"
  m <- .tcga_cached(paste("cna", st$id), .cbio("/cna-genes/fetch", body = list(studyIds = list(st$id))))
  if (!NROW(m)) stop("No copy-number data for this cancer type.")
  m <- m[m$alteration == if (direction == "amp") 2L else -2L, , drop = FALSE]
  pct <- 100 * m$numberOfAlteredCases / pmax(m$numberOfProfiledCases, 1)
  o <- order(-pct, m$hugoGeneSymbol)[seq_len(min(nrow(m), max))]
  m <- m[o, , drop = FALSE]; pct <- pct[o]
  what <- if (direction == "amp") "amplified genes" else "deleted genes"
  .tcga_result(st, .gene_records(m$hugoGeneSymbol, .gene_names(m$entrezGeneId), round(pct, 2),
                                 sprintf("%d of %d tumours \u00b7 %s", m$numberOfAlteredCases, m$numberOfProfiledCases, m$cytoband %||% "")),
               paste0("% of tumours ", if (direction == "amp") "amplified" else "deleted"), what)
}

#' Genes co-expressed with one gene across the tumours (Spearman, RNA-seq).
tcga_coexpressed <- function(study, gene, direction = "positive", minCorrelation = 0.4, max = 3000L) {
  st <- .tcga_study(study)
  g <- .entrez_of(gene)
  minCorrelation <- min(max(suppressWarnings(as.numeric(minCorrelation))[1] %||% 0.4, 0.1), 0.95)
  if (!is.finite(minCorrelation)) minCorrelation <- 0.4
  prof <- paste0(st$id, "_rna_seq_v2_mrna")
  sl <- if (length(.sample_list(prof))) prof else paste0(st$id, "_all")
  co <- .tcga_cached(paste("coexp", st$id, g$entrez), .cbio("/molecular-profiles/co-expressions/fetch",
    body = list(entrezGeneId = g$entrez, sampleListId = sl),
    query = list(molecularProfileIdA = prof, molecularProfileIdB = prof, threshold = 0.1)))
  if (!NROW(co)) stop("No co-expression data for ", g$symbol, " in this cancer type.")
  co$entrez <- suppressWarnings(as.integer(co$geneticEntityId))
  co$q <- stats::p.adjust(co$pValue, "BH")
  rho <- co$spearmansCorrelation
  keep <- switch(direction, negative = rho <= -minCorrelation, both = abs(rho) >= minCorrelation, rho >= minCorrelation)
  co <- co[keep & co$q <= 0.05 & !is.na(co$entrez) & co$entrez != g$entrez, , drop = FALSE]
  co <- co[order(-abs(co$spearmansCorrelation)), , drop = FALSE][seq_len(min(nrow(co), max)), , drop = FALSE]
  sym <- .symbols_of(co$entrez)
  co$gene <- sym$hugoGeneSymbol[match(co$entrez, sym$entrezGeneId)]
  co <- co[!is.na(co$gene), , drop = FALSE]
  .tcga_result(st, .gene_records(co$gene, .gene_names(co$entrez), round(co$spearmansCorrelation, 3),
                                 sprintf("q = %s", formatC(co$q, format = "g", digits = 2))),
               paste("Spearman correlation with", g$symbol), paste("genes co-expressed with", g$symbol))
}

#' Clinical attributes with a few categories, to split the tumours into two groups.
tcga_groups <- function(study) {
  st <- .tcga_study(study)
  .tcga_cached(paste("groups", st$id), {
    attrs <- .cbio(paste0("/studies/", st$id, "/clinical-attributes"))
    attrs <- attrs[attrs$datatype == "STRING", , drop = FALSE]
    counts <- .cbio("/clinical-data-counts/fetch", body = list(
      attributes = lapply(attrs$clinicalAttributeId, function(a) list(attributeId = a)),
      studyViewFilter = list(studyIds = list(st$id))))
    out <- list()
    for (i in seq_len(NROW(counts))) {
      cnt <- counts$counts[[i]]
      if (!NROW(cnt)) next
      cnt <- cnt[!toupper(cnt$value) %in% c("NA", "[NOT AVAILABLE]", "UNKNOWN", ""), , drop = FALSE]
      if (nrow(cnt) < 2 || nrow(cnt) > 20 || sum(cnt$count >= 5) < 2) next
      a <- attrs[attrs$clinicalAttributeId == counts$attributeId[i], , drop = FALSE]
      cnt <- cnt[order(-cnt$count), , drop = FALSE]
      out[[length(out) + 1]] <- list(id = counts$attributeId[i], name = a$displayName[1] %||% counts$attributeId[i],
                                     patient = isTRUE(a$patientAttribute[1]),
                                     values = lapply(seq_len(nrow(cnt)), function(j) list(value = cnt$value[j], count = cnt$count[j])))
    }
    pri <- c("SUBTYPE", "AJCC_PATHOLOGIC_TUMOR_STAGE", "TUMOR_TYPE", "CANCER_TYPE_DETAILED", "SEX", "OS_STATUS", "PFS_STATUS")
    rank <- match(vapply(out, `[[`, "", "id"), pri); rank[is.na(rank)] <- 99L
    list(study = st, attributes = out[order(rank, vapply(out, `[[`, "", "name"))])
  })
}

# Samples (with RNA-seq) of one group: tumours with given clinical values, or mutant / wild type for a gene.
.tcga_group_samples <- function(st, g, rnaSamples) {
  type <- as.character(g$type %||% "clinical")
  if (type == "mutation") {
    ge <- .entrez_of(g$gene)
    seqd <- .sample_list(paste0(st$id, "_sequenced"))
    mut <- .tcga_cached(paste("mutsamples", st$id, ge$entrez), {
      m <- .cbio(paste0("/molecular-profiles/", st$id, "_mutations/mutations/fetch"),
                 body = list(entrezGeneIds = list(ge$entrez), sampleListId = paste0(st$id, "_sequenced")),
                 query = list(projection = "SUMMARY"))
      unique(as.character(m$sampleId))
    })
    s <- if (identical(g$status, "wildtype")) setdiff(seqd, mut) else mut
    label <- paste(ge$symbol, if (identical(g$status, "wildtype")) "wild type" else "mutant")
  } else {
    attr <- as.character(g$attribute %||% "")
    vals <- as.character(unlist(g$values))
    if (!nzchar(attr) || !length(vals)) stop("Choose an attribute and at least one value for each group.")
    a <- Filter(function(x) identical(x$id, attr), tcga_groups(st$id)$attributes)
    if (!length(a)) stop("Unknown attribute: ", attr)
    cd <- .tcga_cached(paste("clin", st$id, attr), .cbio(paste0("/studies/", st$id, "/clinical-data"),
      query = list(clinicalDataType = if (a[[1]]$patient) "PATIENT" else "SAMPLE", attributeId = attr)))
    hit <- cd[cd$value %in% vals, , drop = FALSE]
    s <- if (a[[1]]$patient) rnaSamples[substr(rnaSamples, 1, 12) %in% hit$patientId] else as.character(hit$sampleId)
    label <- paste(vals, collapse = " + ")
  }
  list(samples = intersect(s, rnaSamples), label = label)
}

#' Differentially expressed genes between two groups of tumours (cBioPortal expression
#' enrichment: t-test on log2 RNA-seq values; q-values by Benjamini–Hochberg here).
tcga_de_genes <- function(study, groupA, groupB, direction = "both", minLog2FC = 1, maxQ = 0.05, all = FALSE) {
  st <- .tcga_study(study)
  prof <- paste0(st$id, "_rna_seq_v2_mrna")
  rna <- .tcga_cached(paste("rna", st$id), .sample_list(prof))
  if (!length(rna)) stop("No RNA-seq data for this cancer type.")
  a <- .tcga_group_samples(st, groupA, rna); b <- .tcga_group_samples(st, groupB, rna)
  if (length(intersect(a$samples, b$samples))) {
    common <- intersect(a$samples, b$samples)
    a$samples <- setdiff(a$samples, common); b$samples <- setdiff(b$samples, common)
  }
  if (length(a$samples) < 3 || length(b$samples) < 3)
    stop(sprintf("Each group needs at least 3 tumours with RNA-seq (%s: %d, %s: %d).", a$label, length(a$samples), b$label, length(b$samples)))
  ids <- function(s) lapply(s, function(x) list(caseId = x, molecularProfileId = prof))
  key <- paste("de", st$id, digest_key(c(sort(a$samples), "|", sort(b$samples))))
  e <- .tcga_cached(key, .cbio("/expression-enrichments/fetch", query = list(enrichmentType = "SAMPLE"), body = list(
    list(name = "A", molecularProfileCaseIdentifiers = ids(a$samples)),
    list(name = "B", molecularProfileCaseIdentifiers = ids(b$samples)))))
  if (!NROW(e)) stop("cBioPortal returned no expression comparison.")
  mean_of <- function(stats, nm) vapply(stats, function(s) s$meanExpression[match(nm, s$name)], 0)
  fc <- mean_of(e$groupsStatistics, "A") - mean_of(e$groupsStatistics, "B")
  q <- stats::p.adjust(e$pValue, "BH")
  df <- data.frame(gene = e$hugoGeneSymbol, entrez = e$entrezGeneId, fc = fc, p = e$pValue, q = q, stringsAsFactors = FALSE)
  df <- df[is.finite(df$fc) & !is.na(df$gene), , drop = FALSE]
  minLog2FC <- max(suppressWarnings(as.numeric(minLog2FC))[1] %||% 1, 0, na.rm = TRUE)
  maxQ <- min(max(suppressWarnings(as.numeric(maxQ))[1] %||% 0.05, 1e-10, na.rm = TRUE), 1)
  if (isTRUE(all)) {
    df <- df[order(-df$fc), , drop = FALSE]                        # every gene, ranked by fold change (for GSEA)
  } else {
    keep <- df$q <= maxQ & switch(direction, up = df$fc >= minLog2FC, down = df$fc <= -minLog2FC, abs(df$fc) >= minLog2FC)
    df <- df[keep, , drop = FALSE]
    df <- df[order(df$p, -abs(df$fc)), , drop = FALSE]
  }
  what <- sprintf("%s vs %s", a$label, b$label)
  .tcga_result(st, .gene_records(df$gene, .gene_names(df$entrez), round(df$fc, 3),
                                 sprintf("q = %s", formatC(df$q, format = "g", digits = 2))),
               sprintf("log2 fold change (%s vs %s)", a$label, b$label), paste("DE genes,", what),
               list(groups = list(a = list(label = a$label, n = length(a$samples)), b = list(label = b$label, n = length(b$samples))),
                    keepAll = isTRUE(all)))
}

# The precomputed tumour-vs-normal table (read once; NULL-safe when the file is missing).
.tcga_tn_env <- new.env()
.tcga_tn <- function() {
  if (is.null(.tcga_tn_env$tab)) {
    f <- getExtdataPath("tcga_tumor_normal.rds")
    .tcga_tn_env$tab <- if (file.exists(f)) readRDS(f) else list(meta = list(), cancers = list())
  }
  .tcga_tn_env$tab
}

TCGA_TN_CITATION <- paste(
  "TCGA STAR gene counts (GDC) via the UCSC Xena GDC hub (Goldman et al. 2020, Nat Biotechnol 38:675);",
  "primary tumours vs adjacent normal tissue, limma-voom (Law et al. 2014, Genome Biol 15:R29).")

#' Differentially expressed genes, primary tumour vs adjacent normal tissue (precomputed;
#' only the cancer types with enough normal samples in TCGA).
tcga_tumor_normal <- function(study, direction = "both", minLog2FC = 1, maxQ = 0.05, all = FALSE) {
  st <- .tcga_study(study)
  tn <- .tcga_tn()
  d <- tn$cancers[[st$code]]
  if (is.null(d)) {
    stop(sprintf("TCGA has (almost) no normal tissue samples for %s. Tumour vs normal is available for: %s.",
                 st$code, paste(names(tn$cancers), collapse = ", ")))
  }
  df <- d$genes
  minLog2FC <- max(suppressWarnings(as.numeric(minLog2FC))[1] %||% 1, 0, na.rm = TRUE)
  maxQ <- min(max(suppressWarnings(as.numeric(maxQ))[1] %||% 0.05, 1e-10, na.rm = TRUE), 1)
  if (isTRUE(all)) {
    df <- df[order(-df$log2FC), , drop = FALSE]                    # every gene, ranked by fold change (for GSEA)
  } else {
    keep <- df$q <= maxQ & switch(direction, up = df$log2FC >= minLog2FC, down = df$log2FC <= -minLog2FC, abs(df$log2FC) >= minLog2FC)
    df <- df[keep, , drop = FALSE]
    df <- df[order(df$q, -abs(df$log2FC)), , drop = FALSE]
  }
  .tcga_result(st, .gene_records(df$gene, .gene_names(df$entrez), df$log2FC,
                                 sprintf("q = %s", formatC(df$q, format = "g", digits = 2))),
               "log2 fold change (tumour vs normal)", "tumour vs normal",
               list(groups = list(a = list(label = "Primary tumour", n = d$n$tumour), b = list(label = "Adjacent normal", n = d$n$normal)),
                    keepAll = isTRUE(all)),
               citation = paste0(TCGA_TN_CITATION, " Computed ", tn$meta$built %||% "", "."),
               link = sprintf("https://portal.gdc.cancer.gov/projects/TCGA-%s", d$projects[[1]]))
}

digest_key <- function(x) {
  # SHA-256 fingerprint of a character vector: cache keys shared by all users must not collide
  sodium::bin2hex(sodium::sha256(charToRaw(enc2utf8(paste(x, collapse = "\u0001")))))
}

#' One entry point for the API.
tcga_genes <- function(study, query, ...) {
  a <- list(...)
  switch(as.character(query %||% ""),
    mutated = tcga_mutated_genes(study),
    cna = tcga_cna_genes(study, a$direction %||% "amp"),
    coexpression = tcga_coexpressed(study, a$gene, a$direction %||% "positive", a$minCorrelation %||% 0.4),
    de = tcga_de_genes(study, a$groupA %||% list(), a$groupB %||% list(), a$direction %||% "both",
                       a$minLog2FC %||% 1, a$maxQ %||% 0.05, isTRUE(a$all)),
    normal = tcga_tumor_normal(study, a$direction %||% "both", a$minLog2FC %||% 1, a$maxQ %||% 0.05, isTRUE(a$all)),
    stop("Unknown TCGA query."))
}
