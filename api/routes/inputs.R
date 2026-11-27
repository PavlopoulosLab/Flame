# /api/inputs — input methods. These calls only parse/analyse; lists are
# created with POST /api/lists using the IDs the user selects.

.text_arg <- function(b, field = "text") {
  text <- paste(as.character(unlist(b[[field]] %||% "")), collapse = "\n")
  check_text_size(text)
  text
}

.as_records <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

#* Example input for a method (upload, snps, textmining, volcano, reduction)
#* @get /examples/<kind>
#* @serializer flamejson
function(res, kind) {
  api_handle(res, {
    if (!kind %in% EXAMPLE_KINDS) api_abort(404, "Unknown example.")
    tryCatch(example_input(kind), error = function(e) api_abort(503, paste("The example could not be prepared:", conditionMessage(e))))
  })
}

#* Example input as a downloadable file (so users can check the expected format)
#* @get /examples/<kind>/file
#* @serializer contentType list(type="text/plain; charset=utf-8")
function(res, kind) {
  files <- c(upload = "flame_example_genes.txt", snps = "flame_example_snps.txt",
             textmining = "flame_example_text.txt", volcano = "flame_example_volcano.tsv", volcano_airway = "flame_example_volcano_airway.tsv",
             reduction = "flame_example_umap.tsv", ranked = "flame_example_ranked.tsv", gmt = "flame_example_hallmark.gmt", expression = "flame_example_expression.tsv", mirna = "flame_example_mirnas.txt", metabolites = "flame_example_metabolites.txt",
             survival = "flame_example_survival_expression.tsv", survival_clinical = "flame_example_survival_clinical.tsv",
             bench_qpcr = "flame_example_bench_qpcr.tsv", bench_western = "flame_example_bench_western.tsv", bench_facs = "flame_example_bench_facs.tsv", bench_column = "flame_example_bench_column.tsv",
             bench_viability = "flame_example_bench_viability.tsv", bench_elisa = "flame_example_bench_elisa.tsv", bench_luciferase = "flame_example_bench_luciferase.tsv", bench_qpcr_fold = "flame_example_bench_qpcr_fold.tsv")
  if (!kind %in% names(files)) { res$status <- 404; return("Unknown example") }
  res$setHeader("Content-Disposition", sprintf("attachment; filename=\"%s\"", files[[kind]]))
  if (kind == "survival_clinical") return(example_input("survival")$clinical)
  ex <- example_input(kind)
  if (kind == "upload") ex$text <- paste(sort(strsplit(ex$text, "\n")[[1]]), collapse = "\n")
  ex$text
}

#* Parse pasted/uploaded IDs (counts of unique, duplicate and invalid entries)
#* Body: { text, skipFirstRow? }
#* @post /upload/parse
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    out <- if (isTRUE(b$ranked)) parse_ranked_text(.text_arg(b), isTRUE(b$skipFirstRow))
           else parse_id_text(.text_arg(b), isTRUE(b$skipFirstRow))
    out$ids <- as.list(out$ids)
    if (!is.null(out$scores)) out$scores <- as.list(out$scores)
    out$invalidExamples <- as.list(out$invalidExamples)   # a single example must stay a list, not become a bare string
    out
  })
}

#* Map SNP rsIDs to genes (g:SNPense). Body: { text }
#* @post /snps/map
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    parsed <- parse_snp_ids(.text_arg(body_of(req)))
    if (length(parsed$ids) == 0) api_abort(400, "No valid SNP rsIDs found. Use dbSNP IDs such as rs1130233.")
    result <- gsnpense_convert(parsed$ids)
    if (is.null(result) || nrow(result) == 0) api_abort(502, "Could not map the SNPs to genes. Check the rsIDs or try again later.")
    stats <- summarize_gsnpense_mapping(parsed, result)
    list(rows = .as_records(format_gsnpense_mapping_rows(result)), stats = stats,
         genes = list(USERINPUT = as.list(extract_gsnpense_list_ids(result, "USERINPUT")),
                      ENSG = as.list(extract_gsnpense_list_ids(result, "ENSG"))))
  })
}

#* Find gene/protein mentions in free text (EXTRACT tagger). Body: { text, organismTaxId }
#* @post /textmining/extract
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    text <- as.character(b$text %||% "")[1]
    validation <- validate_extract_text(text)
    if (!isTRUE(validation$ok)) api_abort(400, validation$message)
    taxid <- suppressWarnings(as.integer(b$organismTaxId %||% 9606L))
    if (is.na(taxid) || !taxid %in% ORGANISMS$taxid) api_abort(400, "Unknown organism.")
    extracted <- extract_entities(text, taxid)
    if (is.null(extracted) || nrow(extracted) == 0)
      api_abort(422, "No gene or protein names were found for this organism. Try another organism or more specific text.")
    rows <- format_extract_entity_rows(extracted)
    if (is.null(rows) || nrow(rows) == 0) api_abort(422, "No valid IDs were found in the text.")
    # The client highlights matches itself from these rows (text nodes only);
    # third-party HTML is never sent to the browser.
    list(rows = .as_records(rows), stats = summarize_extract_entity_rows(rows, validation, rawResult = extracted),
         organismTaxId = taxid, organismLabel = .organism_label(taxid))
  })
}

#* Named entities of a document's text, for annotating it (EXTRACT / JensenLab tagger):
#* genes and proteins of the organism, chemicals, organisms, GO terms, tissues, diseases,
#* environments and phenotypes. Body: { text, organismTaxId, kinds?: [genes, chemicals, organisms,
#* process, component, function, tissues, diseases, environments, phenotypes] }
#* @post /textmining/annotate
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    text <- as.character(b$text %||% "")[1]
    validation <- validate_extract_text(text)
    if (!isTRUE(validation$ok)) api_abort(400, validation$message)
    taxid <- suppressWarnings(as.integer(b$organismTaxId %||% 9606L))
    if (is.na(taxid) || !taxid %in% ORGANISMS$taxid) api_abort(400, "Unknown organism.")
    kinds <- as.character(unlist(b$kinds %||% names(EXTRACT_KINDS)))
    label <- .organism_label(taxid)
    # EXTRACT is a web service: ask it from the external-service workers.
    fetch_async(res, quote({
      ents <- extract_document_entities(text, taxid, kinds)
      recs <- if (is.null(ents) || !nrow(ents)) list() else lapply(seq_len(nrow(ents)), function(i) as.list(ents[i, , drop = FALSE]))
      list(entities = recs, kinds = lapply(names(EXTRACT_KINDS), function(k) list(id = k, label = EXTRACT_KINDS[[k]]$label)),
           organismTaxId = taxid, organismLabel = label, characters = nchar(text))
    }), list(text = text, taxid = taxid, kinds = kinds, label = label))
  })
}

#* Parse a table and suggest column roles. Body: { text, fileName?, kind: volcano|reduction }
#* @post /table/prepare
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    kind <- if (identical(b$kind, "reduction")) "reduction" else "volcano"
    tryCatch(prepare_table_input(.text_arg(b), as.character(b$fileName %||% ""), kind),
             error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* Volcano plot points. Body: { text, fileName?, mapping:{id,logfc,pvalue}, pThreshold, lfcThreshold }
#* @post /volcano/points
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    tryCatch(volcano_points(.text_arg(b), as.character(b$fileName %||% ""), b$mapping %||% list(),
                            b$pThreshold %||% 0.05, b$lfcThreshold %||% 1),
             error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* 2D reduction points. Body: { text, fileName?, mapping:{id,x,y,color?,size?} }
#* @post /reduction/points
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    tryCatch(reduction_points(.text_arg(b), as.character(b$fileName %||% ""), b$mapping %||% list()),
             error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* UpSet combinations of ≥ 2 compatible lists. Body: { listIds: [...], mode }
#* @post /upset
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    ws <- require_workspace(req)
    b <- body_of(req)
    ids <- unique(as.character(unlist(b$listIds)))
    if (length(ids) < 2) api_abort(400, "Select at least two lists.")
    if (length(ids) > 6) api_abort(400, "Select at most six lists.")
    rows <- lapply(ids, function(i) get_list(ws, i))
    keyOf <- function(r) paste(r$analyte, r$taxid, r$id_format)
    if (length(unique(vapply(rows, keyOf, character(1)))) > 1)
      api_abort(400, "Lists must share analyte type, organism and ID format.")
    sets <- setNames(lapply(rows, function(r) from_json(r$ids_json)), vapply(rows, function(r) r$name, character(1)))
    mode <- flameNormalizeUpsetMode(b$mode)
    combos <- unname(flameComputeUpsetCombinations(sets, mode))
    list(mode = mode, modeLabel = flameUpsetModeLabel(mode),
         sets = lapply(names(sets), function(n) list(name = n, size = length(sets[[n]]))),
         organismTaxId = rows[[1]]$taxid, analyteType = rows[[1]]$analyte, idFormat = rows[[1]]$id_format,
         combinations = lapply(combos, function(c) { c$ids <- as.list(c$ids); c$sourceNames <- as.list(c$sourceNames); c$sourceIndexes <- as.list(c$sourceIndexes); c }))
  })
}

#* Gene expression matrix: clusters (heatmap), gene map (PCA).
#* Body: { text, fileName?, topN?, k?, log?: true|false|null }
#* @post /expression/overview
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    x <- tryCatch(parse_expression_matrix(paste(as.character(unlist(b$text %||% "")), collapse = "\n"), as.character(b$fileName %||% ""), if (is.null(b$log)) NA else isTRUE(b$log)),
                  error = function(e) api_abort(400, conditionMessage(e)))
    expression_overview(x, b$topN %||% 1000L, b$k %||% 6L)
  })
}

#* Gene expression matrix: two-group comparison (Welch t-test) for the volcano selector.
#* Body: { text, fileName?, groupA:[samples], groupB:[samples], log? }
#* @post /expression/de
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    x <- tryCatch(parse_expression_matrix(paste(as.character(unlist(b$text %||% "")), collapse = "\n"), as.character(b$fileName %||% ""), if (is.null(b$log)) NA else isTRUE(b$log)),
                  error = function(e) api_abort(400, conditionMessage(e)))
    tryCatch(expression_de(x, b$groupA, b$groupB), error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* Clinical table for survival analysis: its columns and the guessed time / event columns.
#* Body: { clinical, clinicalFileName? }
#* @post /expression/survival-columns
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    tryCatch(survival_table_columns(paste(as.character(unlist(b$clinical %||% "")), collapse = "\n"), as.character(b$clinicalFileName %||% "")),
             error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* Gene expression matrix + clinical table: genes linked to survival (univariate Cox model per
#* gene, hazard ratio per standard deviation, optionally stratified by a clinical column).
#* Body: { text, fileName?, log?, clinical, clinicalFileName?, timeColumn?, eventColumn?, strataColumn? }
#* @post /expression/survival
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    x <- tryCatch(parse_expression_matrix(paste(as.character(unlist(b$text %||% "")), collapse = "\n"), as.character(b$fileName %||% ""), if (is.null(b$log)) NA else isTRUE(b$log)),
                  error = function(e) api_abort(400, conditionMessage(e)))
    clin <- tryCatch(parse_survival_table(paste(as.character(unlist(b$clinical %||% "")), collapse = "\n"), as.character(b$clinicalFileName %||% ""),
                                          b$timeColumn, b$eventColumn, b$strataColumn),
                     error = function(e) api_abort(400, conditionMessage(e)))
    tryCatch(expression_survival(x, clin), error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* Kaplan-Meier curves: samples split by a gene's expression or grouped by a clinical column; log-rank test,
#* median survival, numbers at risk and (two groups) the Cox hazard ratio.
#* Body: { text, fileName?, log?, clinical, clinicalFileName?, timeColumn, eventColumn, by: gene|column, gene?, split?, percent?, column? }
#* @post /expression/km
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    x <- tryCatch(parse_expression_matrix(paste(as.character(unlist(b$text %||% "")), collapse = "\n"), as.character(b$fileName %||% ""), if (is.null(b$log)) NA else isTRUE(b$log)),
                  error = function(e) api_abort(400, conditionMessage(e)))
    clin <- tryCatch(parse_survival_table(paste(as.character(unlist(b$clinical %||% "")), collapse = "\n"), as.character(b$clinicalFileName %||% ""),
                                          b$timeColumn, b$eventColumn),
                     error = function(e) api_abort(400, conditionMessage(e)))
    by <- if (identical(as.character(b$by %||% "gene")[1], "column")) "column" else "gene"
    tryCatch(survival_km(x, clin, gene = b$gene, by = by, split = as.character(b$split %||% "median")[1], percent = b$percent %||% 50, column = b$column),
             error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* Gene expression matrix: correlate a gene of interest with every other gene.
#* Body: { text, fileName?, gene, methods?: [pearson|spearman|kendall], samples?: [names], log? }
#* @post /expression/correlate
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    x <- tryCatch(parse_expression_matrix(paste(as.character(unlist(b$text %||% "")), collapse = "\n"), as.character(b$fileName %||% ""), if (is.null(b$log)) NA else isTRUE(b$log)),
                  error = function(e) api_abort(400, conditionMessage(e)))
    tryCatch(expression_correlate(x, b$gene, b$methods %||% CORRELATION_METHODS, b$samples),
             error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* Gene expression matrix: correlation of two genes (Pearson, Spearman, Kendall via cor.test) and their values.
#* Body: { text, fileName?, geneA, geneB, methods?, log? }
#* @post /expression/pair
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    x <- tryCatch(parse_expression_matrix(paste(as.character(unlist(b$text %||% "")), collapse = "\n"), as.character(b$fileName %||% ""), if (is.null(b$log)) NA else isTRUE(b$log)),
                  error = function(e) api_abort(400, conditionMessage(e)))
    tryCatch(expression_pair(x, b$geneA, b$geneB, b$methods %||% CORRELATION_METHODS), error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* Gene expression matrix: values of up to 24 genes per sample, compared between 2-3 sample groups.
#* Body: { text, fileName?, genes:[ids], groups?: { name: [samples] }, test?: parametric|rank, log? }
#* @post /expression/genes
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    x <- tryCatch(parse_expression_matrix(paste(as.character(unlist(b$text %||% "")), collapse = "\n"), as.character(b$fileName %||% ""), if (is.null(b$log)) NA else isTRUE(b$log)),
                  error = function(e) api_abort(400, conditionMessage(e)))
    tryCatch(expression_gene_values(x, b$genes, b$groups, b$test %||% "parametric"), error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* microRNAs to target genes (miRDB via MSigDB). Body: { text, organismTaxId?, minMirnas? }
#* @post /mirna/targets
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    taxid <- suppressWarnings(as.integer(b$organismTaxId %||% 9606L)); if (is.na(taxid)) taxid <- 9606L
    tryCatch(mirna_targets(.text_arg(b), taxid, b$minMirnas %||% 1L), error = function(e) api_abort(400, conditionMessage(e)))
  })
}

#* Expression matrix: pathway activity per sample (ssGSEA, combined z-score or PROGENy).
#* Body: { text, fileName?, method: ssgsea|zscore|progeny, sources?, organismTaxId?, log? }
#* @post /expression/activity
#* @serializer flamejson
function(req, res) {
  api_handle(res, {
    b <- body_of(req)
    x <- tryCatch(parse_expression_matrix(paste(as.character(unlist(b$text %||% "")), collapse = "\n"), as.character(b$fileName %||% ""),
                                          if (is.null(b$log)) NA else isTRUE(b$log)), error = function(e) api_abort(400, conditionMessage(e)))
    method <- as.character(b$method %||% "ssgsea"); if (!method %in% c("ssgsea", "zscore", "progeny")) method <- "ssgsea"
    taxid <- suppressWarnings(as.integer(b$organismTaxId %||% 9606L)); if (is.na(taxid)) taxid <- 9606L
    src <- intersect(as.character(unlist(b$sources %||% "HALLMARK")), names(TOOLS[[ToolId$CLUSTERPROFILER]]$datasourceCodes))
    if (!length(src)) src <- "HALLMARK"
    tryCatch(pathway_activity(x, method, src, taxid), error = function(e) api_abort(422, conditionMessage(e)))
  })
}

# ---- Fetch gene lists from public resources ----------------------------------
# Requests to external services run on the external-service workers (api/jobs.R), with the
# route's local variables (except req and res); .fetch_here is for local computations.
.fetch <- function(res, expr) {
  env <- parent.frame()
  vars <- setdiff(ls(env), c("req", "res"))
  fetch_async(res, substitute(expr), mget(vars, envir = env))
}
.fetch_here <- function(res, expr) api_handle(res, tryCatch(expr, error = function(e) {
  if (inherits(e, "flame_api_error")) stop(e)
  api_abort(422, conditionMessage(e))
}))

#* Search diseases (Open Targets) and disease gene-set libraries (DisGeNET, OMIM, Orphanet, ClinVar, Jensen, GWAS).
#* Body: { query, libraries? } — libraries: which databases to query, any of
#* OpenTargets, DisGeNET, OMIM, Orphanet, ClinVar, Jensen, GWAS (default: all).
#* @post /disease/search
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, {
    q <- as.character(b$query %||% "")
    known <- c("OpenTargets", names(DISEASE_LIBRARIES))
    libs <- if (is.null(b$libraries)) known else intersect(as.character(unlist(b$libraries)), known)
    if (!length(libs)) api_abort(400, "Select at least one database to search.")
    list(diseases = if ("OpenTargets" %in% libs) disease_search(q) else list(),
         sets = disease_library_search(q, setdiff(libs, "OpenTargets")),
         queried = as.list(libs),
         libraries = lapply(names(DISEASE_LIBRARIES), function(k) list(id = k, label = DISEASE_LIBRARIES[[k]]$label)))
  })
}

#* Genes of a disease. Body: { source: opentargets|gwas|<library id>, id?, term?, size? }
#* @post /disease/genes
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, {
    src <- as.character(b$source %||% "opentargets")
    size <- suppressWarnings(as.integer(b$size %||% 500L)); if (is.na(size)) size <- 500L
    if (src == "opentargets") disease_genes_opentargets(as.character(b$id), size)
    else if (src == "gwas") disease_gwas_variants(as.character(b$id), min(size, 1000L))
    else disease_library_genes(src, as.character(b$term))
  })
}

#* Genes linked to PubMed articles. Body: { query (PMIDs or a PubMed search), organismTaxId?, max? }
#* @post /pubmed/genes
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, {
    taxid <- suppressWarnings(as.integer(b$organismTaxId %||% 9606L)); if (is.na(taxid)) taxid <- 9606L
    mx <- suppressWarnings(as.integer(b$max %||% 200L)); if (is.na(mx)) mx <- 200L
    pubmed_genes(as.character(b$query %||% ""), taxid, min(max(mx, 1L), 500L))
  })
}

#* Search GEO signatures (RummaGEO). Body: { query }
#* @post /geo/search
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, geo_search(as.character(b$query %||% "")))
}

#* Genes of one GEO signature. Body: { id }
#* @post /geo/genes
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, {
    id <- as.character(b$id %||% "")
    if (!grepl("^[0-9a-f-]{36}$", id)) api_abort(400, "Invalid gene-set id.")
    geo_genes(id)
  })
}

#* ClinVar: a gene, condition, rsIDs, VCV accessions or a ClinVar search to variants and genes.
#* Body: { query, significance?: any|pathogenic|pathogenic_likely, max? }
#* @post /clinvar/lookup
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, {
    mx <- suppressWarnings(as.integer(b$max %||% 300L)); if (is.na(mx)) mx <- 300L
    clinvar_lookup(as.character(b$query %||% ""), as.character(b$significance %||% "any"), min(max(mx, 1L), 1000L))
  })
}

#* dbSNP: rsIDs or a dbSNP search to variants and genes. Body: { query, max? }
#* @post /dbsnp/lookup
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, {
    mx <- suppressWarnings(as.integer(b$max %||% 300L)); if (is.na(mx)) mx <- 300L
    dbsnp_lookup(as.character(b$query %||% ""), min(max(mx, 1L), 1000L))
  })
}

#* TCGA cancer types: the TCGA PanCancer Atlas studies on cBioPortal.
#* @get /tcga/studies
#* @serializer flamejson
function(req, res) {
  .fetch(res, list(studies = tcga_studies()))
}

#* Clinical attributes of a TCGA cancer type (subtype, stage, sex, ...) and their values,
#* to split the tumours into two groups. Body: { study }
#* @post /tcga/groups
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, tcga_groups(as.character(b$study %||% "")))
}

#* Genes from TCGA (via cBioPortal). Body: { study, query, ... } where query is
#*   mutated                                   most often mutated genes
#*   cna          direction: amp | del         most often amplified / deleted genes
#*   coexpression gene, direction: positive | negative | both, minCorrelation
#*   de           groupA, groupB, direction: up | down | both, minLog2FC, maxQ, all
#*   normal       direction: up | down | both, minLog2FC, maxQ, all  (tumour vs adjacent normal, precomputed)
#* A group is { type: "clinical", attribute, values: [...] } or { type: "mutation", gene, status: mutant | wildtype }.
#* @post /tcga/genes
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, tcga_genes(as.character(b$study %||% ""), as.character(b$query %||% ""),
                         direction = b$direction, gene = b$gene, minCorrelation = b$minCorrelation,
                         groupA = b$groupA, groupB = b$groupB, minLog2FC = b$minLog2FC, maxQ = b$maxQ, all = isTRUE(b$all)))
}

#* Genes linked to a nutrient, food compound or food, from live APIs: CTD (curated
#* chemical-gene interactions, also foods such as tea or olive oil), ChEMBL (human targets
#* with measured activity) or UniProt (enzymes, transporters and binding proteins, via ChEBI).
#* Body: { term, source: ctd | chembl | uniprot, organisms?: human | all (CTD), minPchembl? (ChEMBL), taxid? (UniProt) }
#* @post /nutrition/genes
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, nutrition_genes(as.character(b$term %||% ""), as.character(b$source %||% ""),
                              organisms = if (identical(b$organisms, "all")) "all" else "human",
                              minPchembl = b$minPchembl, taxid = b$taxid))
}

#* Diet and nutrient traits (intake, blood levels...) for a term, from Open Targets; their
#* GWAS Catalog genes come from /disease/genes with source "gwas". Body: { term }
#* @post /nutrition/traits
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch(res, list(traits = nutrition_traits(as.character(b$term %||% ""))))
}

#* Bench data, step 1: the sample columns of a pasted table and a guess of their groups from the
#* column names, so the user can group them before any test. Body: { text, fileName?, layout? }
#* @post /bench/samples
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch_here(res, bench_samples(.text_arg(b), as.character(b$fileName %||% ""), as.character(b$layout %||% "auto")))
}

#* Experimental analysis, "which test?": GraphPad Prism's normality tests per group (and row), equal-SD
#* checks, QQ-plot points and a recommended test. Body: { text, fileName?, layout?, groups?, log?, matched? }
#* @post /bench/normality
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch_here(res, bench_normality(.text_arg(b), as.character(b$fileName %||% ""), as.character(b$layout %||% "auto"),
                                   sampleGroups = b$groups, log = isTRUE(b$log), matched = isTRUE(b$matched)))
}

#* Bench data: a small pasted table (column or grouped layout) analysed with a sensible default test,
#* optional helpers for qPCR (delta-delta Ct), westerns (band / loading control) and FACS phases.
#* Body: { text, fileName?, layout?: auto|column|grouped, helper?: none|qpcr|western|facs, refs?: [genes] (qPCR),
#*        loading? (western), control?, paired?, nonparametric?, allPairs?, log?,
#*        groups?: { <group name>: [sample columns] } (grouped tables: replaces the groups guessed from column names),
#*        test?: auto | t_unpaired | t_welch | t_paired | t_ratio | mann_whitney | ks | wilcoxon | anova | welch_anova | bf_anova |
#*               rm_anova | kruskal | friedman | twoway | one_sample_t | one_sample_wilcoxon,
#*        posthoc?: auto | dunnett | tukey | sidak | bonferroni | holm_sidak | fisher | games_howell | dunnett_t3 | dunn,
#*        across?: holm | bh | none (several rows), hypothetical? (one-sample tests; default 1 on a log scale, else 0) }
#* @post /bench/analyse
#* @serializer flamejson
function(req, res) {
  b <- body_of(req)
  .fetch_here(res, bench_analyse(.text_arg(b), as.character(b$fileName %||% ""), as.character(b$layout %||% "auto"),
                            as.character(b$helper %||% "none"), b$refs, b$loading, b$control,
                            isTRUE(b$paired), isTRUE(b$nonparametric), isTRUE(b$allPairs), isTRUE(b$log),
                            sampleGroups = b$groups, test = as.character(b$test %||% "auto"), posthoc = as.character(b$posthoc %||% "auto"),
                            across = as.character(b$across %||% "holm"), hypothetical = b$hypothetical))
}
