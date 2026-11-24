# =============================================================================
# OpenAPI document of the Flame REST API (v1)
# =============================================================================
# plumber generates the paths from the route annotations; this adds what it
# cannot know: the versioned paths only, tags, the workspace header as an
# "Authorize" scheme, JSON request bodies with examples, typed errors,
# operation ids and an introduction. Served at /openapi.json and shown at
# /__docs__/ (Swagger UI).
# =============================================================================

API_TAGS <- list(
  list(name = "Workspace", description = "Create a workspace (no account needed), check its status, save and load sessions, delete it."),
  list(name = "Reference", description = "Tools, annotation sources, ID types, organisms and limits."),
  list(name = "Lists", description = "Gene, protein or SNP lists (optionally ranked, with scores)."),
  list(name = "Inputs", description = "Turn files, plots, text, microRNAs, SNPs, diseases, PubMed, GEO, ClinVar and dbSNP into gene lists."),
  list(name = "Enrichment runs", description = "Start ORA or GSEA runs with one or more tools, poll their status, read results in any view."),
  list(name = "Comparisons", description = "Compare runs (tools, lists or strategies): combined p-values, consensus ranks, statistics, themes."),
  list(name = "Analyses", description = "Background analyses: transcription factors, kinases, drug signatures, network modules, robustness, metabolites."),
  list(name = "Variants", description = "Variant and GWAS annotation: consequences, genes, frequencies, ClinVar, traits."),
  list(name = "Custom gene sets", description = "Upload your own gene sets (GMT) and test them like any other source."),
  list(name = "Utilities", description = "ID conversion, orthologs and STRING networks."),
  list(name = "Legacy", description = "Flame v2-compatible import of gene lists."),
  list(name = "Health", description = "Server status.")
)
.tag_of <- function(path) {
  seg <- strsplit(sub("^/api/v1/", "", path), "/")[[1]][1]
  switch(seg, workspace = "Workspace", config = , organisms = "Reference", lists = "Lists", inputs = "Inputs",
         runs = "Enrichment runs", comparisons = "Comparisons", analyses = "Analyses", variants = "Variants",
         sources = "Custom gene sets", utilities = "Utilities", legacy = "Legacy", health = "Health", "Other")
}

# Endpoints that work without a workspace header.
.public_ops <- c("POST /api/v1/workspace", "GET /api/v1/config", "GET /api/v1/organisms", "GET /api/v1/organisms/{taxid}/tools",
                 "GET /api/v1/health", "GET /api/v1/legacy", "POST /api/v1/legacy", "GET /api/v1/variants/species")
.is_public <- function(method, path) {
  key <- paste(toupper(method), path)
  key %in% .public_ops || grepl("^GET /api/v1/(inputs|variants)/examples/", key) ||
    (grepl("^/api/v1/inputs/", path) && !grepl("upset$", path))
}

# ---- Small schema helpers ------------------------------------------------------
.s <- function(type, description = NULL, ...) Filter(Negate(is.null), list(type = type, description = description, ...))
.arr <- function(items, description = NULL, ...) .s("array", description, items = items, ...)
.obj <- function(properties, required = NULL, description = NULL, example = NULL) {
  Filter(Negate(is.null), list(type = "object", description = description, required = if (length(required)) as.list(required),
                               properties = properties, example = example))
}
STR <- function(d = NULL, ...) .s("string", d, ...)
INT <- function(d = NULL, ...) .s("integer", d, ...)
NUM <- function(d = NULL, ...) .s("number", d, ...)
BOOL <- function(d = NULL, ...) .s("boolean", d, ...)
STRS <- function(d = NULL, ...) .arr(.s("string"), d, ...)

TAXID <- INT("NCBI taxonomy id of the organism (9606 = human, 10090 = mouse).", example = 9606)
TEXT <- STR("Pasted text or the contents of an uploaded file.")
FILTERS <- .obj(list(
  search = STR("Search terms: words must all match; \"quotes\", OR and -exclude are supported."),
  sources = STRS("Keep only these annotation sources (e.g. GO:BP, KEGG, REAC)."),
  pvalueMax = NUM("Keep terms with p ≤ this value."),
  termSizeMin = INT(), termSizeMax = INT(), intersectionMin = INT(), intersectionMax = INT(),
  termCoverageMin = NUM("Percent of the term's genes found in the list."), termCoverageMax = NUM(),
  runs = STRS("Comparisons: run ids to include (default: all compared runs)."),
  runsMode = STR("Comparisons: any | all — terms found by any or by all included runs.")),
  description = "Result filters (all optional).")
VIEW_BODY <- .obj(list(
  view = STR("table | dotplot | barchart | scatter | heatmap-tg | heatmap-tt | heatmap-gg | network-tg | network-tt | network-gg | sankey | chord | hive | circos"),
  filters = FILTERS,
  top = .s("string", "Number of terms to show, or \"all\".", example = "20"),
  sortBy = STR("Sort terms by: pvalue | coverage | size | intersection."),
  settings = .obj(list(layout = STR("Network layout: force | circle | hierarchical…"), similarityCutoff = NUM(), minShared = INT()))),
  required = "view", example = list(view = "barchart", top = 20, filters = list(sources = list("GO:BP", "KEGG"), pvalueMax = 0.05)))

BODIES <- list(
  "POST /api/v1/workspace" = NULL,
  "PUT /api/v1/workspace/email" = .obj(list(email = STR("Address to notify when runs finish; empty to clear. Used only if the server sends email.")), example = list(email = "")),
  "POST /api/v1/workspace/import" = .obj(list(format = STR("Session format marker (from an exported file)."), lists = .arr(.s("object")), runs = .arr(.s("object")),
                                              sources = .arr(.s("object")), variants = .arr(.s("object"))),
                                         description = "A session file as exported by GET /workspace/export (up to 25 MB)."),
  "POST /api/v1/lists" = .obj(list(
      name = STR("List name (unique in the workspace unless uniqueName is true)."),
      ids = STRS("Identifiers. Give either ids or text."), text = STR("IDs separated by new lines, commas, tabs or semicolons; with ranked = true, \"ID<TAB>score\" lines."),
      scores = .arr(NUM(), "One score per id: makes a ranked list (for GSEA)."), ranked = BOOL("Parse text as ID + score."),
      skipFirstRow = BOOL("Skip a header row in text."), organismTaxId = TAXID,
      analyteType = STR("gene (default) | protein | snp"), idFormat = STR("ID type, e.g. USERINPUT, ENSG, ENTREZGENE_ACC (see /config)."),
      uniqueName = BOOL("Add a suffix if the name is taken."), meta = .s("object", "Free-form provenance (method, source…).")),
    required = c("name", "organismTaxId"),
    example = list(name = "My genes", ids = list("TP53", "MDM2", "CDKN1A", "BAX", "GADD45A", "FAS", "BBC3", "SESN1"), organismTaxId = 9606)),
  "PATCH /api/v1/lists/{id}" = .obj(list(name = STR("New name."), text = STR("New contents: one ID per line (or \"ID score\" pairs for a ranked list)."), ids = STRS("New contents as an array (instead of text).")), example = list(name = "Renamed list")),
  "POST /api/v1/runs" = .obj(list(
      listId = STR("List to analyse."), tools = STRS("gprofiler, string, enrichr, webgestalt, panther, genecodis, clusterprofiler, reactome"),
      datasources = STRS("Annotation sources, e.g. GO:BP, GO:MF, GO:CC, KEGG, REAC, WP, HALLMARK (see /config for each tool)."),
      sourceIds = STRS("Custom gene-set (GMT) ids to test as well."), paradigm = STR("ORA (default) | GSEA (ranked lists; clusterProfiler)."),
      threshold = NUM("Significance threshold (default 0.05)."),
      perTool = .s("object", "Per-tool settings: { <toolKey>: { correction, threshold, backgroundListId } }.")),
    required = c("listId", "tools"),
    example = list(listId = "<list id>", tools = list("gprofiler", "reactome"), datasources = list("GO:BP", "KEGG", "REAC"), threshold = 0.05)),
  "POST /api/v1/runs/{id}/view" = VIEW_BODY,
  "POST /api/v1/runs/{id}/arena3d" = VIEW_BODY,
  "POST /api/v1/runs/{id}/norma" = VIEW_BODY,
  "POST /api/v1/runs/{id}/pathway-links" = .obj(list(termId = STR("Term id, e.g. KEGG:04110 or R-HSA-69278.")), "termId", example = list(termId = "R-HSA-6785807")),
  "POST /api/v1/runs/{id}/themes" = .obj(list(cutoff = NUM("Minimum gene overlap (Jaccard) to link two terms; default 0.35."), filters = FILTERS), example = list(cutoff = 0.35)),
  "POST /api/v1/comparisons" = .obj(list(runIds = STRS("2–10 finished runs."), view = STR("overview | bubble | heatmap"), filters = FILTERS,
                                         top = .s("string", "Number of terms, or \"all\"."), sortBy = STR(), settings = .s("object")),
                                    "runIds", example = list(runIds = list("<run id 1>", "<run id 2>"), view = "overview", top = "all")),
  "POST /api/v1/comparisons/statistics" = .obj(list(runIds = STRS(), filters = FILTERS), "runIds", example = list(runIds = list("<run id 1>", "<run id 2>"))),
  "POST /api/v1/comparisons/themes" = .obj(list(runIds = STRS(), filters = FILTERS, cutoff = NUM()), "runIds", example = list(runIds = list("<run id 1>", "<run id 2>"), cutoff = 0.35)),
  "POST /api/v1/comparisons/overview-terms" = .obj(list(runIds = STRS(), filters = FILTERS, kind = STR("found | unique | blockers | consensus | single…"),
                                                        runId = STR(), metric = STR()), c("runIds", "kind")),
  "POST /api/v1/comparisons/arena3d" = .obj(list(runIds = STRS(), filters = FILTERS, top = .s("string"), sortBy = STR()), "runIds"),
  "POST /api/v1/analyses" = .obj(list(
      type = STR("tf | kinase | drugs | modules | robustness | metabolites"),
      listId = STR("tf, kinase, modules, robustness: the list to analyse."),
      upListId = STR("drugs: up-regulated genes."), downListId = STR("drugs: down-regulated genes."),
      kind = STR("drugs: chemical (default) | crispr"), score = INT("modules: minimum STRING score (default 700)."),
      sources = STRS("robustness: MSigDB collections (GO:BP, KEGG, REAC, WP, HALLMARK…)."),
      text = STR("metabolites: names, KEGG compound or ChEBI ids, one per line."), name = STR("metabolites: name of the resulting run."),
      organismTaxId = TAXID, threshold = NUM("metabolites: FDR threshold (default 0.05).")),
    "type", example = list(type = "tf", listId = "<list id>")),
  "POST /api/v1/variants" = .obj(list(name = STR(), text = STR("rsIDs, VCF lines, chr:pos:ref:alt, HGVS, or GWAS summary statistics (SNP, CHR, BP, P columns)."), organismTaxId = TAXID),
                                 "text", example = list(name = "APOE variants", text = "rs429358\nrs7412", organismTaxId = 9606)),
  "POST /api/v1/sources" = .obj(list(name = STR(), text = STR("GMT file contents: name<TAB>description<TAB>gene1<TAB>gene2…")), c("name", "text")),
  "POST /api/v1/utilities/convert" = .obj(list(listId = STR(), targetFormat = STR("e.g. ENTREZGENE_ACC, ENSG, UNIPROTSWISSPROT"), mappingMode = STR("best | all")), c("listId", "targetFormat"),
                                          example = list(listId = "<list id>", targetFormat = "ENSG", mappingMode = "best")),
  "POST /api/v1/utilities/annotate" = .obj(list(listId = STR("List to annotate.")), "listId"),
  "POST /api/v1/utilities/orthologs" = .obj(list(listId = STR(), targetTaxId = INT(), mappingMode = STR("best | all")), c("listId", "targetTaxId"),
                                            example = list(listId = "<list id>", targetTaxId = 10090, mappingMode = "best")),
  "POST /api/v1/utilities/ppi" = .obj(list(listId = STR(), networkType = STR("functional | physical"), networkEdges = STR("evidence | confidence"), score = INT("Minimum STRING score, 150–999.")),
                                      "listId", example = list(listId = "<list id>", networkType = "functional", score = 700)),
  "POST /api/v1/utilities/ppi/arena3d" = .obj(list(listId = STR(), networkType = STR(), networkEdges = STR(), score = INT()), "listId"),
  "POST /api/v1/legacy" = .obj(list(organism = TAXID), description = "Named gene lists, Flame v2 style: { \"list A\": [genes], \"list B\": [genes], \"organism\": 9606 }.",
                               example = list(`List A` = list("TP53", "MDM2", "CDKN1A"), organism = 9606)),
  "POST /api/v1/inputs/upload/parse" = .obj(list(text = TEXT, ranked = BOOL(), skipFirstRow = BOOL()), "text", example = list(text = "TP53\nMDM2\nCDKN1A")),
  "POST /api/v1/inputs/snps/map" = .obj(list(text = STR("rsIDs, one per line.")), "text", example = list(text = "rs429358\nrs7412")),
  "POST /api/v1/inputs/textmining/extract" = .obj(list(text = STR("Any text, e.g. an abstract."), organismTaxId = TAXID), "text",
                                                  example = list(text = "TP53 activates CDKN1A and MDM2 after DNA damage.", organismTaxId = 9606)),
  "POST /api/v1/inputs/table/prepare" = .obj(list(text = TEXT, fileName = STR(), kind = STR("volcano | reduction")), c("text", "kind")),
  "POST /api/v1/inputs/volcano/points" = .obj(list(text = TEXT, fileName = STR(), mapping = .obj(list(id = STR("Gene column"), logfc = STR("log2FC column"), pvalue = STR("p-value column"))),
                                                   pThreshold = NUM(), lfcThreshold = NUM()), c("text", "mapping")),
  "POST /api/v1/inputs/reduction/points" = .obj(list(text = TEXT, fileName = STR(), mapping = .obj(list(id = STR(), x = STR(), y = STR(), color = STR(), size = STR()))), c("text", "mapping")),
  "POST /api/v1/inputs/upset" = .obj(list(listIds = STRS("2–6 lists with the same organism and ID type."), mode = STR("distinct | intersection | union")), "listIds"),
  "POST /api/v1/inputs/expression/overview" = .obj(list(text = STR("Genes × samples matrix (first column: gene)."), fileName = STR(), topN = INT("Most variable genes to use."), k = INT("Number of clusters."),
                                                        log = BOOL("Force log2 transform (default: automatic).")), "text"),
  "POST /api/v1/inputs/expression/de" = .obj(list(text = STR(), fileName = STR(), groupA = STRS("Sample names of group A."), groupB = STRS("Sample names of group B."), log = BOOL()), c("text", "groupA", "groupB")),
  "POST /api/v1/inputs/expression/correlate" = .obj(list(text = STR(), fileName = STR(), gene = STR("Gene of interest (as in the first column)."), methods = STRS("Any of pearson, spearman, kendall (default: all)."), samples = STRS("Samples to use (default: all; at least 4)."), log = BOOL()), c("text", "gene")),
  "POST /api/v1/inputs/expression/pair" = .obj(list(text = STR(), fileName = STR(), geneA = STR("First gene."), geneB = STR("Second gene."), methods = STRS("Any of pearson, spearman, kendall (default: all)."), log = BOOL()), c("text", "geneA", "geneB")),
  "POST /api/v1/inputs/expression/genes" = .obj(list(text = STR(), fileName = STR(), genes = STRS("Up to 24 gene IDs."), groups = list(type = "object", description = "2-3 groups: { group name: [sample names] }."), test = STR("parametric (Welch t-test / Welch ANOVA) or rank (Wilcoxon / Kruskal-Wallis)."), log = BOOL()), c("text", "genes")),
  "POST /api/v1/inputs/expression/activity" = .obj(list(text = STR(), fileName = STR(), method = STR("ssgsea | zscore | progeny"), sources = STRS("HALLMARK, KEGG, REAC, WP, GO:BP"),
                                                        organismTaxId = TAXID, log = BOOL()), "text"),
  "POST /api/v1/inputs/mirna/targets" = .obj(list(text = STR("miRBase names, one per line."), organismTaxId = TAXID, minMirnas = INT("Keep genes targeted by at least this many microRNAs.")), "text",
                                             example = list(text = "hsa-miR-21-5p\nhsa-miR-155-5p\nhsa-let-7a-5p", organismTaxId = 9606)),
  "POST /api/v1/inputs/disease/search" = .obj(list(query = STR("Disease or trait name."), libraries = STRS("DisGeNET, OMIM, Orphanet, ClinVar, Jensen, GWAS")), "query", example = list(query = "asthma")),
  "POST /api/v1/inputs/disease/genes" = .obj(list(source = STR("opentargets | gwas | DisGeNET | OMIM | Orphanet | ClinVar | Jensen | GWAS"),
                                                  id = STR("opentargets / gwas: EFO or MONDO id from /inputs/disease/search."), term = STR("Libraries: gene-set name from /inputs/disease/search."),
                                                  size = INT("Maximum genes (Open Targets) or variants (GWAS).")), "source",
                                             example = list(source = "opentargets", id = "MONDO_0004979", size = 200)),
  "POST /api/v1/inputs/pubmed/genes" = .obj(list(query = STR("PMIDs, or a PubMed search."), organismTaxId = TAXID, max = INT("Articles to use for a search (≤ 500).")), "query",
                                            example = list(query = "glucocorticoid receptor airway smooth muscle", organismTaxId = 9606, max = 100)),
  "POST /api/v1/inputs/geo/search" = .obj(list(query = STR("GSE accession or keywords.")), "query", example = list(query = "GSE52778")),
  "POST /api/v1/inputs/geo/genes" = .obj(list(id = STR("Signature id from /inputs/geo/search.")), "id"),
  "POST /api/v1/inputs/clinvar/lookup" = .obj(list(query = STR("A gene symbol, a condition, rsIDs, ClinVar accessions (VCV…) or a ClinVar search expression."), significance = STR("any (default) | pathogenic | pathogenic_likely"), max = INT()), "query",
      example = list(query = "APOE", significance = "pathogenic_likely")),
  "POST /api/v1/inputs/dbsnp/lookup" = .obj(list(query = STR("rsIDs or a dbSNP search."), max = INT()), "query",
                                            example = list(query = "APOE[gene] AND pathogenic[clinical_significance]", max = 50)),
  "POST /api/v1/inputs/nutrition/genes" = .obj(list(term = STR("A nutrient, food compound or food (e.g. curcumin, vitamin D, green tea)."),
      source = STR("ctd | chembl | uniprot"), organisms = STR("CTD: human (default) | all (adds mouse and rat genes)."),
      minPchembl = NUM("ChEMBL: minimum pChEMBL (default 5)."), taxid = INT("UniProt: organism (default 9606).")), c("term", "source"),
    example = list(term = "curcumin", source = "ctd")),
  "POST /api/v1/inputs/tcga/groups" = .obj(list(study = STR("TCGA study id from /inputs/tcga/studies, e.g. brca_tcga_pan_can_atlas_2018.")), "study",
                                           example = list(study = "brca_tcga_pan_can_atlas_2018")),
  "POST /api/v1/inputs/tcga/genes" = .obj(list(study = STR("TCGA study id from /inputs/tcga/studies."),
      query = STR("mutated | cna | coexpression | de"), direction = STR("cna: amp | del; coexpression: positive | negative | both; de: up | down | both"),
      gene = STR("coexpression: the gene of interest (symbol)."), minCorrelation = NUM("coexpression: minimum |Spearman| (default 0.4)."),
      groupA = .s("object", "de: {type: clinical, attribute, values: [...]} or {type: mutation, gene, status: mutant | wildtype}"),
      groupB = .s("object", "de: the second group, as groupA."), minLog2FC = NUM("de: minimum |log2 fold change| (default 1)."),
      maxQ = NUM("de: maximum q-value (default 0.05)."), all = BOOL("de: every gene ranked by fold change (for GSEA).")), c("study", "query"),
    example = list(study = "brca_tcga_pan_can_atlas_2018", query = "de", direction = "both",
                   groupA = list(type = "clinical", attribute = "SUBTYPE", values = list("BRCA_Basal")),
                   groupB = list(type = "clinical", attribute = "SUBTYPE", values = list("BRCA_LumA"))))
)

ERROR_SCHEMA <- .obj(list(error = STR("What went wrong, in plain words.")), "error", example = list(error = "Missing or invalid workspace link."))
.err <- function(d) list(description = d, content = list(`application/json` = list(schema = list(`$ref` = "#/components/schemas/Error"))))

.op_id <- function(method, path) {
  parts <- strsplit(sub("^/api/v1/", "", path), "/")[[1]]
  parts <- gsub("[{}]", "", parts)
  parts <- gsub("-([a-z])", "\\U\\1", parts, perl = TRUE)
  parts <- ifelse(seq_along(parts) > 1, paste0(toupper(substr(parts, 1, 1)), substring(parts, 2)), parts)
  paste0(tolower(method), toupper(substr(paste(parts, collapse = ""), 1, 1)), substring(paste(parts, collapse = ""), 2))
}

INTRO <- paste(
  "Functional and literature enrichment of multiple gene lists, with comparison of tools, lists and strategies.",
  "",
  "**Quick start**",
  "1. `POST /api/v1/workspace` → `{ \"id\": \"…\" }`. Click **Authorize** and paste the id (it is sent as the `X-Flame-Workspace` header).",
  "2. `POST /api/v1/lists` with your genes → a list id.",
  "3. `POST /api/v1/runs` with the list id and tools → run ids (HTTP 202).",
  "4. Poll `GET /api/v1/runs/{id}` until `status` is `complete`, then `POST /api/v1/runs/{id}/view` with `{ \"view\": \"table\", \"top\": \"all\" }`.",
  "",
  "Workspaces are private to whoever holds their id and are deleted after a period without use.",
  "All bodies and responses are JSON. Errors return `{ \"error\": \"…\" }` with a 4xx/5xx status.",
  "Limits: 2 MB per request (25 MB for session files), 600 requests per minute per address, a few jobs running per workspace.",
  "A ready-made Python client is linked from the API page of the web app.",
  "",
  "The same endpoints are also served without the version prefix (`/api/…`) for the web app; scripts should use `/api/v1`.",
  sep = "\n")

#' Rewrite the plumber-generated spec into the documented v1 API.
flame_openapi <- function(spec) {
  spec$openapi <- "3.0.3"
  spec$info <- list(title = "Flame 3.0 REST API", version = "1.0.0", description = INTRO,
                    license = list(name = "GPL-3.0", url = "https://www.gnu.org/licenses/gpl-3.0.html"))
  spec$servers <- list(list(url = "/", description = "This server"))
  spec$tags <- API_TAGS
  spec$components <- list(
    securitySchemes = list(workspace = list(type = "apiKey", `in` = "header", name = "X-Flame-Workspace",
                                            description = "Workspace id from POST /api/v1/workspace (32 hex characters).")),
    schemas = list(Error = ERROR_SCHEMA))

  paths <- spec$paths
  keep <- grepl("^/api/v1/", names(paths))
  paths <- paths[keep]
  # "/api/v1/runs/" → "/api/v1/runs" (both work; the short form reads better).
  names(paths) <- sub("(.)/$", "\\1", names(paths))
  for (p in names(paths)) {
    for (m in names(paths[[p]])) {
      op <- paths[[p]][[m]]
      key <- paste(toupper(m), p)
      op$tags <- list(.tag_of(p))
      op$operationId <- .op_id(m, p)
      if (!is.null(op$summary)) {
        # Keep the first sentence as the summary; the rest (and any "Body:" hint) becomes the description.
        full <- op$summary
        op$summary <- sub("(\\. |\\.$| Body:).*$", "", full)
        if (!identical(op$summary, sub("[.]$", "", full))) op$description <- full
      }
      for (i in seq_along(op$parameters)) {
        prm <- op$parameters[[i]]
        if (identical(prm$`in`, "path") && is.null(prm$description))
          prm$description <- switch(prm$name, id = "Id of the object (from the list or create call).", taxid = "NCBI taxonomy id.",
                                    kind = "Example kind.", NULL)
        op$parameters[[i]] <- prm
      }
      if (key %in% names(BODIES) && !is.null(BODIES[[key]])) {
        b <- BODIES[[key]]
        content <- list(schema = b[setdiff(names(b), "example")])
        if (!is.null(b$example)) content$example <- b$example
        op$requestBody <- list(required = TRUE, content = list(`application/json` = content))
      }
      op$security <- if (.is_public(m, p)) list() else list(list(workspace = list()))
      resp <- op$responses %||% list()
      resp$default <- NULL
      if (grepl("^POST /api/v1/(runs|analyses|variants)$", key)) {
        resp[["202"]] <- list(description = "Accepted: the job was queued. Poll its id for status and results.")
        resp[["200"]] <- NULL
      } else if (is.null(resp[["200"]])) {
        resp[["200"]] <- list(description = "OK")
      } else if (identical(resp[["200"]]$description, "OK") || is.null(resp[["200"]]$description)) {
        resp[["200"]]$description <- "OK"
      }
      resp[["400"]] <- .err("Invalid input.")
      if (!.is_public(m, p)) resp[["401"]] <- .err("Missing or invalid X-Flame-Workspace header.")
      if (grepl("\\{id\\}", p)) resp[["404"]] <- .err("Not found in this workspace.")
      resp[["429"]] <- .err("Too many requests or jobs; try again shortly.")
      resp[["500"]] <- .err("Server error.")
      op$responses <- resp[order(names(resp))]
      paths[[p]][[m]] <- op
    }
  }
  # Order paths by tag, then path.
  ord <- order(match(vapply(names(paths), .tag_of, ""), vapply(API_TAGS, `[[`, "", "name")), names(paths))
  spec$paths <- paths[ord]
  spec
}
