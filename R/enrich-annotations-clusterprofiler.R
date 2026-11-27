# =============================================================================
# clusterProfiler annotation sources: where each FLAME source comes from
# =============================================================================
#   GO:BP / GO:MF / GO:CC  enrichGO / gseGO on the organism's Bioconductor OrgDb
#                          (org.*.eg.db: current GO annotations); organisms without
#                          an OrgDb use MSigDB's GO collections instead.
#   KEGG                   enrichKEGG / gseKEGG on current KEGG pathways, downloaded
#                          from the KEGG REST API by clusterProfiler (gson_KEGG) and
#                          kept on disk for FLAME_KEGG_CACHE_DAYS (default 7).
#   everything else        MSigDB via msigdbr; mouse uses MSigDB's own mouse
#                          collections (MH, M1-M8) where they exist.
# =============================================================================

# NCBI taxonomy id -> Bioconductor OrgDb package (GO annotations keyed by NCBI Gene ID)
CP_ORGDB <- c(
  "9606" = "org.Hs.eg.db", "10090" = "org.Mm.eg.db", "10116" = "org.Rn.eg.db", "7955" = "org.Dr.eg.db",
  "7227" = "org.Dm.eg.db", "6239" = "org.Ce.eg.db", "4932" = "org.Sc.sgd.db", "559292" = "org.Sc.sgd.db",
  "9913" = "org.Bt.eg.db", "9615" = "org.Cf.eg.db", "9031" = "org.Gg.eg.db", "9823" = "org.Ss.eg.db",
  "9544" = "org.Mmu.eg.db", "9598" = "org.Pt.eg.db"
)

# NCBI taxonomy id -> KEGG organism code (all clusterProfiler organisms)
CP_KEGG_CODE <- c(
  "9606" = "hsa", "10090" = "mmu", "10116" = "rno", "7955" = "dre", "7227" = "dme", "6239" = "cel",
  "4932" = "sce", "559292" = "sce", "9913" = "bta", "9615" = "cfa", "9031" = "gga", "9823" = "ssc",
  "9544" = "mcc", "9598" = "ptr", "9796" = "ecb", "9685" = "fca", "8364" = "xtr", "13616" = "mdo",
  "9258" = "oaa", "28377" = "acs", "284812" = "spo"
)

CP_GO_ONT <- c("GO:BP" = "BP", "GO:MF" = "MF", "GO:CC" = "CC")

# MSigDB mouse collections (db_species = "MM") for FLAME sources that have one;
# other sources use the human collections mapped to mouse by orthology.
MSIGDB_MOUSE_CODES <- list(
  "GO:BP" = c("M5", "GO:BP"), "GO:MF" = c("M5", "GO:MF"), "GO:CC" = c("M5", "GO:CC"),
  "REAC" = c("M2", "CP:REACTOME"), "WP" = c("M2", "CP:WIKIPATHWAYS"), "BIOCARTA" = c("M2", "CP:BIOCARTA"),
  "CGP" = c("M2", "CGP"), "TF" = c("M3", "GTRD"), "MIRNA" = c("M3", "MIRDB"),
  "HALLMARK" = c("MH", ""), "POSITIONAL" = c("M1", ""), "CELLTYPE" = c("M8", ""), "IMMUNESIGDB" = c("M7", "")
)

#' The OrgDb object for an organism, or NULL when none is installed / known.
cp_orgdb <- function(taxid) {
  pkg <- CP_ORGDB[as.character(taxid)]
  if (is.na(pkg) || !requireNamespace(pkg, quietly = TRUE)) return(NULL)
  getExportedValue(pkg, pkg)
}

#' Which engine serves each requested source for this organism.
#' Returns list(go = <GO sources via OrgDb>, kegg = TRUE/FALSE, msigdb = <sources via MSigDB>).
cp_source_plan <- function(taxid, sources) {
  sources <- unique(as.character(sources))
  go <- if (!is.null(cp_orgdb(taxid))) intersect(sources, names(CP_GO_ONT)) else character(0)
  kegg <- "KEGG" %in% sources && !is.na(CP_KEGG_CODE[as.character(taxid)])
  list(go = go, kegg = kegg, msigdb = setdiff(sources, c(go, if (kegg) "KEGG")))
}

# ---- KEGG: clusterProfiler's download, cached on disk ---------------------------

.kegg_mem <- new.env()

flame_cache_dir <- function(...) {
  d <- file.path(Sys.getenv("FLAME_CACHE_DIR", tools::R_user_dir("flame", "cache")), ...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' KEGG release line ("Last update 2026/11/24"), or "" when KEGG cannot say.
kegg_release <- function() {
  info <- tryCatch(readLines("https://rest.kegg.jp/info/pathway", warn = FALSE), error = function(e) character(0))
  hit <- grep("Last update|Release", info, value = TRUE)
  if (length(hit)) trimws(hit[1]) else ""
}

#' Current KEGG pathways of an organism as a GSON object on NCBI Gene IDs (for enrichKEGG /
#' gseKEGG). Downloaded with clusterProfiler::gson_KEGG and reused for FLAME_KEGG_CACHE_DAYS;
#' if KEGG cannot be reached, an older copy is used (with a note) rather than failing.
kegg_gson <- function(taxid, maxAgeDays = as.numeric(Sys.getenv("FLAME_KEGG_CACHE_DAYS", "7"))) {
  code <- CP_KEGG_CODE[as.character(taxid)]
  if (is.na(code)) stop("KEGG has no pathways for this organism.")
  f <- file.path(flame_cache_dir("kegg"), paste0(code, "_ncbi-geneid.rds"))
  fresh <- function(at) !is.null(at) && difftime(Sys.time(), at, units = "days") < maxAgeDays
  m <- .kegg_mem[[code]]
  if (!is.null(m) && fresh(m$downloaded)) return(m)
  if (is.null(m) && file.exists(f)) m <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.null(m) && fresh(m$downloaded)) { .kegg_mem[[code]] <- m; return(m) }
  got <- tryCatch({
    g <- suppressMessages(clusterProfiler::gson_KEGG(code, KEGG_Type = "KEGG", keyType = "ncbi-geneid"))
    list(gson = g, downloaded = Sys.time(), release = kegg_release(), code = code, stale = FALSE)
  }, error = function(e) e)
  if (!inherits(got, "error")) {
    tmp <- paste0(f, ".tmp")
    saveRDS(got, tmp); file.rename(tmp, f)   # atomic: several workers may download at once
    .kegg_mem[[code]] <- got
    return(got)
  }
  if (!is.null(m)) { m$stale <- TRUE; .kegg_mem[[code]] <- m; return(m) }
  stop("KEGG could not be reached (", conditionMessage(got), "). Try again later or choose other sources.")
}

#' A sentence on the KEGG data a run used, for the run's messages.
kegg_note <- function(k) {
  when <- format(k$downloaded, "%Y-%m-%d")
  paste0("KEGG pathways from KEGG (", if (nzchar(k$release %||% "")) paste0(k$release, ", ") else "",
         "downloaded ", when, ")", if (isTRUE(k$stale)) "; KEGG could not be reached, so this saved copy was used." else ".")
}

#' A sentence on the GO data a run used (the organism's OrgDb build).
go_note <- function(taxid) {
  pkg <- unname(CP_ORGDB[as.character(taxid)])
  md <- tryCatch(AnnotationDbi::metadata(cp_orgdb(taxid)), error = function(e) NULL)
  val <- function(n) if (!is.null(md) && any(md$name == n)) md$value[md$name == n][1] else ""
  go <- val("GOSOURCEDATE"); genes <- val("GOEGSOURCEDATE")
  paste0("GO terms from ", pkg, " ", as.character(utils::packageVersion(pkg)),
         if (nzchar(go)) paste0(" (Gene Ontology of ", go, if (nzchar(genes)) paste0(", gene annotations of ", genes), ")"), ".")
}

#' NCBI Gene ID -> gene symbol, from the OrgDb (or g:Profiler for organisms without one).
cp_symbols <- function(taxid, ids) {
  ids <- unique(as.character(ids[!is.na(ids) & nzchar(ids)]))
  out <- stats::setNames(ids, ids)
  if (!length(ids)) return(out)
  db <- cp_orgdb(taxid)
  sym <- if (!is.null(db)) {
    tryCatch(suppressMessages(AnnotationDbi::mapIds(db, keys = ids, keytype = "ENTREZID",
                                                    column = if (CP_ORGDB[as.character(taxid)] == "org.Sc.sgd.db") "GENENAME" else "SYMBOL",
                                                    multiVals = "first")), error = function(e) NULL)
  } else {
    short <- ORGANISMS$short_name[which(ORGANISMS$taxid == taxid)][1]
    conv <- tryCatch(suppressMessages(gprofiler2::gconvert(ids, organism = short, target = "ENTREZGENE", numeric_ns = "ENTREZGENE_ACC",
                                                            mthreshold = 1, filter_na = TRUE)),
                     error = function(e) NULL)
    if (!is.null(conv) && nrow(conv)) stats::setNames(as.character(conv$name), conv$input) else NULL
  }
  if (!is.null(sym)) {
    ok <- !is.na(sym) & nzchar(sym) & names(sym) %in% ids
    out[names(sym)[ok]] <- sym[ok]
  }
  out
}

# ---- Name -> ID lookups (for Enrichr libraries whose terms carry no IDs) ----------------

.lookup_mem <- new.env()

#' A small table downloaded once and kept on disk for FLAME_KEGG_CACHE_DAYS (NULL if unreachable
#' and never saved). `fetch` returns a data frame.
.cached_table <- function(name, fetch, maxAgeDays = as.numeric(Sys.getenv("FLAME_KEGG_CACHE_DAYS", "7"))) {
  m <- .lookup_mem[[name]]
  if (!is.null(m) && difftime(Sys.time(), m$at, units = "days") < maxAgeDays) return(m$tab)
  f <- file.path(flame_cache_dir("lookups"), paste0(name, ".rds"))
  if (is.null(m) && file.exists(f)) m <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.null(m) && difftime(Sys.time(), m$at, units = "days") < maxAgeDays) { .lookup_mem[[name]] <- m; return(m$tab) }
  tab <- tryCatch(fetch(), error = function(e) NULL)
  if (!is.null(tab) && nrow(tab)) {
    m <- list(tab = tab, at = Sys.time())
    tmp <- paste0(f, ".tmp"); saveRDS(m, tmp); file.rename(tmp, f)
  }
  if (!is.null(m)) .lookup_mem[[name]] <- m
  m$tab
}

.norm_name <- function(x) gsub("[^a-z0-9]+", " ", tolower(trimws(x)))

#' KEGG pathways of an organism: data frame id (e.g. hsa04110), name ("Cell cycle").
kegg_pathway_names <- function(code) {
  .cached_table(paste0("kegg_pathways_", code), function() {
    x <- readLines(paste0("https://rest.kegg.jp/list/pathway/", code), warn = FALSE)
    p <- strsplit(x[nzchar(x)], "\t", fixed = TRUE)
    data.frame(id = vapply(p, `[`, "", 1), name = sub(" - [^-]+\\([^)]*\\)$", "", vapply(p, `[`, "", 2)), stringsAsFactors = FALSE)
  })
}

#' Reactome pathways of one species: data frame id (R-HSA-...), name.
reactome_pathway_names <- function(species = "Homo sapiens") {
  all <- .cached_table("reactome_pathways", function() {
    x <- utils::read.delim("https://reactome.org/download/current/ReactomePathways.txt", header = FALSE,
                           quote = "", stringsAsFactors = FALSE, col.names = c("id", "name", "species"))
    x
  })
  if (is.null(all)) return(NULL)
  all[all$species == species, c("id", "name"), drop = FALSE]
}

#' IDs (and the database's own spelling of the name) for term names, matched ignoring case and
#' punctuation; NA where nothing matches.
lookup_term_ids <- function(terms, table) {
  if (is.null(table) || !nrow(table)) return(list(ids = rep(NA_character_, length(terms)), names = terms))
  i <- match(.norm_name(terms), .norm_name(table$name))
  list(ids = table$id[i], names = ifelse(is.na(i), terms, table$name[i]))
}
