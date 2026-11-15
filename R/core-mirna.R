# =============================================================================
# FLAME core: from microRNAs to their target genes
# =============================================================================
# Start from microRNAs (e.g. differentially expressed miRNAs) and get the genes
# they are predicted to regulate, using the MSigDB miRDB collection (miRDB v6,
# MirTarget scores > 80, via msigdbr). The target genes (all of them, or only
# those targeted by several of the miRNAs) become a gene list for enrichment.
# For mouse and other species msigdbr maps the human predictions by orthology.
# =============================================================================

MIRNA_MAX <- 500L
.mirna_cache <- new.env()

#' Normalise a miRNA name: "hsa-miR-21-5p" / "MIR21" / "let-7a" -> "mir-21-5p" / "mir-21" / "let-7a"
mirna_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^[a-z]{3,4}-(?=(mir|let))", "", x, perl = TRUE)        # species prefix (hsa-, mmu-, rno-…)
  x <- sub("^mir-?", "mir-", x)
  x <- sub("^let-?", "let-", x)
  x <- sub("-?(5p|3p)$", "-\\1", x)
  x
}

#' miRNA -> target genes index for an organism (cached)
mirna_index <- function(taxid = 9606L) {
  species <- MSIGDB_SPECIES[[as.character(taxid)]]
  if (is.null(species)) stop("microRNA targets are available for the organisms MSigDB covers (human, mouse, rat, zebrafish, fly, worm, yeast and others).")
  if (!is.null(.mirna_cache[[species]])) return(.mirna_cache[[species]])
  raw <- as.data.frame(suppressMessages(msigdbr::msigdbr(species = species, collection = "C3", subcollection = "MIR:MIRDB")))
  if (!nrow(raw)) stop("No microRNA target sets are available for this organism.")
  sets <- split(as.character(raw$gene_symbol), raw$gs_name)
  desc <- tapply(as.character(raw$gs_description), raw$gs_name, `[`, 1)
  members <- lapply(names(sets), function(s) {
    toks <- regmatches(desc[[s]], gregexpr("[a-z]{3}-(miR|let)-[0-9A-Za-z-]+", desc[[s]]))[[1]]
    toks <- sub("-$", "", toks)
    if (!length(toks)) toks <- tolower(gsub("_", "-", s))
    unique(mirna_key(toks))
  })
  long <- data.frame(key = unlist(members), set = rep(names(sets), lengths(members)), stringsAsFactors = FALSE)
  idx <- list(species = species, sets = lapply(sets, unique), long = long, orthology = species != "Homo sapiens")
  .mirna_cache[[species]] <- idx
  idx
}

#' Target genes of a set of miRNAs.
#' Input names without an arm (miR-21) match both -5p and -3p.
mirna_targets <- function(text, taxid = 9606L, minMirnas = 1L) {
  ids <- unique(trimws(unlist(strsplit(paste(as.character(text), collapse = "\n"), "[\r\n,;\t ]+"))))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) stop("Paste microRNA names, e.g. hsa-miR-21-5p, one per line.")
  if (length(ids) > MIRNA_MAX) stop(sprintf("At most %d microRNAs at a time.", MIRNA_MAX))
  idx <- mirna_index(taxid)
  per <- lapply(ids, function(id) {
    k <- mirna_key(id)
    hit <- idx$long$set[idx$long$key == k]
    if (!length(hit) && !grepl("-(5p|3p)$", k)) hit <- idx$long$set[idx$long$key %in% paste0(k, c("-5p", "-3p"))]
    hit <- unique(hit)
    genes <- unique(unlist(idx$sets[hit]))
    list(input = id, matched = as.list(hit), targets = length(genes), genes = genes)
  })
  ok <- vapply(per, function(p) p$targets > 0, logical(1))
  if (!any(ok)) stop("None of the microRNAs were found in miRDB. Use miRBase names such as hsa-miR-21-5p or hsa-let-7a-5p.")
  pairs <- do.call(rbind, lapply(per[ok], function(p) data.frame(gene = p$genes, mirna = p$input, stringsAsFactors = FALSE)))
  byGene <- split(pairs$mirna, pairs$gene)
  n <- lengths(byGene)
  o <- order(-n, names(byGene))
  genes <- lapply(o, function(i) list(gene = names(byGene)[i], mirnas = as.list(byGene[[i]]), count = n[[i]]))
  minMirnas <- max(1L, as.integer(minMirnas))
  list(
    organismTaxId = as.integer(taxid), orthology = idx$orthology,
    mirnas = lapply(per, function(p) list(input = p$input, matched = p$matched, targets = p$targets)),
    unmatched = as.list(vapply(per[!ok], `[[`, "", "input")),
    genes = genes, totalTargets = length(byGene),
    selected = as.list(names(byGene)[n >= minMirnas]), minMirnas = minMirnas,
    source = "miRDB v6.0 predictions (MirTarget score > 80) from MSigDB C3 MIR:MIRDB"
  )
}

#' Example: microRNAs often reported in cancer
example_mirnas <- function() {
  list(text = paste(c("hsa-miR-21-5p", "hsa-miR-155-5p", "hsa-let-7a-5p", "hsa-miR-34a-5p", "hsa-miR-17-5p",
                      "hsa-miR-145-5p", "hsa-miR-200c-3p", "hsa-miR-125b-5p", "hsa-miR-221-3p", "hsa-miR-92a-3p"), collapse = "\n"),
       fileName = "flame_example_mirnas.txt")
}
