# =============================================================================
# FLAME core: variant analysis (SNPs, small variants, GWAS summary statistics)
# =============================================================================
# For users who start from variants rather than gene lists:
#   parse_variant_input()  rsIDs, VCF-style "chr pos ref alt", HGVS, or a
#                          GWAS summary-statistics table (SNP + p-value)
#   vep_annotate()         Ensembl VEP: consequence, impact, gene, SIFT,
#                          PolyPhen, gnomAD frequency, ClinVar significance
#   variant_phenotypes()   Ensembl Variation: trait/disease associations
#                          (GWAS Catalog, ClinVar, OMIM, ...)
#   analyse_variants()     everything above + summaries + gene-level scores
# Ensembl REST: https://rest.ensembl.org (GRCh38 for human).
# =============================================================================

ENSEMBL_REST <- "https://rest.ensembl.org"
VARIANT_MAX <- 2000L
IMPACT_ORDER <- c("HIGH", "MODERATE", "LOW", "MODIFIER")

# Species supported by Ensembl VEP REST that Flame offers (taxid -> Ensembl name)
VARIANT_SPECIES <- c(
  "9606" = "homo_sapiens", "10090" = "mus_musculus", "10116" = "rattus_norvegicus",
  "7955" = "danio_rerio", "9913" = "bos_taurus", "9823" = "sus_scrofa",
  "9031" = "gallus_gallus", "9615" = "canis_lupus_familiaris", "9544" = "macaca_mulatta"
)

.chr_ok <- function(x) grepl("^(chr)?([0-9]{1,2}|X|Y|MT|M)$", x, ignore.case = TRUE)

#' Parse variant input. Returns a data frame: input, kind (rsid|region|hgvs),
#' rsid, chr, pos, ref, alt, pvalue. A header with SNP/rsID and P columns turns
#' the input into GWAS summary statistics (p-values kept).
parse_variant_input <- function(text) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  if (nchar(text, type = "bytes") > 2 * MAX_INPUT_BYTES) stop("The input is larger than the 2 MB limit.")
  lines <- trimws(strsplit(text, "\r\n|\n|\r", perl = TRUE)[[1]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "##")]
  if (length(lines) == 0) stop("Paste variants: rsIDs, VCF lines (chr pos ref alt) or GWAS summary statistics.")

  # GWAS summary statistics: a header naming an ID/position column and a p-value column.
  header <- tolower(strsplit(sub("^#", "", lines[1]), "[\t, ]+")[[1]])
  pcol <- which(header %in% c("p", "pval", "p_value", "pvalue", "p-value", "p.value", "p_bolt_lmm"))[1]
  idcol <- which(header %in% c("snp", "rsid", "rs", "id", "variant_id", "markername", "snpid", "rs_id"))[1]
  chrcol <- which(header %in% c("chr", "chrom", "chromosome", "#chrom"))[1]
  poscol <- which(header %in% c("bp", "pos", "position", "base_pair_location"))[1]
  if (!is.na(pcol) && (!is.na(idcol) || (!is.na(chrcol) && !is.na(poscol)))) {
    rows <- strsplit(lines[-1], "[\t, ]+", perl = TRUE)
    get <- function(i) vapply(rows, function(r) if (is.na(i) || length(r) < i) NA_character_ else r[i], character(1))
    rs <- get(idcol); chr <- sub("^chr", "", get(chrcol), ignore.case = TRUE); pos <- suppressWarnings(as.integer(get(poscol)))
    p <- suppressWarnings(as.numeric(get(pcol)))
    isRs <- !is.na(rs) & grepl("^rs[0-9]+$", rs, ignore.case = TRUE)
    df <- data.frame(input = ifelse(isRs, rs, paste(chr, pos)), kind = ifelse(isRs, "rsid", "position"),
                     rsid = ifelse(isRs, tolower(rs), NA), chr = chr, pos = pos, ref = NA_character_,
                     alt = NA_character_, pvalue = p, stringsAsFactors = FALSE)
    df <- df[is.finite(df$pvalue) & df$pvalue > 0 & df$pvalue <= 1 & (isRs | (.chr_ok(chr) & !is.na(pos))), , drop = FALSE]
    if (nrow(df) == 0) stop("No rows with a variant ID (or chromosome and position) and a valid p-value were found.")
    df <- df[order(df$pvalue), , drop = FALSE]
    df <- df[!duplicated(df$input), , drop = FALSE]
    attr(df, "mode") <- "gwas"
    return(df)
  }

  if (!is.na(idcol) || grepl("^#?chrom", lines[1], ignore.case = TRUE)) lines <- lines[-1]
  out <- lapply(lines, function(l) {
    f <- strsplit(l, "[\t, ]+", perl = TRUE)[[1]]
    if (grepl("^rs[0-9]+$", f[1], ignore.case = TRUE))
      return(list(input = f[1], kind = "rsid", rsid = tolower(f[1]), chr = NA, pos = NA, ref = NA, alt = NA))
    if (grepl(":[cgnpm]\\.", f[1]))
      return(list(input = f[1], kind = "hgvs", rsid = NA, chr = NA, pos = NA, ref = NA, alt = NA))
    # chr:pos:ref:alt or chr-pos-ref-alt
    m <- regmatches(f[1], regexec("^(chr)?([0-9XYMT]+)[:-]([0-9]+)[:-]([ACGTN]+)[:/>-]([ACGTN]+)$", f[1], ignore.case = TRUE))[[1]]
    if (length(m) == 6) return(list(input = f[1], kind = "region", rsid = NA, chr = m[3], pos = as.integer(m[4]), ref = toupper(m[5]), alt = toupper(m[6])))
    # VCF-like: chr pos [id] ref alt
    if (length(f) >= 4 && .chr_ok(f[1]) && grepl("^[0-9]+$", f[2])) {
      hasId <- length(f) >= 5 && !grepl("^[ACGTN]+$", f[3], ignore.case = TRUE)
      ref <- if (hasId) f[4] else f[3]; alt <- strsplit(if (hasId) f[5] else f[4], ",")[[1]][1]
      if (grepl("^[ACGTN-]+$", ref, ignore.case = TRUE) && grepl("^[ACGTN-]+$", alt, ignore.case = TRUE))
        return(list(input = l, kind = "region", rsid = if (hasId && grepl("^rs", f[3])) tolower(f[3]) else NA,
                    chr = sub("^chr", "", f[1], ignore.case = TRUE), pos = as.integer(f[2]), ref = toupper(ref), alt = toupper(alt)))
    }
    NULL
  })
  bad <- vapply(out, is.null, logical(1))
  out <- out[!bad]
  if (length(out) == 0) stop("No variants recognised. Use rsIDs (rs123), chr:pos:ref:alt, VCF lines, or HGVS notation.")
  df <- do.call(rbind, lapply(out, function(x) as.data.frame(lapply(x, function(v) if (is.null(v)) NA else v), stringsAsFactors = FALSE)))
  df$pvalue <- NA_real_
  df <- df[!duplicated(df$input), , drop = FALSE]
  attr(df, "mode") <- "variants"
  attr(df, "skipped") <- sum(bad)
  df
}

.ensembl_post <- function(path, body, tries = 3) {
  for (i in seq_len(tries)) {
    resp <- tryCatch(httr::POST(paste0(ENSEMBL_REST, path), body = jsonlite::toJSON(body, auto_unbox = TRUE),
                                httr::content_type_json(), httr::accept_json(), httr::timeout(90)),
                     error = function(e) NULL)
    if (is.null(resp)) { Sys.sleep(1); next }
    code <- httr::status_code(resp)
    if (code == 429) { Sys.sleep(as.numeric(httr::headers(resp)[["retry-after"]] %||% 2)); next }
    if (code >= 500) { Sys.sleep(1); next }
    if (code != 200) return(NULL)
    return(jsonlite::fromJSON(rawToChar(httr::content(resp, "raw")), simplifyVector = FALSE))
  }
  NULL
}

.first <- function(x, default = NA) if (is.null(x) || length(x) == 0) default else x[[1]]

#' One row per input variant from a VEP response element
.vep_row <- function(v) {
  tcs <- v$transcript_consequences %||% list()
  # Prefer the canonical transcript, then the most severe impact.
  pick <- NULL
  if (length(tcs)) {
    imp <- vapply(tcs, function(t) match(t$impact %||% "MODIFIER", IMPACT_ORDER), numeric(1))
    can <- vapply(tcs, function(t) isTRUE(t$canonical == 1), logical(1))
    pick <- tcs[[order(imp, !can)[1]]]
  }
  coloc <- v$colocated_variants %||% list()
  rsids <- unique(unlist(lapply(coloc, function(c) if (grepl("^rs", c$id %||% "")) c$id)))
  clin <- unique(unlist(lapply(coloc, function(c) c$clin_sig)))
  af <- NA_real_
  for (c in coloc) {
    fr <- c$frequencies
    if (!is.null(fr) && length(fr)) {
      vals <- unlist(lapply(fr, function(a) a$gnomade %||% a$gnomadg %||% a$af))
      if (length(vals)) { af <- max(as.numeric(vals), na.rm = TRUE); break }
    }
  }
  genes <- unique(unlist(lapply(tcs, function(t) t$gene_symbol)))
  list(
    input = v$input %||% v$id, id = .first(rsids, v$id %||% ""),
    location = paste0(v$seq_region_name, ":", v$start, if (!identical(v$start, v$end)) paste0("-", v$end) else ""),
    chr = as.character(v$seq_region_name %||% NA), pos = as.integer(v$start %||% NA),
    alleles = v$allele_string %||% "",
    consequence = v$most_severe_consequence %||% "intergenic_variant",
    impact = pick$impact %||% "MODIFIER",
    gene = pick$gene_symbol %||% NA, geneId = pick$gene_id %||% NA,
    allGenes = paste(genes, collapse = ","),
    biotype = pick$biotype %||% NA,
    aminoAcids = pick$amino_acids %||% NA,
    sift = if (!is.null(pick$sift_prediction)) paste0(pick$sift_prediction, " (", pick$sift_score, ")") else NA,
    polyphen = if (!is.null(pick$polyphen_prediction)) paste0(pick$polyphen_prediction, " (", pick$polyphen_score, ")") else NA,
    gnomadAF = af,
    clinicalSignificance = if (length(clin)) paste(clin, collapse = ", ") else NA
  )
}

#' Annotate variants with Ensembl VEP (batches of 200)
vep_annotate <- function(df, species = "homo_sapiens") {
  rows <- list()
  batches <- function(x) split(x, ceiling(seq_along(x) / 200))
  opts <- list(canonical = 1, hgvs = 0, variant_class = 1)
  # Progress: Ensembl VEP calls take ~5-70 % of the analysis, one step per batch of 200.
  totalBatches <- sum(ceiling(table(df$kind) / 200)); doneBatches <- 0
  tick <- function() {
    doneBatches <<- doneBatches + 1
    flame_progress(0.05 + 0.65 * doneBatches / max(1, totalBatches),
                   sprintf("Annotating with Ensembl VEP: batch %d of %d", min(doneBatches + 1, totalBatches), totalBatches))
  }
  flame_progress(0.05, sprintf("Annotating %d variants with Ensembl VEP", nrow(df)))
  for (kind in c("rsid", "region", "hgvs", "position")) {
    sub <- df[df$kind == kind, , drop = FALSE]
    if (nrow(sub) == 0) next
    if (kind == "rsid") {
      for (b in batches(sub$rsid)) {
        res <- .ensembl_post(paste0("/vep/", species, "/id"), c(list(ids = as.list(b)), opts))
        rows <- c(rows, lapply(res %||% list(), .vep_row)); tick()
      }
    } else if (kind == "hgvs") {
      for (b in batches(sub$input)) {
        res <- .ensembl_post(paste0("/vep/", species, "/hgvs"), c(list(hgvs_notations = as.list(b)), opts))
        rows <- c(rows, lapply(res %||% list(), .vep_row)); tick()
      }
    } else {
      # region/position: VCF-style strings "chr pos id ref alt . . ."; positions without
      # alleles are looked up as a placeholder SNV so overlapping genes are still reported.
      # The VCF ID field carries an index (inputs may contain spaces); map back afterwards.
      idx <- paste0("v", seq_len(nrow(sub)))
      vcf <- ifelse(is.na(sub$ref), paste(sub$chr, sub$pos, idx, "N", "A", ". . ."),
                    paste(sub$chr, sub$pos, idx, sub$ref, sub$alt, ". . ."))
      for (b in batches(vcf)) {
        res <- .ensembl_post(paste0("/vep/", species, "/region"), c(list(variants = as.list(b)), opts))
        for (v in res %||% list()) {
          r <- .vep_row(v)
          k <- match(strsplit(v$input %||% "", " ")[[1]][3], idx)
          if (!is.na(k)) r$input <- sub$input[k]
          rows[[length(rows) + 1]] <- r
        }
        tick()
      }
    }
  }
  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, lapply(rows, function(r) as.data.frame(lapply(r, function(v) if (is.null(v) || length(v) == 0) NA else v), stringsAsFactors = FALSE)))
  out$impact <- factor(out$impact, levels = IMPACT_ORDER)
  out
}

#' Trait/disease associations for rsIDs (Ensembl Variation, phenotypes=1)
variant_phenotypes <- function(rsids, species = "homo_sapiens") {
  rsids <- unique(rsids[!is.na(rsids) & grepl("^rs[0-9]+$", rsids)])
  if (length(rsids) == 0) return(data.frame())
  out <- list()
  bs <- split(rsids, ceiling(seq_along(rsids) / 200))
  for (bi in seq_along(bs)) {
    b <- bs[[bi]]
    flame_progress(0.72 + 0.23 * (bi - 1) / length(bs), sprintf("Looking up trait associations (GWAS Catalog, ClinVar, OMIM…): batch %d of %d", bi, length(bs)))
    res <- .ensembl_post(paste0("/variation/", species, "?phenotypes=1"), list(ids = as.list(b)))
    for (id in names(res %||% list())) {
      for (ph in res[[id]]$phenotypes %||% list()) {
        out[[length(out) + 1]] <- list(
          variant = id, trait = ph$trait %||% NA, source = ph$source %||% NA,
          riskAllele = ph$risk_allele %||% NA, pvalue = suppressWarnings(as.numeric(ph$pvalue %||% NA)),
          study = ph$study %||% NA, genes = ph$genes %||% NA)
      }
    }
  }
  if (length(out) == 0) return(data.frame())
  df <- do.call(rbind, lapply(out, function(r) as.data.frame(lapply(r, function(v) if (is.null(v) || length(v) == 0) NA else as.character(v)[1]), stringsAsFactors = FALSE)))
  df$pvalue <- suppressWarnings(as.numeric(df$pvalue))
  df <- df[!is.na(df$trait) & nzchar(df$trait), , drop = FALSE]
  # Drop placeholder traits that carry no information.
  df[!grepl("not (specified|provided)|^none$|^unknown$", df$trait, ignore.case = TRUE), , drop = FALSE]
}

#' Keep the strongest associations per variant (traits are summarised from all)
.top_phenotypes <- function(ph, per = 50L) {
  if (!nrow(ph)) return(ph)
  ph <- ph[order(ph$variant, is.na(ph$pvalue), ph$pvalue), , drop = FALSE]
  ph[stats::ave(seq_len(nrow(ph)), ph$variant, FUN = seq_along) <= per, , drop = FALSE]
}

.count_table <- function(x, name = "value") {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(list())
  t <- sort(table(x), decreasing = TRUE)
  lapply(seq_along(t), function(i) stats::setNames(list(names(t)[i], as.integer(t[i])), c(name, "count")))
}

#' Full analysis: annotation, associations, summaries and gene-level results
analyse_variants <- function(text, taxid = 9606L) {
  species <- VARIANT_SPECIES[[as.character(taxid)]] %||% stop("Variant annotation is not available for this organism.")
  input <- parse_variant_input(text)
  mode <- attr(input, "mode")
  if (nrow(input) > VARIANT_MAX) {
    if (mode == "gwas") input <- input[seq_len(VARIANT_MAX), , drop = FALSE]   # keep the most significant
    else stop(sprintf("At most %d variants can be analysed at once.", VARIANT_MAX))
  }
  ann <- vep_annotate(input, species)
  if (nrow(ann) == 0) stop("Ensembl VEP could not annotate these variants. Check the IDs or try again later.")

  # Carry GWAS p-values onto the annotation.
  if (mode == "gwas") {
    key <- ifelse(input$kind == "rsid", input$rsid, input$input)
    ann$pvalue <- input$pvalue[match(tolower(ann$input), tolower(key))]
  } else ann$pvalue <- NA_real_

  ph <- if (species == "homo_sapiens") variant_phenotypes(unique(c(ann$id, input$rsid)), species) else data.frame()

  variant_report(mode, taxid, species, nrow(input), attr(input, "skipped") %||% 0L, ann, .top_phenotypes(ph))
}

#' Gene table and summaries from annotated variants and their phenotypes. Also
#' used to rebuild an analysis loaded from a session file.
variant_report <- function(mode, taxid, species, inputCount, skipped, ann, ph) {
  # Gene table: variants per gene, worst impact, best p-value (all genes the variant touches).
  pairs <- do.call(rbind, lapply(seq_len(nrow(ann)), function(i) {
    g <- unique(strsplit(ann$allGenes[i] %||% "", ",")[[1]])
    g <- g[nzchar(g) & !is.na(g)]
    if (!length(g)) return(NULL)
    data.frame(gene = g, variant = ann$id[i], impact = as.character(ann$impact[i]), pvalue = ann$pvalue[i], stringsAsFactors = FALSE)
  }))
  genes <- if (is.null(pairs)) data.frame() else {
    agg <- lapply(split(pairs, pairs$gene), function(d) data.frame(
      gene = d$gene[1], variants = length(unique(d$variant)),
      worstImpact = IMPACT_ORDER[min(match(d$impact, IMPACT_ORDER))],
      bestP = if (all(is.na(d$pvalue))) NA_real_ else min(d$pvalue, na.rm = TRUE),
      stringsAsFactors = FALSE))
    g <- do.call(rbind, agg)
    g[order(match(g$worstImpact, IMPACT_ORDER), -g$variants, g$bestP), , drop = FALSE]
  }
  genes$score <- if (nrow(genes) && mode == "gwas") -log10(pmax(genes$bestP, 1e-300)) else NA_real_

  traitSummary <- if (nrow(ph)) {
    agg <- lapply(split(ph, tolower(ph$trait)), function(d) list(
      trait = d$trait[1], variants = length(unique(d$variant)), sources = paste(sort(unique(d$source)), collapse = ", "),
      bestP = if (all(is.na(d$pvalue))) NA else min(d$pvalue, na.rm = TRUE)))
    agg <- agg[order(-vapply(agg, `[[`, numeric(1), "variants"))]
    unname(agg)
  } else list()

  list(
    mode = mode, organismTaxId = taxid, species = species,
    inputCount = inputCount, annotatedCount = nrow(ann),
    skipped = skipped,
    variants = ann, phenotypes = ph, genes = genes,
    summary = list(
      consequences = .count_table(ann$consequence, "consequence"),
      impacts = lapply(IMPACT_ORDER, function(i) list(impact = i, count = sum(ann$impact == i, na.rm = TRUE))),
      chromosomes = .count_table(ann$chr, "chromosome"),
      clinical = .count_table(unlist(strsplit(stats::na.omit(ann$clinicalSignificance), ",\\s*")), "significance"),
      traits = traitSummary
    )
  )
}

VARIANT_TEXT_COLUMNS <- c("input", "id", "location", "chr", "alleles", "consequence", "impact", "gene", "geneId",
                          "allGenes", "biotype", "aminoAcids", "sift", "polyphen", "clinicalSignificance")
VARIANT_NUM_COLUMNS <- c("pos", "gnomadAF", "pvalue")
PHENOTYPE_TEXT_COLUMNS <- c("variant", "trait", "source", "riskAllele", "study")

.typed_df <- function(records, textCols, numCols, required) {
  df <- if (is.data.frame(records)) records else if (length(records)) {
    jsonlite::fromJSON(jsonlite::toJSON(records, auto_unbox = TRUE, null = "null", na = "null"), simplifyVector = TRUE)
  } else NULL
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(data.frame())
  if (!all(required %in% names(df))) stop("missing columns")
  out <- data.frame(row.names = seq_len(nrow(df)))
  for (col in textCols) out[[col]] <- if (col %in% names(df)) substr(gsub("[[:cntrl:]]", " ", as.character(df[[col]])), 1, 500) else NA_character_
  for (col in numCols) out[[col]] <- if (col %in% names(df)) suppressWarnings(as.numeric(df[[col]])) else NA_real_
  out
}

#' Rebuild a variant analysis from session-file JSON. Only known columns with
#' plain types are kept; gene table and summaries are recomputed.
variant_result_from_json <- function(x) {
  if (!is.list(x)) stop("bad analysis")
  ann <- .typed_df(x$variants, VARIANT_TEXT_COLUMNS, VARIANT_NUM_COLUMNS, c("id", "consequence", "impact"))
  if (!nrow(ann)) stop("no variants")
  if (nrow(ann) > VARIANT_MAX) ann <- ann[seq_len(VARIANT_MAX), , drop = FALSE]
  ann$impact <- ifelse(ann$impact %in% IMPACT_ORDER, ann$impact, "MODIFIER")
  ann$pos <- as.integer(ann$pos)
  ph <- .typed_df(x$phenotypes, PHENOTYPE_TEXT_COLUMNS, "pvalue", c("variant", "trait"))
  mode <- if (identical(x$mode, "gwas")) "gwas" else "variants"
  taxid <- suppressWarnings(as.integer(x$organismTaxId)); if (length(taxid) != 1 || is.na(taxid)) taxid <- 9606L
  species <- VARIANT_SPECIES[[as.character(taxid)]] %||% "homo_sapiens"
  n <- suppressWarnings(as.integer(x$inputCount)); if (length(n) != 1 || is.na(n)) n <- nrow(ann)
  variant_report(mode, taxid, species, n, 0L, ann, ph)
}
