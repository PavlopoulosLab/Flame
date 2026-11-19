# Offline tests for ranked lists, variant input parsing and the Arena3D / NORMA
# payload builders. Network-dependent checks (GSEA on MSigDB) run only when
# FLAME_ONLINE_TESTS=true.

test_that("ranked lists keep IDs with finite scores, first occurrence wins", {
  r <- parse_ranked_text("gene\tscore\nTP53\t3.2\nMDM2,-1.5\ntp53 9\nBAD\tNA\n$$$ 1\nEGFR 0")
  expect_equal(r$ids, c("TP53", "MDM2", "EGFR"))
  expect_equal(r$scores, c(3.2, -1.5, 0))
  expect_equal(r$duplicates, 1)
  # header row "gene score" is invalid (non-numeric score) unless skipped
  expect_equal(r$invalid, 3)
  r2 <- parse_ranked_text("gene\tscore\nTP53\t3.2", skipFirstRow = TRUE)
  expect_equal(r2$invalid, 0)
  expect_equal(r2$ids, "TP53")
})

test_that("variant input: rsIDs, chr:pos:ref:alt, VCF lines and HGVS", {
  v <- parse_variant_input(paste(
    "##fileformat=VCFv4.2",
    "rs429358",
    "RS7412",
    "chr19:44908684:T>C",
    "7-140753336-A-T",
    "1\t230710048\trs699\tA\tG",
    "17 7676154 G C,T",
    "ENST00000269305.9:c.215C>G",
    "not a variant",
    "rs429358",
    sep = "\n"))
  expect_equal(attr(v, "mode"), "variants")
  expect_equal(attr(v, "skipped"), 1)
  expect_equal(nrow(v), 7)
  expect_equal(v$kind, c("rsid", "rsid", "region", "region", "region", "region", "hgvs"))
  expect_equal(v$rsid[2], "rs7412")
  region <- v[v$input == "chr19:44908684:T>C", ]
  expect_equal(c(region$chr, region$ref, region$alt), c("19", "T", "C"))
  expect_equal(region$pos, 44908684L)
  vcf <- v[v$pos %in% 230710048L, ]
  expect_equal(vcf$rsid, "rs699")
  expect_equal(v$alt[v$pos %in% 7676154L], "C")   # first ALT allele only
  expect_error(parse_variant_input("hello\nworld"), "No variants recognised")
})

test_that("GWAS summary statistics are detected, filtered and sorted by p", {
  g <- parse_variant_input(paste(
    "SNP\tCHR\tBP\tP",
    "rs6857\t19\t44888997\t2.1e-60",
    "rs429358\t19\t44908684\t1.2e-120",
    "rs1\t1\t100\t1.5",          # invalid p
    ".\t2\t127137039\t2.3e-10",   # no rsID: chromosome + position
    "rs6857\t19\t44888997\t0.5",  # duplicate, weaker
    sep = "\n"))
  expect_equal(attr(g, "mode"), "gwas")
  expect_equal(g$input, c("rs429358", "rs6857", "2 127137039"))
  expect_equal(g$kind, c("rsid", "rsid", "position"))
  expect_equal(g$pvalue[2], 2.1e-60)
})

test_that("Arena3D body: layers, clipping, colour/url validation, unconnected nodes dropped", {
  edges <- data.frame(source = c("A", "A"), target = c("B", paste(rep("x", 300), collapse = "")),
                      source_layer = "L1", target_layer = c("L1", "L2"), weight = c(0.5, NA),
                      stringsAsFactors = FALSE)
  nodes <- data.frame(name = c("A", "B", "lonely", "A"), layer = c("L1", "L1", "L1", "L1"),
                      color = c("#ff0000", "red", NA, "#00ff00"), size = c(9, 0.01, 1, 1),
                      url = c("https://x.org/A", "javascript:alert(1)", NA, NA), description = NA,
                      stringsAsFactors = FALSE)
  b <- arena3d_body(edges, nodes)
  expect_length(b$edges, 2)
  expect_equal(nchar(b$edges[[2]]$target), 200)
  expect_equal(b$edges[[1]]$weight, 0.5)
  expect_null(b$edges[[2]]$weight)
  expect_null(b$edges[[1]]$channel)
  names <- vapply(b$nodes, `[[`, "", "name")
  expect_equal(names, c("A", "B"))                 # duplicate A and unconnected node removed
  expect_equal(b$nodes[[1]]$color, "#ff0000")
  expect_null(b$nodes[[2]]$color)                  # "red" is not #rrggbb
  expect_equal(b$nodes[[1]]$size, 5)               # clamped to 0.2–5
  expect_equal(b$nodes[[2]]$size, 0.2)
  expect_equal(b$nodes[[1]]$url, "https://x.org/A")
  expect_null(b$nodes[[2]]$url)                    # non-http(s) link dropped
  expect_false(b$directed)
  expect_error(arena3d_body(edges[0, ]), "no edges")
})

test_that("Arena3D payloads for result networks put sources and genes on layers", {
  r <- fake_results()
  tg <- arena3d_payload_for_view(r, "network-tg", top = 10)
  layers <- unique(c(vapply(tg$edges, `[[`, "", "source_layer"), vapply(tg$edges, `[[`, "", "target_layer")))
  expect_setequal(layers, c("GO:BP", "KEGG", "REAC", "Genes"))
  expect_equal(sum(vapply(tg$edges, function(e) e$target_layer == "Genes", logical(1))), 15)  # total gene hits
  # every styled node takes part in an edge
  inNet <- unique(unlist(lapply(tg$edges, function(e) c(paste(e$source, e$source_layer), paste(e$target, e$target_layer)))))
  expect_true(all(vapply(tg$nodes, function(n) paste(n$name, n$layer) %in% inNet, logical(1))))
  expect_true(all(vapply(tg$nodes, function(n) is.null(n$url) || grepl("^https?://", n$url), logical(1))))

  hm <- arena3d_payload_for_view(r, "heatmap-tg", top = 10)
  expect_equal(length(hm$edges), length(tg$edges))
  gg <- arena3d_payload_for_view(r, "network-gg", top = 10, settings = list(minShared = 1))
  expect_true(all(vapply(gg$edges, function(e) e$source_layer == "Genes" && e$target_layer == "Genes", logical(1))))
})

test_that("Arena3D payloads for variant reports and STRING networks", {
  result <- list(
    variants = data.frame(id = c("rs1", "rs2", "1_100_A/G"), allGenes = c("APOE,TOMM40", "APOE", ""),
                          impact = factor(c("HIGH", "MODIFIER", "LOW")), consequence = "missense_variant",
                          clinicalSignificance = c("pathogenic", NA, NA), stringsAsFactors = FALSE),
    phenotypes = data.frame(variant = c("rs1", "rs1", "rs2"), trait = c("Alzheimer disease", "LDL cholesterol", "Rare trait"),
                            stringsAsFactors = FALSE),
    summary = list(traits = list(list(trait = "Alzheimer disease"), list(trait = "LDL cholesterol")))
  )
  b <- arena3d_payload_for_variants(result, topTraits = 2)
  layers <- vapply(b$edges, `[[`, "", "target_layer")
  expect_equal(sum(layers == "Genes"), 3)
  expect_equal(sort(vapply(b$edges[layers == "Traits"], `[[`, "", "target")), c("Alzheimer disease", "LDL cholesterol"))
  rs1 <- Filter(function(n) n$name == "rs1", b$nodes)[[1]]
  expect_equal(rs1$color, "#dc2626")
  expect_match(rs1$url, "^https://www\\.ensembl\\.org/")
  expect_false(any(vapply(b$nodes, function(n) n$name == "1_100_A/G", logical(1))))  # no genes, no edges

  net <- list(edges = data.frame(from = c("A", "B"), to = c("B", "C"), score = c(0.9, 0.5),
                                 escore = c(0.8, 0), tscore = c(0.4, 0.5), stringsAsFactors = FALSE),
              nodes = data.frame(id = c("A", "B", "C", "D"), query = c("a", "b", "c", "d"), degree = c(1, 2, 1, 0),
                                 stringsAsFactors = FALSE))
  p <- arena3d_payload_for_ppi(net)
  expect_setequal(unique(vapply(p$edges, `[[`, "", "channel")), c("Experiments", "Text mining"))
  expect_length(p$edges, 3)
  expect_setequal(vapply(p$nodes, `[[`, "", "name"), c("A", "B", "C"))  # D has no interactions
})

test_that("NORMA payload groups terms with their genes", {
  r <- fake_results()
  p <- norma_payload_for_view(r, "network-tg", top = 10)
  expect_true(length(p$edges) > 0)
  expect_true(all(lengths(p$groups) > 1))
  expect_true(all(vapply(p$expression, is.numeric, logical(1))))
  expect_null(norma_payload_for_view(r, "network-gg", top = 10, settings = list(minShared = 1))$expression)
  expect_error(norma_payload_for_view(r[0, ], "network-tg"), "no edges")
})

test_that("tool configuration: GSEA is offered only where it is implemented", {
  gseaTools <- names(Filter(function(t) ParadigmId$GSEA %in% t$paradigms, TOOLS))
  expect_equal(gseaTools, ToolId$CLUSTERPROFILER)
  cp <- TOOLS[[ToolId$CLUSTERPROFILER]]
  expect_true(all(cp$datasources %in% names(cp$datasourceCodes)))
  expect_true(9606L %in% TOOL_ORGANISMS[[ToolId$CLUSTERPROFILER]])
})

test_that("GSEA on MSigDB returns NES and leading-edge genes (online)", {
  skip_if(Sys.getenv("FLAME_ONLINE_TESTS") != "true", "set FLAME_ONLINE_TESTS=true to run")
  ids <- strsplit(example_input("ranked")$text, "\n")[[1]][-1]; ids <- sub("\t.*", "", ids)
  lst <- list(name = "test", ids = ids, scores = seq(3, -3, length.out = length(ids)),
              organismTaxId = 9606L, analyteType = "GENE", idFormat = "USERINPUT")
  params <- build_run_params(ToolId$CLUSTERPROFILER, lst, c("HALLMARK", "KEGG"), 1, "BH", NULL)
  params$paradigm <- ParadigmId$GSEA
  out <- execute_gsea_run(lst, params)
  expect_true(all(c("NES", "Positive Hits", "Term_ID", "P-value") %in% names(out$results)))
  expect_true(nrow(out$results) > 0)
  expect_true(all(is.finite(out$results$NES)))
})

test_that("tool-specific hit IDs are mapped back to the user's IDs", {
  r <- data.frame(`Positive Hits` = c("9606.ENSP1,9606.ENSP2", "TP53,MDM2", "9606.ENSP3"), check.names = FALSE)
  ct <- data.frame(input = c("RPL3", "RPS6", "TP53"), target = c("9606.ENSP1", "9606.ENSP2", "9606.ENSP9"))
  expect_equal(rollback_hit_ids(r, ct)$`Positive Hits`, c("RPL3,RPS6", "TP53,MDM2", "9606.ENSP3"))
  expect_identical(rollback_hit_ids(r, NULL), r)
})

test_that("pathway IDs are recognised across tools", {
  expect_equal(kegg_map_number("KEGG:04110"), "04110")
  expect_equal(kegg_map_number("hsa04110"), "04110")
  expect_equal(kegg_map_number("map03050"), "03050")
  expect_true(is.na(kegg_map_number("GO:0007049")))
  expect_equal(reactome_stable_id("REAC:R-HSA-69278"), "R-HSA-69278")
  expect_equal(reactome_stable_id("HSA-69278"), "R-HSA-69278")
  expect_equal(wikipathways_id("WP:WP179"), "WP179")
})

test_that("KEGG coloured map URL encodes genes and colours by score sign", {
  u <- kegg_colored_url("04110", 9606, c(TP53 = "hsa:7157", MDM2 = "hsa:4193", CDK1 = "hsa:983"),
                        scores = list(TP53 = 2.1, MDM2 = -1.3))
  expect_match(u, "^https://www\\.kegg\\.jp/kegg-bin/show_pathway\\?map=hsa04110&multi_query=")
  q <- utils::URLdecode(sub(".*multi_query=", "", u))
  expect_equal(strsplit(q, "\r\n")[[1]], c("hsa:7157 #ff0000,black", "hsa:4193 #2563eb,white", "hsa:983 #ff0000,black"))
})

test_that("pathway links without network calls: Reactome, GO and searches", {
  l <- pathway_links(list(id = "R-HSA-69278", source = "REAC", name = "Cell Cycle, Mitotic"), c("TP53", "<bad>"), 9606)
  urls <- vapply(l$links, `[[`, "", "url")
  expect_true(any(grepl("^https://reactome.org/PathwayBrowser/#/R-HSA-69278&FLG=TP53$", urls)))
  expect_true(all(grepl("^https://", urls)))
  g <- pathway_links(list(id = "GO:0007049", source = "GO:BP", name = "cell cycle"), character(0), 9606)
  expect_true(any(grepl("QuickGO/term/GO:0007049", vapply(g$links, `[[`, "", "url"))))
})

test_that("variant analyses from session files are rebuilt from validated rows", {
  x <- list(mode = "gwas", organismTaxId = 9606, inputCount = 2,
            variants = list(list(id = "rs1", chr = "19", pos = "100", consequence = "missense_variant", impact = "HIGH",
                                 allGenes = "APOE", pvalue = 1e-10, evil = "<script>"),
                            list(id = "rs2", chr = "1", pos = 5, consequence = "intron_variant", impact = "BOGUS", allGenes = "", pvalue = 0.01)),
            phenotypes = list(list(variant = "rs1", trait = "Alzheimer disease", source = "GWAS catalog", pvalue = 1e-20)),
            summary = list(traits = "ignored"), genes = "ignored")
  r <- variant_result_from_json(x)
  expect_equal(r$mode, "gwas")
  expect_false("evil" %in% names(r$variants))
  expect_equal(r$variants$impact, c("HIGH", "MODIFIER"))
  expect_equal(r$variants$pos, c(100L, 5L))
  expect_equal(r$genes$gene, "APOE")
  expect_equal(r$summary$traits[[1]]$trait, "Alzheimer disease")
  expect_error(variant_result_from_json(list(variants = list())), "no variants")
})

test_that("Cochran's Q matches a hand calculation and ignores uninformative rows", {
  m <- rbind(c(1, 1, 0), c(1, 0, 0), c(1, 1, 1), c(0, 0, 0), c(1, 0, 1), c(1, 1, 0))
  q <- cochran_q(m)
  # Informative rows: 1, 2, 5, 6 -> column sums 4, 2, 1; row sums 2, 1, 2, 2
  cj <- c(4, 2, 1); ri <- c(2, 1, 2, 2); n <- sum(ri)
  expect_equal(q$statistic, signif(2 * (3 * sum(cj^2) - n^2) / (3 * n - sum(ri^2)), 4))
  expect_equal(q$df, 2)
  expect_null(cochran_q(m[, 1:2]))
})

test_that("comparison statistics: overlap, rank agreement, CAT and tests", {
  mk <- function(id, tool, ids, p) {
    r <- fake_results()[0, ]
    r <- r[rep(1, length(ids)), ]
    r$Source <- "GO:BP"; r$Term_ID <- ids; r$Term_ID_noLinks <- ids; r$Function <- paste("term", ids)
    r$`P-value` <- p; r$`-log10Pvalue` <- -log10(p); r$`Term Size` <- seq(10, by = 10, length.out = length(ids))
    r$`Intersection Size` <- 3; r$`Positive Hits` <- "A,B,C"; r$link <- NA
    list(id = id, tool = tool, displayNumber = 1, organism = 9606, listName = "L", analyteType = "GENE",
         inputIds = LETTERS[1:10], params = list(datasources = "GO:BP", threshold = 0.05, metric = "fdr"),
         results = r, conversionTable = data.frame(input = LETTERS, target = LETTERS, name = LETTERS), backgroundSize = 20000L)
  }
  ids <- sprintf("GO:%07d", 1:12)
  runs <- list(mk("a_1", "gProfiler", ids[1:10], 10^-(10:1)), mk("b_2", "STRING", ids[3:12], 10^-(10:1)))
  cs <- suppressMessages(build_comparison(runs))
  s <- comparison_statistics(cs)
  p <- s$pairs[[1]]
  expect_equal(c(p$nA, p$nB, p$shared, p$union), c(10L, 10L, 8L, 12L))
  expect_equal(p$jaccard, round(8 / 12, 3))
  expect_equal(p$overlapCoef, 0.8)
  expect_equal(p$spearman, 1)          # shared terms in the same order in both runs
  expect_equal(p$geneJaccard, 1)
  expect_length(s$cat, 1)
  expect_true(all(s$cat[[1]]$overlap >= 0 & s$cat[[1]]$overlap <= 1))
  expect_equal(vapply(s$agreement, `[[`, 0L, "terms"), c(4L, 8L))
  expect_equal(s$runs[[1]]$unique + s$runs[[2]]$unique, 4L)
  expect_equal(s$tests$detection$method, "McNemar's test")
  expect_length(s$terms, 12)
})

test_that("examples use recognisable gene names, not ORFs or placeholders", {
  expect_equal(named_gene(c("TP53", "C11orf24", "LOC647225", "KIAA0100", "FAM100A", "HLA-A", "NKX2-1", "LINC00473", "HOXA-AS2", "ZNF217")),
               c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE))
  up <- strsplit(example_input("upload")$text, "\n")[[1]]
  expect_true(all(named_gene(up)))
  d <- example_input("de_lists")
  expect_true(all(named_gene(c(d$up, d$down))))
})

test_that("expression matrices: parsing, log transform, clusters and two-group test", {
  ex <- example_input("expression")
  x <- parse_expression_matrix(ex$text, ex$fileName)
  expect_true(x$logged)
  expect_equal(ncol(x$matrix), 8)
  o <- expression_overview(x, topN = 200, k = 4)
  expect_equal(o$shownGenes, 200)
  expect_equal(sort(unique(vapply(o$genes, `[[`, 0L, "cluster"))), 1:4)
  expect_length(o$heatmap$z, 200)
  dex <- grep("^dexamethasone_", x$samples, value = TRUE); unt <- grep("^untreated_", x$samples, value = TRUE)
  de <- expression_de(x, dex, unt)
  expect_gt(de$significant, 0)
  expect_error(expression_de(x, dex[1], unt[1:2]), "at least two")
  expect_error(parse_expression_matrix("gene\tA\nX\t1"), "two")
})

test_that("examples come from the published datasets", {
  up <- strsplit(example_input("upload")$text, "\n")[[1]]
  pool <- example_gene_pool()
  expect_length(pool, 500)
  expect_true(all(c("DUSP1", "KLF15", "PER1") %in% pool))         # glucocorticoid-response genes
  expect_length(up, 200)
  expect_equal(anyDuplicated(up), 0L)
  expect_true(all(up %in% pool))
  # every load is a new random draw of 200 from the pool of 500
  expect_false(setequal(up, strsplit(example_input("upload")$text, "\n")[[1]]))
  d <- example_input("de_lists")
  expect_true("ZBTB16" %in% d$up)
  um <- read.delim(getExamplesPath("pbmc3k_gene_umap.tsv"))
  expect_equal(um$cell_type[um$gene == "MS4A1"], "B cells")
  expect_equal(um$cell_type[um$gene == "LYZ"], "CD14 monocytes")
  gw <- read.delim(getExamplesPath("ad_gwas_bellenguez2022.tsv"))
  expect_true(nrow(gw) >= 80 && "rs6733839" %in% gw$SNP)
  for (k in EXAMPLE_KINDS) expect_true(nzchar(example_input(k)$source %||% ""), info = k)
})

test_that("microRNA names are normalised", {
  expect_equal(mirna_key(c("hsa-miR-21-5p", "MIR21", "let-7a", "hsa-mir-155", "mmu-miR-34a-5p")),
               c("mir-21-5p", "mir-21", "let-7a", "mir-155", "mir-34a-5p"))
})

test_that("microRNA targets from miRDB (online: msigdbr data)", {
  skip_if(Sys.getenv("FLAME_ONLINE_TESTS") != "true", "set FLAME_ONLINE_TESTS=true to run")
  r <- mirna_targets("hsa-miR-21-5p\nmiR-155\nnot-a-mirna", 9606, 2)
  expect_equal(unlist(r$unmatched), "not-a-mirna")
  expect_equal(length(r$mirnas[[2]]$matched), 2)   # both arms
  expect_true(all(vapply(r$genes, `[[`, 0L, "count") >= 1))
  expect_true(length(r$selected) < r$totalTargets)
})
