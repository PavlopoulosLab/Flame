# Offline tests for the Shiny-free core (R/core-*.R). No network access.

test_that("result filters: sources, search grammar, ranges", {
  r <- fake_results()
  expect_equal(nrow(apply_result_filters(r, list(sources = "GO:BP"))), 2)
  expect_equal(apply_result_filters(r, list(search = "cycle -mitotic"))$Term_ID, "GO:1")
  expect_equal(nrow(apply_result_filters(r, list(pvalueMax = 1e-4))), 2)
  expect_equal(nrow(apply_result_filters(r, list(intersectionMin = 3, termSizeMax = 90))), 2)
})

test_that("top/sort ranks by metric with term-id tie break", {
  r <- fake_results()
  expect_equal(apply_top_sort(r, 2, "-log10Pvalue")$Term_ID, c("GO:1", "GO:2"))
  expect_equal(apply_top_sort(r, 1, "Term Coverage")$Term_ID, "GO:2")
  expect_equal(nrow(apply_top_sort(r, .Machine$integer.max)), 4)
})

test_that("every view builds and heatmap/network shapes are consistent", {
  r <- fake_results()
  for (v in VIEW_IDS) {
    x <- build_view(r, v, top = 10)
    expect_equal(x$shownCount, 4, info = v)
  }
  tg <- build_view(r, "heatmap-tg")
  expect_equal(length(tg$cells), 15)                    # 6 + 4 + 3 + 2 term-gene pairs
  tt <- term_similarity_edges(r)
  s12 <- tt$similarity[tt$source == "GO:1" & tt$target == "GO:2"]
  expect_equal(s12, round(3 / 7 * 100, 2))                # |{A,B,C}| / |{A..G}|
  gg <- gene_cooccurrence_edges(r)
  expect_equal(gg$shared[gg$source == "A" & gg$target == "B"], 2)
  net <- build_view(r, "network-tt", settings = list(similarityCutoff = 1, layout = "circle"))
  expect_true(all(vapply(net$network$edges, function(e) e$weight >= 1, logical(1))))
  expect_true(all(vapply(net$network$nodes, function(n) is.numeric(n$x), logical(1))))
})

test_that("term links keep only http(s) URLs", {
  expect_true(is.na(safe_link("javascript:alert(1)")))
  expect_equal(safe_link("https://www.ebi.ac.uk/QuickGO/term/GO:1"), "https://www.ebi.ac.uk/QuickGO/term/GO:1")
  expect_null(extractHref("<a href='javascript:alert(1)'>x</a>"))
  cleaned <- clean_result_links(transform(fake_results(),
    Term_ID = c("<a href='https://x/1'>GO:1</a>", "GO:2", "KEGG:3", "R-HSA-4")))
  expect_equal(cleaned$Term_ID[1], "GO:1")
  expect_equal(cleaned$link[1], "https://x/1")
})

test_that("comparison: Fisher combination, agreement and run filters", {
  mk <- function(id, tool, keep) list(
    id = id, tool = tool, displayNumber = 1, organism = 9606, listName = "L", analyteType = "GENE",
    inputIds = LETTERS[1:10], params = list(datasources = "GO:BP", threshold = 0.05, metric = "fdr"),
    results = fake_results()[keep, ], backgroundSize = 20000,
    conversionTable = data.frame(input = LETTERS[1:10], target = LETTERS[1:10], name = LETTERS[1:10]))
  cs <- suppressMessages(build_comparison(list(mk("a_1", "gProfiler", 1:3), mk("b_2", "STRING", 2:4))))
  res <- cs$getResults()
  expect_equal(nrow(res), 4)
  both <- res[res$Term_ID_noLinks == "GO:2", ]
  expect_equal(both$Rank, 2)
  expect_equal(both$CombPvalue, stats::pchisq(-2 * 2 * log(1e-5), df = 4, lower.tail = FALSE), tolerance = 1e-6)

  # Regression: `runsMode` must not be mistaken for `runs` (R partial matching).
  expect_equal(comparison_view(cs, "overview", list(runsMode = "any"))$filteredCount, 4)
  expect_equal(comparison_view(cs, "overview", list(runsMode = "all"))$filteredCount, 2)
  expect_equal(comparison_view(cs, "overview", list(runs = "a_1"))$filteredCount, 3)
  expect_setequal(comparison_overview_terms(cs, list(), "pin", "consensus"), c("GO:2", "KEGG:3"))
  expect_setequal(comparison_overview_terms(cs, list(), "bubble", "exclusiveTerms", "a_1"), "GO:1")
  hm <- comparison_view(cs, "heatmap", top = 10)
  expect_equal(length(hm$cells), 8)
})

test_that("pasted IDs are split, de-duplicated and validated", {
  p <- parse_id_text("TP53, brca1\nTP53\tEGFR;bad!id\n\nBRCA1")
  expect_equal(p$ids, c("TP53", "brca1", "EGFR"))
  expect_equal(p$duplicates, 2)
  expect_equal(p$invalid, 1)
  expect_error(parse_id_text(strrep("A", 1024 * 1024 + 1)), "1 MB")
})

test_that("volcano and reduction tables are parsed with sensible defaults", {
  txt <- "gene\tlogFC\tpadj\nA\t2\t0.001\nB\t-3\t0.01\nC\t0.1\t0.5\nD\tNA\t0.01"
  prep <- prepare_table_input(txt, "x.tsv", "volcano")
  expect_equal(prep$defaults, list(id = "gene", logfc = "logFC", pvalue = "padj"))
  pts <- volcano_points(txt, "x.tsv", prep$defaults, 0.05, 1)
  expect_equal(pts$plotted, 3)
  expect_equal(pts$counts$up, 1)
  expect_equal(pts$counts$down, 1)
  red <- reduction_points("id,UMAP1,UMAP2,cluster\nA,1,2,c1\nB,3,4,c2", "", list(id = "id", x = "UMAP1", y = "UMAP2", color = "cluster", size = ""))
  expect_equal(red$colorType, "categorical")
  expect_error(volcano_points(txt, "x.tsv", list(id = "gene", logfc = "logFC", pvalue = "logFC")), "different")
})

test_that("custom GMT sources: parsing and hypergeometric ORA", {
  sets <- parse_gmt("S1\tset one\tA\tB\tC\tD\nS2\tset two\tX\tY\tZ\nbad line\n")
  expect_length(sets, 2)
  universe <- c(LETTERS, paste0("G", 1:200))
  bigSets <- c(sets, list(list(id = "BG", name = "background", genes = universe)))
  out <- execute_gmt_run(list(name = "L", ids = c("A", "B", "C", "D")), bigSets, "My sets", 0.05, "BH")
  expect_equal(out$results$Term_ID[1], "S1")
  expect_equal(out$results$`Intersection Size`[1], 4)
  expect_true(out$results$`P-value`[1] < 1e-5)
})

test_that("orthologs: g:Orth's N/A means no ortholog (unticked, not counted)", {
  expect_equal(.clean_chr(c("Trp53", "N/A", "n/a", "None", "NA", NA, " Brca1 ")), c("Trp53", "", "", "", "", "", "Brca1"))
  local_mocked_bindings(gorth_ids = function(...) data.frame(
    input = c("TP53", "XIST"), input_ensg = c("ENSG00000141510", "ENSG00000229807"),
    ortholog_name = c("Trp53", "N/A"), ortholog_ensg = c("ENSMUSG00000059552", "N/A"), description = c("tumor protein p53", "N/A")))
  out <- find_orthologs(c("TP53", "XIST"), 9606, 10090)
  expect_equal(out$rows$ortholog, c("Trp53", ""))
  expect_equal(out$rows$orthologEnsg, c("ENSMUSG00000059552", ""))
  expect_equal(out$stats$mappedInputs, 1L)
})
