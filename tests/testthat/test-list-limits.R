# List size limits (FLAME_MAX_IDS / FLAME_MAX_RANKED_IDS) and tools that take shorter lists.

test_that("lists hold up to MAX_LIST_IDS IDs, ranked lists up to MAX_RANKED_IDS", {
  expect_equal(MAX_LIST_IDS, 20000L)
  expect_equal(MAX_RANKED_IDS, 60000L)
  ids <- paste0("G", seq_len(10000))
  expect_equal(UnrankedAnalyteList$new(name = "big", analyteType = AnalyteType$GENE, ids = ids)$size(), 10000)
  expect_error(UnrankedAnalyteList$new(name = "too big", analyteType = AnalyteType$GENE, ids = paste0("G", seq_len(MAX_LIST_IDS + 1))),
               "Too many items")
})

test_that("PANTHER takes lists of up to 3,000 IDs; other tools have no such limit", {
  expect_equal(TOOLS[[ToolId$PANTHER]]$maxIds, 3000L)
  expect_null(TOOLS[[ToolId$GPROFILER]]$maxIds)
  expect_null(TOOLS[[ToolId$ENRICHR]]$maxIds)
})

test_that("g:Profiler replies are read into gost()'s table, hits named by the input IDs", {
  reply <- list(
    result = list(
      list(source = "GO:BP", native = "GO:0007049", name = "cell cycle", p_value = 1e-8, term_size = 2000,
           query_size = 4, intersection_size = 2, significant = TRUE,
           intersections = list(list("IDA"), list(), list("IEA", "TAS"), list())),
      list(source = "KEGG", native = "KEGG:04115", name = "p53 signaling pathway", p_value = 1e-5, term_size = 70,
           query_size = 4, intersection_size = 1, significant = TRUE,
           intersections = list(list("KEGG"), list(), list(), list()))),
    meta = list(
      genes_metadata = list(query = list(query_1 = list(
        ensgs = list("ENSG00000141510", "ENSG00000135679", "ENSG00000124762", "ENSG00000999999"),
        mapping = list(TP53 = list("ENSG00000141510"), MDM2 = list("ENSG00000135679"), CDKN1A = list("ENSG00000124762"))))),
      result_metadata = list(`GO:BP` = list(domain_size = 20000))))
  local_mocked_bindings(POST = function(...) structure(list(), class = "fake_resp"),
                        status_code = function(...) 200L,
                        content = function(...) jsonlite::toJSON(reply, auto_unbox = TRUE), .package = "httr")
  out <- gprofiler_gost(c("TP53", "MDM2", "CDKN1A"), "hsapiens", c("GO:BP", "KEGG"))
  expect_equal(out$result$term_id, c("GO:0007049", "KEGG:04115"))
  expect_equal(out$result$intersection, c("TP53,CDKN1A", "TP53"))
  expect_equal(out$result$intersection_size, c(2, 1))
  expect_equal(out$meta$result_metadata$`GO:BP`$domain_size, 20000)
})
