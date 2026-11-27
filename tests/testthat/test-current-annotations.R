# clusterProfiler's GO / KEGG / MSigDB sources and Enrichr's library IDs (current databases).

test_that("each source goes to the engine that serves it", {
  skip_if_not_installed("org.Hs.eg.db")
  human <- cp_source_plan(9606, c("GO:BP", "GO:CC", "KEGG", "REAC", "HALLMARK"))
  expect_equal(human$go, c("GO:BP", "GO:CC"))
  expect_true(human$kegg)
  expect_equal(human$msigdb, c("REAC", "HALLMARK"))
  # No OrgDb for horse: GO comes from MSigDB, KEGG still from KEGG.
  horse <- cp_source_plan(9796, c("GO:BP", "KEGG"))
  expect_equal(horse$go, character(0))
  expect_true(horse$kegg)
  expect_equal(horse$msigdb, "GO:BP")
  # Every clusterProfiler organism has a KEGG code; MSigDB never serves KEGG (its copy is from 2011).
  expect_true(all(names(MSIGDB_SPECIES) %in% names(CP_KEGG_CODE)))
  expect_false("CP:KEGG_LEGACY" %in% unlist(TOOLS[[ToolId$CLUSTERPROFILER]]$datasourceCodes))
})

test_that("KEGG downloads are kept on disk and reused; an old copy is used when KEGG is down", {
  withr::local_envvar(FLAME_CACHE_DIR = withr::local_tempdir())
  rm(list = ls(.kegg_mem), envir = .kegg_mem)
  withr::defer(rm(list = ls(.kegg_mem), envir = .kegg_mem))   # no fake KEGG data for later tests
  calls <- 0L
  local_mocked_bindings(kegg_release = function() "Last update 2026/11/24")
  local_mocked_bindings(gson_KEGG = function(...) { calls <<- calls + 1L; structure(list(), class = "fake_gson") }, .package = "clusterProfiler")
  a <- kegg_gson(9606)
  b <- kegg_gson(9606)
  expect_equal(calls, 1L)
  expect_false(a$stale)
  expect_match(kegg_note(a), "Last update 2026/11/24, downloaded")
  # A fresh process reads the file instead of downloading.
  rm(list = ls(.kegg_mem), envir = .kegg_mem)
  kegg_gson(9606)
  expect_equal(calls, 1L)
  # Older than the limit and KEGG unreachable: the saved copy, marked stale.
  rm(list = ls(.kegg_mem), envir = .kegg_mem)
  local_mocked_bindings(gson_KEGG = function(...) stop("timeout"), .package = "clusterProfiler")
  c <- kegg_gson(9606, maxAgeDays = 0)
  expect_true(c$stale)
  expect_match(kegg_note(c), "could not be reached")
  # Nothing saved at all: a clear error.
  expect_error(kegg_gson(10090, maxAgeDays = 0), "KEGG could not be reached")
})

test_that("term names without IDs are matched to KEGG / Reactome IDs", {
  kegg <- data.frame(id = c("hsa04110", "hsa04115"), name = c("Cell cycle", "p53 signaling pathway"))
  hit <- lookup_term_ids(c("CELL CYCLE", "P53 SIGNALING PATHWAY", "Gone pathway"), kegg)
  expect_equal(hit$ids, c("hsa04110", "hsa04115", NA))
  expect_equal(hit$names, c("Cell cycle", "p53 signaling pathway", "Gone pathway"))
  expect_equal(lookup_term_ids("x", NULL)$ids, NA_character_)
})

test_that("Enrichr terms without IDs get real ones (or keep their name as the ID)", {
  local_mocked_bindings(
    kegg_pathway_names = function(code) data.frame(id = "hsa04110", name = "Cell cycle"),
    reactome_pathway_names = function(species) data.frame(id = "R-HSA-1640170", name = "Cell Cycle"))
  s <- EnrichRORAStrategy$new()
  df <- data.frame(database = c("KEGG", "KEGG", "REAC", "REAC", "GO:BP"),
                   Term = c("CELL CYCLE", "OLD MAP", "Cell Cycle", "Inhib of Something", "x"),
                   TermId = c("", "", "R-", "R-", "GO:1"), stringsAsFactors = FALSE)
  out <- s$.__enclos_env__$private$lookupMissingIds(df, "hsapiens")
  expect_equal(out$TermId, c("hsa04110", "KEGG:OLD MAP", "R-HSA-1640170", "REAC:Inhib of Something", "GO:1"))
  expect_equal(out$Term[1], "Cell cycle")
  # Map IDs for pathway numbers only; name IDs stay as they are.
  m <- s$.__enclos_env__$private$mapKEGGIds(data.frame(Source = c("KEGG", "KEGG"), Term_ID = c("hsa04110", "KEGG:OLD MAP")))
  expect_equal(m$Term_ID, c("map04110", "KEGG:OLD MAP"))
})

test_that("Enrichr libraries are current", {
  codes <- TOOLS[[ToolId$ENRICHR]]$organismDatasourceCodes
  expect_equal(codes$hsapiens[["GO:BP"]], "GO_Biological_Process_2026")
  expect_equal(codes$hsapiens[["KEGG"]], "KEGG_2026")
  expect_equal(codes$hsapiens[["REAC"]], "Reactome_Pathways_2024")
  expect_equal(codes$mmusculus[["WP"]], "WikiPathways_2024_Mouse")
})

test_that("clusterProfiler runs on current GO, KEGG and MSigDB (online)", {
  skip_if_not(identical(Sys.getenv("FLAME_ONLINE_TESTS"), "true"), "set FLAME_ONLINE_TESTS=true to run")
  genes <- c("TP53", "MDM2", "CDKN1A", "BAX", "GADD45A", "FAS", "BBC3", "SESN1", "CCNB1", "CDK1", "CCNA2", "BUB1",
             "PLK1", "CDC20", "MCM2", "PCNA", "E2F1", "CCNE1", "CHEK1", "CHEK2", "ATM", "ATR", "RB1", "MYC", "CCND1")
  r <- execute_ora_run(ToolId$CLUSTERPROFILER, list(ids = genes, organismTaxId = 9606, name = "t"),
                       list(datasources = c("GO:BP", "KEGG", "HALLMARK"), threshold = 0.05, metric = "BH",
                            namespace = "ENTREZGENE_ACC", backgroundMode = "genome", geneListName = "t"))
  expect_equal(r$status, "complete")
  expect_setequal(unique(r$results$Source), c("GO:BP", "KEGG", "HALLMARK"))
  expect_true("map04110" %in% r$results$Term_ID_noLinks)   # Cell cycle, current KEGG
  expect_true(any(grepl("^KEGG pathways from KEGG", r$messages)))
  expect_true(any(grepl("^GO terms from org.Hs.eg.db", r$messages)))
})
