terms_df <- function() {
  data.frame(
    Source = c("GO:BP", "GO:BP", "GO:BP", "GO:BP", "KEGG", "KEGG"),
    Term_ID_noLinks = c("GO:0006955", "GO:0002250", "GO:0006954", "GO:0007049", "hsa04060", "hsa04062"),
    Function = c("immune response", "adaptive immune response", "inflammatory response", "cell cycle",
                 "Cytokine-cytokine receptor interaction", "Chemokine signaling pathway"),
    `P-value` = c(1e-10, 1e-8, 1e-7, 1e-3, 1e-6, 1e-5),
    `Positive Hits` = c("A,B,C,D", "A,B,C", "E,F", "G,H", "A,B,C,D", "A,B,C"),
    check.names = FALSE, stringsAsFactors = FALSE) |> within({
      `-log10Pvalue` <- -log10(`P-value`); `Term Size` <- 100; `Query size` <- 50; `Intersection Size` <- 3
      `Gene Ratio` <- 0.06; `Term Coverage %` <- 3; `Fold Enrichment` <- 2
    })
}

test_that("shared-genes reduction merges overlapping terms of the same source only", {
  out <- reduce_redundancy(terms_df(), 0.5, method = "genes")
  expect_setequal(out$Term_ID_noLinks, c("GO:0006955", "GO:0006954", "GO:0007049", "hsa04060"))
  expect_equal(out$Represents[out$Term_ID_noLinks == "GO:0006955"], 2L)
  expect_match(out$Merged[out$Term_ID_noLinks == "hsa04060"], "hsa04062")
})

test_that("Wang similarity matches GOSemSim", {
  skip_if_not_installed("GOSemSim"); skip_if_not_installed("GO.db")
  ids <- c("GO:0006955", "GO:0045087", "GO:0006954", "GO:0007049", "GO:0002376")
  ours <- go_wang_similarity(ids, "BP")
  ref <- GOSemSim::mgoSim(ids, ids, semData = suppressWarnings(GOSemSim::godata(ont = "BP", computeIC = FALSE)),
                          measure = "Wang", combine = NULL)
  expect_equal(unname(ours), unname(ref[ids, ids]), tolerance = 1e-3)
  expect_equal(unname(diag(ours)), rep(1, 5))
  expect_equal(ours, t(ours))
})

test_that("semantic reduction merges close GO terms and leaves other sources alone", {
  skip_if_not_installed("GOSemSim"); skip_if_not_installed("GO.db")
  df <- terms_df()
  out <- reduce_redundancy(df, 0.7, method = "semantic")
  expect_true(all(c("hsa04060", "hsa04062") %in% out$Term_ID_noLinks))      # KEGG untouched
  expect_true("GO:0006955" %in% out$Term_ID_noLinks)                        # most significant kept
  expect_false("GO:0002250" %in% out$Term_ID_noLinks)                       # adaptive immune response merged (Wang 0.85)
  expect_true("GO:0007049" %in% out$Term_ID_noLinks)                        # cell cycle is unrelated
  expect_match(out$Merged[out$Term_ID_noLinks == "GO:0006955"], "adaptive immune response")
  sim <- go_wang_similarity(c("GO:0006955", "GO:0002250"), "BP")
  loose <- reduce_redundancy(df, min(sim[1, 2] + 0.05, 1), method = "semantic")
  expect_true("GO:0002250" %in% loose$Term_ID_noLinks)                      # a stricter cut-off keeps it
})

test_that("REVIGO verdicts map eliminated terms to their representative", {
  df <- terms_df()
  inp <- revigo_input(df)
  expect_equal(inp$ids, c("GO:0006955", "GO:0002250", "GO:0006954", "GO:0007049"))
  revigo_cache_set(revigo_cache_key(inp$ids, inp$pvalues, 9606L, 0.7, "SIMREL"),
                   data.frame(id = inp$ids, representative = c(NA, "GO:0006955", "GO:0006955", NA), stringsAsFactors = FALSE))
  out <- reduce_redundancy(df, 0.7, method = "revigo", taxid = 9606L, measure = "SIMREL")
  expect_setequal(out$Term_ID_noLinks, c("GO:0006955", "GO:0007049", "hsa04060", "hsa04062"))
  expect_equal(out$Represents[out$Term_ID_noLinks == "GO:0006955"], 3L)
  expect_null(revigo_pending(df, list(reduce = TRUE, reduceMethod = "revigo", reduceCutoff = 0.7), 9606L))
  expect_false(is.null(revigo_pending(df, list(reduce = TRUE, reduceMethod = "revigo", reduceCutoff = 0.4), 9606L)))
  v <- build_view(df, "table", list(reduce = TRUE, reduceMethod = "revigo", reduceCutoff = 0.7), top = 20L, taxid = 9606L)
  expect_equal(v$reduction$after, 4L)
  expect_equal(v$reduction$untouched, 2L)
})

test_that("REVIGO web service answers (online)", {
  skip_if_not(identical(Sys.getenv("FLAME_ONLINE_TESTS"), "true"), "set FLAME_ONLINE_TESTS=true to run")
  tab <- revigo_fetch(c("GO:0006955", "GO:0002376", "GO:0006954", "GO:0045087", "GO:0006952"),
                      c(1e-10, 1e-9, 1e-8, 1e-7, 1e-9), 9606L, 0.4, "SIMREL")
  expect_setequal(tab$id, c("GO:0006955", "GO:0002376", "GO:0006954", "GO:0045087", "GO:0006952"))
  expect_true(any(!is.na(tab$representative)))
})
