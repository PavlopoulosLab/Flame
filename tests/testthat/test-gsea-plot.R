test_that("running enrichment score: weighted walk, peak, leading edge and an exact curve", {
  scores <- c(5, 4, 3, 2, 1, -1, -2, -3, -4, -5)
  inSet <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  rs <- gsea_running_score(scores, inSet)
  # hits weigh |score| / sum(|score| of hits) = 5/13, 3/13, 5/13; misses -1/7 each
  walk <- cumsum(ifelse(inSet, abs(scores) / 13, -1 / 7))
  expect_equal(rs$es, max(walk))
  expect_equal(rs$peak, 3L)
  expect_equal(rs$leadingEdge, c(1L, 3L))
  expect_equal(rs$hits, c(1L, 3L, 10L))
  # the polyline goes through the walk at every hit and ends at 0
  expect_equal(rs$y[match(rs$hits, rs$x)], round(walk[rs$hits], 5))
  expect_equal(tail(rs$y, 1), 0)
  neg <- gsea_running_score(scores, rev(inSet))
  expect_lt(neg$es, 0)
  expect_true(all(neg$leadingEdge >= neg$peak))
  expect_error(gsea_running_score(scores, rep(FALSE, 10)), "no genes")
})

test_that("every MSigDB collection is offered for GSEA and maps to an msigdbr subcollection", {
  cp <- TOOLS[[ToolId$CLUSTERPROFILER]]
  ids <- unlist(lapply(MSIGDB_COLLECTIONS, function(cl) names(cl$sources)))
  expect_setequal(ids, cp$datasources)
  expect_true(all(ids %in% names(cp$datasourceCodes)))
  expect_equal(vapply(MSIGDB_COLLECTIONS, `[[`, "", "code"), c("H", paste0("C", 1:9)))
  expect_true(all(GSEA_DEFAULT_DATASOURCES %in% ids))
  expect_true(all(ids %in% names(DATASOURCE_COLORS)))
  counts <- msigdb_set_counts()
  expect_equal(counts[["HALLMARK"]], 50L)
  # KEGG is downloaded from KEGG itself (current pathways), so MSigDB has no count for it.
  expect_true(all(!is.na(counts[setdiff(names(counts), "KEGG")])))
})

test_that("custom signatures are matched to the ranking by input ID or symbol", {
  ct <- data.frame(input = c("ENSG1", "ENSG2", "tp53", "ENSG4"), target = c("101", "102", "7157", ""),
                   name = c("FKBP5", "PER1", "TP53", "XIST"), stringsAsFactors = FALSE)
  sets <- list(list(id = "SIG", name = "My signature", genes = c("fkbp5", "ENSG2", "TP53", "NOTHERE", "XIST")),
               list(id = "NONE", name = "Unrelated", genes = c("AAA", "BBB")))
  out <- custom_gsea_sets(sets, ct, "Mine")
  expect_setequal(out$gene, c("101", "102", "7157"))     # XIST has no NCBI ID, NOTHERE is not ranked
  expect_equal(unique(out$term), "SIG")
  expect_equal(unique(out$source), "Mine")
  expect_equal(nrow(custom_gsea_sets(sets[2], ct, "Mine")), 0)
})
