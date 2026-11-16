test_that("KEGG IDs written differently by tools match in comparisons", {
  res <- data.frame(Source = c("KEGG", "KEGG", "KEGG", "GO:BP", "REAC"),
                    Term_ID_noLinks = c("hsa05210", "map05210", "KEGG:05210", "GO:0008150", "R-HSA-109582"),
                    Term_ID = c("hsa05210", "map05210", "KEGG:05210", "GO:0008150", "R-HSA-109582"))
  out <- canonical_kegg_ids(res)
  expect_equal(out$Term_ID_noLinks, c("map05210", "map05210", "map05210", "GO:0008150", "R-HSA-109582"))
  expect_equal(out$Term_ID, out$Term_ID_noLinks)
  expect_equal(canonical_kegg_ids(data.frame()), data.frame())
})
