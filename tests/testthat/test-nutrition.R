test_that("bad nutrition input is refused before any request", {
  expect_error(nutrition_genes("x", "ctd"), "Type a nutrient")
  expect_error(nutrition_genes("curcumin", "nonsense"), "Unknown source")
})

test_that("nutrients and foods give genes from CTD, UniProt and GWAS traits (online)", {
  skip_if_not(identical(Sys.getenv("FLAME_ONLINE_TESTS"), "true"), "set FLAME_ONLINE_TESTS=true to run")
  ctd <- nutrition_ctd("curcumin")
  expect_gt(ctd$total, 100)
  expect_true(all(c("BCL2", "CASP3") %in% vapply(ctd$genes[1:10], `[[`, "", "gene")))
  expect_true(all(vapply(ctd$genes, function(g) g$direction %in% c("up", "down", "both", "other"), logical(1))))
  expect_equal(nutrition_ctd("omega-3")$term, "Fatty Acids, Omega-3")          # MeSH fallback
  expect_equal(nutrition_ctd("green tea")$term, "Tea")                        # foods are CTD chemicals too
  up <- nutrition_uniprot("folic acid")
  expect_true(all(c("FOLR1", "SLC19A1") %in% vapply(up$genes, `[[`, "", "gene")))
  expect_equal(nutrition_traits("vitamin D")[[1]]$name, "vitamin D level")
})
