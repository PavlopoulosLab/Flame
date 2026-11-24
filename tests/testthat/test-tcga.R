test_that("TCGA queries through cBioPortal give ranked genes (online)", {
  skip_if_not(identical(Sys.getenv("FLAME_ONLINE_TESTS"), "true"), "set FLAME_ONLINE_TESTS=true to run")
  st <- tcga_studies()
  expect_gt(length(st), 30)
  S <- "brca_tcga_pan_can_atlas_2018"
  m <- tcga_genes(S, "mutated")
  top <- vapply(m$genes[1:3], `[[`, "", "gene")
  expect_true(all(c("PIK3CA", "TP53") %in% top))
  expect_equal(m$organismTaxId, 9606L)
  amp <- tcga_genes(S, "cna", direction = "amp")
  expect_true("CCND1" %in% vapply(amp$genes[1:10], `[[`, "", "gene"))
  co <- tcga_genes(S, "coexpression", gene = "ESR1", minCorrelation = 0.6)
  expect_true("FOXA1" %in% vapply(co$genes, `[[`, "", "gene"))
  expect_true(all(vapply(co$genes, `[[`, 0, "value") >= 0.6))
  g <- tcga_groups(S)
  expect_equal(g$attributes[[1]]$id, "SUBTYPE")
  de <- tcga_genes(S, "de", groupA = list(type = "clinical", attribute = "SUBTYPE", values = list("BRCA_Basal")),
                   groupB = list(type = "clinical", attribute = "SUBTYPE", values = list("BRCA_LumA")), direction = "down")
  expect_true("ESR1" %in% vapply(de$genes, `[[`, "", "gene"))                     # ESR1 is low in basal tumours
  expect_true(all(vapply(de$genes, `[[`, 0, "value") <= -1))
  expect_error(tcga_genes("not_a_study", "mutated"), "TCGA cancer types")
})

test_that("bad TCGA input is refused before any request", {
  expect_error(tcga_genes("x", "nonsense"), "Unknown TCGA query")
  expect_error(.entrez_of("not a gene!"), "gene symbol")
})
