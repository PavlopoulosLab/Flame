test_that("bad nutrition input is refused before any request", {
  expect_error(nutrition_genes("x", "ctd"), "Type a nutrient")
  expect_error(nutrition_genes("curcumin", "nonsense"), "Unknown source")
})

test_that("nutrients and foods give genes from CTD, UniProt and GWAS traits (online)", {
  skip_if_not(identical(Sys.getenv("FLAME_ONLINE_TESTS"), "true"), "set FLAME_ONLINE_TESTS=true to run")
  ctd <- tryCatch(nutrition_ctd("curcumin"), error = function(e) e)
  skip_if(inherits(ctd, "error") && grepl("confirm they are human", conditionMessage(ctd)), "CTD asks for a human check (CAPTCHA)")
  if (inherits(ctd, "error")) stop(ctd)
  expect_gt(ctd$total, 100)
  expect_true(all(c("BCL2", "CASP3") %in% vapply(ctd$genes[1:10], `[[`, "", "gene")))
  expect_true(all(vapply(ctd$genes, function(g) g$direction %in% c("up", "down", "both", "other"), logical(1))))
  expect_equal(nutrition_ctd("omega-3")$term, "Fatty Acids, Omega-3")          # MeSH fallback
  expect_equal(nutrition_ctd("green tea")$term, "Tea")                        # foods are CTD chemicals too
  up <- nutrition_uniprot("folic acid")
  expect_true(all(c("FOLR1", "SLC19A1") %in% vapply(up$genes, `[[`, "", "gene")))
  expect_equal(nutrition_traits("vitamin D")[[1]]$name, "vitamin D level")
})

test_that("GEO studies and the LINCS L1000 signature give directed genes (online)", {
  skip_if_not(identical(Sys.getenv("FLAME_ONLINE_TESTS"), "true"), "set FLAME_ONLINE_TESTS=true to run")
  g <- nutrition_geo("vitamin D")
  expect_gt(length(g$studies), 3)
  top <- vapply(utils::head(g$genes, 20), `[[`, "", "gene")
  expect_true("CYP24A1" %in% top)                               # the classic vitamin D target
  expect_equal(g$genes[[match("CYP24A1", top)]]$direction, "up")
  l <- nutrition_lincs("vitamin D")                             # looked up as calcitriol
  expect_equal(l$term, "Calcitriol")
  expect_true(all(vapply(l$genes, `[[`, "", "direction") %in% c("up", "down")))
  expect_error(nutrition_lincs("no-such-compound-xyz"), "no signature")
})
