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

test_that("the precomputed tumour-vs-normal table is sound (offline)", {
  tn <- .tcga_tn()
  expect_gte(length(tn$cancers), 15)
  expect_true(all(c("BRCA", "LIHC", "LUAD", "COADREAD", "PRAD") %in% names(tn$cancers)))
  for (code in names(tn$cancers)) {
    d <- tn$cancers[[code]]
    expect_gte(d$n$normal, 9)
    expect_named(d$genes, c("gene", "entrez", "log2FC", "q"))
    expect_gt(nrow(d$genes), 10000)
    expect_false(anyDuplicated(d$genes$gene) > 0)
  }
  fc <- function(code, g) tn$cancers[[code]]$genes$log2FC[match(g, tn$cancers[[code]]$genes$gene)]
  expect_lt(fc("LIHC", "ALB"), 0)          # less albumin in liver tumours than in normal liver
  expect_gt(fc("PRAD", "AMACR"), 1)        # AMACR, a prostate cancer marker
  expect_gt(fc("COADREAD", "CDH3"), 1)     # P-cadherin, up in colorectal tumours
  expect_lt(fc("LUAD", "AGER"), -2)        # RAGE, lost in lung tumours
})

test_that("TCGA tumour vs normal filters the precomputed table (online: study list)", {
  skip_if_not(identical(Sys.getenv("FLAME_ONLINE_TESTS"), "true"), "set FLAME_ONLINE_TESTS=true to run")
  r <- tcga_genes("lihc_tcga_pan_can_atlas_2018", "normal", direction = "up", minLog2FC = 2, maxQ = 0.01)
  v <- vapply(r$genes, `[[`, 0, "value")
  expect_true(all(v >= 2))
  expect_equal(r$groups$b$label, "Adjacent normal")
  all <- tcga_genes("lihc_tcga_pan_can_atlas_2018", "normal", all = TRUE)
  expect_true(isTRUE(all$keepAll))
  expect_false(is.unsorted(rev(vapply(all$genes, `[[`, 0, "value"))))
  expect_error(tcga_genes("ov_tcga_pan_can_atlas_2018", "normal"), "no normal tissue")
})
