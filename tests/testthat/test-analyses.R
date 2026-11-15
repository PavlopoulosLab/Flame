# Offline tests for themes / word clouds, the fast ORA used by robustness checks,
# ssGSEA pathway activity and Enrichr term parsing. Calls to ChEA3, KEA3,
# Enrichr (LINCS), STRING and KEGG run only when FLAME_ONLINE_TESTS=true.

online <- function() identical(Sys.getenv("FLAME_ONLINE_TESTS"), "true")

test_that("term keywords drop stop words and weight by significance", {
  kw <- term_keywords(c("Regulation of cell cycle", "Mitotic cell cycle checkpoint", "DNA repair"), c(8, 5, 2))
  words <- vapply(kw, `[[`, "", "word")
  expect_equal(words[1], "cycle")                 # in two terms: 8 + 5
  expect_false(any(c("regulation", "of", "cell", "mitotic") %in% words))
  expect_equal(kw[[1]]$weight, 13)
  expect_true(all(c("checkpoint", "dna", "repair") %in% words))
  expect_length(term_keywords(character(0), numeric(0)), 0)
})

test_that("terms sharing most genes form one theme named after the best term", {
  terms <- data.frame(
    id = c("T1", "T2", "T3", "T4"), name = c("cell cycle", "mitotic cell cycle", "apoptosis", "DNA repair"),
    source = c("GO:BP", "GO:BP", "KEGG", "REAC"), pvalue = c(1e-8, 1e-5, 1e-3, 0.02),
    genes = c("A,B,C,D,E,F", "A,B,C,D,G", "X,Y,Z", "M,N"), stringsAsFactors = FALSE)
  th <- term_themes(terms, cutoff = 0.35)
  expect_equal(th$termCount, 4L)
  expect_length(th$themes, 3)
  first <- th$themes[[1]]
  expect_equal(first$label, "cell cycle")
  expect_setequal(unlist(first$terms), c("T1", "T2"))
  expect_setequal(unlist(first$genes), c("A", "B", "C", "D", "E", "F", "G"))
  expect_equal(first$cohesion, round(4 / 7, 3))
  # A strict cutoff keeps every term on its own.
  expect_length(term_themes(terms, cutoff = 0.9)$themes, 4)
})

test_that("run themes work on a result table and fail clearly when empty", {
  th <- run_themes(fake_results())
  expect_gt(length(th$themes), 0)
  expect_gt(length(th$words), 0)
  expect_error(run_themes(fake_results()[0, ]), "No terms")
})

test_that("fast ORA matches a hypergeometric test with BH correction", {
  universe <- paste0("G", 1:200)
  sets <- list(s1 = paste0("G", 1:20), s2 = paste0("G", 101:130), s3 = paste0("G", 190:200))
  query <- paste0("G", c(1:12, 150:157))
  p <- .fast_ora(query, sets, universe)
  raw <- c(stats::phyper(11, 20, 180, 20, lower.tail = FALSE), 1, 1)
  expect_equal(unname(p), stats::p.adjust(raw, "BH"))
  expect_lt(p[["s1"]], 1e-8)
})

test_that("ssGSEA scores a set higher in the samples where its genes are up", {
  set.seed(2)
  m <- matrix(stats::rnorm(400 * 6), 400, 6, dimnames = list(paste0("G", 1:400), paste0("S", 1:6)))
  m[1:30, 4:6] <- m[1:30, 4:6] + 4
  s <- .ssgsea(m, list(up = paste0("G", 1:30), other = paste0("G", 200:240)))
  expect_equal(dim(s), c(2L, 6L))
  expect_true(all(s["up", 4:6] > max(s["up", 1:3])))
})

test_that("Enrichr libraries without term IDs keep their terms, aligned per row", {
  st <- EnrichRORAStrategy$new()
  split <- st$.__enclos_env__$private$splitEnrichrTermIds
  df <- data.frame(database = c("PANGLAO", "GO:BP", "TABULA", "REAC"),
                   Term = c("Adipocytes", "cell adhesion (GO:0007155)", "Lung-pericyte Cell", "Metallothioneins Bind Metals R-HSA-5661231"),
                   stringsAsFactors = FALSE)
  r <- split(df, "hsapiens")
  expect_equal(r$ids, c("PANGLAO:Adipocytes", "GO:0007155", "TABULA:Lung-pericyte Cell", "R-HSA-5661231"))
  expect_equal(r$terms[1:2], c("Adipocytes", "cell adhesion"))
})

test_that("metabolite example comes from the published study", {
  ex <- example_input("metabolites")
  expect_true(all(c("sarcosine", "kynurenine") %in% strsplit(ex$text, "\n")[[1]]))
  expect_match(ex$source, "Sreekumar")
})

test_that("upstream regulators and drug signatures (online)", {
  skip_if_not(online(), "online tests disabled")
  genes <- c("FKBP5", "TSC22D3", "PER1", "ZBTB16", "KLF15", "DUSP1", "CEBPD", "ERRFI1", "SAA1", "MT2A",
             "CDKN1C", "KLF9", "PDK4", "IRS2", "FOXO3", "SGK1", "TIPARP", "NFKBIA", "CRISPLD2", "ALOX15B")
  tf <- upstream_regulators(genes, "tf")
  expect_equal(tf$tool, "ChEA3")
  expect_gt(length(tf$regulators), 10)
  d <- drug_connectivity(genes, character(0))
  expect_true(length(d$mimic) > 0)
})

test_that("metabolites map to KEGG and give pathway terms (online)", {
  skip_if_not(online(), "online tests disabled")
  m <- metabolite_ids(c("sarcosine", "C00148", "not a metabolite"))
  expect_true(all(c("sarcosine", "C00148") %in% names(m$mapped)))
  expect_equal(m$unmapped, "not a metabolite")
})

test_that("Reactome Analysis Service gives Reactome terms with the genes found (online)", {
  skip_if_not(online(), "online tests disabled")
  st <- ReactomeORAStrategy$new()
  genes <- c("FKBP5", "TSC22D3", "PER1", "ZBTB16", "KLF15", "DUSP1", "CEBPD", "ERRFI1", "MT2A", "CDKN1C", "KLF9", "PDK4",
             "IRS2", "FOXO3", "SGK1", "NFKBIA", "FOXO1", "CITED2", "TGFBR2", "VCAM1", "CXCL12", "MAOA", "GLUL")
  r <- st$run(genes, 9606L, NULL, list(datasources = "REAC", threshold = 0.05, metric = "fdr"))
  expect_true(nrow(r$result) > 0)
  expect_true(all(grepl("^R-HSA-", r$result$Term_ID)))
  expect_true(all(r$result$`Intersection Size` > 0))
  expect_gt(r$backgroundSize, 5000)
  expect_null(st$run(genes, 9606L, NULL, list(datasources = "GO:BP", threshold = 0.05, metric = "fdr")))
})
