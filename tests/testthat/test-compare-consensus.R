mk_cons_run <- function(id, tool, ids, p) {
  r <- fake_results()[0, ]
  r <- r[rep(1, length(ids)), ]
  r$Source <- "GO:BP"; r$Term_ID <- ids; r$Term_ID_noLinks <- ids; r$Function <- paste("term", ids)
  r$`P-value` <- p; r$`-log10Pvalue` <- -log10(p); r$`Term Size` <- 50
  r$`Intersection Size` <- 3; r$`Positive Hits` <- "A,B,C"; r$link <- NA
  list(id = id, tool = tool, displayNumber = 1, organism = 9606, listName = "L", analyteType = "GENE",
       inputIds = LETTERS[1:10], params = list(datasources = "GO:BP", threshold = 0.05, metric = "fdr"),
       results = r, conversionTable = data.frame(input = LETTERS, target = LETTERS, name = LETTERS), backgroundSize = 20000L)
}

test_that("RRA score: uniform ranks are unremarkable, consistently high ranks are not", {
  expect_equal(rra_score(c(1, 1, 1)), 1)
  expect_lt(rra_score(c(0.01, 0.01, 0.01)), 1e-4)
  # One strong rank and two misses is weaker than three good ranks.
  expect_gt(rra_score(c(0.002, 1, 1)), rra_score(c(0.04, 0.04, 0.04)))
})

test_that("consensus ranking prefers agreed terms over strong single-run hits; Fisher does not", {
  filler <- function(prefix, n) sprintf("GO:%s%05d", prefix, seq_len(n))
  # "GO:SOLO" is the top term of run a only; "GO:SHARED" is ranked 5th by all three runs.
  runs <- list(
    mk_cons_run("a_1", "gProfiler", c("GO:SOLO", filler("1", 3), "GO:SHARED", filler("1", 30)[-(1:3)]), c(1e-12, 10^-(9:7), 1e-4, seq(1e-3, 0.04, length.out = 27))),
    mk_cons_run("b_2", "STRING", c(filler("2", 4), "GO:SHARED", filler("2", 30)[-(1:4)]), c(10^-(9:6), 1e-4, seq(1e-3, 0.04, length.out = 26))),
    mk_cons_run("c_3", "Enrichr", c(filler("3", 4), "GO:SHARED", filler("3", 30)[-(1:4)]), c(10^-(9:6), 1e-4, seq(1e-3, 0.04, length.out = 26))))
  cs <- suppressMessages(build_comparison(runs))
  pos <- function(v, id) which(vapply(v$terms, `[[`, "", "id") == id)

  fisher <- comparison_view(cs, "bubble", top = "all", sortBy = "-log10Pvalue")
  expect_lt(pos(fisher, "GO:SOLO"), pos(fisher, "GO:SHARED"))

  top <- comparison_view(cs, "top", top = 10, sortBy = "Consensus", settings = list(minAgreement = 1))
  expect_equal(top$terms[[1]]$id, "GO:SHARED")
  expect_equal(top$terms[[1]]$agreement, 3L)
  expect_equal(length(top$terms[[1]]$cells), 3)
  expect_equal(top$terms[[1]]$cells$a_1$rank, 5)

  # Default: found by a majority of runs, so single-run terms are left out.
  maj <- comparison_view(cs, "top", top = "all")
  expect_equal(maj$minAgreement, 2L)
  expect_equal(maj$eligibleCount, 1L)
  expect_equal(maj$terms[[1]]$id, "GO:SHARED")

  best <- comparison_view(cs, "top", top = 3, sortBy = "Best rank", settings = list(minAgreement = 1))
  expect_true("GO:SOLO" %in% vapply(best$terms, `[[`, "", "id"))
})

test_that("top terms: similar terms can be collapsed", {
  ids <- sprintf("GO:%07d", 1:4)
  r1 <- mk_cons_run("a_1", "gProfiler", ids, c(1e-8, 1e-7, 1e-6, 1e-5))
  r2 <- mk_cons_run("b_2", "STRING", ids, c(1e-8, 1e-7, 1e-6, 1e-5))
  r1$results$`Positive Hits` <- r2$results$`Positive Hits` <- c("A,B,C", "A,B,C", "D,E,F", "G,H,I")
  cs <- suppressMessages(build_comparison(list(r1, r2)))
  v <- comparison_view(cs, "top", top = "all", settings = list(collapse = TRUE, collapseCutoff = 0.5))
  expect_equal(v$collapsed, 1L)
  expect_equal(v$terms[[1]]$represents, 2L)
  expect_length(v$terms, 3)
})

test_that("statistics report rank agreement over all terms, not only shared ones", {
  ids <- sprintf("GO:%07d", 1:12)
  cs <- suppressMessages(build_comparison(list(
    mk_cons_run("a_1", "gProfiler", ids[1:10], 10^-(10:1)), mk_cons_run("b_2", "STRING", ids[3:12], 10^-(10:1)))))
  p <- comparison_statistics(cs)$pairs[[1]]
  expect_equal(p$spearman, 1)
  expect_lt(p$spearmanAll, p$spearman)
})

test_that("top terms: one panel per source, each with its own ranking; gene ratio per run", {
  ids <- c(sprintf("GO:%07d", 1:4), sprintf("KEGG:%05d", 1:3))
  mk2 <- function(id, tool) { r <- mk_cons_run(id, tool, ids, 10^-(7:1)); r$results$Source <- rep(c("GO:BP", "KEGG"), c(4, 3)); r }
  cs <- suppressMessages(build_comparison(list(mk2("a_1", "gProfiler"), mk2("b_2", "STRING"))))
  v <- comparison_view(cs, "top", top = 2, settings = list(splitBySource = TRUE))
  expect_equal(vapply(v$panels, `[[`, "", "source"), c("GO:BP", "KEGG"))
  expect_equal(v$shownCount, 4)
  expect_length(v$terms, 4)
  k <- v$panels[[2]]$terms[[1]]
  expect_equal(k$id, "map00001")             # KEGG IDs are matched organism-neutrally
  expect_equal(k$cells$a_1$rank, 1)                 # ranked within KEGG, not after the GO terms
  expect_equal(k$cells$a_1$geneRatio, round(3 / 10, 4))   # 3 genes of 10 mapped inputs
  one <- comparison_view(cs, "top", top = 2)
  expect_length(one$panels, 1)
  expect_null(one$panels[[1]]$source)
})
