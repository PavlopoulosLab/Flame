grp <- function(x) flame:::.bench_group_of(x)

test_that("replicate columns are grouped by name", {
  expect_equal(grp(c("Control 1", "Control_2", "Control rep3", "Control (4)", "Drug + inhibitor 1", "Dex + RU486 2", "Ctrl")),
               c("Control", "Control", "Control", "Control", "Drug + inhibitor", "Dex + RU486", "Ctrl"))
})

test_that("two groups: Welch, paired t, Mann-Whitney equal R's own tests", {
  txt <- "A\tB\n1.1\t2.3\n1.5\t2.9\n0.9\t2.0\n1.3\t2.6"
  a <- c(1.1, 1.5, 0.9, 1.3); b <- c(2.3, 2.9, 2.0, 2.6)
  r <- bench_analyse(txt, "t.tsv", control = "A")
  expect_equal(r$targets[[1]]$comparisons[[1]]$p, t.test(b, a)$p.value)
  expect_equal(c(r$targets[[1]]$comparisons[[1]]$lo, r$targets[[1]]$comparisons[[1]]$hi), t.test(b, a)$conf.int[1:2])
  r <- bench_analyse(txt, "t.tsv", control = "A", paired = TRUE)
  expect_equal(r$targets[[1]]$comparisons[[1]]$p, t.test(b, a, paired = TRUE)$p.value)
  r <- bench_analyse(txt, "t.tsv", control = "A", nonparametric = TRUE)
  expect_equal(r$targets[[1]]$comparisons[[1]]$p, wilcox.test(b, a)$p.value)
})

test_that("three groups: ANOVA, Tukey, Dunnett, Kruskal-Wallis and Friedman", {
  txt <- example_input("bench_column")$text
  d <- read.delim(text = txt, check.names = FALSE)
  y <- unlist(d); g <- factor(rep(names(d), each = nrow(d)), levels = names(d))
  r <- bench_analyse(txt, "c.tsv")
  expect_equal(r$targets[[1]]$omnibus$p, summary(aov(y ~ g))[[1]][1, "Pr(>F)"])
  # Dunnett with a single comparison is the pooled t-test
  two <- bench_analyse("C\tT\n1\t3\n2\t4\n1.5\t3.6", "x.tsv")
  m <- flame:::.dunnett(c(C = 1.5, T = 3.533333), c(C = 3, T = 3), "C", var(c(1, 2, 1.5, 3, 4, 3.6) - rep(c(1.5, 3.533333), each = 3)) * 5 / 4, 4)
  expect_equal(m$p, t.test(c(3, 4, 3.6), c(1, 2, 1.5), var.equal = TRUE)$p.value, tolerance = 1e-4)
  r <- bench_analyse(txt, "c.tsv", allPairs = TRUE)
  tk <- TukeyHSD(aov(y ~ g))$g
  expect_equal(sort(vapply(r$targets[[1]]$comparisons, `[[`, 0, "p")), sort(unname(tk[, "p adj"])), tolerance = 1e-8)
  r <- bench_analyse(txt, "c.tsv", nonparametric = TRUE)
  expect_equal(r$targets[[1]]$omnibus$p, kruskal.test(y ~ g)$p.value)
  r <- bench_analyse(txt, "c.tsv", nonparametric = TRUE, paired = TRUE)
  expect_equal(r$targets[[1]]$omnibus$p, friedman.test(as.matrix(d))$p.value)
})

test_that("qPCR helper: fold change 2^-ddCt against a hand calculation", {
  txt <- "Gene\tC 1\tC 2\tT 1\tT 2\nGAPDH\t18\t18.2\t18.1\t17.9\nIL6\t25\t25.4\t22.1\t21.9"
  r <- bench_analyse(txt, "q.tsv", helper = "qpcr", refs = list("GAPDH"))
  dct_c <- c(25 - 18, 25.4 - 18.2); dct_t <- c(22.1 - 18.1, 21.9 - 17.9)
  expect_equal(r$targets[[1]]$comparisons[[1]]$fold, 2^-(mean(dct_t) - mean(dct_c)))
  expect_equal(r$targets[[1]]$comparisons[[1]]$p, t.test(-dct_t, -dct_c)$p.value)
  expect_equal(length(r$targets), 1L)                                   # the reference gene is not tested
})

test_that("western helper normalises to the loading control", {
  r <- bench_analyse(example_input("bench_western")$text, "w.tsv", helper = "western", loading = "ACTB")
  expect_equal(vapply(r$targets, `[[`, "", "target"), c("p-AKT (S473)", "Cleaved caspase-3"))
  expect_equal(unlist(r$groups), c("Control", "Drug", "Drug + inhibitor"))
  expect_gt(r$targets[[1]]$comparisons[[1]]$fold, 2)
})

test_that("bad tables are refused with a clear message", {
  expect_error(bench_analyse("A\n1\n2", "x.tsv"), "at least two columns")
  expect_error(bench_analyse("A\tB\n1\t2", "x.tsv", helper = "qpcr"), "grouped table")
})

test_that("user-defined sample groups replace the groups guessed from column names", {
  # Column names that say nothing about the groups: S1..S6
  tx <- "Gene\tS1\tS2\tS3\tS4\tS5\tS6\nGAPDH\t18\t18.2\t18.1\t18\t18.1\t18.2\nFKBP5\t27\t27.2\t27.1\t24\t24.2\t24.1"
  guessed <- bench_analyse(tx, "q.tsv", helper = "qpcr", refs = list("GAPDH"))
  expect_true(guessed$needsGroups)                                      # S1..S6 give no groups: ask the user
  expect_length(guessed$targets, 0)
  expect_equal(unlist(guessed$samples), paste0("S", 1:6))
  expect_false(guessed$customGroups)
  r <- bench_analyse(tx, "q.tsv", helper = "qpcr", refs = list("GAPDH"),
                     sampleGroups = list(Vehicle = list("S1", "S2", "S3"), Dex = list("S4", "S5", "S6")))
  expect_equal(unlist(r$groups), c("Vehicle", "Dex"))
  expect_equal(r$control, "Vehicle")
  expect_true(r$customGroups)
  expect_equal(unlist(r$sampleGroups), rep(c("Vehicle", "Dex"), each = 3))
  expect_gt(r$targets[[1]]$comparisons[[1]]$fold, 7)                   # 2^3 = 8-fold up
  # Samples in no group are left out; the chosen control is kept
  q <- example_input("bench_qpcr")$text
  r2 <- bench_analyse(q, "q.tsv", helper = "qpcr", refs = list("GAPDH", "ACTB"), control = "Steroid",
                      sampleGroups = list(Vehicle = list("Control 1", "Control 2", "Control 3"), Steroid = list("Dex 1", "Dex 2", "Dex 3")))
  expect_equal(unlist(r2$groups), c("Steroid", "Vehicle"))
  expect_equal(sum(unlist(r2$sampleGroups) == ""), 3L)                  # the three Dex + RU486 columns
  expect_true(all(vapply(r2$targets[[1]]$summary, `[[`, 0, "n") == 3))
})

test_that("bad sample groups are refused with a clear message", {
  q <- example_input("bench_qpcr")$text
  run <- function(g) bench_analyse(q, "q.tsv", helper = "qpcr", refs = list("GAPDH"), sampleGroups = g)
  expect_error(run(list(A = list("Control 1"), A = list("Dex 1"))), "different name")
  expect_error(run(list(A = list("nope"), B = list("Dex 1"))), "Not a column")
  expect_error(run(list(A = list("Control 1"), B = list("Control 1"))), "only one group")
  expect_error(run(list(A = list("Control 1", "Control 2"), B = list())), "two groups")
})

test_that("step 1 reads the sample columns and guesses groups from their names", {
  s <- bench_samples(example_input("bench_qpcr")$text, "q.tsv")
  expect_equal(s$layout, "grouped")
  expect_equal(length(s$samples), 9L)
  expect_equal(unique(unlist(s$guess)), c("Control", "Dex", "Dex + RU486"))
  expect_true(all(c("GAPDH", "FKBP5") %in% unlist(s$rowNames)))
  # Names with no pattern: no guess, every sample left for the user to group
  s2 <- bench_samples("Gene\tS1\tS2\tS3\tS4\nGAPDH\t18\t18.1\t18\t18.2\nFKBP5\t27\t27.1\t24\t24.1", "q.tsv")
  expect_equal(unlist(s2$samples), paste0("S", 1:4))
  expect_true(all(unlist(s2$guess) == ""))
  # Letter replicate tags
  s4 <- bench_samples("Gene\tWT_a\tWT_b\tKO_a\tKO_b\nGAPDH\t18\t18\t18\t18\nIL6\t25\t25\t23\t23", "q.tsv")
  expect_equal(unlist(s4$guess), c("WT", "WT", "KO", "KO"))
  # Every guessed group would hold one sample: no guess
  s5 <- bench_samples("Gene\tAlpha\tBeta\tGamma\nGAPDH\t18\t18\t18", "q.tsv")
  expect_true(all(unlist(s5$guess) == ""))
  # Column table: each column is a group
  s3 <- bench_samples("Control\tDrug\n1\t2\n1.1\t2.2", "c.tsv")
  expect_equal(s3$layout, "column")
  expect_equal(unlist(s3$guess), c("Control", "Drug"))
})

test_that("many groups are allowed (up to BENCH_MAX_GROUPS)", {
  n <- 20L
  hdr <- paste(c("Gene", paste0("S", seq_len(2 * n))), collapse = "\t")
  ref <- paste(c("GAPDH", rep("18", 2 * n)), collapse = "\t")
  tg <- paste(c("FKBP5", sprintf("%.1f", 27 - rep(seq_len(n), each = 2) / 10 + rep(c(0, 0.05), n))), collapse = "\t")
  groups <- stats::setNames(lapply(seq_len(n), function(i) list(paste0("S", 2 * i - 1), paste0("S", 2 * i))), paste("Dose", seq_len(n)))
  r <- bench_analyse(paste(hdr, ref, tg, sep = "\n"), "q.tsv", helper = "qpcr", refs = list("GAPDH"), sampleGroups = groups)
  expect_equal(length(r$groups), n)
  expect_equal(length(r$targets[[1]]$comparisons), n - 1L)              # each dose vs the first
  too_many <- stats::setNames(lapply(seq_len(BENCH_MAX_GROUPS + 1), function(i) list(paste0("S", i))), paste("G", seq_len(BENCH_MAX_GROUPS + 1)))
  expect_error(bench_analyse(paste(hdr, ref, tg, sep = "\n"), "q.tsv", helper = "qpcr", refs = list("GAPDH"), sampleGroups = too_many), "At most 24 groups")
})

test_that("the six example experiments load and give the expected results", {
  run <- function(kind, ...) { e <- example_input(kind); expect_match(e$source, "made-up"); bench_analyse(e$text, e$fileName, ...) }
  fold <- function(r, tg, a) { t <- Filter(function(x) x$target == tg, r$targets)[[1]]; Filter(function(c) c$a == a, t$comparisons)[[1]]$fold }
  q <- run("bench_qpcr", helper = "qpcr", refs = list("GAPDH", "ACTB"), paired = TRUE)
  expect_gt(fold(q, "FKBP5", "Dex"), 5)                                           # FKBP5, the classic glucocorticoid target
  w <- run("bench_western", helper = "western", loading = "ACTB")
  expect_gt(fold(w, "p-AKT (S473)", "Drug"), 2)
  f <- run("bench_facs")
  expect_equal(unlist(f$groups), c("Control", "Nocodazole"))
  v <- run("bench_viability")
  expect_equal(length(v$groups), 5L)
  expect_lt(v$targets[[1]]$comparisons[[4]]$diff, -70)                            # 25 uM: about 20% of vehicle
  el <- run("bench_elisa", log = TRUE)
  expect_true(all(vapply(el$targets, function(t) fold(el, t$target, "LPS"), 0) > 20))
  l <- run("bench_luciferase", helper = "western", loading = "Renilla luciferase (control)")
  expect_equal(length(l$targets), 1L)                                              # the Renilla control is not tested
  expect_gt(fold(l, "NF-kB firefly luciferase", "TNF"), 6)
  expect_lt(fold(l, "NF-kB firefly luciferase", "TNF + IKK inhibitor"), 3)
})
