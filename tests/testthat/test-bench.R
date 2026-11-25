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
  tg <- paste(c("FKBP5", sprintf("%.2f", 27 - rep(seq_len(n), each = 2) / 10 + rep(c(0, 0.05), n) + rep(c(0.013, -0.007), n) * seq_len(2 * n) / 10)), collapse = "\t")
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
  # the same experiment as fold changes already calculated in Excel: same answer, as plain values on a log2 scale
  qf <- run("bench_qpcr_fold", log = TRUE, paired = TRUE)
  expect_equal(fold(qf, "FKBP5", "Dex"), fold(q, "FKBP5", "Dex"), tolerance = 0.01)
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

test_that("GraphPad's tests: each equals R's reference implementation", {
  # 3 groups x 4 replicates (pairs = replicate number), one target
  A <- c(10.1, 11.4, 9.8, 10.9); B <- c(13.2, 14.9, 12.1, 15.3); C <- c(10.5, 12.8, 9.1, 13.6)
  txt3 <- paste(c("Gene\tA 1\tA 2\tA 3\tA 4\tB 1\tB 2\tB 3\tB 4\tC 1\tC 2\tC 3\tC 4", paste(c("X", A, B, C), collapse = "\t")), collapse = "\n")
  txt2 <- paste(c("Gene\tA 1\tA 2\tA 3\tA 4\tB 1\tB 2\tB 3\tB 4", paste(c("X", A, B), collapse = "\t")), collapse = "\n")
  p1 <- function(r) r$targets[[1]]$comparisons[[1]]$p
  om <- function(r) r$targets[[1]]$omnibus$p
  y <- c(A, B, C); g <- factor(rep(c("A", "B", "C"), each = 4)); s <- factor(rep(1:4, 3))
  # two groups
  expect_equal(p1(bench_analyse(txt2, "t.tsv", test = "t_unpaired")), t.test(B, A, var.equal = TRUE)$p.value)
  expect_equal(p1(bench_analyse(txt2, "t.tsv", test = "t_welch")), t.test(B, A)$p.value)
  expect_equal(p1(bench_analyse(txt2, "t.tsv", test = "t_paired")), t.test(B, A, paired = TRUE)$p.value)
  expect_equal(p1(bench_analyse(txt2, "t.tsv", test = "t_ratio")), t.test(log2(B), log2(A), paired = TRUE)$p.value)
  expect_equal(p1(bench_analyse(txt2, "t.tsv", test = "ks")), suppressWarnings(ks.test(B, A))$p.value)
  expect_equal(p1(bench_analyse(txt2, "t.tsv", test = "mann_whitney")), suppressWarnings(wilcox.test(B, A))$p.value)
  # three groups: omnibus tests
  expect_equal(om(bench_analyse(txt3, "t.tsv", test = "anova")), summary(aov(y ~ g))[[1]][1, "Pr(>F)"])
  expect_equal(om(bench_analyse(txt3, "t.tsv", test = "welch_anova")), oneway.test(y ~ g)$p.value)
  # Brown-Forsythe F* equals the ordinary F when the groups are the same size
  bf <- flame:::.bf_anova(y, g); expect_equal(unname(bf$stat), unname(summary(aov(y ~ g))[[1]][1, "F value"]))
  expect_equal(om(bench_analyse(txt3, "t.tsv", test = "kruskal")), kruskal.test(y ~ g)$p.value)
  expect_equal(om(bench_analyse(txt3, "t.tsv", test = "friedman")), friedman.test(matrix(y, 4))$p.value)
  # RM ANOVA: Geisser-Greenhouse epsilon checked with orthonormal contrasts
  W <- matrix(y, 4); Cn <- qr.Q(qr(contr.helmert(3))); M <- t(Cn) %*% cov(W) %*% Cn
  eps <- sum(diag(M))^2 / (2 * sum(M^2))
  tab <- summary(aov(y ~ g + s))[[1]]
  expect_equal(om(bench_analyse(txt3, "t.tsv", test = "rm_anova")), pf(tab[1, "F value"], eps * 2, eps * tab["Residuals", "Df"], lower.tail = FALSE))
  # post hoc after ordinary ANOVA: Fisher, Bonferroni, Sidak from pooled-SD t tests; Tukey = TukeyHSD
  raw <- pairwise.t.test(y, g, p.adjust.method = "none", pool.sd = TRUE)$p.value
  rawv <- c(raw["B", "A"], raw["C", "A"], raw["C", "B"])
  ps <- function(ph) { r <- bench_analyse(txt3, "t.tsv", test = "anova", posthoc = ph, allPairs = TRUE)
    stats::setNames(vapply(r$targets[[1]]$comparisons, `[[`, 0, "p"), vapply(r$targets[[1]]$comparisons, function(c) paste(c$a, c$b), "")) }
  expect_equal(unname(ps("fisher")[c("B A", "C A", "C B")]), unname(rawv))
  expect_equal(unname(ps("bonferroni")[c("B A", "C A", "C B")]), pmin(1, rawv * 3))
  expect_equal(unname(ps("sidak")[c("B A", "C A", "C B")]), 1 - (1 - rawv)^3)
  hs <- ps("holm_sidak")[c("B A", "C A", "C B")]; o <- order(rawv)
  expect_equal(unname(hs[o]), cummax(1 - (1 - rawv[o])^(3:1)))
  tk <- TukeyHSD(aov(y ~ g))$g
  expect_equal(unname(sort(ps("tukey"))), unname(sort(tk[, "p adj"])), tolerance = 1e-8)
  # unequal-SD post hoc tests reduce to Welch's t test for one comparison
  d2 <- data.frame(group = factor(rep(c("A", "B"), each = 4)), y = c(A, B))
  expect_equal(flame:::.unequal_cmp(d2, list(c("B", "A")), "games_howell", 2)$p, t.test(B, A)$p.value, tolerance = 1e-4)   # ptukey integrates numerically
  expect_equal(flame:::.unequal_cmp(d2, list(c("B", "A")), "dunnett_t3", 2)$p, t.test(B, A)$p.value)
  r <- bench_analyse(txt3, "t.tsv", test = "welch_anova", posthoc = "games_howell")
  expect_equal(length(r$targets[[1]]$comparisons), 3L)                                   # Games-Howell: all pairs
  # Dunnett is vs control only, whatever "all pairs" says
  expect_equal(length(bench_analyse(txt3, "t.tsv", test = "anova", posthoc = "dunnett", allPairs = TRUE)$targets[[1]]$comparisons), 2L)
  # wrong number of groups: a clear message
  expect_error(bench_analyse(txt3, "t.tsv", test = "t_welch"), "compares two groups")
  expect_error(bench_analyse(txt2, "t.tsv", test = "anova"), "three or more groups")
  # assumption checks
  a <- bench_analyse(txt3, "t.tsv", test = "anova")$targets[[1]]$assumptions
  expect_equal(a$normality[[1]]$p, shapiro.test(A)$p.value)
  expect_equal(a$equalSD$name, "Brown–Forsythe test")
})

test_that("one-sample tests, two-way ANOVA and corrections across rows", {
  # fold changes from Excel with the controls exactly 1: each group vs 1 (on log2)
  fc <- "Gene\tC 1\tC 2\tC 3\tD 1\tD 2\tD 3\nFKBP5\t1\t1\t1\t11.7\t12.6\t10.3\nIL6\t1\t1\t1\t0.31\t0.33\t0.28"
  r <- bench_analyse(fc, "f.tsv", log = TRUE, test = "one_sample_t", hypothetical = 1)
  expect_equal(r$targets[[1]]$comparisons[[1]]$p, t.test(log2(c(11.7, 12.6, 10.3)), mu = 0)$p.value)
  expect_match(r$sentence, "vs 1")
  # values as given: vs 100 (% of control)
  v <- "Readout\tVeh 1\tVeh 2\tVeh 3\tDrug 1\tDrug 2\tDrug 3\nViability\t100\t100\t100\t71\t68\t76"
  rv <- bench_analyse(v, "v.tsv", test = "one_sample_t", hypothetical = 100)
  expect_equal(rv$targets[[1]]$comparisons[[1]]$p, t.test(c(71, 68, 76), mu = 100)$p.value)
  # two-way ANOVA = aov(y ~ row * group); comparisons within each row from the pooled error
  e <- example_input("bench_elisa")
  r2 <- bench_analyse(e$text, e$fileName, log = TRUE, test = "twoway", posthoc = "sidak")
  x <- read.delim(text = e$text, check.names = FALSE)
  long <- data.frame(target = rep(x[[1]], 9), group = rep(sub(" [0-9]+$", "", names(x)[-1]), each = 3), v = unlist(x[-1]))
  long$y <- log2(long$v); long$group <- factor(long$group, levels = c("Unstimulated", "LPS", "LPS + dexamethasone"))
  tab <- summary(aov(y ~ target * group, data = long))[[1]]
  expect_equal(r2$anova2[[1]]$p, tab[trimws(rownames(tab)) == "target:group", "Pr(>F)"])
  expect_equal(r2$targets[[1]]$comparisons[[1]]$a, "LPS")
  expect_match(r2$sentence, "two-way ANOVA")
  expect_error(bench_analyse(e$text, e$fileName, test = "twoway", helper = "western", loading = "IL6"), NA)  # 2 rows left: fine
  # corrections across rows: Holm (default), Benjamini-Hochberg, none
  q <- example_input("bench_qpcr")
  main <- function(r) vapply(r$targets, function(t) t$omnibus$p, 0)
  holm <- bench_analyse(q$text, q$fileName, helper = "qpcr", refs = list("GAPDH", "ACTB"))
  bh <- bench_analyse(q$text, q$fileName, helper = "qpcr", refs = list("GAPDH", "ACTB"), across = "bh")
  expect_equal(vapply(bh$targets, function(t) t$omnibus$pAcross, 0), p.adjust(main(bh), "BH"))
  expect_equal(vapply(holm$targets, function(t) t$omnibus$pAcross, 0), p.adjust(main(holm), "holm"))
  expect_null(bench_analyse(q$text, q$fileName, helper = "qpcr", refs = list("GAPDH", "ACTB"), across = "none")$targets[[1]]$omnibus$pAcross)
})

test_that("normality tests equal the reference implementations", {
  skip_if_not_installed("nortest"); skip_if_not_installed("moments")
  set.seed(7)
  for (x in list(rnorm(12), rexp(20), rnorm(50), runif(30), rlnorm(9))) {
    k2 <- moments::agostino.test(x)$statistic[["z"]]^2 + moments::anscombe.test(x)$statistic[["z"]]^2
    expect_equal(flame:::.norm_dagostino(x), pchisq(k2, 2, lower.tail = FALSE), tolerance = 1e-10)
    expect_equal(flame:::.norm_anderson(x), nortest::ad.test(x)$p.value, tolerance = 1e-10)
    expect_equal(flame:::.norm_lilliefors(x), nortest::lillie.test(x)$p.value, tolerance = 1e-10)
    expect_equal(flame:::.norm_shapiro(x), shapiro.test(x)$p.value)
  }
  expect_true(is.na(flame:::.norm_dagostino(rnorm(7))))                            # needs n >= 8, as in Prism
})

test_that("the normality step recommends a test", {
  mk <- function(a, b, c = NULL) {
    cols <- c(sprintf("A %d", seq_along(a)), sprintf("B %d", seq_along(b)), if (!is.null(c)) sprintf("C %d", seq_along(c)))
    paste(c(paste(c("Readout", cols), collapse = "\t"), paste(c("X", a, b, c), collapse = "\t")), collapse = "\n")
  }
  set.seed(3)
  g <- bench_normality(mk(rnorm(10, 10), rnorm(10, 12)))                           # Gaussian, similar SDs
  expect_equal(g$recommendation$test, "t_unpaired")
  expect_equal(length(g$rows[[1]]$groups[[1]]$qq$sample), 10L)
  expect_equal(bench_normality(mk(rnorm(10, 10), rnorm(10, 12)), matched = TRUE)$recommendation$test, "t_paired")
  expect_equal(bench_normality(mk(rnorm(10, 10), rnorm(10, 12), rnorm(10, 11)))$recommendation$test, "anova")
  # clearly skewed, not lognormal either (an outlier): non-parametric
  sk <- bench_normality(mk(c(1, 1.1, 0.9, 1.2, 1, 0.95, 1.05, 1.1, 1, 9), c(2, 2.1, 1.9, 2.2, 2, 2.05, 1.95, 2.1, 2, 2)))
  expect_equal(sk$recommendation$test, "mann_whitney")
  # lognormal values: test on a log scale with a parametric test
  ln <- bench_normality(mk(round(exp(qnorm(ppoints(12), 0, 1.2)), 3), round(exp(qnorm(ppoints(12), 1, 1.2)), 3)))
  expect_true(ln$recommendation$log)
  expect_true(ln$recommendation$test %in% c("t_unpaired", "t_welch"))
  # few values: a warning that normality tests have little power
  expect_match(unlist(bench_normality(example_input("bench_qpcr_fold")$text)$warnings), "little power")
})
