# =============================================================================
# FLAME core: Bench Data, the statistical tests (GraphPad Prism's set)
# =============================================================================
#   Two groups      unpaired t (Student), Welch t, paired t, ratio paired t,
#                   Mann-Whitney, Kolmogorov-Smirnov, Wilcoxon matched pairs
#   3+ groups       ordinary one-way ANOVA, Welch ANOVA, Brown-Forsythe ANOVA,
#                   repeated-measures one-way ANOVA (Geisser-Greenhouse),
#                   Kruskal-Wallis, Friedman
#   vs a value      one-sample t test, Wilcoxon signed-rank
#   rows x groups   two-way ANOVA (in bench_analyse: it pools every target)
#   Post hoc        Dunnett, Tukey, Sidak, Bonferroni, Holm-Sidak, Fisher's LSD
#                   (pooled error); Games-Howell, Dunnett's T3 (unequal SDs);
#                   Dunn (Kruskal-Wallis / Friedman). Each group vs the control
#                   or all pairs.
#   Assumptions     Shapiro-Wilk per group; F test (2 groups) or Brown-Forsythe
#                   (3+) for equal SDs.
# Values y are on the analysis scale (log2 for qPCR / normalised / log data).
# =============================================================================

BENCH_TESTS <- list(
  two = c(t_unpaired = "unpaired t test", t_welch = "unpaired t test with Welch's correction", t_paired = "paired t test",
          t_ratio = "ratio paired t test", mann_whitney = "Mann–Whitney test", ks = "Kolmogorov–Smirnov test",
          wilcoxon = "Wilcoxon matched-pairs signed-rank test"),
  many = c(anova = "ordinary one-way ANOVA", welch_anova = "Welch ANOVA", bf_anova = "Brown–Forsythe ANOVA",
           rm_anova = "repeated-measures one-way ANOVA", kruskal = "Kruskal–Wallis test", friedman = "Friedman test",
           twoway = "two-way ANOVA"),
  value = c(one_sample_t = "one-sample t test", one_sample_wilcoxon = "Wilcoxon signed-rank test"))
BENCH_POSTHOC <- c(dunnett = "Dunnett's test", tukey = "Tukey's test", sidak = "Šidák's test", bonferroni = "Bonferroni's test",
                   holm_sidak = "Holm–Šidák test", fisher = "Fisher's LSD (uncorrected)", games_howell = "Games–Howell test",
                   dunnett_t3 = "Dunnett's T3 test", dunn = "Dunn's test")
.PAIRED_TESTS <- c("t_paired", "t_ratio", "wilcoxon", "rm_anova", "friedman")

#' The test and post hoc test actually used: "auto" follows the paired / non-parametric / all-pairs
#' choices; an explicit test is checked against the number of groups.
.bench_choose <- function(test, posthoc, k, paired, nonpar, allPairs) {
  test <- as.character(test %||% "auto")[1]
  if (identical(test, "auto") || !nzchar(test)) {
    test <- if (k == 2) { if (paired) (if (nonpar) "wilcoxon" else "t_paired") else (if (nonpar) "mann_whitney" else "t_welch") }
            else { if (paired) (if (nonpar) "friedman" else "rm_anova") else (if (nonpar) "kruskal" else "anova") }
  }
  if (test %in% names(BENCH_TESTS$two) && k != 2) stop(sprintf("The %s compares two groups; there are %d. Choose an ANOVA-type test.", BENCH_TESTS$two[[test]], k))
  if (test %in% names(BENCH_TESTS$many) && test != "twoway" && k < 3) stop(sprintf("The %s needs three or more groups; choose a t test for two.", BENCH_TESTS$many[[test]]))
  if (!test %in% c(names(BENCH_TESTS$two), names(BENCH_TESTS$many), names(BENCH_TESTS$value))) stop("Unknown test.")
  posthoc <- as.character(posthoc %||% "auto")[1]
  allowed <- switch(test,
    anova = , rm_anova = , twoway = c("dunnett", "tukey", "sidak", "bonferroni", "holm_sidak", "fisher"),
    welch_anova = , bf_anova = c("dunnett_t3", "games_howell"),
    kruskal = , friedman = "dunn", character(0))
  if (length(allowed) && (identical(posthoc, "auto") || !posthoc %in% allowed))
    posthoc <- switch(test, welch_anova = , bf_anova = if (allPairs) "games_howell" else "dunnett_t3",
                      kruskal = , friedman = "dunn", if (allPairs) "tukey" else "dunnett")
  if (!length(allowed)) posthoc <- NULL
  # Dunnett is vs the control only; Tukey and Games-Howell are all pairs only.
  if (identical(posthoc, "dunnett")) allPairs <- FALSE
  if (length(posthoc) && posthoc %in% c("tukey", "games_howell")) allPairs <- TRUE
  list(test = test, posthoc = posthoc, allPairs = allPairs, paired = test %in% .PAIRED_TESTS)
}

.pairs <- function(groups, ctrl, allPairs) {
  if (allPairs) { cb <- utils::combn(seq_along(groups), 2); lapply(seq_len(ncol(cb)), function(j) groups[rev(cb[, j])]) }
  else lapply(setdiff(groups, ctrl), function(g) c(g, ctrl))
}

#' Comparisons from a pooled error (ANOVA, RM ANOVA, two-way ANOVA): Tukey, Sidak, Bonferroni,
#' Holm-Sidak, Fisher's LSD or Dunnett, with p-values and (except Holm-Sidak) 95% CIs.
.pooled_cmp <- function(means, ns, prs, mse, df, method, k, ctrl) {
  if (method == "dunnett") return(.dunnett(means, ns, ctrl, mse, df))
  means <- stats::setNames(as.numeric(means), names(means)); ns <- stats::setNames(as.numeric(ns), names(ns))   # plain numbers, not tables
  a <- vapply(prs, `[`, "", 1); b <- vapply(prs, `[`, "", 2)
  diff <- unname(means[a] - means[b]); se <- sqrt(mse * (1 / ns[a] + 1 / ns[b]))
  t <- diff / se; raw <- 2 * stats::pt(-abs(t), df); m <- length(prs)
  p <- switch(method,
    fisher = raw, bonferroni = pmin(1, raw * m), sidak = 1 - (1 - raw)^m,
    holm_sidak = { o <- order(raw); adj <- numeric(m); adj[o] <- cummax(1 - (1 - raw[o])^(m - seq_len(m) + 1)); pmin(adj, 1) },
    tukey = stats::ptukey(abs(t) * sqrt(2), k, df, lower.tail = FALSE))
  crit <- switch(method, fisher = stats::qt(0.975, df), bonferroni = stats::qt(1 - 0.025 / m, df),
                 sidak = stats::qt(1 - (1 - 0.95^(1 / m)) / 2, df), tukey = stats::qtukey(0.95, k, df) / sqrt(2), holm_sidak = NA_real_)
  data.frame(a = a, b = b, diff = diff, lo = unname(diff - crit * se), hi = unname(diff + crit * se), p = unname(p), stringsAsFactors = FALSE)
}

#' Comparisons with each group's own SD (after Welch / Brown-Forsythe ANOVA): Games-Howell or Dunnett's T3.
.unequal_cmp <- function(d, prs, method, k) {
  out <- lapply(prs, function(pr) {
    ya <- d$y[d$group == pr[1]]; yb <- d$y[d$group == pr[2]]
    va <- stats::var(ya) / length(ya); vb <- stats::var(yb) / length(yb)
    se <- sqrt(va + vb); df <- (va + vb)^2 / (va^2 / (length(ya) - 1) + vb^2 / (length(yb) - 1))
    data.frame(a = pr[1], b = pr[2], diff = mean(ya) - mean(yb), se = se, df = df, stringsAsFactors = FALSE)
  })
  o <- do.call(rbind, out); t <- o$diff / o$se; m <- nrow(o)
  if (method == "games_howell") {
    o$p <- stats::ptukey(abs(t) * sqrt(2), k, o$df, lower.tail = FALSE); crit <- stats::qtukey(0.95, k, o$df) / sqrt(2)
  } else {                                                    # T3: Studentized maximum modulus, as Sidak on Welch t tests
    o$p <- pmin(1, 1 - (1 - 2 * stats::pt(-abs(t), o$df))^m); crit <- stats::qt(1 - (1 - 0.95^(1 / m)) / 2, o$df)
  }
  data.frame(a = o$a, b = o$b, diff = o$diff, lo = o$diff - crit * o$se, hi = o$diff + crit * o$se, p = o$p, stringsAsFactors = FALSE)
}

# Dunn's test after Friedman: mean within-block ranks, Bonferroni over the comparisons made.
.dunn_friedman <- function(wide, prs) {
  r <- t(apply(wide, 1, rank)); k <- ncol(wide); n <- nrow(wide)
  mr <- colMeans(r); se <- sqrt(k * (k + 1) / (6 * n))
  p <- vapply(prs, function(pr) 2 * stats::pnorm(-abs((mr[[pr[1]]] - mr[[pr[2]]]) / se)), 0)
  data.frame(a = vapply(prs, `[`, "", 1), b = vapply(prs, `[`, "", 2), p = pmin(1, p * length(prs)), stringsAsFactors = FALSE)
}

# Brown-Forsythe test for equal means (F* with Satterthwaite's denominator df).
.bf_anova <- function(y, g) {
  n <- tapply(y, g, length); m <- tapply(y, g, mean); s2 <- tapply(y, g, stats::var); N <- sum(n)
  w <- (1 - n / N) * s2
  Fs <- sum(n * (m - mean(y))^2) / sum(w)
  c_ <- w / sum(w); df2 <- 1 / sum(c_^2 / (n - 1)); df1 <- length(n) - 1
  list(stat = Fs, df1 = df1, df2 = df2, p = stats::pf(Fs, df1, df2, lower.tail = FALSE))
}

# Geisser-Greenhouse epsilon from the subjects x groups table.
.gg_epsilon <- function(wide) {
  S <- stats::cov(wide); k <- ncol(S)
  C <- diag(k) - 1 / k; Sc <- C %*% S %*% C
  min(1, max(1 / (k - 1), sum(diag(Sc))^2 / ((k - 1) * sum(Sc^2))))
}

.fmt_stat <- function(x, digits = 3) formatC(x, format = "f", digits = if (abs(x) >= 100) 1 else 2)

#' Test one target. d: rows with group, pair, y. Returns omnibus (name, p, detail), posthoc, comparisons, assumptions.
.bench_test_target <- function(d, groups, ctrl, ch, hypo = 0) {
  d <- d[d$group %in% groups, , drop = FALSE]
  d$group <- factor(d$group, levels = groups)
  k <- length(groups); test <- ch$test
  if (ch$paired) {                                            # keep complete replicate sets only
    full <- names(which(table(d$pair) == k))
    d <- d[as.character(d$pair) %in% full, , drop = FALSE]
    if (length(full) < 2) return(list(error = "Paired tests need at least 2 complete replicate sets (the same replicate number in every group)."))
  }
  ns <- table(d$group)
  if (any(ns[if (test %in% names(BENCH_TESTS$value)) setdiff(groups, ctrl) else groups] < 2)) return(list(error = "Each group needs at least 2 values."))
  means <- tapply(d$y, d$group, mean)
  prs <- .pairs(groups, ctrl, ch$allPairs)
  val <- function(g) d$y[d$group == g][order(d$pair[d$group == g])]
  assumptions <- .bench_assumptions(d, groups)

  if (test %in% names(BENCH_TESTS$value)) {                    # each group (but the control) vs a hypothetical value
    gs <- setdiff(groups, ctrl)
    # A group whose values are all the same (e.g. a control set to 100 in every experiment) has no variation to
    # test: it is left out with a note, and the other groups are still tested.
    flat <- vapply(gs, function(g) length(unique(val(g))) == 1, TRUE)
    notes <- vapply(gs[flat], function(g) sprintf("%s: all %d values are %s, so it cannot be tested against %s (no variation).",
                                                  g, length(val(g)), format(signif(val(g)[1], 4)), format(signif(hypo, 4))), "")
    gs <- gs[!flat]
    if (!length(gs)) return(list(error = paste(c(notes, "No group could be tested."), collapse = " "), notes = as.list(unname(notes))))
    cmp <- do.call(rbind, lapply(gs, function(g) {
      v <- val(g)
      if (test == "one_sample_t") { tt <- stats::t.test(v, mu = hypo); data.frame(a = g, b = "value", diff = mean(v) - hypo, lo = tt$conf.int[1] - hypo, hi = tt$conf.int[2] - hypo, p = tt$p.value) }
      else { wt <- suppressWarnings(stats::wilcox.test(v, mu = hypo)); data.frame(a = g, b = "value", diff = stats::median(v) - hypo, lo = NA_real_, hi = NA_real_, p = wt$p.value) }
    }))
    return(list(omnibus = list(name = BENCH_TESTS$value[[test]], p = if (nrow(cmp) == 1) cmp$p else NA_real_), posthoc = NULL, comparisons = cmp, assumptions = assumptions,
                notes = as.list(unname(notes))))
  }

  if (k == 2) {
    a <- setdiff(groups, ctrl); ya <- val(a); yc <- val(ctrl)
    r <- switch(test,
      t_unpaired = , t_welch = , t_paired = {
        tt <- stats::t.test(ya, yc, paired = test == "t_paired", var.equal = test == "t_unpaired")
        list(diff = mean(ya) - mean(yc), lo = tt$conf.int[1], hi = tt$conf.int[2], p = tt$p.value,
             detail = sprintf("t = %s, df = %s", .fmt_stat(abs(tt$statistic)), .fmt_stat(tt$parameter)))
      },
      t_ratio = {
        if (any(ya <= 0) || any(yc <= 0)) stop("The ratio paired t test needs positive values.")
        tt <- stats::t.test(log2(ya), log2(yc), paired = TRUE)
        list(diff = mean(ya) - mean(yc), lo = NA_real_, hi = NA_real_, p = tt$p.value,
             detail = sprintf("on log values; geometric mean ratio %s (95%% CI %s to %s)", signif(2^tt$estimate, 3), signif(2^tt$conf.int[1], 3), signif(2^tt$conf.int[2], 3)))
      },
      mann_whitney = , wilcoxon = {
        wt <- suppressWarnings(stats::wilcox.test(ya, yc, paired = test == "wilcoxon"))
        list(diff = stats::median(ya) - stats::median(yc), lo = NA_real_, hi = NA_real_, p = wt$p.value,
             detail = sprintf("%s = %s", if (test == "wilcoxon") "W" else "U", .fmt_stat(wt$statistic)))
      },
      ks = {
        kt <- suppressWarnings(stats::ks.test(ya, yc))
        list(diff = stats::median(ya) - stats::median(yc), lo = NA_real_, hi = NA_real_, p = kt$p.value, detail = sprintf("D = %s", .fmt_stat(kt$statistic)))
      })
    cmp <- data.frame(a = a, b = ctrl, diff = r$diff, lo = r$lo, hi = r$hi, p = r$p, stringsAsFactors = FALSE)
    return(list(omnibus = list(name = BENCH_TESTS$two[[test]], p = r$p, detail = r$detail), posthoc = NULL, comparisons = cmp, assumptions = assumptions))
  }

  post <- ch$posthoc
  postName <- if (ch$allPairs) sprintf("%s (all pairs)", BENCH_POSTHOC[[post]]) else sprintf("%s vs %s", BENCH_POSTHOC[[post]], ctrl)
  if (test %in% c("anova", "rm_anova")) {
    fit <- if (test == "rm_anova") stats::aov(y ~ group + factor(pair), data = d) else stats::aov(y ~ group, data = d)
    tab <- summary(fit)[[1]]
    Fv <- tab[1, "F value"]; df1 <- tab[1, "Df"]; mse <- tab["Residuals", "Mean Sq"]; df <- tab["Residuals", "Df"]
    p <- tab[1, "Pr(>F)"]; detail <- sprintf("F(%s, %s) = %s", df1, df, .fmt_stat(Fv))
    if (test == "rm_anova") {
      wide <- tapply(d$y, list(d$pair, d$group), mean)
      eps <- .gg_epsilon(wide)
      p <- stats::pf(Fv, eps * df1, eps * df, lower.tail = FALSE)
      detail <- sprintf("%s; Geisser–Greenhouse ε = %s", detail, formatC(eps, format = "f", digits = 3))
    }
    cmp <- .pooled_cmp(means, ns, prs, mse, df, post, k, ctrl)
    return(list(omnibus = list(name = BENCH_TESTS$many[[test]], p = p, detail = detail), posthoc = postName, comparisons = cmp, assumptions = assumptions))
  }
  if (test %in% c("welch_anova", "bf_anova")) {
    if (test == "welch_anova") { o <- stats::oneway.test(y ~ group, data = d, var.equal = FALSE); st <- list(stat = o$statistic, df1 = o$parameter[1], df2 = o$parameter[2], p = o$p.value) }
    else st <- .bf_anova(d$y, d$group)
    cmp <- .unequal_cmp(d, prs, post, k)
    return(list(omnibus = list(name = BENCH_TESTS$many[[test]], p = st$p, detail = sprintf("%s(%s, %s) = %s", if (test == "welch_anova") "W" else "F*",
                                .fmt_stat(st$df1), .fmt_stat(st$df2), .fmt_stat(st$stat))), posthoc = postName, comparisons = cmp, assumptions = assumptions))
  }
  if (test == "friedman") {
    wide <- tapply(d$y, list(d$pair, d$group), mean)
    ft <- stats::friedman.test(wide)
    dn <- .dunn_friedman(wide, prs)
    med <- apply(wide, 2, stats::median)
    cmp <- data.frame(a = dn$a, b = dn$b, diff = unname(med[dn$a] - med[dn$b]), lo = NA_real_, hi = NA_real_, p = dn$p)
    return(list(omnibus = list(name = BENCH_TESTS$many[[test]], p = ft$p.value, detail = sprintf("Friedman statistic = %s", .fmt_stat(ft$statistic))),
                posthoc = postName, comparisons = cmp, assumptions = assumptions))
  }
  # Kruskal-Wallis + Dunn
  kt <- stats::kruskal.test(y ~ group, data = d)
  dn <- .dunn(d$y, as.character(d$group), prs)
  med <- tapply(d$y, d$group, stats::median)
  cmp <- data.frame(a = dn$a, b = dn$b, diff = unname(med[dn$a] - med[dn$b]), lo = NA_real_, hi = NA_real_, p = dn$p)
  list(omnibus = list(name = BENCH_TESTS$many[["kruskal"]], p = kt$p.value, detail = sprintf("Kruskal–Wallis statistic = %s", .fmt_stat(kt$statistic))),
       posthoc = postName, comparisons = cmp, assumptions = assumptions)
}

#' Two-way ANOVA over all targets (rows x groups): main effects and interaction, and the comparisons
#' within each row from the pooled error (a family per row, as in Prism).
.bench_twoway <- function(d, groups, ctrl, ch) {
  d <- d[d$group %in% groups, , drop = FALSE]
  d$group <- factor(d$group, levels = groups); d$target <- factor(d$target, levels = unique(d$target))
  if (nlevels(d$target) < 2) stop("A two-way ANOVA needs two or more rows (genes or readouts) besides the reference or normalising rows.")
  if (any(table(d$target, d$group) < 2)) stop("Each row needs at least 2 values in every group for a two-way ANOVA.")
  fit <- stats::aov(y ~ target * group, data = d)
  tab <- summary(fit)[[1]]; rn <- trimws(rownames(tab))
  mse <- tab[rn == "Residuals", "Mean Sq"]; df <- tab[rn == "Residuals", "Df"]
  row <- function(nm, label) list(effect = label, F = signif(tab[rn == nm, "F value"], 4), df1 = tab[rn == nm, "Df"], df2 = df, p = tab[rn == nm, "Pr(>F)"])
  table2 <- list(row("target:group", "Interaction (row × group)"), row("group", "Group (treatment)"), row("target", "Row (gene / readout)"))
  postName <- if (ch$allPairs) sprintf("%s (all pairs, within each row)", BENCH_POSTHOC[[ch$posthoc]]) else sprintf("%s vs %s, within each row", BENCH_POSTHOC[[ch$posthoc]], ctrl)
  per <- lapply(levels(d$target), function(tg) {
    dt <- d[d$target == tg, , drop = FALSE]
    means <- tapply(dt$y, dt$group, mean); ns <- table(dt$group)
    list(target = tg, comparisons = .pooled_cmp(means, ns, .pairs(groups, ctrl, ch$allPairs), mse, df, ch$posthoc, length(groups), ctrl),
         assumptions = .bench_assumptions(dt, groups))
  })
  list(table = table2, posthoc = postName, per = stats::setNames(per, levels(d$target)))
}

#' Normality (Shapiro-Wilk, n = 3-5000) per group and equal SDs (F test for 2 groups, Brown-Forsythe for more).
.bench_assumptions <- function(d, groups) {
  norm <- lapply(groups, function(g) { v <- d$y[d$group == g]
    p <- if (length(v) >= 3 && length(unique(v)) > 1) tryCatch(stats::shapiro.test(v)$p.value, error = function(e) NA_real_) else NA_real_
    list(group = g, n = length(v), p = p) })
  ok <- vapply(groups, function(g) sum(d$group == g) >= 2, TRUE)
  sd_ <- if (sum(ok) >= 2) tryCatch({
    if (length(groups) == 2) list(name = "F test", p = stats::var.test(d$y[d$group == groups[2]], d$y[d$group == groups[1]])$p.value)
    else { dev <- abs(d$y - stats::ave(d$y, d$group, FUN = stats::median)); list(name = "Brown–Forsythe test", p = suppressWarnings(summary(stats::aov(dev ~ d$group))[[1]][1, "Pr(>F)"])) }   # n = 2: a perfect fit, no warning
  }, error = function(e) NULL) else NULL
  list(normality = norm, equalSD = sd_)
}

# =============================================================================
# Normality tests (GraphPad Prism's set), to help choose a test
# =============================================================================
#   D'Agostino-Pearson omnibus K2 (n >= 8), Anderson-Darling (n >= 8),
#   Shapiro-Wilk (n 3-5000), Kolmogorov-Smirnov with the Dallal-Wilkinson-
#   Lilliefors p-value (n >= 5); lognormal vs normal likelihood.

.norm_dagostino <- function(x) {
  n <- length(x); if (n < 8) return(NA_real_)
  m <- mean(x); m2 <- mean((x - m)^2); m3 <- mean((x - m)^3); m4 <- mean((x - m)^4)
  if (m2 == 0) return(NA_real_)
  g1 <- m3 / m2^1.5; b2 <- m4 / m2^2
  # skewness (D'Agostino 1970)
  Y <- g1 * sqrt((n + 1) * (n + 3) / (6 * (n - 2)))
  beta2 <- 3 * (n^2 + 27 * n - 70) * (n + 1) * (n + 3) / ((n - 2) * (n + 5) * (n + 7) * (n + 9))
  W2 <- -1 + sqrt(2 * (beta2 - 1)); delta <- 1 / sqrt(log(sqrt(W2))); alpha <- sqrt(2 / (W2 - 1))
  Zs <- delta * log(Y / alpha + sqrt((Y / alpha)^2 + 1))
  # kurtosis (Anscombe & Glynn 1983)
  E <- 3 * (n - 1) / (n + 1); V <- 24 * n * (n - 2) * (n - 3) / ((n + 1)^2 * (n + 3) * (n + 5))
  xk <- (b2 - E) / sqrt(V)
  sb1 <- 6 * (n^2 - 5 * n + 2) / ((n + 7) * (n + 9)) * sqrt(6 * (n + 3) * (n + 5) / (n * (n - 2) * (n - 3)))
  A <- 6 + 8 / sb1 * (2 / sb1 + sqrt(1 + 4 / sb1^2))
  t3 <- (1 - 2 / A) / (1 + xk * sqrt(2 / (A - 4)))
  Zk <- ((1 - 2 / (9 * A)) - sign(t3) * abs(t3)^(1 / 3)) / sqrt(2 / (9 * A))
  stats::pchisq(Zs^2 + Zk^2, 2, lower.tail = FALSE)
}

.norm_anderson <- function(x) {
  n <- length(x); if (n < 8 || stats::sd(x) == 0) return(NA_real_)
  z <- (sort(x) - mean(x)) / stats::sd(x)
  h <- (2 * seq_len(n) - 1) * (stats::pnorm(z, log.p = TRUE) + rev(stats::pnorm(-z, log.p = TRUE)))
  AA <- (1 + 0.75 / n + 2.25 / n^2) * (-n - mean(h))
  if (AA < 0.2) 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
  else if (AA < 0.34) 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
  else if (AA < 0.6) exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
  else if (AA < 10) exp(1.2937 - 5.709 * AA + 0.0186 * AA^2) else 3.7e-24
}

.norm_lilliefors <- function(x) {
  n <- length(x); if (n < 5 || stats::sd(x) == 0) return(NA_real_)
  p <- stats::pnorm((sort(x) - mean(x)) / stats::sd(x))
  K <- max(seq_len(n) / n - p, p - (seq_len(n) - 1) / n)
  if (n <= 100) { Kd <- K; nd <- n } else { Kd <- K * (n / 100)^0.49; nd <- 100 }
  pv <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 * Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598 / sqrt(nd) + 1.67997 / nd)
  if (pv > 0.1) {
    KK <- (sqrt(n) - 0.01 + 0.85 / sqrt(n)) * K
    pv <- if (KK <= 0.302) 1
      else if (KK <= 0.5) 2.76773 - 19.828315 * KK + 80.709644 * KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
      else if (KK <= 0.9) -4.901232 + 40.662806 * KK - 97.490286 * KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
      else if (KK <= 1.31) 6.198765 - 19.558097 * KK + 23.186922 * KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4 else 0
  }
  min(max(pv, 0), 1)
}

.norm_shapiro <- function(x) if (length(x) >= 3 && length(x) <= 5000 && length(unique(x)) > 1) tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_) else NA_real_

# Probability that the values come from a lognormal rather than a normal distribution (likelihood ratio).
.prob_lognormal <- function(x) {
  if (length(x) < 3 || any(x <= 0) || stats::sd(x) == 0) return(NA_real_)
  n <- length(x); sdn <- sqrt(mean((x - mean(x))^2)); lx <- log(x); sdl <- sqrt(mean((lx - mean(lx))^2))
  if (sdl == 0) return(NA_real_)
  lln <- sum(stats::dnorm(x, mean(x), sdn, log = TRUE)); lll <- sum(stats::dlnorm(x, mean(lx), sdl, log = TRUE))
  1 / (1 + exp(lln - lll))
}

#' The normality tests of each group (per row), equal-SD checks, QQ-plot points, and a recommended test.
#' matched: the same experiment / donor in every group (recommends paired tests).
bench_normality <- function(text, fileName = "", layout = "auto", sampleGroups = NULL, log = FALSE, matched = FALSE) {
  p <- bench_parse(text, fileName, layout, sampleGroups)
  if (isTRUE(p$needsGroups)) stop("Put the samples into groups first.")
  d <- p$data; groups <- p$groups; k <- length(groups)
  if (isTRUE(log)) { if (any(d$value <= 0)) stop("A log scale needs positive values."); d$value <- log2(d$value) }
  one <- function(x) list(n = length(x), shapiro = .norm_shapiro(x), dagostino = .norm_dagostino(x), anderson = .norm_anderson(x),
                          ks = .norm_lilliefors(x), lognormal = if (isTRUE(log)) NA_real_ else .prob_lognormal(x))
  # the test Prism suggests reading: D'Agostino-Pearson from n = 8, Shapiro-Wilk below
  primary <- function(r) if (is.finite(r$dagostino)) r$dagostino else r$shapiro
  rows <- lapply(unique(d$target), function(tg) {
    dt <- d[d$target == tg, , drop = FALSE]
    gr <- lapply(groups, function(g) { x <- dt$value[dt$group == g]; r <- one(x); r$group <- g; r$passed <- if (is.finite(primary(r))) primary(r) >= 0.05 else NA
      qq <- if (length(x) >= 3) { o <- sort(x); list(theoretical = signif(stats::qnorm(stats::ppoints(length(o))), 4), sample = signif(o, 6)) } else NULL
      r$qq <- qq; r })
    dt$y <- dt$value
    list(target = tg, groups = gr, equalSD = .bench_assumptions(dt, groups)$equalSD)
  })
  flat <- unlist(lapply(rows, function(r) lapply(r$groups, function(g) g$passed)))
  tested <- flat[!is.na(flat)]
  allNormal <- length(tested) > 0 && all(tested)
  sdOK <- all(vapply(rows, function(r) is.null(r$equalSD) || !is.finite(r$equalSD$p) || r$equalSD$p >= 0.05, TRUE))
  lnP <- unlist(lapply(rows, function(r) lapply(r$groups, function(g) g$lognormal)))
  lnP <- lnP[is.finite(lnP)]
  minN <- min(unlist(lapply(rows, function(r) lapply(r$groups, function(g) g$n))))
  why <- character(0)
  logSuggest <- !isTRUE(log) && !allNormal && length(lnP) > 0 && mean(lnP > 0.5) > 0.5 && all(d$value > 0)
  if (logSuggest) {
    # do the log values pass?
    lg <- d; lg$value <- log2(lg$value)
    passLog <- unlist(lapply(unique(lg$target), function(tg) lapply(groups, function(g) { x <- lg$value[lg$target == tg & lg$group == g]; r <- one(x); pp <- primary(r); if (is.finite(pp)) pp >= 0.05 else NA })))
    passLog <- passLog[!is.na(passLog)]
    logSuggest <- length(passLog) > 0 && all(passLog)
  }
  parametric <- allNormal || logSuggest
  test <- if (k == 2) { if (!parametric) (if (matched) "wilcoxon" else "mann_whitney") else if (matched) (if (logSuggest) "t_ratio" else "t_paired") else if (sdOK) "t_unpaired" else "t_welch" }
          else { if (!parametric) (if (matched) "friedman" else "kruskal") else if (matched) "rm_anova" else if (sdOK) "anova" else "welch_anova" }
  if (logSuggest && test == "t_ratio") logSuggest <- FALSE                    # the ratio paired t test logs the values itself
  if (allNormal) why <- c(why, "Every group passed the normality test (p ≥ 0.05), so a parametric test fits.")
  else if (logSuggest) why <- c(why, "Some groups failed the normality test, but the values look lognormal and pass on a log scale: test them on a log2 scale with a parametric test.")
  else if (length(tested)) why <- c(why, "At least one group failed the normality test (p < 0.05): a non-parametric test does not assume Gaussian data.")
  if (parametric && !matched && test %in% c("t_unpaired", "anova")) why <- c(why, "The SDs look similar (equal-SD test p ≥ 0.05).")
  if (parametric && !matched && test %in% c("t_welch", "welch_anova")) why <- c(why, "The SDs differ (equal-SD test p < 0.05): Welch's version does not assume equal SDs.")
  if (matched) why <- c(why, "The samples are matched (the same experiment or donor in every group), so a paired / repeated-measures test is used.")
  warn <- character(0)
  if (minN < 8) warn <- c(warn, sprintf("With %d value%s per group, D'Agostino–Pearson and Anderson–Darling need at least 8, and normality tests in general have little power: a pass does not prove the data are Gaussian. With so few values, decide from what is known about this kind of data (e.g. qPCR ΔCt and log fold changes are usually close to Gaussian).", minN, if (minN == 1) "" else "s"))
  list(groups = as.list(groups), rows = rows, log = isTRUE(log),
       recommendation = list(test = test, log = logSuggest, label = c(BENCH_TESTS$two, BENCH_TESTS$many)[[test]], why = as.list(why)),
       warnings = as.list(warn))
}
