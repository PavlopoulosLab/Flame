# =============================================================================
# FLAME core: experimental analysis, the statistical tests (GraphPad Prism's set)
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
    cmp <- do.call(rbind, lapply(gs, function(g) {
      v <- val(g)
      if (test == "one_sample_t") { tt <- stats::t.test(v, mu = hypo); data.frame(a = g, b = "value", diff = mean(v) - hypo, lo = tt$conf.int[1] - hypo, hi = tt$conf.int[2] - hypo, p = tt$p.value) }
      else { wt <- suppressWarnings(stats::wilcox.test(v, mu = hypo)); data.frame(a = g, b = "value", diff = stats::median(v) - hypo, lo = NA_real_, hi = NA_real_, p = wt$p.value) }
    }))
    return(list(omnibus = list(name = BENCH_TESTS$value[[test]], p = if (nrow(cmp) == 1) cmp$p else NA_real_), posthoc = NULL, comparisons = cmp, assumptions = assumptions))
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
