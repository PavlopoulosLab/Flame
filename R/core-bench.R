# =============================================================================
# FLAME core: bench data (quick statistics and figures for small experiments)
# =============================================================================
# A generic, quick tool, not a replacement for Prism, qbase+ or FlowJo: paste a
# small table, get a sensible test and a figure, and send the genes to Flame.
#
#   Column table   one column per group, one row per replicate (one target)
#   Grouped table  one row per gene / target, replicate columns under each group
#                  ("Control 1", "Control 2", ... or repeated "Control" headers)
#
# Helpers: qPCR (Ct -> -dCt, fold change 2^-ddCt), western (band / loading
# control, log2), FACS phases (percentages, stacked bars). Tests: Welch / paired
# t, Mann-Whitney / Wilcoxon; one-way or repeated-measures ANOVA with Dunnett vs
# control (mvtnorm) or Tukey; Kruskal-Wallis + Dunn; Friedman + Wilcoxon (Holm).
# Technical replicates are expected to be averaged already (one value per sample).
# =============================================================================

BENCH_HELPERS <- c("none", "qpcr", "western", "facs")

.bench_group_of <- function(name) {
  # "Control 1", "Control_2", "Control rep3", "Control (4)", "Control.1" -> "Control" (a tag needs a separator before it)
  g <- trimws(sub("(?:[ _.#-]+(?:rep|r|n|sample))?[ _.#-]*\\(?[0-9]+\\)?$", "", name, ignore.case = TRUE, perl = TRUE))
  ifelse(nzchar(g), g, name)
}

#' Parse a pasted bench table into long form: target, group, sample, pair (replicate key), value.
bench_parse <- function(text, fileName = "", layout = "auto") {
  df <- parse_delimited_table(text, fileName)
  if (ncol(df) < 2) stop("Paste a table with at least two columns: groups side by side.")
  num <- lapply(df, function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE))))
  firstText <- mean(is.na(num[[1]]) & nzchar(trimws(as.character(df[[1]])))) > 0.5
  layout <- if (identical(layout, "column") || identical(layout, "grouped")) layout else if (firstText) "grouped" else "column"
  if (layout == "grouped") {
    if (!firstText) stop("A grouped table needs gene or target names in its first column.")
    targets <- make.unique(trimws(as.character(df[[1]])))
    cols <- names(df)[-1]
    groups <- .bench_group_of(cols)
    pos <- stats::ave(seq_along(groups), groups, FUN = seq_along)
    long <- do.call(rbind, lapply(seq_along(cols), function(j) data.frame(
      target = targets, group = groups[j], sample = cols[j], pair = pos[j], value = num[[j + 1]], stringsAsFactors = FALSE)))
  } else {
    groups <- trimws(names(df))
    long <- do.call(rbind, lapply(seq_along(groups), function(j) data.frame(
      target = "value", group = groups[j], sample = paste(groups[j], seq_len(nrow(df))), pair = seq_len(nrow(df)),
      value = num[[j]], stringsAsFactors = FALSE)))
  }
  long <- long[is.finite(long$value), , drop = FALSE]
  if (!nrow(long)) stop("No numbers were found in the table.")
  grp <- unique(long$group)
  if (length(grp) < 2) stop("At least two groups are needed (e.g. Control and Treated columns).")
  if (length(grp) > 12) stop("At most 12 groups are supported.")
  if (length(unique(long$target)) > 60) stop("At most 60 genes or targets are supported.")
  list(layout = layout, data = long, groups = grp, targets = unique(long$target))
}

# ---- helpers ------------------------------------------------------------------------

.bench_transform <- function(p, helper, refs = NULL, loading = NULL, log = FALSE) {
  d <- p$data
  if (helper == "qpcr") {
    refs <- intersect(as.character(unlist(refs)), p$targets)
    if (!length(refs)) stop("Choose the reference gene(s), e.g. GAPDH or ACTB.")
    ref <- stats::aggregate(value ~ sample, d[d$target %in% refs, ], mean)
    d <- d[!d$target %in% refs, , drop = FALSE]
    d$y <- -(d$value - ref$value[match(d$sample, ref$sample)])            # -dCt: higher = more expression
    d <- d[is.finite(d$y), , drop = FALSE]
  } else if (helper == "western") {
    loading <- as.character(loading %||% "")[1]
    if (!loading %in% p$targets) stop("Choose the loading control row, e.g. ACTB or GAPDH.")
    lc <- d[d$target == loading, ]
    d <- d[d$target != loading, , drop = FALSE]
    r <- d$value / lc$value[match(d$sample, lc$sample)]
    if (any(r <= 0, na.rm = TRUE)) stop("Band intensities must be positive.")
    d$y <- log2(r)
    d <- d[is.finite(d$y), , drop = FALSE]
  } else if (isTRUE(log)) {
    if (any(d$value <= 0)) stop("A log2 scale needs positive values.")
    d$y <- log2(d$value)
  } else d$y <- d$value
  d
}

# ---- tests --------------------------------------------------------------------------

.stars <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns"))))

# Dunnett: each group vs control, from the pooled error (mse, df); simultaneous p-values and 95% CIs.
.dunnett <- function(means, ns, ctrl, mse, df) {
  means <- stats::setNames(as.numeric(means), names(means)); ns <- stats::setNames(as.numeric(ns), names(ns))
  others <- setdiff(names(means), ctrl)
  se <- sqrt(mse * (1 / ns[others] + 1 / ns[[ctrl]]))
  t <- (means[others] - means[[ctrl]]) / se
  m <- length(others)
  R <- outer(seq_len(m), seq_len(m), Vectorize(function(i, j) if (i == j) 1 else
    sqrt(ns[[others[i]]] * ns[[others[j]]] / ((ns[[others[i]]] + ns[[ctrl]]) * (ns[[others[j]]] + ns[[ctrl]])))))
  p <- vapply(seq_len(m), function(i) if (!is.finite(t[i])) NA_real_ else
    1 - mvtnorm::pmvt(lower = rep(-abs(t[i]), m), upper = rep(abs(t[i]), m), df = as.integer(df), corr = R,
                      algorithm = mvtnorm::GenzBretz(abseps = 1e-6))[1], 0)
  q <- if (m == 1) stats::qt(0.975, df) else mvtnorm::qmvt(0.95, tail = "both.tails", df = as.integer(df), corr = R)$quantile
  data.frame(a = others, b = ctrl, diff = unname(means[others] - means[[ctrl]]), lo = unname(means[others] - means[[ctrl]] - q * se),
             hi = unname(means[others] - means[[ctrl]] + q * se), p = pmin(pmax(p, 0), 1), stringsAsFactors = FALSE)
}

# Dunn's test after Kruskal-Wallis (rank sums with tie correction), Bonferroni over the comparisons made.
.dunn <- function(y, g, pairs) {
  r <- rank(y); N <- length(y); ties <- table(y)
  s2 <- N * (N + 1) / 12 - sum(ties^3 - ties) / (12 * (N - 1))
  mr <- tapply(r, g, mean); n <- table(g)
  out <- do.call(rbind, lapply(pairs, function(pr) {
    z <- (mr[[pr[1]]] - mr[[pr[2]]]) / sqrt(s2 * (1 / n[[pr[1]]] + 1 / n[[pr[2]]]))
    data.frame(a = pr[1], b = pr[2], p = 2 * stats::pnorm(-abs(z)), stringsAsFactors = FALSE)
  }))
  out$p <- pmin(1, out$p * nrow(out))
  out
}

.bench_test_target <- function(d, groups, ctrl, paired, nonpar, allPairs) {
  d <- d[d$group %in% groups, , drop = FALSE]
  d$group <- factor(d$group, levels = groups)
  k <- length(groups)
  if (paired) {                                              # keep complete replicate sets only
    full <- names(which(table(d$pair) == k))
    d <- d[as.character(d$pair) %in% full, , drop = FALSE]
  }
  ns <- table(d$group)
  if (any(ns < 2)) return(list(error = "Each group needs at least 2 values."))
  means <- tapply(d$y, d$group, mean)
  pairsAll <- utils::combn(groups, 2, simplify = FALSE)
  pairsCtrl <- lapply(setdiff(groups, ctrl), function(g) c(g, ctrl))
  if (k == 2) {
    a <- setdiff(groups, ctrl)
    ya <- d$y[d$group == a]; yc <- d$y[d$group == ctrl]
    if (paired) { ya <- ya[order(d$pair[d$group == a])]; yc <- yc[order(d$pair[d$group == ctrl])] }
    if (!nonpar) {
      tt <- stats::t.test(ya, yc, paired = paired, var.equal = FALSE)
      cmp <- data.frame(a = a, b = ctrl, diff = mean(ya) - mean(yc), lo = tt$conf.int[1], hi = tt$conf.int[2], p = tt$p.value)
      name <- if (paired) "paired t-test" else "Welch's t-test"; om <- tt$p.value
    } else {
      wt <- suppressWarnings(stats::wilcox.test(ya, yc, paired = paired))
      cmp <- data.frame(a = a, b = ctrl, diff = stats::median(ya) - stats::median(yc), lo = NA_real_, hi = NA_real_, p = wt$p.value)
      name <- if (paired) "Wilcoxon signed-rank test" else "Mann–Whitney test"; om <- wt$p.value
    }
    return(list(omnibus = list(name = name, p = om), posthoc = NULL, comparisons = cmp))
  }
  if (!nonpar) {
    fit <- if (paired) stats::aov(y ~ group + factor(pair), data = d) else stats::aov(y ~ group, data = d)
    tab <- summary(fit)[[1]]
    om <- tab[1, "Pr(>F)"]; mse <- tab["Residuals", "Mean Sq"]; df <- tab["Residuals", "Df"]
    if (allPairs) {
      th <- stats::TukeyHSD(fit, "group")$group
      nm <- strsplit(rownames(th), "-", fixed = TRUE)
      cmp <- data.frame(a = vapply(nm, `[`, "", 1), b = vapply(nm, function(x) paste(x[-1], collapse = "-"), ""),
                        diff = th[, "diff"], lo = th[, "lwr"], hi = th[, "upr"], p = th[, "p adj"], stringsAsFactors = FALSE)
      post <- "Tukey's test (all pairs)"
    } else { cmp <- .dunnett(means, ns, ctrl, mse, df); post <- sprintf("Dunnett's test vs %s", ctrl) }
    return(list(omnibus = list(name = if (paired) "repeated-measures ANOVA" else "one-way ANOVA", p = om), posthoc = post, comparisons = cmp))
  }
  if (paired) {
    wide <- tapply(d$y, list(d$pair, d$group), mean)
    om <- stats::friedman.test(wide)$p.value
    prs <- if (allPairs) pairsAll else pairsCtrl
    cmp <- do.call(rbind, lapply(prs, function(pr) data.frame(a = pr[1], b = pr[2], diff = stats::median(wide[, pr[1]] - wide[, pr[2]]), lo = NA_real_, hi = NA_real_,
                                                         p = suppressWarnings(stats::wilcox.test(wide[, pr[1]], wide[, pr[2]], paired = TRUE)$p.value))))
    cmp$p <- stats::p.adjust(cmp$p, "holm")
    return(list(omnibus = list(name = "Friedman test", p = om), posthoc = "Wilcoxon signed-rank tests, Holm-adjusted", comparisons = cmp))
  }
  om <- stats::kruskal.test(y ~ group, data = d)$p.value
  prs <- if (allPairs) pairsAll else pairsCtrl
  dn <- .dunn(d$y, as.character(d$group), prs)
  med <- tapply(d$y, d$group, stats::median)
  cmp <- data.frame(a = dn$a, b = dn$b, diff = unname(med[dn$a] - med[dn$b]), lo = NA_real_, hi = NA_real_, p = dn$p)
  list(omnibus = list(name = "Kruskal–Wallis test", p = om), posthoc = if (allPairs) "Dunn's test (all pairs)" else sprintf("Dunn's test vs %s", ctrl), comparisons = cmp)
}

#' Analyse a bench table: helper conversion, the default (or chosen) test per target, figure data.
bench_analyse <- function(text, fileName = "", layout = "auto", helper = "none", refs = NULL, loading = NULL,
                          control = NULL, paired = FALSE, nonparametric = FALSE, allPairs = FALSE, log = FALSE) {
  helper <- if (helper %in% BENCH_HELPERS) helper else "none"
  p <- bench_parse(text, fileName, layout)
  if (helper %in% c("qpcr", "western") && p$layout != "grouped")
    stop("The qPCR and western helpers need a grouped table: genes or proteins in the first column, samples across.")
  d <- .bench_transform(p, helper, refs, loading, log)
  groups <- p$groups
  ctrl <- as.character(control %||% "")[1]
  if (!ctrl %in% groups) ctrl <- groups[1]
  groups <- c(ctrl, setdiff(groups, ctrl))
  paired <- isTRUE(paired); nonpar <- isTRUE(nonparametric); allPairs <- isTRUE(allPairs)
  logScale <- helper %in% c("qpcr", "western") || isTRUE(log)
  # Shown values: fold change vs the control mean on the log scale, else the values as given.
  disp <- function(dt) if (logScale) 2^(dt$y - mean(dt$y[dt$group == ctrl])) else dt$y
  targets <- unique(d$target)
  res <- lapply(targets, function(tg) {
    dt <- d[d$target == tg, , drop = FALSE]
    dt$shown <- disp(dt)
    t <- tryCatch(.bench_test_target(dt, groups, ctrl, paired, nonpar, allPairs), error = function(e) list(error = conditionMessage(e)))
    summ <- lapply(groups, function(g) { v <- dt$shown[dt$group == g]; n <- length(v)
      list(group = g, n = n, mean = if (n) mean(v) else NA, sd = if (n > 1) stats::sd(v) else NA, sem = if (n > 1) stats::sd(v) / sqrt(n) else NA) })
    c(list(target = tg, values = lapply(seq_len(nrow(dt)), function(i) list(group = dt$group[i], sample = dt$sample[i], pair = dt$pair[i], value = signif(dt$shown[i], 5))),
           summary = summ), t)
  })
  # Several targets: the main p-value of each (2-group test or omnibus) Holm-adjusted across targets.
  main <- vapply(res, function(r) r$omnibus$p %||% NA_real_, 0)
  adj <- rep(NA_real_, length(main)); ok <- is.finite(main)
  if (sum(ok) > 1) adj[ok] <- stats::p.adjust(main[ok], "holm")
  for (i in seq_along(res)) {
    res[[i]]$omnibus$pAcross <- if (sum(ok) > 1) adj[i] else NULL
    if (!is.null(res[[i]]$comparisons)) {
      cmp <- res[[i]]$comparisons
      cmp$stars <- .stars(cmp$p)
      if (logScale) cmp$fold <- 2^cmp$diff
      res[[i]]$comparisons <- lapply(seq_len(nrow(cmp)), function(j) as.list(cmp[j, , drop = FALSE]))
    }
  }
  k <- length(groups)
  first <- Filter(function(r) !is.null(r$omnibus$name), res)
  testName <- if (length(first)) first[[1]]$omnibus$name else ""
  post <- if (length(first)) first[[1]]$posthoc else NULL
  sentence <- sprintf("%d groups%s: %s%s%s.", k, if (paired) ", paired by replicate" else "", testName,
                      if (!is.null(post)) paste0(", then ", post) else "",
                      if (length(targets) > 1) "; p-values Holm-adjusted across targets" else "")
  helperText <- switch(helper,
    qpcr = sprintf("Ct values were normalised to %s (ΔCt) and expressed relative to %s (2^−ΔΔCt); statistics were done on ΔCt.", paste(unlist(refs), collapse = " and "), ctrl),
    western = sprintf("Band intensities were normalised to %s and expressed relative to %s; statistics were done on log2 ratios.", loading, ctrl),
    facs = "Cell-cycle phase percentages were compared for each phase.",
    if (isTRUE(log)) "Values were log2-transformed for testing." else "")
  nRep <- range(vapply(res, function(r) min(vapply(r$summary, `[[`, 0, "n")), 0))
  methods <- trimws(paste(helperText, sprintf("Groups were compared by %s%s%s (n = %s per group).", testName,
                      if (!is.null(post)) paste0(" followed by ", post) else "", if (length(targets) > 1) ", with Holm adjustment across targets" else "",
                      if (nRep[1] == nRep[2]) nRep[1] else paste(nRep, collapse = "–"))))
  warn <- character(0)
  if (nonpar && any(unlist(lapply(res, function(r) vapply(r$summary, `[[`, 0, "n"))) <= 3))
    warn <- c(warn, "With 3 values per group a non-parametric test cannot reach p < 0.05 (the smallest possible two-sided p is 0.10); the t-test or ANOVA is more useful here.")
  list(layout = p$layout, helper = helper, groups = as.list(groups), control = ctrl, paired = paired, nonparametric = nonpar, allPairs = allPairs,
       logScale = logScale, shownAs = if (logScale) "fold change vs control" else "value", targets = res,
       sentence = sentence, methods = methods, warnings = as.list(warn),
       rowNames = as.list(p$targets))
}
