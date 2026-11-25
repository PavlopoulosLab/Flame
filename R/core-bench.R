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
BENCH_MAX_GROUPS <- 24
BENCH_MAX_REPLICATES <- 100   # values per group and target: bench experiments are small, and paired tests grow fast with it

.bench_group_of <- function(name) {
  # "Control 1", "Control_2", "Control rep3", "Control (4)", "Control.1" -> "Control" (a tag needs a separator before it)
  name <- substr(name, 1, 100)   # long names would make the pattern below slow (it backtracks over separators)
  g <- trimws(sub("(?:[ _.#-]+(?:rep|r|n|sample))?[ _.#-]*\\(?[0-9]+\\)?$", "", name, ignore.case = TRUE, perl = TRUE))
  ifelse(nzchar(g), g, name)
}

#' First step after an upload: the table's layout, its sample columns and a guess of their groups
#' from the column names ("" for every sample when the names give no usable groups, e.g. S1..S6),
#' and the row names (genes / targets). No statistics.
bench_samples <- function(text, fileName = "", layout = "auto") {
  df <- parse_delimited_table(text, fileName)
  if (ncol(df) < 2) stop("Paste a table with at least two columns: groups side by side.")
  if (ncol(df) > BENCH_MAX_GROUPS * BENCH_MAX_REPLICATES + 1) stop("The table has too many columns for bench data.")
  names(df) <- substr(names(df), 1, 100)
  num1 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[1]]), fixed = TRUE)))
  firstText <- mean(is.na(num1) & nzchar(trimws(as.character(df[[1]])))) > 0.5
  layout <- if (identical(layout, "column") || identical(layout, "grouped")) layout else if (firstText) "grouped" else "column"
  if (layout == "column") {
    g <- trimws(names(df))
    return(list(layout = layout, samples = as.list(g), guess = as.list(g), rowNames = list(), maxGroups = BENCH_MAX_GROUPS))
  }
  cols <- make.unique(names(df)[-1])
  usable <- function(g) { k <- length(unique(g)); k >= 2 && k <= BENCH_MAX_GROUPS && k < length(g) }   # some group has 2+ samples
  guess <- .bench_group_of(cols)                                             # "Control 1" -> Control
  if (!usable(guess)) {                                                      # "WT_a", "WT-b" -> WT
    byLetter <- trimws(sub("[ _.-]+[A-Za-z]$", "", substr(cols, 1, 100)))
    guess <- if (usable(byLetter)) byLetter else rep("", length(cols))
  }
  list(layout = layout, samples = as.list(cols), guess = as.list(guess),
       rowNames = as.list(make.unique(trimws(as.character(df[[1]])))), maxGroups = BENCH_MAX_GROUPS)
}

#' Parse a pasted bench table into long form: target, group, sample, pair (replicate key), value.
#' sampleGroups (grouped tables): list(<group name> = c(<sample columns>), ...) set by the user;
#' it replaces the groups guessed from the column names, and columns in no group are left out.
bench_parse <- function(text, fileName = "", layout = "auto", sampleGroups = NULL) {
  df <- parse_delimited_table(text, fileName)
  if (ncol(df) < 2) stop("Paste a table with at least two columns: groups side by side.")
  if (ncol(df) > BENCH_MAX_GROUPS * BENCH_MAX_REPLICATES + 1) stop("The table has too many columns for bench data.")
  names(df) <- substr(names(df), 1, 100)
  num <- lapply(df, function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE))))
  firstText <- mean(is.na(num[[1]]) & nzchar(trimws(as.character(df[[1]])))) > 0.5
  layout <- if (identical(layout, "column") || identical(layout, "grouped")) layout else if (firstText) "grouped" else "column"
  if (layout == "grouped") {
    if (!firstText) stop("A grouped table needs gene or target names in its first column.")
    targets <- make.unique(trimws(as.character(df[[1]])))
    cols <- make.unique(names(df)[-1])
    groups <- .bench_group_of(cols)
    if (length(sampleGroups)) groups <- .bench_user_groups(sampleGroups, cols)
    pos <- stats::ave(seq_along(groups), groups, FUN = seq_along)
    long <- do.call(rbind, lapply(seq_along(cols), function(j) data.frame(
      target = targets, group = groups[j], sample = cols[j], pair = pos[j], value = num[[j + 1]], stringsAsFactors = FALSE)))
  } else {
    if (nrow(df) > BENCH_MAX_REPLICATES)
      stop(sprintf("At most %d values per group are supported: this module is meant for bench experiments.", BENCH_MAX_REPLICATES))
    groups <- trimws(names(df))
    long <- do.call(rbind, lapply(seq_along(groups), function(j) data.frame(
      target = "value", group = groups[j], sample = paste(groups[j], seq_len(nrow(df))), pair = seq_len(nrow(df)),
      value = num[[j]], stringsAsFactors = FALSE)))
  }
  samples <- if (layout == "grouped") cols else character(0)
  sampleGroup <- if (layout == "grouped") groups else character(0)
  long <- long[is.finite(long$value) & !is.na(long$group), , drop = FALSE]
  if (!nrow(long)) stop("No numbers were found in the table.")
  grp <- if (length(sampleGroups) && layout == "grouped") intersect(substr(trimws(names(sampleGroups)), 1, 60), unique(long$group)) else unique(long$group)
  # Grouped table whose column names give no usable groups (e.g. S1..S6): hand back the samples so
  # the user can group them, instead of failing.
  if (layout == "grouped" && !length(sampleGroups) && (length(grp) < 2 || length(grp) > BENCH_MAX_GROUPS) && length(unique(long$target)) <= 60)
    return(list(layout = layout, needsGroups = TRUE, data = long, groups = character(0), targets = unique(long$target),
                samples = samples, sampleGroup = rep(NA_character_, length(samples))))
  if (length(grp) < 2) stop("At least two groups are needed (e.g. Control and Treated columns).")
  if (length(grp) > BENCH_MAX_GROUPS) stop(sprintf("At most %d groups are supported.", BENCH_MAX_GROUPS))
  if (length(unique(long$target)) > 60) stop("At most 60 genes or targets are supported.")
  if (max(table(long$group, long$target)) > BENCH_MAX_REPLICATES)
    stop(sprintf("At most %d values per group are supported: this module is meant for bench experiments.", BENCH_MAX_REPLICATES))
  list(layout = layout, data = long, groups = grp, targets = unique(long$target), samples = samples, sampleGroup = sampleGroup)
}

# Group of each sample column from the user's list(<group> = c(<samples>)); NA = not used.
.bench_user_groups <- function(sampleGroups, cols) {
  if (!is.list(sampleGroups) || is.null(names(sampleGroups))) stop("Sample groups must be given as { group name: [samples] }.")
  nm <- substr(trimws(names(sampleGroups)), 1, 60)
  if (any(!nzchar(nm))) stop("Give every sample group a name.")
  if (anyDuplicated(nm)) stop("Give each sample group a different name.")
  if (length(nm) > BENCH_MAX_GROUPS) stop(sprintf("At most %d groups are supported.", BENCH_MAX_GROUPS))
  out <- rep(NA_character_, length(cols))
  for (i in seq_along(nm)) {
    ss <- as.character(unlist(sampleGroups[[i]]))
    bad <- setdiff(ss, cols)
    if (length(bad)) stop("Not a column of the table: ", paste(utils::head(bad, 3), collapse = ", "), ".")
    if (any(!is.na(out[match(ss, cols)]))) stop("A sample can be in only one group.")
    out[match(ss, cols)] <- nm[i]
  }
  out
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

#' Analyse a bench table: helper conversion, the default (or chosen) test per target, figure data.
bench_analyse <- function(text, fileName = "", layout = "auto", helper = "none", refs = NULL, loading = NULL,
                          control = NULL, paired = FALSE, nonparametric = FALSE, allPairs = FALSE, log = FALSE,
                          sampleGroups = NULL, test = "auto", posthoc = "auto", across = "holm", hypothetical = NULL) {
  helper <- if (helper %in% BENCH_HELPERS) helper else "none"
  p <- bench_parse(text, fileName, layout, sampleGroups)
  if (isTRUE(p$needsGroups)) {
    return(list(layout = p$layout, helper = helper, groups = list(), control = "", paired = isTRUE(paired), nonparametric = isTRUE(nonparametric),
                allPairs = isTRUE(allPairs), logScale = helper %in% c("qpcr", "western"), shownAs = "", targets = list(),
                sentence = "", methods = "", warnings = list(), rowNames = as.list(p$targets),
                samples = as.list(p$samples), sampleGroups = as.list(rep("", length(p$samples))), customGroups = FALSE, needsGroups = TRUE))
  }
  if (helper %in% c("qpcr", "western") && p$layout != "grouped")
    stop("The qPCR and western helpers need a grouped table: genes or proteins in the first column, samples across.")
  d <- .bench_transform(p, helper, refs, loading, log)
  groups <- p$groups
  ctrl <- as.character(control %||% "")[1]
  if (!ctrl %in% groups) ctrl <- groups[1]
  groups <- c(ctrl, setdiff(groups, ctrl))
  paired <- isTRUE(paired); nonpar <- isTRUE(nonparametric); allPairs <- isTRUE(allPairs)
  logScale <- helper %in% c("qpcr", "western") || isTRUE(log)
  ch <- .bench_choose(test, posthoc, length(groups), paired, nonpar, allPairs)
  paired <- ch$paired; allPairs <- ch$allPairs
  across <- if (as.character(across %||% "holm")[1] %in% c("holm", "bh", "none")) as.character(across %||% "holm")[1] else "holm"
  # One-sample tests: the hypothetical value in shown units (fold change / value); tested on the analysis scale.
  hypoShown <- suppressWarnings(as.numeric(hypothetical %||% NA))[1]
  if (!is.finite(hypoShown)) hypoShown <- if (logScale) 1 else 0
  if (logScale && hypoShown <= 0) stop("On a log2 scale the hypothetical value must be positive (e.g. 1 for no change).")
  hypo <- if (logScale) log2(hypoShown) else hypoShown
  # Shown values: fold change vs the control mean on the log scale, else the values as given.
  disp <- function(dt) if (logScale) 2^(dt$y - mean(dt$y[dt$group == ctrl])) else dt$y
  targets <- unique(d$target)
  two <- if (ch$test == "twoway") .bench_twoway(d, groups, ctrl, ch) else NULL
  # On the log scale a one-sample test of fold changes vs 1 needs the control at the same reference:
  # fold changes are shown relative to the control mean, so the value is compared on that scale too.
  res <- lapply(targets, function(tg) {
    dt <- d[d$target == tg, , drop = FALSE]
    dt$shown <- disp(dt)
    t <- if (!is.null(two)) list(omnibus = list(name = BENCH_TESTS$many[["twoway"]], p = NA_real_), posthoc = two$posthoc,
                                 comparisons = two$per[[tg]]$comparisons, assumptions = two$per[[tg]]$assumptions)
         else tryCatch(.bench_test_target(dt, groups, ctrl, ch, hypo = if (logScale && ch$test %in% names(BENCH_TESTS$value)) hypo + mean(dt$y[dt$group == ctrl]) else hypo),
                       error = function(e) list(error = conditionMessage(e)))
    summ <- lapply(groups, function(g) { v <- dt$shown[dt$group == g]; n <- length(v)
      list(group = g, n = n, mean = if (n) mean(v) else NA, sd = if (n > 1) stats::sd(v) else NA, sem = if (n > 1) stats::sd(v) / sqrt(n) else NA) })
    c(list(target = tg, values = lapply(seq_len(nrow(dt)), function(i) list(group = dt$group[i], sample = dt$sample[i], pair = dt$pair[i], value = signif(dt$shown[i], 5))),
           summary = summ), t)
  })
  # Several targets: the main p-value of each (2-group test or omnibus) adjusted across targets (Holm or BH FDR).
  main <- vapply(res, function(r) r$omnibus$p %||% NA_real_, 0)
  adj <- rep(NA_real_, length(main)); ok <- is.finite(main)
  doAcross <- sum(ok) > 1 && across != "none"
  if (doAcross) adj[ok] <- stats::p.adjust(main[ok], if (across == "bh") "BH" else "holm")
  acrossName <- if (across == "bh") "Benjamini–Hochberg (FDR)" else "Holm"
  for (i in seq_along(res)) {
    res[[i]]$omnibus$pAcross <- if (doAcross) adj[i] else NULL
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
  if (!nzchar(testName)) testName <- c(BENCH_TESTS$two, BENCH_TESTS$many, BENCH_TESTS$value)[[ch$test]]
  if (is.null(post) && !is.null(two)) post <- two$posthoc
  vsValue <- if (ch$test %in% names(BENCH_TESTS$value)) sprintf(" of each group vs %s", signif(hypoShown, 4)) else ""
  sentence <- sprintf("%d groups%s: %s%s%s%s.", k, if (paired) ", paired by replicate" else "", testName, vsValue,
                      if (!is.null(post)) paste0(", then ", post) else "",
                      if (doAcross) sprintf("; p-values %s-adjusted across targets", acrossName) else "")
  helperText <- switch(helper,
    qpcr = sprintf("Ct values were normalised to %s (ΔCt) and expressed relative to %s (2^−ΔΔCt); statistics were done on ΔCt.", paste(unlist(refs), collapse = " and "), ctrl),
    western = sprintf("Band intensities were normalised to %s and expressed relative to %s; statistics were done on log2 ratios.", loading, ctrl),
    facs = "Cell-cycle phase percentages were compared for each phase.",
    if (isTRUE(log)) "Values were log2-transformed for testing." else "")
  nRep <- range(vapply(res, function(r) min(vapply(r$summary, `[[`, 0, "n")), 0))
  methods <- trimws(paste(helperText, sprintf("Groups were compared by %s%s%s%s (n = %s per group).", testName, vsValue,
                      if (!is.null(post)) paste0(" followed by ", post) else "", if (doAcross) sprintf(", with %s adjustment across targets", acrossName) else "",
                      if (nRep[1] == nRep[2]) nRep[1] else paste(nRep, collapse = "–"))))
  warn <- character(0)
  if (ch$test %in% c("mann_whitney", "wilcoxon", "ks", "kruskal", "friedman", "one_sample_wilcoxon") && any(unlist(lapply(res, function(r) vapply(r$summary, `[[`, 0, "n"))) <= 3))
    warn <- c(warn, "With 3 values per group a non-parametric test cannot reach p < 0.05 (the smallest possible two-sided p is 0.10); the t-test or ANOVA is more useful here.")
  if (!is.null(two) && !logScale && length(targets) > 1)
    warn <- c(warn, "A two-way ANOVA pools the error of all rows: rows measured on different scales (e.g. different cytokines) are better compared on a log scale or one row at a time.")
  list(layout = p$layout, helper = helper, groups = as.list(groups), control = ctrl, paired = paired, nonparametric = nonpar, allPairs = allPairs,
       test = ch$test, posthoc = ch$posthoc, across = across, hypothetical = hypoShown, anova2 = if (!is.null(two)) two$table else NULL,
       logScale = logScale, shownAs = if (logScale) "fold change vs control" else "value", targets = res,
       sentence = sentence, methods = methods, warnings = as.list(warn),
       rowNames = as.list(p$targets),
       samples = as.list(p$samples), sampleGroups = as.list(ifelse(is.na(p$sampleGroup), "", p$sampleGroup)),
       customGroups = length(sampleGroups) > 0)
}
