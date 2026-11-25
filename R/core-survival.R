# =============================================================================
# FLAME core: survival analysis of an expression matrix (genes linked to outcome)
# =============================================================================
# A clinical table gives, per sample, a follow-up time and whether the event
# (death, progression, relapse...) happened. For every gene, a univariate Cox
# proportional-hazards model of survival on the gene's standardized expression
# gives a hazard ratio per standard deviation, its 95% CI, a Wald z and p-value
# (BH-adjusted across genes). Optionally stratified by a clinical column (e.g.
# stage or batch): each stratum keeps its own baseline hazard, which controls for
# that variable without assuming its effect.
#
# The models of all genes are fitted together (vectorised Newton-Raphson with
# Breslow ties), so 20,000 genes take seconds; the results equal survival::coxph.
# =============================================================================

SURVIVAL_EVENT_YES <- c("1", "true", "yes", "dead", "deceased", "died", "death", "event", "progression", "progressed",
                        "recurred", "recurrence", "relapse", "relapsed", "recurred/progressed", "with tumor", "dead with tumor")
SURVIVAL_EVENT_NO <- c("0", "false", "no", "alive", "living", "censored", "diseasefree", "disease free", "tumor free",
                       "alive or dead tumor free", "no event")

#' Event indicator (1 = event, 0 = censored) from 0/1, TRUE/FALSE or words such as
#' "dead"/"alive" and cBioPortal codes such as "1:DECEASED".
survival_event <- function(x) {
  v <- tolower(trimws(as.character(x)))
  v <- sub("^([01]):.*$", "\\1", v)                     # "1:DECEASED", "0:LIVING"
  out <- rep(NA_integer_, length(v))
  out[v %in% SURVIVAL_EVENT_YES] <- 1L
  out[v %in% SURVIVAL_EVENT_NO] <- 0L
  out
}

.guess_column <- function(names, patterns) {
  for (p in patterns) { hit <- grep(p, names, ignore.case = TRUE, perl = TRUE); if (length(hit)) return(names[hit[1]]) }
  NA_character_
}

#' Parse the clinical table: first column = sample ID; time and event columns chosen
#' or guessed from their names (OS_MONTHS / OS_STATUS, time / status, days / vital...).
parse_survival_table <- function(text, fileName = "", timeColumn = NULL, eventColumn = NULL, strataColumn = NULL) {
  df <- parse_delimited_table(text, fileName)
  if (ncol(df) < 3) stop("The clinical table needs a sample ID column, a time column and an event column.")
  cols <- names(df)[-1]
  timeColumn <- as.character(timeColumn %||% "")[1]
  eventColumn <- as.character(eventColumn %||% "")[1]
  if (!nzchar(timeColumn)) timeColumn <- .guess_column(cols, c("^OS[_ .]?(months|time|days)", "PFI?[_ .]?(months|time|days)", "(months|time|days|survival|follow)"))
  if (!nzchar(eventColumn)) eventColumn <- .guess_column(cols, c("^OS[_ .]?(status|event)", "PFI?[_ .]?(status|event)", "(status|event|dead|death|vital|censor)"))
  if (is.na(timeColumn) || !timeColumn %in% cols) stop("Choose the column with the follow-up time.")
  if (is.na(eventColumn) || !eventColumn %in% cols) stop("Choose the column that says whether the event happened.")
  time <- suppressWarnings(as.numeric(as.character(df[[timeColumn]])))
  event <- survival_event(df[[eventColumn]])
  strata <- NULL
  strataColumn <- as.character(strataColumn %||% "")[1]
  if (nzchar(strataColumn)) {
    if (!strataColumn %in% cols) stop("Unknown column to stratify by: ", strataColumn)
    strata <- trimws(as.character(df[[strataColumn]]))
    strata[!nzchar(strata) | toupper(strata) %in% c("NA", "[NOT AVAILABLE]", "UNKNOWN")] <- NA
  }
  list(samples = trimws(as.character(df[[1]])), time = time, event = event, strata = strata, table = df,
       columns = as.list(cols), timeColumn = timeColumn, eventColumn = eventColumn,
       strataColumn = if (nzchar(strataColumn)) strataColumn else NULL)
}

# Match clinical samples to matrix columns: exactly (ignoring case), else by the first
# 12 characters (TCGA patient barcodes, e.g. TCGA-AB-1234-01 -> TCGA-AB-1234).
.match_samples <- function(matrixSamples, clinicalSamples) {
  i <- match(toupper(matrixSamples), toupper(clinicalSamples))
  if (sum(!is.na(i)) < length(matrixSamples) / 2 && all(nchar(clinicalSamples) >= 12)) {
    j <- match(toupper(substr(matrixSamples, 1, 12)), toupper(substr(clinicalSamples, 1, 12)))
    if (sum(!is.na(j)) > sum(!is.na(i))) i <- j
  }
  i
}

#' Univariate Cox models of every gene (rows of X: genes, columns: samples), fitted together.
#' Breslow ties; optional strata. Returns beta (per unit of X), se, z, p.
cox_by_gene <- function(X, time, event, strata = NULL, maxIter = 25L) {
  n <- ncol(X)
  if (is.null(strata)) strata <- rep("all", n)
  o <- order(strata, time)                                  # within each stratum, by time
  X <- X[, o, drop = FALSE]; time <- time[o]; event <- event[o]; strata <- strata[o]
  M <- t(X)                                                 # samples x genes
  G <- ncol(M)
  # For each sample, the first sample of its risk set: same stratum, time >= its time (ties share the set).
  key <- paste(strata, time, sep = "\r")
  first <- match(key, key)                                  # first index with the same stratum and time (rows are sorted)
  runs <- rle(as.character(strata))
  blockEnd <- rep(cumsum(runs$lengths), runs$lengths)       # last index of each sample's stratum
  ev <- which(event == 1L)
  # Reverse cumulative sums within strata: R[i, ] = sum over j in [i, end of stratum],
  # from the reverse cumulative sum over all rows minus what lies beyond the stratum.
  after <- blockEnd + 1L
  revcum <- function(A) {
    C <- apply(A[n:1, , drop = FALSE], 2, cumsum)
    C <- rbind(C[n:1, , drop = FALSE], 0)
    C[seq_len(n), , drop = FALSE] - C[after, , drop = FALSE]
  }
  beta <- rep(0, G)
  for (it in seq_len(maxIter)) {
    eta <- sweep(M, 2, beta, `*`)
    eta <- sweep(eta, 2, apply(eta, 2, max), `-`)            # numerical safety; cancels in the ratios
    w <- exp(eta)
    S0 <- revcum(w); S1 <- revcum(w * M); S2 <- revcum(w * M * M)
    f <- first[ev]
    m1 <- S1[f, , drop = FALSE] / S0[f, , drop = FALSE]
    U <- colSums(M[ev, , drop = FALSE] - m1)
    I <- colSums(S2[f, , drop = FALSE] / S0[f, , drop = FALSE] - m1^2)
    step <- U / I
    step[!is.finite(step)] <- 0
    step <- pmax(pmin(step, 2), -2)                         # damped, as coxph does near separation
    beta <- beta + step
    if (max(abs(step)) < 1e-9) break
  }
  se <- 1 / sqrt(I)
  z <- beta / se
  bad <- !is.finite(z) | abs(beta) > 15                     # no variation, or (quasi-)separation
  beta[bad] <- NA; se[bad] <- NA; z[bad] <- NA
  data.frame(beta = beta, se = se, z = z, p = 2 * stats::pnorm(-abs(z)))
}

#' Genes whose expression is linked to survival (univariate Cox per gene, expression
#' standardized so the hazard ratio is per standard deviation).
expression_survival <- function(x, clin, minEvents = 5L) {
  m <- x$matrix
  i <- .match_samples(colnames(m), clin$samples)
  keep <- !is.na(i)
  time <- clin$time[i[keep]]; event <- clin$event[i[keep]]
  strata <- if (!is.null(clin$strata)) clin$strata[i[keep]] else NULL
  ok <- is.finite(time) & time > 0 & !is.na(event)
  if (!is.null(strata)) ok <- ok & !is.na(strata)
  samples <- colnames(m)[keep][ok]
  time <- time[ok]; event <- event[ok]; if (!is.null(strata)) strata <- strata[ok]
  if (!is.null(strata) && length(unique(strata)) > 50)
    stop(sprintf("The strata column has %d different values; choose a column with at most 50 (e.g. stage or batch).", length(unique(strata))))
  nMatched <- sum(keep)
  if (nMatched == 0) stop("No sample of the clinical table matches a column of the expression table. The first column of the clinical table must hold the sample names.")
  if (length(samples) < 10) stop(sprintf("Only %d samples have a usable time and event; at least 10 are needed.", length(samples)))
  if (sum(event) < minEvents) stop(sprintf("Only %d events (e.g. deaths) among the %d samples; at least %d are needed.", sum(event), length(samples), minEvents))
  X <- m[, samples, drop = FALSE]
  sdv <- .row_sd(X)
  X <- X[sdv > 0, , drop = FALSE]
  Z <- (X - rowMeans(X)) / .row_sd(X)                       # per-gene z-scores: HR per standard deviation
  # Fit in chunks of genes, so a large cohort stays within a few hundred MB of memory.
  chunk <- max(200L, floor(2e6 / ncol(Z)))
  fit <- do.call(rbind, lapply(split(seq_len(nrow(Z)), ceiling(seq_len(nrow(Z)) / chunk)),
                               function(rows) cox_by_gene(Z[rows, , drop = FALSE], time, event, strata)))
  q <- rep(NA_real_, nrow(fit)); okp <- !is.na(fit$p); q[okp] <- stats::p.adjust(fit$p[okp], "BH")
  genes <- rownames(Z)
  o <- order(fit$p, na.last = NA)
  r <- function(v, d = 4) signif(v, d)
  list(
    genes = lapply(o, function(k) list(gene = genes[k], z = round(fit$z[k], 3), hr = r(exp(fit$beta[k])),
                                       lo = r(exp(fit$beta[k] - 1.959964 * fit$se[k])), hi = r(exp(fit$beta[k] + 1.959964 * fit$se[k])),
                                       p = r(fit$p[k]), q = r(q[k]))),
    samples = length(samples), events = sum(event), unmatched = length(colnames(m)) - nMatched,
    excluded = nMatched - length(samples), medianTime = stats::median(time), strata = if (!is.null(strata)) length(unique(strata)) else 0L,
    tested = sum(okp), significant = sum(q <= 0.05, na.rm = TRUE),
    columns = clin$columns, timeColumn = clin$timeColumn, eventColumn = clin$eventColumn, strataColumn = clin$strataColumn,
    note = "Univariate Cox model per gene (expression standardized: hazard ratio per standard deviation), Wald test, Benjamini-Hochberg q-values."
  )
}

#' Columns of a clinical table and the guessed time / event columns (for the dialog's menus).
survival_table_columns <- function(text, fileName = "") {
  df <- parse_delimited_table(text, fileName)
  cols <- names(df)[-1]
  list(columns = as.list(cols), samples = nrow(df),
       timeColumn = .guess_column(cols, c("^OS[_ .]?(months|time|days)", "PFI?[_ .]?(months|time|days)", "(months|time|days|survival|follow)")),
       eventColumn = .guess_column(cols, c("^OS[_ .]?(status|event)", "PFI?[_ .]?(status|event)", "(status|event|dead|death|vital|censor)")))
}

# ---- Kaplan-Meier curves --------------------------------------------------------------------------
# Survival curves of groups of samples: split by a gene's expression (high vs low at the median, the top vs
# bottom third or quarter, a chosen percentile, or the cut-off with the largest log-rank statistic) or by a
# clinical column (e.g. stage). Kaplan-Meier estimates with log-log 95% CIs (Greenwood), censoring marks,
# numbers at risk, median survival, the log-rank test and, for two groups, the Cox hazard ratio (high vs low).
# Base R only; the results equal survival::survfit / survdiff / coxph.

#' Kaplan-Meier estimate of one group: a step at every distinct time.
km_fit <- function(time, event) {
  o <- order(time); time <- time[o]; event <- event[o]
  ut <- unique(time)
  d <- as.numeric(tapply(event, factor(time, levels = ut), sum))
  cens <- as.numeric(tapply(1 - event, factor(time, levels = ut), sum))
  nrisk <- length(time) - c(0, cumsum((d + cens)[-length(ut)]))
  s <- cumprod(1 - d / nrisk)
  gw <- cumsum(ifelse(nrisk > d, d / (nrisk * (nrisk - d)), Inf))  # Greenwood
  se <- sqrt(gw) / abs(log(s))                                       # log-log scale
  lo <- s^exp(1.959964 * se); hi <- s^exp(-1.959964 * se)
  lo[!is.finite(lo) | s <= 0] <- NA; hi[!is.finite(hi) | s <= 0] <- NA
  lo[s == 1] <- 1; hi[s == 1] <- 1
  med <- if (any(s <= 0.5)) ut[which(s <= 0.5)[1]] else NA_real_
  list(time = ut, surv = s, lower = lo, upper = hi, nrisk = nrisk, nevent = d, ncens = cens, median = med)
}

#' Log-rank test of k groups: chi-square with k - 1 degrees of freedom.
logrank_test <- function(time, event, group) {
  g <- factor(group); k <- nlevels(g)
  if (k < 2) return(list(chisq = NA_real_, df = 0L, p = NA_real_, observed = NA, expected = NA))
  et <- sort(unique(time[event == 1]))
  O <- E <- numeric(k); V <- matrix(0, k, k)
  for (t in et) {
    atRisk <- time >= t
    n_j <- tabulate(as.integer(g)[atRisk], k); d_j <- tabulate(as.integer(g)[time == t & event == 1], k)
    n <- sum(n_j); d <- sum(d_j)
    O <- O + d_j; E <- E + d * n_j / n
    if (n > 1) V <- V + d * (n - d) / (n - 1) * (diag(n_j / n) - outer(n_j, n_j) / n^2)
  }
  u <- (O - E)[-k]; Vs <- V[-k, -k, drop = FALSE]
  chisq <- tryCatch(as.numeric(t(u) %*% solve(Vs, u)), error = function(e) NA_real_)
  list(chisq = chisq, df = k - 1L, p = stats::pchisq(chisq, k - 1L, lower.tail = FALSE),
       observed = stats::setNames(as.list(O), levels(g)), expected = stats::setNames(as.list(E), levels(g)))
}

KM_SPLITS <- c(median = "median", tertile = "top vs bottom third", quartile = "top vs bottom quarter", percentile = "a percentile", optimal = "the best cut-off")

#' Kaplan-Meier curves for a gene's expression groups or a clinical column.
#' split: median | tertile | quartile | percentile (with `percent`, the share of samples called high) | optimal.
survival_km <- function(x, clin, gene = NULL, by = c("gene", "column"), split = "median", percent = 50, column = NULL) {
  by <- match.arg(by)
  m <- x$matrix
  i <- .match_samples(colnames(m), clin$samples)
  keep <- !is.na(i)
  if (!any(keep)) stop("No sample of the clinical table matches a column of the expression table. The first column of the clinical table must hold the sample names.")
  samples <- colnames(m)[keep]
  time <- clin$time[i[keep]]; event <- clin$event[i[keep]]
  ok <- is.finite(time) & time >= 0 & !is.na(event)
  note <- NULL; cut <- NULL; geneName <- NULL; levelsOrder <- NULL
  if (by == "gene") {
    gene <- trimws(as.character(gene %||% ""))[1]
    if (!nzchar(gene)) stop("Type a gene to split the samples by its expression.")
    r <- match(toupper(gene), toupper(rownames(m)))
    if (is.na(r)) {
      near <- head(rownames(m)[startsWith(toupper(rownames(m)), toupper(substr(gene, 1, 3)))], 5)
      stop(sprintf("%s is not in the expression table.%s", gene, if (length(near)) paste0(" Similar: ", paste(near, collapse = ", "), ".") else ""))
    }
    geneName <- rownames(m)[r]
    v <- as.numeric(m[r, samples])
    ok <- ok & is.finite(v)
    v <- v[ok]
    split <- if (split %in% names(KM_SPLITS)) split else "median"
    q <- function(p) stats::quantile(v, p, names = FALSE, type = 7)
    grp <- switch(split,
      median = ifelse(v > q(0.5), "High", "Low"),
      tertile = ifelse(v >= q(2 / 3), "High", ifelse(v <= q(1 / 3), "Low", NA)),
      quartile = ifelse(v >= q(0.75), "High", ifelse(v <= q(0.25), "Low", NA)),
      percentile = { p <- min(max(as.numeric(percent %||% 50), 5), 95) / 100; ifelse(v > q(1 - p), "High", "Low") },
      optimal = {
        # Maximally selected log-rank statistic over cut-offs leaving at least 20% of the samples on each side.
        cand <- unique(stats::quantile(v, seq(0.2, 0.8, by = 0.01), names = FALSE, type = 7))
        stat <- vapply(cand, function(cv) { g <- ifelse(v > cv, "High", "Low"); if (length(unique(g)) < 2) NA_real_ else logrank_test(time[ok], event[ok], g)$chisq }, 0)
        best <- cand[which.max(stat)]
        note <- "The best cut-off was chosen to give the smallest p-value, so the p-value is optimistic (not corrected for trying many cut-offs)."
        ifelse(v > best, "High", "Low")
      })
    cut <- switch(split, median = q(0.5), percentile = q(1 - min(max(as.numeric(percent %||% 50), 5), 95) / 100),
                  optimal = best, NULL)
    levelsOrder <- c("Low", "High")
  } else {
    column <- as.character(column %||% "")[1]
    if (!nzchar(column)) stop("Choose the clinical column to group the samples by.")
    df <- clin$table
    if (!column %in% names(df)[-1]) stop("Unknown clinical column: ", column)
    grp <- trimws(as.character(df[[column]]))[i[keep]]
    grp[!nzchar(grp) | toupper(grp) %in% c("NA", "[NOT AVAILABLE]", "[UNKNOWN]", "UNKNOWN", "[DISCREPANCY]", "[NOT EVALUATED]")] <- NA
    grp <- grp[ok]
    if (length(unique(stats::na.omit(grp))) > 12) stop(sprintf("%s has %d different values; choose a column with at most 12 groups (e.g. stage or sex).", column, length(unique(stats::na.omit(grp)))))
    levelsOrder <- sort(unique(stats::na.omit(grp)))
  }
  time <- time[ok]; event <- event[ok]; samples <- samples[ok]
  use <- !is.na(grp)
  time <- time[use]; event <- event[use]; grp <- grp[use]; samples <- samples[use]
  levelsOrder <- levelsOrder[levelsOrder %in% grp]
  if (length(levelsOrder) < 2) stop("The samples fall into fewer than two groups.")
  if (length(time) < 6) stop(sprintf("Only %d samples have a usable time and event.", length(time)))
  lr <- logrank_test(time, event, factor(grp, levels = levelsOrder))
  hr <- NULL
  if (length(levelsOrder) == 2) {
    ind <- matrix(as.numeric(grp == levelsOrder[2]), 1)
    f <- cox_by_gene(ind, time, event)
    if (is.finite(f$beta)) hr <- list(hr = signif(exp(f$beta), 4), lo = signif(exp(f$beta - 1.959964 * f$se), 4), hi = signif(exp(f$beta + 1.959964 * f$se), 4),
                                      p = signif(f$p, 4), of = levelsOrder[2], vs = levelsOrder[1])
  }
  ticks <- pretty(c(0, max(time)), n = 6); ticks <- ticks[ticks <= max(time)]
  r <- function(v, d = 4) signif(v, d)
  groups <- lapply(levelsOrder, function(g) {
    s <- grp == g
    k <- km_fit(time[s], event[s])
    cens <- k$time[k$ncens > 0]
    list(group = g, n = sum(s), events = sum(event[s]), median = r(k$median),
         time = r(k$time), surv = r(k$surv), lower = r(k$lower), upper = r(k$upper),
         censorTime = r(cens), censorSurv = r(k$surv[k$ncens > 0]),
         atRisk = vapply(ticks, function(t) sum(time[s] >= t), 0L))
  })
  list(by = by, gene = geneName, column = if (by == "column") column else NULL, split = if (by == "gene") split else NULL,
       splitLabel = if (by == "gene") KM_SPLITS[[split]] else NULL, cut = if (is.null(cut)) NULL else r(cut),
       samples = length(time), events = sum(event), groups = groups, ticks = ticks,
       logrank = list(chisq = r(lr$chisq), df = lr$df, p = r(lr$p)), hr = hr, note = note,
       timeColumn = clin$timeColumn, eventColumn = clin$eventColumn)
}
