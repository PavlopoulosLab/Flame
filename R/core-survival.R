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
  list(samples = trimws(as.character(df[[1]])), time = time, event = event, strata = strata,
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
