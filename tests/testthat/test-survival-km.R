test_that("Kaplan-Meier, log-rank and hazard ratio match survival::survfit / survdiff / coxph (AML data)", {
  # survival::aml: maintained vs non-maintained chemotherapy (weeks; 0 = censored)
  t1 <- c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161); e1 <- c(1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0)
  t2 <- c(5, 5, 8, 8, 12, 16, 23, 27, 30, 33, 43, 45); e2 <- c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)
  k <- km_fit(t1, e1)
  expect_equal(k$surv[k$time == 9], 10 / 11)
  expect_equal(k$surv[k$time == 18], 10 / 11 * 9 / 10 * 7 / 8)
  expect_equal(k$median, 31)                                    # survfit: median 31 (maintained), 23 (non-maintained)
  expect_equal(km_fit(t2, e2)$median, 23)
  expect_equal(k$nrisk[k$time == 18], 8)
  lr <- logrank_test(c(t1, t2), c(e1, e2), rep(c("Maintained", "Nonmaintained"), c(11, 12)))
  expect_equal(lr$chisq, 3.396, tolerance = 1e-3)              # survdiff: Chisq = 3.4 on 1 df, p = 0.0653
  expect_equal(lr$p, 0.0653, tolerance = 1e-2)
  # hazard ratio: Breslow ties (as Flame's Cox models), against a direct maximisation of the partial likelihood
  # (coxph's default Efron ties give 2.497)
  xg <- rep(0:1, c(11, 12)); tt <- c(t1, t2); ee <- c(e1, e2)
  f <- cox_by_gene(matrix(xg, 1), tt, ee)
  pl <- function(b) sum(vapply(which(ee == 1), function(i) b * xg[i] - log(sum(exp(b * xg[tt >= tt[i]]))), 0))
  bhat <- optimize(pl, c(-5, 5), maximum = TRUE, tol = 1e-10)$maximum
  expect_equal(f$beta, bhat, tolerance = 1e-5)
  expect_equal(exp(f$beta), 2.497, tolerance = 0.02)
  # log-log CI contains the estimate
  expect_true(all(k$lower <= k$surv + 1e-12 & k$surv <= k$upper + 1e-12, na.rm = TRUE))
})

test_that("Kaplan-Meier curves by a gene's expression or a clinical column", {
  e <- example_input("survival")
  x <- parse_expression_matrix(e$text, e$fileName, NA)
  clin <- parse_survival_table(e$clinical, e$clinicalFileName)
  k <- survival_km(x, clin, gene = "cdk1")
  expect_equal(k$gene, "CDK1")
  expect_equal(vapply(k$groups, `[[`, "", "group"), c("Low", "High"))
  expect_equal(sum(vapply(k$groups, `[[`, 0, "n")), k$samples)
  expect_equal(k$hr$of, "High")
  expect_gt(k$hr$hr, 1)                                         # proliferation: high CDK1, shorter survival
  expect_equal(k$groups[[1]]$atRisk[1], k$groups[[1]]$n)
  g1 <- k$groups[[1]]
  expect_equal(sum(g1$nevent) + sum(g1$ncens), g1$n)            # every sample is an event or censored, once
  expect_equal(sum(g1$nevent), g1$events)
  t3 <- survival_km(x, clin, gene = "CDK1", split = "tertile")
  expect_lt(t3$samples, k$samples)                              # the middle third is left out
  expect_match(survival_km(x, clin, gene = "CDK1", split = "optimal")$note, "optimistic")
  st <- survival_km(x, clin, by = "column", column = "STAGE")
  expect_equal(vapply(st$groups, `[[`, "", "group"), c("I", "II", "III", "IV"))
  expect_equal(st$logrank$df, 3L)
  expect_null(st$hr)
  expect_error(survival_km(x, clin, gene = "NOTAGENE1"), "not in the expression table")
})
