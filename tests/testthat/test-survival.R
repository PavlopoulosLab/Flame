test_that("vectorised Cox models match survival::coxph, with and without strata", {
  skip_if_not_installed("survival")
  set.seed(7); n <- 80; G <- 25
  X <- matrix(rnorm(G * n), G, n)
  time <- round(rexp(n, exp(0.7 * X[1, ])) * 30) + 1
  event <- rbinom(n, 1, 0.75); strata <- sample(c("a", "b"), n, TRUE)
  for (st in list(NULL, strata)) {
    fit <- cox_by_gene(X, time, event, st)
    for (i in 1:5) {
      f <- if (is.null(st)) survival::coxph(survival::Surv(time, event) ~ X[i, ], ties = "breslow")
           else survival::coxph(survival::Surv(time, event) ~ X[i, ] + survival::strata(st), ties = "breslow")
      expect_equal(fit$beta[i], unname(stats::coef(f)), tolerance = 1e-6)
      expect_equal(fit$se[i], unname(sqrt(diag(stats::vcov(f)))), tolerance = 1e-6)
    }
  }
})

test_that("event codes are read from numbers, words and cBioPortal codes", {
  expect_equal(survival_event(c("1", "0", "TRUE", "dead", "Alive", "1:DECEASED", "0:LIVING", "1:PROGRESSION", "maybe")),
               c(1L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, NA))
})

test_that("the liver cancer example gives proliferation genes as poor and metabolism genes as good prognosis", {
  ex <- example_input("survival")
  x <- parse_expression_matrix(ex$text, ex$fileName)
  clin <- parse_survival_table(ex$clinical, ex$clinicalFileName)
  expect_equal(c(clin$timeColumn, clin$eventColumn), c("OS_MONTHS", "OS_STATUS"))
  r <- expression_survival(x, clin)
  expect_gt(r$events, 100)
  sig <- Filter(function(g) g$q <= 0.05, r$genes)
  poor <- vapply(Filter(function(g) g$hr > 1, sig), `[[`, "", "gene")
  good <- vapply(Filter(function(g) g$hr < 1, sig), `[[`, "", "gene")
  expect_true(all(c("CDCA8", "KPNA2") %in% poor))
  expect_true("ALDH2" %in% good)
  s <- expression_survival(x, parse_survival_table(ex$clinical, ex$clinicalFileName, strataColumn = "STAGE"))
  expect_equal(s$strata, 4L)
  expect_lt(s$significant, r$significant)
})

test_that("unusable clinical tables are refused with a clear message", {
  ex <- example_input("survival")
  x <- parse_expression_matrix(ex$text, ex$fileName)
  clin <- parse_survival_table("id\ttime\tstatus\nX1\t3\t1\nX2\t5\t0\n", "c.tsv")
  expect_error(expression_survival(x, clin), "No sample of the clinical table matches")
})
