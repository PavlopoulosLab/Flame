test_that("search query keeps current literal substring behavior", {
  data <- data.frame(
    Function = c("Regulation of apoptotic process", "Cell cycle checkpoint"),
    Term_ID = c("GO:0042981", "GO:0000075"),
    Term_ID_noLinks = c("GO:0042981", "GO:0000075"),
    stringsAsFactors = FALSE
  )

  result <- filterRowsBySearchQuery(data, "apoptotic", c("Function", "Term_ID", "Term_ID_noLinks"))

  expect_identical(result$Term_ID_noLinks, "GO:0042981")
})

test_that("unquoted whitespace means AND across substrings", {
  data <- data.frame(
    Function = c("Cell cycle checkpoint", "Cell migration cycle", "Cellular checkpoint"),
    Term_ID_noLinks = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  result <- filterRowsBySearchQuery(data, "cell cycle checkpoint", c("Function", "Term_ID_noLinks"))

  expect_identical(result$Term_ID_noLinks, "A")
})

test_that("quoted text means exact phrase", {
  data <- data.frame(
    Function = c("Cell cycle checkpoint", "Cycle of cell checkpoint"),
    Term_ID_noLinks = c("A", "B"),
    stringsAsFactors = FALSE
  )

  result <- filterRowsBySearchQuery(data, '"cell cycle"', c("Function", "Term_ID_noLinks"))

  expect_identical(result$Term_ID_noLinks, "A")
})

test_that("OR creates alternate AND groups", {
  data <- data.frame(
    Function = c("Apoptosis regulation", "Necrosis pathway", "Inflammation"),
    Term_ID_noLinks = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  result <- filterRowsBySearchQuery(data, "apoptosis regulation OR necrosis", c("Function"))

  expect_identical(result$Term_ID_noLinks, c("A", "B"))
})

test_that("minus and NOT exclude matching rows", {
  data <- data.frame(
    Function = c(
      "Positive regulation of apoptosis",
      "Negative regulation of apoptosis",
      "Positive regulation of necrosis"
    ),
    Term_ID_noLinks = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  minusResult <- filterRowsBySearchQuery(data, "apoptosis -negative", c("Function"))
  notResult <- filterRowsBySearchQuery(data, "apoptosis NOT negative", c("Function"))

  expect_identical(minusResult$Term_ID_noLinks, "A")
  expect_identical(notResult$Term_ID_noLinks, "A")
})

test_that("quoted exclusions are supported", {
  data <- data.frame(
    Function = c("Cell cycle", "Cell cycle checkpoint", "Cell death"),
    Term_ID_noLinks = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  result <- filterRowsBySearchQuery(data, 'cell -"cycle checkpoint"', c("Function"))

  expect_identical(result$Term_ID_noLinks, c("A", "C"))
})

test_that("matching is case-insensitive and searches term IDs", {
  data <- data.frame(
    Function = c("Alpha", "Beta"),
    Term_ID_noLinks = c("GO:0006915", "REAC:R-HSA-123"),
    stringsAsFactors = FALSE
  )

  result <- filterRowsBySearchQuery(data, "go:0006915", c("Function", "Term_ID_noLinks"))

  expect_identical(result$Function, "Alpha")
})

test_that("unmatched quotes do not error", {
  data <- data.frame(
    Function = c("Cell cycle checkpoint", "Cell death"),
    Term_ID_noLinks = c("A", "B"),
    stringsAsFactors = FALSE
  )

  expect_no_error(
    result <- filterRowsBySearchQuery(data, '"cell cycle', c("Function"))
  )
  expect_identical(result$Term_ID_noLinks, "A")
})
