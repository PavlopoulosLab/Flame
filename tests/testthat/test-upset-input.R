# Pure UpSet combination logic (R/core-inputs.R)

findUpsetCombination <- function(combinations, sourceNames) {
  match <- Filter(function(item) setequal(item$sourceNames, sourceNames), combinations)
  if (length(match) == 0) NULL else match[[1]]
}

test_that("upset combinations support intersection, distinct, and union modes", {
  sets <- list(
    A = c("a", "b", "c"),
    B = c("b", "c", "d"),
    C = c("c", "e")
  )

  intersection <- flameComputeUpsetCombinations(sets, UpsetMode$INTERSECTION)
  expect_setequal(findUpsetCombination(intersection, c("A", "B"))$ids, c("b", "c"))
  expect_setequal(findUpsetCombination(intersection, c("A", "B", "C"))$ids, "c")

  distinct <- flameComputeUpsetCombinations(sets, UpsetMode$DISTINCT)
  expect_setequal(findUpsetCombination(distinct, c("A"))$ids, "a")
  expect_setequal(findUpsetCombination(distinct, c("A", "B"))$ids, "b")
  expect_setequal(findUpsetCombination(distinct, c("A", "B", "C"))$ids, "c")
  expect_null(findUpsetCombination(distinct, c("A", "C")))

  union <- flameComputeUpsetCombinations(sets, UpsetMode$UNION)
  expect_setequal(findUpsetCombination(union, c("A", "B"))$ids, c("a", "b", "c", "d"))
  expect_setequal(findUpsetCombination(union, c("A", "C"))$ids, c("a", "b", "c", "e"))
})
