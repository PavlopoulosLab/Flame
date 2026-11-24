extractHelper <- function(name) {
  get(name, envir = asNamespace("flame"))
}

test_that("text mining preview selects non-ambiguous entities by default", {
  formatRows <- extractHelper("format_extract_entity_rows")
  summarizeRows <- extractHelper("summarize_extract_entity_rows")
  validateText <- extractHelper("validate_extract_text")

  raw <- data.frame(
    Name = c("TP53", "TP53", "BRCA1", "BRCA1"),
    Type = c("9606", "9606", "9606", "9606"),
    ID = c("ENSP00000269305", "ENSP00000419060", "ENSP00000350283", "ENSP00000350283"),
    stringsAsFactors = FALSE
  )

  rows <- formatRows(raw)
  stats <- summarizeRows(rows, validateText("TP53 and BRCA1"), raw)

  expect_equal(nrow(rows), 3)
  expect_equal(stats$duplicatesRemoved, 1)
  expect_equal(stats$ambiguousRows, 2)
  expect_equal(stats$selectedRows, 1)

  expect_true(all(rows$ambiguous[rows$name == "TP53"]))
  expect_false(any(rows$selected[rows$name == "TP53"]))
  expect_true(rows$selected[rows$name == "BRCA1"])
})

test_that("text mining selected IDs are resolved against server row IDs", {
  formatRows <- extractHelper("format_extract_entity_rows")
  selectedIds <- extractHelper("extract_textmining_selected_ids")

  rows <- formatRows(data.frame(
    Name = c("TP53", "BRCA1"),
    Type = c("9606", "9606"),
    ID = c("ENSP00000269305", "ENSP00000350283"),
    stringsAsFactors = FALSE
  ))

  selected <- selectedIds(rows, c(rows$rowId[1], "extract-row-does-not-exist", rows$rowId[1]))

  expect_identical(selected, "ENSP00000269305")
})

test_that("document entities get the right tagger codes and database links", {
  expect_equal(EXTRACT_KINDS$diseases$code, -26L)
  expect_equal(extract_entity_link("chemicals", "CIDs00002767"), "https://pubchem.ncbi.nlm.nih.gov/compound/2767")
  expect_equal(extract_entity_link("process", "GO:0006915"), "https://www.ebi.ac.uk/QuickGO/term/GO:0006915")
  expect_equal(extract_entity_link("genes", "ENSP00000269305"), "https://www.ensembl.org/id/ENSP00000269305")
  expect_equal(extract_entity_link("organisms", "10090"), "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=10090")
  expect_error(extract_document_entities("TP53", 9606L, character(0)), "at least one kind")
})
