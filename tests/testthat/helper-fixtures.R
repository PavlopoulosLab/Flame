# Shared test fixtures (testthat loads helper-*.R before the tests).

fake_results <- function(tool = "A") {
  data.frame(
    Source = c("GO:BP", "GO:BP", "KEGG", "REAC"),
    Term_ID = c("GO:1", "GO:2", "KEGG:3", "R-HSA-4"),
    Function = c("cell cycle", "mitotic cell cycle", "apoptosis", "DNA repair"),
    `P-value` = c(1e-8, 1e-5, 1e-3, 0.02),
    `-log10Pvalue` = -log10(c(1e-8, 1e-5, 1e-3, 0.02)),
    `Term Size` = c(100, 50, 80, 40),
    `Query size` = 20,
    `Intersection Size` = c(6, 4, 3, 2),
    `Gene Ratio` = c(6, 4, 3, 2) / 20,
    `Term Coverage %` = c(6, 8, 3.75, 5),
    `Fold Enrichment` = c(3, 4, 2, 2.5),
    `Positive Hits` = c("A,B,C,D,E,F", "A,B,C,G", "D,E,H", "A,H"),
    Term_ID_noLinks = c("GO:1", "GO:2", "KEGG:3", "R-HSA-4"),
    link = c("https://x/GO:1", NA, "javascript:alert(1)", NA),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
