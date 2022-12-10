setwd(this.path::this.dir())

source("./unit_tests_assert_function.R")
source("../global_variables.R")
source("../functions/init.R")

testsPassed <- 0
testsFailed <- 0

columnNames <- c("Taxonomy_ID", "Species_Name", "Common_Name",
                 "gprofiler_ID", "KEGG", "print_name")
arenaListNames <- c("network1", "network2", "network3")

testOrganismsDataInitialized <- function() {
  initializeOrganismsData(filePath = "../organisms_with_kegg.tsv")
  assertTrue(nrow(ORGANISMS_FROM_FILE) > 0)
  assertEquals(columnNames,
               colnames(ORGANISMS_FROM_FILE))
}

testArenaEdgelistInitialized <- function() {
  assertEquals(length(arenaEdgelist), 0)
  initializeArenaEdgelist()
  assertEquals(arenaListNames, names(arenaEdgelist))
}

cat("***** INIT unit tests *****\n\n")

testOrganismsDataInitialized()
testArenaEdgelistInitialized()

cat(sprintf("### PASS: %d - FAIL: %d ###\n", testsPassed, testsFailed))
cat(sprintf("------------------------------\n\n"))
