setwd(this.path::this.dir())

source("./unit_tests_assert_function.R")
source("../global_variables.R")
source("../functions/init.R")
source("../functions/render.R")
source("../functions/interoperability.R")

testsPassed <- 0
testsFailed <- 0

currentArenaEdgelist_FvG <- readRDS("RDS/currentArenaEdgelist_FvG.RDS")
jsonBody_FvG <- readRDS("RDS/jsonBody_FvG.RDS")
currentArenaEdgelist_FvF <- readRDS("RDS/currentArenaEdgelist_FvF.RDS")
jsonBody_FvF <- readRDS("RDS/jsonBody_FvF.RDS")
currentArenaEdgelist_GvG <- readRDS("RDS/currentArenaEdgelist_GvG.RDS")
jsonBody_GvG <- readRDS("RDS/jsonBody_GvG.RDS")

testNetworkExists <- function() {
  assertTrue(existsNetwork(currentArenaEdgelist_FvG))
  initializeArenaEdgelist()
  assertFalse(existsNetwork(arenaEdgelist[["network1"]]))
}

testArenaJSONBodyConstructed <- function() {
  assertEquals(jsonBody_FvG,
               constructArenaJSONBody(currentArenaEdgelist_FvG))
  assertEquals(jsonBody_FvF,
               constructArenaJSONBody(currentArenaEdgelist_FvF))
  assertEquals(jsonBody_GvG,
               constructArenaJSONBody(currentArenaEdgelist_GvG))
}

cat("***** INTEROPERABILITY unit tests *****\n\n")

testNetworkExists()
testArenaJSONBodyConstructed()

cat(sprintf("### PASS: %d - FAIL: %d ###\n", testsPassed, testsFailed))
cat(sprintf("------------------------------\n\n"))
