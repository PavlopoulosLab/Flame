setwd(this.path::this.dir())

source("./unit_tests_assert_function.R")
source("../global_variables.R")
source("../functions/init.R")
source("../functions/links.R")

testsPassed <- 0
testsFailed <- 0

organism <- "hsapiens"
functionalEnrichmentResult <- 
  readRDS("RDS/links_before_agotool_functionalEnrichmentResult.RDS")
agotoolTransformedResult_links <- 
  readRDS("RDS/links_agotool_functionalEnrichmentResult.RDS")
gprofilerTransformedResult_links <-
  readRDS("RDS/links_gprofiler_functionalEnrichmentResult.RDS")

testDBLinksAttached <- function() {
  initializeOrganismsData(filePath = "../organisms_with_kegg.tsv")
  attachDBLinks(organism)
  assertEquals(functionalEnrichmentResult,
               agotoolTransformedResult_links)
  functionalEnrichmentResult <<- 
    readRDS("RDS/links_before_gprofiler_functionalEnrichmentResult.RDS")
  attachDBLinks(organism)
  assertEquals(functionalEnrichmentResult,
               gprofilerTransformedResult_links)
}

cat("***** LINKS unit tests *****\n\n")

testDBLinksAttached()

cat(sprintf("### PASS: %d - FAIL: %d ###\n", testsPassed, testsFailed))
cat(sprintf("------------------------------\n\n"))
