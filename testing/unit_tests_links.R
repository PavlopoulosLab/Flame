setwd(this.path::this.dir())

source("./unit_tests_assert_function.R")
source("../global_variables.R")
source("../functions/init.R")
source("../functions/links.R")

testsPassed <- 0
testsFailed <- 0

organism <- "hsapiens"
enrichmentResults[[currentType_Tool]] <- 
  readRDS("RDS/links_before_agotool_enrichmentResults[[currentType_Tool]].RDS")
agotoolTransformedResult_links <- 
  readRDS("RDS/links_agotool_enrichmentResults[[currentType_Tool]].RDS")
gprofilerTransformedResult_links <-
  readRDS("RDS/links_gprofiler_enrichmentResults[[currentType_Tool]].RDS")

testDBLinksAttached <- function() {
  initializeOrganismsData(filePath = "../organisms_with_kegg.tsv")
  attachDBLinks(organism)
  assertEquals(enrichmentResults[[currentType_Tool]],
               agotoolTransformedResult_links)
  enrichmentResults[[currentType_Tool]] <<- 
    readRDS("RDS/links_before_gprofiler_enrichmentResults[[currentType_Tool]].RDS")
  attachDBLinks(organism)
  assertEquals(enrichmentResults[[currentType_Tool]],
               gprofilerTransformedResult_links)
}

cat("***** LINKS unit tests *****\n\n")

testDBLinksAttached()

cat(sprintf("### PASS: %d - FAIL: %d ###\n", testsPassed, testsFailed))
cat(sprintf("------------------------------\n\n"))
