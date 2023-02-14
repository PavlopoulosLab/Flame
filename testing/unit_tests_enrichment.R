setwd(this.path::this.dir())

source("./unit_tests_assert_function.R")
source("../config/global_variables.R")
source("../functions/render.R")
source("../functions/enrichment/main.R")
source("../functions/enrichment/general.R")
source("../functions/enrichment/agotool.R")
source("../functions/enrichment/gprofiler.R")

testsPassed <- 0
testsFailed <- 0

userInputLists <- list(c("SP1", "MBL2", "NOD2"), c("ACE", "VDR"), c("MBL2"))
inputENSPList <- c(
  "ENSP00000329357", "ENSP00000331327", "ENSP00000261733", "ENSP00000446252",
  "ENSP00000300093", "ENSP00000441543", "ENSP00000340684", "ENSP00000365777",
  "ENSP00000384620", "ENSP00000221930"
)
taxid <- 9606
input <- list()
input$functional_enrichment_datasources <- c("GO:MF", "KEGG")
input$functional_enrichment_organism <- "hsapiens"
input$functional_enrichment_namespace <- "USERINPUT"
input$functional_enrichment_metric <- "False discovery rate"
input$functional_enrichment_threshold <- 0.05
testConvertResult1 <- data.frame(
  input = c("SP1", "MBL2", "NOD2"),
  target = c("SP1", "MBL2", "NOD2"),
  name = c("SP1", "MBL2", "NOD2")
)
testConvertResult2 <- data.frame(
  input = c("MBL2", "MBL2", "MBL2"),
  target = c("ENSP00000363079", "ENSP00000502615", "ENSP00000502789"),
  name = c("MBL2", "MBL2", "MBL2")
)
enrichmentParsedResult <- readRDS("RDS/enrichmentParsedResult.RDS")
enrichmentParsedResult_transformed <- readRDS("RDS/enrichmentParsedResult_transformed.RDS")
hitGenesCount <- c(4, 5, 5, 2, 2, 3, 2, 2, 1, 1, 1)
databaseGenesCount <- c(424, 1138, 1507, 34, 39, 346, 52, 58, 1, 1, 1)
enrichmentScore <- c(0.94, 0.44, 0.33, 5.88, 5.13, 0.87, 3.85, 3.45, 100.00, 100.00, 100.00)
convertedInputs <- c(
  "ENSP00000263025", "ENSP00000433746", "ENSP00000436772", "ENSP00000327293",
  "ENSP00000378625", "ENSP00000378626", "ENSP00000378628", "ENSP00000432292",
  "ENSP00000432479", "ENSP00000432742", "ENSP00000433639"
)
noHitGenes <- c(
  "ENSP00000433746", "ENSP00000436772", "ENSP00000327293", "ENSP00000378625",
  "ENSP00000378626", "ENSP00000378628", "ENSP00000432292", "ENSP00000432479",
  "ENSP00000432742", "ENSP00000433639"
)
aGoToolResultRequestBody <- list(
  "taxid" = 9606,
  "FDR_cutoff" = 0.05,
  "limit_2_entity_type" = "-23;-52",
  "foreground" = "9606.ENSP00000329357%0d9606.ENSP00000331327%0d9606.ENSP00000261733%0d9606.ENSP00000446252%0d9606.ENSP00000300093%0d9606.ENSP00000441543%0d9606.ENSP00000340684%0d9606.ENSP00000365777%0d9606.ENSP00000384620%0d9606.ENSP00000221930",
  "enrichment_method" = "genome",
  "p_value_cutoff" = 1
)
response <- ""
parsedAGoToolResult <- readRDS("RDS/parsedAGoToolResult.RDS")

testInputGeneListsExist <- function() {
  assertTrue(existInputGeneLists())
  userInputLists <<- list()
  assertFalse(existInputGeneLists())
  userInputLists <<- list(c("SP1", "MBL2", "NOD2"), c("ACE", "VDR"), c("MBL2"))
}

testDataSourcesExist <- function() {
  assertTrue(existDataSources())
  input$functional_enrichment_datasources <<- c()
  assertFalse(existDataSources())
  input$functional_enrichment_datasources <<- c("GO:MF", "KEGG")
}

testInputOrganismIsNotEmpty <- function() {
  assertTrue(existInputOrganism())
  input$functional_enrichment_organism <<- ""
  assertFalse(existInputOrganism())
  input$functional_enrichment_organism <<- "hsapiens"
}

testGeneConvert <- function() {
  assertEquals(testConvertResult1,
               geneConvert(userInputLists[[1]],
                           input$functional_enrichment_organism))
  input$functional_enrichment_namespace <<- "ENSP"
  assertEquals(testConvertResult2,
               geneConvert(userInputLists[[3]],
                           input$functional_enrichment_organism))
  input$functional_enrichment_namespace <<- "USERINPUT"
}

testInputGenesConversionTableValid <- function() {
  assertTrue(validInputGenesConversionTable(userInputLists[[1]]))
  assertFalse(validInputGenesConversionTable(NULL))
}

testResultTableTransformation <- function() {
  assertEquals(transformEnrichmentResultTable(enrichmentParsedResult),
               enrichmentParsedResult_transformed)
}

testEnrichmentScoreCalculation <- function() {
  assertEquals(enrichmentScore,
               calculateEnrichmentScore(hitGenesCount, databaseGenesCount))
}

testNoHitGenesFound <- function() {
  enrichmentResults[[currentType_Tool]] <<- readRDS("RDS/gprofilerTransformedResult2.RDS")
  assertEquals(noHitGenes,
               findNoHitGenes(convertedInputs))
}

testAGoToolRequestBodyBuilt <- function() {
  assertEquals(buildAGoToolRequestBody(inputENSPList, taxid),
             aGoToolResultRequestBody)
}

testResponseValid <- function() {
  response <<- sendAGoToolPOSTRequest(aGoToolResultRequestBody)
  assertTrue(isResponseValid(response))
}

testAGoToolResultParsed <- function() {
  assertEquals(parseAGoToolResult(response, taxid),
               parsedAGoToolResult)
}

cat("***** FUNCTIONAL ENRICHMENT unit tests *****\n\n")

testInputGeneListsExist()
testDataSourcesExist()
testInputOrganismIsNotEmpty()
testGeneConvert() # internet query
testInputGenesConversionTableValid() 
testResultTableTransformation()
testEnrichmentScoreCalculation()
testNoHitGenesFound()
testAGoToolRequestBodyBuilt()
testResponseValid()
testAGoToolResultParsed()
  
cat(sprintf("### PASS: %d - FAIL: %d ###\n", testsPassed, testsFailed))
cat(sprintf("------------------------------\n\n"))
