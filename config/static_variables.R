# Input
userInputLists <- list()
checkedListNames <- list() # in combination with js_listNames 
volcanoSelectedItems <- c()

# Enrichment
enrichmentResults <- list()
arenaEdgelist <- list()
gprofilerResult <- list() # for gprofiler ManhattanPlot only
combinationResult <- data.frame()

# Current
currentVariantResults <- data.frame()
currentTextminingResult <- c()
currentVolcano <- data.frame()
currentUpsetMode <- ""

currentUserList <- c()
currentEnrichmentType <- ""
currentOrganism <- ""
currentEnrichmentTool <- ""
currentType_Tool <- ""
currentNamespace <- ""
currentSignificanceMetric <- ""