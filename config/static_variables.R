# Input
userInputLists <- list()
checkedListNames <- list() # in combination with js_listNames 
volcanoSelectedItems <- c()

# Enrichment
enrichmentResults <- list()
arenaEdgelist <- list()
gprofilerResult <- list() # for gprofiler ManhattanPlot only
combinationResult <- data.frame()
enrichmentBackgroundSizes <- list()

# STRING Network
STRINGNetworkData <- list()


# Current
currentVariantResults <- data.frame()
currentTextminingResult <- c()
currentVolcano <- data.frame()
currentUpsetMode <- ""
currentConversionResult <- data.frame()
currentOrthologyResult <- data.frame()

currentUserList <- c()
currentEnrichmentType <- ""
currentOrganism <- ""
currentEnrichmentTool <- ""
currentType_Tool <- ""
currentNamespace <- ""
currentSignificanceMetric <- ""
currentBackgroundList <- c()