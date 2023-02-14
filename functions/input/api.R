resolveAPI <- function() {
  tryCatch({
    query <- parseQueryString(session$clientData$url_search)
    if (apiCallExists(query)) {
      updateTabItems(session, "sideBarId", selected = "file_handler")
      
      if (length(query$f) > 0)
        handleGETFileFromPOSTRequest(paste0(POST_REQUEST_PATH, query$f))
      else if (length(query$url_genes) > 0)
        handleGETRequest(query$url_genes)
      
      if (length(userInputLists) > 0)
        printSuccessfulAPICallText()
    }
  }, error = function(e) {
    print(paste("API error: ", e))
    renderError("Error with external API call.")
  })
}

apiCallExists <- function(query) {
  return((length(query$f) > 0) || (length(query$url_genes) > 0))
}

handleGETFileFromPOSTRequest <- function(jsonFile) {
  jsonLists <- jsonlite::fromJSON(jsonFile)
  jsonLists <- limitLists(jsonLists)
  for (i in 1:length(jsonLists)) {
    jsonList <- jsonLists[i]
    jsonDF <- parseListIntoDataFrame(jsonList)
    if (isValidInputDF(jsonDF, prefix = "")) {
      updateUserInputLists(jsonDF, colnames(jsonDF))
      updateListBoxes()
    }
  }
}

limitLists <- function(inputLists) {
  if (length(inputLists) > LIST_LIMIT) {
    renderWarning(paste0("Keeping the first ", LIST_LIMIT, " lists."))
    inputLists <- inputLists[1:LIST_LIMIT]
  }
  return(inputLists)
}

parseListIntoDataFrame <- function(inputList) {
  if (length(inputList) == 0)
    return(data.frame())
  listName <- names(inputList)
  inputList <- substituteSpecialChars(inputList[[1]], toChar = "")
  inputList <- inputList[inputList != '']
  inputDF <- as.data.frame(inputList)
  colnames(inputDF) <- listName
  return(unique(inputDF))
}

printSuccessfulAPICallText <- function() {
  renderShinyText("url_checked", "Gene list(s) loaded from an API call.")
}

handleGETRequest <- function(urlInput) {
  inputLists <- parseUrlStringIntoLists(urlInput)
  lapply(inputLists, function(inputList) {
    buildUserListFromText(inputList, prefix = "url_gene_list")
  })
}

parseUrlStringIntoLists <- function(urlInput) {
  inputLists <- strsplit(urlInput, ";")[[1]]
  inputLists <- limitLists(inputLists)
  return(inputLists)
}
