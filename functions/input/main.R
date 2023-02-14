handleRandomExample <- function() {
  tryCatch({
    exampleGeneList <- readRDS("examples/random_genes_example.RDS")
    exampleGeneList <- sample(exampleGeneList, RANDOM_GENES_NUMBER, replace = F)
    exampleGeneList <- paste(exampleGeneList, collapse = '\n')
    updateTextAreaInput(session, "textAreaList", value = exampleGeneList)
  }, error = function(e) {
    cat(paste("Random list generation error:  ", e))
    renderError("Random list generation error.")
  })
}

handleClearText <- function() {
  tryCatch({
    updateTextAreaInput(session, "textAreaList", value = "")
  }, error = function(e) {
    cat(paste("List clear error:  ", e))
    renderError("List clear error.")
  })
}

handleTextSubmit <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")
    buildUserListFromText(input$textAreaList, GENE_LIST_PREFIX)
  }, error = function(e) {
    cat(paste("Text submit error:  ", e))
    renderError("Text submit error.")
  }, finally = {
    removeModal()
  })
}

buildUserListFromText <- function(inputText, prefix) {
  inputDF <- parseTextIntoDataFrame(inputText)
  inputDF <- limitInput(inputDF)
  if (isValidInputDF(inputDF, prefix)) {
    randomListName <- getRandomListName(prefix)
    updateUserInputLists(inputDF, randomListName)
    updateListBoxes()
    updateTextAreaInput(session, "textAreaList", value = "")
    updateCheckboxInput(session, "selectAll", value = 0)
    if (prefix == GENE_LIST_PREFIX)
      renderShinyText("url_checked", "")
  }
}

parseTextIntoDataFrame <- function(inputText) {
  inputDF <- substituteSpecialChars(inputText, toChar = "\n")
  inputDF <- strsplit(inputDF, "\n")[[1]]
  inputDF <- as.data.frame(inputDF)
  inputDF <- inputDF[inputDF != "", , drop = F]
  return(unique(inputDF))
}

substituteSpecialChars <- function(text, toChar) {
  text <- gsub(",", toChar, text)
  text <- gsub(" ", toChar, text)
  text <- gsub("\t", toChar, text)
  text <- gsub("\r", toChar, text)
  return(text)
}

limitInput <- function(textData) {
  if (nrow(textData) > GENE_LIST_LIMIT) {
    textData <- textData[c(1:GENE_LIST_LIMIT), , drop = F]
    renderWarning(paste0("Limit reached. Keeping the first ",
                         GENE_LIST_LIMIT, " input items."))
  }
  return(textData)
}

isValidInputDF <- function(inputDF, prefix) {
  isValid <- F
  if (isTextListEmpty(inputDF, prefix))
    return(isValid)
  if (isListLimitReached())
    return(isValid)
  if (isInvalidObjectSize(inputDF, prefix))
    return(isValid)
  isValid <- T
  return(isValid)
}

isTextListEmpty <- function(inputDF, prefix) {
  isEmpty <- F
  if (nrow(inputDF) == 0) {
    if (prefix == GENE_LIST_PREFIX)
      renderWarning("Please, paste your input list in the text area first.")
    isEmpty <- T
  }
  return(isEmpty)
}

isListLimitReached <- function() {
  isReached <- F
  if (length(names(userInputLists)) == LIST_LIMIT) {
    isReached <- T
    renderWarning(
      paste0("You have reached the maximum numbers of lists (", LIST_LIMIT, ").")
    )
  }
  return(isReached)
}

isInvalidObjectSize <- function(inputDF, prefix) {
  isInvalid <- F
  if (object.size(inputDF) > OBJECT_SIZE_LIMIT) {
    isInvalid <- T
    if (prefix == GENE_LIST_PREFIX || prefix == "volcano")
      renderWarning(paste0("Make sure your input is <",
                           OBJECT_SIZE_LIMIT, " bytes."))
  }
  return(isInvalid)
}

getRandomListName <- function(prefix) {
  repeat {
    suffix <- paste(sample(c(0:9, letters, LETTERS), 6), collapse = "")
    listName <- paste(prefix, suffix, sep = "_")
    if (is.na(match(listName, names(userInputLists))))
      break
  }
  return(listName)
}

handleInputFiles <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Uploading files.</p>")
    inputFiles <- limitLists(input$fileUpload)
    
    for (i in 1:nrow(inputFiles)) {
      listName <- (inputFiles$name[i])
      if (notExistsFileName(listName)){
        fileData <- readChar(inputFiles$datapath[i], file.info(inputFiles$datapath[i])$size)
        inputDF <- parseTextIntoDataFrame(fileData)
        if (isValidInputDF(inputDF, prefix = "")) {
          updateUserInputLists(inputDF, listName)
          updateListBoxes()
        }
      }
    }
  }, error = function(e) {
    cat(paste("File upload error:  ", e))
    renderError("File upload error.")
  }, finally = {
    removeModal()
  })
}

limitLists <- function(inputFiles) {
  if (nrow(inputFiles) > LIST_LIMIT) {
    renderWarning(paste0("Keeping the first ", LIST_LIMIT, " files."))
    inputFiles <- inputFiles[1:LIST_LIMIT, ]
  }
  return(inputFiles)
}

limitListName <- function(listName) {
  return(substr(trimws(listName), 1, LISTNAME_NCHAR_LIMIT))
}

notExistsFileName <- function(fileName) {
  notExists <- T
  if (!is.na(match(fileName, names(userInputLists)))) {
    renderWarning(paste0("A list named ", fileName,
                         " already exists. File was not uploaded."))
    notExists <- F
  }
  return(notExists)
}

handleSelectAllLists <- function() {
  tryCatch({
    if (input$selectAll == 0)
      updateCheckboxGroupInput(
        session, "checkboxLists",
        choices = names(userInputLists), selected = NULL
      )
    else
      updateCheckboxGroupInput(
        session, "checkboxLists",
        choices = names(userInputLists), selected = names(userInputLists)
      )
  }, error = function(e) {
    cat(paste("List selection error:  ", e))
    renderError("List selection error.")
  })
}

handlePrepareRenameLists <- function() {
  tryCatch({
    checkedListNames <<-
      names(userInputLists)[which(names(userInputLists) %in% input$checkboxLists)]
    session$sendCustomMessage("handler_renameLists", input$checkboxLists)
  }, error = function(e) {
    cat(paste("List rename error:  ", e))
    renderError("List rename error.")
  })
}

handleRenameLists <- function() {
  tryCatch({
    js_listNames <- input$js_listNames
    for (i in 1:length(js_listNames)) {
      listName <- limitListName(js_listNames[i])
      oldListName <- checkedListNames[i]
      if (listName != oldListName)
        if (isListNameNotEmpty(listName, oldListName))
          if (notExistsListName(listName, oldListName)) {
            position <- match(oldListName, names(userInputLists))
            names(userInputLists)[position] <<-
              names(userInputLists[[position]]) <<- listName # both list and df names
          }
    }
    updateListBoxes()
    updateCheckboxInput(session, "selectAll", value = 0)
  }, error = function(e) {
    cat(paste("List rename error:  ", e))
    renderError("List rename error.")
  })
}

isListNameNotEmpty <- function(listName, oldListName) {
  isNotEmpty <- T
  if (listName == "") {
    isNotEmpty <- F
    renderWarning(paste0("Empty name found. List ",
                         oldListName, " didn't change name."))
  }
  return(isNotEmpty)
}

notExistsListName <- function(listName, oldListName = "") {
  notExists <- T
  if (!is.na(match(listName, names(userInputLists)))) {
    notExists <- F
    if (oldListName == "") # called from Upset plot click
      renderWarning(paste0(
        "Duplicate name: ", listName, ". List was not added."
      ))
    else # called from rename
      renderWarning(paste0(
        "Duplicate name: ", listName, ". List ",
        oldListName, " didn't change name."
      ))
  }
  return(notExists)
}

handleRemoveLists <- function() {
  tryCatch({
    positions <- which(names(userInputLists) %in% input$checkboxLists)
    if (!identical(positions, integer(0))) {
      userInputLists <<- userInputLists[-positions]
      if (length(userInputLists) == 0)
        renderShinyDataTable("selectedListView", data.frame())
      updateListBoxes()
      updateCheckboxInput(session, "selectAll", value = 0)
    }
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with list removal.")
  })
}

handleSelectView <- function() {
  tryCatch({
    selectView <- input$selectView
    outputData <- as.data.frame(userInputLists[names(userInputLists) == selectView])
    renderShinyDataTable("selectedListView", outputData, fileName = selectView)
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with list viewing.")
  })
}
