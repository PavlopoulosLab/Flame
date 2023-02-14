handleUpset <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Generating UpSet Plot.</p>")
    checkboxLists <- input$checkboxLists
    
    if (isValidUpsetSelection()) {
      upsetList <- c()
      for (selectedList in checkboxLists)
        upsetList <- c(upsetList, (userInputLists[[selectedList]]))
      createUpset(upsetList)
    }
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with UpSet Plot.")
  }, finally = {
    removeModal()
  })
}

isValidUpsetSelection <- function() {
  isValid <- T
  if (length(input$checkboxLists) < 2) {
    isValid <- F
    renderWarning("Please, select at least 2 lists.")
  }
  return(isValid)
}

createUpset <- function(upsetList){
  currentUpsetMode <<- input$upsetMode
  upsetModeFunction <- switch(
    currentUpsetMode,
    "Intersection" = upsetjs::generateIntersections,
    "Distinct Combinations" = upsetjs::generateDistinctIntersections,
    "Union" = upsetjs::generateUnions
  )
  renderUpset("upsetjsView", upsetList, upsetModeFunction)
  renderShinyText("hoveredInfoLabel", "Hovered set:")
}

handleUpsetHover <- function() {
  tryCatch({
    hoverSet <- input$upsetjsView_hover
    hoveredElements <- paste(hoverSet$elems, collapse = ', ')
    renderShinyText("hoveredListName", hoverSet$name)
    renderShinyText("hoveredElements", hoveredElements)
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with UpSet Plot hover.")
  })
}

handleUpsetClick <- function() {
  tryCatch({
    upsetjs_click <- input$upsetjsView_click
    if (!identical(as.character(upsetjs_click$elems), character(0))) {
      showModal(modalDialog(
        title = paste0(currentUpsetMode, " clicked set."),
        paste0("Add: ", upsetjs_click$name, " to the lists?"),
        footer = tagList(
          actionButton("upsetClick_ok", "OK"),
          modalButton("Cancel")
        )
      ))
    }
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with UpSet Plot click.")
  })
}

handleUpsetListAccept <- function() {
  tryCatch({
    upsetjs_click <- input$upsetjsView_click
    prefix <- switch(
      currentUpsetMode,
      "Intersection" = "intersect",
      "Distinct Combinations" = "distinct",
      "Union" = "union"
    )
    clickedElements <- as.data.frame(as.character(upsetjs_click$elems))
    if (isInputWithinLimit(clickedElements)) { # might not be within limits after Unions
      listName <- limitListName(paste(prefix, upsetjs_click$name, sep = "_"))
      if (notExistsListName(listName)) {
        updateUserInputLists(clickedElements, listName)
        updateCheckboxInput(session, "selectAll", value = 0)
        updateListBoxes()
      }
      
    }
    removeModal()
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with UpSet Plot list selection.")
  })
}

isInputWithinLimit <- function(clickedElements) {
  isWithinLimits <- T
  if (nrow(clickedElements) > GENE_LIST_LIMIT) {
    isWithinLimits <- F
    renderWarning(sprintf("Input limit is %d. Cannot append this list.",
                         GENE_LIST_LIMIT))
  }
  return(isWithinLimits)
}
