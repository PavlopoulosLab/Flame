handleVolcanoPlot <- function(readCallBackFunction) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Volcano Plot.</p>")
    volcanoInput <- readCallBackFunction()
    if (isValidVolcanoInput(volcanoInput)) {
      updateTabItems(session, "inputPlots", selected = "Volcano Plot")
      renderShinyDataTable("volcanoViewer", volcanoInput,
                           fileName = "volcano")
      currentVolcano <<- prepareVolcanoPlot(volcanoInput)
      appendVolcanoColorColumn()
      renderVolcano()
      renderShinyText("volcanoSelected", "")
      volcanoSelectedItems <<- c()
      shinyjs::show("volcanoPanel")
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not draw Volcano plot.")
  }, finally = {
    removeModal()
  })
}

readVolcanoInput <- function() {
  volcanoFile <- input$volcanoUpload
  fileType <- tolower(substr(volcanoFile$name, nchar(volcanoFile$name) - 2,
                             nchar(volcanoFile$name)))
  if (fileType == "tsv" || fileType == "txt")
    volcanoInput <- read.delim(volcanoFile$datapath, header = T)
  else if (fileType == "csv")
    volcanoInput <- read.csv(volcanoFile$datapath, header = T)
  return(volcanoInput)
}

readVolcanoExample <- function() {
  volcanoInput <- readRDS("examples/volcano_example.RDS")
  return(volcanoInput)
}

isValidVolcanoInput <- function(volcanoInput) {
  isValid <- F
  if (areInvalidVolcanoColumns(volcanoInput))
    return(isValid)
  if (isInvalidObjectSize(volcanoInput, prefix = "volcano"))
    return(isValid)
  isValid <- T
  return(isValid)
}

areInvalidVolcanoColumns <- function(volcanoInput) {
  areInvalid <- F
  if (!all(tolower(colnames(volcanoInput)) %in% c("symbol", "logfc", "pvalue")) ||
    length(colnames(volcanoInput)) != 3) {
    areInvalid <- T
    renderWarning("Need 3 columns: symbol, logFC and pvalue")
  }
  return(areInvalid)
}

prepareVolcanoPlot <- function(volcanoInput) {
  colnames(volcanoInput) <- tolower(colnames(volcanoInput))
  volcanoInput$pvalue <- -log10(volcanoInput$pvalue)
  colnames(volcanoInput)[colnames(volcanoInput) == "pvalue"] <- "-log10Pvalue"
  maxAbsoluteLogFC <- max(max(volcanoInput$logfc), abs(min(volcanoInput$logfc)))
  updateVolcanoSliders(max(volcanoInput$`-log10Pvalue`), maxAbsoluteLogFC)
  return(volcanoInput)
}

appendVolcanoColorColumn <- function() {
  pvalueThreshold <- input$volcano_pvalue_slider
  logFCThreshold <- input$volcano_fc_slider
  currentVolcano$expression <<- "default"
  if (nrow(currentVolcano[(currentVolcano$logfc > logFCThreshold) &
                        (currentVolcano$`-log10Pvalue` > pvalueThreshold), ] > 0))
    currentVolcano[(currentVolcano$logfc > logFCThreshold) &
                   (currentVolcano$`-log10Pvalue` > pvalueThreshold), ]$expression <<- "overexpressed"
  if (nrow(currentVolcano[(currentVolcano$logfc < -logFCThreshold) &
                        (currentVolcano$`-log10Pvalue` > pvalueThreshold), ] > 0))
    currentVolcano[(currentVolcano$logfc < -logFCThreshold) &
                   (currentVolcano$`-log10Pvalue` > pvalueThreshold), ]$expression <<- "underexpressed"
}

handleVolcanoRedraw <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Redrawing Volcano Plot.</p>")
    appendVolcanoColorColumn()
    renderVolcano()
  }, error = function(e) {
    cat(paste("Volcano redraw error:  ", e))
    renderError("Volcano redraw error.")
  }, finally = {
    removeModal()
  })
}

handleVolcanoSubmit <- function() {
  tryCatch({
    if (existVolcanoSelectedItems()) {
      showModal(modalDialog(
        title = "Volcano selected set",
        paste0("Add the selected items to the lists?"),
        footer = tagList(
          actionButton("volcano_ok", "OK"),
          modalButton("Cancel")
        )
      ))
    }
  }, error = function(e) {
    cat(paste("Volcano submit selected list error:  ", e))
    renderError("Volcano submit selected list error.")
  })
}

existVolcanoSelectedItems <- function() {
  existItems <- T
  if (length(volcanoSelectedItems) == 0) {
    existItems <- F
    renderWarning("Hover on the volcano plot,
                  choose the box or lasso and select at least one item first.")
  }
  return(existItems)
}

handleVolcanoListAccept <- function() {
  tryCatch({
    volcanoDF <- as.data.frame(volcanoSelectedItems)
    listName <- getRandomListName("volcano")
    updateUserInputLists(volcanoDF, listName)
    updateCheckboxInput(session, "selectAll", value = 0)
    updateListBoxes()
    removeModal()
  }, error = function(e) {
    print(paste0("Error: ", e))
    renderError("Problem with Volcano Plot list selection.")
  })
}
