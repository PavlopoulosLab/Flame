addConversionResultToInput <- function(prefix, dtype){
  tryCatch({
    shinyjs::hide(paste0(prefix, "_addedInfo"))
    renderModal("<h2>Please wait.</h2><br /><p>Parsing your data.</p>")
    if(prefix=="gconvert") {
      conversionTable <- currentConversionResult
      names <- unlist(conversionTable$name)
      ids <- unlist(conversionTable$target)
    }
    else {
      conversionTable <- currentOrthologyResult
      names <- unlist(conversionTable$ortholog_name)
      ids <- unlist(conversionTable$ortholog_ensg)   
    }
    if(dtype =="name" || dtype == "ortholog_name")
      result <- names
    else
      result <- ids
    buildUserListFromText(paste(result, collapse = "\n"), sprintf("%s_%s", prefix, dtype))
    shinyjs::show(paste0(prefix, "_addedInfo"))
  }, error = function(e) {
    cat(paste("Extracting list from ", prefix, " error: ", e))
    renderError(paste("Extracting list from ", prefix ," text-mining error."))
  }, finally = {
    removeModal()
  })
}