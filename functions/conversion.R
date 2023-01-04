handle_gconvert <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Converting list.</p>")
    gconvertSelect <- input$gconvert_select
    gconvertOrganism <- input$gconvert_organism
    gconvertTarget <- input$gconvert_target
    if (existInputGeneLists()) {
      selectedListItems <- userInputLists[[gconvertSelect]][[1]]
      gconvertOrganism <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$print_name == gconvertOrganism, ]$gprofiler_ID
      convertedOutput <- gconvert(
        selectedListItems, organism = gconvertOrganism, target = gconvertTarget
      )
      if (existsConverteOutput(convertedOutput)) {
        convertedOutput <- convertedOutput[c("input", "target", "name", "description")]
        convertedOutput <- convertedOutput[convertedOutput$target != "nan", ]
        renderShinyDataTable("gconvert_table", convertedOutput,
                             fileName = paste0('conversion_', gconvertSelect))
      }
    }
  }, error = function(e) {
    print(paste("Conversion error: ", e))
    renderError("Error while converting the list input.")
  }, finally = {
    removeModal()
  })
}

existsConverteOutput <- function(convertedOutput) {
  exist <- T
  if (is.null(convertedOutput)) {
    exist <- F
    renderWarning("No results found. Please try another organism (input/target) or list input.")
  }
  return(exist)
}

handle_gorthOrganism <- function() {
  tryCatch({
    gorthOrganism <- input$gorth_organism
    gorthTarget <- input$gorth_target
    if (gorthOrganism == gorthTarget) {
      organismChoices <- ORGANISMS_FROM_FILE$print_name
      updateSelectInput(session, "gorth_target",
                        choices = organismChoices[organismChoices != gorthOrganism])
    }
  }, error = function(e) {
    print(paste("Organism update error: ", e))
    renderError("Error while choosing organism.")
  })
}

handle_gorth <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Searching for homologs.</p>")
    gorthSelect <- input$gorth_select
    gorthOrganism <- input$gorth_organism
    gorthTarget <- input$gorth_target
    if (existInputGeneLists()) {
      selectedListItems <- userInputLists[[gorthSelect]][[1]]
      gorthOrganism <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$print_name == gorthOrganism,]$gprofiler_ID
      gorthTarget <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$print_name == gorthTarget,]$gprofiler_ID
      convertedOutput <- gorth(selectedListItems, source_organism = gorthOrganism,
                           target_organism = gorthTarget)
      if (existsConverteOutput(convertedOutput)) {
        convertedOutput <- convertedOutput[c(
          "input", "input_ensg", "ortholog_name", "ortholog_ensg", "description"
        )]
        renderShinyDataTable("gorth_table", convertedOutput,
                             fileName = paste0('orthology_', gorthSelect))
      }
    }
  }, error = function(e) {
    print(paste("Orthology error: ", e))
    renderError("Error while searching for homologs.")
  }, finally = {
    removeModal()
  })
}
