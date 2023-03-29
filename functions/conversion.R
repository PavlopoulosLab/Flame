handle_gconvert <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Converting list.</p>")
    gconvertSelect <- input$gconvert_select
    gconvertOrganism <- input$gconvert_organism
    gconvertTarget <- input$gconvert_target
    if (existInputGeneLists()) {
      if (isConversionOrganismValid()) {
        selectedListItems <- userInputLists[[gconvertSelect]][[1]]
        gconvertOrganism <- ORGANISMS[ORGANISMS$print_name == gconvertOrganism, ]$short_name
        convertedOutput <- gprofiler2::gconvert(
          selectedListItems, organism = gconvertOrganism, target = gconvertTarget,
          mthreshold = 1, filter_na = T
        )
        if (existsConverteOutput(convertedOutput)) {
          convertedOutput <- convertedOutput[c("input", "target", "name", "description")]
          currentConversionResult <<- convertedOutput
          colnames(convertedOutput) <- c("Input", "Target", "Name", "Description")
          shinyjs::show("gconvert_resultsPanel")
          renderShinyDataTable("gconvert_table", convertedOutput,
                               fileName = paste0('conversion_', gconvertSelect))
        }
      }
    }
  }, error = function(e) {
    print(paste("Conversion error: ", e))
    renderError("Error while converting the list input.")
  }, finally = {
    removeModal()
  })
}

isConversionOrganismValid <- function() {
  isValid <- T
  if (input$gconvert_organism == "") {
    isValid <- F
    renderWarning("Select an input organism.")
  }
  return(isValid)
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
      organismChoices <- ORGANISMS$print_name
      updateSelectizeInput(session, "gorth_target", server = T,
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
      if (areOrthologyOrganismsValid()) {
        selectedListItems <- userInputLists[[gorthSelect]][[1]]
        gorthOrganism <- ORGANISMS[ORGANISMS$print_name == gorthOrganism,]$short_name
        gorthTarget <- ORGANISMS[ORGANISMS$print_name == gorthTarget,]$short_name
        convertedOutput <- 
          gprofiler2::gorth(selectedListItems, 
                            source_organism = gorthOrganism,
                            target_organism = gorthTarget,
                            mthreshold = 1, filter_na = T)
        if (existsConverteOutput(convertedOutput)) {
          convertedOutput <- convertedOutput[c(
            "input", "input_ensg", "ortholog_name", "ortholog_ensg", "description"
          )]
          currentOrthologyResult <<- convertedOutput
          colnames(convertedOutput) <- c("Input", "Input ID", "Ortholog Name", "Ortholog ID", "Description")
          shinyjs::show("gorth_resultsPanel")
          renderShinyDataTable("gorth_table", convertedOutput,
                               fileName = paste0('orthology_', gorthSelect))
        }
      }
    }
  }, error = function(e) {
    print(paste("Orthology error: ", e))
    renderError("Error while searching for homologs.")
  }, finally = {
    removeModal()
  })
}

areOrthologyOrganismsValid <- function() {
  areValid <- T
  if (input$gorth_organism == "" || input$gorth_target == "") {
    areValid <- F
    renderWarning("Select both an input and target organism.")
  }
  return(areValid)
}