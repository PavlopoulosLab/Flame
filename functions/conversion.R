handle_gconvert <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Converting your list.</p>")
    gconvert_select <- input$gconvert_select
    gconvert_organism <- input$gconvert_organism
    gconvert_target <- input$gconvert_target
    
    if (existInputGeneLists()) {
      genesForconvert <- userInputLists[names(userInputLists)==gconvert_select][[1]]
      gconvert_organism <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$print_name == gconvert_organism,]$gprofiler_ID
      converted_genes <- gconvert(unlist(genesForconvert), organism = gconvert_organism, target = c(gconvert_target), numeric_ns = "", mthreshold = Inf, filter_na = T)
      if (!identical(converted_genes, NULL)){
        converted_genes <- converted_genes[-c(1,3,7)]
        for (i in nrow(converted_genes ):1){ # remove nan rows from table
          if (identical(converted_genes$target [i], "nan")) converted_genes <- converted_genes[-i, ]
        }
        output$gconvert_table <- DT::renderDataTable(converted_genes, server = FALSE, 
                                                     extensions = 'Buttons',
                                                     options = list(
                                                       pageLength = 10,
                                                       "dom" = 'T<"clear">lBfrtip',
                                                       buttons = list(list(extend='excel',filename=paste('Conversion_IDs_Results_', gconvert_select, sep="")),
                                                                      list(extend= 'csv',filename=paste('Conversion_IDs_Results_', gconvert_select, sep="")),
                                                                      list(extend='copy',filename=paste('Conversion_IDs_Results_', gconvert_select, sep="")),
                                                                      list(extend='pdf',filename=paste('Conversion_IDs_Results_', gconvert_select, sep="")),
                                                                      list(extend='print',filename=paste('Conversion_IDs_Results_', gconvert_select, sep="")))
                                                     ),rownames= FALSE, escape = FALSE
        )
      }
      else
        renderWarning("No results found. Please try another organism (input/target) or list input.")
    }
  }, error = function(e) {
    print(paste("Error :  ", e))
    renderError("Problem with conversion.")
  }, finally = {
    removeModal()
  })
}

handle_gorthOrganism <- function() {
  tryCatch({
    gorth_organism <- input$gorth_organism
    
    org_choices <- ORGANISMS_FROM_FILE$print_name
    updateSelectInput(session, "gorth_target", choices = org_choices[org_choices!=gorth_organism])
  }, error = function(e) {
    print(paste("Error :  ", e))
    renderError("Problem with organism selection.")
  })
}

handle_gorth <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Searching for homologs.</p>")
    gorth_select <- input$gorth_select
    gorth_organism <- input$gorth_organism
    gorth_target <- input$gorth_target
    
    if (existInputGeneLists()) {
      genesForgorth <- userInputLists[names(userInputLists)==gorth_select][[1]]
      gorth_organism <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$print_name == gorth_organism,]$gprofiler_ID
      gorth_target <- ORGANISMS_FROM_FILE[ORGANISMS_FROM_FILE$print_name == gorth_target,]$gprofiler_ID
      gorth_genes <- gorth(unlist(genesForgorth), source_organism = gorth_organism, target_organism = c(gorth_target), numeric_ns = "", mthreshold = Inf, filter_na = T)
      gorth_genes <- gorth_genes[-c(1,4)]
      if (!identical(gorth_genes, NULL)){
        output$gorth_table<- DT::renderDataTable(gorth_genes, server = FALSE, 
                                                 extensions = 'Buttons',
                                                 options = list(
                                                   pageLength = 10,
                                                   "dom" = 'T<"clear">lBfrtip',
                                                   buttons = list(list(extend='excel',filename=paste('Orthology_Results_', gorth_select, sep="")),
                                                                  list(extend= 'csv',filename=paste('Orthology_Results_', gorth_select, sep="")),
                                                                  list(extend='copy',filename=paste('Orthology_Results_', gorth_select, sep="")),
                                                                  list(extend='pdf',filename=paste('Orthology_Results_', gorth_select, sep="")),
                                                                  list(extend='print',filename=paste('Orthology_Results_', gorth_select, sep="")))
                                                 ),rownames= FALSE, escape = FALSE)
      }
      else
        renderWarning("No results found. Please try another organism (input/target) or list input.")
    }
  }, error = function(e) {
    print(paste("Error :  ", e))
    renderError("Problem with orthology search")
  }, finally = {
    removeModal()
  })
}
