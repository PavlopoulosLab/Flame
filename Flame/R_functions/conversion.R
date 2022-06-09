# This function converts input lists into targeted namespace
# @param gconvert_select: file from available uploaded to convert
# @param gconvert_organism: target organism
# @param gconvert_target: target namespace
# @return void
handle_gconvert <- function(gconvert_select, gconvert_organism, gconvert_target, output){
  if (identical(file_data, list())) session$sendCustomMessage("handler_alert", "Please, first upload your data files.")
  else {
    session$sendCustomMessage("handler_disableAllButtons", T) # disable all buttons until execution is over
    session$sendCustomMessage("handler_startLoader", c(4,10))
    genesForconvert <- file_data[file_names==gconvert_select][[1]]
    gconvert_organism <- organismsFromFile[organismsFromFile$print_name == gconvert_organism,]$gprofiler_ID
    session$sendCustomMessage("handler_startLoader", c(4,30))
    converted_genes <- gconvert(unlist(genesForconvert), organism = gconvert_organism, target = c(gconvert_target), numeric_ns = "", mthreshold = Inf, filter_na = T)
    if (!identical(converted_genes, NULL)){
      converted_genes <- converted_genes[-c(1,3,7)]
      session$sendCustomMessage("handler_startLoader", c(4,70))
      for (i in nrow(converted_genes ):1){ # remove nan rows from table
        if (identical(converted_genes$target [i], "nan")) converted_genes <- converted_genes[-i, ]
      }
      print(converted_genes)
      session$sendCustomMessage("handler_startLoader", c(4,80))
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
    else  session$sendCustomMessage("handler_alert", "No results found. Please try another organism (input/target) or list input.")
    session$sendCustomMessage("handler_startLoader", c(4,100))
    session$sendCustomMessage("handler_finishLoader", 4)
    session$sendCustomMessage("handler_enableAllButtons", T) # now enable buttons again
  }
}

# This function converts input lists into targeted orthologs
# @param gorth_select: file from available uploaded to convert
# @param gorth_organism: source organism
# @param gorth_target: target organism
# @return void
handle_gorth <- function(gorth_select, gorth_organism, gorth_target, output){
  if (identical(file_data, list())) session$sendCustomMessage("handler_alert", "Please, first upload your data files.")
  else {
    session$sendCustomMessage("handler_disableAllButtons", T) # disable all buttons until execution is over
    session$sendCustomMessage("handler_startLoader", c(5,10))
    genesForgorth <- file_data[file_names==gorth_select][[1]]
    gorth_organism <- organismsFromFile[organismsFromFile$print_name == gorth_organism,]$gprofiler_ID
    gorth_target <- organismsFromFile[organismsFromFile$print_name == gorth_target,]$gprofiler_ID
    session$sendCustomMessage("handler_startLoader", c(5,40))
    gorth_genes <- gorth(unlist(genesForgorth), source_organism = gorth_organism, target_organism = c(gorth_target), numeric_ns = "", mthreshold = Inf, filter_na = T)
    gorth_genes <- gorth_genes[-c(1,4)]
    session$sendCustomMessage("handler_startLoader", c(5,80))
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
    else  session$sendCustomMessage("handler_alert", "No results found. Please try another organism (input/target) or list input.")
    session$sendCustomMessage("handler_startLoader", c(5,100))
    session$sendCustomMessage("handler_finishLoader", 5)
    session$sendCustomMessage("handler_enableAllButtons", T) # now enable buttons again
    
  }
}

# This function removes selected source organism from the selection list of target organisms
# @param gorth_organism: source organism to be removed from target list
# @return void
handle_gorthOrganism <- function(gorth_organism, session){
  org_choices <- organismsFromFile$print_name
  updateSelectInput(session, "gorth_target", choices = org_choices[org_choices!=gorth_organism])
}
