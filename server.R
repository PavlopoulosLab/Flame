function(input, output, session) {
  source("global_variables.R", local = TRUE)
  source("functions/general.R", local = TRUE)
  source("functions/init.R", local = TRUE)
  source("functions/render.R", local = TRUE)
  source("functions/update.R", local = TRUE)
  source("functions/reset.R", local = TRUE)
  
  source("functions/fileInput.R", local = TRUE)
  source("functions/enrichment/inputs_panel.R", local = TRUE)
  source("functions/enrichment/main.R", local = TRUE)
  source("functions/enrichment/general.R", local = TRUE)
  source("functions/enrichment/gprofiler.R", local = TRUE)
  source("functions/enrichment/agotool.R", local = TRUE)
  source("functions/links.R", local = TRUE)
  
  source("functions/plots/general.R", local = TRUE)
  source("functions/plots/manhattan.R", local = TRUE)
  source("functions/plots/scatter.R", local = TRUE)
  source("functions/plots/barchart.R", local = TRUE)
  source("functions/plots/heatmaps.R", local = TRUE)
  source("functions/plots/networks.R", local = TRUE)
  source("functions/interoperability.R", local = TRUE)
  
  source("functions/conversion.R", local = TRUE)
  source("functions/stringNetwork.R", local = TRUE)
  
  # Start ####
  initializeServerApp()
  
  # API ####
  output$url_checker <- renderText({ # this component needs to be in landing page in order to be observed on page load
    tryCatch({
      
      query <- parseQueryString(session$clientData$url_search)
      if (length(query$f) > 0){ # GET json file from POST request
        updateTabItems (session, "sideBarId", selected ="file_handler") # change tab to file input
        
        error_flag <- parse_import_data(paste0(POST_REQUEST_PATH, query$f), session)
        if (!error_flag) output$url_checked <- renderText({"Gene list(s) loaded from url."})
        
        paste("") # empty string to not print anything on landing page
      } else if (length(query$url_genes) > 0){ # GET request with written gene lists
        updateTabItems (session, "sideBarId", selected ="file_handler") # change tab to file input
        
        lists <- str_split(query$url_genes, ";")[[1]]
        for (list in lists) {
          flag <- handleTextSubmit(list, "url_gene_list", session)
          if (!flag) break
        }
        
        output$url_checked <- renderText({"Gene list(s) loaded from url."})
        paste("") # empty string to not print anything on landing page
      }
      
    }, error = function(e) {
      print(paste("Error :  ", e))
      renderError("Problem with url query handling.")
    })
  })
  
  observeEvent(input$link_to_fileinput, {
    updateTabItems (session, "sideBarId", selected = "file_handler")
  }, ignoreInit = T)
  
  # File Input ####
  observeEvent(input$files,{
    handleInputFiles(input$files)
  }, ignoreInit = T)
  
  observeEvent(input$textSubmit,{
    handleTextSubmit(input$text, "gene_list")
  }, ignoreInit = T)
  
  observeEvent(input$example,{
    handleRandomExample()
  }, ignoreInit = T)
  
  observeEvent(input$clear, {
    handleClearExample()
  }, ignoreInit = T)
  
  observeEvent(input$selectAll,{
    handleSelectAllFiles(input$selectAll)
  }, ignoreInit = T)
  
  observeEvent(input$rename, {
    handlePrepareRename(input$checkboxFiles)
  }, ignoreInit = T)
  
  observeEvent(input$js_fileNames, {
    handleRename(input$js_fileNames)
  }, ignoreInit = T)
  
  observeEvent(input$remove, {
    handleRemove(input$checkboxFiles)
  }, ignoreInit = T)
  
  observeEvent(input$submitUpset, {
    handleSubmitUpset()
  }, ignoreInit = T)
  
  observeEvent(input$mode, {
    handleModeUpset(input$mode)
  }, ignoreInit = T)
  
  observeEvent(input$upsetjs_hover, {
    handleUpsetHover(input$upsetjs_hover)
  }, ignoreInit = T)
  
  observeEvent(input$upsetjs_click,{
    handleUpsetClick(input$mode, input$upsetjs_click)
  }, ignoreInit = T)
  
  observeEvent(input$intersection_ok,{
    handleIntersection(input$upsetjs_click)
  }, ignoreInit = T)
  
  observeEvent(input$distinct_ok,{
    handleDistinct(input$upsetjs_click)
  }, ignoreInit = T)
  
  observeEvent(input$union_ok,{
    handleUnion(input$upsetjs_click)
  }, ignoreInit = T)
  
  observeEvent(input$distinct_per_file_ok,{
    handleDistinctPerFile(input$upsetjs_click)
  }, ignoreInit = T)
  
  observeEvent(input$selectView,{
    handleSelectView(input$selectView)
  }, ignoreInit = T)
  
  # Functional Enrichment ####
  observeEvent(input$functional_enrichment_tool,{
    handleFunctionalEnrichmentToolSelection()
  }, ignoreInit = T)
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_run")]], {
      currentEnrichmentType <<- enrichmentType
      start_time <- proc.time()
      handleEnrichment()
      end_time <- proc.time()
      print((end_time - start_time)[3])
    }, ignoreInit = T)
  })
  
  # Plots ####
  # Networks
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(NETWORK_IDS, function(networkId) {
      observeEvent(input[[paste(enrichmentType, networkId, "sourceSelect", sep = "_")]], {
        handleDatasourcePicker(enrichmentType, networkId)
      }, ignoreInit = T, ignoreNULL = F)
    })
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(NETWORK_IDS, function(networkId) {
      observeEvent(input[[paste(enrichmentType, networkId, "visualizeNetwork", sep = "_")]], {
        handleEnrichmentNetwork(enrichmentType, networkId)
      }, ignoreInit = T)
    })
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(NETWORK_IDS, function(networkId) {
      observeEvent(input[[paste(enrichmentType, networkId, "arena", sep = "_")]], {
        arenaHandler(enrichmentType, networkId)
      }, ignoreInit = T)
    })
  })
  
  # Heatmaps
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(HEATMAP_IDS, function(heatmapId) {
      observeEvent(input[[paste(enrichmentType, heatmapId, "sourceSelect", sep = "_")]], {
        handleDatasourcePicker(enrichmentType, heatmapId)
      }, ignoreInit = T, ignoreNULL = F)
    })
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(HEATMAP_IDS, function(heatmapId) {
      observeEvent(input[[paste(enrichmentType, heatmapId, "visualizeHeatmap", sep = "_")]], {
        handleHeatmap(enrichmentType, heatmapId)
      }, ignoreInit = T)
    })
  })
  
  # Barchart
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_barchart_sourceSelect")]], {
      handleDatasourcePicker(enrichmentType, "barchart")
    }, ignoreInit = T, ignoreNULL = F)
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_barchart_button")]], {
      handleBarchart(enrichmentType)
    }, ignoreInit = T)
  })
  
  # Scatter
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_scatter_sourceSelect")]], {
      handleDatasourcePicker(enrichmentType, "scatter")
    }, ignoreInit = T, ignoreNULL = F)
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_scatter_button")]], {
      handleScatterPlot(enrichmentType)
    }, ignoreInit = T)
  })

  # Manhattan
  observeEvent(input$manhattan_button, {
    # TODO convert to generic manhattan plot outside gprofiler
    handleManhattanPlot()
  }, ignoreInit = T)
  
  observeEvent(event_data("plotly_click"), {
    handleManhattanClick()
  }, ignoreInit = T)
  
  observeEvent(event_data("plotly_selected"), {
    handleManhattanSelect()
  }, ignoreInit = T)
  
  # STRING ####
  observeEvent(input$runStringNetwork, {
    handleStringNetwork()
  }, ignoreInit = T)
  
  # Conversion ####
  observeEvent(input$gconvert_button,{
    handle_gconvert()
  }, ignoreInit = T)
  
  observeEvent(input$gorth_organism,{
    handle_gorthOrganism()
  }, ignoreInit = T)
  
  observeEvent(input$gorth_button,{
    handle_gorth()
  }, ignoreInit = T)
}
