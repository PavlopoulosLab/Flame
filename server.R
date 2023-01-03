function(input, output, session) {
  source("global_variables.R", local = TRUE)
  source("functions/general.R", local = TRUE)
  source("functions/init.R", local = TRUE)
  source("functions/render.R", local = TRUE)
  source("functions/update.R", local = TRUE)
  source("functions/reset.R", local = TRUE)
  
  source("functions/input/main.R", local = TRUE)
  source("functions/input/upset.R", local = TRUE)
  source("functions/input/text_mining.R", local = TRUE)
  source("functions/input/volcano.R", local = TRUE)
  source("functions/input/api.R", local = TRUE)
  
  source("functions/enrichment/inputs_panel.R", local = TRUE)
  source("functions/enrichment/main.R", local = TRUE)
  source("functions/enrichment/general.R", local = TRUE)
  source("functions/enrichment/gprofiler.R", local = TRUE)
  source("functions/enrichment/agotool.R", local = TRUE)
  source("functions/links.R", local = TRUE)
  
  source("functions/plots/general.R", local = TRUE)
  source("functions/plots/networks.R", local = TRUE)
  source("functions/plots/heatmaps.R", local = TRUE)
  source("functions/plots/barchart.R", local = TRUE)
  source("functions/plots/scatter.R", local = TRUE)
  source("functions/plots/manhattan.R", local = TRUE)
  source("functions/plots/arena3d.R", local = TRUE)
  
  source("functions/stringNetwork.R", local = TRUE)
  source("functions/conversion.R", local = TRUE)
  
  source("functions/input/text_mining.R", local = TRUE)

  # API ####
  observeEvent(session$clientData$url_search, {
    resolveAPI()
  })
  
  # START ####
  initializeServerApp()

  # ~Welcome ####
  observeEvent(input$link_to_fileinput, {
    updateTabItems(session, "sideBarId", selected = "file_handler")
  }, ignoreInit = T)
  
  # INPUT ####
  observeEvent(input$example,{
    handleRandomExample()
  }, ignoreInit = T)
  
  observeEvent(input$clear, {
    handleClearText()
  }, ignoreInit = T)
  
  observeEvent(input$textSubmit,{
    handleTextSubmit()
  }, ignoreInit = T)
  
  observeEvent(input$fileUpload,{
    handleInputFiles()
  }, ignoreInit = T)

  observeEvent(input$selectAll,{
    handleSelectAllLists()
  }, ignoreInit = T)
  
  observeEvent(input$rename, {
    handlePrepareRenameLists()
  }, ignoreInit = T)
  
  observeEvent(input$js_listNames, {
    handleRenameLists()
  }, ignoreInit = T)
  
  observeEvent(input$remove, {
    handleRemoveLists()
  }, ignoreInit = T)
  
  observeEvent(input$selectView,{
    handleSelectView()
  }, ignoreInit = T)
  
  # ~Text-mining ####
  observeEvent(input$textmining_addExample, {
    loadTextMiningExample()
  }, ignoreInit = T)

  observeEvent(input$textmining_clearText, {
    resetTextMiningFields()
  }, ignoreInit = T)
  
  observeEvent(input$textmining_submit, {
    handleTextMining()
  }, ignoreInit = T)
  
  observeEvent(input$textmining_addList,{
    addTextMiningToFiles()
  }, ignoreInit = T)  
  
  observeEvent(input$textmining_delete, {
    deleteTextmining()
  }, ignoreInit = T)
  
  # ~Upset ####
  observeEvent(input$submitUpset, {
    handleUpset()
  }, ignoreInit = T)
  
  observeEvent(input$upsetjsView_hover, {
    handleUpsetHover()
  }, ignoreInit = T)
  
  observeEvent(input$upsetjsView_click, {
    handleUpsetClick()
  }, ignoreInit = T)
  
  observeEvent(input$upsetClick_ok, {
    handleUpsetListAccept()
  }, ignoreInit = T)
  
  # ~Volcano ####
  observeEvent(input$volcanoUpload, {
    handleVolcanoPlot(readVolcanoInput)
  }, ignoreInit = T)

  observeEvent(input$volcano_addExample, {
    handleVolcanoPlot(readVolcanoExample)
  }, ignoreInit = T)
  
  observeEvent(input$volcanoRepaint, {
    handleVolcanoRepaint()
  }, ignoreInit = T)
  
  observeEvent(event_data("plotly_selected", source = "Volcano"), {
    triggeredEvent <- event_data("plotly_selected", source = "Volcano")
    volcanoSelectedItems <<- triggeredEvent$customdata
    renderShinyText("volcanoSelected",
                    paste(volcanoSelectedItems, collapse = ", "))
  }, ignoreInit = T)

  observeEvent(input$volcanoSubmit, {
    handleVolcanoSubmit()
  }, ignoreInit = T)

  observeEvent(input$volcano_ok,{
    handleVolcanoListAccept()
  }, ignoreInit = T)
  
  # ENRICHMENT ####
  observeEvent(input$functional_enrichment_tool,{
    handleFunctionalEnrichmentToolSelection()
  }, ignoreInit = T, ignoreNULL = F)
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_run")]], {
      handleEnrichment(enrichmentType)
    }, ignoreInit = T)
  })
  
  # ~Plots ####
  # ~~Networks ####
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      lapply(NETWORK_IDS, function(networkId) {
        observeEvent(input[[paste(enrichmentType, toolName,
                                  networkId, "sourceSelect", sep = "_")]], {
          handleDatasourcePicker(enrichmentType, toolName, networkId)
        }, ignoreInit = T, ignoreNULL = F)
      })
    })
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      lapply(NETWORK_IDS, function(networkId) {
        observeEvent(input[[paste(enrichmentType, toolName,
                                  networkId, "button", sep = "_")]], {
          handleEnrichmentNetwork(enrichmentType, toolName, networkId)
        }, ignoreInit = T)
      })
    })
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      lapply(NETWORK_IDS, function(networkId) {
        observeEvent(input[[paste(enrichmentType, toolName,
                                  networkId, "arena", sep = "_")]], {
          arenaHandler(enrichmentType, toolName, networkId)
        }, ignoreInit = T)
      })
    })
  })
  
  # ~~Heatmaps ####
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      lapply(HEATMAP_IDS, function(heatmapId) {
        observeEvent(input[[paste(enrichmentType, toolName, heatmapId, "sourceSelect", sep = "_")]], {
          handleDatasourcePicker(enrichmentType, toolName, heatmapId)
        }, ignoreInit = T, ignoreNULL = F)
      })
    })
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      lapply(HEATMAP_IDS, function(heatmapId) {
        observeEvent(input[[paste(enrichmentType, toolName,
                                  heatmapId, "button", sep = "_")]], {
          handleHeatmap(enrichmentType, toolName, heatmapId)
        }, ignoreInit = T)
      })
    })
  })
  
  # ~~Barchart ####
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      observeEvent(input[[paste(enrichmentType, toolName, "barchart_sourceSelect", sep = "_")]], {
        handleDatasourcePicker(enrichmentType, toolName, "barchart")
      }, ignoreInit = T, ignoreNULL = F)
    })
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      observeEvent(input[[paste(enrichmentType, toolName,
                                "barchart_button", sep = "_")]], {
        handleBarchart(enrichmentType, toolName)
      }, ignoreInit = T)
    })
  })
  
  # ~~Scatter ####
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      observeEvent(input[[paste(enrichmentType, toolName, "scatterPlot_sourceSelect", sep = "_")]], {
        handleDatasourcePicker(enrichmentType, toolName, "scatterPlot")
      }, ignoreInit = T, ignoreNULL = F)
    })
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      observeEvent(input[[paste(enrichmentType, toolName,
                                "scatterPlot_button", sep = "_")]], {
        handleScatterPlot(enrichmentType, toolName)
      }, ignoreInit = T)
    })
  })

  # ~~Manhattan ####
  observeEvent(input$manhattan_button, {
    # TODO convert to generic manhattan plot outside gprofiler
    handleManhattanPlot()
  }, ignoreInit = T)
  
  observeEvent(event_data("plotly_click", source = "A"), { # "A" = "Manhattan"
    triggeredEvent <- event_data("plotly_click", source = "A")
    if (isEventFromManhattan(triggeredEvent))
      handleManhattanClick(triggeredEvent$key)
  }, ignoreInit = T)
  
  observeEvent(event_data("plotly_selected", source = "A"), {
    triggeredEvent <- event_data("plotly_selected", source = "A")
    if (isEventFromManhattan(triggeredEvent))
      handleManhattanSelect(triggeredEvent$key)
  }, ignoreInit = T)
  
  # STRING ####
  observeEvent(input$runStringNetwork, {
    handleStringNetwork()
  }, ignoreInit = T)
  
  # CONVERSION ####
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
