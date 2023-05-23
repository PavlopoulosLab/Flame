function(input, output, session) {
  source("config/global_settings.R", local = T)
  source("config/global_variables.R", local = T)
  source("config/server_variables.R", local = T)
  source("config/static_variables.R", local = T)
  source("config/ui_variables.R", local = T)
  
  source("functions/general.R", local = T)
  source("functions/init.R", local = T)
  source("functions/render.R", local = T)
  source("functions/update.R", local = T)
  source("functions/reset.R", local = T)
  
  source("functions/input/main.R", local = T)
  source("functions/input/upset.R", local = T)
  source("functions/input/text_mining.R", local = T)
  source("functions/input/snps.R", local = T)
  source("functions/input/volcano.R", local = T)
  source("functions/input/api.R", local = T)
  source("functions/input/conversion.R", local = T)
  
  source("functions/enrichment/inputs_panel.R", local = T)
  source("functions/enrichment/main.R", local = T)
  source("functions/enrichment/general.R", local = T)
  source("functions/enrichment/agotool.R", local = T)
  source("functions/enrichment/gprofiler.R", local = T)
  source("functions/enrichment/webgestalt.R", local = T)
  source("functions/enrichment/enrichr.R", local = T)
  source("functions/enrichment/combination.R", local = T)
  source("functions/links.R", local = T)
  
  source("functions/plots/general.R", local = T)
  source("functions/plots/networks.R", local = T)
  source("functions/plots/heatmaps.R", local = T)
  source("functions/plots/barchart.R", local = T)
  source("functions/plots/scatter.R", local = T)
  source("functions/plots/manhattan.R", local = T)
  source("functions/plots/arena3d.R", local = T)
  
  source("functions/stringNetwork.R", local = T)
  source("functions/conversion.R", local = T)
  

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
  observeEvent(input$example, {
    handleRandomExample()
  }, ignoreInit = T)
  
  observeEvent(input$input_clear, {
    handleClearText()
  }, ignoreInit = T)
  
  observeEvent(input$text_submit, {
    handleTextSubmit()
  }, ignoreInit = T)
  
  observeEvent(input$fileUpload, {
    handleInputFiles()
  }, ignoreInit = T)

  observeEvent(input$selectAll, {
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
  
  observeEvent(input$selectView, {
    handleSelectView()
  }, ignoreInit = T)
  
  # ~Text-mining ####
  observeEvent(input$textmining_addExample, {
    loadTextMiningExample()
  }, ignoreInit = T)

  observeEvent(input$textmining_clear, {
    resetTextMiningFields()
  }, ignoreInit = T)
  
  observeEvent(input$textmining_submit, {
    handleTextMining()
  }, ignoreInit = T)

  observeEvent(input$textmining_selectAll, {
    shinyjs::runjs("textmining_selectAll(true);")
    
  }, ignoreInit = T)  
    
  observeEvent(input$textmining_selectNone, {
    shinyjs::runjs("textmining_selectAll(false);")
    
  }, ignoreInit = T)  
  
  observeEvent(input$textmining_addList, {
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

  # ~SNPs ####
  observeEvent(input$snp_example, {
    loadVariantExample()
  }, ignoreInit = T)
  
  observeEvent(input$snp_clear, {
    resetVariantFields()
  }, ignoreInit = T)
  
  observeEvent(input$snp_submit, {
    handleVariantSubmit()
  }, ignoreInit = T)
  
  observeEvent(input$snp_fileUpload, {
    handleVariantUpload()
  }, ignoreInit = T)
  
  observeEvent(input$snp_addList, {
    addVariantsToFiles()
  }, ignoreInit = T)  
  
  observeEvent(input$snp_delete, {
    deleteVariants()
  }, ignoreInit = T)
  
  # ~Volcano ####
  observeEvent(input$volcanoUpload, {
    handleVolcanoPlot(readVolcanoInput)
  }, ignoreInit = T)

  observeEvent(input$volcano_addExample, {
    handleVolcanoPlot(readVolcanoExample)
  }, ignoreInit = T)
  
  observeEvent(c(input$volcano_pvalue_slider, input$volcano_fc_slider), {
    updateVolcanoMetricsConversionText(input$volcano_pvalue_slider,
                                       input$volcano_fc_slider)
  }, ignoreInit = T)
  
  observeEvent(input$volcanoRedraw, {
    handleVolcanoRedraw()
  }, ignoreInit = T)
  
  observeEvent(event_data("plotly_selected", source = "Volcano"), {
    triggeredEvent <- event_data("plotly_selected", source = "Volcano")
    volcanoSelectedItems <<- triggeredEvent$customdata
    renderShinyText("volcanoSelected",
                    paste(volcanoSelectedItems, collapse = ", "))
  }, ignoreInit = T)

  observeEvent(input$volcano_submit, {
    handleVolcanoSubmit()
  }, ignoreInit = T)

  observeEvent(input$volcano_ok, {
    handleVolcanoListAccept()
  }, ignoreInit = T)
  
  # ENRICHMENT ####
  observeEvent(input$functional_enrichment_organism, {
    handleFunctionalEnrichmentOrganismSelection()
  }, ignoreInit = T)
  
  observeEvent(input$functional_enrichment_tool, {
    handleFunctionalEnrichmentToolSelection()
  }, ignoreInit = T, ignoreNULL = F)

  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_file")]], {
      handleBackgroundListUpdate(enrichmentType)
    }, ignoreInit = T)
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_background_choice")]], {
      choice <- input[[paste0(enrichmentType, "_enrichment_background_choice")]]
      handleBackgroundModeSelection(choice, enrichmentType)
    }, ignoreInit = T)
  })
    
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_run")]], {
      handleEnrichment(enrichmentType)
    }, ignoreInit = T)
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      observeEvent(input[[
        paste(enrichmentType, toolName, "clear", sep = "_")]], {
          handleEnrichmentResultClear(enrichmentType, toolName)
        }, ignoreInit = T)
    })
  })
  
  # ~Combination ####
  observeEvent(input$combo_datasources, {
      handleComboSourceSelect()
    }, ignoreInit = T, ignoreNULL = T)
  
  observeEvent(input$upsetjsCombo_click, {
    handleComboUpsetClick()
  }, ignoreInit = T)
  
  observeEvent(input$combo_visNetwork_run, {
    handleComboNetwork()
  }, ignoreInit = T)
  
  # ~Plots ####
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    lapply(ENRICHMENT_TOOLS, function(toolName) {
      lapply(ALL_PLOT_IDS, function(plotId) {
        observeEvent(input[[
          paste(enrichmentType, toolName, plotId, "sourceSelect", sep = "_")]], {
            handleDatasourcePicker(enrichmentType, toolName, plotId)
          }, ignoreInit = T, ignoreNULL = F)
      })
    })
  })
  
  # ~~Networks ####
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
      observeEvent(input[[paste(enrichmentType, toolName,
                                "barchart_button", sep = "_")]], {
        handleBarchart(enrichmentType, toolName)
      }, ignoreInit = T)
    })
  })
  
  # ~~Scatter ####
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
  observeEvent(input$string_network_organism, {
    handleStringOrganismSelection()
  }, ignoreInit = T)
  
  observeEvent(input$runStringNetwork, {
    handleStringNetwork()
  }, ignoreInit = T)
  
  # CONVERSION ####
  observeEvent(input$gconvert_button, {
    handle_gconvert()
  }, ignoreInit = T)
  
  observeEvent(input$gorth_organism, {
    handle_gorthOrganism()
  }, ignoreInit = T)
  
  observeEvent(input$gorth_button, {
    handle_gorth()
  }, ignoreInit = T)
  
  observeEvent(input$gconvert_addList, {
    dType <- input$gconvert_dType
    addConversionResultToInput("gconvert", dType)
  }, ignoreInit = T)
  
  observeEvent(input$gorth_addList, {
    dType <- input$gorth_dType
    addConversionResultToInput("gorth", dType)
  }, ignoreInit = T)
}
