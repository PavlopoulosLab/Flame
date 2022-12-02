function(input, output, session) {
  source("global_variables.R", local = TRUE)
  source("functions/general.R", local = TRUE)
  source("functions/init.R", local = TRUE)
  source("functions/render.R", local = TRUE)
  source("functions/update.R", local = TRUE)
  source("functions/reset.R", local = TRUE)
  
  source("functions/fileInput.R", local = TRUE)
  source("functions/functionalEnrichment.R", local = TRUE)
  source("functions/links.R", local = TRUE)
  
  source("functions/plots/general.R", local = TRUE)
  source("functions/plots/manhattan.R", local = TRUE)
  source("functions/plots/scatter.R", local = TRUE)
  source("functions/plots/barchart.R", local = TRUE)
  source("functions/plots/heatmaps.R", local = TRUE)
  source("functions/plots/networks.R", local = TRUE)
  source("functions/interoperability.R", local = TRUE)
  
  source("functions/conversion.R", local = TRUE)
  source("functions/aGotool.R", local = TRUE) 
  source("functions/literatureEnrichment.R", local = TRUE)
  source("functions/stringNetwork.R", local = TRUE)
  
  # Start ####
  initializeServerApp()
  
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
      session$sendCustomMessage("handler_alert", paste("Problem with url query handling: ", e, sep=""))
    })
  })
  
  observeEvent(input$link_to_fileinput, {
    updateTabItems (session, "sideBarId", selected ="file_handler")
  }, ignoreInit = T)
  
  # File Input - Upload Files ####
  
  observeEvent(input$files,{
    handleInputFiles(input$files, session)
  }, ignoreInit = T)
  
  observeEvent(input$textSubmit,{
    handleTextSubmit(input$text,"gene_list", session)
  }, ignoreInit = T)
  
  observeEvent(input$example,{
    handleRandomExample(session)
  }, ignoreInit = T)
  
  observeEvent(input$clear, {
    handleClearExample(session)
  }, ignoreInit = T)
  
  observeEvent(input$selectAll,{
    handleSelectAllFiles(input$selectAll, session)
  }, ignoreInit = T)
  
  observeEvent(input$rename, {
    handlePrepareRename(input$checkboxFiles, session)
  }, ignoreInit = T)
  
  observeEvent(input$js_fileNames,{
    handleRename(input$js_fileNames, session)
  }, ignoreInit = T)
  
  observeEvent(input$remove,{
    handleRemove(input$checkboxFiles, session)
  }, ignoreInit = T)
  
  observeEvent(input$submitUpset,{
    handleSubmitUpset(input$checkboxFiles, input$mode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$mode,{
    handleModeUpset(input$mode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$upsetjs_hover, {
    handleUpsetHover(input$upsetjs_hover, output)
  }, ignoreInit = T)
  
  observeEvent(input$upsetjs_click,{
    handleUpsetClick(input$mode, input$upsetjs_click)
  }, ignoreInit = T)
  
  observeEvent(input$intersection_ok,{
    handleIntersection(input$upsetjs_click, session)
  }, ignoreInit = T)
  
  observeEvent(input$distinct_ok,{
    handleDistinct(input$upsetjs_click, session)
  }, ignoreInit = T)
  
  observeEvent(input$union_ok,{
    handleUnion(input$upsetjs_click, session)
  }, ignoreInit = T)
  
  observeEvent(input$distinct_per_file_ok,{
    handleDistinctPerFile(input$upsetjs_click, session)
  }, ignoreInit = T)
  
  # File Input - View Data ####
  
  observeEvent(input$selectView,{
    handleSelectView(input$selectView, output, session)
  }, ignoreInit = T)
  
  # Functional Enrichment ####
  observeEvent(input$gprofiler,{
      start_time <- proc.time()
      handleFunctionalEnrichment()
      end_time <- proc.time()
      print((end_time - start_time)[3])
  }, ignoreInit = T)
  
  # Plots ####
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
  
  # Scatter
  observeEvent(input$scatter_sourceSelect, {
    handleDatasourcePicker("scatter")
  }, ignoreInit = T, ignoreNULL = F)
  
  observeEvent(input$scatter_button, {
    handleScatterPlot()
  }, ignoreInit = T)
  
  # Barchart
  observeEvent(input$barchart_sourceSelect, {
    handleDatasourcePicker("barchart")
  }, ignoreInit = T, ignoreNULL = F)
  
  observeEvent(input$barchart_button, {
    handleBarchart()
  }, ignoreInit = T)
  
  # Heatmaps
  lapply(heatmapIds, function(heatmapId) {
    observeEvent(input[[paste0(heatmapId, "_sourceSelect")]], {
      handleDatasourcePicker(heatmapId)
    }, ignoreInit = T, ignoreNULL = F)
  })
  
  lapply(heatmapIds, function(heatmapId) {
    observeEvent(input[[paste0(heatmapId, "_visualizeHeatmap")]], {
      handleHeatmap(heatmapId)
    }, ignoreInit = T)
  })
  
  # Networks
  lapply(networkIds, function(networkId) {
    observeEvent(input[[paste0(networkId, "_sourceSelect")]], {
      handleDatasourcePicker(networkId)
    }, ignoreInit = T, ignoreNULL = F)
  })
  
  lapply(networkIds, function(networkId) {
    observeEvent(input[[paste0(networkId, "_visualizeNetwork")]], {
      handleEnrichmentNetwork(networkId)
    }, ignoreInit = T)
  })
  
  lapply(networkIds, function(networkId) {
    observeEvent(input[[paste0(networkId, "_arena")]], {
      arenaHandler(networkId)
    }, ignoreInit = T)
  })
  

  # observeEvent(input$scatter_select,{
  #   handleScatterSelect(input$scatter_select, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$scatter_slider,{
  #   handleScatterPlot(input$scatter_select, input$scatter_slider, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$barchart_sourceSelect,{
  #   handleBarSelect2(input$barchart_sourceSelect, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$barchart_mode,{
  #   handleBarPlot2(input$barchart_sourceSelect, input$barchart_slider, input$barchart_mode, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$barchart_slider,{
  #   handleBarPlot2(input$barchart_sourceSelect, input$barchart_slider, input$barchart_mode, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$heatmap1_sourceSelect,{
  #   handleHeatmapSelect(input$heatmap1_sourceSelect, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$heatmap1_slider,{
  #   handleHeatMap(input$heatmap1_sourceSelect,input$heatmap1_slider, input$heatmap1_axis, input$heatmap1_mode, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$heatmap1_axis,{
  #   handleHeatMap(input$heatmap1_sourceSelect,input$heatmap1_slider, input$heatmap1_axis, input$heatmap1_mode, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$heatmap1_mode,{
  #   handleHeatMap(input$heatmap1_sourceSelect,input$heatmap1_slider, input$heatmap1_axis, input$heatmap1_mode, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$heatmap2_sourceSelect,{
  #   handleHeatmapSelect2(input$heatmap2_sourceSelect, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$heatmap2_slider,{
  #   handleHeatMap2(input$heatmap2_sourceSelect, input$heatmap2_slider, input$heatmap2_mode, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$heatmap2_mode,{
  #   handleHeatMap2(input$heatmap2_sourceSelect,input$heatmap2_slider, input$heatmap2_mode, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network1_sourceSelect,{
  #   handleNetworkSelect(input$network1_sourceSelect, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network1_mode,{
  #   handleNetwork1_old(input$network1_sourceSelect, input$network1_slider, input$network1_mode, output, session)
  # }, ignoreInit = T)
  # 
  # 
  # observeEvent(input$network1_slider, {
  #   tryCatch({
  #     handleNetwork1_old(input$network1_sourceSelect, input$network1_slider, input$network1_mode, output, session)
  #   }, warning = function(w) {
  #     print(paste("Warning:  ", w))
  #   }, error = function(e) {
  #     print(paste("Error :  ", e))
  #     session$sendCustomMessage("handler_alert", paste("Problem while producing network: ", e, sep=""))
  #   }, finally = {
  #     # session$sendCustomMessage("handler_enableAllButtons", T)
  #   })
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network2_sourceSelect,{
  #   handleNetworkSelect2(input$network2_sourceSelect, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network2_mode,{
  #   handleNetwork2_old(input$network2_sourceSelect, input$network2_slider, input$network2_mode,  input$network2_thresholdSlider, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network2_thresholdSlider,{
  #   handleNetwork2_old(input$network2_sourceSelect, input$network2_slider, input$network2_mode,  input$network2_thresholdSlider, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network2_slider, {
  #   tryCatch({
  #     handleNetwork2_old(input$network2_sourceSelect, input$network2_slider, input$network2_mode,  input$network2_thresholdSlider, output, session)
  #   }, warning = function(w) {
  #     print(paste("Warning:  ", w))
  #   }, error = function(e) {
  #     print(paste("Error :  ", e))
  #     session$sendCustomMessage("handler_alert", paste("Problem while producing network: ", e, sep=""))
  #   }, finally = {
  #     # session$sendCustomMessage("handler_enableAllButtons", T)
  #   })
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network3_sourceSelect,{
  #   handleNetworkSelect3(input$network3_sourceSelect, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network3_slider,{
  #   handleNetworkSlider3(input$network3_slider,input$network3_sourceSelect, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network3_mode,{
  #   handleNetwork3_old(input$network3_sourceSelect, input$network3_slider, input$network3_mode,  input$network3_thresholdSlider, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network3_thresholdSlider,{
  #   handleNetwork3_old(input$network3_sourceSelect, input$network3_slider, input$network3_mode,  input$network3_thresholdSlider, output, session)
  # }, ignoreInit = T)
  # 
  # observeEvent(input$network3_slider, {
  #   tryCatch({
  #     handleNetwork3_old(input$network3_sourceSelect, input$network3_slider, input$network3_mode,  input$network3_thresholdSlider, output, session)
  #   }, warning = function(w) {
  #     print(paste("Warning:  ", w))
  #   }, error = function(e) {
  #     print(paste("Error :  ", e))
  #     session$sendCustomMessage("handler_alert", paste("Problem while producing network (Gene Vs Gene): ", e, sep=""))
  #   }, finally = {
  #     # session$sendCustomMessage("handler_enableAllButtons", T)
  #   })
  # }, ignoreInit = T)
  
  # aGoTool ####
  observeEvent(input$aGOtool,{
    tryCatch({
      handleAGotool(input$aGOtoolSelect, input$aGoCorrectionMethod, input$aGOtoolOrganism,input$aGOtoolDatasources,
                    input$aGOtoolPvalue,input$gconvertTargetGotool,input$aGoBarSelect2,
                    input$aGoScatterSelect, input$aGoNetworkSelect, input$aGoNetworkSelect2, input$aGoNetworkSelect3, 
                    input$aGoHeatmapSelect, input$aGoHeatmapSelect2, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem with functional enrichment analysis (aGotool): ", e, sep=""))
    }, finally = {
      session$sendCustomMessage("handler_finishLoader", 12)
      
      session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  ### PLOTS FOR aGOtool RESULTS#####
  
  ### SCATTER aGO ###
  
  observeEvent(input$aGoScatterSelect,{
    handleaGoScatterSelect(input$aGoScatterSelect, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoSliderScatter,{
    handleaGoScatterPlot(input$aGoScatterSelect, input$aGoSliderScatter, output, session)
  }, ignoreInit = T)
  
  ### BARPLOT aGO ###
  
  observeEvent(input$aGoBarSelect2,{
    handleaGoBarSelect2(input$aGoBarSelect2, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoBarplotMode,{
    handleaGoBarPlot2(input$aGoBarSelect2, input$aGoSliderBarplot, input$aGoBarplotMode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoSliderBarplot,{
    handleaGoBarPlot2(input$aGoBarSelect2, input$aGoSliderBarplot, input$aGoBarplotMode, output, session)
  }, ignoreInit = T)
  
  ### HEATMAP aGO ##
  
  observeEvent(input$aGoHeatmapSelect,{
    handleaGoHeatmapSelect(input$aGoHeatmapSelect, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoSliderHeatmap,{
    tryCatch({
      handleaGoHeatMap(input$aGoHeatmapSelect,input$aGoSliderHeatmap, input$aGoHeatmapAxis, input$aGoHeatmapMode, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing heatmap1: ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  observeEvent(input$aGoHeatmapAxis,{
    handleaGoHeatMap(input$aGoHeatmapSelect,input$aGoSliderHeatmap, input$aGoHeatmapAxis, input$aGoHeatmapMode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoHeatmapMode,{
    handleaGoHeatMap(input$aGoHeatmapSelect,input$aGoSliderHeatmap, input$aGoHeatmapAxis, input$aGoHeatmapMode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoHeatmapSelect2,{
    handleaGoHeatmapSelect2(input$aGoHeatmapSelect2, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoSliderHeatmap2,{
    tryCatch({
      handleaGoHeatMap2(input$aGoHeatmapSelect2, input$aGoSliderHeatmap2, input$aGoHeatmapMode2, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing heatmap: ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  observeEvent(input$aGoHeatmapMode2,{
    handleaGoHeatMap2(input$aGoHeatmapSelect2, input$aGoSliderHeatmap2, input$aGoHeatmapMode2, output, session)
  }, ignoreInit = T)
  
  
  ### NETWORK aGO ##
  observeEvent(input$aGoNetworkSelect,{
    handleaGoNetworkSelect(input$aGoNetworkSelect, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoNetworkMode,{
    handleaGoNetwork(input$aGoNetworkSelect, input$aGoSliderNetwork, input$aGoNetworkMode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoSliderNetwork, {
    tryCatch({
      handleaGoNetwork(input$aGoNetworkSelect, input$aGoSliderNetwork, input$aGoNetworkMode, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing network: ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  observeEvent(input$aGoNetworkSelect2,{
    handleaGoNetworkSelect2(input$aGoNetworkSelect2, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoNetworkMode2,{
    handleaGoNetwork2(input$aGoNetworkSelect2, input$aGoSliderNetwork2, input$aGoNetworkMode2,  input$aGoSliderThreshold, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoSliderThreshold,{
    handleaGoNetwork2(input$aGoNetworkSelect2, input$aGoSliderNetwork2, input$aGoNetworkMode2,  input$aGoSliderThreshold, output, session)
  }, ignoreInit = T) 
  
  observeEvent(input$aGoSliderNetwork2, {
    tryCatch({
      handleaGoNetwork2(input$aGoNetworkSelect2, input$aGoSliderNetwork2, input$aGoNetworkMode2,  input$aGoSliderThreshold, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing network: ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  observeEvent(input$aGoNetworkSelect3,{
    handleaGoNetworkSelect3(input$aGoNetworkSelect3, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoSliderNetwork3,{
    handleaGoNetworkSlider3(input$aGoSliderNetwork3,input$aGoNetworkSelect3, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoNetworkMode3,{
    handleaGoNetwork3(input$aGoNetworkSelect3, input$aGoSliderNetwork3, input$aGoNetworkMode3,  input$aGoSliderThreshold3, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$aGoSliderThreshold3,{
    handleaGoNetwork3(input$aGoNetworkSelect3, input$aGoSliderNetwork3, input$aGoNetworkMode3,  input$aGoSliderThreshold3, output, session)
  }, ignoreInit = T) 
  
  observeEvent(input$aGoSliderNetwork3, {
    tryCatch({
      handleaGoNetwork3(input$aGoNetworkSelect3, input$aGoSliderNetwork3, input$aGoNetworkMode3,  input$aGoSliderThreshold3, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing network (Gene Vs Gene): ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  })
  
  observeEvent(input$ago_network_arena, {
    tryCatch({
      showModal(modalDialog(HTML("<h2>Please wait.</h2>
                                 <p>Building network for Arena3Dweb</p>"), footer = NULL))
      arenaHandler(arena_ago_edgelist, input$aGoNetworkSelect)
    }, error = function(e) {
      print(paste("Error while preparing JSON for Arena: ", e))
      shinyalert("Error!", "There was an error with sending the network to Arena3Dweb.", type = "error")
    }, finally = removeModal())
  }, ignoreInit = T)
  
  #Literature Enrichment####
  
  observeEvent(input$literature,{
    tryCatch({
      handleLiteratureEnrich(input$literatureSelect, input$literatureCorrectionMethod, input$literatureOrganism,
                             input$literaturePvalue, input$gconvertTargetLiterature, input$literatureSliderScatter,
                             input$literatureSliderBarplot, input$literatureSliderHeatmap, input$literatureSliderHeatmap2, 
                             input$literatureSliderNetwork,input$literatureSliderNetwork2, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem with functional enrichment analysis (Literature Enrichment): ", e, sep=""))
    }, finally = {
      session$sendCustomMessage("handler_finishLoader", 13)
      
      #   session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  observeEvent(input$literatureSliderScatter,{
    handleLitearureScatterPlot(input$literatureSliderScatter, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$literatureSliderBarplot,{
    handleLiteratureBarPlot2(input$literatureSliderBarplot, input$literatureBarplotMode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$literatureBarplotMode,{
    handleLiteratureBarPlot2(input$literatureSliderBarplot, input$literatureBarplotMode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$literatureSliderHeatmap,{
    tryCatch({
      handleLiteratureHeatMap(input$literatureSliderHeatmap, input$literatureHeatmapAxis, input$literatureHeatmapMode, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing heatmap (Literature Functions Vs Genes Heatmap): ", e, sep=""))
    }, finally = {
      # TODO loading bar remove, after it has been implemented
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  observeEvent(input$literatureHeatmapAxis,{
    handleLiteratureHeatMap(input$literatureSliderHeatmap, input$literatureHeatmapAxis, input$literatureHeatmapMode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$literatureHeatmapMode,{
    handleLiteratureHeatMap(input$literatureSliderHeatmap, input$literatureHeatmapAxis, input$literatureHeatmapMode, output, session)
  }, ignoreInit = T)
  
  
  observeEvent(input$literatureSliderHeatmap2,{
    tryCatch({
      handleLiteratureHeatMap2( input$literatureSliderHeatmap2, input$literatureHeatmapMode2, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing heatmap: ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  observeEvent(input$literatureHeatmapMode2,{
    handleLiteratureHeatMap2( input$literatureSliderHeatmap2, input$literatureHeatmapMode2, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$literatureNetworkMode,{
    handleLiteratureNetwork( input$literatureSliderNetwork, input$literatureNetworkMode, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$literatureSliderNetwork, {
    tryCatch({
      handleLiteratureNetwork( input$literatureSliderNetwork, input$literatureNetworkMode, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing network: ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  observeEvent(input$literatureNetworkMode2,{
    handleLiteratureNetwork2( input$literatureSliderNetwork2, input$literatureNetworkMode2,  input$literatureSliderThreshold, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$literatureSliderThreshold,{
    handleLiteratureNetwork2( input$literatureSliderNetwork2, input$literatureNetworkMode2,  input$literatureSliderThreshold, output, session)
  }, ignoreInit = T) 
  
  observeEvent(input$literatureSliderNetwork2, {
    tryCatch({
      handleLiteratureNetwork2( input$literatureSliderNetwork2, input$literatureNetworkMode2,  input$literatureSliderThreshold, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing network: ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  ### Network LIT 3
  
  observeEvent(input$literatureSliderNetwork3,{
    handleLiteratureNetworkSlider3(input$literatureSliderNetwork3,session)
  }, ignoreInit = T)
  
  observeEvent(input$literatureNetworkMode3,{
    handleLiteratureNetwork3( input$literatureSliderNetwork3, input$literatureNetworkMode3,  input$literatureSliderThreshold3, output, session)
  }, ignoreInit = T)
  
  observeEvent(input$literatureSliderThreshold3,{
    handleLiteratureNetwork3( input$literatureSliderNetwork3, input$literatureNetworkMode3,  input$literatureSliderThreshold3, output, session)
  }, ignoreInit = T) 
  
  observeEvent(input$literatureSliderNetwork3, {
    tryCatch({
      handleLiteratureNetwork3( input$literatureSliderNetwork3, input$literatureNetworkMode3,  input$literatureSliderThreshold3, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing network: ", e, sep=""))
    }, finally = {
      
    })
  }, ignoreInit = T)
  
  observeEvent(input$lit_network_arena, {
    tryCatch({
      showModal(modalDialog(HTML("<h2>Please wait.</h2>
                                 <p>Building network for Arena3Dweb</p>"), footer = NULL))
      arenaHandler(arena_lit_edgelist, "Literature")
    }, error = function(e) {
      print(paste("Error while preparing JSON for Arena: ", e))
      shinyalert("Error!", "There was an error with sending the network to Arena3Dweb.", type = "error")
    }, finally = removeModal())
  }, ignoreInit = T)
  
  #Protein-Protein Network (STRING)-####
  observeEvent(input$runStringNetwork, {
    session$sendCustomMessage("handler_startLoader", c(29,10))
    dataset=input$STRINGnetworkSelect
    organism = input$STRINGnetworkOrganism
    type=input$STRINGnetworkType
    edges=input$STRINGnetworkEdges
    score=input$STRINGnetworkScore
    
    create_string_network(dataset, organism, type, edges, score, output)
    session$sendCustomMessage("handler_startLoader", c(29,100))
    session$sendCustomMessage("handler_finishLoader", 29)
  }, ignoreInit = T)
  
  # Conversion ####
  
  observeEvent(input$gconvert_button,{
    handle_gconvert(input$gconvert_select, input$gconvert_organism, input$gconvert_target, output)
  }, ignoreInit = T)
  
  observeEvent(input$gorth_button,{
    tryCatch({
      handle_gorth(input$gorth_select, input$gorth_organism, input$gorth_target, output)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem with conversion (Orthology Search): ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  }, ignoreInit = T)
  
  observeEvent(input$gorth_organism,{
    handle_gorthOrganism(input$gorth_organism, session)
  }, ignoreInit = T)
}
