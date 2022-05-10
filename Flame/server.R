function(input, output, session) {
  source("global.R", local=TRUE) # public static variables
  source("R_functions/fileInput.R", local=TRUE) # File Input tab functions
  source("R_functions/functionalEnrichment.R", local=TRUE) # Functional Enrichment tab functions
  source("R_functions/plots.R", local=TRUE) # Plots tab functions
  source("R_functions/conversion.R", local=TRUE) # Conversion tab functions
  source("R_functions/aGotool.R", local=TRUE) # aGotool tab functions
  source("R_functions/literatureEnrichment.R", local=TRUE) # literatureEnrichment tab functions
  source("R_functions/stringNetwork.R", local=TRUE)
  
  # CONFIG
  # set_base_url("http://biit.cs.ut.ee/gprofiler_archive3/e102_eg49_p15") # for gprofiler bug
  
  output$url_checker <- renderText({ # this component needs to be in landing page in order to be observed on page load
    tryCatch({
      
      query <- parseQueryString(session$clientData$url_search)
      if (length(query$f) > 0){ # GET json file from POST request
        updateTabItems (session, "A", selected ="file_handler") # change tab to file input
        
        error_flag <- parse_import_data(paste0(POST_REQUEST_PATH, query$f), session)
        if (!error_flag) output$url_checked <- renderText({"Gene list(s) loaded from url."})
        
        paste("") # empty string to not print anything on landing page
      } else if (length(query$url_genes) > 0){ # GET request with written gene lists
        updateTabItems (session, "A", selected ="file_handler") # change tab to file input
        
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
    updateTabItems (session, "A", selected ="file_handler")
  })
  
  # File Input - Upload Files ####
  
  observeEvent(input$files,{
    handleInputFiles(input$files, session)
  })
  
  observeEvent(input$textSubmit,{
    handleTextSubmit(input$text,"gene_list", session)
  })
  
  observeEvent(input$example,{
    handleRandomExample(session)
  })
  
  observeEvent(input$clear, {
    handleClearExample(session)
  })
  
  observeEvent(input$selectAll,{
    handleSelectAllFiles(input$selectAll, session)
  })
  
  observeEvent(input$rename, {
    handlePrepareRename(input$checkboxFiles, session)
  })
  
  observeEvent(input$js_fileNames,{
    handleRename(input$js_fileNames, session)
  })
  
  observeEvent(input$remove,{
    handleRemove(input$checkboxFiles, session)
  })
  
  observeEvent(input$submitUpset,{
    handleSubmitUpset(input$checkboxFiles, input$mode, output, session)
  })
  
  observeEvent(input$mode,{
    handleModeUpset(input$mode, output, session)
  })
  
  observeEvent(input$upsetjs_hover, {
    handleUpsetHover(input$upsetjs_hover, output)
  })
  
  observeEvent(input$upsetjs_click,{
    handleUpsetClick(input$mode, input$upsetjs_click)
  })
  
  observeEvent(input$intersection_ok,{
    handleIntersection(input$upsetjs_click, session)
  })
  
  observeEvent(input$distinct_ok,{
    handleDistinct(input$upsetjs_click, session)
  })
  
  observeEvent(input$union_ok,{
    handleUnion(input$upsetjs_click, session)
  })
  
  observeEvent(input$distinct_per_file_ok,{
    handleDistinctPerFile(input$upsetjs_click, session)
  })
  
  # File Input - View Data ####
  
  observeEvent(input$selectView,{
    handleSelectView(input$selectView, output, session)
  })
  
  # Functional Enrichment ####
  
  session$sendCustomMessage("handler_disableSourcesTabs", T) # disable all source tab panels
  
  observeEvent(input$gprofiler,{
    tryCatch({
      handleEnrichment(input$selectEnrichFile, input$threshold, input$organism,input$datasources, input$user_pvalue,output,
                       input$barSelect2, input$scatterSelect,input$heatmapSelect,input$heatmapSelect2, input$networkSelect, 
                       input$networkSelect2, input$networkSelect3,input$gconvertTargetGprofiler,  session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem with functional enrichment analysis: ", e, sep=""))
    }, finally = {
      session$sendCustomMessage("handler_finishLoader", 2) # gprofiler
      session$sendCustomMessage("handler_enableAllButtons", T)
    })
  })
  
  # Plots ####
  
  observeEvent(event_data("plotly_click"),{
    currentTermID <- event_data("plotly_click")$key
    if (!identical(currentTermID, NULL)) handleManhattanClick(output,currentTermID)  
  }) 
  
  observeEvent(event_data("plotly_selected"),{
    currentTermIDs <- event_data("plotly_selected")$key
    if (!identical(currentTermIDs, NULL)) handleManhattanSelect(output,currentTermIDs)  
  }) 
  
  observeEvent(input$scatterSelect,{
    handleScatterSelect(input$scatterSelect, session)
  })
  
  observeEvent(input$sliderScatter,{
    handleScatterPlot(input$scatterSelect, input$sliderScatter, output, session)
  })
  
  observeEvent(input$barSelect2,{
    handleBarSelect2(input$barSelect2, session)
  })
  
  observeEvent(input$barplotMode,{
    handleBarPlot2(input$barSelect2, input$sliderBarplot, input$barplotMode, output, session)
  })
  
  observeEvent(input$sliderBarplot,{
    handleBarPlot2(input$barSelect2, input$sliderBarplot, input$barplotMode, output, session)
  })
  
  observeEvent(input$heatmapSelect,{
    handleHeatmapSelect(input$heatmapSelect, session)
  })
  
  observeEvent(input$sliderHeatmap,{
    handleHeatMap(input$heatmapSelect,input$sliderHeatmap, input$heatmapAxis, input$heatmapMode, output, session)
  })
  
  observeEvent(input$heatmapAxis,{
    handleHeatMap(input$heatmapSelect,input$sliderHeatmap, input$heatmapAxis, input$heatmapMode, output, session)
  })
  
  observeEvent(input$heatmapMode,{
    handleHeatMap(input$heatmapSelect,input$sliderHeatmap, input$heatmapAxis, input$heatmapMode, output, session)
  })
  
  observeEvent(input$heatmapSelect2,{
    handleHeatmapSelect2(input$heatmapSelect2, session)
  })
  observeEvent(input$sliderHeatmap2,{
    handleHeatMap2(input$heatmapSelect2, input$sliderHeatmap2, input$heatmapMode2, output, session)
  })
  
  observeEvent(input$heatmapMode2,{
    handleHeatMap2(input$heatmapSelect2,input$sliderHeatmap2, input$heatmapMode2, output, session)
  })
  
  observeEvent(input$networkSelect,{
    handleNetworkSelect(input$networkSelect, session)
  })
  
  observeEvent(input$networkMode,{
    handleNetwork(input$networkSelect, input$sliderNetwork, input$networkMode, output, session)
  })
  
  observeEvent(input$sliderNetwork, {
    tryCatch({
      handleNetwork(input$networkSelect, input$sliderNetwork, input$networkMode, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing network: ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  })
  
  observeEvent(input$networkSelect2,{
    handleNetworkSelect2(input$networkSelect2, session)
  })
  
  observeEvent(input$networkMode2,{
    handleNetwork2(input$networkSelect2, input$sliderNetwork2, input$networkMode2,  input$sliderThreshold, output, session)
  })
  
  observeEvent(input$sliderThreshold,{
    handleNetwork2(input$networkSelect2, input$sliderNetwork2, input$networkMode2,  input$sliderThreshold, output, session)
  }) 
  
  observeEvent(input$sliderNetwork2, {
    tryCatch({
      handleNetwork2(input$networkSelect2, input$sliderNetwork2, input$networkMode2,  input$sliderThreshold, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing network: ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  })
  
  observeEvent(input$networkSelect3,{
    handleNetworkSelect3(input$networkSelect3, session)
  })
  
  observeEvent(input$sliderNetwork3,{
    handleNetworkSlider3(input$sliderNetwork3,input$networkSelect3, session)
  })
  
  observeEvent(input$networkMode3,{
    handleNetwork3(input$networkSelect3, input$sliderNetwork3, input$networkMode3,  input$sliderThreshold3, output, session)
  })
  
  observeEvent(input$sliderThreshold3,{
    handleNetwork3(input$networkSelect3, input$sliderNetwork3, input$networkMode3,  input$sliderThreshold3, output, session)
  }) 
  
  observeEvent(input$sliderNetwork3, {
    tryCatch({
      handleNetwork3(input$networkSelect3, input$sliderNetwork3, input$networkMode3,  input$sliderThreshold3, output, session)
    }, warning = function(w) {
      print(paste("Warning:  ", w))
    }, error = function(e) {
      print(paste("Error :  ", e))
      session$sendCustomMessage("handler_alert", paste("Problem while producing network (Gene Vs Gene): ", e, sep=""))
    }, finally = {
      # session$sendCustomMessage("handler_enableAllButtons", T)
    })
  })
  
  # aGoTool ####
  session$sendCustomMessage("handler_disableSourcesTabsaGoTool", T) # disable all source tab panels
  
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
  })
  
  ### PLOTS FOR aGOtool RESULTS#####
  
  ### SCATTER aGO ###
  
  observeEvent(input$aGoScatterSelect,{
    handleaGoScatterSelect(input$aGoScatterSelect, session)
  })
  observeEvent(input$aGoSliderScatter,{
    handleaGoScatterPlot(input$aGoScatterSelect, input$aGoSliderScatter, output, session)
  })
  
  ### BARPLOT aGO ###
  
  observeEvent(input$aGoBarSelect2,{
    handleaGoBarSelect2(input$aGoBarSelect2, session)
  })
  
  observeEvent(input$aGoBarplotMode,{
    handleaGoBarPlot2(input$aGoBarSelect2, input$aGoSliderBarplot, input$aGoBarplotMode, output, session)
  })
  
  observeEvent(input$aGoSliderBarplot,{
    handleaGoBarPlot2(input$aGoBarSelect2, input$aGoSliderBarplot, input$aGoBarplotMode, output, session)
  })
  
  ### HEATMAP aGO ##
  
  observeEvent(input$aGoHeatmapSelect,{
    handleaGoHeatmapSelect(input$aGoHeatmapSelect, session)
  })
  
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
  })
  
  observeEvent(input$aGoHeatmapAxis,{
    handleaGoHeatMap(input$aGoHeatmapSelect,input$aGoSliderHeatmap, input$aGoHeatmapAxis, input$aGoHeatmapMode, output, session)
  })
  
  observeEvent(input$aGoHeatmapMode,{
    handleaGoHeatMap(input$aGoHeatmapSelect,input$aGoSliderHeatmap, input$aGoHeatmapAxis, input$aGoHeatmapMode, output, session)
  })
  
  observeEvent(input$aGoHeatmapSelect2,{
    handleaGoHeatmapSelect2(input$aGoHeatmapSelect2, session)
  })
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
  })
  
  observeEvent(input$aGoHeatmapMode2,{
    handleaGoHeatMap2(input$aGoHeatmapSelect2, input$aGoSliderHeatmap2, input$aGoHeatmapMode2, output, session)
  })
  
  
  ### NETWORK aGO ##
  observeEvent(input$aGoNetworkSelect,{
    handleaGoNetworkSelect(input$aGoNetworkSelect, session)
  })
  
  observeEvent(input$aGoNetworkMode,{
    handleaGoNetwork(input$aGoNetworkSelect, input$aGoSliderNetwork, input$aGoNetworkMode, output, session)
  })
  
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
  })
  
  observeEvent(input$aGoNetworkSelect2,{
    handleaGoNetworkSelect2(input$aGoNetworkSelect2, session)
  })
  
  observeEvent(input$aGoNetworkMode2,{
    handleaGoNetwork2(input$aGoNetworkSelect2, input$aGoSliderNetwork2, input$aGoNetworkMode2,  input$aGoSliderThreshold, output, session)
  })
  
  observeEvent(input$aGoSliderThreshold,{
    handleaGoNetwork2(input$aGoNetworkSelect2, input$aGoSliderNetwork2, input$aGoNetworkMode2,  input$aGoSliderThreshold, output, session)
  }) 
  
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
  })
  
  observeEvent(input$aGoNetworkSelect3,{
    handleaGoNetworkSelect3(input$aGoNetworkSelect3, session)
  })
  
  observeEvent(input$aGoSliderNetwork3,{
    handleaGoNetworkSlider3(input$aGoSliderNetwork3,input$aGoNetworkSelect3, session)
  })
  
  observeEvent(input$aGoNetworkMode3,{
    handleaGoNetwork3(input$aGoNetworkSelect3, input$aGoSliderNetwork3, input$aGoNetworkMode3,  input$aGoSliderThreshold3, output, session)
  })
  
  observeEvent(input$aGoSliderThreshold3,{
    handleaGoNetwork3(input$aGoNetworkSelect3, input$aGoSliderNetwork3, input$aGoNetworkMode3,  input$aGoSliderThreshold3, output, session)
  }) 
  
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
  })
  observeEvent(input$literatureSliderScatter,{
    handleLitearureScatterPlot(input$literatureSliderScatter, output, session)
  })
  
  observeEvent(input$literatureSliderBarplot,{
    handleLiteratureBarPlot2(input$literatureSliderBarplot, input$literatureBarplotMode, output, session)
  })
  
  observeEvent(input$literatureBarplotMode,{
    handleLiteratureBarPlot2(input$literatureSliderBarplot, input$literatureBarplotMode, output, session)
  })
  
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
  })
  
  observeEvent(input$literatureHeatmapAxis,{
    handleLiteratureHeatMap(input$literatureSliderHeatmap, input$literatureHeatmapAxis, input$literatureHeatmapMode, output, session)
  })
  
  observeEvent(input$literatureHeatmapMode,{
    handleLiteratureHeatMap(input$literatureSliderHeatmap, input$literatureHeatmapAxis, input$literatureHeatmapMode, output, session)
  })
  
  
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
  })
  
  observeEvent(input$literatureHeatmapMode2,{
    handleLiteratureHeatMap2( input$literatureSliderHeatmap2, input$literatureHeatmapMode2, output, session)
  })
  
  observeEvent(input$literatureNetworkMode,{
    handleLiteratureNetwork( input$literatureSliderNetwork, input$literatureNetworkMode, output, session)
  })
  
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
  })
  
  observeEvent(input$literatureNetworkMode2,{
    handleLiteratureNetwork2( input$literatureSliderNetwork2, input$literatureNetworkMode2,  input$literatureSliderThreshold, output, session)
  })
  
  observeEvent(input$literatureSliderThreshold,{
    handleLiteratureNetwork2( input$literatureSliderNetwork2, input$literatureNetworkMode2,  input$literatureSliderThreshold, output, session)
  }) 
  
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
  })
  
  ### Network LIT 3
  
  observeEvent(input$literatureSliderNetwork3,{
    handleLiteratureNetworkSlider3(input$literatureSliderNetwork3,session)
  })
  
  observeEvent(input$literatureNetworkMode3,{
    handleLiteratureNetwork3( input$literatureSliderNetwork3, input$literatureNetworkMode3,  input$literatureSliderThreshold3, output, session)
  })
  
  observeEvent(input$literatureSliderThreshold3,{
    handleLiteratureNetwork3( input$literatureSliderNetwork3, input$literatureNetworkMode3,  input$literatureSliderThreshold3, output, session)
  }) 
  
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
  })
  
  #Protein-Protein Network (STRING)-####
  observeEvent(input$network, {
    session$sendCustomMessage("handler_startLoader", c(29,10))
    dataset=input$STRINGnetworkSelect
    organism = input$STRINGnetworkOrganism
    type=input$STRINGnetworkType
    edges=input$STRINGnetworkEdges
    score=input$STRINGnetworkScore
    
    create_string_network(dataset, organism, type, edges, score, output)
    session$sendCustomMessage("handler_startLoader", c(29,100))
    session$sendCustomMessage("handler_finishLoader", 29)
  })
  
  # Conversion ####
  
  observeEvent(input$gconvert_button,{
    handle_gconvert(input$gconvert_select, input$gconvert_organism, input$gconvert_target, output)
  })
  
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
  })
  
  observeEvent(input$gorth_organism,{
    handle_gorthOrganism(input$gorth_organism, session)
  })
  
}
