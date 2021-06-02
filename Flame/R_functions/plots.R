# This function receives the clicked element from the Manhattan plot and prints a table with a single line
# containing the gprofiler result row below
handleManhattanClick <- function(output, currentTermID){
  # event_data("plotly_click", source="A")
  # currentTermID <-  event_data("plotly_click")$key #, source = "A"
  # if (!identical(which(click_event %in% currentTermID), integer(0) )) click_event <<- click_event[click_event != currentTermID]
  # else click_event <<- append( click_event, currentTermID, length(click_event))
  table_man <- all_gost[grepl(currentTermID, all_gost$Term_ID), ] # for more than one: click_event contains the array of clicked elements, so match returns all hit rows if replace: currentTermID with click_event
  # output$manhattan_table <- DT::renderDataTable(as.data.frame(table_man), server = FALSE,
  #                                               extensions = 'Buttons',
  #                                               options = list(
  #                                                 pageLength = 10,
  #                                                 "dom" = 'T<"clear">lBfrtip',
  #                                                 buttons = list(list(extend='excel', filename=paste('Manhattan_Table_', currentTermID, sep="")),
  #                                                                list(extend= 'csv', filename=paste('Manhattan_Table_', currentTermID, sep="")),
  #                                                                list(extend='copy', filename=paste('Manhattan_Table_', currentTermID, sep="")),
  #                                                                list(extend='pdf', filename=paste('Manhattan_Table_', currentTermID, sep="")),
  #                                                                list(extend='print', filename=paste('Manhattan_Table_', currentTermID, sep="")))
  #                                               ), rownames= FALSE, escape = FALSE
  # )
  output$manhattan_table <- renderTableFunc(as.data.frame(table_man), currentTermID, 11, "Manhattan_Table_", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
  session$sendCustomMessage("handler_finishLoader", 3)
}

# This function receives the selected element using the select box option  from the Manhattan plot and prints a table with nrow= Nselected elements
# containing the gprofiler results row below
handleManhattanSelect <- function(output,currentTermIDs){
  currentTermIDs <-  event_data("plotly_selected")$key #, source = "A"
  
  table_man <- all_gost
  table_man <- table_man[0,]
  for (i in 1:length(currentTermIDs))
  {
    table_man[nrow(table_man) + 1,] <- all_gost[grepl(currentTermIDs[i], all_gost$Term_ID), ]
  }
  # output$manhattan_table <- DT::renderDataTable(table_man, server = FALSE,
  #                                               extensions = 'Responsive',
  #                                               options = list(
  #                                                 pageLength = 11,
  #                                                 dom = 'Brftip',
  #                                                 buttons = list(list(extend='collection', buttons=c('csv', 'excel', 'pdf'), text="Download")),
  #                                                 columnDefs = list(list(visible=F, targets=c(8,9)))
  #                                               ), rownames= FALSE, escape = FALSE
  # )
  output$manhattan_table <- renderTableFunc(table_man, "Selected", 11, "Manhattan_Table_", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
  
}

# This event updates the scatterplot slider, an event which is observed in server
# and consequently changes the scatterplot and the respective table
# @param DB_source: one eligible datasource from available
# @return void
handleScatterSelect <- function(DB_source, session){
  if (!identical(DB_source, "")){
    gostres_m <- all_gost[grepl(DB_source, all_gost$Source),]
    # to ALWAYS trigger an observed event from scatterplot slider, first update with a value fo 0
    updateSliderInput(session, "sliderScatter", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "sliderScatter", "Choose a number of results to view:", min = 1,
                      max = length(gostres_m$Term_ID), value = 10, step = 1)
  }
}

# This function draws a scatterplot and the corresponding data table
# @param DB_source: one eligible datasource from available
# @param sliderScatter: the user slider input
# @return void
handleScatterPlot <- function(DB_source, sliderScatter, output, session){
  if (!identical(DB_source, "")){
    session$sendCustomMessage("handler_startLoader", c(7,100))
    scatter_colors <- c('GO:MF'= "#dc3912", 'GO:BP'= "#ff9900", 'GO:CC' = "#109618", KEGG =
                          "#dd4477", REAC = "#3366cc", WP = "#0099c6", TF = "#5574a6", MIRNA = "#22aa99", HPA =
                          "#6633cc", CORUM = "#66aa00", HP = "#990099")
    new_color <- scatter_colors[DB_source][[1]]
    DB_table <- all_gost[grepl(DB_source, all_gost$Source), ]
    DB_table <- subset(DB_table, select=-c(`Positive Hits`))
    DB_table <- DB_table[with(DB_table,order(-`Enrichment Score %`)),]
    output$scatterPlot <- renderUI({plotlyOutput("scatter", height = height_plots(sliderScatter, 15, 110))})
    output$scatter <- renderPlotly({
    
      ggplot(DB_table[1:sliderScatter,], aes(`Enrichment Score %`, `-log10Pvalue`,text=paste("TERM_ID: ", Term_ID,"\n", "FUNCTION: ", Function, sep=""))) + geom_point(fill=new_color,color="black", position = position_dodge(width = 0.8), alpha = 0.7, pch=21, size=3, stroke=0.3)+ labs(x = "ENRICHMENT SCORE %")+ labs(title = DB_source )
    })
    output$scatter_table <- DT::renderDataTable(head(DB_table, sliderScatter), server = FALSE,
                                                extensions = 'Buttons',
                                                options = list(
                                                  "dom" = 'T<"clear">lBfrtip',
                                                  buttons = list(list(extend='excel',filename=paste('ScatterPlot_Table_', DB_source, sep="")),
                                                                 list(extend= 'csv',filename=paste('ScatterPlot_Table_', DB_source, sep="")),
                                                                 list(extend='copy',filename=paste('ScatterPlot_Table_', DB_source, sep="")),
                                                                 list(extend='pdf',filename=paste('ScatterPlot_Table_', DB_source, sep="")),
                                                                 list(extend='print',filename=paste('ScatterPlot_Table_', DB_source, sep="")))
                                                ),rownames= FALSE, escape=F)
    session$sendCustomMessage("handler_finishLoader", 7)
  }
}
# deprecated 
# This event updates the barplot slider, an event which is observed in server
# and consequently changes the barplot and the respective table
# @param DB_source: one eligible datasource from available
# @return void
handleBarSelect <- function(DB_source, session){
  if (!identical(DB_source, "")){
    gostres_m <- all_gost[grepl(DB_source, all_gost$Source),]
    # to ALWAYS trigger an observed event from barplot slider, first update with a value fo 0
    updateSliderInput(session, "sliderBarplot", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "sliderBarplot", "Choose a number of results to view:", min = 1,
                      max = length(gostres_m$Term_ID), value = 10, step = 1)
  }
}
# This event updates the barplot slider, an event which is observed in server
# and consequently changes the barplot and the respective table
# Not called when no sources are selected
# @param DB_source: multiple eligible datasources from available
# @return void
handleBarSelect2 <- function(DB_sources, session){
  gostres_m <- all_gost[grepl(paste(DB_sources, collapse="|"), all_gost$Source),] # collapse="|", instead of for loop
  # to ALWAYS trigger an observed event from barplot slider, first update with a value fo 0
  updateSliderInput(session, "sliderBarplot", "Choose a number of results to view:", min = 1, 
                    max = 1, value = 0, step = 1)
  updateSliderInput(session, "sliderBarplot", "Choose a number of results to view:", min = 1,
                    max = length(gostres_m$Term_ID), value = 10, step = 1)
  # }
}
# deprecated
# This function draws a barplot and the corresponding data table
# @param DB_source: one eligible datasource from available
# @param sliderBarplot: the user slider input
# @return void
handleBarPlot <- function(DB_source, sliderBarplot, barplotMode, output, session){
  if (!identical(DB_source, "")){
    session$sendCustomMessage("handler_startLoader", 8)
    new_color <- bar_colors[DB_source][[1]]
    DB_table <- all_gost[grepl(DB_source,all_gost$Source),] #with links
    barplotInputTable<-gostres[grepl(DB_source, gostres$Source), ] # without links
    
    DB_table <- subset(DB_table, select=-c(`Positive Hits`))
    barplotInputTable <- subset(barplotInputTable, select=-c(`Positive Hits`))
    # Check mode of execution
    if(barplotMode == "-log10Pavlue"){
      output$barplot <- renderUI({plotlyOutput("barplot1", height = height_plots(sliderBarplot, 18, 100))})
      DB_table <- head(DB_table, sliderBarplot)
      barplotInputTable <- head(barplotInputTable, sliderBarplot)
      output$barplot1 <- renderPlotly({
       
        ggplot(barplotInputTable, aes(Term_ID, `-log10Pvalue`,text=paste("FUNCTION: ", Function,"\n", "ENRICHMENT SCORE %: ", `Enrichment Score %`, sep=""))) + geom_bar(stat='identity', fill = new_color)+ coord_flip()+ labs(y = "-LOG10PAVLUE ")+ labs(title = DB_source )+
          geom_text(aes(label=`Intersection Size`), vjust=+0.5, size=3.5)+ scale_x_discrete(limits = barplotInputTable$Term_ID[order(barplotInputTable$`-log10Pvalue`)])
      })
      output$barplot_table <- DT::renderDataTable(DB_table, server = FALSE, 
                                                  extensions = 'Buttons',
                                                  options = list(
                                                    "dom" = 'T<"clear">lBfrtip',
                                                    buttons = list(list(extend='excel',filename=paste(DB_source, '_BarPlot_Table_-logPval', sep="")),
                                                                   list(extend= 'csv',filename=paste(DB_source, '_BarPlot_Table_-logPval', sep="")),
                                                                   list(extend='copy',filename=paste(DB_source, '_BarPlot_Table_-logPval', sep="")),
                                                                   list(extend='pdf',filename=paste(DB_source, '_BarPlot_Table_-logPval', sep="")),
                                                                   list(extend='print',filename=paste(DB_source, '_BarPlot_Table_-logPval', sep="")))
                                                  ),rownames= FALSE, escape=F)
      
    }
    else { # Enrichment Mode
      DB_table <- DB_table[with(DB_table,order(-(`Enrichment Score %`))),]
      barplotInputTable <- barplotInputTable[with(barplotInputTable,order(-(`Enrichment Score %`))),]
      DB_table <- head(DB_table, sliderBarplot)
      barplotInputTable <- head(barplotInputTable, sliderBarplot)
      output$barplot <- renderUI({plotlyOutput("barplot1", height = height_plots(sliderBarplot, 18, 100))})
      output$barplot1 <- renderPlotly({
       
        ggplot(barplotInputTable, aes(Term_ID, `Enrichment Score %`,text=paste("FUNCTION: ", Function,"\n", "-LOG_PVALUE: ", `-log10Pvalue`, sep=""))) + geom_bar(stat='identity', fill = new_color)+coord_flip()+labs(y = "ENRICHMENT SCORE % ")+ labs(title = DB_source )+ 
          geom_text(aes(label=`Intersection Size`), vjust=+0.5, size=3.5)+ scale_x_discrete(limits = barplotInputTable$Term_ID[order(barplotInputTable$`Enrichment Score %`)])
      })
      
      output$barplot_table <- DT::renderDataTable(DB_table, server = FALSE, 
                                                  extensions = 'Buttons',
                                                  options = list(
                                                    "dom" = 'T<"clear">lBfrtip',
                                                    buttons = list(list(extend='excel',filename=paste(DB_source, '_BarPlot_Table_EnScore', sep="")),
                                                                   list(extend= 'csv',filename=paste(DB_source, '_BarPlot_Table_EnScore', sep="")),
                                                                   list(extend='copy',filename=paste(DB_source, '_BarPlot_Table_EnScore', sep="")),
                                                                   list(extend='pdf',filename=paste(DB_source, '_BarPlot_Table_EnScore', sep="")),
                                                                   list(extend='print',filename=paste(DB_source, '_BarPlot_Table_EnScore', sep="")))
                                                  ),rownames= FALSE, escape=F)
      
    }
    session$sendCustomMessage("handler_finishLoader", 8)
  }
}
# THIS ONE
# This function draws a barplot from multiple selected sources and the corresponding data table
# @param DB_source: multiple eligible datasource from available
# @param sliderBarplot: the user slider input
# @return void
handleBarPlot2 <- function(DB_sources, sliderBarplot, barplotMode, output, session){
  if (!identical(DB_sources, NULL)){
    session$sendCustomMessage("handler_startLoader", c(8,30))
    DB_table <- gostres[grepl(paste(DB_sources, collapse="|"), gostres$Source),] # collapse="|", instead of for loop, without links
    all_gost_table <- all_gost[grepl(paste(DB_sources, collapse="|"), all_gost$Source),]
    DB_table <- subset(DB_table, select=-c(`Positive Hits`))
    all_gost_table <- subset(all_gost_table, select=-c(`Positive Hits`))
    #make a figure legend based on DB_source
    
    fonts <- c("GO:MF"= "white", "GO:BP"= "black", "GO:CC" = "black",
      "KEGG" = "white", "REAC" = "white", "WP" = "black", "TF" = "black",
      "MIRNA" = "black", "HPA" = "black", "CORUM" = "black", "HP" = "white")
    
    fig_legend = "<table style='border-spacing: 0;border-collapse: collapse;'><tr><td><b>Colors:&nbsp;&nbsp;</b></td>"
    for(i in 1:length(DB_sources))
    {
      fig_legend<-paste(fig_legend, sprintf("<td style='background-color:%s; color:%s;border: 1px solid black'>&nbsp; %s &nbsp;</td>", bar_colors[DB_sources[i]][[1]][1], fonts[DB_sources[i]][[1]][1], DB_sources[i]), sep="")
    }
    fig_legend<-paste(fig_legend, "</tr></table>")
    output$bar_legend_gprof<- renderUI(HTML(fig_legend))
    session$sendCustomMessage("handler_startLoader", c(8,60))
    # Check mode of execution
    if(barplotMode == "-log10Pavlue"){
      output$barplot <- renderUI({plotlyOutput("barplot1", height = height_plots(sliderBarplot, 18, 100))})
      DB_table <- DB_table[with(DB_table, order(-(`-log10Pvalue`), Function)),] # changed factor to double in the global variable
      DB_table <- head(DB_table, sliderBarplot)
      all_gost_table <- all_gost_table[with(all_gost_table, order(-(`-log10Pvalue`), Function)),] # changed factor to double in the global variable
      all_gost_table <- head(all_gost_table, sliderBarplot)
      DB_table <- DB_table[with(DB_table, order( Function, decreasing = T)),]
      DB_table <- DB_table[with(DB_table, order(-`-log10Pvalue`)),]
      new_colors <- as.character(bar_colors[as.character(DB_table$Source)])
      #new_colors <- bar_colors[as.character(DB_table$Source)]
      
      # par(las=1) # make label text perpendicular to axis
      # par(mar=c(5, 30, 2, 10)) # increase y-axis margin.
      DB_table$Term_ID <- factor(DB_table$Term_ID, levels = unique(DB_table$Term_ID)[order(DB_table$`-log10Pvalue`, decreasing = F)])
      output$barplot1 <- renderPlotly({
        plot_ly(x =  DB_table$`-log10Pvalue`, y = DB_table$Term_ID, 
                marker=list(color=new_colors), 
                type = 'bar', orientation = 'h', 
                text =  DB_table$`Intersection Size`,
                textposition = 'auto',
                textangle = 0,
                hoverinfo = "text",
                hovertext =paste("TERM ID: ",DB_table$Term_ID, "\n-LOG10PAVULE: ",DB_table$`-log10Pvalue`, "\nFUNCTION: ", DB_table$Function,"\n", "ENRICHMENT SCORE %: ", DB_table$`Enrichment Score %`, sep=""))%>% 
          layout(title = paste(DB_sources, collapse="_"),xaxis = list(title = "-LOG10PVALUE "), yaxis=list(title="TERM ID")) 
          
      })
        # output$barplot1 <- renderPlotly({
      #  
      #   ggplot(DB_table, aes(Term_ID, `-log10Pvalue`, text=paste("FUNCTION: ", Function,"\n", "ENRICHMENT SCORE %: ", `Enrichment Score %`, sep=""))) +
      #     geom_bar(stat='identity', fill= new_colors) + coord_flip()+ labs(y = "-LOG10PAVLUE ")+ labs(title = paste(DB_sources, collapse="_") ) +
      #     geom_text(aes(label=`Intersection Size`), vjust=+0.5, size=3.5) + 
      #     scale_x_discrete(limits = DB_table$Term_ID[order(DB_table$`-log10Pvalue`)])
      # })
      
    }
    else { # Enrichment Mode
      DB_table <- DB_table[with(DB_table, order(-(`Enrichment Score %`), Function)),]
      DB_table <- head(DB_table, sliderBarplot)
      all_gost_table <- all_gost_table[with(all_gost_table, order(-(`Enrichment Score %`), Function)),]
      all_gost_table <- head(all_gost_table, sliderBarplot)
      DB_table <- DB_table[with(DB_table, order( Function, decreasing = T)),]
      DB_table <- DB_table[with(DB_table, order(-`Enrichment Score %`)),]
      #new_colors <- bar_colors[as.character(DB_table$Source)]
      new_colors <- as.character(bar_colors[as.character(DB_table$Source)])
      output$barplot <- renderUI({plotlyOutput("barplot1", height = height_plots(sliderBarplot, 18, 100))})
      # output$barplot1 <- renderPlotly({
      #  
      #   ggplot(DB_table, aes(Term_ID, `Enrichment Score %`, text=paste("FUNCTION: ", Function,"\n", "-LOG_PVALUE: ", `-log10Pvalue`, sep=""))) + geom_bar(stat='identity', fill= new_colors)+coord_flip()+labs(y = "ENRICHMENT SCORE % ")+ labs(title = paste(DB_sources, collapse="_") ) +
      #     geom_text(aes(label=`Intersection Size`), vjust=+0.5, size=3.5)+ scale_x_discrete(limits = DB_table$Term_ID[order(DB_table$`Enrichment Score %`)]) +
      #     scale_colour_manual(values = as.vector(new_colors))
      # })
      DB_table$Term_ID <- factor(DB_table$Term_ID, levels = unique(DB_table$Term_ID)[order(DB_table$`Enrichment Score %`, decreasing = F)])
      output$barplot1 <- renderPlotly({
        plot_ly(x =  DB_table$`Enrichment Score %`, y = DB_table$Term_ID, 
                marker=list(color=new_colors), 
                type = 'bar', orientation = 'h', 
                text =  DB_table$`Intersection Size`,
                textposition = 'auto',
                textangle = 0,
                hoverinfo = "text",
                hovertext =paste("TERM ID: ",DB_table$Term_ID, "\n-LOG10PAVULE: ",DB_table$`-log10Pvalue`, "\nFUNCTION: ", DB_table$Function,"\n", "ENRICHMENT SCORE %: ", DB_table$`Enrichment Score %`, sep=""))%>% 
          layout(title = paste(DB_sources, collapse="_"),xaxis = list(title = "ENRICHMENT SCORE "), yaxis=list(title="TERM ID")) 
        
      })
      
    }
    session$sendCustomMessage("handler_startLoader", c(8,90))
    output$barplot_table <- DT::renderDataTable(all_gost_table, server = FALSE,
                                                extensions = 'Buttons',
                                                options = list(
                                                  "dom" = 'T<"clear">lBfrtip',
                                                  buttons = list(list(extend='excel',filename=paste(paste(DB_sources, collapse="_"), '_BarPlot_Table_EnScore', sep="")),
                                                                 list(extend= 'csv',filename=paste(paste(DB_sources, collapse="_"), '_BarPlot_Table_EnScore', sep="")),
                                                                 list(extend='copy',filename=paste(paste(DB_sources, collapse="_"), '_BarPlot_Table_EnScore', sep="")),
                                                                 list(extend='pdf',filename=paste(paste(DB_sources, collapse="_"), '_BarPlot_Table_EnScore', sep="")),
                                                                 list(extend='print',filename=paste(paste(DB_sources, collapse="_"), '_BarPlot_Table_EnScore', sep="")))
                                                ),rownames= FALSE, escape=F)
    session$sendCustomMessage("handler_startLoader", c(8,100))
    session$sendCustomMessage("handler_finishLoader", 8)
  }
}


# This event updates the Heatmap slider, an event which is observed in server
# and consequently changes the Heatmap and the respective table
# @param DB_source: one eligible datasource from available
# @return void
handleHeatmapSelect <- function(DB_source, session){
  if (!identical(DB_source, "")){
    gostres_m <- all_gost[grepl(DB_source, all_gost$Source),]
    # to ALWAYS trigger an observed event from Heatmap slider, first update with a value fo 0
    updateSliderInput(session, "sliderHeatmap", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "sliderHeatmap", "Choose a number of results to view:", min = 2,
                      max = length(gostres_m$Term_ID), value = 10, step = 1)
  }
}

# This function draws a heatmap and the corresponding data table
# @param heatmapSelect: one eligible datasource from available
# @param sliderHeatmap: the user slider input
# @param heatmapAxis: reverse the heatmap s axis
# @param heatmapMode: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleHeatMap <- function(heatmapSelect, sliderHeatmap, heatmapAxis, heatmapMode, output, session){
  if (!identical(heatmapSelect, "")){
    session$sendCustomMessage("handler_startLoader", c(6,30))
    new_color <- bar_colors[heatmapSelect][[1]]
    heatMapGostres <- gostres[grepl(heatmapSelect, gostres$Source), ]
    all_gost_table <- all_gost[grepl(heatmapSelect, all_gost$Source), ]
    if (length(heatMapGostres$Function) == 1) session$sendCustomMessage("handler_alert", "Cannot create a clustered heatmap if the results<2.")
    else{
      if (heatmapMode == "Enrichment Score") {
        heatMapGostres <- heatMapGostres[with(heatMapGostres,order(-`Enrichment Score %`)),]
        all_gost_table <- all_gost_table[with(all_gost_table,order(-`Enrichment Score %`)),]
        
      }
      heatMapGostres <- head(heatMapGostres, sliderHeatmap)
      all_gost_table <- head(all_gost_table, sliderHeatmap)
      output$heatmap_table <- DT::renderDataTable( subset(all_gost_table, select=-c(`Positive Hits`)), server = FALSE, 
                                                   extensions = 'Buttons',
                                                   options = list("dom" = 'T<"clear">lBfrtip',
                                                                  buttons = list(list(extend='excel',filename=paste(heatmapSelect, '_Heatmap1', sep="")),
                                                                                 list(extend= 'csv',filename=paste(heatmapSelect, '_Heatmap1', sep="")),
                                                                                 list(extend='copy',filename=paste(heatmapSelect, '_Heatmap1', sep="")),
                                                                                 list(extend='pdf',filename=paste(heatmapSelect, '_Heatmap1', sep="")),
                                                                                 list(extend='print',filename=paste(heatmapSelect, '_Heatmap1', sep="")))
                                                   ),rownames= FALSE, escape=F)
      all_genes <- paste(heatMapGostres$`Positive Hits`, collapse=",") 
      all_genes <- strsplit(all_genes, ",")
      all_genes <- unique(all_genes[[1]])
      id <- heatMapGostres$Term_ID
      functions <- heatMapGostres$Function
      heatmapTable <- as.data.frame(matrix(0, ncol = length(all_genes), nrow = length(functions)))
      colnames(heatmapTable) <- all_genes
      rownames(heatmapTable) <- id #paste(id, functions, sep="_")
      score <- paste(heatMapGostres$`-log10Pvalue`, collapse=",")
      score <- strsplit(score , ",")
      score <- as.numeric(as.character(score[[1]]))
      score_en <- paste(heatMapGostres$`Enrichment Score %` , collapse=",")
      score_en <- strsplit(score_en , ",")
      score_en <- as.numeric(as.character(score_en[[1]]))
      
      for (i in 1:length(functions)) heatmapTable[i, match(strsplit(as.character(heatMapGostres$`Positive Hits`[i]), ",")[[1]], all_genes)] <- score[i]
      session$sendCustomMessage("handler_startLoader", c(6,60))
      if (heatmapMode == "Enrichment Score") {for (i in 1:length(functions)) heatmapTable[i, match(strsplit(as.character(heatMapGostres$`Positive Hits`[i]), ",")[[1]], all_genes)] <- score_en[i]}
      session$sendCustomMessage("handler_startLoader", c(6,90))
      
      hoverLabelsTable <- heatmapTable                # duplicate the initial input and change the rownames.
      rownames(hoverLabelsTable) <- functions         # custom_hovertext parameter takes as an input a table with ta same dimensions... kt tetoio
      hoverLabelsTable[] <- paste("FUNCTION: ", rownames(hoverLabelsTable))
      hoverLabelsTable[] <- lapply(colnames(hoverLabelsTable), function(colname) {
        paste0(hoverLabelsTable[, colname], ", ", colname)
      })
      
      session$sendCustomMessage("handler_startLoader", c(6,100))
      if (heatmapAxis == "Genes-Functions") {
        heatmapTable <- t(heatmapTable)
        hoverLabelsTable <- t(hoverLabelsTable)
        genesNumber <- length(all_genes)
        
        
        output$heatmapPlot <- renderUI(plotlyOutput("heatmap", height = height_plots(genesNumber, 15, 500)))
        output$heatmap <- renderPlotly({
         
          
          heatmaply(heatmapTable,custom_hovertext = hoverLabelsTable, 
                    xlab = "FUNCTIONS",ylab = "GENES",main = heatmapSelect, fontsize_row = 10, fontsize_col = 10, 
                    grid_gap = 1,show_dendrogram = c(F, F), #margins = c(NA, NA, 60, NA),
                    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = new_color)
          )
        })
      }
      else {
        output$heatmapPlot <- renderUI(plotlyOutput("heatmap", height = height_plots(sliderHeatmap, 15, 200)))
        output$heatmap <- renderPlotly({
         
          heatmaply(heatmapTable, custom_hovertext = hoverLabelsTable, 
                    xlab = "GENES",ylab = "FUNCTIONS",main = heatmapSelect, fontsize_row = 10,fontsize_col = 10,
                    show_dendrogram = c(F, F), grid_gap = 1, #margins = c(NA, NA, 60, NA),
                    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = new_color)
                    
          )
        })
      }
    }
    session$sendCustomMessage("handler_finishLoader", 6)
  }#end of first if
}

# This event updates the Heatmap2 slider, an event which is observed in server
# and consequently changes the Heatmap and the respective table
# @param DB_source: one eligible datasource from available
# @return void
handleHeatmapSelect2 <- function(DB_source, session){
  if (!identical(DB_source, "")){
    gostres_m <- all_gost[grepl(DB_source, all_gost$Source),]
    # to ALWAYS trigger an observed event from Heatmap slider, first update with a value fo 0
    updateSliderInput(session, "sliderHeatmap2", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "sliderHeatmap2", "Choose a number of results to view:", min = 2,
                      max = length(gostres_m$Term_ID), value = 10, step = 1)
  }
}

# This function draws a heatmap2 and the corresponding data table
# @param heatmapSelect: one eligible datasource from available
# @param sliderHeatmap: the user slider input
# @param heatmapAxis: reverse the heatmap s axis
# @param heatmapMode: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleHeatMap2 <- function(heatmapSelect2, sliderHeatmap2, heatmapMode2, output, session){
  if (!identical(heatmapSelect2, "")){
    
    session$sendCustomMessage("handler_startLoader", c(10, 30))
    new_color <- bar_colors[heatmapSelect2][[1]]
    heatMapGostres <- gostres[grepl(heatmapSelect2, gostres$Source), ]
    all_gost_table <- all_gost[grepl(heatmapSelect2, all_gost$Source), ] #with links
    if (length(heatMapGostres$Function) == 1) session$sendCustomMessage("handler_alert", "Cannot create a clustered heatmap if the results<2.")
    else{
      
      if (heatmapMode2 == "Enrichment Score") {
        heatMapGostres <- heatMapGostres[with(heatMapGostres,order(-`Enrichment Score %`)),]
        all_gost_table <- all_gost_table[with(all_gost_table,order(-`Enrichment Score %`)),]
      }
      heatMapGostres <- head(heatMapGostres, sliderHeatmap2)
      all_gost_table <- head(all_gost_table, sliderHeatmap2)
      heatmapTable <- as.data.frame(matrix(0, ncol = length(heatMapGostres$Function), nrow = length(heatMapGostres$Function)))
      colnames(heatmapTable) <- heatMapGostres$Term_ID #paste(heatMapGostres$Term_ID, heatMapGostres$Function, sep="_")
      rownames(heatmapTable) <- heatMapGostres$Term_ID #paste(heatMapGostres$Term_ID, heatMapGostres$Function, sep="_")
      
      similarityColumnTable <- list()
      intersectGenesList <- list()
      
      for (i in 1:(length(heatMapGostres$Function))){
        row <- heatMapGostres$`Positive Hits`[i]
        row <- paste(row, collapse=",") 
        row <- strsplit(row, ",")
        for (j in 1:length(heatMapGostres$Function)) {
          col <- heatMapGostres$`Positive Hits`[j]
          col <- paste(col, collapse=",") 
          col <- strsplit(col, ",")
          unionGenes<-append(col,row)
          unionGenes<- unlist(unionGenes)
          similarityScore=((length(intersect(col[[1]], row[[1]])))/ (length(unique(unionGenes))))*100
          similarityScore= round(similarityScore, digits = 2)
          heatmapTable[i,j] <- similarityScore
          #heatmapTable[j,i] <- similarityScore
          similarityColumnTable[[length(similarityColumnTable) + 1]] <- similarityScore
          intersectGenesList[[length(intersectGenesList) + 1]] <- as.list(intersect(col[[1]], row[[1]]))
        }
      }
      #heatmapTable[ row(heatmapTable) == col(heatmapTable) ] <- 100
      session$sendCustomMessage("handler_startLoader", c(10, 70))
      combinedFunctions <- expand.grid(as.list(heatMapGostres$Function), as.list(heatMapGostres$Function))
      combinedTermIdsLinks <- expand.grid(as.list(all_gost_table$Term_ID), as.list(all_gost_table$Term_ID))
      heatTable <- as.data.frame(cbind("Function_A"=combinedFunctions$Var1,"Term_ID_A"= combinedTermIdsLinks$Var1, 
                                       "Function_B"=combinedFunctions$Var2,"Term_ID_B"= combinedTermIdsLinks$Var2, 
                                       "Similarity Score %"= similarityColumnTable,
                                 "Intersection"= intersectGenesList))
      #heatTable2 <- as.data.frame(cbind("Function_A"=combinedFunctions$Var1,"Term_ID_A"= combinedTermIdsLinks$Var1, "Function_B"=combinedFunctions$Var2,"Term_ID_B"= combinedTermIdsLinks$Var2, "Similarity Score %"= as.character(similarityColumnTable),"Intersection"= intersectGenesList))
      
      
      heatTable$Function_A <- as.character(unlist(heatTable$Function_A))
      heatTable$Function_B <- as.character(unlist(heatTable$Function_B))
      heatTable$`Similarity Score %` <- as.numeric(as.character(heatTable$`Similarity Score %`))
      session$sendCustomMessage("handler_startLoader", c(10, 80))
      for (i in nrow(heatTable):1){ # remove self rows from table
        if (identical(heatTable$Function_A[i], heatTable$Function_B[i])) heatTable <- heatTable[-i, ] # removing self entries
      }
      heatTable <- heatTable[heatTable$`Similarity Score %` > 0, ] # removing zero entries
      #startFor2 <- Sys.time()
      # removing reverse edges
      heatTable <- heatTable[!duplicated(apply(heatTable[, c("Function_A", "Function_B" )],1,function(x) paste(sort(x), collapse='_'))),]
      heatTable <- heatTable[with(heatTable,order(-`Similarity Score %`)),]
      # endFor2 <- Sys.time()
      # print("For2 time: ")
      # print(endFor2 - startFor2)
      
      # output$heatmap_table2 <- DT::renderDataTable(heatTable, server = FALSE,
      #                                              extensions = 'Buttons',
      #                                              options = list("dom" = 'T<"clear">lBfrtip',
      #                                                             buttons = list(list(extend='excel',filename=paste(heatmapSelect2, '_Heatmap2', sep="")),
      #                                                                            list(extend= 'csv',filename=paste(heatmapSelect2, '_Heatmap2', sep="")),
      #                                                                            list(extend='copy',filename=paste(heatmapSelect2, '_Heatmap2', sep="")),
      #                                                                            list(extend='pdf',filename=paste(heatmapSelect2, '_Heatmap2', sep="")),
      #                                                                            list(extend='print',filename=paste(heatmapSelect2, '_Heatmap2', sep="")))
      #                                              ),rownames= FALSE, escape=F)
      
      columnVis<- c(2,3,4,5,6,7)
      output$heatmap_table2 <- renderTableFunc(heatTable, heatmapSelect2, 7, "function-function_heatmap", "Positive Hits", columnVis)
      
      output$heatmapPlot2 <- renderUI(plotlyOutput("heatmap2", height = height_plots(sliderHeatmap2, 18, 500)))
      output$heatmap2 <- renderPlotly({
        
        heatmaply(heatmapTable,
                  xlab = "FUNCTIONS",ylab = "FUNCTIONS", main = heatmapSelect2, fontsize_row = 10, fontsize_col = 10, grid_gap = 1,
                  show_dendrogram = c(F, F),  #margins = c(NA, NA, 70, NA),
                  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = new_color)
        )
      })
    }
    session$sendCustomMessage("handler_startLoader", c(10, 100))
    session$sendCustomMessage("handler_finishLoader", 10)
  }
}

# This event updates the Network slider, an event which is observed in server
# and consequently changes the Network and the respective table
# @param DB_source: one eligible datasource from available
# @return void
handleNetworkSelect <- function(DB_source, session){
  if (!identical(DB_source, "")){
    gostres_m <- all_gost[grepl(DB_source, all_gost$Source),]
    # to ALWAYS trigger an observed event from Heatmap slider, first update with a value fo 0
    updateSliderInput(session, "sliderNetwork", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "sliderNetwork", "Choose a number of results to view:", min = 2,
                      max = length(gostres_m$Term_ID), value = 10, step = 1)
  }
}
# This function draws a network and the corresponding data table
# @param networkSelect: one eligible datasource from available
# @param sliderNetwork: the user slider input
# @param networkMode: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleNetwork <- function(networkSelect,sliderNetwork, networkMode, output, session){
  if (!identical(networkSelect, "")){
    session$sendCustomMessage("handler_startLoader", c(9,30))
    networkGostres <- gostres[grepl(networkSelect, gostres$Source), ] # without links
    all_gost_table <- all_gost[grepl(networkSelect, all_gost$Source), ] # with links
    all_gost_table <- subset(all_gost_table, select=-c(`Positive Hits`)) #remove the Genes column from the table
    
    if (networkMode == "Enrichment Score") {
      networkGostres <- networkGostres[with(networkGostres,order(-`Enrichment Score %`)),]
      all_gost_table <- all_gost_table[with(all_gost_table,order(-`Enrichment Score %`)),]
    }
    
    output$network_table <- DT::renderDataTable(head(all_gost_table, sliderNetwork), server = FALSE, 
                                                extensions = 'Buttons',
                                                options = list("dom" = 'T<"clear">lBfrtip',
                                                               buttons = list(list(extend='excel', filename=paste(networkSelect, '_Network', sep="")),
                                                                              list(extend= 'csv', filename=paste(networkSelect, '_Network', sep="")),
                                                                              list(extend='copy', filename=paste(networkSelect, '_Network', sep="")),
                                                                              list(extend='pdf', filename=paste(networkSelect, '_Network', sep="")),
                                                                              list(extend='print', filename=paste(networkSelect, '_Network', sep="")))
                                                ), rownames= FALSE, escape=F)
    
    networkGostres <- head(networkGostres,sliderNetwork) 
    session$sendCustomMessage("handler_startLoader", c(9,40))
    networkEdgelist <- matrix("", nrow = 0, ncol = 2) #3 
    for (i in 1:nrow(networkGostres)){
      name <- paste(networkGostres$Term_ID[i], networkGostres$Function[i], sep="_")
      lineGenes <- strsplit(as.character(networkGostres$`Positive Hits`[i]), ",")[[1]]
      for (j in 1:length(lineGenes)) networkEdgelist <- rbind(networkEdgelist, c(name, lineGenes[j]))
    }
   
    new_color <- bar_colors[networkSelect][[1]]
    session$sendCustomMessage("handler_startLoader", c(9,70))
    all_genes <- paste(networkGostres$`Positive Hits`, collapse=",") 
    all_genes <- strsplit(all_genes, ",")
    all_genes <- unique(all_genes[[1]])
    construct_visNetwork(networkEdgelist, new_color)
    session$sendCustomMessage("handler_startLoader", c(9,100))
    session$sendCustomMessage("handler_finishLoader", 9)
  }
}

# This event updates the Network2 slider, an event which is observed in server
# and consequently changes the Network and the respective table
# @param DB_source: one eligible datasource from available
# @return void
handleNetworkSelect2 <- function(DB_source, session){
  if (!identical(DB_source, "")){
    gostres_m <- all_gost[grepl(DB_source, all_gost$Source),]
    # to ALWAYS trigger an observed event from Heatmap slider, first update with a value fo 0
    updateSliderInput(session, "sliderNetwork2", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "sliderNetwork2", "Choose a number of results to view:", min = 2,
                      max = length(gostres_m$Term_ID), value = 10, step = 1)
    
  }
}

# This function draws a network2 and the corresponding data table
# @param networkSelect2: one eligible datasource from available
# @param sliderNetwork2: the user slider input
# @param sliderThreshold: cutoff range for edge values to show
# @param networkMode2: choose between two modes. the data are sorted in accordance to the chosen mode
#
# @return void
handleNetwork2 <- function(networkSelect2, sliderNetwork2, networkMode2, sliderThreshold, output, session){
  if (!identical(networkSelect2, "")){
    session$sendCustomMessage("handler_startLoader", c(11,30))
    networkGostres <- gostres[grepl(networkSelect2, gostres$Source), ] # without links
    all_gost_table <- all_gost[grepl(networkSelect2, all_gost$Source), ] # with links
    all_gost_table <- subset(all_gost_table, select=-c(`Positive Hits`)) #remove the Genes column from the table
    
    if (networkMode2 == "Enrichment Score") {
      networkGostres <- networkGostres[with(networkGostres,order(-`Enrichment Score %`)),]
      all_gost_table <- all_gost_table[with(all_gost_table,order(-`Enrichment Score %`)),]
    }
    networkGostres <- head(networkGostres, sliderNetwork2)
    all_gost_table <- head(all_gost_table, sliderNetwork2)
    session$sendCustomMessage("handler_startLoader", c(11,40))
    similarityColumnTable <- list()
    intersectGenesList <- list()
    for (i in 1:length(networkGostres$Function)){
      row <- networkGostres$`Positive Hits`[i]
      row <- paste(row, collapse=",") 
      row <- strsplit(row, ",")
      for (j in 1:length(networkGostres$Function)) {
        col <- networkGostres$`Positive Hits`[j]
        col <- paste(col, collapse=",") 
        col <- strsplit(col, ",")
        unionGenes <- append(col,row)
        unionGenes <- unlist(unionGenes)
        similarityScore <- ((length(intersect(col[[1]], row[[1]]))) / (length(unique(unionGenes))))*100
        similarityScore <- round(similarityScore, digits = 2)
        
        similarityColumnTable[[length(similarityColumnTable) + 1]] <- similarityScore
        intersectGenesList[[length(intersectGenesList) + 1]] <- as.list(intersect(col[[1]], row[[1]]))
      }
    }
    session$sendCustomMessage("handler_startLoader", c(11,60))
    #network input-networkEdgelist
    names <- paste(networkGostres$Term_ID, networkGostres$Function, sep="_")
    names <- expand.grid(as.list(names), as.list(names))
    networkEdgelist <- as.matrix(cbind(as.character(names$Var1), as.character(names$Var2), as.character(similarityColumnTable)))
    networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) > 0, ] 
    networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) >= sliderThreshold, ]
    new_color <- bar_colors[networkSelect2][[1]]
    construct_visNetwork2(networkEdgelist, new_color)
    session$sendCustomMessage("handler_startLoader", c(11,80))
    ##table-netTable
    combinedFunctions <- expand.grid(as.list(networkGostres$Function), as.list(networkGostres$Function))
    combinedTermIdsLinks <- expand.grid(as.list(all_gost_table$Term_ID), as.list(all_gost_table$Term_ID))
    netTable <- as.data.frame(cbind("Function_A"=combinedFunctions$Var1, "Term_ID_A"=combinedTermIdsLinks$Var1, "Function_B"=combinedFunctions$Var2,
                                    "Term_ID_B"=combinedTermIdsLinks$Var2, "Similarity Score %"= similarityColumnTable, "Intersection"= intersectGenesList))
    netTable$Function_A <- as.character(unlist(netTable$Function_A))
    netTable$Function_B <- as.character(unlist(netTable$Function_B))
    netTable$`Similarity Score %` <- as.numeric(as.character(netTable$`Similarity Score %`))
    
    # remove self rows from table
    for (i in nrow(netTable):1){
      if (identical(netTable$Function_A[i], netTable$Function_B[i])) netTable <- netTable[-i, ]
    }
    
    # removing zero entries
    netTable <- netTable[netTable$`Similarity Score %` > 0, ] 
    
    # removing reverse edges
    netTable <- netTable[!duplicated(apply(netTable[, c("Function_A", "Function_B" )],1,function(x) paste(sort(x), collapse='_'))),]
    netTable <- netTable[with(netTable,order(-`Similarity Score %`)),]
    
    netTable <- netTable[netTable$`Similarity Score %` >= sliderThreshold, ]
    columnVis<- c(2,3,4,5,6,7)
    output$network_table2 <- renderTableFunc(netTable, networkSelect2, 7, "function-function_network", "Positive Hits", columnVis)
    session$sendCustomMessage("handler_startLoader", c(11,100))
    session$sendCustomMessage("handler_finishLoader", 11)
  }
}
# This event updates the Network3 slider, an event which is observed in server
# and consequently changes the Network and the respective table
# @param DB_source: one eligible datasource from available
# @return void
handleNetworkSelect3 <- function(DB_source, session){
  if (!identical(DB_source, "")){
    gostres_m <- all_gost[grepl(DB_source, all_gost$Source),]
    # to ALWAYS trigger an observed event from Heatmap slider, first update with a value fo 0
    updateSliderInput(session, "sliderNetwork3", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "sliderNetwork3", "Choose a number of results to view:", min = 2,
                      max = length(gostres_m$Term_ID), value = 10, step = 1)
  }
}
# This event updates the sliderThreshold3 slider, an event which is observed by sliderNetwork3
# and consequently changes the Network3 and the respective table
# @param networkSelect3: one eligible datasource from available
# @return void
handleNetworkSlider3 <- function(sliderNetwork3, networkSelect3, session){
  if (!identical(networkSelect3, "")){
   # networkGostres <- gostres[grepl(networkSelect3, gostres$Source), ]
    # updateSliderInput(session, "sliderThreshold3", "Number of common Functions:", min = 0,
    #                   max = length(head(networkGostres$Function, sliderNetwork3)), value = 0, step = 1)
    
    updateSliderInput(session, "sliderThreshold3", "Number of common Functions:", min = 0,
                      max = sliderNetwork3, value = 0, step = 1)
  }
}

# This function draws a network3 and the corresponding data table
# @param networkSelect3: one eligible datasource from available
# @param sliderNetwork3: the user slider input
# @param sliderThreshold3: cutoff range for edge values to show
# @param networkMode3: choose between two modes. the data are sorted in accordance to the chosen mode
#
# @return void
handleNetwork3 <- function(networkSelect3, sliderNetwork3, networkMode3, sliderThreshold3, output, session){
  if (!identical(networkSelect3, "")){
    #   session$sendCustomMessage("handler_startLoader", 11)
    session$sendCustomMessage("handler_startLoader", c(26,30))
    networkGostres <- gostres[grepl(networkSelect3, gostres$Source), ] # without links
    #all_gost_table <- all_gost[grepl(networkSelect3, all_gost$Source), ] # with links
    #all_gost_table <- subset(all_gost_table, select=-c(`Positive Hits`)) #remove the Genes column from the table
    #lengthFunctions<- nrow(gostres[grepl(networkSelect3, gostres$Source), ]) # check
    
   # networkEdgelist<- geneGeneNetworkInput(networkMode3,networkGostres, sliderNetwork3, networkSelect3, sliderThreshold3)
    
    
    if (networkMode3 == "Enrichment Score") {
      networkGostres <- networkGostres[with(networkGostres,order(-`Enrichment Score %`)),]
      #all_gost_table <- all_gost_table[with(all_gost_table,order(-`Enrichment Score %`)),]
    }
    networkGostres <- head(networkGostres, sliderNetwork3)
    #all_gost_table <- head(all_gost_table, sliderNetwork3)
    col <- networkGostres$`Positive Hits`
    col <- paste(col, collapse=",")
    col <- strsplit(col, ",")
    genes<-unique(unlist(col))
    session$sendCustomMessage("handler_startLoader", c(26,40))
    similarityColumn<-list()
    intersectFunctions <- list()
    #numberCommonFunctionsList <- list()
    totalFunctions<- list()
    for (i in 1:length(genes)){
      for (j in 1:length(genes)) {
        commonFunctions <- networkGostres [grepl(genes[i], networkGostres$`Positive Hits`) & grepl(genes[j], networkGostres$`Positive Hits`),]
        nameCommonFunctions<- commonFunctions$Function
        similarityScore<- nrow(commonFunctions)
        #similarityScore <- (numberCommonFunctions/lengthFunctions)*100
        #similarityScore <- round(similarityScore, digits = 2)
        similarityColumn[[length(similarityColumn) + 1]] <- similarityScore
        intersectFunctions[[length(intersectFunctions) + 1]] <- as.list(nameCommonFunctions)
        #numberCommonFunctionsList[[length(numberCommonFunctionsList) + 1]] <- numberCommonFunctions
        #totalFunctions[[length(totalFunctions) + 1]] <- lengthFunctions
        
      }
    }
    session$sendCustomMessage("handler_startLoader", c(26,70))
    combinedGenes <- expand.grid(as.list(genes), as.list(genes))
    
    #network input-networkEdgelist
    networkEdgelist <- as.matrix(cbind(as.character(combinedGenes$Var1), as.character(combinedGenes$Var2), as.character(similarityColumn)))
    networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) > 0, ]
    networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) >= sliderThreshold3, ]
    new_color <- bar_colors[networkSelect3][[1]]
    fonts <- c("GO:MF"= "white", "GO:BP"= "black", "GO:CC" = "black",
               "KEGG" = "black", "REAC" = "white", "WP" = "black", "TF" = "black",
               "MIRNA" = "black", "HPA" = "black", "CORUM" = "black", "HP" = "white")
    fontColor <- fonts[networkSelect3][[1]]
    construct_visNetwork3(networkEdgelist, new_color,fontColor)
    
    #table-netTable
    
    netTable <- as.data.frame(cbind("Gene_A"= combinedGenes$Var1, "Gene_B"=combinedGenes$Var2, "Common_Functions"= similarityColumn, "Total_Functions"= rep(sliderNetwork3, length(combinedGenes$Var1)), "Functions"= intersectFunctions))
    netTable$Gene_A <- as.character(unlist(netTable$Gene_A))
    netTable$Gene_B <- as.character(unlist(netTable$Gene_B))
    netTable$Common_Functions <- as.numeric(as.character(netTable$Common_Functions))
    
    # remove self rows from table
    for (i in nrow(netTable):1){
      if (identical(netTable$Gene_A[i], netTable$Gene_B[i])) netTable <- netTable[-i, ]
    }
    
    # removing zero entries
    netTable <- netTable[netTable$Common_Functions > 0, ]
    # removing reverse edges
    netTable <- netTable[!duplicated(apply(netTable[, c("Gene_A", "Gene_B" )],1,function(x) paste(sort(x), collapse='_'))),]
    netTable <- netTable[with(netTable,order(-Common_Functions)),]
    netTable <- netTable[netTable$Common_Functions >= sliderThreshold3, ]
   
    columnVis<- c(2,3,4,5,6)
    
    output$network_table3 <- renderTableFunc(netTable, networkSelect3, 6, "gene-gene_network", "Functions",columnVis)
    session$sendCustomMessage("handler_startLoader", c(26,100))
    session$sendCustomMessage("handler_finishLoader", 26) 
  }
}



###  aGotool Plots ####

# This event updates the scatterplot slider, an event which is observed in server
# and consequently changes the scatterplot and the respective table
# @param aGoScatterSelect: one eligible datasource from available
# @return void
handleaGoScatterSelect <- function(aGoScatterSelect, session){
  if (!identical(aGoScatterSelect, "")){
    plot_aGotool <- all_aGotool[grepl(aGoScatterSelect,  all_aGotool$Source),]
    # to ALWAYS trigger an observed event from scatterplot slider, first update with a value fo 0
    updateSliderInput(session, "aGoSliderScatter", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "aGoSliderScatter", "Choose a number of results to view:", min = 1,
                      max = length(plot_aGotool$Term_ID), value = 10, step = 1)
  }
}

# # This function draws a scatterplot and the corresponding data table
# # @param DB_source: one eligible datasource from available
# # @param sliderScatter: the user slider input
# # @return void
handleaGoScatterPlot <- function(aGoScatterSelect, aGoSliderScatter,  output, session){ #aGoScatterMode,
  if (!identical(aGoScatterSelect, "")){
    session$sendCustomMessage("handler_startLoader", c(14,30))
    new_color <- aGoBar_colors[aGoScatterSelect][[1]]
    DB_table <- all_aGotool[grepl(aGoScatterSelect, all_aGotool$Source), ]
    DB_table <- subset(DB_table, select=-c( `Positive Hits`))
    DB_table <- DB_table[with(DB_table,order(-`Enrichment Score %`)),]
    session$sendCustomMessage("handler_startLoader", c(14,60))
    output$aGoScatterPlot <- renderUI({plotlyOutput("aGoScatter", height = height_plots(aGoSliderScatter, 15, 110))})
    #if(aGoScatterMode =="Log10Pavlue - Enrichment Score"){
    
    output$aGoScatter <- renderPlotly({
      
      ggplot(DB_table[1:aGoSliderScatter,], aes(`Enrichment Score %`, `-log10Pvalue`,text=paste("TERM_ID: ", Term_ID,"\n", "FUNCTION: ", Function, sep=""))) + geom_point(fill=new_color,color="black", position = position_dodge(width = 0.8), alpha = 0.7, pch=21, size=3, stroke=0.3)+ labs(x = "ENRICHMENT SCORE %")+ labs(title = aGoScatterSelect )
    })
    session$sendCustomMessage("handler_startLoader", c(14,100))
    output$aGoScatter_table <- DT::renderDataTable(head(DB_table, aGoSliderScatter), server = FALSE,
                                                   extensions = 'Buttons',
                                                   options = list(
                                                     "dom" = 'T<"clear">lBfrtip',
                                                     buttons = list(list(extend='excel',filename=paste('ScatterPlot_Table_', aGoScatterSelect, sep="")),
                                                                    list(extend= 'csv',filename=paste('ScatterPlot_Table_', aGoScatterSelect, sep="")),
                                                                    list(extend='copy',filename=paste('ScatterPlot_Table_', aGoScatterSelect, sep="")),
                                                                    list(extend='pdf',filename=paste('ScatterPlot_Table_', aGoScatterSelect, sep="")),
                                                                    list(extend='print',filename=paste('ScatterPlot_Table_', aGoScatterSelect, sep="")))
                                                   ),rownames= FALSE, escape=F)
    session$sendCustomMessage("handler_finishLoader", 14)
  }
}


# This event updates the barplot slider, an event which is observed in server
# and consequently changes the barplot and the respective table
# Not called when no sources are selected
# @param DB_source: multiple eligible datasources from available
# @return void
handleaGoBarSelect2 <- function(aGoBarSelect2, session){
  plot_aGotool <- all_aGotool[grepl(paste(aGoBarSelect2, collapse="|"), all_aGotool$Source),] # collapse="|", instead of for loop
  # to ALWAYS trigger an observed event from barplot slider, first update with a value fo 0
  updateSliderInput(session, "aGoSliderBarplot", "Choose a number of results to view:", min = 1, 
                    max = 1, value = 0, step = 1)
  updateSliderInput(session, "aGoSliderBarplot", "Choose a number of results to view:", min = 1,
                    max = length(plot_aGotool$Term_ID), value = 10, step = 1)
  
}


# This function draws a barplot from multiple selected sources and the corresponding data table
# @param aGoBarSelect2: multiple eligible datasource from available
# @param aGoSliderBarplot: the user slider input
# @return void
handleaGoBarPlot2 <- function(aGoBarSelect2, aGoSliderBarplot, aGoBarplotMode, output, session){
  if (!identical(aGoBarSelect2, NULL)){
    session$sendCustomMessage("handler_startLoader", c(15,30))
    DB_table <- aGotoolResults[grepl(paste(aGoBarSelect2, collapse="|"), aGotoolResults$Source),] # collapse="|", instead of for loop, without links
    all_aGo_table <- all_aGotool[grepl(paste(aGoBarSelect2, collapse="|"), all_aGotool$Source),]
    DB_table <- subset(DB_table, select=-c( `Positive Hits`))
    all_aGo_table <- subset(all_aGo_table, select=-c(`Positive Hits`))
   
    #make a figure legend based on DB_source
    
    fonts <- c("PFAM" = "white", "INTERPRO" = "white", "UniProt" = "black", "Disease Ontology" = "white")
    fig_legend = "<table style='border-spacing: 0;border-collapse: collapse;'><tr><td><b>Colors:&nbsp;&nbsp;</b></td>"
    for(i in 1:length(aGoBarSelect2)){
      fig_legend<-paste(fig_legend, sprintf("<td style='background-color:%s; color:%s;border: 1px solid black'>&nbsp; %s &nbsp;</td>", aGoBar_colors[aGoBarSelect2[i]][[1]][1], fonts[aGoBarSelect2[i]][[1]][1], aGoBarSelect2[i]), sep="")
      }
    fig_legend<-paste(fig_legend, "</tr></table>")
    output$bar_legend_aGo<- renderUI(HTML(fig_legend))
    session$sendCustomMessage("handler_startLoader", c(15,40))
    # Check mode of execution
    if(aGoBarplotMode == "-log10Pavlue"){
      output$aGoBarplot <- renderUI({plotlyOutput("aGoBarplot1", height = height_plots(aGoSliderBarplot, 18, 100))})
      DB_table <- DB_table[with(DB_table, order(-(`-log10Pvalue`), Function)),] # changed factor to double in the global variable
      DB_table <- head(DB_table, aGoSliderBarplot)
      all_aGo_table <- all_aGo_table[with(all_aGo_table, order(-(`-log10Pvalue`), Function)),] # changed factor to double in the global variable
      all_aGo_table <- head(all_aGo_table, aGoSliderBarplot)
      DB_table <- DB_table[with(DB_table, order( Function, decreasing = T)),]
      DB_table <- DB_table[with(DB_table, order(-`-log10Pvalue`)),]
      new_colors <- as.character(aGoBar_colors[as.character(DB_table$Source)])
     
      DB_table$Term_ID <- factor(DB_table$Term_ID, levels = unique(DB_table$Term_ID)[order(DB_table$ `-log10Pvalue`, decreasing = F)])
      output$aGoBarplot1 <- renderPlotly({
        plot_ly(x =  DB_table$`-log10Pvalue`, y = DB_table$Term_ID, 
                marker=list(color=new_colors), 
                type = 'bar', orientation = 'h', 
                text =  DB_table$`Intersection Size`,
                textposition = 'auto',
                textangle = 0,
                hoverinfo = "text",
                hovertext =paste("TERM ID: ",DB_table$Term_ID, "\n-LOG10PAVULE: ",DB_table$`-log10Pvalue`, "\nFUNCTION: ", DB_table$Function,"\n", "ENRICHMENT SCORE %: ", DB_table$`Enrichment Score %`, sep=""))%>% 
          layout(title = paste(aGoBarSelect2, collapse="_"),xaxis = list(title = "-LOG10PVALUE "), yaxis=list(title="TERM ID")) 
        
      })
      
      
      # output$aGoBarplot1 <- renderPlotly({
      #   
      #   ggplot(DB_table, aes(Term_ID, `-log10Pvalue`, text=paste("FUNCTION: ", Function,"\n", "`Enrichment Score %`: ", `Enrichment Score %`, sep=""))) + geom_bar(stat='identity', fill= new_colors) + coord_flip()+ labs(y = "-LOG10PAVLUE ")+ labs(title = paste(aGoBarSelect2, collapse="_") ) +
      #     geom_text(aes(label=`Intersection Size`), vjust=+0.5, size=3.5) + scale_x_discrete(limits = DB_table$Term_ID[order(DB_table$`-log10Pvalue`)])
      # })
      
    }
    else if(aGoBarplotMode == "Enrichment Score") { # Enrichment Mode
      DB_table <- DB_table[with(DB_table, order(-`Enrichment Score %`, Function)),]
      DB_table <- head(DB_table, aGoSliderBarplot)
      all_aGo_table <- all_aGo_table[with(all_aGo_table, order(-`Enrichment Score %`, Function)),]
      all_aGo_table <- head(all_aGo_table, aGoSliderBarplot)
      DB_table <- DB_table[with(DB_table, order( Function, decreasing = T)),]
      DB_table <- DB_table[with(DB_table, order(-`Enrichment Score %`)),]
      new_colors <- as.character(aGoBar_colors[as.character(DB_table$Source)])
      
      output$aGoBarplot <- renderUI({plotlyOutput("aGoBarplot1", height = height_plots(aGoSliderBarplot, 18, 100))})
      # output$aGoBarplot1 <- renderPlotly({
      #   
      #   ggplot(DB_table, aes(Term_ID, `Enrichment Score %`, text=paste("FUNCTION: ", Function,"\n", "-LOG_PVALUE: ", `-log10Pvalue`, sep=""))) + 
      #     geom_bar(stat='identity',fill= new_colors)+coord_flip()+labs(y = "ENRICHMENT SCORE % ")+ labs(title = paste(aGoBarSelect2, collapse="_") ) +
      #     geom_text(aes(label=`Intersection Size`), vjust=+0.5, size=3.5) + 
      #     scale_x_discrete(limits = DB_table$Term_ID[order(DB_table$`Enrichment Score %`)])+
      #     scale_colour_manual(values = as.vector(new_colors))
      # })
      DB_table$Term_ID <- factor(DB_table$Term_ID, levels = unique(DB_table$Term_ID)[order(DB_table$ `Enrichment Score %`, decreasing = F)])
      output$aGoBarplot1 <- renderPlotly({
        plot_ly(x =  DB_table$`Enrichment Score %`, y = DB_table$Term_ID, 
                marker=list(color=new_colors), 
                type = 'bar', orientation = 'h', 
                text =  DB_table$`Intersection Size`,
                textposition = 'auto',
                textangle = 0,
                hoverinfo = "text",
                hovertext =paste("TERM ID: ",DB_table$Term_ID, "\n-LOG10PAVULE: ",DB_table$`-log10Pvalue`, "\nFUNCTION: ", DB_table$Function,"\n", "ENRICHMENT SCORE %: ", DB_table$`Enrichment Score %`, sep=""))%>% 
          layout(title = paste(aGoBarSelect2, collapse="_"),xaxis = list(title = "ENRICHMENT SCORE"), yaxis=list(title="TERM ID")) 
        
      })
    }
    session$sendCustomMessage("handler_startLoader", c(15,70))
    output$aGoBarplot_table <- DT::renderDataTable(all_aGo_table, server = FALSE,
                                                   extensions = 'Buttons',
                                                   options = list(
                                                     "dom" = 'T<"clear">lBfrtip',
                                                     buttons = list(list(extend='excel',filename=paste(paste(aGoBarSelect2, collapse="_"), '_BarPlot_Table_EnScore', sep="")),
                                                                    list(extend= 'csv',filename=paste(paste(aGoBarSelect2, collapse="_"), '_BarPlot_Table_EnScore', sep="")),
                                                                    list(extend='copy',filename=paste(paste(aGoBarSelect2, collapse="_"), '_BarPlot_Table_EnScore', sep="")),
                                                                    list(extend='pdf',filename=paste(paste(aGoBarSelect2, collapse="_"), '_BarPlot_Table_EnScore', sep="")),
                                                                    list(extend='print',filename=paste(paste(aGoBarSelect2, collapse="_"), '_BarPlot_Table_EnScore', sep="")))
                                                   ),rownames= FALSE, escape=F)
    
    session$sendCustomMessage("handler_startLoader", c(15,100))
    session$sendCustomMessage("handler_finishLoader", 15)
  }
}

# This event updates the aGo Heatmap slider, an event which is observed in server
# and consequently changes the Heatmap and the respective table
# @param aGoHeatmapSelect: one eligible datasource from available
# @return void
handleaGoHeatmapSelect <- function(aGoHeatmapSelect, session){
  if (!identical(aGoHeatmapSelect, "")){
    plot_aGotool <- all_aGotool[grepl(aGoHeatmapSelect, all_aGotool$Source),]
    # to ALWAYS trigger an observed event from Heatmap slider, first update with a value fo 0
    updateSliderInput(session, "aGoSliderHeatmap", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "aGoSliderHeatmap", "Choose a number of results to view:", min = 2,
                      max = length(plot_aGotool$Term_ID), value = 10, step = 1)
  }
}

# This function draws a aGo heatmap and the corresponding data table
# @param aGoHeatmapSelect: one eligible datasource from available
# @param aGoSliderHeatmap: the user slider input
# @param aGoHeatmapAxis: reverse the heatmap s axis
# @param aGoHeatmapMode: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleaGoHeatMap <- function(aGoHeatmapSelect, aGoSliderHeatmap, aGoHeatmapAxis, aGoHeatmapMode, output, session){
  if (!identical(aGoHeatmapSelect, "")){
    session$sendCustomMessage("handler_startLoader", c(16,30))
    new_color <- aGoBar_colors[aGoHeatmapSelect][[1]]
    aGoHeatmap <- aGotoolResults[grepl(aGoHeatmapSelect, aGotoolResults$Source), ]
    all_aGotool_table <- all_aGotool[grepl(aGoHeatmapSelect, all_aGotool$Source), ]
    if (length(aGoHeatmap$Function) == 1) session$sendCustomMessage("handler_alert", "Cannot create a clustered heatmap if the results<2.")
    else{
      if (aGoHeatmapMode == "Enrichment Score") {
        aGoHeatmap <- aGoHeatmap[with(aGoHeatmap,order(-`Enrichment Score %`)),]
        all_aGotool_table <- all_aGotool_table[with(all_aGotool_table,order(-`Enrichment Score %`)),]
        
      }
      
      aGoHeatmap <- head(aGoHeatmap, aGoSliderHeatmap)
      all_aGotool_table <- head(all_aGotool_table, aGoSliderHeatmap)
      output$aGoHeatmap_table <- DT::renderDataTable( subset(all_aGotool_table, select=-c(`Positive Hits`)), server = FALSE, 
                                                      extensions = 'Buttons',
                                                      options = list("dom" = 'T<"clear">lBfrtip',
                                                                     buttons = list(list(extend='excel',filename=paste(aGoHeatmapSelect, '_Heatmap1', sep="")),
                                                                                    list(extend= 'csv',filename=paste(aGoHeatmapSelect, '_Heatmap1', sep="")),
                                                                                    list(extend='copy',filename=paste(aGoHeatmapSelect, '_Heatmap1', sep="")),
                                                                                    list(extend='pdf',filename=paste(aGoHeatmapSelect, '_Heatmap1', sep="")),
                                                                                    list(extend='print',filename=paste(aGoHeatmapSelect, '_Heatmap1', sep="")))
                                                      ),rownames= FALSE, escape=F)
      session$sendCustomMessage("handler_startLoader", c(16,40))
      all_genes <- paste(aGoHeatmap$`Positive Hits`, collapse=",") 
      all_genes <- strsplit(all_genes, ",")
      all_genes <- unique(all_genes[[1]])
      id <- aGoHeatmap$Term_ID
      functions <- aGoHeatmap$Function
      heatmapTable <- as.data.frame(matrix(0, ncol = length(all_genes), nrow = length(functions)))
      colnames(heatmapTable) <- all_genes
      rownames(heatmapTable) <- id #paste(id, functions, sep="_")
      score <- paste(aGoHeatmap$`-log10Pvalue`, collapse=",")
      score <- strsplit(score , ",")
      score <- as.numeric(as.character(score[[1]]))
      score_en <- paste(aGoHeatmap$`Enrichment Score %` , collapse=",")
      score_en <- strsplit(score_en , ",")
      score_en <- as.numeric(as.character(score_en[[1]]))
      session$sendCustomMessage("handler_startLoader", c(16,50))
      for (i in 1:length(functions)) heatmapTable[i, match(strsplit(as.character(aGoHeatmap$`Positive Hits`[i]), ",")[[1]], all_genes)] <- score[i]
      session$sendCustomMessage("handler_startLoader", c(16,60))
      if (aGoHeatmapMode == "Enrichment Score") {for (i in 1:length(functions)) heatmapTable[i, match(strsplit(as.character(aGoHeatmap$`Positive Hits`[i]), ",")[[1]], all_genes)] <- score_en[i]}
      
      session$sendCustomMessage("handler_startLoader", c(16,70))
      hoverLabelsTable <- heatmapTable                # duplicate the initial input and change the rownames.
      rownames(hoverLabelsTable) <- functions         # custom_hovertext parameter takes as an input a table with ta same dimensions... kt tetoio
      hoverLabelsTable[] <- paste("FUNCTION: ", rownames(hoverLabelsTable))
      hoverLabelsTable[] <- lapply(colnames(hoverLabelsTable), function(colname) {
        paste0(hoverLabelsTable[, colname], ", ", colname)
      })
      
      session$sendCustomMessage("handler_startLoader", c(16,90))
      if (aGoHeatmapAxis == "Genes-Functions") {
        heatmapTable <- t(heatmapTable)
        hoverLabelsTable <- t(hoverLabelsTable)
        genesNumber <- length(all_genes)
        
        
        output$aGoHeatmapPlot <- renderUI(plotlyOutput("aGoPlotHeatmap", height = height_plots(genesNumber, 15, 500)))
        output$aGoPlotHeatmap <- renderPlotly({
          
          heatmaply(heatmapTable,custom_hovertext = hoverLabelsTable, 
                    xlab = "FUNCTIONS",ylab = "GENES",main = aGoHeatmapSelect, fontsize_row = 10, fontsize_col = 10, 
                    grid_gap = 1,show_dendrogram = c(F, F), #margins = c(NA, NA, 60, NA),
                    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = new_color)
          )
        })
      }
      else {
        output$aGoHeatmapPlot <- renderUI(plotlyOutput("aGoPlotHeatmap", height = height_plots(aGoSliderHeatmap, 15, 200)))
        output$aGoPlotHeatmap <- renderPlotly({
          
          heatmaply(heatmapTable, custom_hovertext = hoverLabelsTable, 
                    xlab = "GENES",ylab = "FUNCTIONS",main = aGoHeatmapSelect, fontsize_row = 10,fontsize_col = 10,
                    show_dendrogram = c(F, F), grid_gap = 1, #margins = c(NA, NA, 60, NA),
                    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = new_color)
                    
          )
        })
      }
    }
    session$sendCustomMessage("handler_startLoader", c(16,100))
    session$sendCustomMessage("handler_finishLoader", 16)
  }#end of first if
}

# This event updates the Heatmap2 slider, an event which is observed in server
# and consequently changes the Heatmap and the respective table
# @param aGoHeatmapSelect2: one eligible datasource from available
# @return void
handleaGoHeatmapSelect2 <- function(aGoHeatmapSelect2, session){
  if (!identical(aGoHeatmapSelect2, "")){
    plot_aGotool <- all_aGotool[grepl(aGoHeatmapSelect2, all_aGotool$Source),]
    # to ALWAYS trigger an observed event from Heatmap slider, first update with a value fo 0
    updateSliderInput(session, "aGoSliderHeatmap2", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "aGoSliderHeatmap2", "Choose a number of results to view:", min = 2,
                      max = length(plot_aGotool$Term_ID), value = 10, step = 1)
  }
}

# This function draws a heatmap2 and the corresponding data table
# @param aGoHeatmapSelect2: one eligible datasource from available
# @param sliderHeatmap: the user slider input
# @param heatmapAxis: reverse the heatmap s axis
# @param heatmapMode: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleaGoHeatMap2<- function(aGoHeatmapSelect2, aGoSliderHeatmap2, aGoHeatmapMode2, output, session){
  if (!identical(aGoHeatmapSelect2, "")){
    session$sendCustomMessage("handler_startLoader", c(17,30))
    new_color <- aGoBar_colors[aGoHeatmapSelect2][[1]]
    heatmapaGo <- aGotoolResults[grepl(aGoHeatmapSelect2, aGotoolResults$Source), ]
    all_aGo_table <- all_aGotool[grepl(aGoHeatmapSelect2, all_aGotool$Source), ] #with links
    session$sendCustomMessage("handler_startLoader", c(17,40))
    
    if (length(heatmapaGo$Function) == 1) session$sendCustomMessage("handler_alert", "Cannot create a clustered heatmap if the results<2.")
    else{
      
      if (aGoHeatmapMode2 == "Enrichment Score") {
        heatmapaGo <- heatmapaGo[with(heatmapaGo,order(-`Enrichment Score %`)),]
        all_aGo_table <- all_aGo_table[with(all_aGo_table,order(-`Enrichment Score %`)),]
      }
      session$sendCustomMessage("handler_startLoader", c(17,50))
      
      heatmapaGo <- head(heatmapaGo, aGoSliderHeatmap2)
      all_aGo_table <- head(all_aGo_table, aGoSliderHeatmap2)
      heatmapTable <- as.data.frame(matrix(0, ncol = length(heatmapaGo$Function), nrow = length(heatmapaGo$Function)))
      colnames(heatmapTable) <- heatmapaGo$Term_ID #paste(heatmapaGo$Term_ID, heatmapaGo$Function, sep="_")
      rownames(heatmapTable) <- heatmapaGo$Term_ID #paste(heatmapaGo$Term_ID, heatmapaGo$Function, sep="_")
      session$sendCustomMessage("handler_startLoader", c(17,60))
      
      similarityColumnTable <- list()
      intersectGenesList <- list()
      for (i in 1:length(heatmapaGo$Function)){
        row <- heatmapaGo$`Positive Hits`[i]
        row <- paste(row, collapse=",") 
        row <- strsplit(row, ",")
        for (j in 1:length(heatmapaGo$Function)) {
          col <- heatmapaGo$`Positive Hits`[j]
          col <- paste(col, collapse=",") 
          col <- strsplit(col, ",")
          unionGenes<-append(col,row)
          unionGenes<- unlist(unionGenes)
          similarityScore=((length(intersect(col[[1]], row[[1]])))/ (length(unique(unionGenes))))*100
          similarityScore= round(similarityScore, digits = 2)
          heatmapTable[i,j] <- similarityScore
          similarityColumnTable[[length(similarityColumnTable) + 1]] <- similarityScore
          intersectGenesList[[length(intersectGenesList) + 1]] <- as.list(intersect(col[[1]], row[[1]]))
        }
      }
      session$sendCustomMessage("handler_startLoader", c(17,80))
      
      combinedFunctions <- expand.grid(as.list(heatmapaGo$Function), as.list(heatmapaGo$Function))
      combinedTermIdsLinks <- expand.grid(as.list(all_aGo_table$Term_ID), as.list(all_aGo_table$Term_ID))
      heatTable <- as.data.frame(cbind("Function_A"=combinedFunctions$Var1,"Term_ID_A"= combinedTermIdsLinks$Var1,
                                       "Function_B"=combinedFunctions$Var2,"Term_ID_B"= combinedTermIdsLinks$Var2, 
                                       "Similarity Score %"= similarityColumnTable,"Intersection"= intersectGenesList))
      heatTable$Function_A <- as.character(unlist(heatTable$Function_A))
      heatTable$Function_B <- as.character(unlist(heatTable$Function_B))
      heatTable$`Similarity Score %` <- as.numeric(as.character(heatTable$`Similarity Score %`))
      session$sendCustomMessage("handler_startLoader", c(17,90))
      
      # remove self rows from table
      for (i in nrow(heatTable):1){ 
        if (identical(heatTable$Function_A[i], heatTable$Function_B[i])) heatTable <- heatTable[-i, ]
      }
      # removing zero entries
      heatTable <- heatTable[heatTable$`Similarity Score %` > 0, ] 
      
      # removing reverse edges
      heatTable <- heatTable[!duplicated(apply(heatTable[, c("Function_A", "Function_B" )],1,function(x) paste(sort(x), collapse='_'))),]
      heatTable <- heatTable[with(heatTable,order(-`Similarity Score %`)),]
      
      columnVis<- c(2,3,4,5,6,7)
      output$aGoHeatmap_table2 <- renderTableFunc(heatTable, aGoHeatmapSelect2, 7, "function-function_heatmap", "Positive Hits", columnVis)
      
      # output$aGoHeatmap_table2 <- DT::renderDataTable(heatTable, server = FALSE,
      #                                                 extensions = 'Buttons',
      #                                                 options = list("dom" = 'T<"clear">lBfrtip',
      #                                                                buttons = list(list(extend='excel',filename=paste(aGoHeatmapSelect2, '_Heatmap2', sep="")),
      #                                                                               list(extend= 'csv',filename=paste(aGoHeatmapSelect2, '_Heatmap2', sep="")),
      #                                                                               list(extend='copy',filename=paste(aGoHeatmapSelect2, '_Heatmap2', sep="")),
      #                                                                               list(extend='pdf',filename=paste(aGoHeatmapSelect2, '_Heatmap2', sep="")),
      #                                                                               list(extend='print',filename=paste(aGoHeatmapSelect2, '_Heatmap2', sep="")))
      #                                                 ),rownames= FALSE, escape=F)

       output$aGoHeatmapPlot2 <- renderUI(plotlyOutput("drawaGoHeatmapPlot2", height = height_plots(aGoSliderHeatmap2, 18, 500)))
      output$drawaGoHeatmapPlot2 <- renderPlotly({
       
        heatmaply(heatmapTable,
                  xlab = "FUNCTIONS",ylab = "FUNCTIONS", main = aGoHeatmapSelect2, fontsize_row = 10, fontsize_col = 10, grid_gap = 1,
                  show_dendrogram = c(F, F),  #margins = c(NA, NA, 70, NA),
                  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = new_color)
        )
      })
    }
    session$sendCustomMessage("handler_startLoader", c(17,100))
    
    session$sendCustomMessage("handler_finishLoader", 17)
  }
}





# This event updates the Network slider, an event which is observed in server
# and consequently changes the Network and the respective table
# @param aGoNetworkSelect: one eligible datasource from available
# @return void
handleaGoNetworkSelect <- function(aGoNetworkSelect, session){
  if (!identical(aGoNetworkSelect, "")){
    plot_aGotool <- all_aGotool[grepl(aGoNetworkSelect, all_aGotool$Source),]
    # to ALWAYS trigger an observed event from Heatmap slider, first update with a value fo 0
    updateSliderInput(session, "aGoSliderNetwork", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "aGoSliderNetwork", "Choose a number of results to view:", min = 2,
                      max = length(plot_aGotool$Term_ID), value = 10, step = 1)
  }
}
# This function draws a network and the corresponding data table
# @param aGoNetworkSelect: one eligible datasource from available
# @param aGoSliderNetwork: the user slider input
# @param aGoNetworkMode: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleaGoNetwork <- function(aGoNetworkSelect, aGoSliderNetwork, aGoNetworkMode, output, session){
  if (!identical(aGoNetworkSelect, "")){
    session$sendCustomMessage("handler_startLoader", c(18,30))
    networkaGo <- aGotoolResults[grepl(aGoNetworkSelect, aGotoolResults$Source), ] # without links
    aGo_table <- all_aGotool[grepl(aGoNetworkSelect, all_aGotool$Source), ] # with links
    aGo_table <- subset(aGo_table, select=-c(`Positive Hits`)) #remove the Genes column from the table
    session$sendCustomMessage("handler_startLoader", c(18,50))
    if (aGoNetworkMode == "Enrichment Score") {
      networkaGo <- networkaGo[with(networkaGo,order(-`Enrichment Score %`)),]
      aGo_table <- aGo_table[with(aGo_table,order(-`Enrichment Score %`)),]
    }
    
    
    output$aGoNetwork_table <- DT::renderDataTable(head(aGo_table, aGoSliderNetwork), server = FALSE, 
                                                   extensions = 'Buttons',
                                                   options = list("dom" = 'T<"clear">lBfrtip',
                                                                  buttons = list(list(extend='excel', filename=paste(aGoNetworkSelect, '_Network', sep="")),
                                                                                 list(extend= 'csv', filename=paste(aGoNetworkSelect, '_Network', sep="")),
                                                                                 list(extend='copy', filename=paste(aGoNetworkSelect, '_Network', sep="")),
                                                                                 list(extend='pdf', filename=paste(aGoNetworkSelect, '_Network', sep="")),
                                                                                 list(extend='print', filename=paste(aGoNetworkSelect, '_Network', sep="")))
                                                   ), rownames= FALSE, escape=F)
    session$sendCustomMessage("handler_startLoader", c(18,70))
    networkaGo <- head(networkaGo,aGoSliderNetwork) 
    networkEdgelist <- matrix("", nrow = 0, ncol = 2) #3 
    for (i in 1:nrow(networkaGo)){
      name <- paste(networkaGo$Term_ID[i], networkaGo$Function[i], sep="_")
      lineGenes <- strsplit(as.character(networkaGo$`Positive Hits`[i]), ",")[[1]]
      for (j in 1:length(lineGenes)) networkEdgelist <- rbind(networkEdgelist, c(name, lineGenes[j]))
    }
    
    new_color <- aGoBar_colors[aGoNetworkSelect][[1]]
    
    all_genes <- paste(networkaGo$`Positive Hits`, collapse=",") 
    all_genes <- strsplit(all_genes, ",")
    all_genes <- unique(all_genes[[1]])
    session$sendCustomMessage("handler_startLoader", c(18,80))
    construct_visNetworkaGo(networkEdgelist, new_color)
    session$sendCustomMessage("handler_startLoader", c(18,100))
    session$sendCustomMessage("handler_finishLoader", 18)
  }
}

#This event updates the aGo Network2 slider, an event which is observed in server
# and consequently changes the Network and the respective table
# @param aGoNetworkSelect2: one eligible datasource from available
# @return void
handleaGoNetworkSelect2 <- function(aGoNetworkSelect2, session){
  if (!identical(aGoNetworkSelect2, "")){
    plot_aGotool <- all_aGotool[grepl(aGoNetworkSelect2, all_aGotool$Source),]
    # to ALWAYS trigger an observed event from Heatmap slider, first update with a value fo 0
    updateSliderInput(session, "aGoSliderNetwork2", "Choose a number of results to view:", min = 1, 
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "aGoSliderNetwork2", "Choose a number of results to view:", min = 2,
                      max = length(plot_aGotool$Term_ID), value = length(plot_aGotool$Term_ID), step = 1)
    
  }
}

# This function draws a aGO network2 and the corresponding data table
# @param aGoNetworkSelect2: one eligible datasource from available
# @param sliderNetwork2: the user slider input
# @param sliderThreshold: cutoff range for edge values to show
# @param aGoNetworkMode2: choose between two modes. the data are sorted in accordance to the chosen mode
#
# @return void
handleaGoNetwork2 <- function(aGoNetworkSelect2, aGoSliderNetwork2, aGoNetworkMode2,  aGoSliderThreshold, output, session){
  if (!identical(aGoNetworkSelect2, "")){
    session$sendCustomMessage("handler_startLoader", c(19,30))
    networkAGo2 <- aGotoolResults[grepl(aGoNetworkSelect2, aGotoolResults$Source), ] # without links
    all_aGo_table <- all_aGotool[grepl(aGoNetworkSelect2, all_aGotool$Source), ] # with links
    all_aGo_table <- subset(all_aGo_table, select=-c(`Positive Hits`)) #remove the Genes column from the table
    
    if (length(networkAGo2$Function) == 1) session$sendCustomMessage("handler_alert", paste(aGoNetworkSelect2," Cannot create a Function Vs Function network if number of results equals 1.", sep=":"))
    else{
      
      if (aGoNetworkMode2 == "Enrichment Score") {
        networkAGo2 <- networkAGo2[with(networkAGo2,order(-`Enrichment Score %`)),]
        all_aGo_table <- all_aGo_table[with(all_aGo_table,order(-`Enrichment Score %`)),]
      }
      session$sendCustomMessage("handler_startLoader", c(19,40))
      
      networkAGo2 <- head(networkAGo2, aGoSliderNetwork2)
      all_aGo_table <- head(all_aGo_table, aGoSliderNetwork2)
      
      similarityColumnTable <- list()
      intersectGenesList <- list()
      for (i in 1:length(networkAGo2$Function)){
        row <- networkAGo2$`Positive Hits`[i]
        row <- paste(row, collapse=",") 
        row <- strsplit(row, ",")
        for (j in 1:length(networkAGo2$Function)) {
          col <- networkAGo2$`Positive Hits`[j]
          col <- paste(col, collapse=",") 
          col <- strsplit(col, ",")
          unionGenes <- append(col,row)
          unionGenes <- unlist(unionGenes)
          similarityScore <- ((length(intersect(col[[1]], row[[1]]))) / (length(unique(unionGenes))))*100
          similarityScore <- round(similarityScore, digits = 2)
          
          similarityColumnTable[[length(similarityColumnTable) + 1]] <- similarityScore
          intersectGenesList[[length(intersectGenesList) + 1]] <- as.list(intersect(col[[1]], row[[1]]))
        }
      }
      session$sendCustomMessage("handler_startLoader", c(19,70))
      
      #network input-networkEdgelist
      names <- paste(networkAGo2$Term_ID, networkAGo2$Function, sep="_")
      names <- expand.grid(as.list(names), as.list(names))
      networkEdgelist <- as.matrix(cbind(as.character(names$Var1), as.character(names$Var2), as.character(similarityColumnTable)))
      networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) > 0, ]
      networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) >= aGoSliderThreshold, ]
      if (identical(networkEdgelist[,1], networkEdgelist[,2])== TRUE) session$sendCustomMessage("handler_alert", paste(aGoNetworkSelect2," Cannot create a Function Vs Function network. No common genes found", sep=":"))
      else{
        
        
        new_color <- aGoBar_colors[aGoNetworkSelect2][[1]]
        session$sendCustomMessage("handler_startLoader", c(19,90))
        
        construct_visNetworkaGo2(networkEdgelist, new_color)
        ##table-netTable
        combinedFunctions <- expand.grid(as.list(networkAGo2$Function), as.list(networkAGo2$Function))
        combinedTermIdsLinks <- expand.grid(as.list(all_aGo_table$Term_ID), as.list(all_aGo_table$Term_ID))
        netTable <- as.data.frame(cbind("Function_A"=combinedFunctions$Var1, "Term_ID_A"=combinedTermIdsLinks$Var1, "Function_B"=combinedFunctions$Var2,
                                        "Term_ID_B"=combinedTermIdsLinks$Var2, "Similarity Score %"= similarityColumnTable, "Intersection"= intersectGenesList))
        
        netTable$Function_A <- as.character(unlist(netTable$Function_A))
        netTable$Function_B <- as.character(unlist(netTable$Function_B))
        netTable$`Similarity Score %` <- as.numeric(as.character(netTable$`Similarity Score %`))
        
        # remove self rows from table
        for (i in nrow(netTable):1){
          if (identical(netTable$Function_A[i], netTable$Function_B[i])) netTable <- netTable[-i, ]
        }
        # removing zero entries
        netTable <- netTable[netTable$`Similarity Score %` > 0, ] 
        
        # removing reverse edges
        netTable <- netTable[!duplicated(apply(netTable[, c("Function_A", "Function_B" )],1,function(x) paste(sort(x), collapse='_'))),]
        
        netTable <- netTable[with(netTable,order(-`Similarity Score %`)),]
        netTable <- netTable[netTable$`Similarity Score %` >= aGoSliderThreshold, ]
        columnVis<- c(2,3,4,5,6,7)
        output$aGoNetwork_table2 <- renderTableFunc(netTable, aGoNetworkSelect2, 7, "function-function_network", "Positive Hits",columnVis)
      }
    }
    session$sendCustomMessage("handler_startLoader", c(19,100))
    
    session$sendCustomMessage("handler_finishLoader", 19)
  }
}

# # This event updates the Network3 slider, an event which is observed in server
# # and consequently changes the Network and the respective table
# # @param DB_source: one eligible datasource from available
# # @return void
handleaGoNetworkSelect3 <- function(DB_source, session){
  if (!identical(DB_source, "")){
    gostres_m <- aGotoolResults[grepl(DB_source, aGotoolResults$Source),]
    # to ALWAYS trigger an observed event from Network slider, first update with a value fo 0
    updateSliderInput(session, "aGoSliderNetwork3", "Choose a number of results to view:", min = 1,
                      max = 1, value = 0, step = 1)
    updateSliderInput(session, "aGoSliderNetwork3", "Choose a number of results to view:", min = 2,
                      max = length(gostres_m$Term_ID), value = 10, step = 1)
  }
}

# # This event updates the aGosliderThreshold3 slider, an event which is observed by sliderNetwork3
# # and consequently changes the aGoNetwork3 and the respective table
# # @param aGoNetworkSelect3: one eligible datasource from available
# # @return void
handleaGoNetworkSlider3 <- function(aGoSliderNetwork3, aGoNetworkSelect3, session){
  if (!identical(aGoNetworkSelect3, "")){
    updateSliderInput(session, "aGoSliderThreshold3", "Number of common Functions:", min = 0,
                      max = aGoSliderNetwork3, value = 0, step = 1)
  }
}


# # This function draws a network3 and the corresponding data table
# # @param networkSelect3: one eligible datasource from available
# # @param sliderNetwork3: the user slider input
# # @param sliderThreshold3: cutoff range for edge values to show
# # @param networkMode3: choose between two modes. the data are sorted in accordance to the chosen mode
# # @return void
 handleaGoNetwork3 <- function(aGoNetworkSelect3, aGoSliderNetwork3, aGoNetworkMode3, aGoSliderThreshold3, output, session){
  if (!identical(aGoNetworkSelect3, "")){
    session$sendCustomMessage("handler_startLoader", c(27,30))
    networkaGoInput <- aGotoolResults[grepl(aGoNetworkSelect3, aGotoolResults$Source), ] # without links
    if (aGoNetworkMode3 == "Enrichment Score") {
      networkaGoInput <- networkaGoInput[with(networkaGoInput,order(-`Enrichment Score %`)),]
    }
    networkaGoInput <- head(networkaGoInput, aGoSliderNetwork3)
    col <- networkaGoInput$`Positive Hits`
    col <- paste(col, collapse=",")
    col <- strsplit(col, ",")
    genes<-unique(unlist(col))
    session$sendCustomMessage("handler_startLoader", c(27,60))
    similarityColumn<-list()
    intersectFunctions <- list()
    totalFunctions<- list()
    for (i in 1:length(genes)){
      for (j in 1:length(genes)) {
        commonFunctions <- networkaGoInput [grepl(genes[i], networkaGoInput$`Positive Hits`) & grepl(genes[j], networkaGoInput$`Positive Hits`),]
        nameCommonFunctions<- commonFunctions$Function
        similarityScore<- nrow(commonFunctions)
        similarityColumn[[length(similarityColumn) + 1]] <- similarityScore
        intersectFunctions[[length(intersectFunctions) + 1]] <- as.list(nameCommonFunctions)
      }
    }
    session$sendCustomMessage("handler_startLoader", c(27,80))
    
    combinedGenes <- expand.grid(as.list(genes), as.list(genes))

    #network input-networkEdgelist
    networkEdgelist <- as.matrix(cbind(as.character(combinedGenes$Var1), as.character(combinedGenes$Var2), as.character(similarityColumn)))
    networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) > 0, ]
    networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) >= aGoSliderThreshold3, ]
    fonts <- c("PFAM" = "white", "INTERPRO" = "white", "UniProt" = "black", "Disease Ontology" = "white")
    new_color <- aGoBar_colors[aGoNetworkSelect3][[1]]
    fontsColor <- fonts[aGoNetworkSelect3][[1]]
    session$sendCustomMessage("handler_startLoader", c(27,90))
    
    construct_visNetworkaGo3(networkEdgelist, new_color,fontsColor)

    #table-netTable

    netTable <- as.data.frame(cbind("Gene_A"= combinedGenes$Var1, "Gene_B"=combinedGenes$Var2, "Common_Functions"= similarityColumn, "Total_Functions"= rep(aGoSliderNetwork3, length(combinedGenes$Var1)), "Functions"= intersectFunctions))
    netTable$Gene_A <- as.character(unlist(netTable$Gene_A))
    netTable$Gene_B <- as.character(unlist(netTable$Gene_B))
    netTable$Common_Functions <- as.numeric(as.character(netTable$Common_Functions))

    # remove self rows from table
    for (i in nrow(netTable):1){
      if (identical(netTable$Gene_A[i], netTable$Gene_B[i])) netTable <- netTable[-i, ]
    }
    # removing zero entries
    netTable <- netTable[netTable$Common_Functions > 0, ]
    # removing reverse edges
    netTable <- netTable[!duplicated(apply(netTable[, c("Gene_A", "Gene_B" )],1,function(x) paste(sort(x), collapse='_'))),]
    netTable <- netTable[with(netTable,order(-Common_Functions)),]
    netTable <- netTable[netTable$Common_Functions >= aGoSliderThreshold3, ]
    columnVis<- c(2,3,4,5,6)
    output$aGoNetwork_table3 <- renderTableFunc(netTable, aGoNetworkSelect3, 6, "gene-gene_network", "Functions",columnVis)
    session$sendCustomMessage("handler_startLoader", c(27,100))
    session$sendCustomMessage("handler_finishLoader", 27)
    
  }
 }

##### LITERATURE PLOTS####

# This function draws a scatterplot and the corresponding data table
# @param literatureSliderScatter: the user slider input
# @return void
handleLitearureScatterPlot <- function(literatureSliderScatter,  output, session){ 
  if (nrow(all_literature)>0){
    session$sendCustomMessage("handler_startLoader", c(20,30))
    new_color <- "#c95591"
    # DB_table <- all_aGotool[grepl(aGoScatterSelect, all_aGotool$Source), ]
    DB_table <- all_literature
    DB_table <- subset(DB_table, select=-c( `Positive Hits`))
    DB_table <- DB_table[with(DB_table,order(-`Enrichment Score %`)),]
    output$literatureScatterPlot <- renderUI({plotlyOutput("drawliteratureScatterPlot", height = height_plots(literatureSliderScatter, 15, 110))})
    session$sendCustomMessage("handler_startLoader", c(20,80))
    output$drawliteratureScatterPlot <- renderPlotly({
      
      ggplot(DB_table[1:literatureSliderScatter,], aes(`Enrichment Score %`, `-log10Pvalue`,text=paste("TERM_ID: ", Term_ID,"\n", "PUBLICATION: ", Publication, sep=""))) + geom_point(fill=new_color,color="black", position = position_dodge(width = 0.5), alpha = 0.7, pch=21, size=3, stroke=0.3)+ labs(x = "ENRICHMENT SCORE %")+ labs(title = "PubMed" )
    })
    output$literatureScatter_table <- DT::renderDataTable(head(DB_table, literatureSliderScatter), server = FALSE,
                                                          extensions = 'Buttons',
                                                          options = list(
                                                            "dom" = 'T<"clear">lBfrtip',
                                                            buttons = list(list(extend='excel',filename='ScatterPlot_Table_PubMed'),
                                                                           list(extend= 'csv',filename='ScatterPlot_Table_PubMed'),
                                                                           list(extend='copy',filename='ScatterPlot_Table_PubMed'),
                                                                           list(extend='pdf',filename='ScatterPlot_Table_PubMed'),
                                                                           list(extend='print',filename='ScatterPlot_Table_PubMed'))
                                                          ),rownames= FALSE, escape=F)
    session$sendCustomMessage("handler_startLoader", c(20,100))
    session$sendCustomMessage("handler_finishLoader", 20)
  }
}

# This function draws a barplot from multiple selected sources and the corresponding data table
# @param aGoBarSelect2: multiple eligible datasource from available
# @param aGoSliderBarplot: the user slider input
# @return void
handleLiteratureBarPlot2 <- function(literatureSliderBarplot, literatureBarplotMode, output, session){
  
  if (nrow(all_literature)>0){
    session$sendCustomMessage("handler_startLoader", c(21,30))
    DB_table <-LiteratureResults
    all_literature_table <- all_literature
    DB_table <- subset(DB_table, select=-c( `Positive Hits`))
    all_literature_table <- subset(all_literature_table, select=-c(`Positive Hits`))
    new_colors <- "#c95591"
    session$sendCustomMessage("handler_startLoader", c(21,40))
    # Check mode of execution
    if(literatureBarplotMode == "-log10Pavlue"){
      output$literatureBarplot <- renderUI({plotlyOutput("literatureBarplot1", height = height_plots(literatureSliderBarplot, 18, 100))})
      DB_table <- DB_table[with(DB_table, order(-(`-log10Pvalue`), Publication)),] # changed factor to double in the global variable
      DB_table <- head(DB_table, literatureSliderBarplot)
      all_literature_table <- all_literature_table[with(all_literature_table, order(-(`-log10Pvalue`), Publication)),] # changed factor to double in the global variable
      all_literature_table <- head(all_literature_table, literatureSliderBarplot)
      
      DB_table <- DB_table[with(DB_table, order( Publication, decreasing = T )),]
      DB_table <- DB_table[with(DB_table, order(-`-log10Pvalue`)),]
      
      # output$literatureBarplot1 <- renderPlotly({
      #  
      #   ggplot(DB_table, aes(Term_ID, `-log10Pvalue`, text=paste("PUBLICATION: ", Publication,"\n", "ENRICHMENT SCORE %: ", `Enrichment Score %`, sep=""))) + geom_bar(stat='identity', fill= new_colors) + coord_flip()+ labs(y = "-LOG10PAVLUE ")+ labs(title = "PubMed" ) +
      #     geom_text(aes(label=`Intersection Size`), vjust=+0.5, size=3.5) + scale_x_discrete(limits = DB_table$Term_ID[order(DB_table$`-log10Pvalue`)])
      # })
      DB_table$Term_ID <- factor(DB_table$Term_ID, levels = unique(DB_table$Term_ID)[order(DB_table$ `-log10Pvalue`, decreasing = F)])
      output$literatureBarplot1 <- renderPlotly({
        plot_ly(x =  DB_table$`-log10Pvalue`, y = DB_table$Term_ID, 
                marker=list(color=new_colors), 
                type = 'bar', orientation = 'h', 
                text =  DB_table$`Intersection Size`,
                textposition = 'auto',
                textangle = 0,
                hoverinfo = "text",
                hovertext =paste("TERM ID: ",DB_table$Term_ID, "\n-LOG10PAVULE: ",DB_table$`-log10Pvalue`, "\nPUBLICATION: ", DB_table$Publication,"\n", "ENRICHMENT SCORE %: ", DB_table$`Enrichment Score %`, sep=""))%>% 
          layout(title = "PubMed",xaxis = list(title = "-LOG10PVALUE "), yaxis=list(title="TERM ID")) 
        
      })
    }
    else if(literatureBarplotMode == "Enrichment Score") { # Enrichment Mode
      DB_table <- DB_table[with(DB_table, order(-(`Enrichment Score %`),Publication)),]
      DB_table <- head(DB_table, literatureSliderBarplot)
      all_literature_table <- all_literature_table[with(all_literature_table, order(-(`Enrichment Score %`),Publication)),]
      all_literature_table <- head(all_literature_table, literatureSliderBarplot)
      DB_table <- DB_table[with(DB_table, order(Publication, decreasing = T )),]
      DB_table <- DB_table[with(DB_table, order(-`Enrichment Score %`)),]
      output$literatureBarplot <- renderUI({plotlyOutput("literatureBarplot1", height = height_plots(literatureSliderBarplot, 18, 100))})
      # output$literatureBarplot1 <- renderPlotly({
      #  
      #   ggplot(DB_table, aes(Term_ID, `Enrichment Score %`, text=paste("PUBLICATION: ", Publication,"\n", "-LOG_PVALUE: ", `-log10Pvalue`, sep=""))) + geom_bar(stat='identity', fill= new_colors)+coord_flip()+labs(y = "ENRICHMENT SCORE % ")+ labs(title = "PubMed" ) +
      #     geom_text(aes(label=`Intersection Size`), vjust=+0.5, size=3.5)+ scale_x_discrete(limits = DB_table$Term_ID[order(DB_table$`Enrichment Score %`)]) +
      #     scale_colour_manual(values = as.vector(new_colors))
      # })
      DB_table$Term_ID <- factor(DB_table$Term_ID, levels = unique(DB_table$Term_ID)[order(DB_table$ `Enrichment Score %`, decreasing = F)])
      output$literatureBarplot1 <- renderPlotly({
        plot_ly(x =  DB_table$`Enrichment Score %`, y = DB_table$Term_ID, 
                marker=list(color=new_colors), 
                type = 'bar', orientation = 'h', 
                text =  DB_table$`Intersection Size`,
                textposition = 'auto',
                textangle = 0,
                hoverinfo = "text",
                hovertext =paste("TERM ID: ",DB_table$Term_ID, "\n-LOG10PAVULE: ",DB_table$`-log10Pvalue`, "\nPUBLICATION: ", DB_table$Publication,"\n", "ENRICHMENT SCORE %: ", DB_table$`Enrichment Score %`, sep=""))%>% 
          layout(title = "PubMed",xaxis = list(title = "ENRICHMENT SCORE "), yaxis=list(title="TERM ID")) 
      })
    }
    output$literatureBarplot_table <- DT::renderDataTable(all_literature_table, server = FALSE,
                                                          extensions = 'Buttons',
                                                          options = list(
                                                            "dom" = 'T<"clear">lBfrtip',
                                                            buttons = list(list(extend='excel',filename='BarplotPlot_Table_PubMed'),
                                                                           list(extend= 'csv',filename='BarplotPlot_Table_PubMed'),
                                                                           list(extend='copy',filename='BarplotPlot_Table_PubMed'),
                                                                           list(extend='pdf',filename='BarplotPlot_Table_PubMed'),
                                                                           list(extend='print',filename='BarplotPlot_Table_PubMed'))
                                                          ),rownames= FALSE, escape=F)
    session$sendCustomMessage("handler_startLoader", c(21,100))
    session$sendCustomMessage("handler_finishLoader", 21)
  }
}

# This function draws a Literatureheatmap and the corresponding data table
# @param literatureSliderHeatmap: the user slider input
# @param literatureHeatmapAxis: reverse the heatmap s axis
# @param literatureHeatmapMode: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleLiteratureHeatMap<- function(literatureSliderHeatmap, literatureHeatmapAxis, literatureHeatmapMode, output, session){
  if (nrow(all_literature)>0){
    session$sendCustomMessage("handler_startLoader", c(22,30))
    new_color <- "#c95591"
    literatureHeatmap <- LiteratureResults
    all_literature_table <- all_literature
    session$sendCustomMessage("handler_startLoader", c(22,40))
    
    if (length(literatureHeatmap$Publication) == 1) session$sendCustomMessage("handler_alert", "Cannot create a clustered heatmap if the results<2.")
    else{
      if (literatureHeatmapMode == "Enrichment Score") {
        literatureHeatmap <- literatureHeatmap[with(literatureHeatmap,order(-`Enrichment Score %`)),]
        all_literature_table <- all_literature_table[with(all_literature_table,order(-`Enrichment Score %`)),]
      }
      session$sendCustomMessage("handler_startLoader", c(22,50))
      literatureHeatmap <- head(literatureHeatmap, literatureSliderHeatmap)
      all_literature_table <- head(all_literature_table, literatureSliderHeatmap)
      output$literatureHeatmap_table <- DT::renderDataTable( subset(all_literature_table, select=-c(`Positive Hits`)), server = FALSE, 
                                                             extensions = 'Buttons',
                                                             options = list("dom" = 'T<"clear">lBfrtip',
                                                                            buttons = list(list(extend='excel',filename='HeatMap_Table_PubMed'),
                                                                                           list(extend= 'csv',filename='HeatMap_Table_PubMed'),
                                                                                           list(extend='copy',filename='HeatMap_Table_PubMed'),
                                                                                           list(extend='pdf',filename='HeatMap_Table_PubMed'),
                                                                                           list(extend='print',filename='HeatMap_Table_PubMed'))
                                                             ),rownames= FALSE, escape=F)
      all_genes <- paste(literatureHeatmap$`Positive Hits`, collapse=",") 
      all_genes <- strsplit(all_genes, ",")
      all_genes <- unique(all_genes[[1]])
      id <- literatureHeatmap$Term_ID
      functions <- literatureHeatmap$Publication
      heatmapTable <- as.data.frame(matrix(0, ncol = length(all_genes), nrow = length(functions)))
      colnames(heatmapTable) <- all_genes
      rownames(heatmapTable) <- id #paste(id, functions, sep="_")
      score <- paste(literatureHeatmap$`-log10Pvalue`, collapse=",")
      score <- strsplit(score , ",")
      score <- as.numeric(as.character(score[[1]]))
      score_en <- paste(literatureHeatmap$`Enrichment Score %` , collapse=",")
      score_en <- strsplit(score_en , ",")
      score_en <- as.numeric(as.character(score_en[[1]]))
      session$sendCustomMessage("handler_startLoader", c(22,60))
      for (i in 1:length(functions)) heatmapTable[i, match(strsplit(as.character(literatureHeatmap$`Positive Hits`[i]), ",")[[1]], all_genes)] <- score[i]
      if (literatureHeatmapMode == "Enrichment Score") {for (i in 1:length(functions)) heatmapTable[i, match(strsplit(as.character(literatureHeatmap$`Positive Hits`[i]), ",")[[1]], all_genes)] <- score_en[i]}
      
      
      hoverLabelsTable <- heatmapTable                # duplicate the initial input and change the rownames.
      rownames(hoverLabelsTable) <- functions         # custom_hovertext parameter takes as an input a table with ta same dimensions... 
      hoverLabelsTable[] <- paste("PUBLICATION: ", rownames(hoverLabelsTable))
      hoverLabelsTable[] <- lapply(colnames(hoverLabelsTable), function(colname) {
        paste0(hoverLabelsTable[, colname], ", ", colname)
      })
      
      session$sendCustomMessage("handler_startLoader", c(22,80))
      
      if (literatureHeatmapAxis == "Genes-Publications") {
        heatmapTable <- t(heatmapTable)
        hoverLabelsTable <- t(hoverLabelsTable)
        genesNumber <- length(all_genes)
        
        
        output$literatureHeatmapPlot <- renderUI(plotlyOutput("drawliteratureHeatmapPlot", height = height_plots(genesNumber, 15, 500)))
        output$drawliteratureHeatmapPlot <- renderPlotly({
          
          heatmaply(heatmapTable,custom_hovertext = hoverLabelsTable, 
                    xlab = "PUBLICATIONS",ylab = "GENES",main = "PubMed", fontsize_row = 10, fontsize_col = 10, 
                    grid_gap = 1,show_dendrogram = c(F, F), #margins = c(NA, NA, 60, NA),
                    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = new_color)
          )
        })
      }
      else {
        output$literatureHeatmapPlot <- renderUI(plotlyOutput("drawliteratureHeatmapPlot", height = height_plots(literatureSliderHeatmap, 15, 200)))
        output$drawliteratureHeatmapPlot <- renderPlotly({
          
          heatmaply(heatmapTable, custom_hovertext = hoverLabelsTable, 
                    xlab = "GENES",ylab = "PUBLICATIONS",main = "PubMed", fontsize_row = 10,fontsize_col = 10,
                    show_dendrogram = c(F, F), grid_gap = 1, #margins = c(NA, NA, 60, NA),
                    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = new_color)
          )
        })
      }
    }
    session$sendCustomMessage("handler_startLoader", c(22,100))
    session$sendCustomMessage("handler_finishLoader", 22)
  }#end of first if
}
# This function draws a heatmap2 and the corresponding data table
# @param literatureSliderHeatmap2: the user slider input
# @param literatureHeatmapMode2: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleLiteratureHeatMap2<- function(literatureSliderHeatmap2, literatureHeatmapMode2, output, session){
  if (nrow(all_literature)>0){
    session$sendCustomMessage("handler_startLoader", c(23,30))
    new_color <- "#c95591"
    literatureHeatmap2 <- LiteratureResults
    all_literature_table <-all_literature  #with links
    if (length(literatureHeatmap2$Publication) == 1) session$sendCustomMessage("handler_alert", "Cannot create a clustered heatmap if the results<2.")
    else{
      
      if (literatureHeatmapMode2 == "Enrichment Score") {
        literatureHeatmap2 <- literatureHeatmap2[with(literatureHeatmap2,order(-`Enrichment Score %`)),]
        all_literature_table <- all_literature_table[with(all_literature_table,order(-`Enrichment Score %`)),]
      }
      session$sendCustomMessage("handler_startLoader", c(23,40))
      literatureHeatmap2 <- head(literatureHeatmap2, literatureSliderHeatmap2)
      all_literature_table <- head(all_literature_table, literatureSliderHeatmap2)
      heatmapTable <- as.data.frame(matrix(0, ncol = length(literatureHeatmap2$Publication), nrow = length(literatureHeatmap2$Publication)))
      colnames(heatmapTable) <- literatureHeatmap2$Term_ID #paste(literatureHeatmap2$Term_ID, literatureHeatmap2$Publication, sep="_")
      rownames(heatmapTable) <- literatureHeatmap2$Term_ID #paste(literatureHeatmap2$Term_ID, literatureHeatmap2$Publication, sep="_")
      
      similarityColumnTable <- list()
      intersectGenesList <- list()
      # for (i in 1:(length(literatureHeatmap2$Publication)-1)){
      for (i in 1:length(literatureHeatmap2$Publication)){
        row <- literatureHeatmap2$`Positive Hits`[i]
        row <- paste(row, collapse=",") 
        row <- strsplit(row, ",")
        # for (j in (i+1):length(literatureHeatmap2$Publication)) {
        for (j in 1:length(literatureHeatmap2$Publication)) {
          col <- literatureHeatmap2$`Positive Hits`[j]
          col <- paste(col, collapse=",") 
          col <- strsplit(col, ",")
          unionGenes<-append(col,row)
          unionGenes<- unlist(unionGenes)
          similarityScore=((length(intersect(col[[1]], row[[1]])))/ (length(unique(unionGenes))))*100
          similarityScore= round(similarityScore, digits = 2)
          heatmapTable[i,j] <- similarityScore
          # heatmapTable[j,i] <- similarityScore
          similarityColumnTable[[length(similarityColumnTable) + 1]] <- similarityScore
          intersectGenesList[[length(intersectGenesList) + 1]] <- as.list(intersect(col[[1]], row[[1]]))
        }
      }
      # heatmapTable[ row(heatmapTable) == col(heatmapTable) ] <- 100
      
      combinedPublications <- expand.grid(as.list(literatureHeatmap2$Publication), as.list(literatureHeatmap2$Publication))
      combinedTermIdsLinks <- expand.grid(as.list(all_literature_table$Term_ID), as.list(all_literature_table$Term_ID))
      heatTable <- as.data.frame(cbind("Publication_A"=combinedPublications$Var1,"Term_ID_A"= combinedTermIdsLinks$Var1, 
                                       "Publication_B"=combinedPublications$Var2,"Term_ID_B"= combinedTermIdsLinks$Var2, 
                                       "Similarity Score %"= similarityColumnTable,"Intersection"= intersectGenesList))
      
      heatTable$Publication_A <- as.character(unlist(heatTable$Publication_A))
      heatTable$Publication_B <- as.character(unlist(heatTable$Publication_B))
      heatTable$`Similarity Score %` <- as.numeric(as.character(heatTable$`Similarity Score %`))
      
      # remove self rows from table
      for (i in nrow(heatTable):1){ 
        if (identical(heatTable$Publication_A[i], heatTable$Publication_B[i])) heatTable <- heatTable[-i, ]
      }
      # removing zero entries
      heatTable <- heatTable[heatTable$`Similarity Score %` > 0, ] 
      
      # removing reverse edges
      heatTable <- heatTable[!duplicated(apply(heatTable[, c("Publication_A", "Publication_B" )],1,function(x) paste(sort(x), collapse='_'))),]
      heatTable <- heatTable[with(heatTable,order(-`Similarity Score %`)),]
      
      columnVis<- c(2,3,4,5,6,7)
      output$literatureHeatmap_table2 <- renderTableFunc(heatTable, "PubMed", 7, "Publication-Publication_heatmap", "Positive Hits", columnVis)
      
      # output$literatureHeatmap_table2 <- DT::renderDataTable(heatTable, server = FALSE,
      #                                                        extensions = 'Buttons',
      #                                                        options = list("dom" = 'T<"clear">lBfrtip',
      #                                                                       buttons = list(list(extend='excel',filename='PublicationVsPublication_HeatMap_Table_PubMed'),
      #                                                                                      list(extend= 'csv',filename='PublicationVsPublication_HeatMap_Table_PubMed'),
      #                                                                                      list(extend='copy',filename='PublicationVsPublication_HeatMap_Table_PubMed'),
      #                                                                                      list(extend='pdf',filename='PublicationVsPublication_HeatMap_Table_PubMed'),
      #                                                                                      list(extend='print',filename='PublicationVsPublication_HeatMap_Table_PubMed'))
      #                                                        ),rownames= FALSE, escape=F)
      output$literatureHeatmapPlot2 <- renderUI(plotlyOutput("drawliteratureHeatmapPlot2", height = height_plots(literatureSliderHeatmap2, 18, 500)))
      session$sendCustomMessage("handler_startLoader", c(23,80))
      output$drawliteratureHeatmapPlot2 <- renderPlotly({
        
        heatmaply(heatmapTable,
                  xlab = "PUBLICATIONS",ylab = "PUBLICATIONS", main = "PubMed", fontsize_row = 10, fontsize_col = 10, grid_gap = 1,
                  show_dendrogram = c(F, F),  #margins = c(NA, NA, 70, NA),
                  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = new_color)
        )
      })
    }
    session$sendCustomMessage("handler_startLoader", c(23,100))
    session$sendCustomMessage("handler_finishLoader", 23)
  }
}

# This function draws a network and the corresponding data table
# @param literatureSliderNetwork: the user slider input
# @param literatureNetworkMode: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleLiteratureNetwork<- function(literatureSliderNetwork,literatureNetworkMode, output, session){
  if (nrow(all_literature)>0){
    new_color <- "#c95591"
    session$sendCustomMessage("handler_startLoader", c(24,30))
    networkLiterature <- LiteratureResults # without links
    all_literature_table <- all_literature # with links
    all_literature_table <- subset(all_literature_table, select=-c(`Positive Hits`)) #remove the Genes column from the table
    if (literatureNetworkMode == "Enrichment Score") {
      networkLiterature <- networkLiterature[with(networkLiterature,order(-`Enrichment Score %`)),]
      all_literature_table <- all_literature_table[with(all_literature_table,order(-`Enrichment Score %`)),]
    }
    output$literatureNetwork_table <- DT::renderDataTable(head(all_literature_table, literatureSliderNetwork), server = FALSE, 
                                                          extensions = 'Buttons',
                                                          options = list("dom" = 'T<"clear">lBfrtip',
                                                                         buttons = list(list(extend='excel',filename='GeneVsPublication_Network_Table_PubMed'),
                                                                                        list(extend= 'csv',filename='GeneVsPublication_Network_Table_PubMed'),
                                                                                        list(extend='copy',filename='GeneVsPublication_Network_Table_PubMed'),
                                                                                        list(extend='pdf',filename='GeneVsPublication_Network_Table_PubMed'),
                                                                                        list(extend='print',filename='GeneVsPublication_Network_Table_PubMed'))
                                                          ), rownames= FALSE, escape=F)
    session$sendCustomMessage("handler_startLoader", c(24,60))
    networkLiterature <- head(networkLiterature,literatureSliderNetwork) 
    networkEdgelist <- matrix("", nrow = 0, ncol = 2) #3 
    for (i in 1:nrow(networkLiterature)){
      name <- networkLiterature$Term_ID[i]
      #name <- paste(networkLiterature$Term_ID[i], networkLiterature$Publication[i], sep="_")
      lineGenes <- strsplit(as.character(networkLiterature$`Positive Hits`[i]), ",")[[1]]
      for (j in 1:length(lineGenes)) networkEdgelist <- rbind(networkEdgelist, c(name, lineGenes[j]))
    }
    all_genes <- paste(networkLiterature$`Positive Hits`, collapse=",") 
    all_genes <- strsplit(all_genes, ",")
    all_genes <- unique(all_genes[[1]])
    session$sendCustomMessage("handler_startLoader", c(24,80))
    construct_visNetworkLit(networkEdgelist, new_color)
    session$sendCustomMessage("handler_startLoader", c(24,100))
    session$sendCustomMessage("handler_finishLoader", 24)
  }
}

# This function draws a literature network2 and the corresponding data table
# @param literatureSliderNetwork2: the user slider input
# @param literatureSliderThreshold: cutoff range for edge values to show
# @param literatureNetworkMode2: choose between two modes. the data are sorted in accordance to the chosen mode
#
# @return void
handleLiteratureNetwork2 <- function(literatureSliderNetwork2, literatureNetworkMode2,  literatureSliderThreshold, output, session){
  if (nrow(all_literature)>0){
    new_color <- "#c95591"
    session$sendCustomMessage("handler_startLoader", c(25,30))
    networkLiteratute2 <- LiteratureResults # without links
    all_literature_table <- all_literature # with links
    all_literature_table <- subset(all_literature_table, select=-c(`Positive Hits`)) #remove the Genes column from the table
    
    if (length(networkLiteratute2$Publication) == 1) session$sendCustomMessage("handler_alert", paste(aGoNetworkSelect2," Cannot create a Publication Vs Publication network if the results=1.", sep=":"))
    else{
      
      if (literatureNetworkMode2 == "Enrichment Score") {
        networkLiteratute2 <- networkLiteratute2[with(networkLiteratute2,order(-`Enrichment Score %`)),]
        all_literature_table <- all_literature_table[with(all_literature_table,order(-`Enrichment Score %`)),]
      }
      session$sendCustomMessage("handler_startLoader", c(25,50))
      networkLiteratute2 <- head(networkLiteratute2, literatureSliderNetwork2)
      all_literature_table <- head(all_literature_table, literatureSliderNetwork2)
      
      similarityColumnTable <- list()
      intersectGenesList <- list()
      for (i in 1:length(networkLiteratute2$Publication)){
        row <- networkLiteratute2$`Positive Hits`[i]
        row <- paste(row, collapse=",") 
        row <- strsplit(row, ",")
        for (j in 1:length(networkLiteratute2$Publication)) {
          col <- networkLiteratute2$`Positive Hits`[j]
          col <- paste(col, collapse=",") 
          col <- strsplit(col, ",")
          unionGenes <- append(col,row)
          unionGenes <- unlist(unionGenes)
          similarityScore <- ((length(intersect(col[[1]], row[[1]]))) / (length(unique(unionGenes))))*100
          similarityScore <- round(similarityScore, digits = 2)
          
          similarityColumnTable[[length(similarityColumnTable) + 1]] <- similarityScore
          intersectGenesList[[length(intersectGenesList) + 1]] <- as.list(intersect(col[[1]], row[[1]]))
        }
      }
      #network input-networkEdgelist
      names<-networkLiteratute2$Term_ID
      #names <- paste(networkLiteratute2$Term_ID, networkLiteratute2$Publication, sep="_")
      names <- expand.grid(as.list(names), as.list(names))
      networkEdgelist <- as.matrix(cbind(as.character(names$Var1), as.character(names$Var2), as.character(similarityColumnTable)))
      networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) > 0, ]
      networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) >= literatureSliderThreshold, ]
      if (identical(networkEdgelist[,1], networkEdgelist[,2])== TRUE) session$sendCustomMessage("handler_alert", "PubMed: Cannot create a Publication Vs Publication network. No common genes found")
      else{
        session$sendCustomMessage("handler_startLoader", c(25,80))
        construct_visNetworkLit2(networkEdgelist, new_color)
        ##table-netTable
        combinedPublications <- expand.grid(as.list(networkLiteratute2$Publication), as.list(networkLiteratute2$Publication))
        combinedTermIdsLinks <- expand.grid(as.list(all_literature_table$Term_ID), as.list(all_literature_table$Term_ID))
        netTable <- as.data.frame(cbind("Publication_A"=combinedPublications$Var1, "Term_ID_A"=combinedTermIdsLinks$Var1,
                                        "Publication_B"=combinedPublications$Var2,"Term_ID_B"=combinedTermIdsLinks$Var2,
                                        "Similarity Score %"= similarityColumnTable, "Intersection"= intersectGenesList))
        # netTable <- netTable[netTable$`Similarity Score %` >= literatureSliderThreshold, ]
        # 
        # for (i in nrow(netTable):1){ # remove self rows from table
        #   if (identical(netTable$Publication_A[i], netTable$Publication_B[i])) netTable <- netTable[-i, ]
        # }
        # 
        # for (i in nrow(netTable):2){ # remove inverse repetitions from table
        #   for (j in (i-1):1){
        #     if (identical(netTable$Publication_A[i], netTable$Publication_B[j]) && identical(netTable$Publication_A[j], netTable$Publication_B[i])) {
        #       netTable <- netTable[-i, ]
        #       break
        #     }
        #   }
        # }
        
        netTable$Publication_A <- as.character(unlist(netTable$Publication_A))
        netTable$Publication_B <- as.character(unlist(netTable$Publication_B))
        netTable$`Similarity Score %` <- as.numeric(as.character(netTable$`Similarity Score %`))
        
        # remove self rows from table
        for (i in nrow(netTable):1){
          if (identical(netTable$Publication_A[i], netTable$Publication_B[i])) netTable <- netTable[-i, ]
        }
        # removing zero entries
        netTable <- netTable[netTable$`Similarity Score %` > 0, ] 
        
        # removing reverse edges
        netTable <- netTable[!duplicated(apply(netTable[, c("Publication_A", "Publication_B" )],1,function(x) paste(sort(x), collapse='_'))),]
        netTable <- netTable[with(netTable,order(-`Similarity Score %`)),]
        
        netTable <- netTable[netTable$`Similarity Score %` >= literatureSliderThreshold, ]
        columnVis<- c(2,3,4,5,6,7)
        # output$literatureNetwork_table2 <- DT::renderDataTable(netTable, server = FALSE,
        #                                                        extensions = 'Buttons',
        #                                                        options = list("dom" = 'T<"clear">lBfrtip',
        #                                                                       buttons = list(list(extend='excel',filename='PublicationVsPublication_Network_Table_PubMed'),
        #                                                                                      list(extend= 'csv',filename='PublicationVsPublication_Network_Table_PubMed'),
        #                                                                                      list(extend='copy',filename='PublicationVsPublication_Network_Table_PubMed'),
        #                                                                                      list(extend='pdf',filename='PublicationVsPublication_Network_Table_PubMed'),
        #                                                                                      list(extend='print',filename='PublicationVsPublication_Network_Table_PubMed'))
        #                                                        ),rownames= FALSE, escape=F)
        output$literatureNetwork_table2 <- renderTableFunc(netTable, "PubMed", 7, "function-function_network", "Positive Hits",columnVis)
        
      }
    }
    session$sendCustomMessage("handler_startLoader", c(25,100))
    session$sendCustomMessage("handler_finishLoader", 25)
  }
}

# This event updates the literatureSliderThreshold3 slider, an event which is observed by literatureSliderNetwork3
# and consequently changes the LiteratureNetwork3 and the respective table
# @return void
handleLiteratureNetworkSlider3 <- function(literatureSliderNetwork3, session){
  if (nrow(all_literature)>0){
    updateSliderInput(session, "literatureSliderThreshold3", "Number of common Publications:", min = 0,
                      max = literatureSliderNetwork3, value = 0, step = 1)
  }
}
# This function draws a network3 and the corresponding data table
# @param literatureSliderNetwork3: the user slider input
# @param literatureSliderThreshold3: cutoff range for edge values to show
# @param literatureNetworkMode3: choose between two modes. the data are sorted in accordance to the chosen mode
# @return void
handleLiteratureNetwork3<- function(literatureSliderNetwork3, literatureNetworkMode3,  literatureSliderThreshold3, output, session){
    if (nrow(all_literature)>0){
      session$sendCustomMessage("handler_startLoader", c(28,30))
     literatureplotInput<- LiteratureResults
     new_color <- "#c95591"
    if (literatureNetworkMode3 == "Enrichment Score") {
      literatureplotInput <- literatureplotInput[with(literatureplotInput,order(-`Enrichment Score %`)),]
    }
     session$sendCustomMessage("handler_startLoader", c(28,40))
    literatureplotInput <- head(literatureplotInput, literatureSliderNetwork3)
    col <- literatureplotInput$`Positive Hits`
    col <- paste(col, collapse=",")
    col <- strsplit(col, ",")
    genes<-unique(unlist(col))

    similarityColumn<-list()
    intersectPublications <- list()
    totalPublications<- list()
    for (i in 1:length(genes)){
      for (j in 1:length(genes)) {
        commonPublications <- literatureplotInput [grepl(genes[i], literatureplotInput$`Positive Hits`) & grepl(genes[j], literatureplotInput$`Positive Hits`),]
        nameCommonPublications<- commonPublications$Publication
        similarityScore<- nrow(commonPublications)
        similarityColumn[[length(similarityColumn) + 1]] <- similarityScore
        intersectPublications[[length(intersectPublications) + 1]] <- as.list(nameCommonPublications)
      }
    }
    combinedGenes <- expand.grid(as.list(genes), as.list(genes))

    #network input-networkEdgelist
    networkEdgelist <- as.matrix(cbind(as.character(combinedGenes$Var1), as.character(combinedGenes$Var2), as.character(similarityColumn)))
    networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) > 0, ]
    networkEdgelist <- networkEdgelist[as.numeric(as.character(networkEdgelist[,3])) >= literatureSliderThreshold3, ]
    session$sendCustomMessage("handler_startLoader", c(28,80))
    construct_visNetworkLiterature3(networkEdgelist, new_color)

    #table-netTable

    netTable <- as.data.frame(cbind("Gene_A"= combinedGenes$Var1, "Gene_B"=combinedGenes$Var2, "Common_Publications"= similarityColumn, "Total_Publications"= rep(literatureSliderNetwork3, length(combinedGenes$Var1)), "Publications"= intersectPublications))
    netTable$Gene_A <- as.character(unlist(netTable$Gene_A))
    netTable$Gene_B <- as.character(unlist(netTable$Gene_B))
    netTable$Common_Publications <- as.numeric(as.character(netTable$Common_Publications))

    # remove self rows from table
    for (i in nrow(netTable):1){
      if (identical(netTable$Gene_A[i], netTable$Gene_B[i])) netTable <- netTable[-i, ]
    }
    # removing zero entries
    netTable <- netTable[netTable$Common_Publications > 0, ]
    # removing reverse edges
    netTable <- netTable[!duplicated(apply(netTable[, c("Gene_A", "Gene_B" )],1,function(x) paste(sort(x), collapse='_'))),]
    netTable <- netTable[with(netTable,order(-Common_Publications)),]
    netTable <- netTable[netTable$Common_Publications >= literatureSliderThreshold3, ]
    columnVis<- c(2,3,4,5,6)
    output$literatureNetwork_table3 <- renderTableFunc(netTable, "PubMed", 6, "gene-gene_network", "Publications",columnVis)
    
#     output$literatureNetwork_table3 <- DT::renderDataTable(  cbind(' ' = '&oplus;', netTable), escape = F,
#                                                       extensions =  c('Responsive', 'Buttons'),
#                                                       options = list("dom" = 'T<"clear">lBfrtip',
#                                                                      buttons = list(list(extend='excel',filename="PubMed_Network"),
#                                                                                     list(extend= 'csv',filename="PubMed_Network"),
#                                                                                     list(extend='copy',filename="PubMed_Network"),
#                                                                                     list(extend='pdf',filename="PubMed_Network"),
#                                                                                     list(extend='print',filename="PubMed_Network")),
#                                                                      columnDefs = list(
#                                                                        list(visible = FALSE, targets = c(0,6)),
#                                                                        list(orderable = FALSE, className = 'details-control', targets = 1)
#                                                                      )
#                                                       ),
#                                                       callback = JS("
#                                                     table.column(1).nodes().to$().css({cursor: 'pointer'});
#                                                     var format = function(d) {
#                                                       return '<div style=\"background-color:#eee; padding: .5em;\"> <b>Publications:</b> ' +
#                                                               d[6] + '</div>';
#                                                     };
#                                                     table.on('click', 'td.details-control', function() {
#                                                       var td = $(this), row = table.row(td.closest('tr'));
#                                                       if (row.child.isShown()) {
#                                                         row.child.hide();
#                                                         td.html('&oplus;');
#                                                       } else {
#                                                         row.child(format(row.data())).show();
#                                                         td.html('&CircleMinus;');
#                                                       }
#                                                     });"
#                                                     ))
    session$sendCustomMessage("handler_startLoader", c(28,100))
     session$sendCustomMessage("handler_finishLoader", 28) 
   }
 }

# Sub Routines ####

# This function calculates the height of the barplot plot. The height is variable and depends on the value of slider
# @param num_entries: integer value of slider, with number of entries to print
# @return height: total calculated pixels to be assigned to div height
height_plots <-function(num_entries, number, standard){
  height <- paste( ((num_entries*number) + standard), "px", sep="")
  return(height)
}

# This function constructs and draws a network out of heatmaps adjacency matrix
# @param edgelist, 2 column matrix with source -> target, function -> gene
# @param color, based on current database source, respective manhattan color
construct_visNetwork <- function(edgelist, color){
  set.seed(123)
  tryCatch({
    graph <- createGraph(as.matrix(edgelist)) # as.matrix() ?
    data <- toVisNetworkData(graph)
    nodes <- data$nodes
    nodes <- cbind(nodes, font.size=24)
    nodes <- cbind(nodes, group=1)
    nodes$group[grepl(":", nodes$id, fixed = T)] <- 2
    edges <- data$edges
    output$network <- renderVisNetwork({
      
      visNetwork(nodes = nodes, edges = edges) %>%
        visGroups(groupname = "1", color = "lightgrey", shape = "circle") %>%
        visGroups(groupname = "2", color = color, shape = "diamond") %>%
        visEdges(color = "black") %>%
        visIgraphLayout(layout = "layout_nicely") %>% # layout_in_circle # layout_with_fr # layout_with_kk
        visInteraction(navigationButtons = TRUE, hover = TRUE)
    })
    
    
  }, warning = function(w) {
    print(paste("Warning:  ", w))
  }, error = function(e) {
    print(paste("Error :  ", e))
    session$sendCustomMessage("handler_alert", paste("Can't create network."))
  }, finally = {})
}

# This function constructs and draws a network out of heatmaps adjacency matrix
# @param edgelist, 2 column matrix with source -> target, function -> gene
# @param color, based on current database source, respective manhattan color
construct_visNetworkaGo <- function(edgelist, color){
  set.seed(123)
  tryCatch({
    graph <- createGraph(as.matrix(edgelist)) # as.matrix() ?
    data <- toVisNetworkData(graph)
    nodes <- data$nodes
    nodes <- cbind(nodes, font.size=24)
    nodes <- cbind(nodes, group=1)
    nodes$group[grepl("_", nodes$id, fixed = T)] <- 2  #uniprot and pfam exoun allo sep symbol oxi :
    edges <- data$edges
    output$aGoNetwork <- renderVisNetwork({
      
      visNetwork(nodes = nodes, edges = edges) %>%
        visGroups(groupname = "1", color = "lightgrey", shape = "circle") %>%
        visGroups(groupname = "2", color = color, shape = "diamond") %>%
        visEdges(color = "black") %>%
        visIgraphLayout(layout = "layout_nicely") %>% # layout_in_circle # layout_with_fr # layout_with_kk
        visInteraction(navigationButtons = TRUE, hover = TRUE)
    })
    
    
  }, warning = function(w) {
    print(paste("Warning:  ", w))
  }, error = function(e) {
    print(paste("Error :  ", e))
    session$sendCustomMessage("handler_alert", paste("Can't create network."))
  }, finally = {})
}

# This function constructs and draws a network out of heatmaps adjacency matrix
# @param edgelist, 2 column matrix with source -> target, function -> gene
# @param color, based on current database source, respective manhattan color
construct_visNetworkLit <- function(edgelist, color){
  set.seed(123)
  tryCatch({
    graph <- createGraph(as.matrix(edgelist)) # as.matrix() ?
    data <- toVisNetworkData(graph)
    nodes <- data$nodes
    nodes <- cbind(nodes, font.size=24)
    nodes <- cbind(nodes, group=1)
    nodes$group[grepl("PMID:", nodes$id, fixed = T)] <- 2  #PubMed sep= :
    edges <- data$edges
    output$literatureNetwork <- renderVisNetwork({
      
      visNetwork(nodes = nodes, edges = edges) %>%
        visGroups(groupname = "1", color = "lightgrey", shape = "circle") %>%
        visGroups(groupname = "2", color = color, shape = "diamond") %>%
        visEdges(color = "black") %>%
        visIgraphLayout(layout = "layout_nicely") %>% # layout_in_circle # layout_with_fr # layout_with_kk
        visInteraction(navigationButtons = TRUE, hover = TRUE)
    })
    
    
  }, warning = function(w) {
    print(paste("Warning:  ", w))
  }, error = function(e) {
    print(paste("Error :  ", e))
    session$sendCustomMessage("handler_alert", paste("Can't create network."))
  }, finally = {})
}


construct_visNetwork2 <- function(edgelist, color){
  set.seed(123)
  tryCatch({
    graph <- createGraph(as.matrix(edgelist)) # as.matrix() ?
    data <- toVisNetworkData(graph)
    nodes <- data$nodes
    nodes <- cbind(nodes, font.size=24)
    nodes <- cbind(nodes, group=1)
    nodes$group[grepl(":", nodes$id, fixed = T)] <- 2
    edges <- data$edges
    edges$label <- edges$weight
    edges$weight <- mapper(edges$weight, 0.5, 5)
    colnames(edges)[3] <- "width" # required to work
    output$network2 <- renderVisNetwork({
      
      visNetwork(nodes = nodes, edges = edges) %>%
        visGroups(groupname = "1", color = "lightgrey", shape = "circle") %>%
        visGroups(groupname = "2", color = color, shape = "diamond") %>%
        visEdges(color = "black") %>%
        visIgraphLayout(layout = "layout_nicely") %>% # layout_in_circle # layout_with_fr # layout_with_kk
        visInteraction(navigationButtons = TRUE, hover = TRUE)
    })
    
  }, warning = function(w) {
    print(paste("Warning:  ", w))
  }, error = function(e) {
    print(paste("Error :  ", e))
    session$sendCustomMessage("handler_alert", paste("Can't create network."))
  }, finally = {})
}

construct_visNetworkaGo2 <- function(edgelist, color){
  set.seed(123)
  tryCatch({
    graph <- createGraph(as.matrix(edgelist)) # as.matrix() ?
    data <- toVisNetworkData(graph)
    nodes <- data$nodes
    nodes <- cbind(nodes, font.size=24)
    nodes <- cbind(nodes, group=1)
    nodes$group[grepl("_", nodes$id, fixed = T)] <- 2
    edges <- data$edges
    edges$label <- edges$weight
    edges$weight <- mapper(edges$weight, 0.5, 5)
    colnames(edges)[3] <- "width" # required to work
    output$aGoNetwork2 <- renderVisNetwork({
    
      visNetwork(nodes = nodes, edges = edges) %>%
        visGroups(groupname = "1", color = "lightgrey", shape = "circle") %>%
        visGroups(groupname = "2", color = color, shape = "diamond") %>%
        visEdges(color = "black") %>%
        visIgraphLayout(layout = "layout_nicely") %>% # layout_in_circle # layout_with_fr # layout_with_kk
        visInteraction(navigationButtons = TRUE, hover = TRUE)
    })
    
  }, warning = function(w) {
    print(paste("Warning:  ", w))
  }, error = function(e) {
    print(paste("Error :  ", e))
    session$sendCustomMessage("handler_alert", paste("Can't create network."))
  }, finally = {})
}

construct_visNetworkLit2 <- function(edgelist, color){
  set.seed(123)
  tryCatch({
    graph <- createGraph(as.matrix(edgelist)) # as.matrix() ?
    data <- toVisNetworkData(graph)
    nodes <- data$nodes
    nodes <- cbind(nodes, font.size=24)
    nodes <- cbind(nodes, group=1)
    nodes$group[grepl("PMID:", nodes$id, fixed = T)] <- 2 #pubmed id sep= :
    edges <- data$edges
    edges$label <- edges$weight
    edges$weight <- mapper(edges$weight, 0.5, 5)
    colnames(edges)[3] <- "width" # required to work
    output$literatureNetwork2 <- renderVisNetwork({
      
      visNetwork(nodes = nodes, edges = edges) %>%
        visGroups(groupname = "1", color = "lightgrey", shape = "circle") %>%
        visGroups(groupname = "2", color = color, shape = "diamond") %>%
        visEdges(color = "black") %>%
        visIgraphLayout(layout = "layout_nicely") %>% # layout_in_circle # layout_with_fr # layout_with_kk
        visInteraction(navigationButtons = TRUE, hover = TRUE)
    })
    
  }, warning = function(w) {
    print(paste("Warning:  ", w))
  }, error = function(e) {
    print(paste("Error :  ", e))
    session$sendCustomMessage("handler_alert", paste("Can't create network."))
  }, finally = {})
}
construct_visNetwork3 <- function(edgelist, color, fonts){
  set.seed(123)
  tryCatch({
    graph <- createGraph(as.matrix(edgelist)) # as.matrix() ?
    data <- toVisNetworkData(graph)
    nodes <- data$nodes
    nodes <- cbind(nodes, font.size=24)
    nodes <- cbind(nodes, group=1)
    #nodes$group[grepl(":", nodes$id, fixed = T)] <- 2
    edges <- data$edges
    edges$label <- edges$weight
    edges$weight <- mapper(edges$weight, 0.5, 5)
    colnames(edges)[3] <- "width" # required to work
    output$network3 <- renderVisNetwork({
      
      visNetwork(nodes = nodes, edges = edges) %>%
        #visGroups(groupname = "1", color = "lightgrey", shape = "circle") %>%
        visGroups(groupname = "1", color = color, shape = "circle") %>%
        visEdges(color = "black") %>%
        visNodes(font = list(color = fonts ))%>%
        visIgraphLayout(layout = "layout_nicely") %>% # layout_in_circle # layout_with_fr # layout_with_kk
        visInteraction(navigationButtons = TRUE, hover = TRUE)
    })
    
  }, warning = function(w) {
    print(paste("Warning:  ", w))
  }, error = function(e) {
    print(paste("Error :  ", e))
    session$sendCustomMessage("handler_alert", paste("Can't create network."))
  }, finally = {})
}

construct_visNetworkaGo3 <- function(edgelist, color, fonts){
  set.seed(123)
  tryCatch({
    graph <- createGraph(as.matrix(edgelist)) # as.matrix() ?
    data <- toVisNetworkData(graph)
    nodes <- data$nodes
    nodes <- cbind(nodes, font.size=24)
    nodes <- cbind(nodes, group=1)
    #nodes$group[grepl(":", nodes$id, fixed = T)] <- 2
    
    edges <- data$edges
    edges$label <- edges$weight
    edges$weight <- mapper(edges$weight, 0.5, 5)
    colnames(edges)[3] <- "width" # required to work
    output$aGoNetwork3 <- renderVisNetwork({
      
      visNetwork(nodes = nodes, edges = edges) %>%
        #visGroups(groupname = "1", color = "lightgrey", shape = "circle") %>%
        visGroups(groupname = "1", color = color, shape = "circle") %>%
        visEdges(color = "black") %>%
        visNodes(font = list(color = fonts ))%>%
        visIgraphLayout(layout = "layout_nicely") %>% # layout_in_circle # layout_with_fr # layout_with_kk
        visInteraction(navigationButtons = TRUE, hover = TRUE)
    })
    
  }, warning = function(w) {
    print(paste("Warning:  ", w))
  }, error = function(e) {
    print(paste("Error :  ", e))
    session$sendCustomMessage("handler_alert", paste("Can't create network."))
  }, finally = {})
}


construct_visNetworkLiterature3 <- function(edgelist, color){
  set.seed(123)
  tryCatch({
    graph <- createGraph(as.matrix(edgelist)) # as.matrix() ?
    data <- toVisNetworkData(graph)
    nodes <- data$nodes
    nodes <- cbind(nodes, font.size=24)
    nodes <- cbind(nodes, group=1)
    #nodes$group[grepl(":", nodes$id, fixed = T)] <- 2
    edges <- data$edges
    edges$label <- edges$weight
    edges$weight <- mapper(edges$weight, 0.5, 5)
    colnames(edges)[3] <- "width" # required to work
    output$literatureNetwork3 <- renderVisNetwork({
      visNetwork(nodes = nodes, edges = edges) %>%
        #visGroups(groupname = "1", color = "lightgrey", shape = "circle") %>%
        visGroups(groupname = "1", color = color, shape = "circle") %>%
        visEdges(color = "black") %>%
        visNodes(font = list(color = "#ffffff" ))%>%
        visIgraphLayout(layout = "layout_nicely") %>% # layout_in_circle # layout_with_fr # layout_with_kk
        visInteraction(navigationButtons = TRUE, hover = TRUE)
    })
    
  }, warning = function(w) {
    print(paste("Warning:  ", w))
  }, error = function(e) {
    print(paste("Error :  ", e))
    session$sendCustomMessage("handler_alert", paste("Can't create network."))
  }, finally = {})
}

createGraph <- function(edgelist) {
  graph <- graph_from_edgelist(edgelist[, 1:2], directed = FALSE)
  if (ncol(edgelist) == 3) E(graph)$weight <- as.double(edgelist[, 3])
  else E(graph)$weight <- rep(1, nrow(edgelist))
  # remove loops and multiple edges, simplify sum aggregates same edges
  graph <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "max"))
  graph <- delete.vertices(graph, degree(graph)==0) # deleting isolate nodes
  return(graph)
}

# This function translates an array of values into another number range in [min, max]
# @param inArr: initial array of numbers
# @param min: target min value
# @param max: target max value
# @return outArr: translated numbers array into new range
mapper <- function(inArr, min, max){
  outArr <- inArr
  inArr_min <- min(inArr)
  inArr_max <- max(inArr)
  if (inArr_max - inArr_min != 0){
    for (i in 0:length(inArr)){
      outArr[i] <- (inArr[i] - inArr_min) * (max - min) / (inArr_max - inArr_min) + min;
    }
  } else outArr[] <- 0.3;
  return(outArr);
}


renderTableFunc <- function(input_table, networkSelect, col, suffix, mode, columnsVis){
  DT::renderDataTable(  cbind(' ' = '&oplus;', input_table), escape = F, 
                        extensions =  c( 'Buttons'),
                        options = list("dom" = 'T<"clear">lBfrtip',
                                       buttons = list(list(extend='excel', exportOptions = list(columns = columnsVis),filename=paste(networkSelect, suffix, sep="_")),
                                                      list(extend= 'csv', exportOptions = list(columns = columnsVis), filename=paste(networkSelect, suffix, sep="_")),
                                                      list(extend='copy', exportOptions = list(columns = columnsVis), filename=paste(networkSelect, suffix, sep="_")),
                                                      list(extend='pdf', exportOptions = list(columns = columnsVis), filename=paste(networkSelect, suffix, sep="_")),
                                                      list(extend='print', exportOptions = list(columns = columnsVis), filename=paste(networkSelect, suffix, sep="_"))),
                                       columnDefs = list(
                                         list(visible = FALSE, targets = c(0,col)),
                                         list(orderable = FALSE, className = 'details-control', targets = 1)
                                       )
                        ),
                        callback = JS(paste("
                                            table.column(1).nodes().to$().css({cursor: 'pointer'});
                                            var format = function(d) {
                                              return '<div style=\"background-color:#eee; padding: .5em;\"> <b>", mode, ":</b> ' +
                                                      d[", col, "] + '</div>';
                                            };
                                            table.on('click', 'td.details-control', function() {
                                              var td = $(this), row = table.row(td.closest('tr'));
                                              if (row.child.isShown()) {
                                                row.child.hide();
                                                td.html('&oplus;');
                                              } else {
                                                row.child(format(row.data())).show();
                                                td.html('&CircleMinus;');
                                              }
                                            });", sep="")
                        ))
}
  