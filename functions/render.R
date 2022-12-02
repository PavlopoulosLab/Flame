renderError <- function(prompt) {
  if (exists("session"))
    shinyalert::shinyalert("Error!", prompt, type = "error")
}

renderWarning <- function(prompt) {
  if (exists("session"))
    shinyalert::shinyalert("Warning!", prompt, type = "warning")
}

renderModal <- function(prompt) {
  showModal(modalDialog(HTML(prompt), footer = NULL))
}

renderShinyText <- function(shinyOutputId, prompt) {
  output[[shinyOutputId]] <- renderText(prompt)
}

renderShinyDataTable <- function(shinyOutputId, outputData,
                                 caption = NULL, fileName = "") {
  output[[shinyOutputId]] <- DT::renderDataTable(
    outputData,
    server = FALSE, 
    extensions = 'Buttons',
    caption = caption,
    options = list(
      dom = 'Blfiprt',
      buttons = list(
        list(extend = 'excel', filename = fileName),
        list(extend = 'csv', filename = fileName),
        list(extend = 'copy', filename = fileName),
        list(extend = 'pdf', filename = fileName),
        list(extend = 'print', filename = fileName))
    ),
    rownames = FALSE,
    escape = F
  )
}

renderEnrichmentTable <- function(shinyOutputId, input_table,
                                  caption, fileName, mode,
                                  hiddenColumns, expandableColumn){
  output[[shinyOutputId]] <- DT::renderDataTable(
    server = FALSE,
    cbind(' ' = '&oplus;', input_table),
    escape = F, 
    extensions = c('Buttons'),
    caption = caption,
    options = list(
      "dom" = 'T<"clear">lBfrtip',
      buttons = list(
        list(extend = 'excel', filename = fileName),
        list(extend = 'csv', filename = fileName),
        list(extend = 'copy', filename = fileName),
        list(extend = 'pdf', filename = fileName),
        list(extend = 'print', filename = fileName)
      ),
      columnDefs = list(
        list(visible = FALSE, targets = hiddenColumns),
        list(orderable = FALSE, className = 'details-control', targets = 1)
      )
    ),
    callback = JS(paste0(
      "table.column(1).nodes().to$().css({cursor: 'pointer'});
      var format = function(d) {
        return '<div style=\"background-color:#eee; padding: .5em;\"> <b>", mode, ":</b> ' +
                d[", expandableColumn, "] + '</div>';
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
      });"
    ))
  ) 
}

renderManhattanEnrichmentTable <- function(manhattanTable) {
  renderEnrichmentTable("manhattan_table",
                        manhattanTable,
                        caption = "Selected Terms", 
                        fileName = "_selected_enriched_gprofiler",
                        mode = "Positive Hits",
                        hiddenColumns = c(0, 11, 12),
                        expandableColumn = 11)
}

renderManhattanPlot <- function(){ 
  output$manhattan <- renderPlotly({
    gostplot(
      gprofilerResult,
      capped = TRUE,
      interactive = T,
      pal = DATASOURCE_COLORS
    )
  })
}

renderScatterPlot <- function(shinyOutputId, scatterData) {
  output[[shinyOutputId]] <- renderPlotly({
    plot_ly(data = scatterData,
            x = ~`-log10Pvalue_jittered`,
            y = ~`Enrichment Score %_jittered`,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              size = 15,
              line = list(
                color = 'rgb(0, 0, 0)',
                width = 1
              )),
            color = ~Source,
            colors = DATASOURCE_COLORS,
            hoverinfo = "text",
            hovertext = ~paste0("TERM_ID: ", Term_ID_noLinks,
                           "\nFUNCTION: ", Function,
                           "\nEnrichment Score %: ", `Enrichment Score %`,
                           "\n-log10Pvalue: ", `-log10Pvalue`)) %>%
      layout(
        xaxis = list(title = "-log10Pvalue"),
        yaxis = list(title = "Enrichment Score")
      ) 
  })
}

renderBarchart <- function(shinyOutputId, barchartData, column, height) {
  output[[shinyOutputId]] <- renderPlotly({
    plot_ly(
      data = barchartData,
      x = barchartData[[column]],
      y = ~Term_ID,
      type = 'bar',
      orientation = 'h',
      color = ~Source,
      colors = DATASOURCE_COLORS,
      text = ~`Intersection Size`,
      textfont = list(color = '#000000', size = 16),
      textposition = 'outside',
      hoverinfo = "text",
      hovertext = ~paste0("TERM_ID: ", Term_ID_noLinks,
                     "\nFUNCTION: ", Function,
                     "\nEnrichment Score %: ", `Enrichment Score %`,
                     "\n-log10Pvalue: ", `-log10Pvalue`)
    ) %>%
      layout(height = height)
  })
}

renderHeatmap <- function(shinyOutputId, heatmapTable, color,
                          yAxisColumn, xAxisColumn, weightColumn, height) {
  output[[shinyOutputId]] <- renderPlotly({
    plot_ly(
      data = heatmapTable,
      y = heatmapTable[[yAxisColumn]],
      x = heatmapTable[[xAxisColumn]],
      z = heatmapTable[[weightColumn]],
      type = 'heatmap',
      xgap = 3,
      ygap = 3,
      colors = colorRamp(c("white", color)),
      hoverinfo = "text",
      hovertext = generateHeatmapHoverText(shinyOutputId)
    ) %>%
      layout(height = height)
  })
}

generateHeatmapHoverText <- function(shinyOutputId) {
  hoverText <- switch(
    shinyOutputId,
    "heatmap1" = ~paste0("TERM ID: ", Term_ID_noLinks,
                         "\nFUNCTION: ", Function,
                         "\nGENE: ", `Positive Hits`,
                         "\nEnrichment Score %: ", `Enrichment Score %`,
                         "\n-log10Pvalue: ", `-log10Pvalue`,
                         "\nIntersection Size: ", `Intersection Size`),
    "heatmap2" = ~paste0("Function 1 ID: ", `Source Id`,
                         "\nFunction 1 Name: ", `Source Name`,
                         "\nFunction 2 ID: ", `Target Id`,
                         "\nFunction 2 Name: ", `Target Name`,
                         "\nCommon Genes: ", `Common Genes`,
                         "\nTotal Genes: ", `Total Genes`,
                         "\nSimilarity Score %: ", `Similarity Score %`),
    "heatmap3" = ~paste0("Gene 1: ", `Source Name`,
                         "\nGene 2: ", `Target Name`,
                         "\nCommon Functions: ", `Common Functions`)
  )
  return(hoverText)
}

renderShinyVisNetwork <- function(networkId, nodes, edges, layout) {
  shinyjs::show(networkId)
  output[[networkId]] <- renderVisNetwork({
    set.seed(123)
    visNetwork(nodes = nodes, edges = edges, background = "white") %>%
      visGroups(groupname = "Gene", color = GENE_NODE_COLOR, shape = "square") %>%
      visGroups(groupname = "GO:MF", color = DATASOURCE_COLORS["GO:MF"][[1]], shape = "hexagon") %>%
      visGroups(groupname = "GO:BP", color = DATASOURCE_COLORS["GO:BP"][[1]], shape = "hexagon") %>%
      visGroups(groupname = "GO:CC", color = DATASOURCE_COLORS["GO:CC"][[1]], shape = "hexagon") %>%
      visGroups(groupname = "KEGG", color = DATASOURCE_COLORS["KEGG"][[1]], shape = "diamond") %>%
      visGroups(groupname = "REAC", color = DATASOURCE_COLORS["REAC"][[1]], shape = "diamond") %>%
      visGroups(groupname = "WP", color = DATASOURCE_COLORS["WP"][[1]], shape = "diamond") %>%
      visGroups(groupname = "TF", color = DATASOURCE_COLORS["TF"][[1]], shape = "star") %>%
      visGroups(groupname = "MIRNA", color = DATASOURCE_COLORS["MIRNA"][[1]], shape = "star") %>%
      visGroups(groupname = "HPA", color = DATASOURCE_COLORS["HPA"][[1]], shape = "triangle") %>%
      visGroups(groupname = "CORUM", color = DATASOURCE_COLORS["CORUM"][[1]], shape = "triangle") %>%
      visGroups(groupname = "HP", color = DATASOURCE_COLORS["HP"][[1]], shape = "triangleDown") %>%
      visEdges(color = "black") %>%
      visIgraphLayout(layout = layout) %>%
      visInteraction(navigationButtons = TRUE, hover = TRUE)
  })
}
