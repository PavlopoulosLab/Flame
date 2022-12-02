resetGprofilerResults <- function() {
  # TODO reset all tabs and subtubs
  hide("conversionBoxes")
  output$gprofParameters <- renderText("")
  output$genesNotFound <- renderText("")
  output$notConverted <- renderText("")
  renderShinyDataTable("conversionTable", data.frame())
  resetTables()
  resetPlots()
}

resetTables <- function(){
  output$table_all <- renderDataTable(c())
  output$table_gomf <- renderDataTable(c())
  output$table_gocc <- renderDataTable(c())
  output$table_gobp <- renderDataTable(c())
  output$table_kegg <- renderDataTable(c())
  output$table_reac <- renderDataTable(c())
  output$table_wp <- renderDataTable(c())
  output$table_tf <- renderDataTable(c())
  output$table_mirna <- renderDataTable(c())
  output$table_corum <- renderDataTable(c())
  output$table_hpa <- renderDataTable(c())
  output$table_hp <- renderDataTable(c())
}

resetPlots <- function() {
  resetManhattanPlot()
  resetManhattanTable()
  resetNetworkPlots()
  resetNetworkTables()
}

resetManhattanPlot <- function() {
  output$manhattan <- renderPlotly(c())
}

resetManhattanTable <- function() {
  output$manhattan_table <- DT::renderDataTable(data.frame())
}

resetNetworkPlots <- function() {
  lapply(networkIds, function(networkId) {
    output[[networkId]] <- renderVisNetwork({})
    shinyjs::hide(networkId)
  })
}

resetNetworkTables <- function() {
  lapply(networkIds, function(networkId) {
    output[[paste0(networkId, "_table")]] <- renderDataTable(c())
  })
  lapply(networkIds, function(networkId) {
    output[[paste0(networkId, "_edgelist")]] <- renderDataTable(c())
  })
}

resetEdgelist_ViewAndArenaObjects <- function(networkId) {
  arenaEdgelist[[networkId]] <<- data.frame()
  output[[paste0(networkId, "_edgelist")]] <- renderDataTable(c())
  output[[networkId]] <- renderVisNetwork({})
  shinyjs::hide(networkId)
}
