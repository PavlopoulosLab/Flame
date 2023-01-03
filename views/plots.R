generatePlotsPanel <- function() {
  tabPanel(
    title = "Plots", 
    icon = icon("chart-bar"),
    do.call(
      tabsetPanel,
      (lapply(PLOT_TABNAMES, function(tabName) {
        generatePlotPanel(tabName)
      }))
    )
  )
}

generatePlotPanel <- function(tabName) {
  plotPanelDiv <- switch(
    tabName,
    "Network" = do.call(
      tabsetPanel,
      (lapply(NETWORK_IDS, function(plotId) {
        generatePlotPanelOrDiv(plotId)
      })
      )
    ),
    "Heatmap" = do.call(
      tabsetPanel,
      (lapply(HEATMAP_IDS, function(plotId) {
        generatePlotPanelOrDiv(plotId)
      })
      )
    ),
    "Barchart" = generatePlotPanelOrDiv("barchart"),
    "Scatter Plot" = generatePlotPanelOrDiv("scatterPlot"),
    "Manhattan" = {
      if (currentEnrichmentTool == "gProfiler")
        generateManhattanPanel()
      else
        NULL
    }
  )
  
  if (!is.null(plotPanelDiv))
    return(
      tabPanel(
        title = tabName,
        tags$br(),
        plotPanelDiv
      )
    )
  else
    return(NULL)
}

generatePlotPanelOrDiv <- function(plotId) {
  # general values
  returnType <- "tabPanel"
  title <- NULL
  multipleDatasources <- T
  plotExtraFluidRow <- NULL
  actionButtonColumns <- 12
  plotExtraControl <- NULL
  plotOutputDiv <- plotlyOutput(paste(currentType_Tool, plotId, sep = "_"))
  plotExtraOutputDiv <- NULL
  
  uiTermKeyword <- str_to_title(UI_TERM_KEYWORD[[currentEnrichmentType]])
  # conditions
  if (plotId %in% c("barchart", "scatterPlot"))
    returnType <- "tags$div"
  if (plotId %in% c("network1", "heatmap1"))
    title <- paste0(uiTermKeyword, " Vs Genes")
  if (plotId %in% c("network2", "heatmap2"))
    title <- paste0(uiTermKeyword, " Vs ", uiTermKeyword)
  if (plotId %in% c("network3", "heatmap3"))
    title <- "Genes Vs Genes"
  if (plotId %in% c("heatmap1", "heatmap2", "heatmap3")) {
    multipleDatasources <- F
    plotOutputDiv <- tags$div(
      class = "heatmapOutput",
      plotlyOutput(paste(currentType_Tool, plotId, sep = "_"))
    )
  }
  if (plotId %in% c("network1", "network2", "network3")) {
    plotExtraFluidRow <- generateNetworkExtraControl(plotId)
    plotExtraOutputDiv <- tags$div(
      tags$br(),
      generateColorCodingLegend(),
      DT::dataTableOutput(paste(
        currentType_Tool, plotId, "edgelist", sep = "_"
      ))
    )
    plotOutputDiv <- tags$div(
      class = "networkOutput",
      visNetworkOutput(paste(
        currentType_Tool, plotId, sep = "_"), height = VIS_NET_HEIGHT
      ))
  }
  if (plotId == "heatmap1") {
    plotExtraControl <- column(
      4,
      tags$div(
        class = "drawUp",
        radioButtons(
          inputId = paste(currentType_Tool, plotId, "axis", sep = "_"),
          label = "Axes",
          choices = c(paste0(uiTermKeyword, "-Genes"),
                      paste0("Genes-", uiTermKeyword)),
          inline = TRUE,
        )
      )
    )
    actionButtonColumns <- 8
  }
  if (plotId == "barchart") 
    plotOutputDiv <- tags$div(
      class = "barchartOutput",
      plotlyOutput(paste(currentType_Tool, "barchart", sep = "_"))
    )

  return(
    # returnType is either "tabPanel" or "tags$div"
    eval(parse(text = returnType))( 
      title = title,
      tags$br(),
      fluidRow(
        column(
          4,
          pickerInput(
            inputId = paste(currentType_Tool, plotId, "sourceSelect", sep = "_"),
            label = "Select term datasource(s):", 
            choices = NULL, multiple = multipleDatasources,
            options = list('actions-box' = TRUE)
          )
        ),
        column(
          4,
          sliderInput(
            inputId = paste(currentType_Tool, plotId, "slider", sep = "_"),
            label = paste0("Filter number of top ",
                           UI_TERM_KEYWORD[[currentEnrichmentType]], ":"),
            min = 1, max = 10, value = 10, step = 1
          )
        ),
        column(
          4, 
          radioButtons(
            inputId = paste(currentType_Tool, plotId, "mode", sep = "_"),
            label = paste0("Order retrieved ",
                           UI_TERM_KEYWORD[[currentEnrichmentType]], " by:"),
            choices = c("-log10Pvalue", "Enrichment Score"),
            inline = TRUE
          )
        )
      ),
      plotExtraFluidRow,
      fluidRow(
        column(
          actionButtonColumns,
          actionButton(
            inputId = paste(currentType_Tool, plotId, "button", sep = "_"),
            label = "Generate", icon("palette"), class = "submit_button"
          )
        ),
        plotExtraControl
      ),
      tags$hr(),
      plotOutputDiv,
      plotExtraOutputDiv,
      tags$br(),
      DT::dataTableOutput(paste(currentType_Tool, plotId, "table", sep = "_"))
    )
  )
}

generateNetworkExtraControl <- function(networkId) {
  label <- switch(
    networkId,
    "network1" = NULL,
    "network2" = "Similarity score cut-off (%):",
    "network3" = paste0(
      "Number of common ", UI_TERM_KEYWORD[[currentEnrichmentType]], ":"
    )
  )
  
  if (!is.null(label))
    exclusiveNetworkComponent <- sliderInput(
      inputId = paste(currentType_Tool, networkId, "thresholdSlider", sep = "_"),
      label = label, min = 1, max = 100, value = 10, step = 1
    )
  else
    exclusiveNetworkComponent <- NULL
  
  return(
    fluidRow(
      column(
        4,
        actionButton(
          inputId = paste(currentType_Tool, networkId, "arena", sep = "_"),
          label = "Visualize 3D",
          class = "arena_button"
        )
      ),
      column(
        4,
        selectInput(
          inputId = paste(currentType_Tool, networkId, "layout", sep = "_"),
          label = "Choose layout algorithm:",
          choices = as.vector(unlist(LAYOUT_CHOICES))
        )
      ),
      column(
        4,
        tags$div(
          class = "drawUp",
          exclusiveNetworkComponent
        )
      )
    )
  )
}

generateColorCodingLegend <- function() {
  fluidRow(
    do.call(
      box, c(
        class = "legend", title = "Legend", status = "primary",
        solidHeader = T, width = 12, collapsible = T, collapsed = T,
        lapply(LEGEND_ITEMS, function(legendList) {
          do.call(
            column, c(
              width = 2,
              lapply(legendList, function(source) {
                fontColor <- "white"
                if (source %in% c("GENE", "DO", "GO:BP", "UNIPROT", "BTO"))
                  fontColor <- "black"
                tags$div(
                  tags$p(source),
                  style = paste0(
                    "background-color: ", DATASOURCE_COLORS[source][[1]],
                    "; color: ", fontColor, ";"
                  )
                )
              })
            )
          )}
        )
      )
    )
  )
}

generateManhattanPanel <- function() {
  return(
    tabPanel(
      title = "Manhattan",
      tags$br(),
      actionButton(inputId = "manhattan_button", label = "Generate",
                   icon("palette"), class = "submit_button"),
      tags$hr(),
      plotlyOutput("manhattan", width = "100%", inline = FALSE),
      tags$br(),
      DT::dataTableOutput("manhattan_table")
    )
  )
}
