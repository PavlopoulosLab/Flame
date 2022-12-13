generateEnrichmentPage <- function(enrichmentType) {
  currentEnrichmentType <<- enrichmentType
  
  return(
    tags$div(
      tags$h3(paste0(str_to_title(currentEnrichmentType), " Enrichment Analysis")),
      tags$br(),
      generateEnrichmentControlPanel(),
      tags$hr(),
      generateEnrichmentResultsPanel()
    )
  )
}

generateEnrichmentControlPanel <- function() {
  availableTools <- switch(
    currentEnrichmentType,
    "functional" = {
      datasourceChoices <- eval(
        parse(text = paste0(DEFAULT_TOOL_UPPER, "_DATASOURCES_PRINT")))
      datasourceSelected <- eval(
        parse(text = paste0(DEFAULT_TOOL_UPPER, "_DATASOURCES_DEFAULT_SELECTED")))
      metrics <- eval(
        parse(text = paste0(DEFAULT_TOOL_UPPER, "_METRICS")))
      ENRICHMENT_TOOLS
    },
    "literature" = {
      datasourceChoices <- AGOTOOL_DATASOURCES_PRINT_LITERATURE
      datasourceSelected <- AGOTOOL_DATASOURCES_PRINT_LITERATURE
      metrics <- "P-value"
      "aGOtool"
    }
  )
  
  return(
    tags$div(
      fluidRow(
        column(
          4,
          selectInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_file"),
            label = "1. Select file:",
            choices = NULL,
            width = "80%"
          ),
          selectizeInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_organism"),
            label = "2. Select organism:",
            choices = ORGANISMS_FROM_FILE$print_name,
            selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
            multiple = F,
            width = "80%",
            options = list(placeholder = 'Select an option or start typing...')
          )
        ),
        column(
          4,
          pickerInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_tool"),
            label = "3. Select enrichment tool:",
            choices = availableTools,
            selected = DEFAULT_TOOL,
            multiple = T,
            width = "89.5%",
            options = list('actions-box' = TRUE)
          ),
          pickerInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_datasources"),
            label = "4. Select datasources:", 
            choices = datasourceChoices,
            selected = datasourceSelected,
            multiple = T,
            width = "89.5%",
            options = list('actions-box' = TRUE)
          ),
          selectInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_namespace"),
            label = "5. Select namespace conversion:",
            choices = eval(
              parse(text = paste0(DEFAULT_TOOL_UPPER, "_NAMESPACES"))
            ),
            width = "80%"
          )
        ),
        column(
          4,
          selectInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_metric"),
            label = "6. Select significance metric:",
            choices = metrics,
            width = "80%"
          ),
          selectInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_threshold"),
            label = "7. Select significance threshold:",
            choices = c(0.05, 0.01),
            width = "80%"
          )
        )
      ),
      fluidRow(
        column(
          4,
          actionButton(
            inputId = paste0(currentEnrichmentType, "_enrichment_run"),
            label = "Run analysis",
            icon("paper-plane"), class = "btn-submit")
          )
      )
    )
  )
}

generateEnrichmentResultsPanel <- function() {
  if (currentEnrichmentType == "functional") {
    return(
      tags$div(
        class = "toolTabs",
        tabsetPanel(
          tabPanel(
            title = ENRICHMENT_TOOLS[1],
            generateToolPanel(ENRICHMENT_TOOLS[1])
          ),
          tabPanel(
            title = ENRICHMENT_TOOLS[2],
            generateToolPanel(ENRICHMENT_TOOLS[2])
          )
        )
      )
    )
  } else { # "literature"
    return(generateToolPanel("aGOtool"))
  }
}

generateToolPanel <- function(toolName) {
  currentEnrichmentTool <<- toolName
  currentType_Tool <<- paste(currentEnrichmentType, currentEnrichmentTool, sep = "_")
  
  return(
    tags$div(
      tags$br(),
      generateParametersBox(),
      tabsetPanel(
        generateResultsPanel(),
        generatePlotsPanel()
      )
    )
  )
}

generateParametersBox <- function() {
  box(
    title = "Parameters", 
    width = NULL,
    status = "primary", 
    solidHeader = T,
    collapsible = T,
    verbatimTextOutput(
      outputId = paste(currentType_Tool, "enrichment_parameters", sep = "_")
    )
  )
}

generateResultsPanel <- function() {
  sourcesPanel <- switch(
    currentEnrichmentType,
    "functional" = {
      tabsetPanel(
        id = paste(currentType_Tool, "sources_panel", sep = "_"),
        tabPanel("ALL", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_all", sep = "_"))),
        tabPanel("INTERPRO", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_interpro", sep = "_"))),
        tabPanel("PFAM", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_pfam", sep = "_"))),
        tabPanel("UNIPROT", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_uniprot", sep = "_"))),
        tabPanel("GO:MF", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_gomf", sep = "_"))),
        tabPanel("GO:CC", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_gocc", sep = "_"))),
        tabPanel("GO:BP", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_gobp", sep = "_"))),
        tabPanel("KEGG", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_kegg", sep = "_"))),
        tabPanel("REAC", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_reac", sep = "_"))),
        tabPanel("WP", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_wp", sep = "_"))),
        tabPanel("DO", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_do", sep = "_"))),
        tabPanel("BTO", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_brenda", sep = "_"))),
        tabPanel("TF", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_tf", sep = "_"))),
        tabPanel("MIRNA", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_mirna", sep = "_"))),
        tabPanel("CORUM", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_corum", sep = "_"))),
        tabPanel("HPA", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_hpa", sep = "_"))),
        tabPanel("HP", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_hp", sep = "_")))
      )
    },
    "literature" = {
      tabsetPanel(
        id = paste(currentType_Tool, "sources_panel", sep = "_"),
        tabPanel("PUBMED", tags$br(),
                 DT::dataTableOutput(paste(currentType_Tool, "table_pubmed", sep = "_")))
      )
    }
  )
  
  return(
    tabPanel(
      title = "Results",
      icon = icon("table"),
      tags$div(
        id = paste(currentType_Tool, "resultsDiv", sep = "_"),
        class = "enrichmentResultsDiv",
        tags$br(),
        sourcesPanel
      ),
      tags$br(),
      tags$div(
        id = paste(currentType_Tool, "conversionBoxes", sep = "_"),
        box(
          title = "Conversion Table", 
          width = NULL,
          status = "primary", 
          solidHeader = TRUE,
          collapsible = TRUE,
          DT::dataTableOutput(paste(currentType_Tool, "conversionTable", sep = "_"))
        ),
        box(
          title = "Unconverted Genes", 
          width = NULL,
          status = "primary", 
          solidHeader = TRUE,
          collapsible = TRUE,
          verbatimTextOutput(paste(currentType_Tool, "notConverted", sep = "_"))
        )
      ),
      box(
        title = "No-hit Inputs", 
        width = NULL,
        status = "primary", 
        solidHeader = TRUE,
        collapsible = TRUE,
        verbatimTextOutput(paste(currentType_Tool, "genesNotFound", sep = "_"))
      )
    )
  )
}

generatePlotsPanel <- function() {
  tabPanel(
    "Plots", 
    icon = icon("chart-bar"),
    tabsetPanel(
      generateNetworkPanels(),
      generateHeatmapPanels(),
      generateBarchartPanel(),
      generateScatterplotPanel(),
      generateManhattanPanel()
    )
  )
}

generateNetworkPanels <- function() {
  tabPanel(
    "Network",
    tags$br(),
    tabsetPanel(
      generateNetworkPanel("network1"),
      generateNetworkPanel("network2"),
      generateNetworkPanel("network3")
    )
  )
}

generateNetworkPanel <- function(networkId) {
  exclusiveComponent <- switch(
    networkId,
    "network1" = {
      tabName <- paste0(str_to_title(UI_TERM_KEYWORD[[currentEnrichmentType]]),
                        " Vs Genes")
      NULL
    },
    "network2" = {
      tabName <- paste0(str_to_title(UI_TERM_KEYWORD[[currentEnrichmentType]]),
                        " Vs ", str_to_title(UI_TERM_KEYWORD[[currentEnrichmentType]]))
      sliderInput(
        inputId = paste(currentType_Tool, "network2_thresholdSlider", sep = "_"),
        label = "Similarity score cut-off (%):",
        min = 0, max = 100, value = 30, step = 1)
    },
    "network3" = {
      tabName <- "Genes Vs Genes"
      sliderInput(
        inputId = paste(currentType_Tool, "network3_thresholdSlider", sep = "_"),
        label = paste0("Number of common ", UI_TERM_KEYWORD[[currentEnrichmentType]], ":"),
        min = 1, max = 10, value = 10, step = 1)
    }
  )
    
  return(
    tabPanel(
      title = tabName,
      tags$br(),
      fluidRow(
        column(
          4,
          pickerInput(
            inputId = paste(currentType_Tool, networkId, "sourceSelect", sep = "_"),
            label = "Select term datasources:",
            choices = NULL,
            multiple = TRUE,
            options = list('actions-box' = TRUE)
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
          radioButtons(
            inputId = paste(currentType_Tool, networkId, "mode", sep = "_"),
            label = paste0("Order retrieved ", UI_TERM_KEYWORD[[currentEnrichmentType]], " by:"),
            choices = c("-log10Pvalue", "Enrichment Score"), inline = TRUE)
        )
      ),
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
          sliderInput(
            inputId = paste(currentType_Tool, networkId, "slider", sep = "_"),
            label = paste0("Filter number of top ", UI_TERM_KEYWORD[[currentEnrichmentType]], ":"),
            min = 1, max = 10, value = 10, step = 1)
        ),
        column(
          4,
          exclusiveComponent
        )
      ),
      fluidRow(
        column(
          12,
          actionButton(
            inputId = paste(currentType_Tool, networkId, "visualizeNetwork", sep = "_"),
            label = "Visualize",
            icon("paper-plane"), class = "btn-submit")
        )
      ),
      tags$hr(),
      tags$div(
        class = "networkOutput",
        visNetworkOutput(paste(currentType_Tool, networkId, sep = "_"),
                         height = VIS_NET_HEIGHT)
      ),
      tags$br(),
      generateColorCodingLegend(),
      DT::dataTableOutput(paste(currentType_Tool, networkId, "edgelist", sep = "_")),
      tags$br(),
      DT::dataTableOutput(paste(currentType_Tool, networkId, "table", sep = "_"))
    )
  )
}

generateColorCodingLegend <- function() {
  fluidRow(
    box(
      class = "legend",
      title = "Legend",
      status = "primary",
      solidHeader = T,
      collapsible = T,
      collapsed = T,
      width = 12,
      column(
        2,
        tags$div(
          tags$p("GO:MF"),
          style = paste0("background-color: ", DATASOURCE_COLORS["GO:MF"][[1]], ";")
        ),
        tags$div(
          tags$p("GO:BP"), style = paste0("background-color: ", DATASOURCE_COLORS["GO:BP"][[1]], "; color: black;")
        ),
        tags$div(
          tags$p("GO:CC"), style = paste0("background-color: ", DATASOURCE_COLORS["GO:CC"][[1]], ";")
        )
      ),
      column(
        2,
        tags$div(
          tags$p("KEGG"),
          style = paste0("background-color: ", DATASOURCE_COLORS["KEGG"][[1]], ";")
        ),
        tags$div(
          tags$p("Reactome"),
          style = paste0("background-color: ", DATASOURCE_COLORS["REAC"][[1]], ";")
        ),
        tags$div(
          tags$p("WikiPathways"),
          style = paste0("background-color: ", DATASOURCE_COLORS["WP"][[1]], ";")
        )
      ),
      column(
        2,
        tags$div(
          tags$p("Interpro"),
          style = paste0("background-color: ", DATASOURCE_COLORS["INTERPRO"][[1]], ";")
        ),
        tags$div(
          tags$p("PFAM"),
          style = paste0("background-color: ", DATASOURCE_COLORS["PFAM"][[1]], ";")
        ),
        tags$div(
          tags$p("UniProt keywords"),
          style = paste0("background-color: ", DATASOURCE_COLORS["UNIPROT"][[1]], "; color: black;")
        )
      ),
      column(
        2,
        tags$div(
          tags$p("Disease Ontology"),
          style = paste0("background-color: ", DATASOURCE_COLORS["DO"][[1]], "; color: black;")
        ),
        tags$div(
          tags$p("BTO"),
          style = paste0("background-color: ", DATASOURCE_COLORS["BTO"][[1]], "; color: black;")
        ),
        tags$div(
          tags$p("HPO"),
          style = paste0("background-color: ", DATASOURCE_COLORS["HP"][[1]], ";")
        )
      ),
      column(
        2,
        tags$div(
          tags$p("TRANSFAC"),
          style = paste0("background-color: ", DATASOURCE_COLORS["TF"][[1]], ";")
        ),
        tags$div(
          tags$p("miRTarBase"),
          style = paste0("background-color: ", DATASOURCE_COLORS["MIRNA"][[1]], ";")
        ),
        tags$div(
          tags$p("HPA"),
          style = paste0("background-color: ", DATASOURCE_COLORS["HPA"][[1]], ";")
        )
      ),
      column(
        2,
        tags$div(
          tags$p("CORUM"),
          style = paste0("background-color: ", DATASOURCE_COLORS["CORUM"][[1]], ";")
        ),
        tags$div(
          tags$p("Gene"),
          style = paste0("background-color: ", GENE_NODE_COLOR, "; color: black;")
        ),
        tags$div(
          tags$p("PUBMED"),
          style = paste0("background-color: ", LITERATURE_NODE_COLOR, "; color: black;")
        )
      )
    )
  )
}

generateHeatmapPanels <- function() {
  tabPanel(
    "Heatmap",
    tags$br(),
    tags$div(
      tabsetPanel(
        generateHeatmapPanel("heatmap1"),
        generateHeatmapPanel("heatmap2"),
        generateHeatmapPanel("heatmap3")
      )
    )
  )
}

generateHeatmapPanel <- function(heatmapId) {
  exclusiveComponent <- switch(
    heatmapId,
    "heatmap1" = {
      tabName <- paste0(str_to_title(UI_TERM_KEYWORD[[currentEnrichmentType]]),
                        " Vs Genes")
      radioButtons(
        inputId = paste0(currentType_Tool, "_heatmap1_axis"),
        label = "Axes",
        choices = c("Functions-Genes", "Genes-Functions"),
        inline = TRUE,
      )
    },
    "heatmap2" = {
      tabName <- paste0(str_to_title(UI_TERM_KEYWORD[[currentEnrichmentType]]),
                        " Vs ", str_to_title(UI_TERM_KEYWORD[[currentEnrichmentType]]))
      NULL
    },
    "heatmap3" = {
      tabName <- "Genes Vs Genes"
      NULL
    }
  )
  
  return(
    tabPanel(
      title = tabName,
      tags$br(),
      fluidRow(
        column(
          4,
          selectInput(
            inputId = paste(currentType_Tool, heatmapId, "sourceSelect", sep = "_"),
            label = "Select term datasource:", 
            choices = NULL
          )
        ),
        column(
          4,
          sliderInput(
            inputId = paste(currentType_Tool, heatmapId, "slider", sep = "_"),
            label = paste0("Filter number of top ", UI_TERM_KEYWORD[[currentEnrichmentType]], ":"),
            min = 1, max = 10, value = 10, step = 1
          )
        ),
        column(
          4, 
          radioButtons(
            inputId = paste(currentType_Tool, heatmapId, "mode", sep = "_"),
            label = paste0("Order retrieved ", UI_TERM_KEYWORD[[currentEnrichmentType]], " by:"),
            choices = c("-log10Pvalue", "Enrichment Score"),
            inline = TRUE
          )
        )
      ),
      fluidRow(
        column(
          8,
          actionButton(
            inputId = paste(currentType_Tool, heatmapId, "visualizeHeatmap", sep = "_"),
            label = "Visualize",
            icon("paper-plane"), class = "btn-submit")
        ),
        column(
          4, 
          tags$div(
            class = "heatmapAxis",
            exclusiveComponent
          )
        )
      ),
      tags$hr(),
      tags$div(
        class = "heatmapOutput",
        plotlyOutput(paste(currentType_Tool, heatmapId, sep = "_"))
      ),
      tags$br(),
      DT::dataTableOutput(paste(currentType_Tool, heatmapId, "table", sep = "_"))
    )
  )
}

generateBarchartPanel <- function() {
  tabPanel(
    "Barchart",
    tags$br(),
    fluidRow(
      column(
        4, 
        pickerInput(
          inputId = paste(currentType_Tool, "barchart_sourceSelect", sep = "_"),
          label = "Select term datasources:", 
          choices = NULL, multiple = TRUE,
          options = list('actions-box' = TRUE)
        )
      ),
      column(
        4,
        sliderInput(
          inputId = paste(currentType_Tool, "barchart_slider", sep = "_"),
          label = paste0("Filter number of top ", UI_TERM_KEYWORD[[currentEnrichmentType]], ":"),
          min = 1, max = 10, value = 10, step = 1
        )
      ),
      column(
        4, 
        radioButtons(
          inputId = paste(currentType_Tool, "barchart_mode", sep = "_"),
          label = paste0("Order retrieved ", UI_TERM_KEYWORD[[currentEnrichmentType]], " by:"),
          choices = c("-log10Pvalue", "Enrichment Score"),
          inline = TRUE
        )
      )
    ),
    actionButton(inputId = paste(currentType_Tool, "barchart_button", sep = "_"),
                 label = "Visualize",
                 icon("paper-plane"), class = "btn-submit"),
    tags$hr(),
    tags$div(
      class = "barchartOutput",
      plotlyOutput(paste(currentType_Tool, "barchart", sep = "_"))
    ),
    tags$br(),
    DT::dataTableOutput(paste(currentType_Tool, "barchart_table", sep = "_"))
  )
}

generateScatterplotPanel <- function() {
  tabPanel(
    "Scatter Plot",
    tags$br(),
    fluidRow(
      column(
        4, 
        pickerInput(
          inputId = paste(currentType_Tool, "scatter_sourceSelect", sep = "_"),
          label = "Select term datasources:",
          choices = NULL, multiple = TRUE,
          options = list('actions-box' = TRUE)
        )
      ),
      column(
        4,
        sliderInput(
          inputId = paste(currentType_Tool, "scatter_slider", sep = "_"),
          label = paste0("Filter number of top ", UI_TERM_KEYWORD[[currentEnrichmentType]], ":"),
          min = 1, max = 10, value = 10, step = 1
        )
      ),
      column(
        4, 
        radioButtons(
          inputId = paste(currentType_Tool, "scatter_mode", sep = "_"),
          label = paste0("Order retrieved ", UI_TERM_KEYWORD[[currentEnrichmentType]], " by:"),
          choices = c("-log10Pvalue", "Enrichment Score"),
          inline = TRUE
        )
      )
    ),
    actionButton(inputId = paste(currentType_Tool, "scatter_button", sep = "_"),
                 label = "Visualize",
                 icon("paper-plane"), class = "btn-submit"),
    tags$hr(),
    plotlyOutput(paste(currentType_Tool, "scatterPlot", sep = "_")),
    tags$br(), 
    DT::dataTableOutput(paste(currentType_Tool, "scatterPlot_table", sep = "_"))
  )
}

generateManhattanPanel <- function() {
  if (currentEnrichmentTool != "gProfiler")
    return()
    
  return(
    tabPanel(
      "Manhattan Plot",
      tags$br(),
      actionButton(inputId = "manhattan_button",
                   label = "Visualize",
                   icon("paper-plane"), class = "btn-submit"),
      tags$hr(),
      plotlyOutput("manhattan", width = "100%", inline = FALSE),
      tags$br(),
      DT::dataTableOutput("manhattan_table")
    )
  )
}
