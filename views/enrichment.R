generateEnrichmentPage <- function(enrichmentType) {
  tags$div(
    tags$h3(paste0(str_to_title(enrichmentType), " Enrichment Analysis")),
    tags$br(),
    generateEnrichmentControlPanel(enrichmentType),
    tags$hr(),
    generateEnrichmentResultsPanel(enrichmentType) # TODO foreach tool from Picker
  )
}

generateEnrichmentControlPanel <- function(enrichmentType = "functional") {
  availableTools <- switch(
    enrichmentType,
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
      "aGoTool"
    }
  )
  
  return(
    tags$div(
      fluidRow(
        column(
          4,
          selectInput(inputId = paste0(enrichmentType, "_enrichment_file"),
                      label = "1. Select file:", choices = NULL),
          selectizeInput(
            inputId = paste0(enrichmentType, "_enrichment_organism"),
            label = "2. Select organism:",
            choices = ORGANISMS_FROM_FILE$print_name,
            selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
            multiple = F,
            options = list(placeholder = 'Select an option or start typing...')
          )
        ),
        column(
          4,
          selectInput(inputId = paste0(enrichmentType, "_enrichment_tool"),
                      label = "3. Select enrichment tool:",
                      choices = availableTools,
                      selected = DEFAULT_TOOL),
          pickerInput(
            inputId = paste0(enrichmentType, "_enrichment_datasources"),
            label = "4. Select datasources:", 
            choices = datasourceChoices,
            selected = datasourceSelected,
            multiple = T,
            options = list('actions-box' = TRUE)
          ),
          selectInput(
            inputId = paste0(enrichmentType, "_enrichment_namespace"),
            label = "5. Select namespace conversion:",
            choices = eval(
              parse(text = paste0(DEFAULT_TOOL_UPPER, "_NAMESPACES")))
          )
        ),
        column(
          4,
          selectInput(
            inputId = paste0(enrichmentType, "_enrichment_metric"),
            label = "6. Select significance metric:",
            choices = metrics,
          ),
          selectInput(
            inputId = paste0(enrichmentType, "_enrichment_threshold"),
            label = "7. Select significance threshold:",
            choices = c(0.05, 0.01)
          )
        )
      ),
      fluidRow(
        column(
          4,
          actionButton(
            inputId = paste0(enrichmentType, "_enrichment_run"),
            label = "Run analysis",
            icon("paper-plane"), class = "btn-submit")
          )
      )
    )
  )
}

generateEnrichmentResultsPanel <- function(enrichmentType = "functional") {
  tags$div(
    generateParametersBox(enrichmentType),
    tabsetPanel(
      generateResultsPanel(enrichmentType),
      tabPanel(
        "Plots", 
        icon = icon("chart-bar"),
        tabsetPanel(
          generateNetworkPanels(enrichmentType),
          generateHeatmapPanels(enrichmentType),
          generateBarchartPanel(enrichmentType),
          generateScatterplotPanel(enrichmentType),
          generateManhattanPanel(enrichmentType)
        )
      )
    )
  )
}

generateParametersBox <- function(enrichmentType) {
  box(
    title = "Parameters", 
    width = NULL,
    status = "primary", 
    solidHeader = T,
    collapsible = T,
    verbatimTextOutput(outputId = paste0(enrichmentType, "_enrichment_parameters"))
  )
}

generateResultsPanel <- function(enrichmentType) {
  sourcesPanel <- switch(
    enrichmentType,
    "functional" = {
      tabsetPanel(
        id = paste0(enrichmentType, "_sources_panel"),
        tabPanel("ALL", tags$br(), DT::dataTableOutput("functional_table_all")),
        tabPanel("INTERPRO", tags$br(), DT::dataTableOutput("functional_table_interpro")),
        tabPanel("PFAM", tags$br(), DT::dataTableOutput("functional_table_pfam")),
        tabPanel("UNIPROT", tags$br(), DT::dataTableOutput("functional_table_uniprot")),
        tabPanel("GO:MF", tags$br(), DT::dataTableOutput("functional_table_gomf")),
        tabPanel("GO:CC", tags$br(), DT::dataTableOutput("functional_table_gocc")),
        tabPanel("GO:BP", tags$br(), DT::dataTableOutput("functional_table_gobp")),
        tabPanel("KEGG", tags$br(), DT::dataTableOutput("functional_table_kegg")),
        tabPanel("REAC", tags$br(), DT::dataTableOutput("functional_table_reac")),
        tabPanel("WP", tags$br(), DT::dataTableOutput("functional_table_wp")),
        tabPanel("DO", tags$br(), DT::dataTableOutput("functional_table_do")),
        tabPanel("BTO", tags$br(), DT::dataTableOutput("functional_table_brenda")),
        tabPanel("TF", tags$br(), DT::dataTableOutput("functional_table_tf")),
        tabPanel("MIRNA", tags$br(), DT::dataTableOutput("functional_table_mirna")),
        tabPanel("CORUM", tags$br(), DT::dataTableOutput("functional_table_corum")),
        tabPanel("HPA", tags$br(), DT::dataTableOutput("functional_table_hpa")),
        tabPanel("HP", tags$br(), DT::dataTableOutput("functional_table_hp"))
      )
    },
    "literature" = {
      tabsetPanel(
        id = paste0(enrichmentType, "_sources_panel"),
        tabPanel("PUBMED", tags$br(), DT::dataTableOutput("literature_table_pubmed"))
      )
    }
  )
  
  return(
    tabPanel(
      title = "Results",
      icon = icon("table"),
      tags$div(
        id = paste0(enrichmentType, "ResultsDiv"),
        class = "enrichmentResultsDiv",
        tags$br(),
        sourcesPanel
      ),
      tags$br(),
      tags$div(
        id = paste0(enrichmentType, "_conversionBoxes"),
        box(
          title = "Conversion Table", 
          width = NULL,
          status = "primary", 
          solidHeader = TRUE,
          collapsible = TRUE,
          DT::dataTableOutput(paste0(enrichmentType, "_conversionTable"))
        ),
        box(
          title = "Unconverted Genes", 
          width = NULL,
          status = "primary", 
          solidHeader = TRUE,
          collapsible = TRUE,
          verbatimTextOutput(paste0(enrichmentType, "_notConverted"))
        )
      ),
      box(
        title = "No-hit Inputs", 
        width = NULL,
        status = "primary", 
        solidHeader = TRUE,
        collapsible = TRUE,
        verbatimTextOutput(paste0(enrichmentType, "_genesNotFound"))
      )
    )
  )
}

generateNetworkPanels <- function(enrichmentType) {
  tabPanel(
    "Network",
    tags$br(),
    tabsetPanel(
      generateNetworkPanel("network1", enrichmentType),
      generateNetworkPanel("network2", enrichmentType),
      generateNetworkPanel("network3", enrichmentType)
    )
  )
}

generateNetworkPanel <- function(networkId, enrichmentType) {
  exclusiveComponent <- switch(
    networkId,
    "network1" = {
      tabName <- "Functions Vs Genes"
      NULL
    },
    "network2" = {
      tabName <- "Functions Vs Functions"
      sliderInput(
        inputId = paste0(enrichmentType, "_network2_thresholdSlider"),
        label = "Similarity score cut-off (%):",
        min = 0, max = 100, value = 30, step = 1)
    },
    "network3" = {
      tabName <- "Genes Vs Genes"
      sliderInput(
        inputId = paste0(enrichmentType, "_network3_thresholdSlider"),
        label = "Number of common functions:",
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
            inputId = paste(enrichmentType, networkId, "sourceSelect", sep = "_"),
            label = "Select term datasources:",
            choices = NULL,
            multiple = TRUE,
            options = list('actions-box' = TRUE)
          )
        ),
        column(
          4,
          selectInput(
            inputId = paste(enrichmentType, networkId, "layout", sep = "_"),
            label = "Choose layout algorithm:",
            choices = as.vector(unlist(LAYOUT_CHOICES))
          )
        ),
        column(
          4, 
          radioButtons(
            inputId = paste(enrichmentType, networkId, "mode", sep = "_"),
            label = "Order retrieved terms by:",
            choices = c("-log10Pvalue", "Enrichment Score"), inline = TRUE)
        )
      ),
      fluidRow(
        column(
          4,
          actionButton(
            inputId = paste(enrichmentType, networkId, "arena", sep = "_"),
            label = "Visualize 3D",
            class = "arena_button"
          )
        ),
        column(
          4,
          sliderInput(
            inputId = paste(enrichmentType, networkId, "slider", sep = "_"),
            label = "Filter number of top terms:",
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
            inputId = paste(enrichmentType, networkId, "visualizeNetwork", sep = "_"),
            label = "Visualize",
            icon("paper-plane"), class = "btn-submit")
        )
      ),
      tags$hr(),
      visNetworkOutput(paste(enrichmentType, networkId, sep = "_"),
                       height = VIS_NET_HEIGHT),
      tags$br(),
      generateColorCodingLegend(),
      DT::dataTableOutput(paste(enrichmentType, networkId, "edgelist", sep = "_")),
      tags$br(),
      DT::dataTableOutput(paste(enrichmentType, networkId, "table", sep = "_"))
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

generateHeatmapPanels <- function(enrichmentType) {
  tabPanel(
    "Heatmap",
    tags$br(),
    tags$div(
      tabsetPanel(
        generateHeatmapPanel("heatmap1", enrichmentType),
        generateHeatmapPanel("heatmap2", enrichmentType),
        generateHeatmapPanel("heatmap3", enrichmentType)
      )
    )
  )
}

generateHeatmapPanel <- function(heatmapId, enrichmentType) {
  exclusiveComponent <- switch(
    heatmapId,
    "heatmap1" = {
      tabName <- "Functions Vs Genes"
      radioButtons(
        inputId = paste0(enrichmentType, "_heatmap1_axis"),
        label = "Axes",
        choices = c("Functions-Genes", "Genes-Functions"),
        inline = TRUE,
      )
    },
    "heatmap2" = {
      tabName <- "Functions Vs Functions"
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
            inputId = paste(enrichmentType, heatmapId, "sourceSelect", sep = "_"),
            label = "Select term datasource:", 
            choices = NULL
          )
        ),
        column(
          4,
          sliderInput(
            inputId = paste(enrichmentType, heatmapId, "slider", sep = "_"),
            label = "Filter number of top terms:",
            min = 1, max = 10, value = 10, step = 1
          )
        ),
        column(
          4, 
          radioButtons(
            inputId = paste(enrichmentType, heatmapId, "mode", sep = "_"),
            label = "Order retrieved terms by:",
            choices = c("-log10Pvalue", "Enrichment Score"),
            inline = TRUE
          )
        )
      ),
      fluidRow(
        column(
          8,
          actionButton(
            inputId = paste(enrichmentType, heatmapId, "visualizeHeatmap", sep = "_"),
            label = "Visualize",
            icon("paper-plane"), class = "btn-submit")
        ),
        column(4, exclusiveComponent)
      ),
      tags$hr(),
      plotlyOutput(paste(enrichmentType, heatmapId, sep = "_")),
      tags$br(),
      DT::dataTableOutput(paste(enrichmentType, heatmapId, "table", sep = "_"))
    )
  )
}

generateBarchartPanel <- function(enrichmentType) {
  tabPanel(
    "Barchart",
    tags$br(),
    fluidRow(
      column(
        4, 
        pickerInput(
          inputId = paste0(enrichmentType, "_barchart_sourceSelect"),
          label = "Select term datasources:", 
          choices = NULL, multiple = TRUE,
          options = list('actions-box' = TRUE)
        )
      ),
      column(
        4,
        sliderInput(
          inputId = paste0(enrichmentType, "_barchart_slider"),
          label = "Filter number of top terms:",
          min = 1, max = 10, value = 10, step = 1
        )
      ),
      column(
        4, 
        radioButtons(
          inputId = paste0(enrichmentType, "_barchart_mode"),
          label = "Order retrieved terms by:",
          choices = c("-log10Pvalue", "Enrichment Score"),
          inline = TRUE
        )
      )
    ),
    actionButton(inputId = paste0(enrichmentType, "_barchart_button"),
                 label = "Visualize",
                 icon("paper-plane"), class = "btn-submit"),
    tags$hr(),
    plotlyOutput(paste0(enrichmentType, "_barchart")),
    tags$br(),
    DT::dataTableOutput(paste0(enrichmentType, "_barchart_table"))
  )
}

generateScatterplotPanel <- function(enrichmentType) {
  tabPanel(
    "Scatter Plot",
    tags$br(),
    fluidRow(
      column(
        4, 
        pickerInput(
          inputId = paste0(enrichmentType, "_scatter_sourceSelect"),
          label = "Select term datasources:",
          choices = NULL, multiple = TRUE,
          options = list('actions-box' = TRUE)
        )
      ),
      column(
        4,
        sliderInput(
          inputId = paste0(enrichmentType, "_scatter_slider"),
          label = "Filter number of top terms:",
          min = 1, max = 10, value = 10, step = 1
        )
      ),
      column(
        4, 
        radioButtons(
          inputId = paste0(enrichmentType, "_scatter_mode"),
          label = "Order retrieved terms by:",
          choices = c("-log10Pvalue", "Enrichment Score"),
          inline = TRUE
        )
      )
    ),
    actionButton(inputId = paste0(enrichmentType, "_scatter_button"),
                 label = "Visualize",
                 icon("paper-plane"), class = "btn-submit"),
    tags$hr(),
    plotlyOutput(paste0(enrichmentType, "_scatterPlot")),
    tags$br(), 
    DT::dataTableOutput(paste0(enrichmentType, "_scatterPlot_table"))
  )
}

generateManhattanPanel <- function(enrichmentType) {
  if (enrichmentType == "literature")
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
