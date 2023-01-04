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
            label = "1. Select list:",
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
            icon("paper-plane"), class = "submit_button")
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
        do.call(
          tabsetPanel,
          (lapply(ENRICHMENT_TOOLS, function(toolName) {
            tabPanel(
              title = toolName,
              generateToolPanel(toolName)
            )
          })
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
