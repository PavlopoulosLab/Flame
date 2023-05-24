generateEnrichmentPage <- function(enrichmentType) {
  currentEnrichmentType <<- enrichmentType
  
  return(
    tags$div(
      tags$h3(paste0(stringr::str_to_title(currentEnrichmentType), " Enrichment Analysis")),
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
      datasourceChoices <- NULL
      datasourceSelected <- NULL
      metrics <- NULL
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
            choices = NULL,
            selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
            multiple = F,
            width = "80%",
            options = list(placeholder = 'Select an option or start typing...')
          ),
          radioGroupButtons(
            inputId = paste0(currentEnrichmentType, "_enrichment_background_choice"),
            label = "3. Select background:",
            choiceNames = c("Whole genome", "User-submitted list"),
            choiceValues = c("genome", "user_list"),
            selected = "genome",
            justified = T,
            width = "80%"
            ),
          div(id=paste0(currentEnrichmentType, "_enrichment_background_container"),
              style="display:none",
          selectInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_background_list"),
            label = "Select background list:",
            choices = NULL,
            width = "80%"
          )),
        ),
        column(
          4,
          pickerInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_tool"),
            label = "4. Select enrichment tool:",
            choices = availableTools,
            selected = DEFAULT_TOOL,
            multiple = T,
            width = "89.5%",
            options = list('actions-box' = TRUE)
          ),
          pickerInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_datasources"),
            label = "5. Select datasources:", 
            choices = datasourceChoices,
            selected = datasourceSelected,
            multiple = T,
            width = "89.5%",
            options = list('actions-box' = TRUE)
          ),
          selectInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_namespace"),
            label = "6. Select namespace conversion:",
            choices = NAMESPACES[["AGOTOOL"]],
            width = "80%"
          ) %>% 
            bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Default tool namespace conversions:
aGOtool: ENSEMBL Protein ID
gProfiler: User Input
WebGestalt: Entrez Gene Accession
enrichR: Entrez Gene Name"
              )
          )
        ),
        column(
          4,
          selectInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_metric"),
            label = "7. Select significance metric:",
            choices = metrics,
            width = "80%"
          ),
          selectInput(
            inputId = paste0(currentEnrichmentType, "_enrichment_threshold"),
            label = "8. Select significance threshold:",
            choices = c(0.05, 0.01),
            width = "80%"
          ),
          radioButtons(
            inputId = paste0(currentEnrichmentType, "_enrichment_inputConversion"),
            label = "9. Select result namespace:",
            choices = c("Original input names", "Converted input names"),
            inline = TRUE
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
          tabsetPanel, c(
            id = "toolTabsPanel",
            lapply(c(ENRICHMENT_TOOLS, "Combination"), function(toolName) {
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
  if (toolName != "Combination") {
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
  } else {
    generateCombinationPanel()
  }
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
    ),
    actionButton(paste(currentType_Tool, "clear", sep = "_"), "Clear", icon("broom"))
  )
}

generateResultsPanel <- function() {
  sourcesPanel <- switch(
    currentEnrichmentType,
    "functional" = {
      do.call(
        tabsetPanel, c(
          id = paste(currentType_Tool, "sources_panel", sep = "_"),
          lapply(TAB_NAMES_CODES, function(tabName) {
            generateEnrichmentTab(tabName)
          })
        )
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
          tabsetPanel(
            tabPanel("Input List", DT::dataTableOutput(paste(currentType_Tool, "conversionTable_input", sep = "_"))),
            tabPanel("Reference Background", 
                     div(id=paste(currentType_Tool, "conversionTable_genome_div", sep = "_"),
                         h3("No custom background was submitted by the user, the entire selected genome was used instead.")),
                     div(id=paste(currentType_Tool, "conversionTable_reference_div", sep = "_"), style="display:none",
                       DT::dataTableOutput(paste(currentType_Tool, "conversionTable_reference", sep = "_")))
                     )
            )
        ),
        box(
          title = "Unconverted Inputs",
          class = "conversionBox",
          width = NULL,
          status = "primary", 
          solidHeader = TRUE,
          collapsible = TRUE,
          verbatimTextOutput(paste(currentType_Tool, "notConverted_input", sep = "_")),
          tags$hr(),
          div(id=paste(currentType_Tool, "notConverted_reference_div", sep = "_"), style="display:none",
            verbatimTextOutput(paste(currentType_Tool, "notConverted_reference", sep = "_"))
            )
        )
      ),
      box(
        title = "No-hit Inputs",
        class = "conversionBox",
        width = NULL,
        status = "primary", 
        solidHeader = TRUE,
        collapsible = TRUE,
        verbatimTextOutput(paste(currentType_Tool, "genesNotFound", sep = "_"))
      )
    )
  )
}

generateEnrichmentTab <- function(tabName) {
  tabPanel(
    title = names(TAB_NAMES[TAB_NAMES == tabName]),
    tags$br(),
    DT::dataTableOutput(paste(currentType_Tool, "table",
                              tabName, sep = "_"))
  )
}

generateCombinationPanel <- function() {
  return(
    tags$div(
      tags$br(),
      pickerInput(
        inputId = "combo_datasources",
        label = "Select datasources:", 
        choices = NULL,
        selected = NULL,
        multiple = T,
        options = list('actions-box' = TRUE)
      ),
      tags$br(),
      tabsetPanel(
        tabPanel(
          title = "Table",
          icon = icon("table"),
          DT::dataTableOutput("combo_table")
        ),
        tabPanel(
          title = "UpSet Plot",
          icon = icon("chart-column"),
          upsetjs::upsetjsOutput("upsetjsCombo"),
          DT::dataTableOutput("combo_upsetClick_table")
        ),
        tabPanel(
          title = "Combo Network",
          icon = icon("network-wired"),
          fluidRow(
            column(
              4,
              pickerInput(
                inputId = "combo_tool_picker",
                label = "Select edges from tools:",
                choices = c(),
                selected = c(),
                multiple = T,
                width = "80%",
                options = list('actions-box' = TRUE)
              ),
              actionButton(
                inputId = "combo_visNetwork_run",
                label = "Visualize Network",
                icon("paper-plane"), class = "submit_button"
              )
            ),
            column(
              4,
              sliderInput(
                inputId = "combo_rank_slider",
                label = "Choose minimum rank threshold:",
                min = 1, max = 1,
                value = 1,
                step = 1,
                width = "70%"
              ),
              selectInput(
                inputId = "combo_network_layout",
                label = "Choose layout algorithm:",
                choices = as.vector(unlist(LAYOUT_CHOICES))
              )
            )
          ),
          tags$div(
            class = "networkOutput",
            visNetworkOutput("combo_visNetwork", height = VIS_NET_HEIGHT)
          ),
          DT::dataTableOutput("combo_network_table")
        )
      )
    )
  )
}
