generateGprofilerPage <- function() {
  tags$div(
    h3("Functional Enrichment Analysis: gProfiler"),
    br(),
    fluidRow(
      column(4,
             selectInput("selectEnrichFile", "Select file for analysis", width = "65%",
                         choices = NULL),
             selectizeInput("organism", label = " Select organism:",choices = ORGANISMS_FROM_FILE$print_name, multiple = F,
                            selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",width = "65%",
                            options = list(placeholder = 'Select an option or start typing...'))
      ),
      column(4,
             pickerInput("datasources", "Select datasources", 
                         choices=list('Gene Ontology'=list("Gene Ontology-Molecular Function (GO:MF)"="GO:MF", "Gene Ontology-Cellular Component (GO:CC)"= "GO:CC", "Gene Ontology-Biological Process (GO:BP)"="GO:BP"),
                                      'Biological Pathways'= list("KEGG PATHWAY"="KEGG", "Reactome"="REAC", "WikiPathways"="WP"),
                                      'Regulatory motifs in DNA'= list("TRANSFAC"= "TF","miRTarBase"= "MIRNA"),
                                      'Protein Databases' = list( "Human Protein Atlas (HPA)"="HPA","CORUM"= "CORUM"),
                                      'Human Phenotype Ontology' =list("Human Phenotype Ontology"= "HP")),
                         options = list('actions-box' = TRUE), multiple = TRUE,
                         selected = c("Gene Ontology-Molecular Function (GO:MF)"="GO:MF","Gene Ontology-Cellular Component (GO:CC)"= "GO:CC",
                                      "Gene Ontology-Biological Process (GO:BP)"="GO:BP","KEGG PATHWAY"="KEGG")
                         
             ),
             selectInput(
               inputId = "gconvertTargetGprofiler",
               label = "Select namespace conversion:",
               choices = c(
                 "User Input" = "USERINPUT", "ENSEMBL Gene ID" = "ENSG",
                 "ENSEMBL Protein ID" = "ENSP", "ENSEMBL Transcript ID" = "ENST",
                 "Entrez Gene Name" = "ENTREZGENE", "Entrez Gene Accession" = "ENTREZGENE_ACC",
                 "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME",
                 "UniProt Gene Name" = "UNIPROT_GN", "UniProt Accession" = "UNIPROT_GN_ACC", 
                 "UniProt Archive" = "UNIPARC", "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC",
                 "RefSeq mRNA" = "REFSEQ_MRNA", "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
                 "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC",
                 "EMBL Accession" = "EMBL", "ChEMBL" = "CHEMBL", 
                 "WIKIGENE ID" = "WIKIGENE" 
               ),
               selected = "USERINPUT"
             )
      ),
      column(4,
             selectInput("threshold","Significance threshold:", choices = c("g:SCS threshold"="gSCS", "Benjamini-Hochberg FDR"="fdr","Bonferroni correction"="bonferroni"), selected = ("g:SCS threshold"="gSCS")),
             selectInput("user_pvalue", "P-value correction cut-off:", choices = c(0.05, 0.01)))
    ),
    actionButton("gprofiler", "Run analysis", icon("paper-plane"), class = "btn-submit"),
    tags$br(),
    tags$hr(),
    box(
      title = "Parameters", 
      width = NULL,
      status = "primary", 
      solidHeader = T,
      collapsible = T,
      verbatimTextOutput("gprofParameters")
    ),
    tabsetPanel(
      tabPanel(
        title = "Results",
        icon = icon("table"),
        tags$div(
          id = "enrichmentResultsDiv",
          tags$br(),
          tabsetPanel(
            id = "sources_panel_gprofiler",
            tabPanel("ALL", tags$br(), DT::dataTableOutput("table_all")),
            tabPanel("GO:MF", tags$br(), DT::dataTableOutput("table_gomf")),
            tabPanel("GO:CC", tags$br(), DT::dataTableOutput("table_gocc")),
            tabPanel("GO:BP", tags$br(), DT::dataTableOutput("table_gobp")),
            tabPanel("KEGG", tags$br(), DT::dataTableOutput("table_kegg")),
            tabPanel("REAC", tags$br(), DT::dataTableOutput("table_reac")),
            tabPanel("WP", tags$br(), DT::dataTableOutput("table_wp")),
            tabPanel("TF", tags$br(), DT::dataTableOutput("table_tf")),
            tabPanel("MIRNA", tags$br(), DT::dataTableOutput("table_mirna")),
            tabPanel("CORUM", tags$br(), DT::dataTableOutput("table_corum")),
            tabPanel("HPA", tags$br(), DT::dataTableOutput("table_hpa")),
            tabPanel("HP", tags$br(), DT::dataTableOutput("table_hp"))
          )
        ),
        tags$br(),
        tags$div(
          id = "conversionBoxes",
          box(
            title = "Conversion Table", 
            width = NULL,
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("conversionTable")
          ),
          box(
            title = "Unconverted Genes", 
            width = NULL,
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("notConverted")
          )
        ),
        box(
          title = "No-hit Inputs", 
          width = NULL,
          status = "primary", 
          solidHeader = TRUE,
          collapsible = TRUE,
          verbatimTextOutput("genesNotFound")
        ),
      ),
      tabPanel(
        "Plots", 
        icon = icon("chart-bar"),
        tabsetPanel(
          tabPanel(
            "Network",
            tags$br(),
            generateNetworkPanels()
          ),
          tabPanel(
            "Heatmap",
            tags$br(),
            generateHeatmapPanels()
          ),
          tabPanel(
            "Barchart",
            tags$br(),
            fluidRow(
              column(
                4, 
                pickerInput(
                  inputId = "barchart_sourceSelect",
                  label = "Select term datasources:", 
                  choices = NULL, multiple = TRUE,
                  options = list('actions-box' = TRUE)
                )
              ),
              column(
                4,
                sliderInput(
                  inputId = "barchart_slider",
                  label = "Filter number of top terms:",
                  min = 1, max = 10, value = 10, step = 1
                )
              ),
              column(
                4, 
                radioButtons(
                  inputId = "barchart_mode",
                  label = "Order retrieved terms by:",
                  choices = c("Enrichment Score", "-log10Pvalue"),
                  inline = TRUE
                )
              )
            ),
            actionButton(inputId = "barchart_button",
                         label = "Visualize",
                         icon("paper-plane"), class = "btn-submit"),
            tags$hr(),
            plotlyOutput("barchart"),
            tags$br(),
            DT::dataTableOutput("barchart_table")
          ),
          tabPanel(
            "Scatter Plot",
            tags$br(),
            fluidRow(
              column(
                4, 
                pickerInput(
                  inputId = "scatter_sourceSelect",
                  label = "Select term datasources:",
                  choices = NULL, multiple = TRUE,
                  options = list('actions-box' = TRUE)
                )
              ),
              column(
                4,
                sliderInput(
                  inputId = "scatter_slider",
                  label = "Filter number of top terms:",
                  min = 1, max = 10, value = 10, step = 1
                )
              ),
              column(
                4, 
                radioButtons(
                  inputId = "scatter_mode",
                  label = "Order retrieved terms by:",
                  choices = c("Enrichment Score", "-log10Pvalue"),
                  inline = TRUE
                )
              )
            ),
            actionButton(inputId = "scatter_button",
                         label = "Visualize",
                         icon("paper-plane"), class = "btn-submit"),
            tags$hr(),
            plotlyOutput("scatterPlot"),
            tags$br(), 
            DT::dataTableOutput("scatterPlot_table")
          ),
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
      )
    )
  )
}

generateHeatmapPanels <- function() {
  tags$div(
    tabsetPanel(
      generateHeatmapPanel("heatmap1"),
      generateHeatmapPanel("heatmap2"),
      generateHeatmapPanel("heatmap3")
    )
  )
}

generateHeatmapPanel <- function(heatmapId) {
  exclusiveComponent <- switch(
    heatmapId,
    "heatmap1" = {
      tabName <- "Functions Vs Genes"
      radioButtons(
        inputId = "heatmap1_axis",
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
            inputId = paste0(heatmapId, "_sourceSelect"),
            label = "Select term datasource:", 
            choices = NULL
          )
        ),
        column(
          4,
          sliderInput(
            inputId = paste0(heatmapId, "_slider"),
            label = "Filter number of top terms:",
            min = 1, max = 10, value = 10, step = 1
          )
        ),
        column(
          4, 
          radioButtons(
            inputId = paste0(heatmapId, "_mode"),
            label = "Order retrieved terms by:",
            choices = c("Enrichment Score", "-log10Pvalue"),
            inline = TRUE
          )
        )
      ),
      fluidRow(
        column(
          8,
          actionButton(inputId = paste0(heatmapId, "_visualizeHeatmap"),
                       label = "Visualize",
                       icon("paper-plane"), class = "btn-submit")
        ),
        column(4, exclusiveComponent)
      ),
      tags$hr(),
      plotlyOutput(heatmapId),
      tags$br(),
      DT::dataTableOutput(paste0(heatmapId, "_table"))
    )
  )
}

generateNetworkPanels <- function() {
  tabsetPanel(
    generateNetworkPanel("network1"),
    generateNetworkPanel("network2"),
    generateNetworkPanel("network3")
  )
}

generateNetworkPanel <- function(networkId) {
  exclusiveComponent <- switch(
    networkId,
    "network1" = {
      tabName <- "Functions Vs Genes"
      NULL
    },
    "network2" = {
      tabName <- "Functions Vs Functions"
      sliderInput("network2_thresholdSlider", "Similarity score cut-off (%):",
                  min = 0, max = 100, value = 30, step = 1)
    },
    "network3" = {
      tabName <- "Genes Vs Genes"
      sliderInput("network3_thresholdSlider", "Number of common functions:",
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
            inputId = paste0(networkId, "_sourceSelect"),
            label = "Select term datasources:",
            choices = NULL,
            multiple = TRUE,
            options = list('actions-box' = TRUE)
          )
        ),
        column(
          4,
          selectInput(
            inputId = paste0(networkId, "_layout"),
            label = "Choose layout algorithm:",
            choices = as.vector(unlist(LAYOUT_CHOICES))
          )
        ),
        column(
          4, 
          radioButtons(paste0(networkId, "_mode"), "Order retrieved terms by:",
                       choices = c("Enrichment Score", "-log10Pvalue"),
                       inline = TRUE)
        )
      ),
      fluidRow(
        column(
          4,
          actionButton(
            inputId = paste0(networkId, "_arena"),
            label = "Visualize 3D",
            class = "arena_button"
          )
        ),
        column(
          4,
          sliderInput(paste0(networkId, "_slider"),
                      "Filter number of top terms:",
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
          actionButton(inputId = paste0(networkId, "_visualizeNetwork"),
                      label = "Visualize",
                      icon("paper-plane"), class = "btn-submit")
        )
      ),
      tags$hr(),
      visNetworkOutput(networkId, height = VIS_NET_HEIGHT),
      tags$br(),
      generateColorCodingLegend(),
      DT::dataTableOutput(paste0(networkId, "_edgelist")),
      tags$br(),
      DT::dataTableOutput(paste0(networkId, "_table"))
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
      collapsed = F,
      width = 12,
      column(
        3,
        tags$div(
          tags$p("GO:MF"),
          style = paste0("background-color: ", DATASOURCE_COLORS["GO:MF"][[1]], ";")
        ),
        tags$div(
          tags$p("GO:BP"), style = paste0("background-color: ", DATASOURCE_COLORS["GO:BP"][[1]], ";")
        ),
        tags$div(
          tags$p("GO:CC"), style = paste0("background-color: ", DATASOURCE_COLORS["GO:CC"][[1]], ";")
        )
      ),
      column(
        3,
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
        3,
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
          3,
          tags$div(
            tags$p("CORUM"),
            style = paste0("background-color: ", DATASOURCE_COLORS["CORUM"][[1]], ";")
          ),
          tags$div(
            tags$p("HPO"),
            style = paste0("background-color: ", DATASOURCE_COLORS["HP"][[1]], ";")
          ),
          tags$div(
            tags$p("Gene"),
            style = paste0("background-color: ", GENE_NODE_COLOR, "; color: black;")
          )
        )
    )
  )
}

