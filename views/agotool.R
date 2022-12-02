generateAGOToolPage <- function() {
  tags$div(
    h3("Functional Enrichment Analysis: aGoTool"),
    br(),
    fluidRow(
      column(4, 
             selectInput("aGOtoolSelect", "Select file for analysis", choices = NULL,width = "65%"),
             selectizeInput("aGOtoolOrganism", label = " Select organism:",choices = ORGANISMS_FROM_FILE$print_name, multiple = F,
                            selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",width = "65%",
                            options = list(placeholder = 'Select an option or start typing...'))),
      column(4, 
             pickerInput("aGOtoolDatasources", "Select datasources", 
                         choices=list("PFAM" = -55,"INTERPRO" = -54,"UniProt" = -51,"Disease Ontology" = -26),
                         options = list('actions-box' = TRUE), multiple = TRUE,selected =list("PFAM" = -55,"INTERPRO" = -54,"UniProt" = -51,"Disease Ontology" = -26)
             ),
             selectInput("gconvertTargetGotool","Select ID type for output:",  selected = "USERINPUT",
                         choices = c("User Input"="USERINPUT", "ChEMBL"="CHEMBL", "Entrez Gene Name" = "ENTREZGENE", "Entrez Gene Accession" = "ENTREZGENE_ACC",
                                     "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME", "UniProt Accession" = "UNIPROT_GN_ACC",
                                     "UniProt Gene Name" = "UNIPROT_GN", "EMBL Accession" = "EMBL", "ENSEMBL Protein ID" = "ENSP",
                                     "ENSEMBL Gene ID" = "ENSG", "ENSEMBL Transcript ID" = "ENST", "UniProt Archive" ="UNIPARC",
                                     "WIKIGENE ID" = " WIKIGENE", "RefSeq mRNA" = "REFSEQ_MRNA", "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
                                     "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC", "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC"))
      ),
      column(4,
             selectInput("aGoCorrectionMethod","Select significance threshold:", choices = c("P-value", "Corrected P-value (FDR)")),
             selectInput("aGOtoolPvalue", "P-value correction cut-off:", choices = list(0.10, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01),
                         selected = 0.05))
    ),
    actionButton("aGOtool", "Run analysis",icon("paper-plane"), class = "btn-submit"),
    br(),
    tags$hr(),
    uiOutput("aGo_Parameters"),
    uiOutput("notconvertaGo"),
    uiOutput("nothitaGo"),
    
    tabsetPanel(
      tabPanel("Results",icon = icon("table"),
               br(),
               tabsetPanel(id ="sources_panel_aGoTool",
                           tabPanel("ALL", br(), DT::dataTableOutput("aGo_all")),
                           tabPanel("UNIPROT", br(), DT::dataTableOutput("uniprotTable")),
                           tabPanel("PFAM", br(), DT::dataTableOutput("pfamTable")),
                           tabPanel("INTERPRO", br(), DT::dataTableOutput("interproTable")),
                           tabPanel("Disease Ontology", br(), DT::dataTableOutput("diseaseTable")))
      ),#tabPanel Results end
      tabPanel("Plots",icon=icon("chart-bar"),
               tabsetPanel(
                 tabPanel("Scatter Plot",
                          br(),br(),
                          fluidRow(
                            column(4, selectInput("aGoScatterSelect", "Select datasource", choices = NULL)),
                            column(4, sliderInput("aGoSliderScatter", "Choose a number of results to view:", min = 1, max = 10, value = 10, step = 1))
                          ),
                          br(),
                          tags$hr(),
                          uiOutput("aGoScatterPlot"),
                          br(),
                          DT::dataTableOutput("aGoScatter_table")
                 ),
                 tabPanel("Barchart",
                          br(),br(),
                          fluidRow(
                            column(4,
                                   pickerInput("aGoBarSelect2", "Select datasources",
                                               choices = NULL,
                                               options = list('actions-box' = TRUE), multiple = TRUE
                                   )
                            ),
                            column(4, radioButtons("aGoBarplotMode", "Select barplot", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                            column(4, sliderInput("aGoSliderBarplot", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                   br())
                          ),
                          br(),
                          tags$hr(),
                          uiOutput( "aGoBarplot"),
                          br(),
                          htmlOutput("bar_legend_aGo"),
                          br(),
                          DT::dataTableOutput("aGoBarplot_table")
                 ),
                 tabPanel("Heatmap",
                          br(),br(),
                          tabsetPanel(
                            tabPanel("Heatmap: Functions Vs Genes",
                                     br(),
                                     fluidRow(
                                       column(12, selectInput("aGoHeatmapSelect", "Select datasource", choices = NULL))
                                     ),
                                     fluidRow(
                                       column(4, radioButtons("aGoHeatmapMode", "Select type of Heatmap", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                       column(4, radioButtons("aGoHeatmapAxis", "Reverse Axis", choices = c( "Functions-Genes","Genes-Functions"), inline = TRUE)),
                                       column(4, sliderInput("aGoSliderHeatmap", "Choose number of results to view:", min = 2, max = 10, value = 10, step = 1)),
                                     ),
                                     br(),
                                     tags$hr(),
                                     uiOutput("aGoHeatmapPlot"),
                                     br(),
                                     DT::dataTableOutput("aGoHeatmap_table"),
                                     br()
                            ),
                            tabPanel("Heatmap: Functions Vs Functions",
                                     br(),
                                     fluidRow(
                                       column(4, selectInput("aGoHeatmapSelect2", "Select datasource", choices = NULL)),
                                       column(4, radioButtons("aGoHeatmapMode2", "Select type of Heatmap", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                       column(4, sliderInput("aGoSliderHeatmap2", "Choose number of results to view:", min = 2, max = 10, value = 10, step = 1)),
                                     ),
                                     br(),
                                     tags$hr(),
                                     uiOutput("aGoHeatmapPlot2"),
                                     br(),
                                     DT::dataTableOutput("aGoHeatmap_table2"),
                                     br()
                            )
                          )
                 ), #tabPanel "Heatmap" end
                 tabPanel("Network",
                          br(), br(),
                          tabsetPanel(
                            tabPanel("Network: Functions Vs Genes",
                                     br(),
                                     fluidRow(
                                       column(4, selectInput("aGoNetworkSelect", "Select datasource", choices = NULL),
                                              br(),
                                              actionButton("ago_network_arena", "Visualize 3D", class = "arena_button"),
                                              br()),
                                       column(4, radioButtons("aGoNetworkMode", "Select network", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                       column(4, sliderInput("aGoSliderNetwork", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1))
                                     ),
                                     br(),
                                     tags$hr(),
                                     visNetworkOutput("aGoNetwork", height = "1000px"),
                                     br(),
                                     DT::dataTableOutput("aGoNetwork_table"),
                                     DT::dataTableOutput("aGoNetwork_edgelist"),
                                     br()
                            ), # tabPanel"Network: Functions Vs Genes" end
                            tabPanel("Network: Functions Vs Functions",
                                     br(),
                                     fluidRow(
                                       column(12, selectInput("aGoNetworkSelect2", "Select datasource", choices = NULL))
                                     ),
                                     fluidRow(
                                       column(4, sliderInput("aGoSliderThreshold", "Similarity Score cut-off (%):", min = 0, max = 100, value = 30, step = 1)),
                                       column(4, radioButtons("aGoNetworkMode2", "Select network", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                       column(4, sliderInput("aGoSliderNetwork2", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                              br())
                                     ),
                                     br(),
                                     tags$hr(),
                                     visNetworkOutput("aGoNetwork2", height = "1000px"),
                                     br(),
                                     DT::dataTableOutput("aGoNetwork_table2"),
                                     DT::dataTableOutput("aGoNetwork_edgelist2"),
                                     br()
                            ),# tabPanel("Network: Functions Vs Functions") end
                            tabPanel("Network: Gene Vs Gene",
                                     br(),
                                     fluidRow(
                                       column(12, selectInput("aGoNetworkSelect3", "Select datasource", choices = NULL))
                                     ),
                                     fluidRow(
                                       column(4, sliderInput("aGoSliderThreshold3", "Number of common Functions:", min = 0, max = 100, value = 0, step = 1)),
                                       column(4, radioButtons("aGoNetworkMode3", "Select network", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                       column(4, sliderInput("aGoSliderNetwork3", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                              br())
                                     ),
                                     br(),
                                     tags$hr(),
                                     visNetworkOutput("aGoNetwork3", height = "1000px"),
                                     br(),
                                     DT::dataTableOutput("aGoNetwork_table3"),
                                     DT::dataTableOutput("aGoNetwork_edgelist3"),
                                     br()
                            )# tabPanel("Network: Gene Vs Gene") end
                          ) #tabsetPanel network end
                          
                 )
               )
      )
    )
  )
}
