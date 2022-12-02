generateLiteraturePage <- function() {
  tags$div(
    h3("Literature Enrichment"),
    br(),
    fluidRow(
      column(4, selectInput("literatureSelect", "Select file for analysis", choices = NULL,width = "75%")),
      
      column(4, 
             selectizeInput("literatureOrganism", label = " Select organism:",choices = ORGANISMS_FROM_FILE$print_name, multiple = F,
                            selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",width = "75%",
                            options = list(placeholder = 'Select an option or start typing...')),
             selectInput("gconvertTargetLiterature","Select ID type for output:", selected = "USERINPUT",width = "75%",
                         choices = c("User Input"="USERINPUT", "ChEMBL"="CHEMBL", "Entrez Gene Name" = "ENTREZGENE", "Entrez Gene Accession" = "ENTREZGENE_ACC",
                                     "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME", "UniProt Accession" = "UNIPROT_GN_ACC",
                                     "UniProt Gene Name" = "UNIPROT_GN", "EMBL Accession" = "EMBL", "ENSEMBL Protein ID" = "ENSP",
                                     "ENSEMBL Gene ID" = "ENSG", "ENSEMBL Transcript ID" = "ENST", "UniProt Archive" ="UNIPARC",
                                     "WIKIGENE ID" = " WIKIGENE", "RefSeq mRNA" = "REFSEQ_MRNA", "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
                                     "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC", "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC"))
      ),
      column(4,
             selectInput("literatureCorrectionMethod","Select significance threshold:", choices = c("P-value", "Corrected P-value (FDR)")),
             selectInput("literaturePvalue", "P-value correction cut-off:", 
                         choices = list(0.10, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01),
                         selected = 0.05))
    ),
    actionButton("literature", "Run analysis",icon("paper-plane"), class = "btn-submit"),
    br(),
    tags$hr(),
    br(),
    uiOutput("literature_Parameters"),
    uiOutput("notconvertLit"),
    uiOutput("nothitLit"),
    br(),
    
    tabsetPanel(
      tabPanel("Results",icon = icon("table"),
               br(),
               DT::dataTableOutput("literatureTable"),
      ),#tabPanel Results end
      tabPanel("Plots",icon=icon("chart-bar"),
               tabsetPanel(
                 tabPanel("Scatter Plot",
                          br(),
                          fluidRow(
                            column(12, sliderInput("literatureSliderScatter", "Choose the number of results:", min = 1, max = 10, value = 10, step = 1))
                          ),
                          br(),
                          tags$hr(),
                          uiOutput("literatureScatterPlot"),
                          br(),
                          DT::dataTableOutput("literatureScatter_table")
                 ),
                 tabPanel("Barchart",
                          br(),
                          fluidRow(
                            column(6, radioButtons("literatureBarplotMode", "Select barplot", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                            column(6, sliderInput("literatureSliderBarplot", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                   br())
                          ),
                          br(),
                          tags$hr(),
                          uiOutput( "literatureBarplot"),
                          br(),
                          DT::dataTableOutput("literatureBarplot_table")
                 ),
                 tabPanel("Heatmap",
                          br(),
                          tabsetPanel(
                            tabPanel("Heatmap: Publications Vs Genes",
                                     br(),
                                     fluidRow(
                                       column(4, radioButtons("literatureHeatmapMode", "Select type of Heatmap", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                       column(4, radioButtons("literatureHeatmapAxis", "Reverse Axis", choices = c( "Publications-Genes","Genes-Publications"), inline = TRUE)),
                                       column(4, sliderInput("literatureSliderHeatmap", "Choose number of results to view:", min = 2, max = 10, value = 10, step = 1)),
                                     ),
                                     br(),
                                     tags$hr(),
                                     uiOutput("literatureHeatmapPlot"),
                                     br(),
                                     DT::dataTableOutput("literatureHeatmap_table"),
                                     br()
                            ),
                            tabPanel("Heatmap: Publications Vs Publications",
                                     br(),
                                     fluidRow(
                                       column(6, radioButtons("literatureHeatmapMode2", "Select type of Heatmap", choices = c("Enrichment Score","-log10Pvalue"), inline = TRUE)),
                                       column(6, sliderInput("literatureSliderHeatmap2", "Choose number of results to view:", min = 2, max = 10, value = 10, step = 1)),
                                     ),
                                     br(),
                                     tags$hr(),
                                     uiOutput("literatureHeatmapPlot2"),
                                     br(),
                                     DT::dataTableOutput("literatureHeatmap_table2"),
                                     br()
                            )
                          )
                 ), #tabPanel "Heatmap" end
                 tabPanel("Network",
                          br(),
                          tabsetPanel(
                            tabPanel("Network: Publications Vs Genes",
                                     br(),
                                     fluidRow(
                                       column(6, radioButtons("literatureNetworkMode", "Select network", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE),
                                              br(),
                                              actionButton("lit_network_arena", "Visualize 3D", class = "arena_button"),
                                              br()),
                                       column(6, sliderInput("literatureSliderNetwork", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1))
                                     ),
                                     br(),
                                     tags$hr(),
                                     visNetworkOutput("literatureNetwork", height = "1000px"),
                                     br(),
                                     DT::dataTableOutput("literatureNetwork_table"),
                                     DT::dataTableOutput("literatureNetwork_edgelist"),
                                     br()
                            ), # tabPanel"Network: Functions Vs Genes" end
                            
                            tabPanel("Network: Publications Vs Publications",
                                     br(),
                                     fluidRow(
                                       column(4, sliderInput("literatureSliderThreshold", "Similarity Score cut-off (%):", min = 0, max = 100, value = 30, step = 1)),
                                       column(4, radioButtons("literatureNetworkMode2", "Select network", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                       column(4, sliderInput("literatureSliderNetwork2", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                              br())
                                     ),
                                     br(),
                                     tags$hr(),
                                     visNetworkOutput("literatureNetwork2", height = "1000px"),
                                     br(),
                                     DT::dataTableOutput("literatureNetwork_table2"),
                                     DT::dataTableOutput("literatureNetwork_edgelist2"),
                                     br()
                            ),
                            tabPanel("Network: Gene Vs Gene",
                                     br(),
                                     fluidRow(
                                       column(4, sliderInput("literatureSliderThreshold3", "Number of common Publications:", min = 0, max = 100, value = 0, step = 1)),
                                       column(4, radioButtons("literatureNetworkMode3", "Select network", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                       column(4, sliderInput("literatureSliderNetwork3", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                              br())
                                     ),
                                     br(),
                                     tags$hr(),
                                     visNetworkOutput("literatureNetwork3", height = "1000px"),
                                     br(),
                                     DT::dataTableOutput("literatureNetwork_table3"),
                                     DT::dataTableOutput("literatureNetwork_edgelist3"),
                                     br()
                            )
                          )
                 )
               )
      )
    )
  )
}
