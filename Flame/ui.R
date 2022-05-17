# Required Libraries
library(shinydashboard)
library(upsetjs)
library(shinyWidgets)
library(plotly)
library(stringr)
library(gprofiler2)
library(DT)
library(rlist)
library(heatmaply)
library(igraph)
library(visNetwork)
library(httr)
library(htmltools)
library(bsplus)
library(curl)
library(jsonlite)

source("R_functions/help.R", local=TRUE) # help pages

options(bitmapType = "cairo")
#set the max size for each file
options(shiny.maxRequestSize = 1.0 *1024^2) #uploaded files <1MB each

dashboardPage(title="Flame", skin="yellow",
              
              dashboardHeader(
                titleWidth = "356px", # set width of title
                title = tags$a(href='http://bib.fleming.gr:8084/app/flame/', tags$img(src='logo.png')) # Use image in title
                
              ), # dashboardHeader End
              
              dashboardSidebar(
                
                width = "356px",
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "Flame.css")), # CSS
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href="loading-bar.css")), # loading bar CSS
                tags$head(tags$script(src = "Flame.js")),
                tags$head(tags$script(src = "rshiny_handlers.js")), # R to JS
                tags$head(tags$script(src = "update_rshiny_values.js")), # JS to R
                tags$head(tags$script(src = "slider_update_overide.js")),
                tags$head(tags$script(src = "loading-bar.js")), # loading bar JS
                sidebarMenu(
                  id="A",
                  menuItem("HomePage", tabName ="homepage", icon = icon("home")),
                  tags$hr(),
                  menuItem("File Input", tabName = "file_handler", icon = icon("file alternate outline")),
                  tags$hr(),
                  menuItem("Functional Enrichment Analysis", tabName = "fEnrichment", icon = icon("cogs"),
                           menuSubItem("Ontologies & Pathways (gProfiler)", tabName = "gProfiler", icon = icon("dna")),
                           menuSubItem("Domains & Diseases (aGOtool)", tabName = "aGOtool",icon = icon("heartbeat"))),
                  tags$hr(),
                  menuItem("Literature Enrichment", tabName = "literature_search", icon = icon("book")),
                  tags$hr(),
                  menuItem("Network Analysis", tabName = "string_network", icon=icon("project-diagram")),
                  tags$hr(),
                  menuItem("Conversion", tabName = "Conversion", icon = icon("exchange"),
                           menuSubItem("Gene ID Conversion", tabName = "gconvert"),
                           menuSubItem("Orthology Search", tabName = "gorth")),
                  tags$hr(),
                  menuItem("Help", tabName = "help", icon = icon("question")),
                  menuItem("About", tabName = "about", icon = icon("info"))
                )
              ), # dashboardSidebar End
              
              dashboardBody(
                div(class="ldBar", "data-preset"="bubble"),
                tabItems(
                  tabItem("homepage", 
                          fluidRow(column(12,
                                          h1(strong(HTML("Welcome to <i>Flame</i>"))),
                                          h3(strong("A web tool for Functional and Literature Enrichment analysis of multiple sets")),
                                          tags$hr(style="height:2px; border-color: #ffa600;"),
                                          h3(HTML("With <i>Flame</i> one can:")),
                                          h4(HTML("
                                           <ul>
                                        <li>Upload <b>multiple</b> gene/protein lists<br>
                                        <li> Combine lists by calculating their unions and intersections with the help of <b>UpSet plots</b><br>
                                        <li>Perform <b>functional enrichment</b> analysis on any of the combined lists<br>
                                        <li>Perform <b>literature enrichment</b> analysis on any of the combined lists<br>
                                        <li>Generate protein-protein interaction networks<br>
                                        <li><b>Visualize</b> and <b>export</b> the functional enrichment results<br>
                                        <li><b>Explore</b> results with the use of heatmaps, bar charts, Manhattan plots, networks and tables<br>
                                        <li>Perform <b>network analysis</b> and see gene-function, function-function and gene-gene associations<br>
                                        <li>Apply <b>interactive</b> filters on the generated plots<br>
                                        <li> Perform <b>cross-database</b> and <b>cross-species id conversions</b>
                                         </ul>
                                        <br><br>
                                        Get started by uploading your gene lists"),actionLink("link_to_fileinput", "here.")
                                          ),
                                          textOutput("url_checker"),
                                          
                          )
                          )
                  ),
                  tabItem("file_handler",
                          h3("File Input"),
                          br(),
                          tabsetPanel(id="panels",
                                      tabPanel("Upload Files", 
                                               fluidRow(
                                                 column(4,
                                                        br(),
                                                        fileInput("files", "1. Choose a File", multiple = TRUE,
                                                                  accept = c(".tsv", ".csv", ".txt"))%>%
                                                          shinyInput_label_embed(
                                                            shiny_iconlink() %>%
                                                              bs_embed_popover(
                                                                title = "Upload up to 10 files (up to 1MB each).", content = "Choose a favorite", placement = "left"
                                                              )
                                                          )
                                                 ),
                                                 column(4, 
                                                        br(),
                                                        textAreaInput("text","or 2. Paste your gene list", placeholder = "Write or paste your list here.\nClick the Example Button to load an example list.", height="80px") %>%
                                                          shinyInput_label_embed(
                                                            shiny_iconlink() %>%
                                                              bs_embed_popover(
                                                                title = "Input can consist of mixed typed of IDs separated by comma, space, new line or tab.", placement = "left"
                                                              )
                                                          ),
                                                        actionButton("textSubmit", "Add to files ", icon("paper-plane")),
                                                        actionButton("example", "Example", icon("example")),
                                                        actionButton("clear", "Clear", icon("broom"))
                                                 ),
                                                 column(4,
                                                        br(),
                                                        verbatimTextOutput("url_checked"),
                                                        checkboxGroupInput("checkboxFiles", label = ("3.Select files for Upset plot or Rename/Remove your uploaded Files")),
                                                        checkboxInput("selectAll", "Select/Deselect All"),
                                                        actionButton("rename", "Rename", icon("pencil alternate")),
                                                        actionButton("remove", "Remove", icon("trash"))
                                                 ),
                                               ), 
                                               br(),
                                               tags$hr(),
                                               actionButton("submitUpset", "Create Upset plot", class = "btn-submit"),
                                               br(),br(),
                                               column(4, radioButtons("mode", "Select Mode", c("Intersection","Distinct Intersections", "Distinct per File", "Union") , inline = TRUE)%>%
                                                        shinyInput_label_embed(
                                                          shiny_iconlink() %>%
                                                            bs_embed_popover(
                                                              title = "Select at least 2 files to create the Upset plot from the checkbox file list.
                                                    \nThe UpSet plot Intersection option visualizes the total number of common elements amongthe selected sets, even though they may also participate in other sets.
                                                    \nThe Distinct Intersections option visualizes the common number of genes, among chosen sets, that do not exist in any other set. This option is the closest to a Venn diagram.
                                                    \nThe Distinct elements per file shows the distinct elements of each input list.
                                                    \nThe Union option appends the unique elements among chosen sets and creates all possible combinations.
                                                    ", placement = "left"
                                                            )
                                                        )
                                               ),
                                               upsetjsOutput("upsetjs"),
                                               br(), br(),br(),
                                               fluidRow(
                                                 column(2, textOutput("Hovered_Set")),
                                                 column(2, textOutput("hovered")),
                                                 column(8, textOutput("hoveredElements"))
                                               ),
                                      ),
                                      tabPanel("View Data",
                                               br(),
                                               selectInput("selectView", "Select file to view", choices = NULL, width = "25%"),
                                               DT::dataTableOutput("contents")
                                      )
                          ) # tabsetPanel End
                  ), # tabItem "file_handler" End
                  ### gPROFILER ####
                  tabItem("gProfiler", 
                          h3("Functional Enrichment Analysis: gProfiler"),
                          br(),
                          fluidRow(
                            column(4,
                                   selectInput("selectEnrichFile", "Select file for analysis", width = "65%",
                                               choices = NULL),
                                   selectizeInput("organism", label = " Select organism:",choices = organismsFromFile$print_name, multiple = F,
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
                                   selectInput("gconvertTargetGprofiler","Select ID type for output:", selected = "USERINPUT",
                                               choices = c("User Input"="USERINPUT", "ChEMBL"="CHEMBL", "Entrez Gene Name" = "ENTREZGENE", "Entrez Gene Accession" = "ENTREZGENE_ACC",
                                                           "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME", "UniProt Accession" = "UNIPROT_GN_ACC",
                                                           "UniProt Gene Name" = "UNIPROT_GN", "EMBL Accession" = "EMBL", "ENSEMBL Protein ID" = "ENSP",
                                                           "ENSEMBL Gene ID" = "ENSG", "ENSEMBL Transcript ID" = "ENST", "UniProt Archive" ="UNIPARC",
                                                           "WIKIGENE ID" = " WIKIGENE", "RefSeq mRNA" = "REFSEQ_MRNA", "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
                                                           "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC", "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC")
                                   )
                            ),
                            column(4,
                                   selectInput("threshold","Significance threshold:", choices = c("g:SCS threshold"="gSCS", "Benjamini-Hochberg FDR"="fdr","Bonferroni correction"="bonferroni"), selected = ("g:SCS threshold"="gSCS")),
                                   selectInput("user_pvalue", "P-value correction cut-off:", choices = c(0.05, 0.01)))
                          ),
                          actionButton("gprofiler", "Run analysis",icon("paper-plane"), class = "btn-submit"),
                          br(),
                          tags$hr(),
                          uiOutput("gprofParameters"),
                          uiOutput("notconvert"),
                          uiOutput("nothit"),
                          tabsetPanel(
                            tabPanel("Results", icon = icon("table"),
                                     br(),
                                     tabsetPanel(id ="sources_panel",
                                                 tabPanel("ALL", br(), DT::dataTableOutput("table_all")),
                                                 tabPanel("GO:MF", br(), DT::dataTableOutput("table_gomf")),
                                                 tabPanel("GO:CC", br(), DT::dataTableOutput("table_gocc")),
                                                 tabPanel("GO:BP", br(), DT::dataTableOutput("table_gobp")),
                                                 tabPanel("KEGG", br(), DT::dataTableOutput("table_kegg")),
                                                 tabPanel("REAC", br(), DT::dataTableOutput("table_reac")),
                                                 tabPanel("WP", br(), DT::dataTableOutput("table_wp")),
                                                 tabPanel("TF", br(), DT::dataTableOutput("table_tf")),
                                                 tabPanel("MIRNA", br(), DT::dataTableOutput("table_mirna")),
                                                 tabPanel("CORUM", br(), DT::dataTableOutput("table_corum")),
                                                 tabPanel("HPA", br(), DT::dataTableOutput("table_hpa")),
                                                 tabPanel("HP", br(), DT::dataTableOutput("table_hp"))
                                     )
                            ),#tabpanel results end
                            tabPanel("Plots", icon=icon("chart-bar"),
                                     tabsetPanel(
                                       tabPanel("Manhattan Plot",
                                                br(),br(),
                                                plotlyOutput("manhattan", width = "100%", inline = FALSE), 
                                                br(),
                                                DT::dataTableOutput("manhattan_table")
                                       ),
                                       tabPanel("Scatter Plot",
                                                br(),br(),
                                                fluidRow(
                                                  column(6, selectInput("scatterSelect", "Select datasource", choices = NULL)),
                                                  column(6, sliderInput("sliderScatter", "Choose a number of results to view:", min = 1, max = 10, value = 10, step = 1))
                                                ),
                                                br(),
                                                tags$hr(),
                                                uiOutput("scatterPlot"),
                                                br(), 
                                                DT::dataTableOutput("scatter_table")
                                       ),
                                       tabPanel("Barchart",
                                                br(),
                                                br(),
                                                fluidRow(
                                                  column(4, 
                                                         pickerInput("barSelect2", "Select datasources", 
                                                                     choices = NULL,
                                                                     options = list('actions-box' = TRUE), multiple = TRUE
                                                         )
                                                  ),
                                                  column(4, radioButtons("barplotMode", "Select barplot", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                  column(4, sliderInput("sliderBarplot", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                         br())
                                                ),
                                                br(),
                                                tags$hr(),
                                                uiOutput( "barplot"),
                                                br(),
                                                htmlOutput("bar_legend_gprof"),
                                                br(),
                                                DT::dataTableOutput("barplot_table")
                                       ),
                                       tabPanel("Heatmap",
                                                br(),br(),
                                                tabsetPanel(
                                                  tabPanel("Heatmap: Functions Vs Genes",
                                                           br(),
                                                           fluidRow(
                                                             column(12, selectInput("heatmapSelect", "Select datasource", choices = NULL))
                                                           ),
                                                           fluidRow(
                                                             column(4, radioButtons("heatmapMode", "Select type of Heatmap", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                                             column(4, radioButtons("heatmapAxis", "Reverse Axis", choices = c( "Functions-Genes","Genes-Functions"), inline = TRUE)),
                                                             column(4, sliderInput("sliderHeatmap", "Choose number of results to view:", min = 2, max = 10, value = 10, step = 1)),
                                                           ),
                                                           br(),tags$hr(),
                                                           uiOutput("heatmapPlot"),
                                                           br(),
                                                           DT::dataTableOutput("heatmap_table"),
                                                           br()
                                                  ),
                                                  tabPanel("Heatmap: Functions Vs Functions",
                                                           br(),
                                                           fluidRow(
                                                             column(4, selectInput("heatmapSelect2", "Select datasource", choices = NULL)),
                                                             column(4, radioButtons("heatmapMode2", "Select type of Heatmap", choices = c("Enrichment Score", "-log10Pvalue"), inline = TRUE)),
                                                             column(4, sliderInput("sliderHeatmap2", "Choose number of results to view:", min = 2, max = 10, value = 10, step = 1)),
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           uiOutput("heatmapPlot2"),
                                                           br(),
                                                           DT::dataTableOutput("heatmap_table2"),
                                                           br()
                                                  ) 
                                                )
                                       ),
                                       tabPanel("Network",
                                                br(),
                                                br(),
                                                tabsetPanel(
                                                  tabPanel("Network: Functions Vs Genes",
                                                           br(),
                                                           fluidRow(
                                                             column(4, selectInput("networkSelect", "Select datasource", choices = NULL)),
                                                             column(4, radioButtons("networkMode", "Select network", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                             column(4, sliderInput("sliderNetwork", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                                    br())
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           visNetworkOutput("network", height = "1000px"),
                                                           br(),
                                                           DT::dataTableOutput("network_table"),
                                                           br()
                                                  ),
                                                  tabPanel("Network: Functions Vs Functions",
                                                           br(),
                                                           fluidRow(
                                                             column(12, selectInput("networkSelect2", "Select datasource", choices = NULL))
                                                           ),
                                                           fluidRow(
                                                             column(4, sliderInput("sliderThreshold", "Similarity Score cut-off (%):", min = 0, max = 100, value = 30, step = 1)),
                                                             column(4, radioButtons("networkMode2", "Select network", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                             column(4, sliderInput("sliderNetwork2", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                                    br())
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           visNetworkOutput("network2", height = "1000px"),
                                                           br(),
                                                           DT::dataTableOutput("network_table2"),
                                                           br()
                                                  ),
                                                  tabPanel("Network: Gene Vs Gene",
                                                           br(),
                                                           fluidRow(
                                                             column(12, selectInput("networkSelect3", "Select datasource", choices = NULL))
                                                           ),
                                                           fluidRow(
                                                             column(4, sliderInput("sliderThreshold3", "Number of common Functions:", min = 0, max = 100, value = 0, step = 1)),
                                                             column(4, radioButtons("networkMode3", "Select network", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                             column(4, sliderInput("sliderNetwork3", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                                    br())
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           visNetworkOutput("network3", height = "1000px"),
                                                           br(),
                                                           DT::dataTableOutput("network_table3"),
                                                           br()
                                                  )
                                                )
                                       )     
                                     )
                            )#tabpanel plots end
                          )#tabsetpanel end
                  ), #tabItem gProfiler End
                  ### aGo tool ####              
                  tabItem("aGOtool",
                          h3("Functional Enrichment Analysis: aGoTool"),
                          br(),
                          fluidRow(
                            column(4, 
                                   selectInput("aGOtoolSelect", "Select file for analysis", choices = NULL,width = "65%"),
                                   selectizeInput("aGOtoolOrganism", label = " Select organism:",choices = organismsFromFile$print_name, multiple = F,
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
                                                  column(4, radioButtons("aGoBarplotMode", "Select barplot", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
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
                                                             column(4, selectInput("aGoNetworkSelect", "Select datasource", choices = NULL)),
                                                             column(4, radioButtons("aGoNetworkMode", "Select network", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                             column(4, sliderInput("aGoSliderNetwork", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                                    br())
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           visNetworkOutput("aGoNetwork", height = "1000px"),
                                                           br(),
                                                           DT::dataTableOutput("aGoNetwork_table"),
                                                           br()
                                                  ), # tabPanel"Network: Functions Vs Genes" end
                                                  tabPanel("Network: Functions Vs Functions",
                                                           br(),
                                                           fluidRow(
                                                             column(12, selectInput("aGoNetworkSelect2", "Select datasource", choices = NULL))
                                                           ),
                                                           fluidRow(
                                                             column(4, sliderInput("aGoSliderThreshold", "Similarity Score cut-off (%):", min = 0, max = 100, value = 30, step = 1)),
                                                             column(4, radioButtons("aGoNetworkMode2", "Select network", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                             column(4, sliderInput("aGoSliderNetwork2", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                                    br())
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           visNetworkOutput("aGoNetwork2", height = "1000px"),
                                                           br(),
                                                           DT::dataTableOutput("aGoNetwork_table2"),
                                                           br()
                                                  ),# tabPanel("Network: Functions Vs Functions") end
                                                  tabPanel("Network: Gene Vs Gene",
                                                           br(),
                                                           fluidRow(
                                                             column(12, selectInput("aGoNetworkSelect3", "Select datasource", choices = NULL))
                                                           ),
                                                           fluidRow(
                                                             column(4, sliderInput("aGoSliderThreshold3", "Number of common Functions:", min = 0, max = 100, value = 0, step = 1)),
                                                             column(4, radioButtons("aGoNetworkMode3", "Select network", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                             column(4, sliderInput("aGoSliderNetwork3", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                                    br())
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           visNetworkOutput("aGoNetwork3", height = "1000px"),
                                                           br(),
                                                           DT::dataTableOutput("aGoNetwork_table3"),
                                                           br()
                                                  )# tabPanel("Network: Gene Vs Gene") end
                                                ) #tabsetPanel network end
                                                
                                       ) # tabPanel"Network" end
                                     ) #tabsetPanel plots end
                                     
                            ) #tabPanel plots end
                          ) # tabsetPanel
                  ),#tabItem aGoTool end
                  
                  ### LITERATURE SEARCH####
                  tabItem("literature_search",
                          h3("Literature Enrichment"),
                          br(),
                          fluidRow(
                            column(4, selectInput("literatureSelect", "Select file for analysis", choices = NULL,width = "75%")),
                            
                            column(4, 
                                   selectizeInput("literatureOrganism", label = " Select organism:",choices = organismsFromFile$print_name, multiple = F,
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
                                                  column(6, radioButtons("literatureBarplotMode", "Select barplot", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
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
                                                             column(6, radioButtons("literatureNetworkMode", "Select network", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                             column(6, sliderInput("literatureSliderNetwork", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                                    br())
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           visNetworkOutput("literatureNetwork", height = "1000px"),
                                                           br(),
                                                           DT::dataTableOutput("literatureNetwork_table"),
                                                           br()
                                                  ), # tabPanel"Network: Functions Vs Genes" end
                                                  
                                                  tabPanel("Network: Publications Vs Publications",
                                                           br(),
                                                           fluidRow(
                                                             column(4, sliderInput("literatureSliderThreshold", "Similarity Score cut-off (%):", min = 0, max = 100, value = 30, step = 1)),
                                                             column(4, radioButtons("literatureNetworkMode2", "Select network", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                             column(4, sliderInput("literatureSliderNetwork2", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                                    br())
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           visNetworkOutput("literatureNetwork2", height = "1000px"),
                                                           br(),
                                                           DT::dataTableOutput("literatureNetwork_table2"),
                                                           br()
                                                  ),# tabPanel("Network: Functions Vs Functions") end
                                                  tabPanel("Network: Gene Vs Gene",
                                                           br(),
                                                           fluidRow(
                                                             column(4, sliderInput("literatureSliderThreshold3", "Number of common Publications:", min = 0, max = 100, value = 0, step = 1)),
                                                             column(4, radioButtons("literatureNetworkMode3", "Select network", choices = c("Enrichment Score", "-log10Pavlue"), inline = TRUE)),
                                                             column(4, sliderInput("literatureSliderNetwork3", "Choose number of results to view:", min = 1, max = 10, value = 10, step = 1),
                                                                    br())
                                                           ),
                                                           br(),
                                                           tags$hr(),
                                                           visNetworkOutput("literatureNetwork3", height = "1000px"),
                                                           br(),
                                                           DT::dataTableOutput("literatureNetwork_table3"),
                                                           br()
                                                  )# tabPanel("Network: Functions Vs Functions") end
                                                ) #tabsetPanel network end
                                       ) # tabPanel"Network" end
                                     ) #tabsetPanel plots end
                            ) #tabPanel plots end
                          ) # tabsetPanel
                  ),#tabItem Literature END
                  ### PROTEIN-PROTEIN INTERACTION NETWORK ####
                  tabItem("string_network",
                          tags$head(tags$script(src = 'https://string-db.org/javascript/combined_embedded_network_v2.0.2.js')),
                          h3("Protein-Protein Interaction Network"),
                          br(),
                          fluidRow(width=12,
                                   column(4, selectInput("STRINGnetworkSelect", "Select file for analysis", choices = NULL),
                                   ),
                                   column(4,
                                          selectizeInput("STRINGnetworkOrganism", label = " Select organism:",choices = organismsFromFile$print_name, multiple = F,
                                                         selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",width = "65%",
                                                         options = list(placeholder = 'Select an option or start typing...')
                                          ),
                                          br(),
                                          radioButtons(
                                            inputId = 'STRINGnetworkType', 
                                            label = 'Select interaction type:', 
                                            choiceNames = list ("Full network","Physical subnetwork"),
                                            choiceValues = list('functional', 'physical')
                                          )%>%
                                            shinyInput_label_embed(
                                              shiny_iconlink() %>%
                                                bs_embed_popover(
                                                  title = "Full network: edges indicate both functional and physical associations.\nPhysical subnetwork: edges indicate existence of a physical complex.", placement = "left"
                                                )
                                            )
                                   ),
                                   column(4,
                                          radioButtons(
                                            inputId = 'STRINGnetworkEdges', 
                                            label = 'Select meaning of network edges:', 
                                            choiceNames = list ("Evidence","Confidence"),
                                            choiceValues = list('evidence', 'confidence')
                                          )%>%
                                            shinyInput_label_embed(
                                              shiny_iconlink() %>%
                                                bs_embed_popover(
                                                  title = "Evidence: edge colors indicate the type of interaction evidence.\nConfidence: edge thickness indicates the strength of data support based on the interaction score", placement = "left"
                                                )
                                            ),
                                          br(),
                                          pickerInput(
                                            inputId = 'STRINGnetworkScore', 
                                            label = 'Select interaction score cut-off:', 
                                            choices = list (`highest confidence (0.900)` = 900,
                                                            `high confidence (0.700)` = 700,
                                                            `medium confidence (0.400)` = 400,
                                                            `low confidence (0.150)` = 150),
                                            selected = list(`medium confidence (0.400)` = 400)
                                          )
                                   )
                          ),
                          actionButton("network", "Run analysis",icon("paper-plane"), class = "btn-submit"),
                          br(),
                          tags$hr(),
                          br(),
                          verbatimTextOutput("networkParameters"),
                          br(),
                          htmlOutput("string_legend"),
                          br(),br(),
                          htmlOutput("string_export_buttons"),
                          br(),
                          htmlOutput("string_viewer")
                          # tabsetPanel
                  ),#tabItem NETWORK END
                  ####CONVERSION TAB    ####       
                  tabItem("gconvert",
                          h3("Gene ID Conversion"),
                          br(),
                          fluidRow(
                            column(4,
                                   selectInput("gconvert_select", "Select file to convert", choices=NULL),
                                   actionButton("gconvert_button", "Convert IDs", class = "btn-submit"),
                                   br()),
                            column(8,
                                   selectizeInput("gconvert_organism", label = " Select input organism:",choices = organismsFromFile$print_name, multiple = F,
                                                  selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]", width = "35%",
                                                  options = list(placeholder = 'Select an option or start typing...')),
                                   selectInput("gconvert_target","Target namespace", selected = "ENTREZGENE", width = "35%",
                                               choices = c("ChEMBL"="CHEMBL", "Entrez Gene Name" = "ENTREZGENE", "Entrez Gene Accession" = "ENTREZGENE_ACC",
                                                           "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME", "UniProt Accession" = "UNIPROT_GN_ACC",
                                                           "UniProt Gene Name" = "UNIPROT_GN", "EMBL Accession" = "EMBL", "ENSEMBL Protein ID" = "ENSP",
                                                           "ENSEMBL Gene ID" = "ENSG", "ENSEMBL Transcript ID" = "ENST", "UniProt Archive" ="UNIPARC",
                                                           "WIKIGENE ID" = " WIKIGENE", "RefSeq mRNA" = "REFSEQ_MRNA", "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
                                                           "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC", "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC")
                                   )
                            )
                          ),
                          tags$hr(),
                          DT::dataTableOutput("gconvert_table")
                  ),
                  tabItem("gorth",
                          h3("Orthology Search"),
                          br(),
                          fluidRow(
                            column(4, selectInput("gorth_select", "Select file for orthology search", choices = NULL),
                                   actionButton("gorth_button", "Orthology search", class = "btn-submit")),
                            column(8, 
                                   selectizeInput("gorth_organism", label = " Select input organism:",choices = organismsFromFile$print_name, multiple = F,
                                                  selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",width = "35%",
                                                  options = list(placeholder = 'Select an option or start typing...')),
                                   
                                   selectizeInput("gorth_target", label = " Select target organism:",choices = organismsFromFile$print_name, multiple = F,
                                                  selected = "Mus musculus (Mouse) [NCBI Tax. ID: 10090]",width = "35%",
                                                  options = list(placeholder = 'Select an option or start typing...')))
                          ),
                          tags$hr(),
                          DT::dataTableOutput("gorth_table")
                  ),
                  tabItem("about",
                          fluidRow(column(12,
                                          HTML("<h2> About <i>Flame</i> </h2>
                                          <hr style=height:2px;border-width:0;color:#ffa600;background-color:#ffa600;>
                                          <p>Flame is actively developed and maintained by the Bioinformatics and Integrative Biology Lab</p>
                                          
                                          <h3> Developers </h3>
                                          <ul>
                                              <li> Evangelos Karatzas, karatzas[at]fleming[dot]gr 
                                              <li> Foteini Thanati, fotinithanati[at]gmail[dot]com
                                            	<li> Fotis A. Baltoumas, baltoumas[at]fleming[dot]gr 
                                          	  <li> Georgios A. Pavlopoulos, pavlopoulos[at]fleming[dot]gr 
                                          </ul>
                                          <h3>Code Availability</h3>
                                          <p>The source code for Flame can be found in <a href='https://github.com/PavlopoulosLab/Flame/' target='_blank'>GitHub</a> repository.</p>
                                          <h3> Related Software </h3>
                                          <ul>
                                              <li> <a href='https://biit.cs.ut.ee/gprofiler/gost' target='_blank'>gProfiler: Functional Enrichment Analysis</a>
                                              <li> <a href='https://string-db.org/' target='_blank'>STRING-DB: A database of protein-protein interactions, offering network visualization options</a>
                                              <li> <a href='https://agotool.org/' target='_blank'>aGOtool: Protein-centric enrichment analysis and literature search with abundance-bias correction</a>
                                              <li> <a href='http://bib.fleming.gr:3838/OnTheFly/' target='_blank'>OnTheFly<sup>2.0</sup>: Extract Biological Information from Documents</a>
                                          </ul>
                                          <h3> Cite <i>Flame</i> </h3>
                                          <p style='font-size:15px'>If you find Flame useful in your work please cite:</p>
                                          
                                         &bull; F. Thanati, E. Karatzas, F. Baltoumas, D. J. Stravopodis, A. G. Eliopoulos, and G. Pavlopoulos, <b>“FLAME: a web tool for functional and literature enrichment analysis 
                                          of multiple gene lists,”</b> Biology, 2021, <a href='https://www.mdpi.com/2079-7737/10/7/665' target='_blank'>doi: 10.3390/biology10070665</a>

                                                ")
                          )
                          )
                  ),
                  ##### HELP ####
                  tabItem("help",
                          fluidRow(
                            column(12, 
                                   tabsetPanel(
                                     tabPanel(h5("File Input"),
                                              br(),
                                              fluidRow(
                                                column(12,box( title = "Upload File(s)", collapsible = TRUE, collapsed = F,
                                                               solidHeader = TRUE, status = "primary", width = NULL, 
                                                               fileInputTab)),
                                                column(12,box( title = "Upset plot", collapsible = TRUE, collapsed = TRUE,
                                                               solidHeader = TRUE, status = "primary",  width = NULL,
                                                               upsetTab, splitLayout())),
                                                column(12,box(title ="View Data" , collapsible = TRUE, collapsed = TRUE,
                                                              solidHeader = TRUE, status = "primary", width = NULL,viewData)
                                                )
                                              )
                                     ),#tabpanel 1 end
                                     tabPanel(h5("Functional Enrichment Analysis"),
                                              br(),
                                              fluidRow(
                                                column(12,box( title = "Functional Enrichment Parameters", collapsible = TRUE, collapsed = F,
                                                               solidHeader = TRUE, status = "primary", width = NULL,gprofInput)),
                                                column(12,box(title = "Functional Enrichment Results", collapsible = TRUE, collapsed = TRUE,
                                                              solidHeader = TRUE, status = "primary", width = NULL, gprofOutput,
                                                ))
                                              )
                                     ),#tabpanel 2 end
                                     tabPanel(h5("aGotool Analysis"),
                                              br(),
                                              fluidRow(
                                                column(12,box( title = "aGotool Analysis Parameters", collapsible = TRUE, collapsed = F,
                                                               solidHeader = TRUE, status = "primary", width = NULL, aGoInput)),
                                                column(12,box(title = "aGotool Analysis Results", collapsible = TRUE, collapsed = TRUE,
                                                              solidHeader = TRUE, status = "primary",width = NULL,aGoOutput))
                                              )
                                     ),#tabpanel 3 end
                                     tabPanel(h5("Literature Search"),
                                              br(),
                                              fluidRow(
                                                column(12,box( title = "Literature Search Parameters", collapsible = TRUE, width = NULL,
                                                               collapsed = F, status = "primary",solidHeader = TRUE, literatureInput)),
                                                column(12,box(title = "Literature Search Results", collapsible = TRUE, collapsed = TRUE,
                                                              solidHeader = TRUE, status = "primary", width = NULL, literatureOutput))
                                              )
                                     ),#tabpanel 4 end
                                     tabPanel(h5("Plots"),
                                              br(),
                                              tabsetPanel(
                                                tabPanel(h5("Manhattan Plot"),
                                                         br(),
                                                         fluidRow(
                                                           column(12,box(manPlot, collapsible = TRUE, collapsed = F, status = "primary", width = NULL)))),
                                                tabPanel(h5("Scatter Plot"), br(),
                                                         fluidRow(
                                                           column(12,box(scatterPlot, collapsible = TRUE, collapsed = F, status = "primary", width = NULL)))),
                                                tabPanel(h5("Barchart"), br(),
                                                         fluidRow(
                                                           column(12,box(barPlot, collapsible = TRUE, collapsed = F, status = "primary", width = NULL)))),
                                                tabPanel(h5("Heatmap"), br(),
                                                         fluidRow(
                                                           column(12,box(heatMapHelp, collapsible = TRUE, collapsed = F, status = "primary", width = NULL)))),
                                                tabPanel(h5("Network"), br(),
                                                         fluidRow(
                                                           column(12, box(networkHelp, collapsible = TRUE, collapsed = F, status = "primary", width = NULL))))
                                              ) #tabsetpanel 5 end 
                                     ), #tabPanel 6 end
                                     tabPanel(h5("Network Analysis"),
                                              br(),
                                              fluidRow(
                                                column(12,box( title = "Network Analysis", collapsible = TRUE, collapsed = F,
                                                               solidHeader = TRUE, status = "primary", width = NULL, proteinNetwork)),
                                              )
                                     ),#tabpanel 7 end
                                     tabPanel(h5("Conversion"),
                                              br(),
                                              fluidRow(
                                                column(12,box( title = "Gene ID Conversion", collapsible = TRUE, collapsed = F,
                                                               solidHeader = TRUE, status = "primary", width = NULL, gconvertTab)),
                                                column(12,box(title = "Orthology Search", collapsible = TRUE, collapsed = TRUE,
                                                              solidHeader = TRUE, status = "primary", width = NULL,gorthTab))
                                              )
                                     ),#tabpanel 8 end
                                     tabPanel(h5("API"),
                                              br(),
                                              fluidRow(
                                                column(12,box( title = "POST Request", collapsible = TRUE, collapsed = F,
                                                               solidHeader = TRUE, status = "primary", width = NULL, POSTRequestHelp)),
                                                column(12,box( title = "GET Request", collapsible = TRUE, collapsed = F,
                                                               solidHeader = TRUE, status = "primary", width = NULL, GETRequestHelp))
                                              )
                                     )#tabpanel 9 end
                                   )
                            )
                          )#fluidRow end 
                  )# tabItem help end
                  
                ), # tabItems End
                fluidRow(column(12,
                                HTML("<footer>
                                     © 2021 <a href=\"https://sites.google.com/site/pavlopoulossite\" target=\"_blank\">Bioinformatics and Integrative Biology Lab</a> | 
                                     <a href=\"https://www.fleming.gr\" target=\"_blank\">Biomedical Sciences Research Center \"Alexander Fleming\"</a>
                                     </footer>")
                ))
              ) # dashboardBody End
              
) # dashboardPage End
