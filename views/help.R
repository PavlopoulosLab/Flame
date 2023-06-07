generateHelpPage <- function() {
  fluidRow(
    column(
      12, 
      tabsetPanel(
        generateHelpFileInputTabPanel(),
        generteHelpEnrichmentAnalysisTabPanel(),
        generateHelpPlotsTabPanel(),
        generateHelpNetworkAnalysisTabPanel(),
        generateHelpConversionTabPanel(),
        generateHelpAPITabPanel()
      )
    )
  )
}

generateHelpFileInputTabPanel <- function() {
  tabPanel(
    h5("Input Options"),
    tags$br(),
    fluidRow(
      column(
        12,
        box(
          title = "Upload", collapsible = T, collapsed = F,
          solidHeader = T, status = "primary", width = NULL,
          generateHelpUpload()
        )
      ),
      column(
        12,
        box(
          title = "SNPs", collapsible = T, collapsed = T,
          solidHeader = T, status = "primary", width = NULL,
          generateHelpSNP()
        )
      ),
      column(
        12,
        box(
          title = "Text-mining", collapsible = T, collapsed = T,
          solidHeader = T, status = "primary", width = NULL,
          generateHelpTextMining()
        )
      ),
      column(
        12,
        box(
          title = "Volcano", collapsible = T, collapsed = T,
          solidHeader = T, status = "primary", width = NULL,
          generateHelpVolcano()
        )
      ),
      column(
        12,
        box(
          title ="View" , collapsible = T, collapsed = T,
          solidHeader = T, status = "primary", width = NULL,
          generateHelpFileInputViewData()
        )
      ),
      column(
        12,
        box(
          title = "UpSet Plot", collapsible = T, collapsed = T,
          solidHeader = T, status = "primary",  width = NULL,
          generateHelpFileInputUpsetTab()
        )
      )
    )
  )
}

generateHelpUpload <- function() {
  HTML('
   <p>
   <b>Flame</b> offers various input options including: (a) Simple gene/protein lists,
   (b) Variants, (c) Text and (d) Gene expression files
   </p>
      <div>
      <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Upload.png"
           style = "width: 600px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "sideBySide">
        <p style = "text-align: justify">
         You can import your gene lists either by a simple <b>(1)</b> paste or via <b>(2)</b> upload.
         Acceptable file formats: Flat text (.txt, .tsv, .csv).
         The size of each file cannot exceed 1 MB.
         A maximum of 10 files can be uploaded/created in the text area for each session.

        <b>(3)</b> By clicking “Add to files” the list is appended in the box with the current lists. 
         You can find a dedicated “Example” button next to the “Add to files” button.

        <b>(4)</b> You can change the name of your lists by choosing a list and then clicking the “rename” button. 
        Similarly you can delete any unwanted lists by clicking the “Remove” button

         Notably, Flame does not allow lists with the same name; thus providing options 
         for renaming and deleting.

        </p>
      </div>
   </div>

  
    ')
}

generateHelpSNP <- function() {
  HTML('
   <p>
   
   <b>Variants</b>
   Flame is capable of parsing and annotating lists of single nucleotide
   polymorphisms (SNPs), through the “SNP” input option.
   
   </p>
   
      <div>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/SNP.png"
           style = "width: 1200px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "wholeRow">
        <p style = "text-align: justify">
       You can import your list, a whitespace or comma-delimited list of SNP rs-codes in the dbSNP format, 
       either by (1) a simple paste or (2) by uploading your file.
        By clicking on the “Submit” button, the input ids are mapped to their corresponding genes 
        and associated Ensembl identifiers through the g:SNPense functionality of g:Profiler and are then 
        searched against dbSNP to retrieve additional metadata.
        (3) All these data, including the chromosome number, strand, genomic coordinates and 
        the variant’s reported effects, are presented in a table.
        (4)The annotated SNPs can be downloaded, while (5) their associated genes can be added to
        the list of Flame inputs, using either their Entrez gene name or their Ensembl ids. 
        Since dbSNP has dropped support for non-human variants in recent versions, this functionality
        is currently available only for Homo sapiens (human).

        </p>
      </div>
   </div>

  
    ')
}

generateHelpTextMining <- function() {
  HTML('
     <div>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/TextMining.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "wholeRow">
        <p style = "text-align: justify">
     In this version, users can input genes/proteins derived from text-mining by providing:
     <b>(1) </b> free text by writing in the white box and <b>(2)</b> selecting the organism of preference.
     Flame will report the <b>(3)</b> identified genes/proteins in an interactive and easy-to-filter table
     <b>(6)</b> as well as <b>(4)</b> an annotated text with the identified terms highlighted. In mouse-hover <b>(5)</b>, 
     a popup window will appear, providing information about the entity as well as direct links to the 
     STRING and Ensembl databases. The user can select from the table the genes/proteins of interest by clicking 
     on each one <b>(7)</b> or on the “Select all” button <b>(8)</b>. The annotated terms can be downloaded or <b>(9) </b>added to the 
     list of Flame inputs, using their associated Ensembl ids. This functionality is supported for all currently available 
     organisms in Flame (~14,000 taxa).


        </p>
      </div>
   </div>

  
    ')
}

generateHelpVolcano <- function() {
  HTML('
   <p>
   
   <b>Variants</b>
   Flame is capable of parsing and annotating lists of single nucleotide
   polymorphisms (SNPs), through the “SNP” input option.
   
   </p>
   
      <div>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/Volcano.png"
           style = "width: 1200px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "wholeRow">
        <p style = "text-align: justify">
       Flame offers the ability to <b>(1)</b> upload expression data in tab delimited or comma separated format, 
       or run a pre-set example (format: gene name - log fold change - statistical significance).
      The respective table is then loaded next to the file input (mandatory column names: symbol, pvalue, logFC).
      In the View Tab, a Volcano plot will be generated automatically.
      You can adjust both the p-value <b>(2)</b> and FC thresholds <b>(3)</b> (red lines on the plot) to 
      mark your under- and over-expressed genes of preference.
      You may <b>(4)</b> apply a selection lasso or rectangle to mark gene sets <b>(5)</b> 
      of interest directly on the Volcano plot, and append them to the Flame lists which can then be further processed and enriched.


        </p>
      </div>
   </div>

  
    ')
}


generateHelpFileInputViewData <- function(){
  HTML('  <div>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/viewData.png"
           style = "width: 1200px; border: 1px solid black;"/> 
        </div>
        </div>
         <div class = "wholeRow">
        <p style = "text-align: justify">
        All the bioentities of the uploaded files and the files created by the Upset plot are displayed in a table in the View Data Tab. The files can be download in several formats.
       </p>
      </div>
   </div>
     ')
}


generateHelpFileInputUpsetTab <- function(){
  HTML('  <div>
  
      <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Upsetplot1.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "sideBySide">
        <p style = "text-align: justify">
           The UpSet plot is a sophisticated alternative of a Venn-diagram and
           is prefered for many sets (>5) where a Venn diagram becomes incomprehensive. 
           UpSet plots help in understanding the relationships among different sets by visualizing
           their intersections as a matrix. 
           To this end, each column corresponds to a set whereas each row to one segment in a Venn diagram.
           In its current version, Flame supports four UpSet plot modes:
           <ul style="font-size:18px">
              <li>intersection</li>
              <li>distinct combinations</li>
              <li>union</li>
              
          </ul>
          </p>
          
          <p>

          Files can be selected by clicking the checkboxes next to their names in the checkbox file list. 
          You should select <b> at least 2 files </b> to create the plot. After the selection of the files,
          click the <b> Generate </b> button.
          </p>
          
      </div>
   </div>
   
   <hr />
   <div>
     <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Upsetplot2.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "sideBySide">
        <p style = "text-align: justify">
           An UpSet plot consists of two axes and a connected-dot matrix. 
           The vertical rectangles represent the number of elements participating in each list combination.
           The connected-dots matrix indicates which combination of lists corresponds to which vertical rectangle.
           Finally, the horizontal bars (Set Size) denote the participation of hovered objects (from the vertical rectangles) in the respective lists.
           Flame supports four UpSet plot modes:
           <ul  style="font-size:18px">
            <li><b>Intersections:</b> creates all file combinations as long as they share at least one element, 
            allowing an element to participate in more than one combination.</li>
            <li><b>Distinct Combinations:</b> creates file combinations only for distinct elements that 
            do not participate in other lists.</li>
            <li><b>Unions:</b> constructs all available file combinations, 
            showing their total unique elements.</li>
          </ul>
          </p>
  
      </div>
   </div>
   
   <hr />
   <div>
    <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Upsetplot3.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "sideBySide">
        <p style = "text-align: justify">
        On mouse-hover over <b>(1)</b>  each UpSet plot element combination (vertical rectangles), 
        Flame presents the <b>(2)</b> respective genes in a table, while on a mouse click the user can append
        <b>(3)</b> the selected list of files with a new file containing the selected genes and process it separately.
      </div>
      </div>
     ')
}


generteHelpEnrichmentAnalysisTabPanel <- function(){
  tabPanel(
    h5("Functional/Literature Enrichment Analysis"),
    tags$br(),
    fluidRow(
      column(
        12,
        box(
          title = "Functional Enrichment Parameters",
          collapsible = T, collapsed = F, solidHeader = T,
          status = "primary", width = NULL,
          generateHelpFunctionalEnrichmentControlPanel()
        )
      ),
      column(
        12,
        box(
          title = "Functional Enrichment Results", collapsible = T, collapsed = T,
          solidHeader = T, status = "primary", width = NULL,
          generateHelpEncrichmentResults()
          
        )
      )
      ,
      column(
        12,
        box(
          title = "Literature Enrichment", collapsible = T, collapsed = T,
          solidHeader = T, status = "primary", width = NULL,
          generateHelpLiteratureEncrichment()
        )
      )
    ))
}


generateHelpFunctionalEnrichmentControlPanel <- function() {
  HTML('
  <div>
    <p style = "text-align:justify">
      Flame offers multiple tools for functional enrichment analysis.
      Functional enrichment is the process of identifying implicated functional terms from a given input list 
      of genes or proteins.
    </p>
  </div>
  
  
  <div>
    <div class = "wholeRow">
      <div class = "helpImage_div">
        <img src = "help_images/Enrichment1.png"
        style = "border: 1px solid black; width: 1000px;">
      </div>
    </div>
          
    <div class = "wholeRow">            
      <ol type="1">
      <li><b>Select list:</b> Select one of your input lists.</li> 
      <li><b>Select organism:</b> You select the organism that matches your input query list.
          <b>Flame</b> currently allows enrichment for <b> 14,436 </b>organisms. Flame proposes the 
          most appropriate enrichment tool for your selected organism.
          Default organism is human (Homo sapiens). </li> 
      <li><b>Select</b> if the enrichment analysis is performed against a <b> backround list </b> 
      (submitted by the user) or <b> the whole organism. </b> </li>
      
      <li><b>Select enrichment tool:</b> </li> 
        <ul>
          <li><b> aGotool</b></li>
          <li><b> gProfiler</b></li>
          <li><b> WebGestalt</b></li>
          <li><b> enrichR</b></li>
        </ul>
        You can either select a single tool (default selection) or a combination (up to four).
        
        <li><b>Select datasources:</b> </li> 
        <ul>
          <li> Gene Ontology: Biological Process, Molecular Function and Cellular Component</li>
          <li> Metabolic Pathways: KEGG, Reactome, WikiPathways, PANTHER </li>
          <li> Disease: Disease Ontology, DisGeNet, OMIM, GLAD4U</li>
          <li> Proteins: Interpro, PFAM, UniProt keywords, CORUM  </li>
          <li> Phenotypes: Human Phenotype Ontology (for H. sapiens only!)</li>
          <li> Tissues: Human Protein Atlas (HPA), Brenda Tissue Ontology </li>
          <li> Drugs: DrugBank, GLAD4U</li>
          <li> Regulatory motifs: TRANSFAC, miRTarBas</li>
        </ul>
      
      <li><b>Select namespace conversion:</b> 
            For each enrichment tool, you can select the gene ID type. 
            <ul>
            <li> aGotool: Ensembl IDs </li>
            <li> gProfiler: Ensembl, Entrez, Uniprot, RefSeq, EMBL, ChEMBL, WikiGene, User Input (default selection) </li>
            <li> WebGestalt: Entrez gene accession identifiers (default selection), User Input </li>
            <li> enrichR: Entrez gene names (default selection), User Input </li>
            </ul>
            When more than one tool is selected, the default namespaces are used.
        
     <li><b>Select significance metrics:</b>  
        Statistical significance is a measure of reliability in the result of an analysis.
        You can select the statistical metric for each enrichment tool: </li>
        <ul>
        <li> aGotool: p-value, False DIscovery Rate (FDR) </li>
        <li> gProfiler: g:SCS (adjusted p-value), FDR, Bonferroni </li>
        <li> WebGestalt: Bonferroni & five FDR types (BH, BY, Holm, Hochberg, Hommel)</li>
        <li> enrichR: Adjusted p-value </li>
        </ul>
        
    <li><b> Select significance threshold: </b></li>
    You can select a value of either <b>0.01</b> or <b>0.05</b> (default).
         
            
    <li><b> Select results namespace: </b></li>
    You can select the name type of your results: either <b>original input names</b> (default)
    or <b> converted input names</b>.
    </ol>
    </div>
  </div>
 ')
}

generateHelpEncrichmentResults <- function() {
  HTML(' 
    <div>
      <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Enrichment_Results_1.png"
           style = "width: 600px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "sideBySide">
        <p style = "text-align: justify">
           Here, you can find the results for each enrichment tool selected for the analyis.
           A <i>Combination</i> tab is automatically generated when more than one tool has yielded results.
           <br /><br />
           You can view all query parameters for each executed pipeline (file, organism, datasources, namespace,
           significance metrics and threshold).
        </p>
      </div>
   </div>

   <div>
    <div class = "wholeRow">
      <div class = "helpImage_div">
        <img src = "help_images/Enrichment_Results_2.png" style="
        border: 1px solid black; width: 1200px;">
      </div>
    </div>

    <div class = "wholeRow">
      <br />
      <p>
       The <b>Results</b> table contains the output functionally enriched terms.
       Flame can generate a variety of plots for your results in the <b>Plots</b> tab. 
       Visit the <b>Plots</b> Help Page for more information.
       <br />
       (1) You can view results either in a single table (ALL), or per selected datasource.
       <br />
       (2) You can also copy, print or download your results in various formats such as:
      </p>
       <ul style="font-size:18px !important">
        <li>Excel</li>
        <li>CSV</li>
        <li>PDF</li>
       </ul>
     
     <p>
       (3) Each row contains information regarding:
       </p>
       <ol style="font-size: 18px">
         <li><b>Source</b>: the source database (GO, KEGG etc.) </li>
         <li><b>Term ID: </b> the unique term identifier with a hyperlink that 
         points to the correspoding link for the term.</li>
         <li><b>Function:</b> the name/description of the enriched term.</li>
         <li><b>p-value:</b> hypergeometric p-value after correction for multiple testing.</li>
         <li><b>-log10Pvalue:</b> An alternative format of p-value that is used for scoring and plots.</li>
         <li><b>Term size:</b> number of genes that are annotated to the term.</li>
         <li><b>Query size:</b> number of genes that were included in the query.</li>
         <li><b>Intersection Size:</b> number of genes in the input query 
                 that are annotated in the corresponding term.</li>
         <li><b>Enrichement Score%:</b> Intersection Size / Term size * 100%</li>
       </ol>
     
      <p>
       (4) Each row can be extented using the (+) button showing the <b>positive hits</b>.
        Positive hits is a comma separated list of genes from the query that are annotated 
        in the corresponding term.
        <br />
      </p>
    </div>
  </div>

  <hr />
  
  <div>
  
  <p>
  The <b>Combination</b> panel is automatically generated when more than one enrichment tools have been executed.
  </p>
  
    <div class = "wholeRow">
      <div class = "helpImage_div">
       <img src = "help_images/combination_table.png" style="
       border: 1px solid black; width: 900px;">
      </div>
    </div>
     
    <div class = "wholeRow">
      <p>
       (1) Datasources: You can view the aggregated results of one or more selected datasources. 
       <br /><br />
       (2) You can also copy, print or download your results in various formats such as:
      </p>
      
      <ul style="font-size:18px !important">
        <li>Excel</li>
        <li>CSV</li>
        <li>PDF</li>
      </ul>
     
     <p>
      (3) The Combination table contains information regarding:
     </p>
       <ol style="font-size: 18px">
         <li><b>Source</b>: the source database (GO, KEGG etc.) </li>
         <li><b>Term ID: </b> the unique term identifier with a hyperlink that 
         points to the correspoding link for the term.</li>
         <li><b>Function:</b> the name/description of the enriched term.</li>
         <li><b>Tools:</b> the tool(s) returning the enriched term. </li>
         <li><b>X<sup>2</sup></b> and <b>Comb.P-value:</b> metrics using the Fisher method, 
         as means to evaluate the significance (or lack thereof) of 
         the enriched terms.</li>

         <li><b>Rank:</b> the number of tools returning the enriched term. </li>
      </ol>
      
    </div>
  </div>
  
   <hr />
   
  <div>
  
    <div class = "wholeRow">
      <div class = "helpImage_div">
       <img src = "help_images/Combo_Network.png" style="
       border: 1px solid black; width: 800px;">
      </div>
    </div>
     
    <div class = "wholeRow">
      <p>
      The <b>combination tab</b> in the enrichment analysis results panel now offers 
      the option to create and visualize <b>Function vs Gene interaction networks </b> (“Combo Network” tab) for 
      the combined results, in a similar manner to the networks created for each individual tool. 
      <b>(1)</b>Users can select which tools (aGOtool, g:Profiler, WebGestalt or EnrichR) to include in the analysis,
      as well as <b>(2)</b> the minimun threshold and <b>(3)</b> the layout algorythm. Clicking on <b>(4)</b> "Visualize Network" tab,
      the network with the selected properties will be created.
      Furthermore, users can also view the properties of each gene-function connection interactively, 
      including the combination of tools which have generated it, by <b>(5)</b> hovering over the network edges. 
      The visible network edgelist is also depicted in table format below the network.
      </p>
     </div> 
     </div>
     
     <br />
      
  <div>
  
    <div class = "sideBySide">
      <div class = "helpImage_div">
       <img src = "help_images/Conversion_Table.png" style="
       border: 1px solid black; width: 700px;">
      </div>
    </div>
     
    <div class = "sideBySide">
      <p>
      In the Conversion Table you can find the results of namespace conversion.
      The Input column (1) contains the input genes of the selected list. 
      The Target column (2) contains the respective genes converted in the selected namespace. 
      The Name column (3) contains the most usual gene names for the input
      </p>
     </div> 
     </div>
     
     <br />
      
     <div> 
       <div class = "sideBySide">
      <div class = "helpImage_div">
       <img src = "help_images/No_hits.png" style="
       border: 1px solid black; width: 700px;">
      </div>
    </div>
     <div class = "sideBySide">
      <p>
      No-hit Inputs: Converted genes that did not match any of the output terms.
      </p>
     
    </div>
  </div>
  ')
}

generateHelpLiteratureEncrichment <- function() {
  HTML('
    <div class="col-md-12"> 
      <p style="font-size:18px"; text-align: justify>
        Flame allows users to retrieve scientific articles
        that are related to the genes/proteins provided in the input lists.
        The <b>Literature Enrichment Analysis</b> pipelines is similar to the aforementioned
        <b>Functional Enrichment Analysis</b>. You can select the parameters for your analysis and your results can be 
        generated as interactive tables or plots. In Literature Analysis, article IDs are clickable and link to the
        relevant page in PubMed.
      </p>
    </div>
  ')
}

generateHelpaGotoolAnalysisTabPanel <- function(){
  tabPanel(h5("aGotool Analysis"),
           tags$br(),
           fluidRow(
             column(12,box( title = "aGotool Analysis Parameters", collapsible = T, collapsed = F,
                            solidHeader = T, status = "primary", width = NULL, generateHelpaGotoolInput())),
             column(12,box(title = "aGotool Analysis Results", collapsible = T, collapsed = T,
                           solidHeader = T, status = "primary",width = NULL,generateHelpaGotoolOutput()))
           )
  )
}

generateHelpaGotoolInput <- function(){
  HTML('
                               <p>
                               <h3>1. Select the Parameters</h3>
                      <ol type="1">
                                 <li><b>Select list.</b> </li> 
                                 <li><b>Select organism:</b> select organism that matches your input query gene list. A choice among 197 species is given. Default organism is human (Homo sapiens). </li> 
                                 <li><b>Select datasources:</b> </li> 
                                    <ul>
                                      <li> UniProt keywords</li>
                                      <li> Disease Ontology</li>
                                      <li> Interpro</li>
                                      <li> PFAM</li>
                                    </ul>
                                 <li><b>Select ID type for output:</b> Define the ID type that will be used in the analysis, as well as in the output.
                                Results can be reported as Entrez, UniProt, EMBL, ENSEMBL, ChEMBL, WikiGene and RefSeq identifiers. </li> 
                                 <li><b>Select significance threshold: </b>  Define the type of evaluation threshold.  Two options are given: p-value and  corrected p-value (FDR). </li>
                                 <li><b>P-value correction cut-off:</b> User-defined p-value threshold provides a possibility to additionally filter results. 
                                 The threshold defaults to p=0.05, meaning that all significant results are shown. </li>
                                 </ol></p>
                                 <div class="col-md-12">
                                <img src = "help_images/aGoInput.png" style="border: 1px solid black">
                              </div>
                   ')
}

generateHelpaGotoolOutput <- function(){
  HTML(' <p style = "text-align:justify">The results are displayed in Tables for all enrichment terms (ALL Tab), and for each category separately.
                      </p>
                             <p>
                               <h3>1. Results</h3>
                              Each results table contains information about the:
                                <ul>
                                 <li><b>Source</b> </li> 
                                 <li><b>Term ID: </b>the unique term identifier. In the table, Term ID is a hyperlink that points to the correspoding data source of the term </li> 
                                 <li><b>Function: </b>the short name of the function</li> 
                                  <li><b>p-value: </b>hypergeometric p-value after correction for multiple testing</li> 
                                   <li><b>Term size: </b>number of genes that are annotated to the term </li> 
                                    <li><b>Query size: </b> number of genes that were included in the query</li> 
                                     <li><b>Intersection Size: </b> number of genes in the input query that are annotated to the corresponding term </li> 
                                      <li><b>Enrichement Score%: </b>Intersection Size/Term size*100% </li>
                                       <li><b>Positive Hits: </b>a comma separated list of genes from the query that are annotated to the corresponding term</li> 
                                 </ul>
                                 </p>
                                  <p>
                                 
                                  <br>
                                <img src = "help_images/paramaGo.png" style="border: 1px solid black">
                                </p>
                                <br>
                                 <p>
                                <img src = "help_images/aGoOutput.png" style="border: 1px solid black">
                                </p>
                                
                   ')
}



generateHelpPlotsTabPanel <- function(){
  tabPanel(h5("Plots"),
           tags$br(),
           fluidRow(
             column(
               12,
               box(
                 title = "Network",
                 collapsible = T, collapsed = F, solidHeader = T,
                 status = "primary", width = NULL,
                 generateHelpPlotsNetwork()
               )
             ),
             column(
               12,
               box(
                 title = "Heatmap", collapsible = T, collapsed = T,
                 solidHeader = T, status = "primary", width = NULL,
                 generateHelpPlotsHeatmap()

               )
             )
             ,
             column(
               12,
               box(
                 title = "Barchart", collapsible = T, collapsed = T,
                 solidHeader = T, status = "primary", width = NULL,
                 generateHelpPlotsBar()
               )
             ),

             column(
               12,
               box(
                 title = "Scatter Plot", collapsible = T, collapsed = T,
                 solidHeader = T, status = "primary", width = NULL,
                 generateHelpPlotsScatter()
               )
             ),

             column(
               12,
               box(
                 title = "Manhattan", collapsible = T, collapsed = T,
                 solidHeader = T, status = "primary", width = NULL,
                 generateHelpPlotsManhattan()
               )
             ),
             
             column(
               12,
               box(
                 title = "Combination UpSet", collapsible = T, collapsed = T,
                 solidHeader = T, status = "primary", width = NULL,
                 generateHelpPlotsUpset()
               )
             )
           )
          
  ) 
}

generateHelpPlotsNetwork <- function() {
  HTML('
  <div>
    <p>
      The Plots tab contains various visualizations options including networks, 
      heatmaps, barcharts, scatter plots and a Manhattan plot (for the gProfiler pipeline only).
      The first Tab offers three different network visualizations: <b> (a) Functions Vs Genes </b>,
          <b>(b) Functions Vs Functions</b> and <b>(c)Genes Vs Genes</b>.
          
    </p>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/Networkplot1.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "wholeRow">
       <p>
 
        The first tab,<b> Functions Vs Genes</b> connect terms and input genes with an edge wherever
        a gene was enriched in the respective function.
        </p>
        <ol style="font-size:18px">
        
        <li>In the first option, you can select one or more datasources of results to view on the network</li>
        <li>A slider to filter out results by keeping the top n (max value 500).</li>
        <li>Order by which the top results will be filtered; either by -log10Pvalue, or by Enrichment score.</li>
        <li>Layout algorithm choices available in the igraph package.</li>
        <li>A radio button to choose if term results will be shown with their IDs or names in the network view.</li>
        <li>A button that sends the current visible network to Arena3Dweb in order to visualize it in 3D space;
        each different datasource will be assigned in its respective layer in 3D space.</li>
        </ol>
        </div>
  </div>
  
  <hr />
  <div>
   <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Networkplot2.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "sideBySide">
       <p>
       
       Similar to Functions Vs Genes.Here functions are connected with each other 
       with edge weights relative to the number of their shared genes. An extra option 
       named “Similarity score cut-off(%)” allows you to filter out edges below a percent
       of shared genes.

        </p>
        </div>
        </div>
        
        <hr />
  <div>
   <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Networkplot3.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "sideBySide">
       <p>
        In the Genes Vs Genes tab, gene nodes are interconnected based on the number
        of their shared functions. An extra slider allows you to filter out edges below
        a threshold of shared functions.
        </p>
        </div>
        </div>
        
         <hr />
  <div>
   <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/Networkplot4.png"
           style = "width: 1200px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "wholeRow">
       <p>
       
        The main interactive network view. This view can be panned in every direction and/or
        zoomed in and out. There is a collapsible legend that shows the colors for each datasource.
        Nodes can be dragged across the network. An edgelist table is also rendered below the network 
        view containing information on the databases, Ids and names for both source and target nodes, 
        along with additional information such as shared genes and similarity scores, depending on the network type.

        </p>
        </div>
        </div>
        
  <hr />
  <div>
   <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Networkplot5.png"
           style = "width: 900px; border: 1px solid black;"/> 
    </div>
      </div>
       <div class = "sideBySide">
       <p>
       All visible nodes/terms of the network are then appended 
       on the Enrichment Results table.
       </p>
      </div>
    </div>
    
  
  ')
}

generateHelpPlotsHeatmap <- function(){
  HTML(' <div>
      <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Heatmap1.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "sideBySide">
       <p>
        Similarly, heatmaps offer the same three 
        viewing options with the networks.
        </p>
        <ol style="font-size:18px">
        
        <li>Only one datasource can be selected at a time for a heatmap view</li>
        <li>You can filter the number of top viewed functions in the heatmap (max 500).</li>
        <li>Score by which results are ordered; either by -log10Pvalue or by Enrichment Score</li>
        <li>Format in which to show terms on the plot; either term IDs or Names.</li>
        </ol>
        </div>
  </div> 
  <hr />
  <div>
      <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Heatmap2.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "sideBySide">
       <p>
         A <b>heatmap view </b>
            You can hover above any association and a tooltip 
            with key information about the functional term will be shown.
       </p>
        </div>
  </div> 
  
                   ')
}

generateHelpPlotsBar<-function(){
  HTML('<div>
      <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Bar1.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "sideBySide">
       <p>
        The barchart tab offers the following parameters:

        </p>
        <ol style="font-size:18px">
        
        <li>Select one or more datasources</li>
        <li>Choose number of top function to filter (top 500)</li>
        <li>Choose how to order results: by either -log10Pvalue or Enrichment Score</li>
        <li>Choose to view results either with their ID or Name</li>
        </ol>
        </div>
  </div> 
  <hr />
  <div>
      <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Bar2.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "sideBySide">
       <p>
         The <b>bar plot view. </b>
          On mouse hovering, a tooltip with key information about the functional term will be shown.
Next to the bars, the genes found in the function (x) vs the total genes of the function (y) will be shown in a x/y format.
The bars are colored relative to their data source and the respective colors’ legend can be seen on the top right corner of the plot.
Terms that are colored in blue will be clickable and open the associated link in the respective database.


       </p>
        </div>
  </div> 
                   ')}

generateHelpPlotsScatter <-function(){
  HTML(' <div>
      <div class = "sideBySide">
        <div class = "helpImage_div">
           <img src = "help_images/Scatter1.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "sideBySide">
       <p>
       The scatter plot control panel, with similar options with the aforementioned plots.
       </p>
       <ol style="font-size:18px"> 
          <li>Select one or more datasources</li>
          <li>Filter number of top functions (max 500)</li>
          <li>Order the retrieved terms either by -log10Pvalue or by Enrichment Score</li>
        </ol>
        </div>
        </div>
        
        <hr  />
        
         <div>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/Scatter2.png"
           style = "width: 900px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "wholeRow">
       <p>
       The scatter plot view. The x axis represents the -log10Pvalues and the y axis the Enrichment Scores of the top retrieved terms.
        On mouse-hover extra information regarding a node appear such as the term id, function name and both score values.
        A small jitter value has been applied on the circles so they do not fall upon each other when they have the exact same scores.
        Terms are colored according their data source and the various categories can be seen in the legend (top right corner of the plot).

       </p>
        </div>
        </div>
                   ')}

generateHelpPlotsManhattan <- function(){HTML('
      <div>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/manPlot.png"
           style = "width: 1200px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "wholeRow">
       <p>
       A Manhattan plot (available only for gProfiler).

Functional terms are organized according to their chromosomal sequence along the x-axis and colored by their data source.
The Y-axis implies the significance (-log10Pvalue).
Hovering over a data point generates a popup window with key information about the functional term such as its id, name and pvalue.
By selecting a set of points using a lasso or a rectangle, the Manhattan plot will be redrawn showing information about the selected items only.
Upon selection, the corresponding table will be automatically updated.
Export options are also supported and the table is also downloadable.
The plot is fully interactive and one can zoom in and isolate an area of interest.
        </p>
        </div>
        </div>
                   ')
}

generateHelpPlotsUpset <- function(){HTML('
      <div>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/CombinationUpset.png"
           style = "width: 1200px; border: 1px solid black;"/> 
        </div>
      </div>
       <div class = "wholeRow">
       <p>
     <b>Distinct intersections</b> 
     of reported results. <b> A) </b> A selection box to limit the comparisons to certain biomedical entities. 
     <b> B) </b> A searchable and interactive table reporting the intersecting biomedical entries from each tool.
     <b> C) </b> A visual representation of the distinct intersections with the use of interactive Upset plots. 
     <b> D) </b> Part of a table showing 5 of the 16 biomedical entries of the distinct intersection set which corresponds to the chosen UpSet bar. 


        </p>
        </div>
        </div>
                   ')
}

generateHelpNetworkAnalysisTabPanel <- function(){
  tabPanel(h5("Network Analysis"),
           tags$br(),
           fluidRow(
             column(12,box( title = "Network Analysis", collapsible = T, collapsed = F,
                            solidHeader = T, status = "primary", width = NULL, generateHelpNetworkProteinNetwork())),
           )
  )
}

generateHelpNetworkProteinNetwork <- function(){
  HTML('
  <p>
    The network view summarizes the network of predicted associations for a
    particular group of proteins. The network nodes are proteins.
    The edges represent the predicted functional associations. 
  </p>
  <div>
    <div class = "wholeRow">
      <div class = "helpImage_div">
         <img src = "help_images/NetworkAnalysis1.png"
         style = "width: 1100px; border: 1px solid black;"/> 
      </div>
    </div>
    
    <div class = "wholeRow">
     <p>
      Network Parameters:
     </p>
     
      <ol style = "font-size: 18px">
        <li>You need to select an input list from the Select list option.</li>
        <li>You then need to select an organism that matches with the ids 
        included in the selected input list.</li>
        <li>Select interaction type between <b>Full network:</b> edges indicate
        both functional and physical associations and <b>Physical subnetwork:</b>
        edges indicate existence of a physical complex.
        <li>Select the meaning of network edges between: <b>Confidence:</b> edge 
        thickness indicates the strength of data support based on the interaction score.
        and <b>Evidence:</b> edge colors indicate the type of interaction evidence.
        <li>Select interaction cut-off score: this puts a threshold on the confidence score,
        such that only interaction above this score are included in the predicted network.
        Lower scores mean more interactions, but also more false positives.</li>
      </ol>
     </div>
     </div>
     <hr />
     <div>
       <div class = "wholeRow">
       <div class = "helpImage_div">
       <img src = "help_images/NetworkAnalysis2.png"
       style = "width: 1100px; border: 1px solid black;"/> 
    </div>
  </div>
    
           <div class = "wholeRow">
            <ol style = "font-size: 18px">
              <li>Parameters for the STRING network query</li>
              <li>Descriptions for colored and white nodes</li>
              <li>Edge legend</li>
              <li>STRING functionality buttons</li>
              <ul>
              <li>Open in STRING: opens the network in the external STRING web site.</li>
              <li>Download Network: downloads the current STRING edgelist in STRING 
                  format along with the edge scores</li>
              <li>Export Image: exports a png image view of the network</li>
              </ul>
              </ol>

         </div>
         </div>
         <hr />
     <div>
       <div class = "wholeRow">
        <div class = "helpImage_div">
          <img src = "help_images/NetworkAnalysis3.png"
          style = "width: 1100px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "wholeRow">
      <p>
        The STRING network viewer. Clicking on a node opens a window
        with relative information.
      </p>
       </div>
     </div>
                   ')
  
  
}

generateHelpConversionTabPanel <- function(){
  tabPanel(h5("Conversion"),
           tags$br(),
           fluidRow(
             column(12,box( title = "Gene ID Conversion", collapsible = T, collapsed = F,
                            solidHeader = T, status = "primary", width = NULL, generateHelpConversiongConvertTab())),
             column(12,box(title = "Orthology Search", collapsible = T, collapsed = T,
                           solidHeader = T, status = "primary", width = NULL,generateHelpConvesriongOrthTab()))
           )
  )
}

generateHelpConversiongConvertTab <- function(){
  HTML(' <div>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/GeneIDConversion.png"
           style = "width: 1100px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "wholeRow">
        <p style = "text-align: justify">
           The gene conversion tab.
        </p>
        <ol style = "font-size: 18px">
          <li>You need to select the input list of interest</li>
          <li>Then assign the relative input list organism, 
              from 800+ choices included in gProfiler.</li>
          <li>Then assign the target ID namespace to convert your inputs.</li>
          <li>Results can be downloaded in various formats.</li>
          <li>The results table contains 4 columns:</li>
        
          <ul style = "font-size: 18px">
            <li>the input, as pasted/uploaded by the user</li>
            <li>the converted name in target namespace</li>
            <li>the usual gene name</li>
            <li>a description for the gene</li>
  
          </ul>
        </ol>
      </div>
   </div>

 ')
}

generateHelpConvesriongOrthTab <- function() {
  HTML('
    <div>
      <div class = "wholeRow">
        <div class = "helpImage_div">
           <img src = "help_images/Orthology.png"
           style = "width: 1100px; border: 1px solid black;"/> 
        </div>
      </div>
    
      <div class = "wholeRow">
        <p style = "text-align: justify">
           The orthology conversion tab
        </p>
        <ol style = "font-size: 18px">
         <li>Again first select a list</li>
         <li>Then an input organism from the 800+ available in gProfiler</li>
         <li>Then another gProfiler organism as target</li>
         <li>Various download formats</li>
         <ul>
         <li>The results table contains 5 columns:</li>
         <li>the input gene names/ids</li>
         <li>the input ENSG ids</li>
         <li>the ortholog names in the target organism</li>
         <li>the ortholog ENSG ids</li>
         <li>a description for the gene</li>
         </ul>
         </ol>

      </div>
   </div>

  ')
}

generateHelpAPITabPanel <- function(){
  tabPanel(h5("API"),
           tags$br(),
           fluidRow(
             column(12,box( title = "POST Request", collapsible = T, collapsed = F,
                            solidHeader = T, status = "primary", width = NULL, generateHelpAPIPOSTRequest())),
             column(12,box( title = "GET Request", collapsible = T, collapsed = F,
                            solidHeader = T, status = "primary", width = NULL, generateHelpAPIGETRequest()))
           )
  )
}

generateHelpAPIPOSTRequest <- function(){
  HTML('
<p>
  To open <i>Flame</i> from an external application, we offer an API that allows a <b>POST</b> request along with a JSON object.
  The API link is <b><u><i>https://bib.fleming.gr/bib/api/flame</i></u></b>. Don\'t forget to set the <b>Header Content-Type</b> to <b>application/json</b>. Here, we provide an example of the required JSON object format:
</p>
<h3>Simple 3 gene list example</h3>
<pre>
{
  "gene_list1": ["GNAS", "ABCG2", "WT1", "CDK2", "FLT1", "CCN2", "INSR"],
  "gene_list2": ["MMP1", "PTGS2", "C3", "PON1", "LDLR", "HBA1", "CYP1B1", "PTEN", "SNCA", "RAC1", "BCL2", "HLA-DRB1", "IL13", "GRN", "NF1", "AHR", "YAP1", "LCN2", "FOXO3", "LEPR", "ABCB1", "STAT3", "HRAS", "NOS3", "HLA-DQA1", "BRCA1", "EZH2", "CDKN1B"],
  "gene_list3": ["HLA-C", "PTEN", "UGT1A1", "CDH1", "MDM2", "EGFR", "FMR1", "VEGFA", "ERCC1", "VWF", "CFH", "OGG1", "TGFB1", "IGF2", "ADRB2", "AGER", "CETP", "CYP2D6", "KIT", "HTT", "ACE2", "CASP8", "HDAC1", "GJB2", "IL4", "EDN1", "OPRM1", "NOS3", "ERCC2", "NFKB1", "KCNQ1"]
}
</pre>
<p>
  The server then returns a JSON response with the url that links to the <i>Flame</i> application, having the requested gene lists (up to 10) loaded:
</p>
<pre>
{
  "url": "https://bib.fleming.gr:8084/app/flame?f=110407183nWHu5VfeP9fM.json"
}
</pre>
')}

generateHelpAPIGETRequest <- function(){HTML('
For shorter gene lists, we also allow <b>GET</b> requests in the following format <b><u><i>https://bib.fleming.gr:8084/app/flame/?url_genes=MCL1,TTR;APOE,ACE2;TLR4,HMOX1,TP73</i></u></b> where gene lists are separated with semicolon (;) and genes with comma (,).
')
  
  
}
