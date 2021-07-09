fileInputTab <-  HTML('<b>Flame</b> provides the option to select and upload <b>multiple files</b> simultaneously and/or write in a text area field.
                      <p>
                      <br>
                      <img src = "help_images/fileinput1.png" style="float:left;border: 1px solid black;margin-right:20px; ">
                        <h3>1. Choose a file</h4>
                      Click the <b>Browse</b> button of the upload form to select and upload one or multiple files. 
                      Acceptable file formats: 
                      <br>&bull; Flat text (.txt, .tsv, .csv)<br>
                        <h3>2. Paste your gene list</h3>
                      The text area field forms a basic <b>text input area</b>, enabling the creation of <b>custom list</b> by writing or pasting a section of a list. 
                      Input can consist of mixed typed of IDs separated by comma, space, new line or tab.
                      By pressing the <b><i>Add to files</b></i> button the formed text can be added to the list of files for further analysis, whereas the <b><i>Clear</b></i> button can be used to discard the unwanted text area input. Clicking
                      the <b><i>Example</i></b> button will generate an example of 100 genes in the form. Every single time a different gene list is generated.
                      <br>
                      <b>Notes:</b><br>
                      &bull; The size of each file cannot exceed 1 MB.<br>
                      &bull; A maximum of 10 files can be uploaded/created in the text area for each session.<br>
                      &bull; Notably, Flame does not allow lists with the same name; thus providing options for renaming and deleting.
                      
                      </p>
                      <div class="col-md-12"><hr></div>
                              <p>
                                <img src = "help_images/fileinput2.png" style="float:left;border: 1px solid black;margin-right:20px;">
                                <br>
                                <h3>3.  Select files for Upset plot / Rename / Remove</h3>
                                After file submission, a <b>checkbox list</b> will appear, containing all uploaded files and/or submitted texts <i>(Figure 2)</i>. Any additional uploaded or created files are appended to the selection list.
                                <br>
                                Files can be selected and manipulated by clicking the checkboxes next to their names. One or multiple files can be:<br>
                                 &bull; deleted by selecting them and clicking the <b><i>Remove</b></i> button.<br>
                                 &bull; renamed, by selecting them and clicking the <b><i>Rename</b></i> button. A dialog box will appear, asking you to type the new name of the selected file.</li>
                              </p>
                           
                      ')
upsetTab<- HTML('     The <b>UpSet plot</b> is a sophisticated alternative of a Venn-diagram and is prefered for many sets (>5) where a Venn diagram becomes 
                      incomprehensive. UpSet plots help in understanding the relationships among different sets by visualizing their intersections as a matrix. 
                      To this end, each column corresponds to a set whereas each row to one segment in a Venn diagram. <br>
                      In its current version, Flame supports four UpSet plot modes:<br>
                      &bull; intersection<br>
                      &bull; distinct intersection<br>
                      &bull; distinct per file and<br> 
                      &bull; union 
                      
                      <div class="col-md-12"><hr></div>
                      <p>
                      <h3>1. Select files to create upset plot</h3>
                      Files can be selected  by clicking the checkboxes next to their names in the checkbox file list.
                      You should select at least 2 files to create the plot. After the selection of the files, click the <b><i>Create Upset plot </b></i> button.
                      <br>
                      <img src = "help_images/upset1.png" style="float:left;border: 1px solid black;margin-right:20px;">
                      </p>
                      
                      <div class="col-md-12"><hr></div>
                      
                      <div class="col-md-12">
                      <p>
                      <h3>2. Upset Modes </h3>
                      An UpSet plot consists of two axes and a connected-dot matrix. The vertical rectangles represent the number of elements
                      participating in each list combination. The connected-dots matrix indicates which combination of lists corresponds to which vertical rectangle. 
                      Finally, the horizontal bars (Set Size) denote the participation of hovered objects (from the vertical rectangles) in the respective lists. 
                      <br>
                      <b>Flame</b> supports four UpSet plot modes: <br>
                                
                                 &bull;<b>Intersections:</b> creates all file combinations as long as they share at least one element, allowing an element to participate in more than one combination.  <br> 
                                 &bull;<b>Distinct intersections:</b> creates file combinations only for distinct elements that do not participate in other lists.  <br> 
                                 &bull;<b>Distinct elements per file:</b> shows the distinct elements of each input list. <br> 
                                 &bull;<b>Unions:</b> constructs all available file combinations, showing their total unique elements.  
                       <br>
                       </p>
                       </div>
                       <div class="col-md-12">
                            <img src = "help_images/upsetModes.png"  style="float:left;border: 1px solid black;margin-right:20px;">

                          </div>  
                             <div class="col-md-12"><hr></div>
                      
                       <div class="col-md-12">
                         <p>
                        <h3>3. Select UpSet plot element to create a new file</h3>
                     On <b>mouse-hover</b> over each UpSet plot element combination (vertical rectangles), Flame presents the respective genes in a table, while on a <b>mouse click</b> the
                      user can append the selected list of files with a new file containing the selected genes and process it separately.</p>
                     </div>
                        <div class="col-md-12">
                        <img src = "help_images/upset2.png"  style="float:left;border: 1px solid black;margin-right:20px;">
                        </div>
                        
                         
                       
                      ')
viewData <-HTML('<p>
                <img src = "help_images/viewData.png" style="float:left;border: 1px solid black;margin-right:20px;">
                  <h3>3.View Data</h3>
                    All the bioentities of the uploaded files and the files created by the Upset plot are displayed in a table in the View Data Tab.
                     The files can be download in several formats.
                      </p>
                   ')
  
  
gconvertTab <- HTML('<p><b>Gene ID Conversion</b> is a gene ID mapping tool that allows conversion of genes, proteins, microarray probes, common names, various database identifiers, etc.
                      Also supports major public databases and naming conventions like Uniprot, EMBL, RefSeq, Entrez  and organism-specific ID schemas, corresponding
                       mappings are retrieved from Ensembl. 
                      <br><br></p>
                              <p>
                                <img src = "help_images/gconvert.png" style="border: 1px solid black">
                         
                      <ul>
                       <li>The user needs to select the file for conversion from the <b>Select file to convert</b> option. </li>
                      <li>The file may consist of a mixture of IDs of different types</li>
                      <li>The user needs to select an organism that matches with the input gene list from the <b>Select input organism</b> option and the target database from the <b>Target namespace</b> option to which all input IDs will be converted to. 
                      Default target database is Entrez Gene Names. </li>
                      <li>Input IDs that have no corresponding entry in target database will not be displayed .</li>
                      <li>In case of chormosomal regions ENSG genes from these regions are retrieved automatically. Genes need not fit the region fully, and hence one may even study single nucleotides (SNPs). 
                      The format that is accepted is chromosome:start:end for chromosomal regions (e.g. X:1:2000000).</li>
                      <li>In case of term ID, the tool retrieves all genes of a given organism associated to the given term. 
                      For example, when queried for GO:0007507 (heart development) with organism H. sapiens, it retrieves about a hundred human genes associated to heart development.</li>
                      </ul>
                     </p>
                   ')
gorthTab <- HTML('<p><b>Orthology Search</b> is a tool for mapping orthologous genes between related organisms based on data collected into the Ensembl database. Orthologous genes are similar in sequence and are likely conserved through evolution since a 
                  common ancestor. Orhologous genes may also carry out similar function and are therefore relevant in functional analysis. Ortholog mappings are based on Ensembl alignments. 
                      <br><br></p>
                              <p>
                                <img src = "help_images/gorth.png" style="border: 1px solid black">
                                
                      <ul>
                       <li>The user needs to select the file for conversion from the <b>Select file for orthology search</b> option. </li>
                      <li>The input can be a mixed list of IDs for genes or other biomolecules of the organism of interest that the user also needs to select as a target organism.</li>
                      <li>The user needs to select an organism that matches with the input gene list from the <b>Select input organism</b> option and the target organism from the <b>Select target organism</b> option.
                      All genes from the input are then mapped to the orthologous genes of the target organism. </li>
                      <li>Input IDs that have no corresponding entry in either the given organism or the target organism will not be displayed.</li>
                      <li>In case of chormosomal regions ENSG genes from these regions are retrieved automatically. Genes need not fit the region fully, and hence one may even study single nucleotides (SNPs). 
                      The format that is accepted is chromosome:start:end for chromosomal regions (e.g. X:1:2000000).</li>
                      <li>In case of term ID, the tool retrieves all genes of a given organism associated to the given term. 
                      For example, when queried for GO:0007507 (heart development) with organism H. sapiens, it retrieves about a hundred human genes associated to heart development.</li>
                      </ul>
                      </p>
                   ')
    proteinNetwork <- HTML('<p>The network view summarizes the network of predicted associations for a particular group of proteins. The network nodes are proteins.
                                The edges represent the predicted functional associations. 
                            </p>
                            <div class="col-md-12"><hr></div>
                           <p>
                            
                                <img src = "help_images/proteinNetworkInput.png" style="border: 1px solid black">
                                <br>
                               
                            
                                <h3>1. Network Parameters</h3>
                                <ul>
                                  <li>The user needs to select the file from the <b>Select file for analysis</b> option. </li>
                                  <li>The user needs to select an organism that matches with the input list from the <b>Select input organism</b> option and the<b> parameters:</b>.</li>
                                  <ul>
                                  <li><b>Confidence:</b> edge thickness indicates the strength of data support based on the interaction score. </li>
                                  <li><b> Evidence:</b> edge colors indicate the type of interaction evidence.</li>
                                  <li><b>Full network</b>: edges indicate both functional and physical associations.</li>
                                  <li><b>Physical subnetwork</b>: edges indicate existence of a physical complex.</li>
                                  <li><b> Interaction score cut-off</b>: puts a threshold on the confidence score, such that only interaction above this score are 
                                  included in the predicted network. Lower score mean more interaction, but also more false positives. </li>
                                  </ul>
                                </ul>
                            </p>
                           
                      <div class="col-md-12"><hr></div>
                      <div class="col-md-12">
                          <h3>2. Network Results</h3>
                          <ul>
                            
                            <li>Clicking on a node gives several details about the protein.</li>
                            <li>The network is downloadable in TSV file. Also you can export the network as an image or redirect the network to STRING database for further analysis.</li>
                          </ul>
                      </div>
                      <div class="col-md-12">
                          <img src = "help_images/proteinNetwork.png" style="border: 1px solid black">
                          <br>
                        
                      </div>
                   ')
gprofInput <- HTML(' <p style = "text-align:justify"><b>Functional Enrichment Analysis: gProfiler</b> is a tool that performs functional enrichment analysis, also known as over-representation analysis (ORA) or
                        gene set enrichment analysis, on input gene list. It maps genes to known functional information sources and detects statistically significantly enriched terms. The input can consist of mixed types of gene
                        IDs (proteins, transcripts, microarray IDs, etc), SNP IDs, chromosomal intervals or term IDs.
                      <br><br></p>
                              
                               <div class="col-md-12">
                               <h3>1. Select the Parameters</h3>
                      <ol type="1">
                                 <li><b>Select file for analysis.</b> </li> 
                                 <li><b>Select organism:</b> select organism that matches your input query gene list. A choice among 197 species is given. Default organism is human (Homo sapiens). </li> 
                                 <li><b>Select datasources:</b> </li> 
                                    <ul>
                                      <li> Gene Ontology: Biological Process, Molecular Function and Cellular Component</li>
                                      <li> Metabolic Pathways: KEGG, Reactome, WikiPathways</li>
                                      <li> Regulatory Motifs: TransFac, miRTarBase</li>
                                      <li> Protein Databases: Human Protein Atlas (for H. sapiens only!), CORUM</li>
                                      <li> Phenotypes: Human Phenotype Ontology (for H. sapiens only!)</li>
                                    </ul>
                                  
                                 <li><b>Select ID type for output:</b> Define the ID type that will be used in the analysis, as well as in the output.
                                 Enrichment analysis is basically performed using ENSEMBL identifiers, while, based on the user’s choice. Results can be reported as Entrez, UniProt, EMBL, ENSEMBL, ChEMBL, WikiGene and RefSeq identifiers. </li> 
                                 <li><b>Significance threshold:</b>  Define the type of evaluation threshold.  Three options are given: g:SCS (the default g:Profiler p-value), Bonferroni and False Discovery Rate (FDR) </li>
                                 <li><b>P-value correction cut-off:</b> User-defined p-value threshold provides a possibility to additionally filter results. 
                                 The threshold defaults to p=0.05, meaning that all significant results are shown. </li>
                                 </ol></p>
                                 <div class="col-md-12">
                                <img src = "help_images/gprofInput.png" style="border: 1px solid black">
                                
                              </div>
                      </div>
                   ')

gprofOutput <- HTML(' <p style = "text-align:justify">The results are displayed in Tables for all enrichment terms (ALL Tab), and for each category separately.
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
                                     <li><b>Intersection Size: </b> the number of genes in the input query that are annotated to the corresponding term </li> 
                                      <li><b>Enrichement Score%: </b>Intersection Size/Term size*100% </li>
                                       <li><b>Positive Hits: </b>a comma separated list of genes from the query that are annotated to the corresponding term</li> 
                                 </ul>
                                 </p>
                                 <div class="col-md-12">
                                 <img src = "help_images/paramGprof.png" style="border: 1px solid black">
                                <img src = "help_images/gprofOutput.png" style="border: 1px solid black">
                                <br>
                              </div>
                   ')
manPlot <- HTML(' <p style = "text-align:justify">In the case of <b>Functional Enrichment Analysis: gProfiler</b> only, an adjusted to the selected data sources
                                  interactive <b>Manhattan plot </b>is offered for a clearer overview. <br>
                                  In this plot:<br>
                                   &bull; Functional terms are organized along the x-axis and colored by their data source. <br>
                                    &bull; Y-axis shows the significance (p-value) of each term.<br>
                                     &bull; Hovering over a data point generates a popup window with key information about the functional term. <br>
                                      &bull; By selecting a set of points using a lasso or a rectangle, the Manhattan plot 
                                  will be redrawn showing information about the selected items only. <br>
                                   &bull; Upon selection, the corresponding tables will be automatically updated.  <br>
                                    &bull; Export options are also supported and the table is also downloadable. <br>
                                     &bull; The plot is fully interactive and one can zoom in and isolate an area of interest.
                                 <br>
                                 
                                <img src = "help_images/manPlot.png" style="border: 1px solid black">
                                
                               </p>
                   ')

scatterPlot <- HTML(' <p style = "text-align:justify">In all of the enrichment-type analyses offered by Flame, <b>for every source</b>, the most significant
                        functional terms will be shown in a <b>Scatter plot</b>: <br><br>
                       &bull; The user can further customize to adjust the </b>desired number of terms</b> analyzed. Enriched Terms are sorted according to the enrichment score.<br>
                       &bull; On <b>mouse hovering</b>, a tooltip with key information about the functional term will be shown.<br>
                       &bull; The terms depicted in the plot will also appear in <b>table</b> format below the graph.
                       The number of terms in the table will be the same number of terms as in the graph. <br>
                        &bull; The plot is fully interactive and one can zoom in and isolate an area of interest and adjust the number of elements which will be shown. Export options are also supported.<br>
                       &bull; The results can also be <b>downloaded</b> through the table below the plot, in <b>CSV, Excel or PDF format</b>.
                       </p>
                       <br>
                        <div class="col-md-12">
                        <img src = "help_images/scatterPlot.png" style="border: 1px solid black">
                              </div>
                   ')
barPlot <- HTML(' <p style = "text-align:justify">In all of the enrichment-type analyses offered by Flame, <b>for every source</b>, or <b> multiple selected sources </b> the most significant
                        functional terms will be shown in a <b>Barchart</b>: <br><br>
                       &bull; The <b>Select barplot</b> option sorts the terms and the barplot in accordance to Enrichment Score (Default option) or -log10Pvalue.<br>
                       &bull; The user can further customize to adjust the <b>desired number of terms</b> analyzed. Enriched Terms are sorted according to the selected option.<br>
                       &bull; On <b>mouse hovering</b>, a tooltip with key information about the functional term will be shown.<br>
                       &bull; The terms depicted in the barplot will also appear in<b> table</b> format below the graph.
                       The number of terms in the table will be the same number of terms as in the graph. <br>
                       &bull; The plot is fully interactive and one can zoom in and isolate an area of interest,and adjust the number of elements which will be shown. Export options are also supported.<br>
                       &bull; The results can also be <b>downloaded</b> through the table below the plot, in <b>CSV, Excel or PDF format</b>.
                       </p>
                       <br>
                        <div class="col-md-12">
                        <img src = "help_images/barPlot.png" style="border: 1px solid black">
                              </div>
                   ')
heatMapHelp <- HTML(' <p style = "text-align:justify">In all of the enrichment-type analyses offered by Flame, <b>for every source</b>, 2 types of <b> Heatmaps </b> are available: <br><br>
                         In the first case, genes are plotted against functional terms, while in the second case functional terms are plotted against themselves (square matrix). A color-scheme is used to capture the enrichment 
                        or the statistical score for a particular functional term. In the second mode (functional terms vs functional terms), a cell value depicts a similarity capturing the number of common genes. 
                        This is calculated as the summation of the unique common genes between a pair of functional terms divided by the number of total unique genes found to be associated with the functional terms. 
                        <br>
                       &bull; The <b>Select type of heatmap</b> option sorts the terms and the Heatmap in accordance to Enrichment Score (Default option) or -log10Pvalue.<br>
                       &bull; The user can further customize to adjust the <b>desired number of terms</b> analyzed. Enriched Terms are sorted according to the selected option.<br>
                       &bull; On <b>mouse hovering</b>, a tooltip with key information about the functional term will be shown.<br>
                       &bull; The terms depicted in the heatmap will also appear in<b> table</b> format below the graph.<br>
                       &bull; All of the heatmaps are fully <b>interactive</b> and one can zoom in and isolate an area of interest, swap the x and y axes and adjust the number of elements which will be shown. Notably, 
                        all heatmaps are clustered after applying a hierarchical clustering method and export options are also supported.<br> 
                       &bull; The results can also be <b>downloaded</b> through the table below the plot, in <b>CSV, Excel or PDF format</b>.
                       </p>
                       <br>
                        <div class="col-md-12">
                        <img src = "help_images/heatMap.png" style="border: 1px solid black">
                              </div>
                   ')
networkHelp <-HTML(' <p style = "text-align:justify"><b>Flame</b> offers  three types of networks: <br>
                            <b>1.</b> Nodes represent genes and/or functional terms (colored differently), whereas edges represent the similarity scores between them. <br>
                            <b>2.</b>Nodes represent the functional terms, while edges reflect the number of common genes between them. <br>
                            <b>3.</b>Nodes represent genes which are connected based on the number of common functions or processes they are involved in. <br>

                        <br><br>
                       &bull; The <b>Select network</b> option sorts the terms in accordance to Enrichment Score (Default option) or -log10Pvalue.<br>
                       &bull; The user can further customize to adjust the <b>desired number of terms</b> analyzed. Enriched Terms are sorted according to the selected option.<br>
                       &bull; Users can apply an <b>edge-cutoff</b> based on the <b>similarity score</b> to reduce the network s density, and make visualization more appealing and more informative. <br>
                       &bull; The terms depicted in the networks and the interactions among them  will also appear in<b> table</b> format below the graph.<br>
                       &bull; All of the networks are fully <b>interactive</b> and one can zoom in and isolate an area of interest and adjust the number of elements which will be shown. <b>Export options as an Image</b> are also supported.<br> 
                       &bull; The results can also be <b>downloaded</b> through the table below the plot, in <b>CSV, Excel or PDF format</b>.
                       </p>
                       <br>
                        <div class="col-md-12">
                        <img src = "help_images/network.png" style="border: 1px solid black">
                              </div>
                   ')
aGoInput <- HTML(' 
                               <p>
                               <h3>1. Select the Parameters</h3>
                      <ol type="1">
                                 <li><b>Select file for analysis.</b> </li> 
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

aGoOutput <- HTML(' <p style = "text-align:justify">The results are displayed in Tables for all enrichment terms (ALL Tab), and for each category separately.
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
                                     <li><b>Intersection Size: </b> the number of genes in the input query that are annotated to the corresponding term </li> 
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
literatureInput <- HTML('   <p><h3>1. Select the Parameters</h3>
                                <ol type="1">
                                 <li><b>Select file for analysis.</b> </li> 
                                 <li><b>Select organism:</b> select organism that matches your input query gene list. A choice among 197 species is given. Default organism is human (Homo sapiens). </li> 
                                 <li><b>PubMed Publications</b> is used as a source for Literature Enrichment </li> 
                                 <li><b>Select ID type for output:</b> Define the ID type that will be used in the analysis, as well as in the output.
                                Results can be reported as Entrez, UniProt, EMBL, ENSEMBL, ChEMBL, WikiGene and RefSeq identifiers. </li> 
                                 <li><b>Select significance threshold: </b>  Define the type of evaluation threshold.  Two options are given: p-value and  corrected p-value (FDR). </li>
                                 <li><b>P-value correction cut-off:</b> User-defined p-value threshold provides a possibility to additionally filter results. 
                                 The threshold defaults to p=0.05, meaning that all significant results are shown. </li>
                                 </ol>
                                 </p>
                                 <p>
                                <img src = "help_images/literatureInput.png" style="border: 1px solid black">
                              </p>
                   ')

literatureOutput <-HTML(' 
                             <p>
                               <h3>1. Results</h3>
                             Results contains information about the:
                                <ul>
                                 <li><b>Source</b> </li> 
                                 <li><b>Term ID: </b>the unique term identifier. In the table, Term ID is a hyperlink that points to the correspoding publication in PubMed </li> 
                                 <li><b>Publication: </b>the name of the Publication</li> 
                                  <li><b>p-value: </b>hypergeometric p-value after correction for multiple testing</li> 
                                   <li><b>Term size: </b>number of genes that are annotated to the term </li> 
                                    <li><b>Query size: </b> number of genes that were included in the query</li> 
                                     <li><b>Intersection Size: </b> the number of genes in the input query that are annotated to the corresponding term </li> 
                                      <li><b>Enrichement Score%: </b>Intersection Size/Term size*100% </li>
                                       <li><b>Positive Hits: </b>a comma separated list of genes from the query that are annotated to the corresponding term</li> 
                                 </ul>
                                 </p>
                                  <p>
                                 
                                  <br>
                                <img src = "help_images/paramLiter.png" style="border: 1px solid black">
                                </p>
                                <br>
                                 <p>
                                <img src = "help_images/literatureOutput.png" style="border: 1px solid black">
                                </p>
                   ')