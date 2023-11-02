welcomePage <- fluidRow(
  column(
    12,
    HTML(
      "<h1><strong>Welcome to <i>Flame</i> (v2.0)</strong></h1>
      <h3><strong>Visualization and interpretation of
      functional and literature enrichment analysis results from multiple sets</strong></h3>"
    ),
    tags$hr(style = "height:2px; border-color: #ffa600;"),
    HTML(
      "<h3>With <i>Flame</i> one can:</h3>
      <h4>
      <div class = \"expandText\">
        <ul>
          <li>Upload <b>multiple</b> gene/protein lists, SNPs, free text or gene expression results (Volcano plot)<br />
          <li><b>Combine lists</b> by calculating their unions and intersections with the help of UpSet plots<br />
          <li>Perform <b>functional enrichment</b> analysis on any of the combined lists for more than 14,000 organisms, using four tools (aGOtool, gProfiler, WebGestalt, enrichR)<br />
          <li><b>Combine</b> and <b>compare</b> enrichment results from multiple runs<br />
          <li>Perform <b>literature enrichment</b> analysis on any of the combined lists, using aGOtool<br />
          <li>Generate <b>protein-protein interaction</b> networks via STRING<br />
          <li><b>Visualize</b> results with the use of networks, heatmaps, bar charts, scatter plots and searchable tables<br />
          <li>Perform <b>network analysis</b> at the gene-function, function-function and gene-gene association levels<br />
          <li>Visualize networks in interactive, <b>multi-layered</b> views with Arena3D<sup>web</sup><br />
          <li>Combine data sources and apply <b>interactive filters</b> on the generated plots<br />
          <li>Perform <b>cross-database</b> and <b>cross-species id conversions</b>
          <li>Utilize our <b>API</b> to connect Flame with external applications</b>
        </ul>
      </div>
      <br /><br />
      Get started by uploading your gene lists"
    ),
    actionLink("link_to_fileinput", " here."),
    HTML("</h4>"),
    HTML("<hr>
               <h4> Cite <i>Flame</i> </h4>
      <p style='font-size:15px'>If you find Flame useful in your work please cite:</p>
      <ul>
        <li> Karatzas, E., Baltoumas, F.A., Aplakidou, E., Kontou, P.I., Stathopoulos, P., Stefanis, L., Bagos, P.G., Pavlopoulos, G.A. (2023) <b>Flame (v2.0): advanced integration and interpretation of functional enrichment results from multiple sources.</b> 
        <i>Bioinformatics</i>. Volume 39, Issue 8, August 2023, btad490; doi: <a href='https://doi.org/10.1093/bioinformatics/btad490' target='_blank'>10.1093/bioinformatics/btad490</a>; Pubmed: <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10423032/' target='_blank'>37540207</a> </li>
        <li> Thanati, F., Karatzas, E., Baltoumas, F.A., Stravopodis, D.J., Eliopoulos, A.G., Pavlopoulos, G.A. (2021) <b>FLAME: a web tool for functional and literature enrichment analysis 
      of multiple gene lists,</b> <i>Biology</i>. 2021; 10(7):665; doi: <a href='https://www.mdpi.com/2079-7737/10/7/665' target='_blank'>10.3390/biology10070665</a>; Pubmed: <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8301326/' target='_blank'>34356520</a></li>
      </ul>
         ")
  )
)
