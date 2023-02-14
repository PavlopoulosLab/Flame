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
    HTML("</h4>")
  )
)
