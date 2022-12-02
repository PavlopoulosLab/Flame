welcomePage <- fluidRow(
  column(
    12,
    HTML(
      "<h1><strong>Welcome to <i>Flame</i></strong></h1>
      <br /><h3><strong>A web tool for Functional and
      Literature Enrichment analysis of multiple sets</strong></h3>"
    ),
    tags$hr(style = "height:2px; border-color: #ffa600;"),
    HTML(
      "<h3>With <i>Flame</i> one can:</h3>
      <h4>
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
      <br /><br />
      Get started by uploading your gene lists"
    ),
    actionLink("link_to_fileinput", " here."),
    HTML("</h4>"),
    textOutput("url_checker"),
  )
)
