# Flame
Visualization and interpretation of functional and literature enrichment analysis results from multiple sets

With Flame one can:
* Upload multiple gene/protein lists, SNPs, free text or gene expression results (Volcano plot)
* Combine lists by calculating their unions and intersections with the help of UpSet plots
* Perform functional enrichment analysis on any of the combined lists for more than 14,000 organisms, using four tools (aGOtool, gProfiler, WebGestalt, enrichR)
* Combine and compare enrichment results from multiple runs
* Perform literature enrichment analysis on any of the combined lists, using aGOtool
* Generate protein-protein interaction networks via STRING
* Visualize results with the use of networks, heatmaps, bar charts, scatter plots and searchable tables
* Perform network analysis at the gene-function, function-function and gene-gene association levels
* Visualize networks in interactive, multi-layered views with Arena3Dweb
* Combine data sources and apply interactive filters on the generated plots
* Perform cross-database and cross-species id conversions
* Utilize our API to connect Flame with external applications

Flame is written in R/Shiny and JavaScript and is available at http://flame.pavlopouloslab.info

# Installation

* Download and install the Flame image from Docker Hub: https://hub.docker.com/r/pavlopouloslab/flame

* Otherwise, download this GitHub repo and run Flame via RStudio. R (https://www.r-project.org/) and RStudio (https://rstudio.com/) must be installed. Make sure the following R libraries are also installed:
**shinyjs**, **shinyalert**, **bsplus**, **upsetjs**, **stringr**, **httr**, **httpuv**, **curl**, **jsonlite**, **gprofiler2**, **WebGestaltR**, **shinydashboard**, **shinyWidgets**, **DT**, **tidyr**, **enrichR**, **plotly**, **igraph**, **visNetwork**.
To start the program, double click on the **Flame.Rproj** file. This opens the RStudio process. Then, open the **server.R** file in RStudio and in the Run App options choose "Run External" and then click Run App.

# Cite
Karatzas, E., Baltoumas, F.A., Aplakidou, E., Kontou, P.I., Stathopoulos, P., Stefanis, L., Bagos, P.G. and Pavlopoulos, G.A. (2023)
**Flame (v2.0): advanced integration and interpretation of functional enrichment results from multiple sources**
*Bioinformatics*, Volume 39, Issue 8, August 2023, btad490
https://doi.org/10.1093/bioinformatics/btad490
Thanati, F., Karatzas, E., Baltoumas, F.A., Stravopodis, D.J., Eliopoulos, A.G. and Pavlopoulos, G.A. (2021)
**FLAME: a web tool for functional and literature enrichment analysis of multiple gene lists.**
*Biology*, 10(7), p.665, doi: 10.3390/biology10070665
https://www.mdpi.com/2079-7737/10/7/665
