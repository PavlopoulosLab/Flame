<!-- Badges -->

[![Docker Pulls](https://img.shields.io/docker/pulls/pavlopouloslab/flame.svg)](https://hub.docker.com/r/pavlopouloslab/flame)
[![Shiny App](https://img.shields.io/badge/Shiny-online-brightgreen)](http://flame-enrich.org)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

# Flame: Functional & Literature Enrichment Analysis

> Visualization and interpretation of functional and literature enrichment analysis results from multiple sets.

---

## 📖 Table of Contents

1. [Overview](#overview)
2. [Key Features](#key-features)
3. [Installation](#installation)
4. [Usage](#usage)
5. [Publications](#publications)
6. [License](#license)

---

## 📝 Overview

Flame is a web application (R/Shiny + JavaScript) for comprehensive enrichment analysis. It allows you to upload various input types, combine and compare sets, run functional and literature enrichments through multiple tools, and visualize results interactively.

---

## 🚀 Key Features

* **Multiple Input Types**: Gene/protein lists, SNPs, free text, or expression results (volcano plots).
* **Set Operations**: Union and intersection of lists with UpSet plots.
* **Functional Enrichment**: Supports aGOtool, gProfiler, WebGestalt, and enrichR across 14,000+ organisms.
* **Literature Enrichment**: Powered by aGOtool.
* **Network Integration**: Generate STRING protein–protein interaction networks.
* **Visualization Options**: Networks, heatmaps, bar charts, scatter plots, and searchable tables.
* **Network Analysis**: Gene–function, function–function, and gene–gene association levels.
* **Interactive 3D Views**: Multi-layered network visualization via Arena3Dweb.
* **Filtering & Integration**: Combine sources and apply interactive filters.
* **ID Conversions**: Cross-database and cross-species.
* **API Access**: Integrate Flame with external applications.

---

## 🛠 Installation

### Docker (Recommended)

```bash
# Pull the Flame image
docker pull pavlopouloslab/flame
# Run the container
docker run -p 3838:3838 pavlopouloslab/flame
```

### From Source

1. Clone the repository:

   ```bash
   ```

git clone [https://github.com/PavlopoulosLab/Flame.git](https://github.com/PavlopoulosLab/Flame.git)
cd Flame

````
2. Ensure R (>=4.0) and RStudio are installed.
3. Install required R packages:
   ```r
install.packages(c(
  "shinyjs", "shinyalert", "bsplus", "upsetjs", "stringr",
  "httr", "httpuv", "curl", "jsonlite", "gprofiler2",
  "WebGestaltR", "shinydashboard", "shinyWidgets", "DT",
  "tidyr", "enrichR", "plotly", "igraph", "visNetwork"
))
````

4. Open **Flame.Rproj** in RStudio.
5. Open **server.R**, select **Run External**, then click **Run App**.

---

## 💻 Usage

Access the web interface at [http://flame-enrich.org](http://flame-enrich.org) or run locally as above. Upload your data, explore enrichment analyses, and customize visualizations using the intuitive UI.

---

## 📚 Publications

* **Flame (v2.0): advanced integration and interpretation of functional enrichment results from multiple sources**
  Karatzas E., Baltoumas F.A., Aplakidou E., Kontou P.I., Stathopoulos P., Stefanis L., Bagos P.G., Pavlopoulos G.A.
  *Bioinformatics*. 2023 Aug;39(8)\:btad490.
  doi: [10.1093/bioinformatics/btad490](https://doi.org/10.1093/bioinformatics/btad490)

* **FLAME: a web tool for functional and literature enrichment analysis of multiple gene lists**
  Thanati F., Karatzas E., Baltoumas F.A., Stravopodis D.J., Eliopoulos A.G., Pavlopoulos G.A.
  *Biology*. 2021 Jul;10(7):665.
  doi: [10.3390/biology10070665](https://doi.org/10.3390/biology10070665)

---

## 📄 License

This project is licensed under the **MIT License** – see the [LICENSE](LICENSE) file for details.
