<!-- Badges -->

[![Docker Pulls](https://img.shields.io/docker/pulls/pavlopouloslab/flame.svg)](https://hub.docker.com/r/pavlopouloslab/flame)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](LICENSE)

# Flame 3.0: Functional Analysis and Integration made easy

Flame runs functional and literature enrichment on one or many gene lists with seven established tools
(g:Profiler, STRING, Enrichr, WebGestalt, PANTHER, GeneCodis, clusterProfiler) plus your own GMT gene sets,
combines their results, and lets you explore them with interactive plots and networks. It also starts from
variants and GWAS results. It is free and open to everyone: no login, no registration.

## Features

* **Inputs**: paste/upload IDs (19 formats) or ranked lists with scores, select genes on a volcano or
  2D-reduction (UMAP/t-SNE/PCA) plot, map SNPs to genes, find genes in free text, and combine lists with UpSet
  set operations.
* **Enrichment**: seven tools across 14,000+ organisms; over-representation (ORA) and, with clusterProfiler/fgsea
  on MSigDB, gene set enrichment (GSEA) of ranked lists; custom GMT sources, custom backgrounds, per-tool settings.
* **Variants and GWAS**: rsIDs, VCF lines, HGVS or GWAS summary statistics annotated with Ensembl VEP (consequence,
  impact, gnomAD frequency, ClinVar) and trait associations; Manhattan plot, variants → genes → traits Sankey, and
  gene lists or ranked gene scores for enrichment.
* **Results**: dot plot, bar chart, scatter, three heatmaps (term×gene, term×term, gene×gene), three networks,
  Sankey, chord and hive plots, and a searchable table; shared selection, undo/redo, and exports (PNG/SVG/CSV/HTML/PDF).
* **Comparison**: compare runs or lists with Fisher's combined p-value, agreement filters, overview, UpSet, chord,
  bubble plot and heatmap.
* **External viewers**: open result networks, comparisons, variant reports and STRING networks in Arena3D
  (arena3d.org, 3D multilayer networks) and result networks in NORMA.
* **Utilities**: ID conversion (g:Convert), orthologs (g:Orth), STRING protein interaction networks.
* **Display**: light/dark themes and chart palettes (Flame, colour-blind safe, vivid, pastel, grayscale).
* **Workspaces**: private bookmarkable links, session save/load, automatic expiry; REST API with a Python client.

## Architecture

```
R/            Flame R package: enrichment strategies and science core (no UI)
  core-run.R        run one enrichment from plain data
  core-views.R      data for the 10 result views
  core-compare.R    comparison of runs (Fisher's method)
  core-inputs.R     input parsing (IDs, volcano/2D tables, UpSet)
  core-utilities.R  ID conversion, orthologs, STRING network, custom GMT sources
api/          REST API (plumber): routes/, SQLite workspace store, background job workers (mirai)
web/          Web app (React + TypeScript + Vite; Plotly, vis-network)
inst/extdata/ organism tables and example data
tests/        testthat tests for the core
```

## Running locally

Requirements: R ≥ 4.4 and Node.js ≥ 20. If R packages are built from source (e.g. Homebrew R or Linux),
`WebGestaltR` needs a Rust toolchain (`brew install rust`) and `plumber` needs libsodium (`brew install libsodium`).
`install_packages.R` installs the CRAN packages and, through BiocManager, the Bioconductor packages `clusterProfiler`
and `fgsea` (used with `msigdbr` for MSigDB ORA and GSEA). MSigDB gene sets are downloaded on the first
clusterProfiler run for each organism and cached in memory, so that first run takes longer.

```bash
git clone https://github.com/PavlopoulosLab/Flame.git
cd Flame
Rscript install_packages.R
npm --prefix web install
npm --prefix web run build
Rscript api/run.R
```

Then open http://127.0.0.1:8000. The same server provides the web app and the API (`/api/...`, interactive
reference at `/__docs__/`).

For front-end development with hot reload, run `Rscript api/run.R` and `npm --prefix web run dev` (port 5173, proxies `/api`).

### Configuration (environment variables)

| Variable | Default | Purpose |
|---|---|---|
| `FLAME_PORT`, `FLAME_HOST` | `8000`, `127.0.0.1` | Listening address (use `0.0.0.0` in containers) |
| `FLAME_DB` | `api/data/flame.sqlite` | Workspace database |
| `FLAME_WORKERS` | `4` | Background enrichment workers |
| `FLAME_RETENTION_DAYS` | `30` | Delete workspaces after this many days without use |
| `FLAME_MAX_LISTS` | `10` | Lists per workspace (0 = unlimited) |
| `FLAME_PUBLIC_URL` | – | Public base URL (used in links and emails) |
| `FLAME_SMTP_URL`, `FLAME_SMTP_FROM` | – | Optional "results ready" emails |

### Deployment

Run the server behind an HTTPS reverse proxy (nginx, Caddy, …) on port 443 and forward to `FLAME_PORT`.
The server sets security headers and a strict Content-Security-Policy; the web app loads no third-party
scripts, fonts or cookies.

### Tests

```r
pkgload::load_all(); testthat::test_dir("tests/testthat")
```

## Publications

If you use Flame, please cite:

* Karatzas E., Baltoumas F.A., Aplakidou E., Kontou P.I., Stathopoulos P., Stefanis L., Bagos P.G., Pavlopoulos G.A. (2023)
  **Flame (v2.0): advanced integration and interpretation of functional enrichment results from multiple sources.**
  *Bioinformatics* 39(8):btad490. doi: [10.1093/bioinformatics/btad490](https://doi.org/10.1093/bioinformatics/btad490)
* Thanati F., Karatzas E., Baltoumas F.A., Stravopodis D.J., Eliopoulos A.G., Pavlopoulos G.A. (2021)
  **FLAME: a web tool for functional and literature enrichment analysis of multiple gene lists.**
  *Biology* 10(7):665. doi: [10.3390/biology10070665](https://doi.org/10.3390/biology10070665)

## License

This project is licensed under the **GNU General Public License v3.0 (GPL-3)** – see the [LICENSE](LICENSE) file for details.
