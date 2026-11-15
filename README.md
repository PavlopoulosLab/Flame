<!-- Badges -->

[![Docker Pulls](https://img.shields.io/docker/pulls/pavlopouloslab/flame.svg)](https://hub.docker.com/r/pavlopouloslab/flame)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](LICENSE)

# Flame 3.0: Functional Analysis and Integration made easy

Flame runs functional and literature enrichment on one or many gene lists with eight established tools
(g:Profiler, STRING, Enrichr, WebGestalt, PANTHER, GeneCodis, Reactome, clusterProfiler) plus your own GMT gene sets,
combines their results, and lets you explore them with interactive plots and networks. It also starts from
variants and GWAS results. It is free and open to everyone.

## Features

* **Inputs**: paste/upload IDs (19 formats) or ranked lists with scores; select genes on a volcano plot (draggable
  thresholds), a 2D plot (UMAP/t-SNE/PCA) or an expression matrix (co-expression clusters, gene map, two-group test,
  pathway activity per sample); map SNPs to genes; find genes in text; microRNAs → miRDB targets; UpSet set
  operations; and **fetch gene lists** from a disease (Open Targets, DisGeNET, OMIM, Orphanet, ClinVar, GWAS Catalog),
  PubMed articles, GEO datasets (RummaGEO) or dbSNP.
* **Enrichment**: eight tools across 14,000+ organisms; over-representation (ORA) and, with clusterProfiler/fgsea on
  MSigDB, gene set enrichment (GSEA) of ranked lists; literature enrichment (PubMed, GeneSigDB, Rummagene); cell types;
  custom GMT sources, custom backgrounds, per-tool settings.
* **Beyond term lists**: transcription factors (ChEA3) and kinases (KEA3), drug and CRISPR signatures that mimic or
  reverse a response (LINCS L1000), metabolite and lipid pathways (KEGG), STRING network modules with module enrichment,
  and robustness checks (bootstrap stability, random-list control, FDR thresholds).
* **Results**: table, dot plot, bar chart, scatter, heatmaps, networks, Sankey, chord, hive and Circos (edge bundling)
  views; functional **themes** and word clouds; a selection panel with ranked, clickable genes, GeneCards and genome
  browser links, and KEGG maps with the user's genes coloured; undo/redo; PNG/SVG/CSV/HTML/PDF exports.
* **Comparison**: compare tools, lists or strategies with Fisher's combined p-value, a **consensus rank** (Robust Rank
  Aggregation) and a rank chart, a table of which run found what, agreement statistics (Jaccard, rank correlation,
  McNemar / Cochran's Q, concordance at the top, MDS), UpSet, chord, Circos, bubble plot and heatmap.
* **Variants and GWAS**: rsIDs, VCF lines, HGVS or GWAS summary statistics annotated with Ensembl VEP (consequence,
  impact, gnomAD frequency, ClinVar) and trait associations; Manhattan and genome Circos plots; genes → enrichment.
* **Networks**: STRING networks as multi-edge evidence networks; open networks, comparisons and variant reports in
  Arena3D (3D multilayer networks) or NORMA, or directly in STRING.
* **Display**: light/dark themes; every visualization has its own palette (Flame, colour-blind safe, vivid, pastel,
  grayscale); box and lasso selection on every chart; works at phone width.
* **Workspaces**: private bookmarkable links, session save/load (lists, runs, analyses), automatic expiry.
* **Programmatic access**: versioned REST API (`/api/v1`) with an OpenAPI 3 document and Swagger UI, and a Python
  command-line client (`web/public/flame_client.py`).

## Architecture

```
R/                 Flame R package: enrichment strategies and science core (no UI)
  enrich-strategy-*.R  one strategy per tool (g:Profiler, STRING, Enrichr, WebGestalt, PANTHER, GeneCodis,
                       clusterProfiler, Reactome)
  core-run.R           run one enrichment from plain data
  core-views.R         data for the result views
  core-compare.R       comparison of runs (Fisher's method, statistics)
  core-themes.R        functional themes and word clouds
  core-analyses.R      drug signatures, network modules, robustness, pathway activity, metabolites
  core-regulators.R    transcription factors and kinases (ChEA3, KEA3)
  core-fetch.R         gene lists from Open Targets, NCBI (PubMed, dbSNP), RummaGEO, GWAS Catalog
  core-inputs.R        input parsing (IDs, volcano/2D tables, expression, microRNAs, UpSet) and examples
  core-variants.R      variant and GWAS annotation
api/               REST API (plumber): routes/, openapi.R, SQLite workspace store, background workers (mirai)
web/               Web app (React + TypeScript + Vite; Plotly, vis-network, D3)
  e2e/                 browser smoke tests (Playwright)
inst/extdata/      organism tables and example data from published studies
data-raw/          script that rebuilds the example data
tests/testthat/    R tests; tests/api/ end-to-end API tests
docs/parity.md     feature checklist and what is verified
Dockerfile, docker-compose.yml
```

## Running locally

Requirements: R ≥ 4.4 and Node.js ≥ 20. If R packages are built from source (e.g. Homebrew R or Linux),
`WebGestaltR` needs a Rust toolchain (`brew install rust`) and `plumber` needs libsodium (`brew install libsodium`).
`install_packages.R` installs the CRAN packages and, through BiocManager, the Bioconductor packages `clusterProfiler`,
`fgsea` (used with `msigdbr` for MSigDB ORA and GSEA) and `progeny` (pathway activity). MSigDB gene sets are downloaded on the first
clusterProfiler run for each organism and cached in memory, so that first run takes longer.

```bash
git clone https://github.com/PavlopoulosLab/Flame.git
cd Flame
Rscript install_packages.R
npm --prefix web install
npm --prefix web run build
Rscript api/run.R
```

Then open http://127.0.0.1:8000. The same server provides the web app and the versioned REST API (`/api/v1/...`; interactive
reference at `/__docs__/`, OpenAPI 3 specification at `/openapi.json`).

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
| `FLAME_WORKSPACE_LIMIT` | `30` | New workspaces per hour per client address (0 = no limit) |
| `FLAME_RATE_LIMIT` | `600` | API requests per minute per client address (0 = no limit, e.g. for local testing) |
| `FLAME_NCBI_API_KEY` | – | NCBI E-utilities key for the PubMed and dbSNP inputs (10 instead of 3 requests per second; recommended for public servers) |

### Docker

The `Dockerfile` builds one image with the web app and the REST API (Node builds the web app, then an
R 4.6 image from rocker installs the R packages and runs the server as an unprivileged user).

```bash
docker build -t pavlopouloslab/flame:3.0 .
docker run -d --name flame -p 8000:8000 -v flame-data:/data pavlopouloslab/flame:3.0
```

Or with Compose (same settings, restarts automatically):

```bash
docker compose up -d
```

Open http://localhost:8000. Workspaces are stored in the `flame-data` volume (`/data/flame.sqlite`), so they
survive container upgrades. Set the variables in the table above with `-e` (or in `docker-compose.yml`);
`FLAME_HOST` is already `0.0.0.0` inside the image. The container reports its health from `/api/v1/health`.
Use `--build-arg R_VERSION=…` to build on another R release. The first build takes a while (Bioconductor
packages); later builds reuse the cached layers unless `install_packages.R` changes.

### Deployment

Run the server behind an HTTPS reverse proxy (nginx, Caddy, …) on port 443 and forward to `FLAME_PORT`.
The server sets security headers and a strict Content-Security-Policy; the web app loads no third-party
scripts, fonts or cookies.

### Tests

```bash
# R core (add FLAME_ONLINE_TESTS=true to include the tests that call external services)
Rscript -e 'pkgload::load_all(); testthat::test_dir("tests/testthat")'

# Start a test server without rate limits, then run the REST API and browser tests against it
FLAME_PORT=8014 FLAME_RATE_LIMIT=0 FLAME_WORKSPACE_LIMIT=0 Rscript api/run.R &
python3 tests/api/api_e2e.py http://127.0.0.1:8014/api/v1      # ~110 checks, live external services
npm --prefix web test                                           # web unit tests
cd web && npx playwright install && FLAME_URL=http://127.0.0.1:8014 npx playwright test   # Chromium, Firefox, WebKit
```

`docs/parity.md` lists every feature and how it was verified.

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
