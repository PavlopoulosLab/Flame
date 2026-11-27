<!-- Badges -->

[![Docker Pulls](https://img.shields.io/docker/pulls/pavlopouloslab/flame.svg)](https://hub.docker.com/r/pavlopouloslab/flame)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](LICENSE)

<p align="center"><img src="docs/flame-logo.png" alt="Flame logo" width="160"></p>

# Flame 3.0: The bioscientist’s everyday toolkit

Flame takes you from data to interpretation in one workspace. Start from expression data, TCGA tumours, survival
tables, variants and GWAS results, microRNAs, nutrients and foods, PDF documents, bench measurements or public
resources, and turn them into gene lists. Analyse the lists with eight established enrichment tools (g:Profiler,
STRING, Enrichr, WebGestalt, PANTHER, GeneCodis, Reactome, clusterProfiler) plus your own GMT gene sets, ask what
regulates them and which drugs mimic or reverse them, combine and compare the results, and export
publication-ready figures. It is free and open to everyone, and everything is also available through a REST API.

The workspace has four tabs: **Input** (every way to add a gene list), **Enrichment** (runs and comparisons),
**Regulators & drugs** (what controls a list, protein network) and **Gene tools** (inspect and transform a list).

## Features

* **Inputs**: paste/upload IDs (19 formats) or ranked lists with scores; select genes on a volcano plot (preview of
  the table, draggable thresholds; save the up- and down-regulated genes as two linked lists and go straight to their
  GO enrichment), a 2D plot (UMAP/t-SNE/PCA) or an expression matrix (co-expression clusters, gene map, two-group test,
  correlation with a gene of interest by Pearson, Spearman and Kendall, boxplots of 2–3 sample groups with tests,
  pathway activity per sample, survival analysis with Kaplan–Meier curves and a univariate Cox model per gene with
  optional strata); map SNPs
  to genes; find genes, chemicals, diseases and seven other entity types in text or in **PDF documents** (EXTRACT;
  the PDFs stay in the browser, several open in tabs, and an annotated PDF can be downloaded); microRNAs → miRDB
  targets; **TCGA** tumours through cBioPortal (most mutated, amplified or deleted genes, genes co-expressed with a
  gene, genes that differ between two groups of tumours, genes higher or lower in tumours than in adjacent normal
  tissue for 16 cancer types, or your own MAF file); **nutrients and foods** (ChEMBL
  targets, UniProt proteins through ChEBI, GWAS Catalog diet and nutrient traits; CTD when it accepts automated
  queries); **Bench Data**: analysis of small lab experiments (qPCR Ct, ELISA, western blot band intensities, viability,
  reporter assays, flow cytometry percentages or any values per group, with an automatic test, Dunnett or Tukey post hoc tests, fold changes with confidence intervals and
  publication-style figures in colour or black and white, with axis breaks); UpSet set
  operations (intersection, union, XOR and more, with selectable bars); and **fetch gene lists** from a disease
  (choose among Open Targets, DisGeNET, OMIM, Orphanet, ClinVar, Jensen DISEASES and GWAS Catalog; merge several
  results), PubMed articles, GEO datasets (RummaGEO) or ClinVar (by gene, condition, rsID or accession); or start
  from variants or GWAS results and save the affected genes. Every selection is listed before it is saved, and
  saving returns to the Input tab.
* **Lists**: always in view; click a list to see its contents with gene annotations (symbol, Ensembl ID,
  description), edit it, or tick several lists to compare them (UpSet) or run the same enrichment on all of them.
* **Gene tools**: annotate a list (current symbol, name, gene type, chromosome location, Ensembl and Entrez IDs;
  Excel/TSV export), check and clean it (unrecognised IDs, outdated symbols, duplicates), filter it (by gene type or
  chromosome, drop placeholder names), convert IDs and find orthologs. Annotation uses MyGene.info.
* **Enrichment**: eight tools across 14,000+ organisms; over-representation (ORA) and, with clusterProfiler/fgsea,
  gene set enrichment (GSEA) of ranked lists on every MSigDB collection (H, C1–C9, grouped as on the MSigDB website)
  with the classic GSEA enrichment plot; literature enrichment (PubMed, GeneSigDB, Rummagene); cell types; your own
  pathway sets or a pasted signature (GMT, uploaded in the New run panel; ORA, or GSEA with a ranked list), custom
  backgrounds (one of your lists, chosen per tool), per-tool settings. Up to 20 runs per workspace can be queued or
  running at once.
* **Up- and down-regulated genes**: a dedicated step runs GO enrichment (and optionally KEGG, Reactome,
  WikiPathways) of both halves of a volcano selection with several tools at once; the results show them in one
  figure (up-regulated terms to the right, terms enriched in both on both sides, down-regulated terms to the left)
  or as two panels.
* **Beyond term lists**: transcription factors (ChEA3) and kinases (KEA3), drug and CRISPR signatures that mimic or
  reverse a response (LINCS L1000), the STRING protein network with module detection and per-module enrichment,
  and robustness checks (bootstrap stability, random-list control, FDR thresholds).
* **Redundancy reduction**: one representative term per group of similar terms, by shared genes (any source), by GO
  semantic similarity (Wang measure, as in GOSemSim) or through the REVIGO web service.
* **Results**: all annotation sources together or one tab per source (GO:BP, GO:CC, KEGG…); table, dot plot, bar chart
  (coloured by source, significance or one colour), scatter, heatmaps, networks, Sankey, chord, hive and Circos (edge
  bundling) views; GSEA enrichment plots; functional **themes** and word clouds; a selection panel with ranked, clickable genes, GeneCards and genome
  browser links, and KEGG maps with the user's genes coloured; undo/redo; chart text styling (font, titles, axis titles, tick labels, legend; *Aa Text*); image export of every
  figure (PNG, SVG, PDF, JPEG, WebP) and CSV/HTML exports.
* **Comparison**: compare tools, lists or strategies with Fisher's combined p-value, a **consensus rank** (Robust Rank
  Aggregation), a **Top terms** figure (terms most runs agree on, similar terms merged, optionally one panel per
  annotation source) and a rank chart, a table of which run found what, agreement statistics (Jaccard, rank correlation,
  McNemar / Cochran's Q, concordance at the top, MDS), UpSet, chord, Circos, bubble plot and heatmap.
* **Variants and GWAS** (an input option): rsIDs, VCF lines, HGVS or GWAS summary statistics annotated with Ensembl
  VEP (consequence, impact, gnomAD frequency, ClinVar) and trait associations; Manhattan and genome Circos plots;
  genes → a list → enrichment.
* **Networks**: STRING networks as multi-edge evidence networks; open networks, comparisons and variant reports in
  Arena3D (3D multilayer networks) or NORMA, or directly in STRING.
* **Display**: light/dark themes; every visualization has its own palette (Flame, colour-blind safe, vivid, pastel,
  grayscale, or your own colours), offered only where there is a chart or network; box and lasso selection on charts; progress bars on
  slow steps; works at phone width.
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
  core-analyses.R      drug signatures, network modules, robustness, pathway activity
  core-utilities.R     gene tools: annotation (MyGene.info), ID conversion, orthologs, STRING network, custom GMT
  core-regulators.R    transcription factors and kinases (ChEA3, KEA3)
  core-fetch.R         gene lists from Open Targets, NCBI (PubMed, ClinVar, dbSNP), RummaGEO, GWAS Catalog
  core-inputs.R        input parsing (IDs, volcano/2D tables, microRNAs, UpSet) and examples
  core-expression.R    expression matrices: clusters, gene map, two-group test, correlation, boxplots
  core-survival.R      survival tables and a univariate Cox model per gene
  core-tcga.R          TCGA through cBioPortal (mutations, copy number, co-expression, two-group comparison)
  core-nutrition.R     nutrients and foods (ChEMBL, UniProt/ChEBI, GWAS traits, CTD)
  core-bench.R         bench data (qPCR, western blot, flow cytometry): helpers and tests
  core-redundancy.R    redundancy reduction (shared genes, Wang GO similarity, REVIGO)
  func-extract.R       EXTRACT tagger for text and PDF documents
  core-variants.R      variant and GWAS annotation
api/               REST API (plumber): routes/, openapi.R, SQLite workspace store, background workers (mirai)
web/               Web app (React + TypeScript + Vite; Plotly, vis-network, D3)
  src/pages/           Home, Workspace (Input, Enrichment, Regulators & drugs, Gene tools tabs),
                       Results, Compare, Variants, Help
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
`fgsea` (used with `msigdbr` for MSigDB ORA and GSEA), `progeny` (pathway activity) and the `org.*.db` annotation packages
of 13 organisms (about 1.9 GB; current GO for `enrichGO` / `gseGO`). MSigDB gene sets are downloaded on the first
clusterProfiler run for each organism and cached in memory, so that first run takes longer. KEGG pathways (`enrichKEGG` /
`gseKEGG`) are downloaded from the KEGG REST API by clusterProfiler and kept in `FLAME_CACHE_DIR` for a week; if KEGG
cannot be reached, the last saved copy is used. KEGG is free for academic use; see kegg.jp for commercial licensing.

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
| `FLAME_FETCH_WORKERS` | `2` | Workers for requests to external services (TCGA, nutrients, diseases, EXTRACT, REVIGO, Arena3D, NORMA); at most four times as many such requests wait at once |
| `FLAME_MAX_ACTIVE_JOBS` | `20` | Enrichment runs and analyses a workspace may have queued or running at once; `FLAME_WORKERS` of them run at the same time, the rest wait their turn |
| `FLAME_RETENTION_DAYS` | `30` | Delete workspaces after this many days without use |
| `FLAME_CACHE_DIR` | R's user cache dir | Where downloaded KEGG pathways and the KEGG/Reactome name lists are kept |
| `FLAME_KEGG_CACHE_DAYS` | `7` | Days before KEGG pathways (and the name lists) are downloaded again |
| `FLAME_MAX_LISTS` | `10` | Lists per workspace (0 = unlimited) |
| `FLAME_PUBLIC_URL` | – | Public base URL (used in links and emails) |
| `FLAME_SMTP_URL`, `FLAME_SMTP_FROM` | – | Optional "results ready" emails |
| `FLAME_WORKSPACE_LIMIT` | `30` | New workspaces per hour per client address (0 = no limit) |
| `FLAME_RATE_LIMIT` | `600` | API requests per minute per client address (0 = no limit, e.g. for local testing) |
| `FLAME_TRUST_PROXY` | – | `1` to take the client address from `X-Forwarded-For` when the reverse proxy runs on another machine |
| `FLAME_MAX_EXPRESSION_MB`, `FLAME_MAX_SESSION_MB` | `20`, `25` | Largest expression table and session file accepted |
| `FLAME_MAX_IDS`, `FLAME_MAX_RANKED_IDS` | `20000`, `60000` | Most IDs in a list, and in a list with scores (ranked list, for GSEA) |
| `FLAME_NCBI_API_KEY` | – | NCBI E-utilities key for the PubMed and ClinVar inputs (10 instead of 3 requests per second; recommended for public servers) |
| `FLAME_HTTP_PORT` | `8000` | Compose only: host port mapped to `FLAME_PORT` inside the container |

### Docker Hub

<!-- Maintainer note: when releasing a new version, also tag and push `latest`
     (docker tag pavlopouloslab/flame:X.Y pavlopouloslab/flame:latest && docker push pavlopouloslab/flame:latest),
     and bump the "3.0" tag used below, in README_DOCKERHUB.md, and in the Dockerfile's image version LABEL. -->

Ready-to-use images are published at [hub.docker.com/r/pavlopouloslab/flame](https://hub.docker.com/r/pavlopouloslab/flame) — no build step needed:

```bash
docker pull pavlopouloslab/flame:3.0
docker run -d --name flame -p 8000:8000 -v flame-data:/data pavlopouloslab/flame:3.0
```

Or with Compose:

```yaml
services:
  flame:
    image: pavlopouloslab/flame:3.0
    ports:
      - "8000:8000"
    volumes:
      - flame-data:/data
    restart: unless-stopped
volumes:
  flame-data:
```

Open http://localhost:8000. Set any variable from the table above with `-e` (or `environment:` in Compose);
`FLAME_HOST` is already `0.0.0.0` inside the image. Pin a version tag (e.g. `3.0`) for reproducible deployments.
This published image is built for the main path (`FLAME_BASE_PATH=/`); build from source (below) if you need a
subpath deployment or another `R_VERSION`.

### Docker (build from source)

The `Dockerfile` builds one image with the web app and the REST API (Node builds the web app, then an
R 4.6 image from rocker installs the R packages and runs the server as an unprivileged user).

```bash
docker build -t pavlopouloslab/flame:3.0 .
docker run -d --name flame -p 8000:8000 -v flame-data:/data pavlopouloslab/flame:3.0
```

Or with Compose (same settings, restarts automatically):

```bash
cp .env.example .env   # edit .env to override any variable from the table above
docker compose up -d
```

`docker-compose.yml` reads every variable from `.env` (ignored by git) as `${VAR:-default}`, so anything left
commented out in `.env`, or a missing `.env` file altogether, falls back to the defaults in the table above.

Open http://localhost:8000 (or `FLAME_HTTP_PORT` from `.env`). Workspaces are stored in the `flame-data` volume
(`/data/flame.sqlite`), so they survive container upgrades. Outside Compose, set the variables in the table
above with `-e`; `FLAME_HOST` is already `0.0.0.0` inside the image. The container reports its health from
`/api/v1/health`. Use `--build-arg R_VERSION=…` to build on another R release, or `--build-arg FLAME_BASE_PATH=…`
to serve the app under a subpath. The first build takes a while (Bioconductor packages); later builds reuse the
cached layers unless `install_packages.R` changes.

### Deployment

Run the server behind an HTTPS reverse proxy (nginx, Caddy, …) on port 443 and forward to `FLAME_PORT`.
The server sets security headers and a strict Content-Security-Policy; the web app loads no third-party
scripts, fonts or cookies.

- Request bodies larger than the biggest allowed upload (session file or expression table, ~25 MB by
  default), and bodies without a `Content-Length` (chunked), are refused before they are read. Set the
  proxy's limit to match, e.g. `client_max_body_size 26m;` in nginx.
- The per-client rate limit (`FLAME_RATE_LIMIT` requests per minute, default 600) uses the connection
  address. `X-Forwarded-For` is used only when the request comes from a proxy on the same machine, or
  when `FLAME_TRUST_PROXY=1`, and then only its last entry, the one the proxy added
  (`proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;`).
- Add `Strict-Transport-Security` at the proxy.

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
