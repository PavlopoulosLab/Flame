<!-- Badges -->

[![Docker Pulls](https://img.shields.io/docker/pulls/pavlopouloslab/flame.svg)](https://hub.docker.com/r/pavlopouloslab/flame)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](LICENSE)

# Flame 3.0: Functional Enrichment Integration made easy

Flame runs functional and literature enrichment on one or many gene lists with six established tools
(g:Profiler, STRING, Enrichr, WebGestalt, PANTHER, GeneCodis) plus your own GMT gene sets, combines
their results, and lets you explore them with interactive plots and networks. It is free and open to
everyone: no login, no registration.

## Features

* **Inputs**: paste/upload IDs (19 formats), select genes on a volcano or 2D-reduction (UMAP/t-SNE/PCA) plot,
  map SNPs to genes, find genes in free text, and combine lists with UpSet set operations.
* **Enrichment**: six tools across 14,000+ organisms, custom GMT sources, custom backgrounds, per-tool significance settings.
* **Results**: dot plot, bar chart, scatter, three heatmaps (term×gene, term×term, gene×gene), three networks
  (with layouts and Arena3Dweb 3D export) and a searchable table; shared selection and exports (PNG/SVG/CSV/HTML/PDF).
* **Comparison**: compare runs or lists with Fisher's combined p-value, agreement filters, overview, bubble plot and heatmap.
* **Utilities**: ID conversion (g:Convert), orthologs (g:Orth), STRING protein interaction networks.
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
