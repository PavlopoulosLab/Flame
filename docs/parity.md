# Flame 3.0 — feature checklist

## Status (2026-11-15)

Tick an item only after it was verified in the running app (browser) or by an automated test.
`(API)` = verified through the REST API only; the screen itself still needs a click-through.

Automated checks:
- **REST API** (`tests/api/api_e2e.py`): 109 end-to-end checks on `/api/v1`, with live calls to all eight
  enrichment tools and the external services (g:SNPense, EXTRACT, g:Convert, g:Orth, STRING, Ensembl VEP,
  Arena3D, NORMA, ChEA3, KEA3, Enrichr/LINCS, KEGG, Open Targets, NCBI, RummaGEO, GWAS Catalog); GSEA;
  variants and GWAS; background analyses; sessions; the OpenAPI document; the v2 `?url_genes=` import.
- **R core** (`tests/testthat`): 50 test blocks, including themes, rank aggregation inputs, fast ORA, ssGSEA,
  Enrichr term parsing and (with `FLAME_ONLINE_TESTS=true`) ChEA3, LINCS, KEGG metabolites and Reactome.
- **Browser** (`web/e2e`, Playwright): 33 smoke tests on Chromium, Firefox and WebKit — Home, adding a list,
  workspace sections, volcano example with its own palette, Help/API/About, Swagger, dark theme, phone width
  (Home, Help, About, workspace); plus an online test (`FLAME_ONLINE=1`) running the worked example.
- **Web units** (`npm test`): rank aggregation (Robust Rank Aggregation).

Still to click through by hand: heatmap T×G and G×G, term–term and gene–gene networks, Find orthologs dialog,
custom GMT upload dialog, workspace undo toasts, the disease / PubMed / GEO / ClinVar dialogs at phone width.

## Shell
- [x] Workspace / Results / Comparison screens, Run Details panel
- [x] Light/dark theme toggle
- [x] Section tabs with colours: Enrichment, Regulators & drugs, Variants & GWAS, Gene tools
- [x] Session: Save, Load, Clear all, status pill (API: export/import/clear; dialog checked earlier)
- [x] Help, API, How to cite, About and Privacy pages; links in the footer
- [x] Limits: lists per workspace (FLAME_MAX_LISTS), request size, rate limit (FLAME_RATE_LIMIT), new workspaces per hour (FLAME_WORKSPACE_LIMIT) (API)
- [x] Organism search and tool support per organism (API)
- [ ] Sidebar collapse (not carried over: the rail is part of the page layout)

## Workspace
- [x] Lists rail: count, + Add list, rename, delete, CSV export (API for rename/CSV)
- [x] Custom gene sets: GMT upload and use in runs (API)
- [x] Run cards: status, progress bar, errors, results count, remove with undo, multi-select, Compare
- [x] Run de-duplication (exact reuse; datasource-only change → new run) (API)
- [x] Jobs window with live progress for runs, variant analyses and background analyses

## Input methods
- [x] Paste / upload IDs (with scores for ranked lists)
- [x] SNPs (g:SNPense), text mining (EXTRACT)
- [x] Volcano plot (draggable thresholds, −log10 p and p axes), 2D plot (UMAP / t-SNE / PCA)
- [x] Expression matrix: clusters, gene map, two-group test, pathway activity per sample (ssGSEA, z-score, PROGENy)
- [x] microRNAs → miRDB targets
- [x] Combine lists (UpSet; copy and download combinations)
- [x] From a disease: Open Targets (scored, ranked), DisGeNET, OMIM, Orphanet, ClinVar, Jensen DISEASES, GWAS Catalog genes and variants (API; dialog checked in the browser)
- [x] From PubMed articles (NCBI gene2pubmed), from a GEO dataset (RummaGEO), from ClinVar (the dbSNP lookup remains in the API)

## Gene tools
- [x] Convert IDs (g:Convert)
- [ ] Find orthologs dialog (API verified)
- [x] STRING network: multi-edge evidence view (one edge per channel, toggles), confidence view, modules, selection → list, TSV/PNG, Arena3D, Open in STRING (also before building)

## Enrichment runs
- [x] Tools: g:Profiler, STRING, Enrichr, WebGestalt, PANTHER, GeneCodis, clusterProfiler, Reactome; tool–organism compatibility
- [x] ORA and GSEA (ranked lists); sources by category, presets (defaults, cell types, literature)
- [x] Per-tool significance metric, threshold and background (API)
- [x] Literature sources: PubMed (STRING), GeneSigDB, Rummagene, with links to the articles

## Results (per run)
- [x] Table first, dot plot, bar chart, scatter, Sankey, chord, hive, Circos, themes and word cloud
- [ ] Heatmaps T×G / G×G and term–term / gene–gene networks (T×T and term–gene verified)
- [x] Filters, search grammar, Top N, sort, undo/redo, exports (PNG/SVG/PDF/JPEG/WebP images with resolution, background and legend options; CSV/HTML/print)
- [x] Selection panel with ranked, clickable genes (GeneCards links), pathway links (KEGG colouring), copy buttons
- [x] Arena3D and NORMA buttons; each view has its own colour palette

## Comparison
- [x] Table with per-run p-values or ranks, consensus rank (Robust Rank Aggregation), agreement, combined p
- [x] Ranks (bump chart), themes, overview, statistics, UpSet, chord, Circos, bubble, heatmap
- [x] Filters: runs any/all, sources, search, combined p, term size

## Regulators, drugs and more
- [x] Transcription factors (ChEA3), kinases (KEA3), drug and CRISPR signatures (LINCS L1000)
- [x] Cell types (PanglaoDB, CellMarker, Azimuth, Tabula Sapiens, MSigDB C8), metabolite pathways (KEGG)
- [x] Network modules (STRING + Louvain + g:Profiler), robustness (bootstrap, random lists, FDR thresholds)
- [x] Stored with the workspace (survive restarts), included in session files, removable (API)

## Variants and GWAS
- [x] rsIDs, VCF, chr:pos:ref:alt, HGVS, summary statistics; Ensembl VEP; traits; Manhattan, Circos; genes → enrichment

## Public access, API and deployment
- [x] Bookmarkable private workspace link; retention and delete-my-data; no tracking or third-party cookies; self-hosted assets
- [x] Landing page with licence, examples (one per analysis), Help, API, How to cite
- [x] How to cite: previous Flame papers and the latest publication of every resource used (Crossref-checked DOIs)
- [x] REST API versioned under `/api/v1`; OpenAPI 3 document (`/openapi.json`) and Swagger UI with Authorize and examples
- [x] Python client: enrich, gsea, variants, expression, mirna, analysis, fetch, session
- [x] Docker image definition and Compose file (image build still to run on a machine with Docker)
