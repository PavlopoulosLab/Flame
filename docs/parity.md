# Flame rewrite — parity tracker

## Status (2026-11-15)
Verified so far:
- API: 74 automated end-to-end checks pass (every route; live calls to all seven tools, g:SNPense, EXTRACT, g:Convert, g:Orth, STRING, Ensembl VEP, Arena3D and NORMA; GSEA on a ranked list; variant and GWAS analyses; session export/import; legacy `?url_genes=`).
- R core: 186 test expectations pass (incl. online GSEA and microRNA checks) (views, filters, search, Fisher combination, run filters, input parsing, ranked lists, variant and GWAS parsing, GMT ORA, Arena3D and NORMA payloads, pathway links and KEGG colouring, hit-ID rollback, session variant import, link safety), plus an online GSEA test (`FLAME_ONLINE_TESTS=true`).
- Browser click-through: Home and every example link; all six input methods; Convert IDs and the STRING network; New run panel with per-tool settings and the ORA/GSEA switch; result views dot plot, bar chart, scatter, heatmap T×T, term–gene network, table, Sankey, chord and hive; comparison overview, UpSet, chord, bubble plot and heatmap; variant report; palette switching; undo/redo; Arena3D button (opens www.arena3d.org); all Help, API, How to cite, About and Privacy pages; phone width and dark mode.

Fixed during the click-through: modal footer overlapping content; WebGL-only plots (now SVG); drag-selection cleared by redraw; example lists identical (clock seed); UpSet example not preselecting lists; inverted heatmap colours; unreadable gene column; overlapping bubble labels; comparison run filter (R partial matching); WebGestalt wrongly offered for GSEA.

Still to check by hand: heatmap T×G and G×G views, term–term and gene–gene networks in the browser, Find orthologs dialog, session save/load dialog, custom GMT upload dialog, workspace undo toasts.

Shiny app (v3) → plumber API + React/TypeScript web app. Tick an item only after it is verified in the running new app.
Legend: (finish) = shown but never wired in v3, implemented in the rewrite.

## Shell
- [x] Workspace / Results / Comparison screens, Run Details panel
- [ ] Sidebar collapse (button, ⌘/Ctrl+B), section collapse
- [x] Light/dark theme toggle
- [ ] Session menu: Save, Load, Clear all (finish)
- [ ] Help menu: tour, documentation, report issue, GitHub, About (finish)
- [ ] Limits: 10 lists (FLAME_MAX_LISTS), 3000 IDs per list, 1 MB text/volcano/reduction files
- [ ] Organism search (12,809 organisms, top 50), 13 common organisms
- [ ] 19 ID formats with examples

## Workspace
- [ ] Lists rail: count, + Add List, analyte tabs, rename (Enter/Esc), delete
- [ ] List export CSV (finish)
- [ ] Custom sources: GMT upload and use in runs (finish)
- [ ] Quick actions: Convert IDs, Find Orthologs, View PPI Network
- [ ] Filters: analyte, paradigm, list chips, tool chips
- [ ] Empty states: welcome, ready, no-match
- [ ] Run cards: loading/normal/error/no-results, details tooltip, close, multi-select (⌘A, Esc), open results
- [ ] Compare button (≥ 2 selected)
- [ ] Run de-duplication (exact reuse; datasource-only change → new run)

## Input methods
- [x] Upload/Paste: paste, .txt/.csv/.tsv file, skip first row, unique/duplicate/invalid counts, example
- [x] SNPs: rsID validation, gSNPense mapping table, output Symbol/ENSG, example
- [x] Text mining: EXTRACT entities, table + annotated text, popups, select all/clear, links, example
- [x] Volcano: parse, column mapping defaults, thresholds, plot, lasso/box, quick select (sig/up/down), clear, max 3000, example
- [x] 2D reduction: mapping (ID, X, Y, colour, size), plot, selection, example
- [x] UpSet: compatible-list check, intersection / distinct / union, chart, bar preview, create list

## Quick actions
- [x] Convert IDs: target format, all/best mapping, preview table, create list
- [ ] Find Orthologs: target organism, all/best, preview, create list
- [x] PPI network: STRING-supported organisms, 500-ID limit, full/physical, evidence/confidence, score, network view, legends, stats, open in STRING, add list, TSV, PNG

## Enrichment run form
- [x] List picker (newest first), ORA and GSEA (ranked lists; Topology not offered)
- [x] Tools: gProfiler, STRING, EnrichR, WebGestalt, PANTHER, GeneCodis (+ clusterProfiler); tool–organism compatibility
- [ ] Sources accordion with tool-support dots, select/clear, defaults GO:MF/CC/BP + KEGG
- [ ] Per-tool significance metric + threshold, background (genome / list), reset
- [x] Async run with live status

## Results (per run)
- [ ] Tabs: dotplot, barchart, scatter, heatmap T×G/T×T/G×G, network T×G/T×T/G×G, table; defaults + "more", closable
- [ ] Filters: sources, search grammar, p max, term size, intersection, coverage; count; reset
- [ ] Toolbar: Top N (/ total), sort by, settings, Generate, Export, 3D
- [ ] Selection sidebar: details, genes, external link, remove, clear, export Copy/CSV/HTML/Print
- [x] Table: column picker, row selection, export
- [x] Dotplot: x-axis, labels, palette, lasso/click selection, PNG/SVG/PDF
- [x] Barchart, Scatter: selection, PNG/SVG
- [ ] Heatmaps: orientation/labels, cell/pair selection, PNG/SVG
- [ ] Networks: layout setting (finish), similarity / shared-function cutoffs, node/edge selection, PNG/PDF (finish), Arena3D via arena3d.org (finish; verified for term–gene, term–term, gene–gene)
- [ ] Run Details panel: parameters, sources, summary, no-hit inputs, mapping, copy

## Comparison
- [x] Overview (D3): per-run and cross-run metrics, bubble/pin selection, label click (finish), PNG/SVG
- [x] Bubble: top/sort/labels, selection with highlight (finish)
- [x] Heatmap: −log10 p per run, selection with highlight (finish)
- [x] Filters: runs any/all, consensus, source, search, combined p (active runs), term size
- [ ] Selection exports Copy/CSV/Excel/PDF/Print (finish)

## Public access and help
- [x] No login or registration anywhere; optional email notification only
- [x] Bookmarkable workspace link with job status; private, unguessable
- [ ] Privacy page, retention period, delete my data; no tracking/third-party cookies; all assets self-hosted
- [x] Landing page: licence, Try an example, Help, API, How to cite
- [x] Help: tutorial, per-feature pages with interpretation, FAQ, live sample output, sample input downloads
- [x] How to cite: new paper + Karatzas et al. 2023 (Bioinformatics) + Thanati et al. 2021 (Biology)
- [x] API page (all endpoints), Python template client, v2 `?url_genes=` and POST entry points

## New in 3.0 (beyond parity)
- [x] clusterProfiler on MSigDB (ORA) for 20 organisms, incl. Hallmark, BioCarta, PID, cell types, ImmuneSigDB, oncogenic
- [x] Ranked lists (IDs with scores; whole volcano table) and GSEA with fgsea; NES axis and column
- [x] Variants and GWAS: rsIDs, VCF, chr:pos:ref:alt, HGVS, summary statistics; Ensembl VEP; traits; Manhattan plot; Sankey; gene lists and ranked gene scores
- [x] Extra views: Sankey, chord, hive; comparison UpSet and chord
- [x] Arena3D (arena3d.org) for result networks, comparisons, variant reports and STRING networks; NORMA for result networks
- [x] Chart palettes (Flame, colour-blind safe, vivid, pastel, grayscale) applied to every chart
- [x] Undo/redo on results and comparisons; undoable list/run deletion and Clear all in the workspace
- [x] Brand: logo with Flame 3.0 wordmark, gradient hero title ("Functional Analysis and Integration made easy")
- [x] Network style (vis-network): physics solvers, hierarchical layout, node shapes/sizing, edge curves/width/opacity, labels, interaction, clustering
- [x] Circos with hierarchical edge bundling: results (terms ↔ genes), comparison (runs), variants (genome)
- [x] Sessions: status bar (saved / changed / unsaved, running jobs, retention), selective save (parts, runs, display settings, gzip), load preview with add / replace / new workspace, Ctrl/⌘+S; variant analyses and ranked-list scores included
- [x] Genome browser links for variants and genes (Ensembl, UCSC, NCBI, dbSNP, gnomAD, ClinVar, GWAS Catalog, LitVar, GeneCards, GTEx, HPA, UniProt)
- [x] Pathway links: KEGG maps with the user's genes coloured (KEGG API), Reactome with genes flagged, WikiPathways, QuickGO/AmiGO/HPO, cross-database searches
- [x] STRING hits shown as the user's gene IDs (mapped back from STRING protein IDs, also for stored runs)
- [x] Comparison statistics: Jaccard / overlap heatmap, MDS, Spearman / Kendall, concordance scatter, concordance at the top, McNemar / Cochran's Q, Kruskal–Wallis, paired Wilcoxon, BH adjustment
- [x] Box / lasso selection on every chart (Plotly mode switch; vis-network, Circos and hive overlays); variant report charts select variants; STRING network selection saved as a list
- [x] Preloaded examples for every analysis option (GSEA, compare tools / strategies / lists, GMT, PPI, volcano, UMAP, expression, microRNAs, variants, GWAS); examples use recognisable gene names
- [x] Concept icons on main buttons and example cards; Arena3D / NORMA buttons with concept icons
- [x] UpSet plots: copy / download combinations
- [x] Expression-matrix input: co-expression clusters, gene PCA, two-group Welch t-test into the volcano selector
- [x] microRNA input: miRDB targets via MSigDB, targets shared by several miRNAs
- [x] Help: input file formats with downloadable examples and tab-preserving samples; API page grouped by area; Python CLI (enrich, gsea, variants, expression, mirna, session)
