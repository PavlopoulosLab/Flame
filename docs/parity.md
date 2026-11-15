# Flame rewrite — parity tracker

## Status (2026-11-15)
Verified so far:
- API: 54 automated end-to-end checks pass (every route, live calls to all six tools, g:SNPense, EXTRACT, g:Convert, g:Orth, STRING network, session export/import, legacy `?url_genes=`).
- R core: 76 offline tests pass (views, filters, search, Fisher combination, run filters, input parsing, GMT ORA, link safety).
- Browser click-through: Home and every example link; all six input methods (paste, volcano with quick select, 2D plot with box selection, SNPs, text mining, UpSet); Convert IDs and the STRING network (Find orthologs via API); New run panel with per-tool settings; result views dot plot, bar chart, scatter, heatmap T×T, term–gene network and table; comparison overview, bubble plot and heatmap; all Help, API, How to cite, About and Privacy pages; phone width and dark mode.

Fixed during the click-through: modal footer overlapping content; WebGL-only plots (now SVG); drag-selection cleared by redraw; example lists identical (clock seed); UpSet example not preselecting lists; inverted heatmap colours; unreadable gene column; overlapping bubble labels; comparison run filter (R partial matching).

Still to check by hand: heatmap T×G and G×G, term–term and gene–gene networks, Arena3D export, Find orthologs dialog, session save/load dialog, custom GMT upload dialog.

Shiny app (v3) → plumber API + React/TypeScript web app. Tick an item only after it is verified in the running new app.
Legend: (finish) = shown but never wired in v3, implemented in the rewrite.

## Shell
- [ ] Workspace / Results / Comparison screens, Run Details panel
- [ ] Sidebar collapse (button, ⌘/Ctrl+B), section collapse
- [ ] Light/dark theme toggle
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
- [ ] Upload/Paste: paste, .txt/.csv/.tsv file, skip first row, unique/duplicate/invalid counts, example
- [ ] SNPs: rsID validation, gSNPense mapping table, output Symbol/ENSG, example
- [ ] Text mining: EXTRACT entities, table + annotated text, popups, select all/clear, links, example
- [ ] Volcano: parse, column mapping defaults, thresholds, plot, lasso/box, quick select (sig/up/down), clear, max 3000, example
- [ ] 2D reduction: mapping (ID, X, Y, colour, size), plot, selection, example
- [ ] UpSet: compatible-list check, intersection / distinct / union, chart, bar preview, create list

## Quick actions
- [ ] Convert IDs: target format, all/best mapping, preview table, create list
- [ ] Find Orthologs: target organism, all/best, preview, create list
- [ ] PPI network: STRING-supported organisms, 500-ID limit, full/physical, evidence/confidence, score, network view, legends, stats, open in STRING, add list, TSV, PNG

## Enrichment run form
- [ ] List picker (newest first), ORA (GSEA/Topology disabled)
- [ ] Tools: gProfiler, STRING, EnrichR, WebGestalt, PANTHER, GeneCodis; tool–organism compatibility
- [ ] Sources accordion with tool-support dots, select/clear, defaults GO:MF/CC/BP + KEGG
- [ ] Per-tool significance metric + threshold, background (genome / list), reset
- [ ] Async run with live status

## Results (per run)
- [ ] Tabs: dotplot, barchart, scatter, heatmap T×G/T×T/G×G, network T×G/T×T/G×G, table; defaults + "more", closable
- [ ] Filters: sources, search grammar, p max, term size, intersection, coverage; count; reset
- [ ] Toolbar: Top N (/ total), sort by, settings, Generate, Export, 3D
- [ ] Selection sidebar: details, genes, external link, remove, clear, export Copy/CSV/HTML/Print
- [ ] Table: column picker, row selection, export
- [ ] Dotplot: x-axis, labels, palette, lasso/click selection, PNG/SVG/PDF
- [ ] Barchart, Scatter: selection, PNG/SVG
- [ ] Heatmaps: orientation/labels, cell/pair selection, PNG/SVG
- [ ] Networks: layout setting (finish), similarity / shared-function cutoffs, node/edge selection, PNG/PDF (finish), Arena3D (finish)
- [ ] Run Details panel: parameters, sources, summary, no-hit inputs, mapping, copy

## Comparison
- [ ] Overview (D3): per-run and cross-run metrics, bubble/pin selection, label click (finish), PNG/SVG
- [ ] Bubble: top/sort/labels, selection with highlight (finish)
- [ ] Heatmap: −log10 p per run, selection with highlight (finish)
- [ ] Filters: runs any/all, consensus, source, search, combined p (active runs), term size
- [ ] Selection exports Copy/CSV/Excel/PDF/Print (finish)

## Public access and help
- [ ] No login or registration anywhere; optional email notification only
- [ ] Bookmarkable workspace link with job status; private, unguessable
- [ ] Privacy page, retention period, delete my data; no tracking/third-party cookies; all assets self-hosted
- [ ] Landing page: licence, Try an example, Help, API, How to cite
- [ ] Help: tutorial, per-feature pages with interpretation, FAQ, live sample output, sample input downloads
- [ ] How to cite: new paper + Karatzas et al. 2023 (Bioinformatics) + Thanati et al. 2021 (Biology)
- [ ] API page (all endpoints), Python template client, v2 `?url_genes=` and POST entry points
