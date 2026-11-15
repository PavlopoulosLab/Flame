import { keepPreviousData, useQuery } from '@tanstack/react-query'
import { useEffect, useMemo, useRef, useState } from 'react'
import { Link, useNavigate, useParams } from 'react-router-dom'
import { api, getWorkspaceId, saveText } from '../api/client'
import { useConfig, useWorkspace } from '../api/hooks'
import type { ResultFilters, RunDetails, Term, ViewData } from '../api/types'
import type { NetworkHandle, PlotHandle } from '../components/charts'
import { CopyButton, ErrorNote, Field, LinkMenu, Modal, Note, OpenExternalButton, Spinner } from '../components/ui'
import { geneLinks } from '../lib/links'
import { ToolLogo } from '../components/ToolLogo'
import { ThemesView } from '../features/themes/Themes'
import { Arena3DIcon, NormaIcon } from '../components/ExternalIcons'
import { UndoRedoButtons } from '../components/Undo'
import { useHistory } from '../lib/history'
import { useNetworkStyle } from '../lib/networkStyle'
import { LayoutSelect, NetworkOptions } from '../components/NetworkOptions'
import { BarchartView, ChordView, CircosView, DotplotView, HeatmapView, HiveView, NetworkView, SankeyView, ScatterView, TABLE_COLUMNS, TableView, type ViewSettings } from '../features/results/views'
import { downloadSvg } from '../components/viz'
import { PaletteSelect } from '../components/PaletteSelect'
import { PaletteScope, usePaletteKey } from '../lib/palette'
import { formatNum, formatP, isHttpUrl, plural, slug, splitGenes, toCsv, toHtmlTable } from '../lib/format'

const LAYOUT_LABELS: Record<string, string> = {
  force: 'Force-directed (igraph)', graphopt: 'GraphOpt', kk: 'Kamada–Kawai', mds: 'Multidimensional scaling', tree: 'Tree',
  star: 'Star', circle: 'Circle', grid: 'Grid', random: 'Random', hierarchical: 'Hierarchical (igraph tree)',
}

const VIEW_HELP: Record<string, string> = {
  dotplot: 'Each dot is a term: position = gene ratio (or the chosen metric), size = number of your genes in the term, colour = significance.',
  barchart: 'Most significant terms, coloured by annotation source.',
  scatter: 'Significance (x) against the share of the term covered by your genes (y).',
  'heatmap-tg': 'Which of your genes belong to which terms.',
  'heatmap-tt': 'How much terms overlap: shared genes / all genes of the two terms.',
  'heatmap-gg': 'How many of the shown terms each pair of genes shares.',
  'network-tg': 'Terms connected to the genes they contain.',
  'network-tt': 'Terms connected when they share genes (edge = similarity %).',
  'network-gg': 'Genes connected when they share terms.',
  table: 'All terms with full statistics. Click a row to select it; click a header to sort.',
  themes: 'Overlapping terms grouped into functional themes by the genes they share; the word cloud summarises all terms (bigger = more significant). Click words or themes to select their terms.',
  sankey: 'Flows from your genes (left) through the terms they belong to (middle) to annotation sources (right). Wide flows = genes shared by many terms.',
  chord: 'Each arc is a term; ribbons join terms that share genes (ribbon width = number of shared genes). Click an arc or ribbon to select.',
  circos: 'Terms are grouped by annotation source around the circle, with your genes in their own sector. Links join terms to their genes and are bundled through each sector, so shared biology shows as thick bundles. Outer bars: −log10 p for terms, share of terms for genes. Hover a term or gene to trace its links.',
  hive: 'Genes on one axis, terms on one axis per source, ordered by rank (genes by how many terms contain them). Links connect genes to their terms.',
}

// Extra views built in the browser from existing data (server view in `from`).
const EXTRA_TABS = [
  { tabId: 'themes', label: 'Themes & word cloud', icon: '', defaultVisible: true, hasArena3D: false, tabType: 'custom', from: 'table' },
  { tabId: 'sankey', label: 'Sankey (genes → terms)', icon: '', defaultVisible: false, hasArena3D: false, tabType: 'plotly', from: 'heatmap-tg' },
  { tabId: 'chord', label: 'Chord (term overlap)', icon: '', defaultVisible: false, hasArena3D: false, tabType: 'svg', from: 'heatmap-tt' },
  { tabId: 'hive', label: 'Hive plot', icon: '', defaultVisible: false, hasArena3D: false, tabType: 'svg', from: 'heatmap-tg' },
  { tabId: 'circos', label: 'Circos (bundled)', icon: '', defaultVisible: false, hasArena3D: false, tabType: 'svg', from: 'heatmap-tg' },
]

const SORTS = [
  { value: '-log10Pvalue', label: 'Significance' },
  { value: 'Term Coverage', label: 'Term coverage' },
  { value: 'Gene Ratio', label: 'Gene ratio' },
  { value: 'Fold Enrichment', label: 'Fold enrichment' },
  { value: 'NES', label: '|NES| (GSEA)' },
]

function useDebounced<T>(value: T, ms = 300) {
  const [v, setV] = useState(value)
  useEffect(() => {
    const h = setTimeout(() => setV(value), ms)
    return () => clearTimeout(h)
  }, [value, ms])
  return v
}

export default function ResultsPage() {
  return <PaletteScope id="results"><ResultsPageBody /></PaletteScope>
}

function ResultsPageBody() {
  const ws = getWorkspaceId()!
  const { runId = '', tab } = useParams()
  const navigate = useNavigate()
  const { data: config } = useConfig()
  const { data: summary } = useWorkspace(ws)
  const run = summary?.runs.find((r) => r.id === runId)
  const tabs = [...(config?.resultTabs ?? []), ...EXTRA_TABS]
  const [visible, setVisible] = useState<string[]>(() => {
    try {
      const saved = sessionStorage.getItem('flame-visible-tabs-v3')
      if (saved) return JSON.parse(saved)
    } catch { /* ignore */ }
    return ['table', 'themes', 'dotplot', 'barchart', 'network-tg']
  })
  const view = tab && tabs.some((t) => t.tabId === tab) ? tab : visible[0] ?? 'table'
  usePaletteKey(view)
  useEffect(() => {
    if (!visible.includes(view)) setVisible((v) => [...v, view])
  }, [view, visible])
  useEffect(() => {
    try { sessionStorage.setItem('flame-visible-tabs-v3', JSON.stringify(visible)) } catch { /* ignore */ }
  }, [visible])

  const [filters, setFilters] = useState<ResultFilters>({})
  const [top, setTop] = useState<number | 'all'>(20)
  const [sortBy, setSortBy] = useState('-log10Pvalue')
  const [settings, setSettings] = useState<ViewSettings>({ labels: 'name', xAxis: 'geneRatio', palette: 'OrRd', layout: 'force', similarityCutoff: 10, minShared: 3 })
  const [columns, setColumns] = useState<string[]>(['source', 'id', 'name', 'pvalue', 'intersection', 'coverage', 'genes'])
  const [selected, setSelected] = useState<Set<string>>(new Set())
  useEffect(() => {
    if (run?.paradigm === 'GSEA') {
      setSettings((st) => ({ ...st, xAxis: 'nes', barMetric: 'nes' }))
      setColumns((c) => (c.includes('nes') ? c : [...c.slice(0, 4), 'nes', ...c.slice(4)]))
    }
  }, [run?.paradigm])
  const [showDetails, setShowDetails] = useState(false)
  const [moreOpen, setMoreOpen] = useState(false)
  const plotRef = useRef<PlotHandle | null>(null)
  const networkRef = useRef<NetworkHandle | null>(null)
  const svgRef = useRef<SVGSVGElement | null>(null)
  // Undo/redo covers filters, ranking, view settings, table columns and the selection.
  const history = useHistory(
    { filters, top, sortBy, settings, columns, selected: [...selected].sort() },
    (snap) => {
      setFilters(snap.filters); setTop(snap.top); setSortBy(snap.sortBy)
      setSettings(snap.settings); setColumns(snap.columns); setSelected(new Set(snap.selected))
    },
  )
  const netStyle = useNetworkStyle('results')
  const debouncedFilters = useDebounced(filters)
  const viewSettings = { layout: settings.layout, similarityCutoff: settings.similarityCutoff, minShared: settings.minShared }

  const q = useQuery({
    queryKey: ['view', ws, runId, view, debouncedFilters, top, sortBy, view.startsWith('network') ? viewSettings : null],
    queryFn: () => api<ViewData>(`/runs/${encodeURIComponent(runId)}/view`, { body: { view: EXTRA_TABS.find((t) => t.tabId === view)?.from ?? view, filters: debouncedFilters, top, sortBy, settings: viewSettings } }),
    enabled: run?.status === 'complete',
    placeholderData: keepPreviousData,
  })
  // All filtered terms, for the selection panel (independent of top-N).
  const all = useQuery({
    queryKey: ['view-all', ws, runId],
    queryFn: () => api<ViewData>(`/runs/${encodeURIComponent(runId)}/view`, { body: { view: 'table', top: 'all' } }),
    enabled: run?.status === 'complete',
  })
  const termIndex = useMemo(() => new Map((all.data?.terms ?? []).map((t) => [t.id, t])), [all.data])
  const sources = all.data?.availableSources ?? []

  const onSelect = (ids: string[], additive: boolean) => {
    setSelected((s) => {
      if (!additive) return new Set(ids)
      const n = new Set(s)
      const allIn = ids.every((id) => n.has(id))
      ids.forEach((id) => (allIn ? n.delete(id) : n.add(id)))
      return n
    })
  }

  if (!summary || !config) return <div className="page-center"><Spinner /></div>
  if (!run) return <div className="page-center stack"><ErrorNote error="This run does not exist in your workspace." /><Link to={`/w/${ws}`}>Back to workspace</Link></div>

  const gsea = run?.paradigm === 'GSEA'
  const selectedTerms = [...selected].map((id) => termIndex.get(id)).filter((t): t is Term => !!t)
  const sourceColor = (src: string) => config?.datasourceColors?.[src] ?? '#a1a1aa'
  const isNetwork = view.startsWith('network')
  const isSvg = view === 'chord' || view === 'hive' || view === 'circos'
  const isGsea = run.paradigm === 'GSEA'
  const isPlot = !isNetwork && !isSvg && view !== 'table'
  const arenaView = isNetwork ? view : ({ 'heatmap-tg': 'heatmap-tg', sankey: 'heatmap-tg', hive: 'heatmap-tg', circos: 'heatmap-tg', 'heatmap-tt': 'network-tt', chord: 'network-tt', 'heatmap-gg': 'network-gg' } as Record<string, string>)[view]
  const exportRows = (terms: Term[]) => terms.map((t) => ({ ...t, pvalue: formatP(t.pvalue) }))
  const exportCols = TABLE_COLUMNS.map((c) => ({ key: c.key, label: c.label }))
  const baseName = slug(`${run.title}_${run.listName}`)

  return (
    <div className="results">
      <header className="results-head">
        <Link to={`/w/${ws}`} className="btn btn-ghost btn-sm">← Workspace</Link>
        <div>
          <h1 className="run-title"><ToolLogo tool={run.toolKey} size={28} /> {run.title} <span className="muted">· {run.listName}</span></h1>
          <p className="muted small">{gsea ? 'Gene set enrichment (GSEA) · ' : ''}{run.organismName.replace(/ \[NCBI.*$/, '')} · {run.resultCount} significant terms · {run.params.metric} &lt; {run.params.threshold}</p>
        </div>
        <button className="btn btn-secondary btn-sm" onClick={() => setShowDetails(true)}>Run details</button>
      </header>

      {run.status !== 'complete' ? (
        <div className="page-center"><Spinner label="This run is still in progress…" /></div>
      ) : !run.resultCount ? (
        <Note kind="warning">No significant terms were found. Try more annotation sources, a looser threshold, or another tool.</Note>
      ) : (
        <>
          <nav className="view-tabs" aria-label="Result views">
            {visible.map((id) => {
              const t = tabs.find((x) => x.tabId === id)
              if (!t) return null
              return (
                <span key={id} className={`view-tab${id === view ? ' active' : ''}`}>
                  <button onClick={() => navigate(`/w/${ws}/runs/${runId}/${id}`)}>{t.label}</button>
                  {visible.length > 1 && (
                    <button className="tab-close" aria-label={`Hide ${t.label}`} onClick={() => {
                      const next = visible.filter((v) => v !== id)
                      setVisible(next)
                      if (id === view) navigate(`/w/${ws}/runs/${runId}/${next[0]}`)
                    }}>×</button>
                  )}
                </span>
              )
            })}
            <div className="menu-wrap">
              <button className="btn btn-ghost btn-sm" aria-expanded={moreOpen} onClick={() => setMoreOpen((o) => !o)}>+ More views</button>
              {moreOpen && (
                <div className="menu" onMouseLeave={() => setMoreOpen(false)}>
                  {tabs.filter((t) => !visible.includes(t.tabId)).map((t) => (
                    <button key={t.tabId} onClick={() => { setMoreOpen(false); setVisible((v) => [...v, t.tabId]); navigate(`/w/${ws}/runs/${runId}/${t.tabId}`) }}>{t.label}</button>
                  ))}
                  {tabs.every((t) => visible.includes(t.tabId)) && <span className="muted small menu-note">All views are open.</span>}
                </div>
              )}
            </div>
          </nav>

          <div className="results-body">
            <aside className="filters" aria-label="Filters">
              <h2>Filters</h2>
              <Field label="Search terms" hint={<>Words must all match; use "quotes", OR, and -exclude.</>}>
                <input className="input" type="search" value={filters.search ?? ''} placeholder='e.g. "cell cycle" -mitotic'
                  onChange={(e) => setFilters({ ...filters, search: e.target.value })} />
              </Field>
              <div className="field">
                <span className="field-label">Sources</span>
                <div className="chip-wrap">
                  {sources.map((s) => {
                    const on = !filters.sources || filters.sources.includes(s)
                    return (
                      <button key={s} type="button" className={`chip${on ? ' on' : ''}`} aria-pressed={on} style={{ borderColor: config.datasourceColors[s] }}
                        onClick={() => {
                          const cur = filters.sources ?? sources
                          const next = cur.includes(s) ? cur.filter((x) => x !== s) : [...cur, s]
                          setFilters({ ...filters, sources: next.length === sources.length ? undefined : next })
                        }}>{s}</button>
                    )
                  })}
                </div>
              </div>
              <Field label="P-value at most">
                <input className="input" type="number" step="any" min="0" value={filters.pvalueMax ?? ''} placeholder="any"
                  onChange={(e) => setFilters({ ...filters, pvalueMax: e.target.value ? Number(e.target.value) : null })} />
              </Field>
              <details>
                <summary className="small">More filters</summary>
                {([['termSize', 'Term size'], ['intersection', 'Genes found'], ['termCoverage', 'Coverage %']] as const).map(([k, lab]) => (
                  <div key={k} className="range-field">
                    <span className="field-label">{lab}</span>
                    <input className="input input-sm" type="number" placeholder="min" aria-label={`${lab} minimum`} value={(filters[`${k}Min`] as number | null | undefined) ?? ''}
                      onChange={(e) => setFilters({ ...filters, [`${k}Min`]: e.target.value ? Number(e.target.value) : null })} />
                    <input className="input input-sm" type="number" placeholder="max" aria-label={`${lab} maximum`} value={(filters[`${k}Max`] as number | null | undefined) ?? ''}
                      onChange={(e) => setFilters({ ...filters, [`${k}Max`]: e.target.value ? Number(e.target.value) : null })} />
                  </div>
                ))}
              </details>
              <p className="muted small">{q.data ? `${q.data.filteredCount} of ${q.data.totalCount} terms match` : ''}</p>
              <button className="btn btn-ghost btn-sm" onClick={() => setFilters({})}>Reset filters</button>
            </aside>

            <section className="view-area">
              <div className="toolbar">
                <label>Show <select className="input input-sm" value={String(top)} onChange={(e) => setTop(e.target.value === 'all' ? 'all' : Number(e.target.value))}>
                  {[10, 20, 30, 50, 100].map((n) => <option key={n} value={n}>top {n}</option>)}<option value="all">all</option>
                </select></label>
                <label>by <select className="input input-sm" value={sortBy} onChange={(e) => setSortBy(e.target.value)}>
                  {SORTS.map((s) => <option key={s.value} value={s.value}>{s.label}</option>)}
                </select></label>
                {view !== 'table' && view !== 'heatmap-gg' && (
                  <label>Labels <select className="input input-sm" value={settings.labels} onChange={(e) => setSettings({ ...settings, labels: e.target.value as 'name' | 'id' })}>
                    <option value="name">Term names</option><option value="id">Term IDs</option></select></label>
                )}
                {view === 'dotplot' && <>
                  <label>X axis <select className="input input-sm" value={settings.xAxis} onChange={(e) => setSettings({ ...settings, xAxis: e.target.value as ViewSettings['xAxis'] })}>
                    <option value="geneRatio">Gene ratio</option><option value="foldEnrichment">Fold enrichment</option><option value="coverage">Term coverage</option>
                    {isGsea && <option value="nes">NES (GSEA)</option>}</select></label>
                  <label>Colours <select className="input input-sm" value={settings.palette} onChange={(e) => setSettings({ ...settings, palette: e.target.value as ViewSettings['palette'] })}>
                    <option>OrRd</option><option>Viridis</option><option>Cividis</option><option>Magma</option></select></label>
                </>}
                {view === 'barchart' && (
                  <label>Bars <select className="input input-sm" value={settings.barMetric ?? 'log10p'} onChange={(e) => setSettings({ ...settings, barMetric: e.target.value as 'log10p' | 'coverage' })}>
                    <option value="log10p">Significance</option><option value="coverage">Coverage</option>{isGsea && <option value="nes">NES</option>}</select></label>
                )}
                {(view === 'heatmap-tt' || view === 'heatmap-gg') && (
                  <label>Colours <select className="input input-sm" value={settings.heatScale ?? ''} onChange={(e) => setSettings({ ...settings, heatScale: e.target.value || undefined })}>
                    <option value="">Palette default</option>{['Viridis', 'Cividis', 'YlOrRd', 'Blues', 'Greys', 'Hot'].map((c) => <option key={c} value={c}>{c}</option>)}</select></label>
                )}
                {view === 'heatmap-tg' && (
                  <label>Rows <select className="input input-sm" value={settings.orientation ?? 'terms-rows'} onChange={(e) => setSettings({ ...settings, orientation: e.target.value as ViewSettings['orientation'] })}>
                    <option value="terms-rows">Terms</option><option value="genes-rows">Genes</option></select></label>
                )}
                {isNetwork && (
                  <label>Layout <LayoutSelect style={netStyle.style} onChange={netStyle.setStyle} staticLayout={settings.layout}
                    staticLayouts={config.networkLayouts.map((l) => ({ value: l, label: LAYOUT_LABELS[l] ?? l }))}
                    onStaticLayout={(l) => setSettings({ ...settings, layout: l })} /></label>
                )}
                {isNetwork && <NetworkOptions style={netStyle.style} onChange={netStyle.setStyle} onReset={netStyle.reset} handle={networkRef} />}
                {view === 'network-tt' && (
                  <label>Min. similarity % <input className="input input-sm input-narrow" type="number" min={1} max={100} value={settings.similarityCutoff}
                    onChange={(e) => setSettings({ ...settings, similarityCutoff: Number(e.target.value) })} /></label>
                )}
                {view === 'network-gg' && (
                  <label>Min. shared terms <input className="input input-sm input-narrow" type="number" min={1} max={50} value={settings.minShared}
                    onChange={(e) => setSettings({ ...settings, minShared: Number(e.target.value) })} /></label>
                )}
                {view === 'table' && <ColumnPicker columns={columns} setColumns={setColumns} />}
                <span className="toolbar-spacer" />
                <PaletteSelect />
                <UndoRedoButtons h={history} />
                <ExportMenu items={[
                  ...(isPlot ? [{ label: 'PNG image', run: () => plotRef.current?.toImage('png', baseName) }, { label: 'SVG image', run: () => plotRef.current?.toImage('svg', baseName) }] : []),
                  ...(isNetwork ? [{ label: 'PNG image', run: () => networkRef.current?.toPng(baseName) }] : []),
                  ...(isSvg ? [{ label: 'SVG image', run: () => downloadSvg(svgRef.current, `${baseName}_${view}`) }] : []),
                  { label: 'Shown terms (CSV)', run: () => saveText(toCsv(exportRows(q.data?.terms ?? []), exportCols), `${baseName}_${view}.csv`, 'text/csv') },
                  { label: 'All filtered terms (CSV)', run: () => api<ViewData>(`/runs/${runId}/view`, { body: { view: 'table', filters, top: 'all', sortBy } })
                      .then((d) => saveText(toCsv(exportRows(d.terms), exportCols), `${baseName}_all.csv`, 'text/csv')) },
                ]} />
                {arenaView && <OpenExternalButton path={`/runs/${encodeURIComponent(runId)}/arena3d`} label="Arena3D" caption="3D layers" icon={<Arena3DIcon />}
                  title="Open in Arena3D as a 3D multilayer network: terms of each annotation source on their own layer, genes on another"
                  body={{ view: arenaView, filters, top, sortBy, settings: viewSettings }} />}
                {isNetwork && <OpenExternalButton path={`/runs/${encodeURIComponent(runId)}/norma`} label="NORMA" caption="annotated groups" icon={<NormaIcon />}
                  title="Open this network in NORMA, with terms as annotated groups" body={{ view, filters, top, sortBy, settings: viewSettings }} />}
              </div>
              <p className="muted small view-help">{VIEW_HELP[view]}{isGsea ? ' For GSEA, positive NES = genes of the set sit at the top of your ranking; negative = at the bottom. "Genes found" are the leading-edge genes.' : ''}</p>
              <ErrorNote error={q.error} />
              {q.isLoading && <Spinner />}
              {q.data && q.data.shownCount === 0 && <Note kind="warning">No terms match the current filters.</Note>}
              {q.data && q.data.shownCount > 0 && (() => {
                const props = { data: q.data, config, settings, selected, onSelect, plotRef, networkRef, networkStyle: netStyle.style }
                switch (view) {
                  case 'dotplot': return <DotplotView {...props} />
                  case 'barchart': return <BarchartView {...props} />
                  case 'scatter': return <ScatterView {...props} />
                  case 'table': return <TableView {...props} columns={columns} runId={runId} />
                  case 'sankey': return <SankeyView {...props} />
                  case 'chord': return <ChordView {...props} svgRef={svgRef} />
                  case 'hive': return <HiveView {...props} svgRef={svgRef} />
                  case 'circos': return <CircosView {...props} svgRef={svgRef} />
                  case 'themes': return <ThemesView path={`/runs/${encodeURIComponent(runId)}/themes`} body={{ filters: debouncedFilters }} baseName={baseName}
                    terms={all.data?.terms ?? []} onSelect={onSelect} colorOf={sourceColor} />
                  default: return view.startsWith('heatmap') ? <HeatmapView {...props} /> : <NetworkView {...props} />
                }
              })()}
              <p className="muted small">Tip: click, box- or lasso-select items to add them to the selection panel. Shift-click adds to the selection.</p>
            </section>

            <SelectionPanel terms={selectedTerms} config={config} baseName={baseName} taxid={run.organismTaxId} runId={runId} onRemove={(id) => onSelect([id], true)} onClear={() => setSelected(new Set())}
              allTerms={all.data?.terms ?? []} listName={run.listName} onSelect={onSelect} />
          </div>
        </>
      )}
      {showDetails && <RunDetailsModal runId={runId} onClose={() => setShowDetails(false)} />}
    </div>
  )
}

function ColumnPicker({ columns, setColumns }: { columns: string[]; setColumns: (c: string[]) => void }) {
  const [open, setOpen] = useState(false)
  return (
    <div className="menu-wrap">
      <button className="btn btn-ghost btn-sm" onClick={() => setOpen((o) => !o)} aria-expanded={open}>Columns</button>
      {open && (
        <div className="menu" onMouseLeave={() => setOpen(false)}>
          {TABLE_COLUMNS.map((c) => (
            <label key={c.key} className="check">
              <input type="checkbox" checked={columns.includes(c.key)} onChange={() => setColumns(columns.includes(c.key) ? columns.filter((x) => x !== c.key) : [...columns, c.key])} />
              {c.label}
            </label>
          ))}
        </div>
      )}
    </div>
  )
}

export function ExportMenu({ items, label = 'Export' }: { items: { label: string; run: () => void }[]; label?: string }) {
  const [open, setOpen] = useState(false)
  return (
    <div className="menu-wrap">
      <button className="btn btn-secondary btn-sm" onClick={() => setOpen((o) => !o)} aria-expanded={open}>{label} ▾</button>
      {open && (
        <div className="menu menu-right" onMouseLeave={() => setOpen(false)}>
          {items.map((i) => <button key={i.label} onClick={() => { setOpen(false); i.run() }}>{i.label}</button>)}
        </div>
      )}
    </div>
  )
}

/** Pathway maps (KEGG with the genes coloured, Reactome with genes flagged), ontology pages and cross-database searches for one term. */
function PathwayLinks({ runId, termId }: { runId: string; termId: string }) {
  const q = useQuery({
    queryKey: ['pathway-links', getWorkspaceId(), runId, termId],
    queryFn: () => api<{ links: { label: string; url: string; group: string }[]; notes: string[] }>(`/runs/${encodeURIComponent(runId)}/pathway-links`, { body: { termId } }),
    staleTime: Infinity,
  })
  if (q.isLoading) return <p className="muted small">Finding pathway maps…</p>
  if (!q.data) return null
  const groups = [...new Set(q.data.links.map((l) => l.group))]
  return (
    <div className="pathway-links">
      {groups.map((g) => (
        <div key={g}>
          <div className="link-menu-group">{g}</div>
          <ul className="plain-list">
            {q.data!.links.filter((l) => l.group === g && isHttpUrl(l.url)).map((l) => (
              <li key={l.url}><a href={l.url} target="_blank" rel="noopener noreferrer">{l.label} ↗</a></li>
            ))}
          </ul>
        </div>
      ))}
      {q.data.notes.map((n) => <p key={n} className="muted small">{n}</p>)}
    </div>
  )
}

type GeneRank = 'selected' | 'all' | 'score' | 'input'
interface RankedGene { gene: string; inSelected: number; inAll: number; score: number | null; position: number | null; terms: string[] }

/**
 * Genes of the selected terms, ranked (by how many selected terms contain
 * them, how many significant terms overall, the list's own scores, or their
 * position in the input list). Click a gene to see its terms and select them.
 */
function GeneRanking({ terms, allTerms, listName, taxid, focus, setFocus, onSelect, colorOf }: {
  terms: Term[]; allTerms: Term[]; listName: string; taxid: number; focus: string | null; setFocus: (g: string | null) => void
  onSelect: (ids: string[], additive: boolean) => void; colorOf: (source: string) => string | undefined
}) {
  const listQ = useQuery({
    queryKey: ['list-for-genes', getWorkspaceId(), listName],
    queryFn: () => api<{ ids: string[]; scores?: number[] }>(`/lists/${encodeURIComponent(listName)}`),
    staleTime: 5 * 60_000, retry: false,
  })
  const ranked = !!listQ.data?.scores?.length
  const [by, setBy] = useState<GeneRank | null>(null)
  // One term selected: every gene is in it once, so rank by how many significant terms share the gene.
  const mode: GeneRank = by ?? (ranked ? 'score' : terms.length === 1 ? 'all' : 'selected')
  const [limit, setLimit] = useState(30)
  const genes = useMemo(() => {
    const up = (g: string) => g.toUpperCase()
    const pos = new Map<string, number>(), sc = new Map<string, number>()
    ;(listQ.data?.ids ?? []).forEach((g, i) => { if (!pos.has(up(g))) pos.set(up(g), i + 1); const v = listQ.data?.scores?.[i]; if (typeof v === 'number' && !sc.has(up(g))) sc.set(up(g), v) })
    const allCount = new Map<string, number>()
    allTerms.forEach((t) => splitGenes(t.genes).forEach((g) => allCount.set(up(g), (allCount.get(up(g)) ?? 0) + 1)))
    const m = new Map<string, RankedGene>()
    terms.forEach((t) => splitGenes(t.genes).forEach((g) => {
      const r = m.get(up(g)) ?? { gene: g, inSelected: 0, inAll: allCount.get(up(g)) ?? 0, score: sc.get(up(g)) ?? null, position: pos.get(up(g)) ?? null, terms: [] }
      r.inSelected += 1; r.terms.push(t.id); m.set(up(g), r)
    }))
    const key = (r: RankedGene) => mode === 'selected' ? -r.inSelected * 1e6 - r.inAll : mode === 'all' ? -r.inAll * 1e3 - r.inSelected
      : mode === 'score' ? -(Math.abs(r.score ?? 0)) : (r.position ?? 1e9)
    return [...m.values()].sort((a, b) => key(a) - key(b) || a.gene.localeCompare(b.gene))
  }, [terms, allTerms, listQ.data, mode])
  const value = (r: RankedGene) => mode === 'selected' ? r.inSelected : mode === 'all' ? r.inAll : mode === 'score' ? r.score ?? 0 : r.position ?? 0
  const max = Math.max(1e-9, ...genes.map((r) => Math.abs(value(r))))
  const fmt = (r: RankedGene) => mode === 'score' ? (r.score == null ? '—' : formatNum(r.score, 3)) : mode === 'input' ? (r.position == null ? '—' : `#${r.position}`) : String(value(r))
  const f = focus ? genes.find((r) => r.gene === focus) : null
  const withGene = focus ? allTerms.filter((t) => splitGenes(t.genes).some((g) => g.toUpperCase() === focus.toUpperCase())).map((t) => t.id) : []
  if (!genes.length) return null
  return (
    <section className="gene-rank" aria-label="Genes of the selected terms">
      <div className="row between wrap">
        <h3>Genes <span className="count">{genes.length}</span></h3>
        <CopyButton compact title="Copy the genes in this order" text={genes.map((r) => r.gene).join('\n')} />
      </div>
      <label className="small">Rank by{' '}
        <select className="input input-sm" value={mode} onChange={(e) => { setBy(e.target.value as GeneRank); setLimit(30) }}>
          <option value="selected">selected terms containing the gene</option>
          <option value="all">all significant terms containing it</option>
          {ranked && <option value="score">score in your list (|value|)</option>}
          <option value="input">position in your list</option>
        </select>
      </label>
      <ol className="gene-rank-list">
        {genes.slice(0, limit).map((r, i) => {
          const v = value(r)
          return (
            <li key={r.gene} className={focus === r.gene ? 'on' : ''}>
              <button type="button" className="gene-rank-row" onClick={() => setFocus(focus === r.gene ? null : r.gene)} aria-pressed={focus === r.gene}
                title={`${r.gene}: in ${r.inSelected} selected and ${r.inAll} significant terms${r.score != null ? ` · score ${r.score}` : ''}${r.position ? ` · #${r.position} in your list` : ''}`}>
                <span className="muted rank-n">{i + 1}</span>
                <span className="gene-name">{r.gene}</span>
                <span className="gene-bar"><i style={{ width: `${(Math.abs(v) / max) * 100}%`, background: mode === 'score' ? (v < 0 ? '#2563eb' : '#e11d48') : undefined }} /></span>
                <span className="small gene-val">{fmt(r)}</span>
              </button>
              {taxid === 9606 && <a className="gc-mini" href={`https://www.genecards.org/cgi-bin/carddisp.pl?gene=${encodeURIComponent(r.gene)}`} target="_blank" rel="noopener noreferrer"
                title={`${r.gene} in GeneCards`} aria-label={`${r.gene} in GeneCards`}>GC</a>}
            </li>
          )
        })}
      </ol>
      {genes.length > limit && <button className="btn btn-ghost btn-sm" onClick={() => setLimit((l) => l + 50)}>Show more ({genes.length - limit} left)</button>}
      {f && (
        <div className="gene-focus small">
          <div className="row between wrap"><strong><LinkMenu label={f.gene} links={geneLinks(f.gene, taxid)} /></strong>
            <button className="icon-btn" aria-label="Close gene details" onClick={() => setFocus(null)}>×</button></div>
          <div>In {f.inSelected} of the selected terms{f.score != null ? ` · score ${formatNum(f.score, 3)}` : ''}{f.position ? ` · #${f.position} in ${listName}` : ''}</div>
          <ul className="plain-list">{f.terms.map((id) => { const t = terms.find((x) => x.id === id); return t ? <li key={id}><span className="dot" style={{ background: colorOf(t.source) }} />{t.name}</li> : null })}</ul>
          {withGene.length > f.inSelected && <button className="btn btn-secondary btn-sm" onClick={() => onSelect(withGene, true)}>Add all {withGene.length} terms with {f.gene}</button>}
        </div>
      )}
    </section>
  )
}

function SelectionPanel({ terms, config, baseName, taxid, runId, onRemove, onClear, allTerms, listName, onSelect }: {
  terms: Term[]; config: ReturnType<typeof useConfig>['data']; baseName: string; taxid: number; runId: string; onRemove: (id: string) => void; onClear: () => void
  allTerms: Term[]; listName: string; onSelect: (ids: string[], additive: boolean) => void
}) {
  const [open, setOpen] = useState<string | null>(null)
  const [focus, setFocus] = useState<string | null>(null)
  const hasFocus = (t: Term) => !!focus && splitGenes(t.genes).some((g) => g.toUpperCase() === focus.toUpperCase())
  const cols = [{ key: 'source', label: 'Source' }, { key: 'id', label: 'Term ID' }, { key: 'name', label: 'Term' }, { key: 'pvalue', label: 'P-value' }, { key: 'genes', label: 'Genes' }]
  const rows = terms.map((t) => ({ ...t, pvalue: formatP(t.pvalue) }))
  return (
    <aside className="selection" aria-label="Selected terms">
      <div className="row between">
        <h2>Selected <span className="count">{terms.length}</span></h2>
        {terms.length > 0 && <button className="btn btn-ghost btn-sm" onClick={onClear}>Clear</button>}
      </div>
      {terms.length > 0 && (
        <div className="row wrap sel-copy">
          <CopyButton label="Copy term names" text={terms.map((t) => t.name).join('\n')} />
          <CopyButton label="Copy all genes" title="Copy every gene of the selected terms, one per line" text={[...new Set(terms.flatMap((t) => splitGenes(t.genes)))].join('\n')} />
        </div>
      )}
      {!terms.length && <p className="muted small">Click terms in any view to collect them here, see their genes and export them.</p>}
      <ul className="plain-list">
        {terms.map((t) => (
          <li key={t.id} className={`sel-item${focus ? (hasFocus(t) ? ' has-gene' : ' no-gene') : ''}`}>
            <div className="row between">
              <button className="sel-title" onClick={() => setOpen(open === t.id ? null : t.id)} aria-expanded={open === t.id}>
                <span className="dot" style={{ background: config?.datasourceColors[t.source] }} />{t.name}
              </button>
              <span className="row">
                <CopyButton compact text={splitGenes(t.genes).join('\n')} title={`Copy the genes of ${t.name}`} />
                <button className="icon-btn" aria-label={`Remove ${t.name}`} onClick={() => onRemove(t.id)}>×</button>
              </span>
            </div>
            {open === t.id && (
              <div className="sel-detail small">
                <div>{t.id} · {t.source} · p = {formatP(t.pvalue)}</div>
                <div>{t.intersection} of {formatNum(t.termSize)} genes</div>
                <div className="gene-chips">{splitGenes(t.genes).slice(0, 40).map((g) => <span key={g} className="gene"><LinkMenu label={g} links={geneLinks(g, taxid)} /></span>)}
                  {splitGenes(t.genes).length > 40 && <span className="muted small">+{splitGenes(t.genes).length - 40} more (copy to get all)</span>}</div>
                {isHttpUrl(t.link) && <a href={t.link} target="_blank" rel="noopener noreferrer">Open in {config?.linkProviders[t.source] ?? 'source database'} ↗</a>}
                <PathwayLinks runId={runId} termId={t.id} />
              </div>
            )}
          </li>
        ))}
      </ul>
      {terms.length > 0 && <GeneRanking terms={terms} allTerms={allTerms} listName={listName} taxid={taxid} focus={focus} setFocus={setFocus} onSelect={onSelect}
        colorOf={(src) => config?.datasourceColors[src]} />}
      {terms.length > 0 && (
        <div className="row wrap">
          <CopyButton text={terms.map((t) => `${t.id}\t${t.name}\t${formatP(t.pvalue)}\t${t.genes}`).join('\n')} />
          <button className="btn btn-ghost btn-sm" onClick={() => saveText(toCsv(rows, cols), `${baseName}_selection.csv`, 'text/csv')}>CSV</button>
          <button className="btn btn-ghost btn-sm" onClick={() => saveText(toHtmlTable('Selected terms', rows, cols), `${baseName}_selection.html`, 'text/html')}>HTML</button>
          <button className="btn btn-ghost btn-sm" onClick={() => {
            const url = URL.createObjectURL(new Blob([toHtmlTable('Selected terms', rows, cols)], { type: 'text/html' }))
            const w = window.open(url, '_blank', 'noopener')
            if (!w) saveText(toHtmlTable('Selected terms', rows, cols), `${baseName}_selection.html`, 'text/html')
            setTimeout(() => URL.revokeObjectURL(url), 60_000)
          }}>Print / PDF</button>
        </div>
      )}
    </aside>
  )
}

function RunDetailsModal({ runId, onClose }: { runId: string; onClose: () => void }) {
  const q = useQuery({ queryKey: ['details', getWorkspaceId(), runId], queryFn: () => api<RunDetails>(`/runs/${encodeURIComponent(runId)}/details`) })
  const d = q.data
  const text = d ? [
    `Tool: ${d.tool}`, `Organism: ${d.organismName}`, `Input list: ${d.listName} (${d.inputSize} IDs)`,
    `Mapped: ${d.mappedCount}`, `Background: ${d.backgroundList ?? 'tool default (genome)'}`,
    `Namespace: ${d.params.namespace}`, `Correction: ${d.params.metric} < ${d.params.threshold}`,
    `Sources: ${[d.params.datasources].flat().join(', ')}`, `Significant terms: ${d.significantTerms}`,
  ].join('\n') : ''
  return (
    <Modal title="Run details" onClose={onClose} subtitle="Everything needed to report or reproduce this analysis.">
      {q.isLoading && <Spinner />}
      <ErrorNote error={q.error} />
      {d && (
        <div className="stack">
          <dl className="details">
            <dt>Tool</dt><dd>{d.tool}</dd>
            <dt>Organism</dt><dd>{d.organismName}</dd>
            <dt>Input list</dt><dd>{d.listName} ({plural(d.inputSize ?? 0, 'ID')})</dd>
            <dt>Background</dt><dd>{d.backgroundList ?? 'Tool default (whole genome)'}{d.backgroundSize ? ` · ${d.backgroundSize.toLocaleString()} genes` : ''}</dd>
            <dt>ID namespace</dt><dd>{d.params.namespace}</dd>
            <dt>Significance</dt><dd>{d.params.metric} &lt; {d.params.threshold}</dd>
            <dt>Sources searched</dt><dd>{[d.params.datasources].flat().join(', ')}</dd>
            <dt>Sources with hits</dt><dd>{(d.sourcesWithHits ?? []).join(', ') || 'none'}</dd>
            <dt>Significant terms</dt><dd>{d.significantTerms}</dd>
            <dt>Mapped input IDs</dt><dd>{d.mappedCount} of {d.inputSize}</dd>
          </dl>
          {!!d.unmapped?.length && <details><summary>{plural(d.unmapped.length, 'ID')} could not be mapped</summary><p className="mono small">{d.unmapped.join(', ')}</p></details>}
          {!!d.noHitAnalytes?.length && <details><summary>{plural(d.noHitAnalytes.length, 'input')} in no significant term</summary><p className="mono small">{d.noHitAnalytes.join(', ')}</p></details>}
          {!!d.messages?.length && <Note kind="warning">{d.messages.join(' ')}</Note>}
          <div className="row end"><CopyButton text={text} label="Copy summary" /></div>
        </div>
      )}
    </Modal>
  )
}
