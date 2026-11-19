import { keepPreviousData, useQuery } from '@tanstack/react-query'
import { useEffect, useMemo, useRef, useState, type CSSProperties } from 'react'
import { Link, useSearchParams } from 'react-router-dom'
import { api, getWorkspaceId, saveText } from '../api/client'
import { useConfig } from '../api/hooks'
import { ListsPanel } from '../components/ListsPanel'
import type { ComparisonData, ComparisonFilters, ComparisonTerm } from '../api/types'
import { NetworkGraph, Plot, type NetworkHandle, type PlotHandle } from '../components/charts'
import { LayoutSelect, NetworkOptions } from '../components/NetworkOptions'
import { useNetworkStyle } from '../lib/networkStyle'
import { BackToWorkspace, CopyButton, ErrorNote, Field, Modal, Note, OpenExternalButton, Segmented, Spinner } from '../components/ui'
import { ExportMenu } from './Results'
import { ChordDiagram, CircosBundle, SetOverlapUpSet } from '../components/viz'
import { usePlainSelection } from '../lib/imageExport'
import { UndoRedoButtons } from '../components/Undo'
import { ComparisonStatistics } from '../features/compare/Statistics'
import { CompareTable } from '../features/compare/CompareTable'
import { RankChart } from '../features/compare/RankChart'
import { saveXlsx, sheetsByCategory } from '../lib/xlsx'
import { ThemesView } from '../features/themes/Themes'
import { Arena3DIcon, NormaIcon } from '../components/ExternalIcons'
import { useHistory } from '../lib/history'
import { formatNum, formatP, plural, splitGenes, toCsv, toHtmlTable } from '../lib/format'
import { categoricalColor, PaletteScope, reverseScale, roleColors, sequentialScale, usePaletteKey, usePaletteMode } from '../lib/palette'
import { PaletteSelect } from '../components/PaletteSelect'

type View = 'table' | 'ranks' | 'themes' | 'overview' | 'bubble' | 'heatmap' | 'upset' | 'chord' | 'circos' | 'stats' | 'network'
const SORTS = [{ value: '-log10Pvalue', label: 'Combined significance' }, { value: 'Fold Enrichment', label: 'Fold enrichment' }, { value: 'Agreement', label: 'Agreement between runs' }]

function useDebounced<T>(v: T, ms = 300) {
  const [d, setD] = useState(v)
  useEffect(() => { const h = setTimeout(() => setD(v), ms); return () => clearTimeout(h) }, [v, ms])
  return d
}

const EMPTY_SELECTION = new Set<string>()

export default function ComparePage() {
  return <PaletteScope id="compare"><ComparePageBody /></PaletteScope>
}

/**
 * Asks what to send before opening the comparison in Arena3D or NORMA.
 * Arena3D: a Tools layer (the compared runs), one layer per chosen term category, optionally a Genes layer.
 * NORMA: the terms of the chosen categories as a network, with each tool as an annotation group.
 */
function ExportNetworkDialog({ target, onClose, runIds, filters, sortBy, sources, counts, shown, runLabels }: {
  target: 'arena3d' | 'norma'; onClose: () => void; runIds: string[]; filters: ComparisonFilters; sortBy: string
  sources: string[]; counts: Map<string, number>; shown: string[]; runLabels: string[]
}) {
  const arena = target === 'arena3d'
  const [picked, setPicked] = useState<string[]>(shown.filter((x) => sources.includes(x)))
  const [tools, setTools] = useState(true)
  const [genes, setGenes] = useState(false)
  const [perSource, setPerSource] = useState(arena ? 30 : 20)
  const toggle = (src: string) => setPicked((cur) => (cur.includes(src) ? cur.filter((x) => x !== src) : [...cur, src]))
  const termTotal = picked.reduce((n, src) => n + Math.min(perSource, counts.get(src) ?? 0), 0)
  const layers = [...(arena && tools ? ['Tools'] : []), ...picked, ...(arena && genes ? ['Genes'] : [])]
  const problem = picked.length === 0 ? 'Tick at least one term category.' : arena && !tools && !genes ? 'Include the Tools layer or the Genes layer: the term layers need something to link to.' : null
  return (
    <Modal title={arena ? 'Open in Arena3D' : 'Open in NORMA'} size="lg" onClose={onClose} palette={false}
      subtitle={arena ? 'Choose the layers of the 3D network. The tools sit on one layer and each term category on its own layer.' : 'Choose the term categories to draw. Each tool becomes an annotation group around the terms it found.'}>
      {arena && (
        <div className="field">
          <span className="field-label">Layers besides the terms</span>
          <label className="check"><input type="checkbox" checked={tools} onChange={(e) => setTools(e.target.checked)} /> <span><strong>Tools</strong> — one node per compared run ({runLabels.join(', ')}), linked to the terms it found</span></label>
          <label className="check"><input type="checkbox" checked={genes} onChange={(e) => setGenes(e.target.checked)} /> <span><strong>Genes</strong> — the genes behind the terms, linked to their terms (can be large)</span></label>
        </div>
      )}
      <div className="field">
        <div className="row between wrap">
          <span className="field-label">{arena ? 'Term layers (one per category)' : 'Term categories'}</span>
          <span className="row">
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => setPicked(sources)}>Select all</button>
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => setPicked([])}>None</button>
          </span>
        </div>
        <div className="export-sources">
          {sources.map((src) => (
            <label key={src} className="check"><input type="checkbox" checked={picked.includes(src)} onChange={() => toggle(src)} /> {src} <span className="muted small">{(counts.get(src) ?? 0).toLocaleString()} terms</span></label>
          ))}
        </div>
      </div>
      <Field label="Terms per category" hint={`The top terms of each category by ${SORTS.find((x) => x.value === sortBy)?.label.toLowerCase() ?? 'significance'}, after the filters of this page.`}>
        <select className="input" value={perSource} onChange={(e) => setPerSource(Number(e.target.value))}>
          {[10, 20, 30, 50, 100].map((n) => <option key={n} value={n}>top {n}</option>)}
        </select>
      </Field>
      {problem ? <Note kind="warning">{problem}</Note> : (
        <p className="small">
          {arena
            ? <>Will send <strong>{layers.length} layers</strong>: {layers.join(', ')} — about {termTotal} terms{tools ? ` and ${runLabels.length} tools` : ''}.</>
            : <>Will send about <strong>{termTotal} terms</strong> from {picked.join(', ')}, with <strong>{runLabels.length} annotation groups</strong> ({runLabels.join(', ')}). Terms are linked when they share genes; terms sharing none are left out.</>}
        </p>
      )}
      <div className="row end">
        <button type="button" className="btn btn-ghost" onClick={onClose}>Cancel</button>
        {!problem && (arena
          ? <OpenExternalButton path="/comparisons/arena3d" label="Open in Arena3D" className="btn btn-primary" title="Send these layers to Arena3D and open it in a new tab"
              body={{ runIds, filters, top: perSource, sortBy, layers: { tools, genes, sources: picked } }} />
          : <OpenExternalButton path="/comparisons/norma" label="Open in NORMA" className="btn btn-primary" title="Send these categories to NORMA and open it in a new tab"
              body={{ runIds, filters, top: perSource, sortBy, sources: picked }} />)}
      </div>
      <p className="muted small">Only what is listed here is sent: term names and IDs{arena && genes ? ', gene names' : ''}{arena ? '' : ', the genes shared between terms as link weights'} and the run labels. The link opens in a new tab and expires after a while.</p>
    </Modal>
  )
}

function ComparePageBody() {
  const ws = getWorkspaceId()!
  const [params] = useSearchParams()
  const runIds = (params.get('runs') ?? '').split(',').filter(Boolean)
  const { data: config } = useConfig()
  const [view, setView] = useState<View>('table')
  usePaletteKey(view)
  const [filters, setFilters] = useState<ComparisonFilters>({ runsMode: 'any' })
  const [top, setTop] = useState<number | 'all'>(20)
  const [sortBy, setSortBy] = useState('-log10Pvalue')
  const [labels, setLabels] = useState<'Function' | 'Term_ID'>('Function')
  const [showLabels, setShowLabels] = useState(false)
  const [selected, setSelected] = useState<Set<string>>(new Set())
  // Drawn selection; empty while an image is exported without highlighting.
  const shownSel = usePlainSelection(selected, EMPTY_SELECTION)
  const plotRef = useRef<PlotHandle | null>(null)
  const svgRef = useRef<SVGSVGElement | null>(null)
  const networkRef = useRef<NetworkHandle | null>(null)
  const netStyle = useNetworkStyle('compare', { engine: 'barnesHut', labelSize: 12 })
  const [circosTop, setCircosTop] = useState<number | 'all'>(60)
  // Export dialog (Arena3D or NORMA): asks which layers / term categories to send.
  const [exportTo, setExportTo] = useState<'arena3d' | 'norma' | null>(null)
  const pm = usePaletteMode()
  const history = useHistory(
    { view, filters, top, sortBy, labels, selected: [...selected].sort() },
    (s) => { setView(s.view); setFilters(s.filters); setTop(s.top); setSortBy(s.sortBy); setLabels(s.labels); setSelected(new Set(s.selected)) },
  )
  const f = useDebounced(filters)

  const q = useQuery({
    queryKey: ['compare', ws, runIds.join(','), view, f, top, sortBy],
    queryFn: () => api<ComparisonData>('/comparisons', { body: { runIds, view: view === 'upset' || view === 'chord' || view === 'circos' || view === 'stats' || view === 'table' || view === 'ranks' || view === 'themes' ? 'overview' : view === 'network' ? 'bubble' : view, filters: f, top, sortBy, settings: { labels } } }),
    enabled: runIds.length >= 2,
    placeholderData: keepPreviousData,
  })
  const all = useQuery({
    queryKey: ['compare-all', ws, runIds.join(',')],
    queryFn: () => api<ComparisonData>('/comparisons', { body: { runIds, view: 'overview' } }),
    enabled: runIds.length >= 2,
  })
  const index = useMemo(() => new Map((all.data?.terms ?? []).map((t) => [t.id, t])), [all.data])
  const runs = all.data?.runs ?? q.data?.runs ?? []
  // Terms per annotation source, for the source tabs (GO:BP, GO:CC, KEGG…).
  const allSources = all.data?.availableSources ?? []
  const sourceCounts = useMemo(() => {
    const m = new Map<string, number>()
    ;(all.data?.terms ?? []).forEach((t) => m.set(t.source, (m.get(t.source) ?? 0) + 1))
    return m
  }, [all.data])
  // The source tab that is open: 'all', one source, or none when several (but not all) are ticked in the filters.
  const sourceTab = !filters.sources || filters.sources.length === allSources.length ? 'all' : filters.sources.length === 1 ? filters.sources[0] : null
  const activeRuns = filters.runs ?? runs.map((r) => r.id)
  const selectedTerms = [...selected].map((id) => index.get(id)).filter((t): t is ComparisonTerm => !!t)

  const onSelect = (ids: string[], additive: boolean) => setSelected((s) => {
    if (!additive) return new Set(ids)
    const n = new Set(s)
    const allIn = ids.every((i) => n.has(i))
    ids.forEach((i) => (allIn ? n.delete(i) : n.add(i)))
    return n
  })
  const overviewClick = (kind: 'bubble' | 'pin', metric: string, runId?: string) =>
    api<string[]>('/comparisons/overview-terms', { body: { runIds, filters: f, kind, metric, runId } }).then((ids) => onSelect(ids, false))

  if (runIds.length < 2) return <div className="page-center stack"><Note kind="warning">Select at least two finished runs in your workspace to compare them.</Note><Link to={`/w/${ws}`}>Back to workspace</Link></div>
  if (!config) return <Spinner />

  const label = (t: ComparisonTerm) => (labels === 'Term_ID' ? t.id : t.name.length > 50 ? `${t.name.slice(0, 49)}…` : t.name)

  return (
    <div className="results">
      <header className="results-head">
        <BackToWorkspace ws={ws} />
        <div>
          <h1>Comparing {plural(runIds.length, 'run')}</h1>
          <p className="muted small">{runs.map((r) => `${r.label} · ${r.listName}`).join('  |  ')}</p>
        </div>
        <span className="page-actions" role="toolbar" aria-label="View actions">
          {view !== 'table' && <PaletteSelect />}
          <UndoRedoButtons h={history} />
          <button type="button" className="btn btn-secondary btn-sm" onClick={() => setExportTo('arena3d')} title="Open the comparison in Arena3D as layers: choose which layers to include">
            <span className="ext-btn"><Arena3DIcon /><span className="ext-btn-text"><strong>Arena3D</strong><span>choose the layers…</span></span><span aria-hidden="true" className="ext-btn-arrow">↗</span></span>
          </button>
          <button type="button" className="btn btn-secondary btn-sm" onClick={() => setExportTo('norma')} title="Open the comparison in NORMA with the tools as annotation groups: choose which term categories to include">
            <span className="ext-btn"><NormaIcon /><span className="ext-btn-text"><strong>NORMA</strong><span>choose the categories…</span></span><span aria-hidden="true" className="ext-btn-arrow">↗</span></span>
          </button>
        </span>
      </header>
      <p className="muted small">Terms found by several runs get a combined p-value (Fisher's method). Agreement = how many of the active runs found a term.</p>

      {exportTo && (
        <ExportNetworkDialog target={exportTo} onClose={() => setExportTo(null)} runIds={runIds} filters={f} sortBy={sortBy}
          sources={allSources} counts={sourceCounts} shown={filters.sources?.length ? filters.sources : allSources}
          runLabels={runs.filter((r) => activeRuns.includes(r.id)).map((r) => r.label)} />
      )}
      {allSources.length > 1 && (
        <nav className="source-tabs" role="tablist" aria-label="Annotation source">
          <button type="button" role="tab" aria-selected={sourceTab === 'all'} className={`source-tab${sourceTab === 'all' ? ' active' : ''}`}
            onClick={() => setFilters({ ...filters, sources: undefined })}>
            All sources <span className="count">{(all.data?.terms ?? []).length.toLocaleString()}</span>
          </button>
          {allSources.map((src) => (
            <button key={src} type="button" role="tab" aria-selected={sourceTab === src} className={`source-tab${sourceTab === src ? ' active' : ''}`}
              style={{ '--src-color': config.datasourceColors[src] ?? '#a1a1aa' } as CSSProperties} title={`Compare the runs on ${src} only, in every view`}
              onClick={() => setFilters({ ...filters, sources: [src] })}>
              {src} <span className="count">{(sourceCounts.get(src) ?? 0).toLocaleString()}</span>
            </button>
          ))}
          {sourceTab === null && <span className="muted small">{filters.sources?.length} sources shown (set in Filters)</span>}
        </nav>
      )}
      <nav className="view-tabs" aria-label="Comparison views">
        {(['table', 'ranks', 'themes', 'overview', 'stats', 'upset', 'network', 'chord', 'circos', 'bubble', 'heatmap'] as View[]).map((v) => (
          <span key={v} className={`view-tab${v === view ? ' active' : ''}`}><button onClick={() => setView(v)}>{
            { table: 'Table', ranks: 'Ranks', themes: 'Themes', overview: 'Overview', stats: 'Statistics', upset: 'Overlap (UpSet)', network: 'Network (runs–terms)', chord: 'Chord (shared terms)', circos: 'Circos (bundled)', bubble: 'Bubble plot', heatmap: 'Heatmap' }[v]}</button></span>
        ))}
      </nav>

      <div className="results-body">
        <aside className="filters" aria-label="Your lists and filters">
          <ListsPanel />
          <h2>Filters</h2>
          <div className="field">
            <span className="field-label">Runs</span>
            {runs.map((r) => (
              <label key={r.id} className="check">
                <input type="checkbox" checked={activeRuns.includes(r.id)} onChange={() => {
                  const next = activeRuns.includes(r.id) ? activeRuns.filter((x) => x !== r.id) : [...activeRuns, r.id]
                  setFilters({ ...filters, runs: next.length === runs.length ? undefined : next })
                }} />
                <i className="dot" style={{ background: `var(--color-${r.tool.toLowerCase().replace(/[^a-z]/g, '')}, var(--accent-color))` }} />{r.label}
              </label>
            ))}
            <Segmented label="Run match" value={filters.runsMode ?? 'any'} onChange={(v) => setFilters({ ...filters, runsMode: v })}
              options={[{ value: 'any', label: 'Found by any' }, { value: 'all', label: 'Found by all' }]} />
          </div>
          <Field label="Search terms"><input className="input" type="search" value={filters.search ?? ''} onChange={(e) => setFilters({ ...filters, search: e.target.value })} /></Field>
          <div className="range-field">
            <span className="field-label">Found by (runs)</span>
            <input className="input input-sm" type="number" min={1} placeholder="min" aria-label="Agreement minimum" value={filters.consensusMin ?? ''} onChange={(e) => setFilters({ ...filters, consensusMin: e.target.value ? Number(e.target.value) : null })} />
            <input className="input input-sm" type="number" min={1} placeholder="max" aria-label="Agreement maximum" value={filters.consensusMax ?? ''} onChange={(e) => setFilters({ ...filters, consensusMax: e.target.value ? Number(e.target.value) : null })} />
          </div>
          <div className="field">
            <span className="field-label">Sources</span>
            <div className="chip-wrap">
              {(all.data?.availableSources ?? []).map((s) => {
                const srcs = all.data?.availableSources ?? []
                const on = !filters.sources || filters.sources.includes(s)
                return <button key={s} type="button" className={`chip${on ? ' on' : ''}`} aria-pressed={on} style={{ borderColor: config.datasourceColors[s] }}
                  onClick={() => { const cur = filters.sources ?? srcs; const next = cur.includes(s) ? cur.filter((x) => x !== s) : [...cur, s]; setFilters({ ...filters, sources: next.length === srcs.length ? undefined : next }) }}>{s}</button>
              })}
            </div>
          </div>
          <Field label="Combined p-value at most"><input className="input" type="number" step="any" min="0" placeholder="any" value={filters.pvalueMax ?? ''} onChange={(e) => setFilters({ ...filters, pvalueMax: e.target.value ? Number(e.target.value) : null })} /></Field>
          <div className="range-field">
            <span className="field-label">Term size</span>
            <input className="input input-sm" type="number" placeholder="min" aria-label="Term size minimum" value={filters.termSizeMin ?? ''} onChange={(e) => setFilters({ ...filters, termSizeMin: e.target.value ? Number(e.target.value) : null })} />
            <input className="input input-sm" type="number" placeholder="max" aria-label="Term size maximum" value={filters.termSizeMax ?? ''} onChange={(e) => setFilters({ ...filters, termSizeMax: e.target.value ? Number(e.target.value) : null })} />
          </div>
          <p className="muted small">{q.data ? `${q.data.filteredCount} of ${q.data.totalCount} terms match` : ''}</p>
          <button className="btn btn-ghost btn-sm" onClick={() => setFilters({ runsMode: 'any' })}>Reset filters</button>
        </aside>

        <section className="view-area">
          {(view === 'upset' || view === 'chord' || view === 'circos') && q.data && q.data.terms.length > 0 && (() => {
            const active = q.data!.runs.filter((r) => q.data!.activeRunIds.includes(r.id))
            const runColors = active.map((r, i) => categoricalColor(pm, r.tool, active.map((x) => x.tool), `var(--color-${r.tool.toLowerCase().replace(/[^a-z]/g, '')}, ${['#f97316', '#8b5cf6', '#e11d48', '#6366f1'][i % 4]})`))
            if (view === 'upset') {
              return (
                <>
                  <p className="muted small">Each bar counts terms for the runs marked below it (left: terms per run). Click one or more bars to select their terms — they appear in the Selected panel — or use a set operation.</p>
                  <SetOverlapUpSet sets={active.map((r) => ({ id: r.id, label: r.label }))} colors={runColors}
                    membership={q.data!.terms.map((t) => ({ item: t.id, sets: t.runIds.filter((r) => q.data!.activeRunIds.includes(r)) }))}
                    onSelect={(items) => onSelect(items, false)} describe={(id) => q.data!.terms.find((t) => t.id === id)?.name ?? id} />
                </>
              )
            }
            if (view === 'circos') {
              const activeIds = active.map((r) => r.id)
              const shown = [...q.data!.terms].filter((t) => t.runIds.some((r) => activeIds.includes(r)))
                .sort((a, b) => (a.combPvalue ?? 1) - (b.combPvalue ?? 1)).slice(0, circosTop === 'all' ? undefined : circosTop)
              const maxL = Math.max(1, ...shown.map((t) => -Math.log10(Math.max(t.combPvalue ?? 1, 1e-300))))
              const srcColor = (src: string) => categoricalColor(pm, src, q.data!.availableSources, config.datasourceColors[src])
              const ordered = [...shown].sort((a, b) => a.source.localeCompare(b.source) || (a.combPvalue ?? 1) - (b.combPvalue ?? 1))
              const items = active.flatMap((r) => ordered.filter((t) => t.runIds.includes(r.id)).map((t) => ({
                id: `${r.id}::${t.id}`, sector: r.id, label: labels === 'Term_ID' ? t.id : t.name, color: srcColor(t.source),
                value: -Math.log10(Math.max(t.combPvalue ?? 1, 1e-300)) / maxL,
                title: `${t.name} (${t.id})\n${t.source} · found by ${t.runIds.filter((x) => activeIds.includes(x)).length} of ${activeIds.length} runs · combined p = ${formatP(t.combPvalue)}` })))
              const links = shown.flatMap((t) => {
                const rs = t.runIds.filter((r) => activeIds.includes(r))
                return rs.flatMap((a, i) => rs.slice(i + 1).map((b) => ({ source: `${a}::${t.id}`, target: `${b}::${t.id}`, color: srcColor(t.source), title: `${t.name}: ${a} ↔ ${b}` })))
              })
              const selKeys = new Set(items.filter((it) => shownSel.has(it.id.split('::')[1])).map((it) => it.id))
              return (
                <>
                  <div className="toolbar">
                    <label>Show <select className="input input-sm" value={String(circosTop)} onChange={(e) => setCircosTop(e.target.value === 'all' ? 'all' : Number(e.target.value))}>
                      {[30, 60, 100, 200].map((n) => <option key={n} value={n}>top {n} terms</option>)}<option value="all">all terms</option></select></label>
                  </div>
                  <p className="muted small">One sector per run, holding the terms it found (coloured by source, in the same order in every run). Bundled links join the same term across runs, so consensus terms form thick bundles and run-specific terms have none. Outer bars: combined −log10 p.</p>
                  <CircosBundle sectors={active.map((r, i) => ({ id: r.id, label: r.label, color: runColors[i] }))} items={items} links={links}
                    svgRef={svgRef} exportName="flame_comparison_circos" selected={selKeys} trackLabel="combined −log10 p" linkLabel="shared-term links" defaultLabels={items.length <= 120}
                    onSelect={(ids, add) => onSelect([...new Set(ids.map((id) => id.split('::')[1]))], add)} />
                </>
              )
            }
            const ids = active.map((r) => r.id)
            const m = ids.map(() => ids.map(() => 0))
            q.data!.terms.forEach((t) => {
              const rs = t.runIds.filter((r) => ids.includes(r))
              rs.forEach((a) => rs.forEach((b) => { if (a !== b) m[ids.indexOf(a)][ids.indexOf(b)] += 1 }))
            })
            return (
              <>
                <p className="muted small">Arcs are runs; ribbons show how many terms two runs share. Click a ribbon to select the terms both runs found.</p>
                {m.every((row) => row.every((v) => v === 0)) ? <Note kind="warning">The runs share no terms under the current filters.</Note> :
                  <ChordDiagram names={active.map((r) => r.label)} matrix={m} colors={runColors} svgRef={svgRef} exportName="flame_comparison_chord" unit="shared terms"
                    onSelect={(i, j) => onSelect(q.data!.terms.filter((t) => t.runIds.includes(ids[i]) && (j === undefined || t.runIds.includes(ids[j]))).map((t) => t.id), false)} />}
              </>
            )
          })()}
          {view === 'table' && q.data && (
            <CompareTable terms={q.data.terms} runs={q.data.runs.filter((r) => q.data!.activeRunIds.includes(r.id))} selected={selected}
              onSelect={onSelect} labels={labels}
              colors={Object.fromEntries(q.data.runs.map((r, i, all) => [r.id, categoricalColor(pm, r.tool, all.map((x) => x.tool), ['#f97316', '#8b5cf6', '#e11d48', '#6366f1'][i % 4])]))} />
          )}
          {view === 'ranks' && q.data && (
            <RankChart terms={q.data.terms} runs={q.data.runs.filter((r) => q.data!.activeRunIds.includes(r.id))} selected={shownSel} onSelect={onSelect} labels={labels}
              colorOf={(src) => categoricalColor(pm, src, q.data?.availableSources ?? [], config.datasourceColors[src])} />
          )}
          {view === 'themes' && (
            <ThemesView path="/comparisons/themes" body={{ runIds, filters: f }} baseName="flame_comparison" terms={all.data?.terms ?? []}
              onSelect={onSelect} colorOf={(src) => categoricalColor(pm, src, q.data?.availableSources ?? [], config.datasourceColors[src])} />
          )}
          {view === 'stats' && q.data && (
            <ComparisonStatistics runIds={runIds} filters={f} onSelect={onSelect}
              colors={Object.fromEntries(q.data.runs.map((r, i, all) => [r.id, categoricalColor(pm, r.tool, all.map((x) => x.tool), ['#f97316', '#8b5cf6', '#e11d48', '#6366f1'][i % 4])]))} />
          )}
          {view !== 'overview' && view !== 'upset' && view !== 'chord' && view !== 'circos' && view !== 'stats' && view !== 'table' && view !== 'ranks' && view !== 'themes' && (
            <div className="toolbar">
              <label>Show <select className="input input-sm" value={String(top)} onChange={(e) => setTop(e.target.value === 'all' ? 'all' : Number(e.target.value))}>
                {[10, 20, 30, 50].map((n) => <option key={n} value={n}>top {n}</option>)}<option value="all">all</option></select></label>
              <label>by <select className="input input-sm" value={sortBy} onChange={(e) => setSortBy(e.target.value)}>{SORTS.map((s) => <option key={s.value} value={s.value}>{s.label}</option>)}</select></label>
              {view === 'bubble' && <label className="check"><input type="checkbox" checked={showLabels} onChange={(e) => setShowLabels(e.target.checked)} /> Show labels</label>}
              <label>Labels <select className="input input-sm" value={labels} onChange={(e) => setLabels(e.target.value as 'Function' | 'Term_ID')}><option value="Function">Term names</option><option value="Term_ID">Term IDs</option></select></label>
              {view === 'network' && <label>Layout <LayoutSelect style={netStyle.style} onChange={netStyle.setStyle} /></label>}
              {view === 'network' && <NetworkOptions style={netStyle.style} onChange={netStyle.setStyle} onReset={netStyle.reset} handle={networkRef} />}
              <span className="toolbar-spacer" />
              <ExportMenu items={[{ label: 'Image (PNG, SVG, PDF, JPEG, WebP)…', run: () => (view === 'network' ? networkRef.current?.exportImage('flame_comparison_network') : plotRef.current?.exportImage(`flame_comparison_${view}`)) }]} />
            </div>
          )}
          <ErrorNote error={q.error} />
          {q.isLoading && <Spinner label="Combining runs" />}
          {q.data && !q.data.terms.length && <Note kind="warning">No terms match the current filters.</Note>}
          {q.data && q.data.terms.length > 0 && view === 'overview' && q.data.chart && (
            <Overview data={q.data} colors={roleColors(pm).series} onBubble={(metric, runId) => overviewClick('bubble', metric, runId)} onPin={(metric) => overviewClick('pin', metric)} plotRef={plotRef} />
          )}
          {q.data && q.data.terms.length > 0 && view === 'bubble' && (
            <Plot handleRef={plotRef} exportName={`flame_comparison_${view}`} height={520} onSelect={onSelect} data={[...new Set(q.data.terms.map((t) => t.source))].map((src) => {
              const ts = q.data!.terms.filter((t) => t.source === src)
              return {
                type: 'scatter' as const, mode: (showLabels ? 'text+markers' : 'markers') as 'markers', name: src, x: ts.map((t) => t.foldEnrichment ?? 0), y: ts.map((t) => t.negLog10p ?? 0),
                customdata: ts.map((t) => t.id), text: ts.map(label), textposition: 'top center' as const, textfont: { size: 10 },
                hovertext: ts.map((t) => `${t.name}<br>${t.id}<br>found by ${t.agreement} run(s)<br>combined p = ${formatP(t.activeCombPvalue)}`), hoverinfo: 'text' as const,
                marker: { size: ts.map((t) => 10 + 8 * (t.agreement ?? 1)), color: categoricalColor(pm, src, q.data!.availableSources, config.datasourceColors[src]), opacity: 0.8,
                  line: { width: ts.map((t) => (shownSel.has(t.id) ? 3 : 0)), color: '#f97316' } },
              }
            })} layout={{ xaxis: { title: { text: 'Mean fold enrichment (active runs)' } }, yaxis: { title: { text: '−log10 combined p-value' } }, legend: { orientation: 'h', y: -0.2 } }} />
          )}
          {q.data && q.data.terms.length > 0 && view === 'network' && (() => {
            // Runs (tools) as boxes, the shown terms as dots; a link means "this run found this term".
            const d = q.data!
            const active = d.runs.filter((r) => d.activeRunIds.includes(r.id))
            const runColor = (r: (typeof active)[number]) => categoricalColor(pm, r.tool, d.runs.map((x) => x.tool), ['#f97316', '#8b5cf6', '#e11d48', '#6366f1'][d.runs.indexOf(r) % 4])
            const srcColor = (src: string) => categoricalColor(pm, src, d.availableSources, config.datasourceColors[src])
            const termIds = new Set(d.terms.map((t) => t.id))
            const nodes = [
              ...active.map((r) => ({ id: `run:${r.id}`, label: r.label, title: `${r.label} · ${r.listName} — click to select the terms it found`, color: runColor(r), shape: 'box', size: 22 })),
              ...d.terms.map((t) => ({ id: t.id, label: label(t), title: `${t.name} (${t.id}, ${t.source}) · found by ${t.agreement ?? t.runIds.length} of ${active.length} runs · combined p = ${formatP(t.activeCombPvalue)}`,
                color: srcColor(t.source), shape: 'dot', size: 8 + 4 * (t.agreement ?? 1) })),
            ]
            const edges = d.terms.flatMap((t) => t.runIds.filter((r) => d.activeRunIds.includes(r)).map((r) => ({ id: `${r}|${t.id}`, from: `run:${r}`, to: t.id, color: runColor(active.find((x) => x.id === r)!) })))
            const pick = (ids: string[], add: boolean) => {
              // A run node stands for every shown term it found.
              const terms = ids.flatMap((id) => (id.startsWith('run:') ? d.terms.filter((t) => t.runIds.includes(id.slice(4))).map((t) => t.id) : termIds.has(id) ? [id] : []))
              if (terms.length) onSelect([...new Set(terms)], add)
            }
            return (
              <>
                <NetworkGraph exportName="flame_comparison_network" nodes={nodes} edges={edges} handleRef={networkRef} style={netStyle.style} height={560} selected={[...shownSel]} onSelect={pick} />
                <p className="muted small net-legend">
                  <strong>Runs:</strong> {active.map((r) => <span key={r.id} className="legend-item"><i style={{ background: runColor(r), borderRadius: 2 }} />{r.label}</span>)}
                  <strong>Terms by source:</strong> {[...new Set(d.terms.map((t) => t.source))].map((src) => <span key={src} className="legend-item"><i style={{ background: srcColor(src) }} />{src}</span>)}
                </p>
                <p className="muted small">The {d.terms.length} terms shown (set with <em>Show</em> and <em>by</em>) are linked to every run that found them; terms in the middle are shared, terms at the edge belong to one run. Larger dots were found by more runs. Click a term to select it, or a run to select all of its terms; drag nodes to rearrange.</p>
              </>
            )
          })()}
          {q.data && q.data.terms.length > 0 && view === 'heatmap' && (() => {
            const d = q.data!
            const runIdsActive = d.activeRunIds
            const runLabel = (id: string) => d.runs.find((r) => r.id === id)?.label ?? id
            const val = new Map((d.cells ?? []).map((c) => [`${c.term}|${c.run}`, c.value]))
            return <Plot handleRef={plotRef} exportName={`flame_comparison_${view}`} height={Math.max(360, 26 * d.terms.length + 120)} onSelect={onSelect} data={[{
              type: 'heatmap' as const, x: runIdsActive.map(runLabel), y: d.terms.map((t) => `${label(t)} [${t.source}]`),
              z: d.terms.map((t) => runIdsActive.map((r) => val.get(`${t.id}|${r}`) ?? null)) as number[][],
              customdata: d.terms.map((t) => runIdsActive.map(() => t.id)) as unknown as string[],
              colorscale: sequentialScale(pm, 'YlOrRd'), reversescale: reverseScale(sequentialScale(pm, 'YlOrRd')), colorbar: { title: { text: '−log10 p' }, thickness: 12 }, xgap: 2, ygap: 1, hoverongaps: false,
              hovertemplate: '%{y}<br>%{x}: −log10 p = %{z:.2f}<extra></extra>',
            }]} layout={{ margin: { l: 360, r: 20, t: 10, b: 80 }, yaxis: { automargin: true, autorange: 'reversed' } }} />
          })()}
          {(view === 'bubble' || view === 'heatmap') && <p className="muted small">Blank heatmap cells: the run did not report that term. Click points or cells to select terms.</p>}
        </section>

        <aside className="selection" aria-label="Selected terms">
          <div className="row between"><h2>Selected <span className="count">{selectedTerms.length}</span></h2>
            {selectedTerms.length > 0 && <button className="btn btn-ghost btn-sm" onClick={() => setSelected(new Set())}>Clear</button>}</div>
          {selectedTerms.length > 0 && (
            <div className="row wrap sel-copy">
              <CopyButton label="Copy term names" text={selectedTerms.map((t) => t.name).join('\n')} />
              <CopyButton label="Copy all genes" title="Copy every gene of the selected terms, one per line" text={[...new Set(selectedTerms.flatMap((t) => splitGenes(t.unionHits)))].join('\n')} />
            </div>
          )}
          {!selectedTerms.length && <p className="muted small">Click bubbles, pins, points or cells to collect terms here.</p>}
          <ul className="plain-list">
            {selectedTerms.slice(0, 300).map((t) => (
              <li key={t.id} className="sel-item">
                <div className="row between"><span className="sel-title"><span className="dot" style={{ background: config.datasourceColors[t.source] }} />{t.name}</span>
                  <span className="row">
                    <CopyButton compact text={splitGenes(t.unionHits).join('\n')} title={`Copy the genes of ${t.name}`} />
                    <button className="icon-btn" aria-label={`Remove ${t.name}`} onClick={() => onSelect([t.id], true)}>×</button>
                  </span></div>
                <div className="small muted">{t.id} · {t.tools} · p = {formatP(t.combPvalue)}</div>
                <div className="gene-chips">{splitGenes(t.unionHits).slice(0, 40).map((g) => <span key={g} className="gene">{g}</span>)}
                  {splitGenes(t.unionHits).length > 40 && <span className="muted small">+{splitGenes(t.unionHits).length - 40} more (copy to get all)</span>}</div>
              </li>
            ))}
          </ul>
          {selectedTerms.length > 0 && <SelectionExports terms={selectedTerms} />}
        </aside>
      </div>
    </div>
  )
}

function SelectionExports({ terms }: { terms: ComparisonTerm[] }) {
  const cols = [{ key: 'source', label: 'Source' }, { key: 'id', label: 'Term ID' }, { key: 'name', label: 'Term' }, { key: 'tools', label: 'Found by' },
    { key: 'rank', label: 'Runs' }, { key: 'combPvalue', label: 'Combined p' }, { key: 'termSize', label: 'Term size' }, { key: 'unionHits', label: 'Genes (any run)' }, { key: 'intersectionHits', label: 'Genes (all runs)' }]
  const rows = terms.map((t) => ({ ...t, combPvalue: formatP(t.combPvalue), termSize: formatNum(t.termSize) }))
  const html = toHtmlTable('Selected terms (comparison)', rows, cols)
  return (
    <div className="row wrap">
      <CopyButton text={terms.map((t) => `${t.id}\t${t.name}\t${t.tools}\t${formatP(t.combPvalue)}`).join('\n')} />
      <button className="btn btn-ghost btn-sm" onClick={() => saveText(toCsv(rows, cols), 'flame_comparison_selection.csv', 'text/csv')}>CSV</button>
      <button className="btn btn-ghost btn-sm" onClick={() => saveXlsx(sheetsByCategory(terms.map((t) => ({ Source: t.source, 'Term ID': t.id, Term: t.name, 'Found by': t.tools,
        Runs: t.rank, 'Combined p': t.combPvalue, 'Term size': t.termSize, 'Genes (all runs)': t.intersectionHits, 'Genes (any run)': t.unionHits })), 'Source', 'Selected terms'),
        'flame_comparison_selection.xlsx')}>Excel</button>
      <button className="btn btn-ghost btn-sm" onClick={() => saveText(html, 'flame_comparison_selection.html', 'text/html')}>HTML</button>
      <button className="btn btn-ghost btn-sm" onClick={() => { const w = window.open(URL.createObjectURL(new Blob([html], { type: 'text/html' })), '_blank'); w?.addEventListener('load', () => w.print()) }}>Print / PDF</button>
    </div>
  )
}

function Overview({ data, colors: series, onBubble, onPin, plotRef }: { data: ComparisonData; colors: string[]; onBubble: (metric: string, runId: string) => void; onPin: (metric: string) => void; plotRef: { current: PlotHandle | null } }) {
  const chart = data.chart!
  const metrics = ['termsFound', 'exclusiveTerms', 'consensusBlocker'] as const
  const colors = { termsFound: series[0], exclusiveTerms: series[1], consensusBlocker: series[2] }
  return (
    <div className="stack">
      <div className="pin-grid">
        {(['uniqueTerms', 'consensus', 'singleRun'] as const).map((k) => (
          <button key={k} className="pin-card" onClick={() => onPin(k)} title={`${chart.metricDescriptions[k]} — click to select these terms`}>
            <span className="pin-value">{chart.pins[k]}</span>
            <span className="pin-label">{chart.pinLabels[k]}</span>
            <span className="muted small">{chart.metricDescriptions[k]}</span>
          </button>
        ))}
      </div>
      <Plot handleRef={plotRef} exportName="flame_comparison_overview" height={380} data={metrics.map((m) => ({
        type: 'bar' as const, name: chart.metricLabels[m], x: chart.runs.map((r) => r.runLabel), y: chart.runs.map((r) => r[m]),
        customdata: chart.runs.map((r) => `${m}|${r.runId}`), marker: { color: colors[m] }, hovertemplate: `%{x}<br>${chart.metricLabels[m]}: %{y}<extra></extra>`,
      }))} layout={{ barmode: 'group', yaxis: { title: { text: 'Terms' } }, legend: { orientation: 'h', y: -0.2 } }}
        onSelect={(keys) => { const [m, r] = (keys[0] ?? '').split('|'); if (m && r) onBubble(m, r) }} />
      <ul className="muted small">
        <li><strong>Terms found</strong>: {chart.metricDescriptions.termsFound}.</li>
        <li><strong>Exclusive terms</strong>: {chart.metricDescriptions.exclusiveTerms}.</li>
        <li><strong>Consensus blocker</strong>: {chart.metricDescriptions.consensusBlocker} — the run that keeps these terms out of the consensus.</li>
      </ul>
      <p className="muted small">Click a bar or a summary card to select those terms.</p>
    </div>
  )
}
