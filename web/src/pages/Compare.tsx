import { keepPreviousData, useQuery } from '@tanstack/react-query'
import { useEffect, useMemo, useRef, useState } from 'react'
import { Link, useSearchParams } from 'react-router-dom'
import { api, getWorkspaceId, saveText } from '../api/client'
import { useConfig } from '../api/hooks'
import type { ComparisonData, ComparisonFilters, ComparisonTerm } from '../api/types'
import { Plot, type PlotHandle } from '../components/charts'
import { CopyButton, ErrorNote, Field, Note, Segmented, Spinner } from '../components/ui'
import { ExportMenu } from './Results'
import { formatNum, formatP, plural, splitGenes, toCsv, toHtmlTable } from '../lib/format'

type View = 'overview' | 'bubble' | 'heatmap'
const SORTS = [{ value: '-log10Pvalue', label: 'Combined significance' }, { value: 'Fold Enrichment', label: 'Fold enrichment' }, { value: 'Agreement', label: 'Agreement between runs' }]

function useDebounced<T>(v: T, ms = 300) {
  const [d, setD] = useState(v)
  useEffect(() => { const h = setTimeout(() => setD(v), ms); return () => clearTimeout(h) }, [v, ms])
  return d
}

export default function ComparePage() {
  const ws = getWorkspaceId()!
  const [params] = useSearchParams()
  const runIds = (params.get('runs') ?? '').split(',').filter(Boolean)
  const { data: config } = useConfig()
  const [view, setView] = useState<View>('overview')
  const [filters, setFilters] = useState<ComparisonFilters>({ runsMode: 'any' })
  const [top, setTop] = useState<number | 'all'>(20)
  const [sortBy, setSortBy] = useState('-log10Pvalue')
  const [labels, setLabels] = useState<'Function' | 'Term_ID'>('Function')
  const [showLabels, setShowLabels] = useState(false)
  const [selected, setSelected] = useState<Set<string>>(new Set())
  const plotRef = useRef<PlotHandle | null>(null)
  const f = useDebounced(filters)

  const q = useQuery({
    queryKey: ['compare', ws, runIds.join(','), view, f, top, sortBy],
    queryFn: () => api<ComparisonData>('/comparisons', { body: { runIds, view, filters: f, top, sortBy, settings: { labels } } }),
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
        <Link to={`/w/${ws}`} className="btn btn-ghost btn-sm">← Workspace</Link>
        <div>
          <h1>Comparing {plural(runIds.length, 'run')}</h1>
          <p className="muted small">{runs.map((r) => `${r.label} · ${r.listName}`).join('  |  ')}</p>
        </div>
      </header>
      <p className="muted small">Terms found by several runs get a combined p-value (Fisher's method). Agreement = how many of the active runs found a term.</p>

      <nav className="view-tabs" aria-label="Comparison views">
        {(['overview', 'bubble', 'heatmap'] as View[]).map((v) => (
          <span key={v} className={`view-tab${v === view ? ' active' : ''}`}><button onClick={() => setView(v)}>{v === 'overview' ? 'Overview' : v === 'bubble' ? 'Bubble plot' : 'Heatmap'}</button></span>
        ))}
      </nav>

      <div className="results-body">
        <aside className="filters" aria-label="Filters">
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
          {view !== 'overview' && (
            <div className="toolbar">
              <label>Show <select className="input input-sm" value={String(top)} onChange={(e) => setTop(e.target.value === 'all' ? 'all' : Number(e.target.value))}>
                {[10, 20, 30, 50].map((n) => <option key={n} value={n}>top {n}</option>)}<option value="all">all</option></select></label>
              <label>by <select className="input input-sm" value={sortBy} onChange={(e) => setSortBy(e.target.value)}>{SORTS.map((s) => <option key={s.value} value={s.value}>{s.label}</option>)}</select></label>
              {view === 'bubble' && <label className="check"><input type="checkbox" checked={showLabels} onChange={(e) => setShowLabels(e.target.checked)} /> Show labels</label>}
              <label>Labels <select className="input input-sm" value={labels} onChange={(e) => setLabels(e.target.value as 'Function' | 'Term_ID')}><option value="Function">Term names</option><option value="Term_ID">Term IDs</option></select></label>
              <span className="toolbar-spacer" />
              <ExportMenu items={[{ label: 'PNG image', run: () => plotRef.current?.toImage('png', 'flame_comparison') }, { label: 'SVG image', run: () => plotRef.current?.toImage('svg', 'flame_comparison') }]} />
            </div>
          )}
          <ErrorNote error={q.error} />
          {q.isLoading && <Spinner label="Combining runs" />}
          {q.data && !q.data.terms.length && <Note kind="warning">No terms match the current filters.</Note>}
          {q.data && q.data.terms.length > 0 && view === 'overview' && q.data.chart && (
            <Overview data={q.data} onBubble={(metric, runId) => overviewClick('bubble', metric, runId)} onPin={(metric) => overviewClick('pin', metric)} plotRef={plotRef} />
          )}
          {q.data && q.data.terms.length > 0 && view === 'bubble' && (
            <Plot handleRef={plotRef} height={520} onSelect={onSelect} data={[...new Set(q.data.terms.map((t) => t.source))].map((src) => {
              const ts = q.data!.terms.filter((t) => t.source === src)
              return {
                type: 'scatter' as const, mode: (showLabels ? 'text+markers' : 'markers') as 'markers', name: src, x: ts.map((t) => t.foldEnrichment ?? 0), y: ts.map((t) => t.negLog10p ?? 0),
                customdata: ts.map((t) => t.id), text: ts.map(label), textposition: 'top center' as const, textfont: { size: 10 },
                hovertext: ts.map((t) => `${t.name}<br>${t.id}<br>found by ${t.agreement} run(s)<br>combined p = ${formatP(t.activeCombPvalue)}`), hoverinfo: 'text' as const,
                marker: { size: ts.map((t) => 10 + 8 * (t.agreement ?? 1)), color: config.datasourceColors[src] ?? '#9ca3af', opacity: 0.8,
                  line: { width: ts.map((t) => (selected.has(t.id) ? 3 : 0)), color: '#f97316' } },
              }
            })} layout={{ xaxis: { title: { text: 'Mean fold enrichment (active runs)' } }, yaxis: { title: { text: '−log10 combined p-value' } }, legend: { orientation: 'h', y: -0.2 } }} />
          )}
          {q.data && q.data.terms.length > 0 && view === 'heatmap' && (() => {
            const d = q.data!
            const runIdsActive = d.activeRunIds
            const runLabel = (id: string) => d.runs.find((r) => r.id === id)?.label ?? id
            const val = new Map((d.cells ?? []).map((c) => [`${c.term}|${c.run}`, c.value]))
            return <Plot handleRef={plotRef} height={Math.max(360, 26 * d.terms.length + 120)} onSelect={onSelect} data={[{
              type: 'heatmap' as const, x: runIdsActive.map(runLabel), y: d.terms.map((t) => `${label(t)} [${t.source}]`),
              z: d.terms.map((t) => runIdsActive.map((r) => val.get(`${t.id}|${r}`) ?? null)) as number[][],
              customdata: d.terms.map((t) => runIdsActive.map(() => t.id)) as unknown as string[],
              colorscale: 'YlOrRd', reversescale: true, colorbar: { title: { text: '−log10 p' }, thickness: 12 }, xgap: 2, ygap: 1, hoverongaps: false,
              hovertemplate: '%{y}<br>%{x}: −log10 p = %{z:.2f}<extra></extra>',
            }]} layout={{ margin: { l: 360, r: 20, t: 10, b: 80 }, yaxis: { automargin: true, autorange: 'reversed' } }} />
          })()}
          {view !== 'overview' && <p className="muted small">Blank heatmap cells: the run did not report that term. Click points or cells to select terms.</p>}
        </section>

        <aside className="selection" aria-label="Selected terms">
          <div className="row between"><h2>Selected <span className="count">{selectedTerms.length}</span></h2>
            {selectedTerms.length > 0 && <button className="btn btn-ghost btn-sm" onClick={() => setSelected(new Set())}>Clear</button>}</div>
          {!selectedTerms.length && <p className="muted small">Click bubbles, pins, points or cells to collect terms here.</p>}
          <ul className="plain-list">
            {selectedTerms.slice(0, 300).map((t) => (
              <li key={t.id} className="sel-item">
                <div className="row between"><span className="sel-title"><span className="dot" style={{ background: config.datasourceColors[t.source] }} />{t.name}</span>
                  <button className="icon-btn" aria-label={`Remove ${t.name}`} onClick={() => onSelect([t.id], true)}>×</button></div>
                <div className="small muted">{t.id} · {t.tools} · p = {formatP(t.combPvalue)}</div>
                <div className="gene-chips">{splitGenes(t.unionHits).slice(0, 20).map((g) => <span key={g} className="gene">{g}</span>)}</div>
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
      <button className="btn btn-ghost btn-sm" onClick={() => saveText(html, 'flame_comparison_selection.xls', 'application/vnd.ms-excel')}>Excel</button>
      <button className="btn btn-ghost btn-sm" onClick={() => saveText(html, 'flame_comparison_selection.html', 'text/html')}>HTML</button>
      <button className="btn btn-ghost btn-sm" onClick={() => { const w = window.open(URL.createObjectURL(new Blob([html], { type: 'text/html' })), '_blank'); w?.addEventListener('load', () => w.print()) }}>Print / PDF</button>
    </div>
  )
}

function Overview({ data, onBubble, onPin, plotRef }: { data: ComparisonData; onBubble: (metric: string, runId: string) => void; onPin: (metric: string) => void; plotRef: { current: PlotHandle | null } }) {
  const chart = data.chart!
  const metrics = ['termsFound', 'exclusiveTerms', 'consensusBlocker'] as const
  const colors = { termsFound: '#f97316', exclusiveTerms: '#2563eb', consensusBlocker: '#a1a1aa' }
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
      <Plot handleRef={plotRef} height={380} data={metrics.map((m) => ({
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
