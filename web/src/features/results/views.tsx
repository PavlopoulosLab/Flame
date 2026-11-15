import { useMemo, useState } from 'react'
import type { AppConfig, Term, ViewData } from '../../api/types'
import { NetworkGraph, Plot, type NetworkHandle, type PlotHandle } from '../../components/charts'
import { formatNum, formatP, isHttpUrl, splitGenes } from '../../lib/format'

export interface ViewSettings {
  labels?: 'name' | 'id'
  xAxis?: 'geneRatio' | 'foldEnrichment' | 'coverage'
  palette?: 'OrRd' | 'Viridis' | 'Cividis' | 'Magma'
  barMetric?: 'log10p' | 'coverage'
  orientation?: 'terms-rows' | 'genes-rows'
  layout?: string
  similarityCutoff?: number
  minShared?: number
}

export interface ViewProps {
  data: ViewData
  config: AppConfig
  settings: ViewSettings
  selected: Set<string>
  onSelect: (termIds: string[], additive: boolean) => void
  plotRef: { current: PlotHandle | null }
  networkRef: { current: NetworkHandle | null }
}

const label = (t: Term, s: ViewSettings, max = 55) => {
  const text = s.labels === 'id' ? t.id : t.name
  return text.length > max ? `${text.slice(0, max - 1)}…` : text
}
const color = (cfg: AppConfig, source: string) => cfg.datasourceColors[source] ?? '#9ca3af'

function bySource(terms: Term[]) {
  const m = new Map<string, Term[]>()
  terms.forEach((t) => m.set(t.source, [...(m.get(t.source) ?? []), t]))
  return m
}

const hover = (t: Term) =>
  `<b>${escapeForPlotly(t.name)}</b><br>${escapeForPlotly(t.id)} · ${escapeForPlotly(t.source)}<br>p = ${formatP(t.pvalue)}<br>Genes: ${t.intersection} of ${t.termSize}`

// Plotly hovertext accepts a small HTML subset; escape data so names render as text.
function escapeForPlotly(s: string) {
  return String(s ?? '').replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' })[c]!)
}

const selLine = (ids: string[], selected: Set<string>) => ({
  color: ids.map((id) => (selected.has(id) ? '#f97316' : 'rgba(0,0,0,0)')),
  width: ids.map((id) => (selected.has(id) ? 3 : 0)),
})

export function DotplotView({ data, settings, selected, onSelect, plotRef }: ViewProps) {
  const terms = [...data.terms].reverse()
  const xKey = settings.xAxis ?? 'geneRatio'
  const xLabel = { geneRatio: 'Gene ratio', foldEnrichment: 'Fold enrichment', coverage: 'Term coverage (%)' }[xKey]
  const plot = useMemo(() => [{
    type: 'scatter' as const, mode: 'markers' as const,
    x: terms.map((t) => t[xKey] ?? 0), y: terms.map((t) => label(t, settings)), customdata: terms.map((t) => t.id),
    text: terms.map(hover), hoverinfo: 'text' as const,
    marker: {
      size: terms.map((t) => 8 + Math.sqrt(t.intersection) * 4), color: terms.map((t) => t.log10p),
      colorscale: settings.palette === 'Viridis' ? 'Viridis' : settings.palette === 'Cividis' ? 'Cividis' : settings.palette === 'Magma' ? 'Hot' : 'OrRd',
      // Higher −log10 p (more significant) gets the darker / stronger colour.
      reversescale: false,
      showscale: true, colorbar: { title: { text: '−log10 p' }, thickness: 12 },
      line: selLine(terms.map((t) => t.id), selected),
    },
  }], [terms, settings, xKey, selected])
  if (xKey === 'foldEnrichment' && data.terms.every((t) => t.foldEnrichment == null)) {
    return <p className="muted">Fold enrichment needs the background size, which this tool does not report. Choose another x-axis.</p>
  }
  return <Plot data={plot} height={Math.max(360, 26 * terms.length + 80)} handleRef={plotRef} onSelect={onSelect}
    layout={{ margin: { l: 320, r: 20, t: 10, b: 50 }, xaxis: { title: { text: xLabel } }, yaxis: { automargin: true } }} />
}

export function BarchartView({ data, config, settings, selected, onSelect, plotRef }: ViewProps) {
  const metric = settings.barMetric ?? 'log10p'
  const terms = [...data.terms].reverse()
  const plot = useMemo(() => [...bySource(terms)].map(([src, ts]) => ({
    type: 'bar' as const, orientation: 'h' as const, name: src,
    x: ts.map((t) => (metric === 'log10p' ? t.log10p : t.coverage)), y: ts.map((t) => label(t, settings)),
    customdata: ts.map((t) => t.id), text: ts.map(hover), hoverinfo: 'text' as const, textposition: 'none' as const,
    marker: { color: color(config, src), line: selLine(ts.map((t) => t.id), selected) },
  })), [terms, metric, settings, config, selected])
  return <Plot data={plot} height={Math.max(360, 24 * terms.length + 80)} handleRef={plotRef} onSelect={onSelect}
    layout={{ barmode: 'stack', margin: { l: 320, r: 20, t: 10, b: 50 }, xaxis: { title: { text: metric === 'log10p' ? '−log10 p-value' : 'Term coverage (%)' } },
      yaxis: { automargin: true, categoryorder: 'array', categoryarray: terms.map((t) => label(t, settings)) }, legend: { orientation: 'h', y: -0.15 } }} />
}

export function ScatterView({ data, config, selected, onSelect, plotRef }: ViewProps) {
  const plot = useMemo(() => [...bySource(data.terms)].map(([src, ts]) => ({
    type: 'scatter' as const, mode: 'markers' as const, name: src,
    x: ts.map((t) => t.log10p), y: ts.map((t) => t.coverage), customdata: ts.map((t) => t.id),
    text: ts.map(hover), hoverinfo: 'text' as const,
    marker: { size: 11, color: color(config, src), opacity: 0.85, line: selLine(ts.map((t) => t.id), selected) },
  })), [data.terms, config, selected])
  return <Plot data={plot} height={480} handleRef={plotRef} onSelect={onSelect}
    layout={{ xaxis: { title: { text: '−log10 p-value' } }, yaxis: { title: { text: 'Term coverage (%)' } }, legend: { orientation: 'h', y: -0.2 } }} />
}

/** Heatmaps: T×G presence, T×T similarity %, G×G shared terms. */
export function HeatmapView({ data, settings, onSelect, plotRef }: ViewProps) {
  const termById = new Map(data.terms.map((t) => [t.id, t]))
  const cells = data.cells ?? []
  const plot = useMemo(() => {
    if (data.view === 'heatmap-tg') {
      const termIds = data.terms.map((t) => t.id)
      const genes = [...new Set(cells.map((c) => String(c.gene)))].sort()
      const has = new Set(cells.map((c) => `${c.term}|${c.gene}`))
      const rowsAreTerms = settings.orientation !== 'genes-rows'
      const rows = rowsAreTerms ? termIds : genes
      const cols = rowsAreTerms ? genes : termIds
      const name = (id: string) => (termById.has(id) ? label(termById.get(id)!, settings, 40) : id)
      return [{
        type: 'heatmap' as const, x: cols.map(name), y: rows.map(name), showscale: false,
        z: rows.map((r) => cols.map((c) => (has.has(rowsAreTerms ? `${r}|${c}` : `${c}|${r}`) ? 1 : 0))),
        customdata: rows.map((r) => cols.map((c) => (rowsAreTerms ? r : c))) as unknown as string[],
        colorscale: [[0, 'rgba(0,0,0,0.03)'], [1, '#f97316']] as never, hovertemplate: '%{y} · %{x}<extra></extra>', xgap: 1, ygap: 1,
      }]
    }
    if (data.view === 'heatmap-tt') {
      const ids = data.terms.map((t) => t.id)
      const sim = new Map(cells.map((c) => [`${c.source}|${c.target}`, Number(c.similarity)]))
      const names = ids.map((id) => label(termById.get(id)!, settings, 40))
      return [{
        type: 'heatmap' as const, x: names, y: names, z: ids.map((a) => ids.map((b) => sim.get(`${a}|${b}`) ?? 0)),
        customdata: ids.map((a) => ids.map((b) => `${a},${b}`)) as unknown as string[],
        colorscale: 'YlOrRd', reversescale: true, colorbar: { title: { text: 'Similarity %' }, thickness: 12 }, hovertemplate: '%{y}<br>%{x}<br>%{z}% shared genes<extra></extra>',
      }]
    }
    const genes = [...new Set(cells.map((c) => String(c.source)))].sort()
    const shared = new Map(cells.map((c) => [`${c.source}|${c.target}`, Number(c.shared)]))
    return [{
      type: 'heatmap' as const, x: genes, y: genes, z: genes.map((a) => genes.map((b) => shared.get(`${a}|${b}`) ?? 0)),
      customdata: genes.map((a) => genes.map((b) => `gene:${a},gene:${b}`)) as unknown as string[],
      colorscale: 'Blues', reversescale: true, colorbar: { title: { text: 'Shared terms' }, thickness: 12 }, hovertemplate: '%{y} · %{x}<br>%{z} shared terms<extra></extra>',
    }]
  }, [data.view, cells, data.terms, settings, termById])
  const n = data.view === 'heatmap-gg' ? new Set(cells.map((c) => c.source)).size : data.terms.length
  // Cell clicks: customdata holds term ids, "a,b" term pairs, or "gene:A,gene:B" gene pairs.
  const resolve = (keys: string[], additive: boolean) => {
    const out = new Set<string>()
    keys.forEach((k) => k.split(',').forEach((part) => {
      if (part.startsWith('gene:')) return
      if (termById.has(part)) out.add(part)
    }))
    const genes = keys.flatMap((k) => k.split(',').filter((p) => p.startsWith('gene:')).map((p) => p.slice(5)))
    if (genes.length) data.terms.filter((t) => genes.every((g) => splitGenes(t.genes).includes(g))).forEach((t) => out.add(t.id))
    onSelect([...out], additive)
  }
  return <Plot data={plot} height={Math.max(420, 18 * n + 160)} handleRef={plotRef} onSelect={resolve}
    layout={{ margin: { l: 260, r: 20, t: 10, b: 160 }, xaxis: { tickangle: -45, automargin: true }, yaxis: { automargin: true } }} />
}

const SHAPES: Record<string, string> = {
  'GO:MF': 'hexagon', 'GO:BP': 'hexagon', 'GO:CC': 'hexagon', KEGG: 'diamond', REAC: 'diamond', WP: 'diamond',
  DO: 'triangleDown', DISGENET: 'triangleDown', OMIM: 'triangleDown', DRUGBANK: 'star', INTERPRO: 'star', PFAM: 'star', Gene: 'dot',
}

export function NetworkView({ data, config, settings, selected, onSelect, networkRef }: ViewProps) {
  const net = data.network
  const nodes = useMemo(() => (net?.nodes ?? []).map((n) => ({
    id: n.id, x: n.x, y: n.y, label: n.kind === 'gene' ? n.label : settings.labels === 'id' ? n.id : truncate(n.name, 30),
    title: n.kind === 'gene' ? n.label : `${n.name} (${n.id}, ${n.group})`,
    color: n.kind === 'gene' ? config.geneNodeColor : color(config, n.group), shape: SHAPES[n.group] ?? (n.kind === 'gene' ? 'dot' : 'square'),
  })), [net, settings.labels, config])
  const edges = useMemo(() => (net?.edges ?? []).map((e) => ({ id: e.id, from: e.from, to: e.to, width: e.width,
    title: data.view === 'network-tt' ? `${e.weight}% similar` : data.view === 'network-gg' ? `${e.weight} shared terms` : undefined })), [net, data.view])
  const termIds = new Set(data.terms.map((t) => t.id))
  const selectedNodes = [...selected].filter((id) => nodes.some((n) => n.id === id))
  if (!net || !net.nodes.length) {
    return <p className="muted">No edges pass the current cut-off. Lower the {data.view === 'network-tt' ? 'similarity' : 'shared-terms'} threshold or show more terms.</p>
  }
  return (
    <NetworkGraph nodes={nodes} edges={edges} selected={selectedNodes} handleRef={networkRef} height={600}
      onSelect={(ids, add) => {
        const terms = ids.filter((id) => termIds.has(id))
        const genes = ids.filter((id) => !termIds.has(id))
        const viaGenes = data.terms.filter((t) => genes.some((g) => splitGenes(t.genes).includes(g))).map((t) => t.id)
        onSelect([...new Set([...terms, ...viaGenes])], add)
      }} />
  )
}

function GenesCell({ genes }: { genes: string }) {
  const list = splitGenes(genes)
  const shown = list.slice(0, 12).join(', ')
  return <span title={list.join(', ')}>{shown}{list.length > 12 ? <span className="muted"> … +{list.length - 12} more</span> : null}</span>
}

const truncate = (s: string, n: number) => (s.length > n ? `${s.slice(0, n - 1)}…` : s)

export const TABLE_COLUMNS: { key: keyof Term | 'genesCount'; label: string; numeric?: boolean }[] = [
  { key: 'source', label: 'Source' }, { key: 'id', label: 'Term ID' }, { key: 'name', label: 'Term' },
  { key: 'pvalue', label: 'P-value', numeric: true }, { key: 'log10p', label: '−log10 p', numeric: true },
  { key: 'termSize', label: 'Term size', numeric: true }, { key: 'intersection', label: 'Genes found', numeric: true },
  { key: 'coverage', label: 'Coverage %', numeric: true }, { key: 'geneRatio', label: 'Gene ratio', numeric: true },
  { key: 'foldEnrichment', label: 'Fold enrichment', numeric: true }, { key: 'genes', label: 'Genes' },
]

export function TableView({ data, config, selected, onSelect, columns }: ViewProps & { columns: string[] }) {
  const [sort, setSort] = useState<{ key: string; dir: 1 | -1 } | null>(null)
  const cols = TABLE_COLUMNS.filter((c) => columns.includes(c.key))
  const rows = useMemo(() => {
    if (!sort) return data.terms
    return [...data.terms].sort((a, b) => {
      const x = a[sort.key as keyof Term], y = b[sort.key as keyof Term]
      return (typeof x === 'number' && typeof y === 'number' ? x - y : String(x).localeCompare(String(y))) * sort.dir
    })
  }, [data.terms, sort])
  return (
    <div className="table-scroll results-table">
      <table className="table">
        <thead>
          <tr>
            <th aria-label="Selected" />
            {cols.map((c) => (
              <th key={c.key} aria-sort={sort?.key === c.key ? (sort.dir === 1 ? 'ascending' : 'descending') : 'none'}>
                <button className="th-btn" onClick={() => setSort((s) => (s?.key === c.key ? { key: c.key, dir: (s.dir * -1) as 1 | -1 } : { key: c.key, dir: c.numeric ? -1 : 1 }))}>
                  {c.label}{sort?.key === c.key ? (sort.dir === 1 ? ' ▲' : ' ▼') : ''}
                </button>
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((t) => (
            <tr key={t.id} className={selected.has(t.id) ? 'selected' : ''} onClick={() => onSelect([t.id], true)}>
              <td><input type="checkbox" checked={selected.has(t.id)} readOnly aria-label={`Select ${t.name}`} /></td>
              {cols.map((c) => (
                <td key={c.key} className={c.key === 'genes' ? 'genes-cell' : ''}>
                  {c.key === 'source' ? <span className="source-tag" style={{ borderColor: color(config, t.source) }}>{t.source}</span>
                    : c.key === 'id' && isHttpUrl(t.link) ? <a href={t.link} target="_blank" rel="noopener noreferrer" onClick={(e) => e.stopPropagation()}>{t.id}</a>
                    : c.key === 'genes' ? <GenesCell genes={t.genes} />
                    : c.key === 'pvalue' ? formatP(t.pvalue)
                    : c.numeric ? formatNum(t[c.key as keyof Term] as number)
                    : String(t[c.key as keyof Term] ?? '')}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}
