import { useCallback, useMemo, useState } from 'react'
import { api } from '../../api/client'
import type { AppConfig, Term, ViewData } from '../../api/types'
import { NetworkGraph, Plot, type NetworkHandle, type PlotHandle } from '../../components/charts'
import type { NetworkStyle } from '../../lib/networkStyle'
import { ChordDiagram, CircosBundle, HivePlot } from '../../components/viz'
import { formatNum, formatP, isHttpUrl, splitGenes } from '../../lib/format'
import { GeneList } from '../../components/ui'
import { categoricalColor, reverseScale, roleColors, sequentialScale, usePaletteMode } from '../../lib/palette'

/** Source colours: configured colours, or Okabe–Ito in colour-blind safe mode. */
export function useSourceColor(config: AppConfig, sources: string[]) {
  const cb = usePaletteMode()
  const keyStr = [...new Set(sources)].sort().join('\u0001')
  // Stable between renders, so views that depend on it (e.g. networks) are not rebuilt needlessly.
  return useCallback((s: string) => categoricalColor(cb, s, keyStr ? keyStr.split('\u0001') : [], config.datasourceColors[s] ?? '#9ca3af'), [cb, keyStr, config])
}

export interface ViewSettings {
  labels?: 'name' | 'id'
  xAxis?: 'geneRatio' | 'foldEnrichment' | 'coverage' | 'nes'
  palette?: 'OrRd' | 'Viridis' | 'Cividis' | 'Magma'
  heatScale?: string
  barMetric?: 'log10p' | 'coverage' | 'nes'
  /** Bar colours: by source (default), by significance with a colour scale, or one colour (barColor). */
  barColors?: 'source' | 'direction' | 'OrRd' | 'Viridis' | 'Cividis' | 'Magma' | 'single'
  barColor?: string
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
  networkStyle?: NetworkStyle
  networkRef: { current: NetworkHandle | null }
  /** Default file name for image exports. */
  exportName?: string
  /** Figure title (one plot per annotation source). */
  title?: string
}

const label = (t: Term, s: ViewSettings, max = 55) => {
  const text = s.labels === 'id' ? t.id : t.name
  return text.length > max ? `${text.slice(0, max - 1)}…` : text
}

/** Per-term axis labels, unique within a chart: Plotly merges equal category labels into one row
 *  (e.g. two long names that truncate alike), which breaks the ordering. Repeats get invisible suffixes.
 *  A term listed twice (up and down in a mirrored plot) keeps one row. */
const axisLabels = (terms: Term[], s: ViewSettings) => {
  const seen = new Map<string, number>()
  const out = new Map<string, string>()
  terms.forEach((t) => {
    if (out.has(t.id)) return
    const base = label(t, s), n = seen.get(base) ?? 0
    seen.set(base, n + 1)
    out.set(t.id, base + '\u200b'.repeat(n))
  })
  return out
}
/** Rows of a chart in drawing order (bottom first), one per term. */
const rowOrder = (terms: Term[], labels: Map<string, string>) => [...new Set(terms.map((t) => labels.get(t.id)!))]

/** Up & down figure: up-regulated values to the right, down-regulated always to the left. */
const dirSign = (t: Term) => (t.direction === 'down' ? -1 : 1)
const DIR_LABEL = { up: 'Up-regulated genes', down: 'Down-regulated genes' }
/** Axis ticks for a mirrored axis, labelled with absolute values (|−3| shown as 3). */
const absTicks = (max: number) => {
  const raw = max / 4, mag = 10 ** Math.floor(Math.log10(raw)), step = [1, 2, 5, 10].map((k) => k * mag).find((k) => k >= raw) ?? raw
  const vals: number[] = []
  for (let v = -Math.floor(max / step) * step; v <= max + 1e-9; v += step) vals.push(Number(v.toPrecision(10)))
  return { tickvals: vals, ticktext: vals.map((v) => String(Number(Math.abs(v).toPrecision(4)))) }
}

/** Up & down figure, top-down: up-regulated terms (strongest first), terms enriched in both lists,
 *  then down-regulated terms (strongest last, at the bottom). */
function sortSections(terms: Term[], v: (t: Term) => number) {
  const best = new Map<string, number>()
  terms.forEach((t) => best.set(t.id, Math.max(best.get(t.id) ?? 0, Math.abs(v(t)))))
  const of = (sec: Term['section']) => terms.filter((t) => t.section === sec)
  return [
    ...of('up').sort((a, b) => best.get(b.id)! - best.get(a.id)!),
    ...of('shared').sort((a, b) => best.get(b.id)! - best.get(a.id)! || (a.direction === 'up' ? -1 : 1)),
    ...of('down').sort((a, b) => best.get(a.id)! - best.get(b.id)!),
  ]
}

/**
 * Layout of the up & down figure: rows in the given top-down order (returned bottom first, as
 * Plotly draws them), faint lines between the up, shared and down groups, and an axis centred
 * on zero labelled with absolute values (down to the left, up to the right).
 */
function upDownLayout(terms: Term[], labels: Map<string, string>, values: number[], axisTitle: string) {
  const top: string[] = [], groupOf = new Map<string, Term['section']>()
  terms.forEach((t) => { const l = labels.get(t.id)!; if (!groupOf.has(l)) { top.push(l); groupOf.set(l, t.section) } })
  const rows = [...top].reverse()
  // A line between two neighbouring rows of different groups (category i sits at y = i).
  const shapes = rows.slice(1).flatMap((l, i) => (groupOf.get(l) !== groupOf.get(rows[i])
    ? [{ name: `group-${i}`, type: 'line' as const, xref: 'paper' as const, x0: 0, x1: 1, y0: i + 0.5, y1: i + 0.5, layer: 'below' as const,
        line: { color: 'rgba(128,128,128,0.5)', width: 1, dash: 'dot' as const } }]
    : []))
  const max = Math.max(...values.map(Math.abs), 1e-9) * 1.08
  return {
    rows, shapes,
    xaxis: { title: { text: `${axisTitle}  (down-regulated ← 0 → up-regulated)` }, range: [-max, max], zeroline: true, zerolinewidth: 1.5, ...absTicks(max) },
  }
}

function bySource(terms: Term[]) {
  const m = new Map<string, Term[]>()
  terms.forEach((t) => m.set(t.source, [...(m.get(t.source) ?? []), t]))
  return m
}

const hover = (t: Term) =>
  `${t.direction ? `<i>${DIR_LABEL[t.direction]}</i><br>` : ''}<b>${escapeForPlotly(t.name)}</b><br>${escapeForPlotly(t.id)} · ${escapeForPlotly(t.source)}<br>p = ${formatP(t.pvalue)}<br>Genes: ${t.intersection} of ${t.termSize}${t.nes != null ? `<br>NES = ${t.nes.toFixed(2)}` : ''}${(t.represents ?? 1) > 1 ? `<br>Represents ${t.represents! - 1} similar term${t.represents === 2 ? '' : 's'}` : ''}`

// Plotly hovertext accepts a small HTML subset; escape data so names render as text.
function escapeForPlotly(s: string) {
  return String(s ?? '').replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' })[c]!)
}

// Selected items get an outline in the text colour (dark on light, light on dark), which stands out
// on any palette; while something is selected, everything else is faded.
const selColor = () => getComputedStyle(document.documentElement).getPropertyValue('--heading-color').trim() || '#18181b'
// `rest` is the outline of unselected items (none by default).
const selLine = (ids: string[], selected: Set<string>, rest: { color: string; width: number } = { color: 'rgba(0,0,0,0)', width: 0 }) => {
  const c = selColor()
  return {
    color: ids.map((id) => (selected.has(id) ? c : rest.color)),
    width: ids.map((id) => (selected.has(id) ? 2.5 : rest.width)),
  }
}
// `anySelected` covers the whole plot, so traces (sources) without a selected item fade too.
const selOpacity = (ids: string[], selected: Set<string>, base = 1, anySelected = ids.some((id) => selected.has(id))) =>
  anySelected ? ids.map((id) => (selected.has(id) ? base : 0.25)) : base

// Plotly has no built-in OrRd; light orange → dark red, so more significant = darker.
const ORRD: [number, string][] = [[0, '#fee8c8'], [0.5, '#fc8d59'], [1, '#b30000']]

// Dotplot row spacing and dot diameters (px).
const ROW = 28, DOT_MIN = 7, DOT_MAX = 22

/** The significance colour scale of the dot plot and the bar chart. */
function significanceScale(choice: 'OrRd' | 'Viridis' | 'Cividis' | 'Magma', cb: ReturnType<typeof usePaletteMode>): string | [number, string][] {
  return choice === 'OrRd' ? (cb === 'flame' ? ORRD : sequentialScale(cb, 'OrRd')) : choice === 'Magma' ? 'Hot' : choice
}
/** The default single bar colour: the palette's first colour. */
export const defaultBarColor = (cb: ReturnType<typeof usePaletteMode>) => categoricalColor(cb, 'x', ['x'], '#f97316')

export function DotplotView({ data, settings, selected, onSelect, plotRef, exportName, title }: ViewProps) {
  const cb = usePaletteMode()
  const choice = settings.palette ?? 'OrRd'
  const dotScale = significanceScale(choice, cb)
  const xKey = settings.xAxis ?? 'geneRatio'
  const upDown = data.terms.some((t) => t.section)
  const xOf = (t: Term) => dirSign(t) * (t[xKey] ?? 0)
  // Rows follow the x axis (largest at the top); Plotly draws the first category at the bottom.
  // Up & down figure: the caller's order (sections, top-down).
  const terms = upDown ? sortSections(data.terms, xOf) : [...data.terms].sort((a, b) => xOf(a) - xOf(b))
  const yLabel = axisLabels(terms, settings)
  const xLabel = { geneRatio: 'Gene ratio', foldEnrichment: 'Fold enrichment', coverage: 'Term coverage (%)', nes: 'Normalized enrichment score (NES)' }[xKey]
  // Dot area is proportional to the number of genes; the largest dot just fits its row, so
  // neighbours never overlap.
  const counts = terms.map((t) => t.intersection)
  const kMin = Math.min(...counts), kMax = Math.max(...counts, 1)
  const sizeOf = (k: number) => Math.max(DOT_MIN, DOT_MAX * Math.sqrt(k / kMax))
  // Colour bar on top right, size legend below it; both sized in px of the plotting area.
  const ud = upDown ? upDownLayout(terms, yLabel, terms.map(xOf), xLabel) : null
  const rows = ud ? ud.rows : rowOrder(terms, yLabel)
  const height = Math.max(360, ROW * rows.length + 110)
  const area = height - 60
  const cbLen = Math.min(220, area * 0.5) / area
  const legendY = 1 - cbLen - 40 / area
  // A few reference sizes for the "Genes" legend.
  const legendCounts = [...new Set(kMax === kMin ? [kMin] : [kMin, Math.round((kMin + kMax) / 2), kMax])]
  const plot = useMemo(() => [{
    type: 'scatter' as const, mode: 'markers' as const, showlegend: false,
    x: terms.map(xOf), y: terms.map((t) => yLabel.get(t.id)!), customdata: terms.map((t) => t.id),
    text: terms.map(hover), hoverinfo: 'text' as const,
    marker: {
      size: counts.map(sizeOf), symbol: upDown ? terms.map((t) => (t.direction === 'down' ? 'diamond' : 'circle')) : 'circle', color: terms.map((t) => t.log10p),
      colorscale: dotScale as never,
      reversescale: typeof dotScale === 'string' && reverseScale(dotScale),
      showscale: true, colorbar: { title: { text: '−log10 p' }, thickness: 12, len: cbLen, y: 1, yanchor: 'top' as const },
      line: selLine(terms.map((t) => t.id), selected, { color: 'rgba(255,255,255,0.9)', width: 1 }),
      opacity: selOpacity(terms.map((t) => t.id), selected),
    },
  }], [terms, yLabel, xKey, selected, cb]) // eslint-disable-line react-hooks/exhaustive-deps
  if (xKey === 'foldEnrichment' && data.terms.every((t) => t.foldEnrichment == null)) {
    return <p className="muted">Fold enrichment needs the background size, which this tool does not report. Choose another x-axis.</p>
  }
  return <Plot title={title} exportName={exportName} colorKey="flame_results_dotplot" data={plot as never} height={height} handleRef={plotRef} onSelect={onSelect}
    layout={{
      margin: { l: 320, r: 20, t: 10, b: 50 },
      xaxis: ud ? ud.xaxis : { title: { text: xLabel } },
      yaxis: { automargin: true, categoryorder: 'array', categoryarray: rows },
      // Size legend under the colour bar, drawn as pixel-sized circles (Plotly's legend caps marker sizes).
      showlegend: false,
      shapes: [...(ud ? ud.shapes : []), ...legendCounts.map((k, i) => {
        const d = sizeOf(k), y = legendY - (28 + i * (DOT_MAX + 8) + DOT_MAX / 2) / area
        return { name: `legend-size-${i}`, type: 'circle' as const, xref: 'paper' as const, yref: 'paper' as const, xsizemode: 'pixel' as const, ysizemode: 'pixel' as const,
          xanchor: 1.03, yanchor: y, x0: DOT_MAX / 2 - d / 2, x1: DOT_MAX / 2 + d / 2, y0: -d / 2, y1: d / 2,
          fillcolor: 'rgba(128,128,128,0.45)', line: { color: 'rgba(128,128,128,0.9)', width: 1 } }
      })] as never,
      annotations: ([
        ...(upDown ? [{ name: 'dir-legend', xref: 'paper' as const, yref: 'paper' as const, x: 1.03, y: legendY - (40 + legendCounts.length * (DOT_MAX + 8)) / area, xanchor: 'left' as const, yanchor: 'top' as const,
          text: '● up<br>◆ down', align: 'left' as const, showarrow: false, font: { size: 11 } }] : []),
        { name: 'legend-title', xref: 'paper' as const, yref: 'paper' as const, x: 1.03, y: legendY, xanchor: 'left' as const, yanchor: 'top' as const, text: 'Genes', showarrow: false },
        ...legendCounts.map((k, i) => ({ name: `legend-label-${i}`, xref: 'paper' as const, yref: 'paper' as const, x: 1.03, xshift: DOT_MAX + 8, xanchor: 'left' as const,
          y: legendY - (28 + i * (DOT_MAX + 8) + DOT_MAX / 2) / area, yanchor: 'middle' as const, text: String(k), showarrow: false, font: { size: 11 } })),
      ]) as never,
    }} />
}

export function BarchartView({ data, config, settings, selected, onSelect, plotRef, exportName, title }: ViewProps) {
  const cb = usePaletteMode()
  const colorOf = useSourceColor(config, data.availableSources)
  const metric = settings.barMetric ?? 'log10p'
  const upDown = data.terms.some((t) => t.section)
  // The up & down figure is coloured by direction unless another colouring is chosen.
  const colors = settings.barColors ?? (upDown ? 'direction' : 'source')
  const value = (t: Term) => dirSign(t) * (metric === 'log10p' ? t.log10p : metric === 'nes' ? t.nes ?? 0 : t.coverage)
  // Bars follow their length (longest at the top); Plotly draws the first category at the bottom.
  // Up & down figure: the caller's order (sections, top-down).
  const terms = upDown ? sortSections(data.terms, value) : [...data.terms].sort((a, b) => value(a) - value(b))
  const yLabel = axisLabels(terms, settings)
  const metricLabel = metric === 'log10p' ? '−log10 p-value' : metric === 'nes' ? 'NES (positive = top of ranking)' : 'Term coverage (%)'
  const ud = upDown ? upDownLayout(terms, yLabel, terms.map(value), metricLabel) : null
  const rows = ud ? ud.rows : rowOrder(terms, yLabel)
  const anySel = terms.some((t) => selected.has(t.id))
  const plot = useMemo(() => {
    const bar = (ts: Term[], name: string, marker: Record<string, unknown>) => ({
      type: 'bar' as const, orientation: 'h' as const, name, showlegend: colors === 'source',
      x: ts.map(value), y: ts.map((t) => yLabel.get(t.id)!),
      customdata: ts.map((t) => t.id), text: ts.map(hover), hoverinfo: 'text' as const, textposition: 'none' as const,
      marker: { ...marker, line: selLine(ts.map((t) => t.id), selected), opacity: selOpacity(ts.map((t) => t.id), selected, 1, anySel) },
    })
    if (colors === 'direction' && upDown) {
      const rc = roleColors(cb)
      return [...(['up', 'down'] as const).map((d) => ({ ...bar(terms.filter((t) => t.direction === d), DIR_LABEL[d], { color: d === 'up' ? rc.up : rc.down }), showlegend: true }))]
    }
    if (colors === 'source' || colors === 'direction') return [...[...bySource(terms)].map(([src, ts]) => bar(ts, src, { color: colorOf(src) }))]
    if (colors === 'single') return [bar(terms, 'Terms', { color: settings.barColor || defaultBarColor(cb) })]
    // Shaded by significance (−log10 p), whatever the bar length shows, with a colour bar as in the dot plot.
    const scale = significanceScale(colors, cb)
    return [bar(terms, 'Terms', { color: terms.map((t) => t.log10p), colorscale: scale, reversescale: typeof scale === 'string' && reverseScale(scale),
      showscale: true, colorbar: { title: { text: '−log10 p' }, thickness: 12, len: Math.min(1, 260 / Math.max(360, 24 * terms.length + 80)), y: 1, yanchor: 'top' } })]
  }, [terms, yLabel, metric, config, selected, colors, settings.barColor, cb]) // eslint-disable-line react-hooks/exhaustive-deps
  return <Plot title={title} exportName={exportName} colorKey="flame_results_barchart" data={plot as never} height={Math.max(360, 24 * rows.length + 80)} handleRef={plotRef} onSelect={onSelect}
    layout={{ barmode: 'relative', margin: { l: 320, r: 20, t: 10, b: 50 },
      xaxis: ud ? ud.xaxis : { title: { text: metricLabel } }, shapes: (ud?.shapes ?? []) as never,
      yaxis: { automargin: true, categoryorder: 'array', categoryarray: rows }, legend: { orientation: 'h', y: -0.15 },
      showlegend: (colors === 'direction' && upDown) || ((colors === 'source' || colors === 'direction') && new Set(terms.map((t) => t.source)).size > 1) }} />
}

export function ScatterView({ data, config, selected, onSelect, plotRef, exportName, title }: ViewProps) {
  const colorOf = useSourceColor(config, data.availableSources)
  const plot = useMemo(() => [...bySource(data.terms)].map(([src, ts]) => ({
    type: 'scatter' as const, mode: 'markers' as const, name: src,
    x: ts.map((t) => t.log10p), y: ts.map((t) => t.coverage), customdata: ts.map((t) => t.id),
    text: ts.map(hover), hoverinfo: 'text' as const,
    marker: { size: 11, color: colorOf(src), opacity: selOpacity(ts.map((t) => t.id), selected, 0.85, data.terms.some((t) => selected.has(t.id))), line: selLine(ts.map((t) => t.id), selected) },
  })), [data.terms, config, selected])
  return <Plot title={title} exportName={exportName} colorKey="flame_results_scatter" data={plot} height={480} handleRef={plotRef} onSelect={onSelect}
    layout={{ xaxis: { title: { text: '−log10 p-value' } }, yaxis: { title: { text: 'Term coverage (%)' } }, legend: { orientation: 'h', y: -0.2 }, showlegend: new Set(data.terms.map((t) => t.source)).size > 1 }} />
}

/** Heatmaps: T×G presence, T×T similarity %, G×G shared terms. */
export function HeatmapView({ data, settings, onSelect, plotRef, exportName, title }: ViewProps) {
  const cb = usePaletteMode()
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
        colorscale: [[0, 'rgba(0,0,0,0.03)'], [1, cb === 'flame' ? '#f97316' : categoricalColor(cb, 'x', ['x'])]] as never, hovertemplate: '%{y} · %{x}<extra></extra>', xgap: 1, ygap: 1,
      }]
    }
    if (data.view === 'heatmap-tt') {
      const ids = data.terms.map((t) => t.id)
      const sim = new Map(cells.map((c) => [`${c.source}|${c.target}`, Number(c.similarity)]))
      const names = ids.map((id) => label(termById.get(id)!, settings, 40))
      return [{
        type: 'heatmap' as const, x: names, y: names, z: ids.map((a) => ids.map((b) => sim.get(`${a}|${b}`) ?? 0)),
        customdata: ids.map((a) => ids.map((b) => `${a},${b}`)) as unknown as string[],
        colorscale: settings.heatScale ?? sequentialScale(cb, 'YlOrRd'), reversescale: reverseScale(settings.heatScale ?? sequentialScale(cb, 'YlOrRd')), colorbar: { title: { text: 'Similarity %' }, thickness: 12 }, hovertemplate: '%{y}<br>%{x}<br>%{z}% shared genes<extra></extra>',
      }]
    }
    const genes = [...new Set(cells.map((c) => String(c.source)))].sort()
    const shared = new Map(cells.map((c) => [`${c.source}|${c.target}`, Number(c.shared)]))
    return [{
      type: 'heatmap' as const, x: genes, y: genes, z: genes.map((a) => genes.map((b) => shared.get(`${a}|${b}`) ?? 0)),
      customdata: genes.map((a) => genes.map((b) => `gene:${a},gene:${b}`)) as unknown as string[],
      colorscale: settings.heatScale ?? sequentialScale(cb, 'Blues'), reversescale: reverseScale(settings.heatScale ?? sequentialScale(cb, 'Blues')), colorbar: { title: { text: 'Shared terms' }, thickness: 12 }, hovertemplate: '%{y} · %{x}<br>%{z} shared terms<extra></extra>',
    }]
  }, [data.view, cells, data.terms, settings, termById, cb])
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
  return <Plot title={title} exportName={exportName} colorKey={`flame_results_${data.view}`} data={plot} height={Math.max(420, 18 * n + 160)} handleRef={plotRef} onSelect={resolve}
    layout={{ margin: { l: 260, r: 20, t: 10, b: 160 }, xaxis: { tickangle: -45, automargin: true }, yaxis: { automargin: true } }} />
}

const SHAPES: Record<string, string> = {
  'GO:MF': 'hexagon', 'GO:BP': 'hexagon', 'GO:CC': 'hexagon', KEGG: 'diamond', REAC: 'diamond', WP: 'diamond',
  DO: 'triangleDown', DISGENET: 'triangleDown', OMIM: 'triangleDown', DRUGBANK: 'star', INTERPRO: 'star', PFAM: 'star', Gene: 'dot',
}

export function NetworkView({ data, config, settings, selected, onSelect, networkRef, networkStyle, exportName, title }: ViewProps) {
  const colorOf = useSourceColor(config, data.availableSources)
  const cb = usePaletteMode()
  const net = data.network
  const nodes = useMemo(() => (net?.nodes ?? []).map((n) => ({
    id: n.id, x: n.x, y: n.y, label: n.kind === 'gene' ? n.label : settings.labels === 'id' ? n.id : truncate(n.name, 30),
    title: n.kind === 'gene' ? n.label : `${n.name} (${n.id}, ${n.group})`,
    color: n.kind === 'gene' ? (cb === 'flame' ? config.geneNodeColor : '#d4d4d8') : colorOf(n.group), shape: SHAPES[n.group] ?? (n.kind === 'gene' ? 'dot' : 'square'),
  })), [net, settings.labels, config, colorOf, cb])
  const edges = useMemo(() => (net?.edges ?? []).map((e) => ({ id: e.id, from: e.from, to: e.to, width: e.width, value: Number(e.weight) || 1,
    title: data.view === 'network-tt' ? `${e.weight}% similar` : data.view === 'network-gg' ? `${e.weight} shared terms` : undefined })), [net, data.view])
  const termIds = new Set(data.terms.map((t) => t.id))
  const selectedNodes = [...selected].filter((id) => nodes.some((n) => n.id === id))
  if (!net || !net.nodes.length) {
    return <p className="muted">No edges pass the current cut-off. Lower the {data.view === 'network-tt' ? 'similarity' : 'shared-terms'} threshold or show more terms.</p>
  }
  return (
    <NetworkGraph title={title} exportName={exportName} nodes={nodes} edges={edges} selected={selectedNodes} handleRef={networkRef} height={600} style={networkStyle}
      onSelect={(ids, add) => {
        const terms = ids.filter((id) => termIds.has(id))
        const genes = ids.filter((id) => !termIds.has(id))
        const viaGenes = data.terms.filter((t) => genes.some((g) => splitGenes(t.genes).includes(g))).map((t) => t.id)
        onSelect([...new Set([...terms, ...viaGenes])], add)
      }} />
  )
}

/** The genes of a term: the first few, expandable to the full list (as in Flame 2.0) and collapsible again. */
function GenesCell({ genes }: { genes: string }) {
  return <GeneList genes={splitGenes(genes)} />
}

const truncate = (s: string, n: number) => (s.length > n ? `${s.slice(0, n - 1)}…` : s)

/** A representative term with the similar terms it stands for (redundancy reduction), listed on demand. */
function MergedTerms({ term }: { term: Term }) {
  const [open, setOpen] = useState(false)
  const merged = (term.merged ?? '').split(' | ').filter(Boolean)
  return (
    <>
      {term.name}{' '}
      <button type="button" className="gene-list-toggle merged-toggle" aria-expanded={open} title={`This term represents ${merged.length} similar, less significant term${merged.length === 1 ? '' : 's'} — click to ${open ? 'hide' : 'list'} them`}
        onClick={(e) => { e.stopPropagation(); setOpen((o) => !o) }}>+{merged.length} similar {open ? '▴' : '▾'}</button>
      {open && <ul className="merged-terms">{merged.map((m) => <li key={m}>{m}</li>)}</ul>}
    </>
  )
}

export const TABLE_COLUMNS: { key: keyof Term | 'genesCount'; label: string; numeric?: boolean }[] = [
  { key: 'source', label: 'Source' }, { key: 'id', label: 'Term ID' }, { key: 'name', label: 'Term' },
  { key: 'pvalue', label: 'P-value', numeric: true }, { key: 'log10p', label: '−log10 p', numeric: true },
  { key: 'termSize', label: 'Term size', numeric: true }, { key: 'intersection', label: 'Genes found', numeric: true },
  { key: 'coverage', label: 'Coverage %', numeric: true }, { key: 'geneRatio', label: 'Gene ratio', numeric: true },
  { key: 'foldEnrichment', label: 'Fold enrichment', numeric: true }, { key: 'nes', label: 'NES', numeric: true }, { key: 'genes', label: 'Genes' },
]

export function TableView({ data, config, selected, onSelect, columns, runId }: ViewProps & { columns: string[]; runId?: string }) {
  const colorOf = useSourceColor(config, data.availableSources)
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
                <td key={c.key} className={c.key === 'genes' ? 'genes-cell' : c.key === 'id' ? 'id-cell' : c.numeric ? 'num' : ''}>
                  {c.key === 'source' ? <span className="source-tag" style={{ borderColor: colorOf(t.source) }}>{t.source}</span>
                    : c.key === 'id' && runId && isKeggTerm(t) ? <KeggMapLink runId={runId} term={t} />
                    : c.key === 'id' && isHttpUrl(t.link) ? <a href={t.link} target="_blank" rel="noopener noreferrer" onClick={(e) => e.stopPropagation()}>{t.id}</a>
                    : c.key === 'name' && (t.represents ?? 1) > 1 ? <MergedTerms term={t} />
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

// ---------------------------------------------------------------------------
// Sankey: genes → terms → annotation sources
// ---------------------------------------------------------------------------
export function SankeyView({ data, config, settings, onSelect, plotRef, exportName, title }: ViewProps) {
  const colorOf = useSourceColor(config, data.availableSources)
  const pm = usePaletteMode()
  const plot = useMemo(() => {
    const termIds = data.terms.map((t) => t.id)
    // Keep the genes shared by most terms readable; the rest flow from one "Other genes" node.
    const count = new Map<string, number>()
    data.terms.forEach((t) => splitGenes(t.genes).forEach((g) => count.set(g, (count.get(g) ?? 0) + 1)))
    const genes = [...count.entries()].sort((a, b) => b[1] - a[1]).slice(0, 40).map(([g]) => g)
    const shown = new Set(genes)
    const others = count.size - genes.length
    const sources = [...new Set(data.terms.map((t) => t.source))]
    const nodes = [...genes.map((g) => `g:${g}`), ...(others > 0 ? ['g:__other'] : []), ...termIds.map((t) => `t:${t}`), ...sources.map((s) => `s:${s}`)]
    const idx = new Map(nodes.map((n, i) => [n, i]))
    const src: number[] = [], tgt: number[] = [], val: number[] = []
    data.terms.forEach((t) => {
      const gs = splitGenes(t.genes)
      gs.filter((g) => shown.has(g)).forEach((g) => { src.push(idx.get(`g:${g}`)!); tgt.push(idx.get(`t:${t.id}`)!); val.push(1) })
      const rest = gs.filter((g) => !shown.has(g)).length
      if (rest) { src.push(idx.get('g:__other')!); tgt.push(idx.get(`t:${t.id}`)!); val.push(rest) }
      src.push(idx.get(`t:${t.id}`)!); tgt.push(idx.get(`s:${t.source}`)!); val.push(gs.length)
    })
    const term = new Map(data.terms.map((t) => [t.id, t]))
    return [{
      type: 'sankey' as const, arrangement: 'snap' as const,
      node: {
        label: nodes.map((n) => (n === 'g:__other' ? `Other genes (${others})` : n.startsWith('t:') ? label(term.get(n.slice(2))!, settings, 40) : n.slice(2))),
        color: nodes.map((n) => (n.startsWith('g:') ? (pm === 'flame' ? config.geneNodeColor : '#bdbdbd') : colorOf(n.startsWith('t:') ? term.get(n.slice(2))!.source : n.slice(2)))),
        customdata: nodes.map((n) => (n.startsWith('t:') ? n.slice(2) : '')), pad: 6, thickness: 12,
      },
      link: { source: src, target: tgt, value: val, color: 'rgba(161,161,170,0.3)' },
    }]
  }, [data.terms, settings, config, colorOf, pm])
  return <Plot title={title} exportName={exportName} colorKey="flame_results_sankey" data={plot as never} height={Math.max(480, Math.min(1100, data.terms.length * 34))} handleRef={plotRef}
    onSelect={(ids, add) => onSelect(ids.filter(Boolean), add)} layout={{ margin: { l: 10, r: 10, t: 10, b: 10 } }} />
}

// ---------------------------------------------------------------------------
// Chord: shared genes between the shown terms
// ---------------------------------------------------------------------------
export function ChordView({ data, config, settings, selected, onSelect, svgRef, exportName, title }: ViewProps & { svgRef: { current: SVGSVGElement | null } }) {
  const colorOf = useSourceColor(config, data.availableSources)
  const ids = data.terms.map((t) => t.id)
  const matrix = useMemo(() => {
    const m = ids.map(() => ids.map(() => 0))
    ;(data.cells ?? []).forEach((c) => {
      const i = ids.indexOf(String(c.source)), j = ids.indexOf(String(c.target))
      if (i >= 0 && j >= 0 && i !== j) m[i][j] = Number(c.common)
    })
    return m
  }, [data.cells, ids.join('|')])  // eslint-disable-line react-hooks/exhaustive-deps
  if (matrix.every((row) => row.every((v) => v === 0))) return <p className="muted">These terms share no genes. Show more terms to see overlaps.</p>
  return (
    <ChordDiagram title={title} exportName={exportName} names={data.terms.map((t) => label(t, settings, 40))} matrix={matrix} colors={data.terms.map((t) => colorOf(t.source))}
      svgRef={svgRef} selected={new Set(ids.map((id, i) => (selected.has(id) ? i : -1)).filter((i) => i >= 0))}
      onSelect={(i, j) => onSelect(j === undefined ? [ids[i]] : [ids[i], ids[j]], true)} />
  )
}

// ---------------------------------------------------------------------------
// Hive: genes on one axis, terms on one axis per annotation source
// ---------------------------------------------------------------------------
export function HiveView({ data, config, settings, selected, onSelect, svgRef, exportName }: ViewProps & { svgRef: { current: SVGSVGElement | null } }) {
  const colorOf = useSourceColor(config, data.availableSources)
  const pm = usePaletteMode()
  const { axes, nodes, links } = useMemo(() => {
    const sources = [...new Set(data.terms.map((t) => t.source))]
    const geneCount = new Map<string, number>()
    data.terms.forEach((t) => splitGenes(t.genes).forEach((g) => geneCount.set(g, (geneCount.get(g) ?? 0) + 1)))
    const genes = [...geneCount.entries()].sort((a, b) => b[1] - a[1]).map(([g]) => g)
    const geneColor = pm === 'flame' ? '#71717a' : '#636363'
    const axes = [{ name: 'Genes', color: geneColor }, ...sources.map((s) => ({ name: s, color: colorOf(s) }))]
    const nodes = [
      ...genes.map((g, i) => ({ id: `g:${g}`, label: `${g} — in ${geneCount.get(g)} terms`, axis: 'Genes', rank: i, color: geneColor })),
      ...sources.flatMap((s) => data.terms.filter((t) => t.source === s).map((t, i) => ({ id: t.id, label: `${t.name} (${t.id}) · p = ${formatP(t.pvalue)}`, axis: s, rank: i, color: colorOf(s) }))),
    ]
    const links = data.terms.flatMap((t) => splitGenes(t.genes).map((g) => ({ source: `g:${g}`, target: t.id })))
    return { axes, nodes, links }
  }, [data.terms, colorOf, pm])
  void settings
  return (
    <HivePlot exportName={exportName} axes={axes} nodes={nodes} links={links} svgRef={svgRef} selected={selected}
      onSelect={(ids) => {
        const terms = ids.filter((id) => !id.startsWith('g:'))
        const genes = ids.filter((id) => id.startsWith('g:')).map((id) => id.slice(2))
        const viaGenes = data.terms.filter((t) => genes.some((g) => splitGenes(t.genes).includes(g))).map((t) => t.id)
        onSelect([...new Set([...terms, ...viaGenes])], false)
      }} />
  )
}

/** Circos: terms grouped by annotation source and a sector of genes; bundled term–gene links; outer bars = −log10 p (terms) or term count (genes). */
export function CircosView({ data, config, settings, selected, onSelect, svgRef, exportName }: ViewProps & { svgRef: { current: SVGSVGElement | null } }) {
  const colorOf = useSourceColor(config, data.availableSources)
  const pm = usePaletteMode()
  const { sectors, items, links } = useMemo(() => {
    const geneColor = pm === 'flame' ? '#71717a' : '#636363'
    const sources = [...new Set(data.terms.map((t) => t.source))]
    const count = new Map<string, number>()
    data.terms.forEach((t) => splitGenes(t.genes).forEach((g) => count.set(g, (count.get(g) ?? 0) + 1)))
    const maxP = Math.max(1, ...data.terms.map((t) => t.log10p ?? 0))
    const maxC = Math.max(1, ...count.values())
    const sectors = [...sources.map((s) => ({ id: s, label: s, color: colorOf(s) })), { id: '__genes', label: 'Genes', color: geneColor }]
    const items = [
      ...sources.flatMap((s) => data.terms.filter((t) => t.source === s).map((t) => ({
        id: t.id, sector: s, label: settings.labels === 'id' ? t.id : t.name, value: (t.log10p ?? 0) / maxP,
        title: `${t.name} (${t.id})\n${t.source} · p = ${formatP(t.pvalue)} · ${splitGenes(t.genes).length} genes` }))),
      ...[...count.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])).map(([g, c]) => ({
        id: `g:${g}`, sector: '__genes', label: g, value: c / maxC, title: `${g}: in ${c} of the shown terms`,
        // Terms and genes each get half of the circle when genes outnumber terms.
        weight: count.size > data.terms.length ? data.terms.length / count.size : 1 })),
    ]
    const links = data.terms.flatMap((t) => splitGenes(t.genes).map((g) => ({ source: t.id, target: `g:${g}`, title: `${t.name} — ${g}` })))
    return { sectors, items, links }
  }, [data.terms, colorOf, pm, settings.labels])
  return (
    <CircosBundle exportName={exportName} sectors={sectors} items={items} links={links} svgRef={svgRef} selected={selected} linkLabel="term–gene links"
      trackLabel="−log10 p (terms), share of terms (genes)"
      onSelect={(ids, add) => {
        const terms = ids.filter((id) => !id.startsWith('g:'))
        const genes = ids.filter((id) => id.startsWith('g:')).map((id) => id.slice(2))
        const viaGenes = data.terms.filter((t) => genes.some((g) => splitGenes(t.genes).includes(g))).map((t) => t.id)
        onSelect([...new Set([...terms, ...viaGenes])], add)
      }} />
  )
}

/** KEGG pathway terms (any tool's ID format: KEGG:04110, hsa04110, map04110). */
// KEGG MEDICUS sets (MSigDB) are KEGG networks, not pathway maps: no map link.
export const isKeggTerm = (t: Term) => (/KEGG/i.test(t.source) && !/MEDICUS/i.test(t.source)) || /^(KEGG:|path:)?(map|hsa|mmu|rno|dre|dme|cel|sce|[a-z]{3,4})\d{5}$/.test(t.id)

/**
 * Link that opens the KEGG pathway map with the run's genes highlighted in red
 * (KEGG Mapper coloring; gene IDs mapped through the KEGG API on the server).
 */
function KeggMapLink({ runId, term }: { runId: string; term: Term }) {
  const [busy, setBusy] = useState(false)
  const open = (e: React.MouseEvent) => {
    e.preventDefault(); e.stopPropagation()
    if (busy) return
    setBusy(true)
    const win = window.open('', '_blank')   // opened in the click so popup blockers allow it
    api<{ links: { label: string; url: string }[] }>(`/runs/${encodeURIComponent(runId)}/pathway-links`, { body: { termId: term.id } })
      .then((r) => {
        const best = r.links.find((l) => /highlighted/.test(l.label)) ?? r.links.find((l) => /KEGG map/.test(l.label))
        const url = best?.url ?? term.link
        if (url && isHttpUrl(url)) { if (win) { win.opener = null; win.location.href = url } else window.open(url, '_blank', 'noopener') }
        else win?.close()
      })
      .catch(() => { if (term.link && isHttpUrl(term.link) && win) win.location.href = term.link; else win?.close() })
      .finally(() => setBusy(false))
  }
  return (
    <a href={term.link ?? '#'} target="_blank" rel="noopener noreferrer" onClick={open} className="kegg-link"
      title="Open the KEGG pathway map with your genes highlighted in red">
      {term.id} <span className="kegg-badge" aria-hidden="true">{busy ? '…' : '● map'}</span>
    </a>
  )
}
