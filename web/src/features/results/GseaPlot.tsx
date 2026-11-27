import { useQuery } from '@tanstack/react-query'
import type Plotly from 'plotly.js-dist-min'
import { api, getWorkspaceId } from '../../api/client'
import type { GseaPlotData, GseaPlotTerm, Term } from '../../api/types'
import { Plot } from '../../components/charts'
import { CopyButton, ErrorNote, Note, Spinner } from '../../components/ui'
import { formatP, slug } from '../../lib/format'

type PlotData = Partial<Plotly.PlotData>[]

/** Ranked-score band under the barcode: red = top of the ranking, blue = bottom (as in the GSEA desktop plot). */
const BAND: [number, string][] = [[0, '#2166ac'], [0.5, '#f7f7f7'], [1, '#b2182b']]

/**
 * Classic GSEA enrichment plots, one standalone figure per term: the running enrichment score
 * with its peak, a barcode of where the set's genes sit in the ranking, and the ranked scores.
 */
export function GseaPlots({ runId, terms, selected, exportName }: { runId: string; terms: Term[]; selected: Set<string>; exportName: string }) {
  const ws = getWorkspaceId()!
  const chosen = terms.filter((t) => selected.has(t.id))
  const shown = (chosen.length ? chosen : terms).slice(0, 12)
  const ids = shown.map((t) => t.id)
  const q = useQuery({
    queryKey: ['gsea-plot', ws, runId, ids],
    queryFn: () => api<GseaPlotData>(`/runs/${encodeURIComponent(runId)}/gsea-plot`, { body: { terms: ids } }),
    enabled: ids.length > 0,
  })
  if (!ids.length) return <Note>No terms to plot.</Note>
  if (q.error) return <ErrorNote error={q.error} />
  if (!q.data) return <Spinner label="Computing the running enrichment scores" />
  return (
    <div className="stack">
      <p className="muted small">
        {chosen.length ? `Enrichment plots of the ${shown.length} selected term${shown.length > 1 ? 's' : ''}` : `Enrichment plots of the first ${shown.length} terms shown in the table; select terms (table, dot plot…) to plot those instead`}
        {chosen.length > 12 || (!chosen.length && terms.length > 12) ? ' (at most 12 at once)' : ''}. Each plot is a standalone figure: use “Image” on it to save it.
      </p>
      {q.data.terms.map((t) => <GseaPlot key={t.id} term={t} data={q.data!} exportName={`${exportName}_${slug(t.name).slice(0, 40)}`} />)}
    </div>
  )
}

function GseaPlot({ term, data, exportName }: { term: GseaPlotTerm; data: GseaPlotData; exportName: string }) {
  const up = term.es >= 0
  const lineColor = '#1a9850'
  const peakY = term.es
  const traces: PlotData = [
    {
      type: 'scatter', mode: 'lines', x: term.x, y: term.y, xaxis: 'x', yaxis: 'y', line: { color: lineColor, width: 2.2 },
      hovertemplate: 'Rank %{x}<br>Running ES %{y:.3f}<extra></extra>', name: 'Running enrichment score',
    },
    {
      type: 'scatter', mode: 'markers', x: [term.peak], y: [peakY], xaxis: 'x', yaxis: 'y',
      marker: { color: '#d73027', size: 8 }, hovertemplate: `Peak (ES = ${term.es.toFixed(3)}) at rank ${term.peak}<extra></extra>`, name: 'Peak',
    },
    // Barcode: one tick per gene of the set, at its rank.
    {
      type: 'scatter', mode: 'lines', xaxis: 'x', yaxis: 'y2', line: { color: '#111', width: 0.8 }, hoverinfo: 'skip', name: 'Genes of the set',
      x: term.hits.flatMap((h) => [h, h, null]), y: term.hits.flatMap(() => [0, 1, null]),
    },
    {
      type: 'scatter', mode: 'markers', xaxis: 'x', yaxis: 'y2', x: term.hits, y: term.hits.map(() => 0.5),
      marker: { size: 6, opacity: 0 }, text: term.hitGenes, hovertemplate: '%{text}<br>rank %{x}<extra></extra>', name: 'Genes',
    },
    {
      type: 'heatmap', xaxis: 'x', yaxis: 'y2', x: data.metric.x, y: [-0.55], z: [data.metric.y], zmid: 0,
      colorscale: BAND, showscale: false, hoverinfo: 'skip', name: 'Ranking',
    } as Partial<Plotly.PlotData>,
    {
      type: 'scatter', mode: 'lines', xaxis: 'x', yaxis: 'y3', x: data.metric.x, y: data.metric.y, fill: 'tozeroy',
      line: { color: '#7f7f7f', width: 1 }, fillcolor: 'rgba(127,127,127,0.45)', hovertemplate: 'Rank %{x}<br>Score %{y:.3f}<extra></extra>', name: 'Ranked list metric',
    },
  ]
  const shapes: Partial<Plotly.Shape>[] = [
    { type: 'line', xref: 'x', yref: 'y', x0: 0, x1: data.n, y0: 0, y1: 0, line: { color: '#999', width: 1 } },
    { type: 'line', xref: 'x', yref: 'y', x0: term.peak, x1: term.peak, y0: 0, y1: peakY, line: { color: '#d73027', width: 1.2, dash: 'dash' } },
    // Leading edge: the part of the ranking before (positive ES) or after (negative ES) the peak.
    { type: 'rect', xref: 'x', yref: 'paper', x0: up ? 0 : term.peak, x1: up ? term.peak : data.n, y0: 0.3, y1: 1,
      fillcolor: 'rgba(215,48,39,0.06)', line: { width: 0 }, layer: 'below' },
  ]
  if (data.zeroCross != null) shapes.push({ type: 'line', xref: 'x', yref: 'paper', x0: data.zeroCross, x1: data.zeroCross, y0: 0, y1: 0.27, line: { color: '#666', width: 1, dash: 'dot' } })
  const annotations: Partial<Plotly.Annotations>[] = [
    {
      xref: 'paper', yref: 'paper', x: up ? 0.99 : 0.01, y: up ? 0.98 : 0.36, xanchor: up ? 'right' : 'left', yanchor: up ? 'top' : 'bottom', showarrow: false, align: up ? 'right' : 'left',
      text: `NES = ${term.nes.toFixed(2)}<br>adj. p = ${formatP(term.padj)}<br>Leading edge: ${term.leadingEdge.length} of ${term.setSize} genes`,
      font: { size: 12 }, bgcolor: 'rgba(255,255,255,0.75)',
    },
    // Inside the ranked-metric panel, in the corners the area plot leaves free.
    { xref: 'paper', yref: 'paper', x: 0.01, y: 0.01, xanchor: 'left', yanchor: 'bottom', showarrow: false, text: 'Top of ranking (score > 0)', font: { size: 11, color: '#b2182b' } },
    { xref: 'paper', yref: 'paper', x: 0.99, y: 0.26, xanchor: 'right', yanchor: 'top', showarrow: false, text: 'Bottom of ranking (score < 0)', font: { size: 11, color: '#2166ac' } },
  ]
  if (data.zeroCross != null) annotations.push({ xref: 'x', yref: 'y3', x: data.zeroCross, y: 0, xanchor: 'left', yanchor: 'bottom', showarrow: false, text: ` zero cross at ${data.zeroCross.toLocaleString()}`, font: { size: 10, color: '#666' } })
  const layout: Partial<Plotly.Layout> = {
    showlegend: false, hovermode: 'closest', margin: { l: 70, r: 20, t: 20, b: 50 },
    xaxis: { anchor: 'y3', range: [0, data.n], title: { text: 'Rank in ordered gene list' }, zeroline: false, showgrid: false },
    yaxis: { domain: [0.42, 1], title: { text: 'Running enrichment score' }, zeroline: false },
    yaxis2: { domain: [0.3, 0.4], range: [-1.1, 1], showticklabels: false, ticks: '', showline: false, showgrid: false, zeroline: false, fixedrange: true },
    yaxis3: { domain: [0, 0.27], title: { text: 'Ranked list metric' }, zeroline: false },
    shapes, annotations,
  }
  return (
    <section className="chart-card">
      <Plot data={traces} layout={layout} height={520} title={term.source && term.source !== term.name ? `${term.name} (${term.source})` : term.name} exportName={exportName} tools={false} />
      <details>
        <summary className="small">Leading-edge genes ({term.leadingEdge.length}): the genes before the peak that drive the enrichment</summary>
        <p className="mono small">{term.leadingEdge.join(', ')}</p>
        <CopyButton text={term.leadingEdge.join('\n')} label="Copy genes" />
      </details>
    </section>
  )
}
