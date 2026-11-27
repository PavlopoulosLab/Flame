import { useQuery } from '@tanstack/react-query'
import { useMemo, useState } from 'react'
import { api, getWorkspaceId, saveText } from '../../api/client'
import type { ComparisonFilters } from '../../api/types'
import { saveXlsx } from '../../lib/xlsx'
import { Plot } from '../../components/charts'
import { ErrorNote, Note, Spinner } from '../../components/ui'
import { formatP, toCsv } from '../../lib/format'
import { categoricalColor, reverseScale, sequentialScale, usePaletteMode } from '../../lib/palette'

interface Test { statistic?: number; df?: number; p: number; method: string }
interface RunStat { id: string; label: string; tool: string; listName: string; paradigm: string; terms: number; unique: number; medianTermSize?: number; medianLog10p?: number; bySource: Record<string, number> }
interface PairStat {
  a: string; b: string; nA: number; nB: number; shared: number; union: number; jaccard: number; overlapCoef: number; geneJaccard?: number
  spearman?: number; spearmanP?: number; spearmanPAdj?: number; spearmanAll?: number; spearmanAllP?: number; spearmanAllPAdj?: number; kendall?: number; kendallP?: number; kendallPAdj?: number
  medianDiff?: number; wilcoxonP?: number; wilcoxonPAdj?: number; mcnemarP?: number; mcnemarPAdj?: number
}
interface StatTerm { id: string; name: string; source: string; termSize?: number; p: Record<string, number | null | Record<string, never>> }
export interface ComparisonStats {
  runs: RunStat[]; pairs: PairStat[]; tests: { detection?: Test; termSize?: Test; significance?: Test }
  cat: { a: string; b: string; k: number[]; overlap: number[] }[]; mds: { id: string; x: number; y: number }[] | null
  agreement: { runs: number; terms: number }[]; terms: StatTerm[]; filteredCount: number; note: string
}

const pOf = (t: StatTerm, r: string): number | null => (typeof t.p[r] === 'number' ? (t.p[r] as number) : null)

type Metric = 'jaccard' | 'overlapCoef' | 'geneJaccard' | 'spearmanAll' | 'spearman' | 'kendall'
const METRICS: { value: Metric; label: string; hint: string }[] = [
  { value: 'jaccard', label: 'Jaccard (shared / all terms)', hint: 'How much the two term sets overlap: 1 = identical, 0 = nothing in common.' },
  { value: 'overlapCoef', label: 'Overlap coefficient', hint: 'Shared terms divided by the smaller set: 1 = the smaller run is contained in the larger.' },
  { value: 'geneJaccard', label: 'Gene agreement in shared terms', hint: 'For terms both runs found, how similar the genes they report are (mean Jaccard).' },
  { value: 'spearmanAll', label: 'Rank correlation, all terms (Spearman ρ)', hint: 'Do the runs rank terms in the same order? Uses every term either run found; a term a run did not report counts as its least significant.' },
  { value: 'spearman', label: 'Rank correlation, shared terms only (Spearman ρ)', hint: 'Order of the terms both runs found. Ignores the terms only one run found, so it overstates agreement when the runs share few terms.' },
  { value: 'kendall', label: 'Rank correlation, shared terms only (Kendall τ)', hint: 'Like Spearman on shared terms, more robust to ties and outliers.' },
]

const fmt = (v: number | undefined | null, d = 2) => (v == null || !Number.isFinite(v) ? '—' : v.toFixed(d))

function interpret(t: Test | undefined, what: string, yes: string, no: string) {
  if (!t) return <span className="muted">Not enough data.</span>
  const sig = t.p < 0.05
  return <>{sig ? yes : no} <span className="muted small">({t.method}{t.statistic != null ? `, statistic ${t.statistic}` : ''}{t.df != null ? `, df ${t.df}` : ''}, p = {formatP(t.p)}; {what})</span></>
}

export function ComparisonStatistics({ runIds, filters, colors, onSelect }: {
  runIds: string[]; filters: ComparisonFilters; colors: Record<string, string>; onSelect: (ids: string[], additive: boolean) => void
}) {
  const pm = usePaletteMode()
  const q = useQuery({
    queryKey: ['compare-stats', getWorkspaceId(), runIds.join(','), filters],
    queryFn: () => api<ComparisonStats>('/comparisons/statistics', { body: { runIds, filters } }),
  })
  const [metric, setMetric] = useState<Metric>('jaccard')
  const [pairSel, setPairSel] = useState<[string, string] | null>(null)
  const [onlyShared, setOnlyShared] = useState(false)
  const d = q.data
  const runs = d?.runs ?? []
  const label = useMemo(() => new Map(runs.map((r) => [r.id, r.label])), [runs])
  const pair: [string, string] | null = pairSel ?? (runs.length >= 2 ? [runs[0].id, runs[1].id] : null)
  const sources = useMemo(() => [...new Set((d?.terms ?? []).map((t) => t.source))].sort(), [d])
  const srcColor = (s: string) => categoricalColor(pm, s, sources)

  if (q.isLoading) return <Spinner label="Computing statistics" />
  if (q.error) return <ErrorNote error={q.error} />
  if (!d) return null
  if (runs.length < 2) return <Note kind="warning">Select at least two runs in the Runs filter.</Note>

  const pairOf = (a: string, b: string) => d.pairs.find((p) => (p.a === a && p.b === b) || (p.a === b && p.b === a))
  const shared = (a: string, b: string) => d.terms.filter((t) => pOf(t, a) != null && pOf(t, b) != null).map((t) => t.id)
  const ids = runs.map((r) => r.id)

  // Pairwise heatmap.
  const z = ids.map((a) => ids.map((b) => (a === b ? 1 : (pairOf(a, b)?.[metric] ?? null))))
  const scale = pm === 'flame' ? [[0, '#f5f3ff'], [0.5, '#a78bfa'], [1, '#4c1d95']] : sequentialScale(pm, 'Viridis')
  const heat = [{
    type: 'heatmap' as const, x: ids.map((r) => label.get(r)!), y: ids.map((r) => label.get(r)!), z,
    zmin: metric === 'spearman' || metric === 'spearmanAll' || metric === 'kendall' ? -1 : 0, zmax: 1,
    colorscale: scale as never, reversescale: typeof scale === 'string' && reverseScale(scale),
    text: ids.map((a) => ids.map((b) => {
      const p = pairOf(a, b)
      if (a === b || !p) return label.get(a)!
      return `${label.get(a)} vs ${label.get(b)}<br>shared ${p.shared} of ${p.union} terms<br>Jaccard ${fmt(p.jaccard)} · overlap ${fmt(p.overlapCoef)}<br>Spearman ρ all terms ${fmt(p.spearmanAll)} · shared only ${fmt(p.spearman)}<br>gene agreement ${fmt(p.geneJaccard)}`
    })),
    hoverinfo: 'text' as const, customdata: ids.map((a) => ids.map((b) => `${a}|${b}`)),
    texttemplate: '%{z:.2f}', showscale: true,
  }]

  // Concordance scatter for the chosen pair.
  const scatter = pair ? (() => {
    const [a, b] = pair
    const pts = d.terms.filter((t) => (onlyShared ? pOf(t, a) != null && pOf(t, b) != null : pOf(t, a) != null || pOf(t, b) != null))
    const max = Math.max(1, ...pts.flatMap((t) => [pOf(t, a), pOf(t, b)].filter((v): v is number => v != null).map((v) => -Math.log10(v))))
    return {
      traces: [
        ...sources.map((s) => {
          const sub = pts.filter((t) => t.source === s)
          return {
            type: 'scatter' as const, mode: 'markers' as const, name: s,
            x: sub.map((t) => (pOf(t, a) != null ? -Math.log10(pOf(t, a)!) : 0)), y: sub.map((t) => (pOf(t, b) != null ? -Math.log10(pOf(t, b)!) : 0)),
            text: sub.map((t) => `${t.name} (${t.id})<br>${label.get(a)}: ${pOf(t, a) != null ? `p = ${formatP(pOf(t, a))}` : 'not found'}<br>${label.get(b)}: ${pOf(t, b) != null ? `p = ${formatP(pOf(t, b))}` : 'not found'}`),
            hoverinfo: 'text' as const, customdata: sub.map((t) => t.id),
            marker: { size: 8, color: srcColor(s), opacity: 0.75, line: { width: 0.5, color: 'rgba(0,0,0,0.25)' } },
          }
        }),
        { type: 'scatter' as const, mode: 'lines' as const, x: [0, max], y: [0, max], line: { dash: 'dot' as const, color: '#a1a1aa', width: 1 }, hoverinfo: 'skip' as const, showlegend: false },
      ],
      layout: {
        xaxis: { title: { text: `−log10 p · ${label.get(a)} (0 = not found)` }, rangemode: 'tozero' as const },
        yaxis: { title: { text: `−log10 p · ${label.get(b)} (0 = not found)` }, rangemode: 'tozero' as const },
        legend: { orientation: 'h' as const, y: -0.25 }, margin: { l: 70, r: 20, t: 10, b: 110 },
      },
    }
  })() : null
  const pp = pair ? pairOf(pair[0], pair[1]) : undefined

  // Concordance at the top.
  const catTraces = d.cat.map((c, i) => ({
    type: 'scatter' as const, mode: 'lines+markers' as const, name: `${label.get(c.a)} vs ${label.get(c.b)}`, x: c.k, y: c.overlap,
    marker: { size: 4 }, line: { width: 2, color: categoricalColor(pm, `${c.a}|${c.b}`, d.cat.map((x) => `${x.a}|${x.b}`)) }, legendrank: i,
    customdata: c.k.map((k) => `cat|${c.a}|${c.b}|${k}`), hovertemplate: 'top %{x}: %{y:.0%} shared<extra>%{fullData.name}</extra>',
  }))

  // Terms per source per run (stacked bars).
  const srcBars = sources.map((s) => ({
    type: 'bar' as const, name: s, x: runs.map((r) => r.label), y: runs.map((r) => r.bySource[s] ?? 0),
    marker: { color: srcColor(s) }, customdata: runs.map((r) => `src|${r.id}|${s}`), hovertemplate: `${s}: %{y} terms<extra>%{x}</extra>`,
  }))

  // Term sizes per run (box with all points, selectable).
  const boxes = runs.map((r) => {
    const sub = d.terms.filter((t) => pOf(t, r.id) != null && t.termSize != null)
    return {
      type: 'box' as const, name: r.label, y: sub.map((t) => t.termSize!), boxpoints: 'all' as const, jitter: 0.5, pointpos: 0,
      marker: { size: 4, color: colors[r.id], opacity: 0.6 }, line: { color: colors[r.id] }, customdata: sub.map((t) => t.id),
      text: sub.map((t) => t.name), hovertemplate: '%{text}<br>%{y} genes<extra>%{fullData.name}</extra>',
    }
  })

  const agreeBars = [{
    type: 'bar' as const, x: d.agreement.map((a) => `${a.runs} of ${runs.length}`), y: d.agreement.map((a) => a.terms),
    marker: { color: d.agreement.map((a) => (a.runs === runs.length ? '#16a34a' : a.runs === 1 ? '#a1a1aa' : '#8b5cf6')) },
    customdata: d.agreement.map((a) => `agree|${a.runs}`), hovertemplate: '%{y} terms found by %{x} runs<extra></extra>',
  }]

  const resolve = (keys: string[], additive: boolean) => {
    const out = new Set<string>()
    keys.forEach((k) => {
      const [kind, ...rest] = k.split('|')
      if (kind === 'src') { const [r, s] = rest; d.terms.filter((t) => t.source === s && pOf(t, r) != null).forEach((t) => out.add(t.id)) }
      else if (kind === 'agree') { const n = Number(rest[0]); d.terms.filter((t) => ids.filter((r) => pOf(t, r) != null).length === n).forEach((t) => out.add(t.id)) }
      else if (kind === 'cat') {
        const [a, b, kk] = rest; const k = Number(kk)
        const top = (r: string) => d.terms.filter((t) => pOf(t, r) != null).sort((x, y) => pOf(x, r)! - pOf(y, r)!).slice(0, k).map((t) => t.id)
        const tb = new Set(top(b)); top(a).filter((id) => tb.has(id)).forEach((id) => out.add(id))
      } else out.add(k)
    })
    onSelect([...out], additive)
  }

  const pairRows = d.pairs.map((p) => ({
    runA: label.get(p.a), runB: label.get(p.b), termsA: p.nA, termsB: p.nB, shared: p.shared, jaccard: p.jaccard, overlap: p.overlapCoef,
    geneAgreement: p.geneJaccard ?? '', spearmanAllTerms: p.spearmanAll ?? '', spearmanAllTermsPadj: p.spearmanAllPAdj ?? '', spearman: p.spearman ?? '', spearmanPadj: p.spearmanPAdj ?? '', kendall: p.kendall ?? '', kendallPadj: p.kendallPAdj ?? '',
    medianLog10pDiff: p.medianDiff ?? '', wilcoxonPadj: p.wilcoxonPAdj ?? '', mcnemarPadj: p.mcnemarPAdj ?? '',
  }))
  const pairCols = Object.keys(pairRows[0] ?? {}).map((k) => ({ key: k, label: k }))

  return (
    <div className="stack stats-view">
      <p className="muted small">{d.note} {d.filteredCount} terms after filters. Click bars, cells, curves or points — or drag a box or lasso on the scatter plots — to select terms.</p>

      <section className="chart-card">
        <h3>At a glance</h3>
        <div className="table-scroll">
          <table className="table">
            <thead><tr><th>Run</th><th>List</th><th>Method</th><th>Terms</th><th>Only this run</th><th>Median term size</th><th>Median −log10 p</th></tr></thead>
            <tbody>{runs.map((r) => (
              <tr key={r.id}><td><i className="dot" style={{ background: colors[r.id] }} /> {r.label}</td><td>{r.listName}</td><td>{r.paradigm}</td>
                <td>{r.terms}</td><td>{r.unique}</td><td>{fmt(r.medianTermSize, 0)}</td><td>{fmt(r.medianLog10p)}</td></tr>
            ))}</tbody>
          </table>
        </div>
        <ul className="stat-tests">
          <li><strong>Number of terms.</strong> {interpret(d.tests.detection, 'terms found by at least one run', 'Some runs report more terms than others, more than expected by chance.', 'No evidence that the runs report different numbers of terms.')}
            <br /><span className="muted small">This compares how many terms each run reports, not which terms are right. For agreement on the terms themselves, see Concordance at the top and Pairwise similarity.</span></li>
          <li><strong>Term size.</strong> {interpret(d.tests.termSize, 'genes per term', 'The runs favour terms of different sizes (broad vs. specific).', 'The runs report terms of similar size.')}</li>
          <li><strong>Significance.</strong> {interpret(d.tests.significance, '−log10 p of found terms', 'Some runs report systematically smaller p-values.', 'The runs report similar significance levels.')}
            <br /><span className="muted small">Tools compute and correct p-values differently, so a difference here describes how each tool reports, not which one is more reliable.</span></li>
        </ul>
      </section>

      <div className="chart-grid">
        <section className="chart-card">
          <h3>Concordance at the top</h3>
          <p className="muted small">Of each run's k most significant terms, the share that the other run also ranks in its top k. High curves mean the runs agree on what matters most. This is the most direct check that two tools agree: it uses each tool's own ranking, so it does not depend on how the tool computes p-values. Click a point to select the agreed terms.</p>
          <Plot colorKey="flame_stats_concordance" data={catTraces} height={360} onSelect={resolve}
            layout={{ xaxis: { title: { text: 'Top k terms' } }, yaxis: { title: { text: 'Shared in top k' }, tickformat: '.0%', range: [0, 1.05] }, legend: { orientation: 'h', y: -0.3 }, margin: { l: 60, r: 20, t: 10, b: 100 } }} />
        </section>
        <section className="chart-card">
          <h3>Agreement</h3>
          <p className="muted small">How many terms were found by how many runs. Click a bar to select those terms.</p>
          <Plot colorKey="flame_stats_agreement" data={agreeBars} height={360} onSelect={resolve} layout={{ xaxis: { title: { text: 'Found by' } }, yaxis: { title: { text: 'Terms' } } }} />
        </section>
      </div>

      <div className="chart-grid">
        <section className="chart-card">
          <div className="row between wrap">
            <h3>Pairwise similarity</h3>
            <select className="input input-sm" value={metric} onChange={(e) => setMetric(e.target.value as Metric)} aria-label="Similarity measure">
              {METRICS.map((m) => <option key={m.value} value={m.value}>{m.label}</option>)}
            </select>
          </div>
          <p className="muted small">{METRICS.find((m) => m.value === metric)?.hint} Click a cell to compare that pair below and select their shared terms.</p>
          <Plot colorKey="flame_stats_similarity" data={heat as never} height={Math.max(300, 70 * runs.length + 140)} layout={{ margin: { l: 140, r: 20, t: 10, b: 120 }, xaxis: { tickangle: -30 } }}
            onSelect={(keys) => {
              const [a, b] = (keys[0] ?? '').split('|')
              if (a && b && a !== b) { setPairSel([a, b]); onSelect(shared(a, b), false) }
            }} />
        </section>

        {d.mds && (
          <section className="chart-card">
            <h3>Map of runs (MDS)</h3>
            <p className="muted small">Runs close together report similar terms (distance = 1 − Jaccard). Click a run to select its terms.</p>
            <Plot colorKey="flame_stats_mds" height={360} data={[{
              type: 'scatter', mode: 'text+markers', x: d.mds.map((m) => m.x), y: d.mds.map((m) => m.y), text: d.mds.map((m) => label.get(m.id)!), textposition: 'top center',
              marker: { size: 16, color: d.mds.map((m) => colors[m.id]) }, customdata: d.mds.map((m) => `run|${m.id}`), hoverinfo: 'text',
            }]} layout={{ xaxis: { zeroline: false, showticklabels: false, title: { text: 'MDS 1' } }, yaxis: { zeroline: false, showticklabels: false, title: { text: 'MDS 2' } } }}
              onSelect={(keys, add) => onSelect(keys.flatMap((k) => { const r = k.split('|')[1]; return d.terms.filter((t) => pOf(t, r) != null).map((t) => t.id) }), add)} />
          </section>
        )}
      </div>

      {scatter && pair && (
        <section className="chart-card">
          <div className="row between wrap">
            <h3>Concordance of significance</h3>
            <span className="row wrap">
              <select className="input input-sm" value={pair[0]} aria-label="Run on the x-axis" onChange={(e) => setPairSel([e.target.value, pair[1] === e.target.value ? pair[0] : pair[1]])}>
                {runs.map((r) => <option key={r.id} value={r.id}>{r.label}</option>)}</select>
              <span className="muted">vs</span>
              <select className="input input-sm" value={pair[1]} aria-label="Run on the y-axis" onChange={(e) => setPairSel([pair[0] === e.target.value ? pair[1] : pair[0], e.target.value])}>
                {runs.map((r) => <option key={r.id} value={r.id}>{r.label}</option>)}</select>
              <label className="check"><input type="checkbox" checked={onlyShared} onChange={(e) => setOnlyShared(e.target.checked)} /> Shared terms only</label>
            </span>
          </div>
          {pp && (
            <p className="small">
              {pp.shared} shared of {pp.union} terms (Jaccard {fmt(pp.jaccard)}).
              {' '}Rank agreement over all their terms: Spearman ρ = {fmt(pp.spearmanAll)} (adj. p {formatP(pp.spearmanAllPAdj)}); on the shared terms only: ρ = {fmt(pp.spearman)} (adj. p {formatP(pp.spearmanPAdj)}), Kendall τ = {fmt(pp.kendall)} (adj. p {formatP(pp.kendallPAdj)}).
              {' '}Median difference in −log10 p: {fmt(pp.medianDiff)} (paired Wilcoxon adj. p {formatP(pp.wilcoxonPAdj)}){pp.medianDiff != null && pp.wilcoxonPAdj != null && pp.wilcoxonPAdj < 0.05 ? ` — ${pp.medianDiff > 0 ? label.get(pp.a) : label.get(pp.b)} reports stronger p-values` : ''}.
              {' '}Gene agreement in shared terms: {fmt(pp.geneJaccard)}.
            </p>
          )}
          <p className="muted small">Points on the diagonal are equally significant in both runs; points on an axis were found by only one run. Drag a box or lasso to select terms.</p>
          <Plot colorKey="flame_stats_pvalues" data={scatter.traces} layout={scatter.layout} height={480} dragmode="lasso" onSelect={resolve} />
        </section>
      )}

      <div className="chart-grid">
        <section className="chart-card">
          <h3>Terms per annotation source</h3>
          <p className="muted small">Which databases each run's findings come from. Click a segment to select those terms.</p>
          <Plot colorKey="flame_stats_sources" data={srcBars} height={380} onSelect={resolve} layout={{ barmode: 'stack', legend: { orientation: 'h', y: -0.3 }, yaxis: { title: { text: 'Terms' } }, margin: { l: 60, r: 20, t: 10, b: 110 } }} />
        </section>
        <section className="chart-card">
          <h3>Term size</h3>
          <p className="muted small">Genes per term for each run: lower = more specific terms. Box or lasso-select points to select terms.</p>
          <Plot colorKey="flame_stats_sizes" data={boxes} height={380} dragmode="select" onSelect={resolve} layout={{ yaxis: { title: { text: 'Term size (genes)' }, type: 'log' }, showlegend: false }} />
        </section>
      </div>

      <section className="chart-card">
        <div className="row between wrap">
          <h3>Pairwise statistics</h3>
          <span className="row">
            <button className="btn btn-ghost btn-sm" title="Excel workbook: runs, pairwise statistics, tests, agreement" onClick={() => saveXlsx([
              { name: 'Runs', rows: runs.map((r) => ({ Run: r.label, List: r.listName, Method: r.paradigm, Terms: r.terms, 'Only this run': r.unique, 'Median term size': r.medianTermSize, 'Median -log10 p': r.medianLog10p, ...Object.fromEntries(Object.entries(r.bySource ?? {}).map(([k, v]) => [`Terms ${k}`, v])) })) },
              { name: 'Pairs', rows: pairRows as Record<string, unknown>[] },
              { name: 'Tests', rows: Object.entries(d.tests).filter(([, t]) => t).map(([k, t]) => ({ Test: k, Method: t!.method, Statistic: t!.statistic, df: t!.df, P: t!.p })) },
              { name: 'Agreement', rows: d.agreement.map((a) => ({ 'Found by (runs)': a.runs, Terms: a.terms })) },
            ], 'flame_comparison_statistics.xlsx')}>Excel</button>
            <button className="btn btn-ghost btn-sm" onClick={() => saveText(toCsv(pairRows as Record<string, unknown>[], pairCols), 'flame_comparison_statistics.csv', 'text/csv')}>CSV</button>
          </span>
        </div>
        <div className="table-scroll">
          <table className="table">
            <thead><tr><th>Runs</th><th>Shared / all</th><th>Jaccard</th><th>Overlap</th><th>Gene agreement</th><th title="Over every term either run found">Spearman ρ (all terms)</th><th title="Only terms both runs found">Spearman ρ (shared)</th><th title="Only terms both runs found">Kendall τ (shared)</th><th title="Positive: the first run reports smaller p-values. Tools compute p-values differently, so this describes reporting, not reliability.">Δ median −log10 p</th><th title="Do the two runs report different numbers of terms?">Number of terms (McNemar)</th></tr></thead>
            <tbody>{d.pairs.map((p) => (
              <tr key={`${p.a}|${p.b}`} className="clickable" onClick={() => { setPairSel([p.a, p.b]); onSelect(shared(p.a, p.b), false) }}>
                <td>{label.get(p.a)} vs {label.get(p.b)}</td><td>{p.shared} / {p.union}</td><td>{fmt(p.jaccard)}</td><td>{fmt(p.overlapCoef)}</td><td>{fmt(p.geneJaccard)}</td>
                <td>{fmt(p.spearmanAll)} <span className="muted small">({formatP(p.spearmanAllPAdj)})</span></td>
                <td>{fmt(p.spearman)} <span className="muted small">({formatP(p.spearmanPAdj)})</span></td>
                <td>{fmt(p.kendall)} <span className="muted small">({formatP(p.kendallPAdj)})</span></td>
                <td>{fmt(p.medianDiff)} <span className="muted small">({formatP(p.wilcoxonPAdj)})</span></td>
                <td>{formatP(p.mcnemarPAdj)}</td>
              </tr>
            ))}</tbody>
          </table>
        </div>
        <p className="muted small">Adjusted p-values (Benjamini–Hochberg across pairs) in brackets. Click a row to compare that pair above.</p>
      </section>
    </div>
  )
}
