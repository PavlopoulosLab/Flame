import { useMemo } from 'react'
import type { ComparisonCell, ComparisonData, ComparisonRunMeta, ComparisonTerm, TopTermsPanel } from '../../api/types'
import { Plot, type PlotHandle } from '../../components/charts'
import { CopyButton, Note } from '../../components/ui'
import { formatP } from '../../lib/format'
import { usePaletteMode } from '../../lib/palette'

export type TopBarMode = 'tools' | 'score'

export const cellOf = (t: ComparisonTerm, run: string): ComparisonCell | null => {
  const c = t.cells?.[run]
  return c && typeof (c as ComparisonCell).p === 'number' ? (c as ComparisonCell) : null
}

const SORT_TEXT: Record<string, string> = {
  Consensus: 'consensus across tools (Robust Rank Aggregation of each tool’s ranking; Kolde et al., 2012)',
  Agreement: 'the number of tools that reported them, then their mean rank',
  'Best rank': 'their best rank in any single tool',
  '-log10Pvalue': 'Fisher’s combined p-value',
  'Fold Enrichment': 'mean fold enrichment',
}

/** Rank bins for the dot colour: the position of the term in that tool's own results. */
const RANK_BINS: { max: number; label: string }[] = [
  { max: 5, label: '1–5' }, { max: 10, label: '6–10' }, { max: 25, label: '11–25' },
  { max: 50, label: '26–50' }, { max: 100, label: '51–100' }, { max: Infinity, label: '>100' },
]
const RAMPS: Record<string, string[]> = {
  flame: ['#7f1d1d', '#c2410c', '#f97316', '#fbbf24', '#fde68a', '#fefce8'],
  cb: ['#00204d', '#31446b', '#666970', '#958f78', '#cbba69', '#ffea46'],
  gray: ['#18181b', '#3f3f46', '#71717a', '#a1a1aa', '#d4d4d8', '#f4f4f5'],
}
const BAR_COLOR: Record<string, string> = { flame: '#7c3aed', cb: '#31446b', gray: '#52525b' }

/** Column label: the tool name, with the run number only when a tool appears more than once. */
export function runLabeller(runs: ComparisonRunMeta[]) {
  const n = new Map<string, number>()
  runs.forEach((r) => n.set(r.tool, (n.get(r.tool) ?? 0) + 1))
  return (r: ComparisonRunMeta) => ((n.get(r.tool) ?? 0) > 1 ? r.label : r.tool)
}

const pct = (x: number) => `${Number((100 * x).toPrecision(x >= 0.1 ? 3 : 2))}%`

/**
 * Best terms across the compared runs, ready for a figure. Optionally one panel per annotation
 * source. Left: dot plot (term x tool; colour = rank of the term in that tool's results, size =
 * gene ratio, empty = not reported). Right: bars (how many tools reported the term, or the
 * consensus score). All panels share their axes, so the figure exports as one image.
 */
export function TopTerms({ data, labels, barMode, collapseCutoff, selected, onSelect, plotRef }: {
  data: ComparisonData; labels: 'Function' | 'Term_ID'; barMode: TopBarMode; collapseCutoff: number; selected: Set<string>
  onSelect: (ids: string[], additive: boolean) => void; plotRef: { current: PlotHandle | null }
}) {
  const pm = usePaletteMode()
  const runs = useMemo(() => data.runs.filter((r) => data.activeRunIds.includes(r.id)), [data])
  const M = runs.length
  const panels: TopTermsPanel[] = useMemo(() => (data.panels?.length ? data.panels : [{ source: null, terms: data.terms }]), [data])
  const rows = panels.reduce((n, p) => n + p.terms.length, 0)
  // Split by source: one standalone plot per source; otherwise one plot with every source.
  const standalone = panels.length > 1 || !!panels[0]?.source
  const heightOf = (group: TopTermsPanel[]) => Math.max(420, 24 * group.reduce((n, p) => n + p.terms.length, 0) + 46 * group.length + 280)

  const figs = useMemo(() => {
    const colLabel = runLabeller(runs)
    const cols = runs.map(colLabel)
    const ramp = RAMPS[pm] ?? RAMPS.flame
    const barColor = BAR_COLOR[pm] ?? BAR_COLOR.flame
    const grid = getComputedStyle(document.documentElement).getPropertyValue('--border-color').trim() || '#e4e4e7'
    const allTerms = panels.flatMap((p) => p.terms)
    const ratioOf = (c: ComparisonCell) => c.geneRatio ?? null
    const useRatio = allTerms.some((t) => runs.some((r) => { const c = cellOf(t, r.id); return c != null && ratioOf(c) != null }))
    const sizeVal = (c: ComparisonCell) => (useRatio ? ratioOf(c) ?? 0 : c.genes)
    const maxSize = Math.max(1e-9, ...allTerms.flatMap((t) => runs.map((r) => { const c = cellOf(t, r.id); return c ? sizeVal(c) : 0 })))
    const size = (v: number) => 6 + 16 * Math.sqrt(Math.max(0, v) / maxSize)
    const nameCount = new Map<string, number>()
    allTerms.forEach((t) => nameCount.set(t.name, (nameCount.get(t.name) ?? 0) + 1))
    const name = (t: ComparisonTerm, split: boolean) => {
      const base = labels === 'Term_ID' ? t.id : t.name.length > 60 ? `${t.name.slice(0, 59)}…` : t.name
      const dup = !split && labels !== 'Term_ID' && (nameCount.get(t.name) ?? 0) > 1
      const n = dup ? `${base} [${t.source}]` : base
      return t.represents && t.represents > 1 ? `${n} (+${t.represents - 1})` : n
    }
    const split = standalone
    const scoreLine = -Math.log10(0.05)
    const maxScore = Math.max(scoreLine * 1.1, ...allTerms.map((t) => -Math.log10(Math.max(t.consensusScore ?? 1, 1e-300))))

    // Scales (dot size, bar axis) are shared by all plots, so separate plots stay comparable.
    const build = (panels: TopTermsPanel[]) => {
    const rows = panels.reduce((n, p) => n + p.terms.length, 0)
    const height = heightOf(panels)
    // Vertical layout: panels stacked, each as tall as its number of terms.
    const top = 1, gap = panels.length > 1 ? 46 / height : 0
    const usable = top - gap * (panels.length - 1)
    let cursor = top
    const domains = panels.map((p) => { const h = usable * (p.terms.length / Math.max(1, rows)); const d: [number, number] = [Math.max(0, cursor - h), cursor]; cursor -= h + gap; return d })

    const traces: Record<string, unknown>[] = []
    const layout: Record<string, unknown> = {
      margin: { l: 20, r: 20, t: 20, b: 110 }, showlegend: true, barmode: 'overlay', bargap: 0.3,
      legend: { orientation: 'h', y: -70 / height, yanchor: 'top', x: 0, xanchor: 'left', groupclick: 'toggleitem', traceorder: 'grouped', entrywidth: 0, tracegroupgap: 24 },
      annotations: [] as unknown[], shapes: [] as unknown[],
    }
    const binShown = new Set<number>()

    panels.forEach((p, i) => {
      const last = i === panels.length - 1
      const yId = i === 0 ? 'y' : `y${i + 1}`
      const dx = 2 * i + 1, bx = 2 * i + 2
      const dId = dx === 1 ? 'x' : `x${dx}`, bId = `x${bx}`
      const ids = p.terms.map((t) => t.id)
      layout[i === 0 ? 'yaxis' : `yaxis${i + 1}`] = {
        domain: domains[i], type: 'category', categoryorder: 'array', categoryarray: ids, tickvals: ids, ticktext: p.terms.map((t) => name(t, split)),
        autorange: 'reversed', automargin: true, gridcolor: grid, anchor: dId,
      }
      layout[dx === 1 ? 'xaxis' : `xaxis${dx}`] = {
        domain: [0, 0.56], type: 'category', categoryorder: 'array', categoryarray: cols, anchor: yId, gridcolor: grid,
        ...(i === 0 ? { side: 'top', tickangle: -45, automargin: true } : { matches: 'x', showticklabels: false }),
      }
      layout[`xaxis${bx}`] = {
        domain: [0.62, 0.98], anchor: yId, gridcolor: grid, zeroline: false, showticklabels: last,
        ...(barMode === 'tools'
          ? { range: [0, M + 0.2], dtick: 1, title: last ? { text: `Tools that reported the term (of ${M})` } : undefined }
          : { range: [0, maxScore * 1.05], title: last ? { text: 'Consensus: −log10 RRA score (dashed: 0.05)' } : undefined }),
        ...(i > 0 ? { matches: 'x2' } : {}),
      }
      if (panels.length > 1 && p.source) {
        ;(layout.annotations as unknown[]).push({ xref: 'paper', yref: 'paper', x: 0, xanchor: 'right', y: domains[i][1], yanchor: 'bottom', yshift: 4, text: `<b>${p.source}</b>`, showarrow: false, font: { size: 13 } })
      }
      if (barMode === 'score') {
        ;(layout.shapes as unknown[]).push({ type: 'line', xref: bId, yref: 'paper', x0: scoreLine, x1: scoreLine, y0: domains[i][0], y1: domains[i][1], line: { dash: 'dash', width: 1, color: '#71717a' } })
      }

      // Dots, one trace per rank bin (so the legend lists the bins).
      RANK_BINS.forEach((bin, bi) => {
        const lo = bi === 0 ? 0 : RANK_BINS[bi - 1].max
        const pts = p.terms.flatMap((t) => runs.flatMap((r, ri) => { const c = cellOf(t, r.id); return c && c.rank > lo && c.rank <= bin.max ? [{ t, r, c, col: cols[ri] }] : [] }))
        if (!pts.length) return
        binShown.add(bi)
        traces.push({
          type: 'scatter', mode: 'markers', xaxis: dId, yaxis: yId, name: bin.label, legendgroup: 'rank', showlegend: false,
          x: pts.map((q) => q.col), y: pts.map((q) => q.t.id), customdata: pts.map((q) => q.t.id), hoverinfo: 'text',
          text: pts.map((q) => `${q.t.name} (${q.t.id})<br>${q.col}: rank ${q.c.rank} · p = ${formatP(q.c.p)}<br>${q.c.genes} genes${q.c.geneRatio != null ? ` (${pct(q.c.geneRatio)} of the input genes it recognised)` : ''}`),
          marker: {
            size: pts.map((q) => size(sizeVal(q.c))), color: ramp[bi], opacity: 1,
            line: { width: pts.map((q) => (selected.has(q.t.id) ? 3 : 0.6)), color: pts.map((q) => (selected.has(q.t.id) ? '#f97316' : 'rgba(0,0,0,0.45)')) },
          },
        })
      })

      // Bars.
      traces.push({
        type: 'bar', orientation: 'h', xaxis: bId, yaxis: yId, showlegend: false, name: 'bars',
        x: p.terms.map((t) => (barMode === 'tools' ? t.agreement ?? 0 : -Math.log10(Math.max(t.consensusScore ?? 1, 1e-300)))),
        y: ids, customdata: ids, textposition: 'none', hoverinfo: 'text',
        text: p.terms.map((t) => `${t.name} (${t.id})<br>reported by ${t.agreement} of ${M} tools<br>RRA score ${formatP(t.consensusScore)}${t.represents && t.represents > 1 ? `<br>stands for ${t.represents} similar terms:<br>${(t.merged ?? '').split(' | ').slice(0, 6).join('<br>')}` : ''}`),
        marker: { color: barColor, line: { width: p.terms.map((t) => (selected.has(t.id) ? 3 : 0)), color: '#f97316' } },
      })
    })

    // Colour legend: one fixed-size swatch per rank bin that occurs.
    RANK_BINS.forEach((bin, bi) => {
      if (!binShown.has(bi)) return
      traces.push({ type: 'scatter', mode: 'markers', xaxis: 'x', yaxis: 'y', x: [null], y: [null], name: bin.label, legendgroup: 'rank', legendrank: bi,
        legendgrouptitle: { text: 'Rank in that tool:' }, marker: { size: 11, color: ramp[bi], line: { width: 0.6, color: 'rgba(0,0,0,0.45)' } }, hoverinfo: 'skip' })
    })
    // Size legend: reference dots without data.
    const refs = [...new Set([maxSize, maxSize / 2, maxSize / 8].map((v) => (useRatio ? Number(v.toPrecision(1)) : Math.max(1, Math.round(v)))))].filter((v) => v > 0)
    refs.forEach((v, k) => traces.push({
      type: 'scatter', mode: 'markers', xaxis: 'x', yaxis: 'y', x: [null], y: [null], name: useRatio ? pct(v) : v === 1 ? '1 gene' : `${v} genes`,
      legendgroup: 'size', legendrank: 100 + k, legendgrouptitle: { text: useRatio ? 'Gene ratio:' : 'Genes:' },
      marker: { size: size(v), color: '#a1a1aa', line: { width: 0.6, color: 'rgba(0,0,0,0.45)' } }, hoverinfo: 'skip',
    }))
    return { traces, layout, height, title: panels.length === 1 && panels[0].source ? panels[0].source : undefined, key: panels.map((p) => p.source ?? 'all').join('|') }
    }
    return standalone ? panels.map((p) => build([p])) : [build(panels)]
  }, [panels, runs, labels, selected, pm, M, barMode, standalone])

  if (!rows) {
    return <Note kind="warning">{data.minAgreement == null ? 'No terms match the current filters.' : `No term was reported by at least ${data.minAgreement} of ${M} tools under the current filters. Lower “Found by at least”.`}</Note>
  }
  const caption = figureLegend(data, runs, panels, barMode, collapseCutoff)
  return (
    <div className="stack">
      <p className="muted small">
        {rows} of {data.eligibleCount} terms reported by at least {data.minAgreement} of {M} tools{standalone ? `, one plot per annotation source (${panels.length}), each ranked on its own; save each with its “Image” button` : ''}, ranked by {SORT_TEXT[data.sortBy ?? 'Consensus'] ?? data.sortBy}
        {data.collapsed ? `; ${data.collapsed} similar terms merged into a representative (+n after the name)` : ''}.
      </p>
      {figs.map((fig, i) => (
        <section key={fig.key} className={standalone ? 'chart-card' : undefined}>
          <Plot handleRef={i === 0 ? plotRef : undefined} colorKey="flame_comparison_top_terms" exportName={`flame_comparison_top_terms${fig.title ? `_${fig.title.replace(/[^A-Za-z0-9]+/g, '_')}` : ''}`}
            title={fig.title} height={fig.height} onSelect={onSelect} data={fig.traces as never} layout={fig.layout as never} />
        </section>
      ))}
      <div className="chart-card">
        <div className="row between wrap"><strong className="small">Figure legend</strong><CopyButton label="Copy legend" text={caption} /></div>
        <p className="small">{caption}</p>
      </div>
    </div>
  )
}

/** A figure legend that states what the figure shows and how the terms were chosen. */
function figureLegend(data: ComparisonData, runs: ComparisonRunMeta[], panels: TopTermsPanel[], barMode: TopBarMode, collapseCutoff: number) {
  const label = runLabeller(runs)
  const lists = [...new Set(runs.map((r) => r.listName))]
  const orgs = [...new Set(runs.map((r) => r.organism).filter(Boolean))]
  const sizes = [...new Set(runs.map((r) => r.inputSize).filter((n): n is number => !!n))]
  const thr = [...new Set(runs.map((r) => `${r.metric ?? ''} ${r.threshold ?? ''}`.trim()).filter(Boolean))]
  const M = runs.length
  const src = panels.length > 1 || panels[0]?.source ? ` One plot per annotation source (${panels.map((p) => p.source).join(', ')}); terms are ranked within each source.` : ''
  return [
    `Top enriched terms reported by at least ${data.minAgreement} of ${M} enrichment tools (${runs.map(label).join(', ')})`,
    ` for the gene list${lists.length > 1 ? 's' : ''} ${lists.map((l) => `“${l}”`).join(', ')}${sizes.length === 1 ? ` (${sizes[0]} genes` : ' ('}${orgs.length ? `${sizes.length === 1 ? ', ' : ''}${orgs.join(', ')}` : ''})`,
    `${thr.length === 1 ? `; significance threshold ${thr[0]} in each tool` : thr.length ? '; significance thresholds as set in each tool' : ''}.`,
    ` Terms are ordered by ${SORT_TEXT[data.sortBy ?? 'Consensus'] ?? data.sortBy}.`,
    data.collapsed ? ` Terms of the same source sharing at least ${Math.round(collapseCutoff * 100)}% of their genes are merged into the higher-ranked term (+n: number of merged terms).` : '',
    src,
    ' Dots: the tool reported the term; colour, the rank of the term among that tool’s results (1 = most significant); size, the gene ratio (genes in the term ÷ input genes the tool recognised). No dot: the tool did not report the term.',
    barMode === 'tools' ? ' Bars: number of tools that reported the term.'
      : ' Bars: consensus score, −log10 of the Robust Rank Aggregation score (the probability of ranks this good if the tools ranked terms at random, Bonferroni-corrected); dashed line, 0.05.',
  ].join('')
}

/** Rows for the CSV export of the shown top terms. */
export function topTermsRows(data: ComparisonData) {
  const runs = data.runs.filter((r) => data.activeRunIds.includes(r.id))
  const label = runLabeller(runs)
  const rows = data.terms.map((t, i) => ({
    position: i + 1, source: t.source, id: t.id, name: t.name, foundBy: t.agreement, consensusScore: t.consensusScore,
    meanRank: t.meanRank, bestRank: t.bestRank, fisherCombinedP: t.activeCombPvalue ?? '', represents: t.represents ?? 1, merged: t.merged ?? '',
    ...Object.fromEntries(runs.flatMap((r) => {
      const c = cellOf(t, r.id); const l = label(r)
      return [[`p ${l}`, c?.p ?? ''], [`rank ${l}`, c?.rank ?? ''], [`genes ${l}`, c?.genes ?? ''], [`gene ratio ${l}`, c?.geneRatio ?? '']]
    })),
  }))
  const cols = Object.keys(rows[0] ?? {}).map((k) => ({ key: k, label: k }))
  return { rows, cols }
}
