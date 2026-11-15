import { useMemo, useState } from 'react'
import type { ComparisonRunMeta } from '../../api/types'
import { Plot } from '../../components/charts'
import { CopyButton } from '../../components/ui'
import { formatP } from '../../lib/format'
import { aggregateRanks } from '../../lib/rankAgg'
import { categorical, usePaletteMode } from '../../lib/palette'
import { hit, type Row } from './CompareTable'

/**
 * Bump chart: the rank of each term in each run (1 = most significant), for
 * the terms with the best consensus rank (Robust Rank Aggregation). Lines that
 * stay at the top are terms every run agrees on; click a line to select it.
 */
export function RankChart({ terms, runs, selected, onSelect, labels }: {
  terms: Row[]; runs: ComparisonRunMeta[]; colorOf?: (source: string) => string
  selected: Set<string>; onSelect: (ids: string[], additive: boolean) => void; labels: 'Function' | 'Term_ID'
}) {
  const [n, setN] = useState(20)
  const pal = categorical(usePaletteMode())
  const ids = runs.map((r) => r.id)
  const byId = useMemo(() => new Map(terms.map((t) => [t.id, t])), [terms])
  const agg = useMemo(() => aggregateRanks(terms.map((t) => t.id), ids, (tid, r) => { const t = byId.get(tid); return t ? hit(t, r)?.p ?? null : null }),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [terms, ids.join()])
  const top = [...agg.values()].sort((a, b) => a.consensus - b.consensus).slice(0, n)
  const worst = Math.max(1, ...top.flatMap((r) => Object.values(r.ranks).filter((x): x is number => x != null)))
  const missing = Math.ceil(worst * 1.08) + 1
  const name = (id: string) => { const t = byId.get(id); return t ? (labels === 'Term_ID' ? t.id : t.name) : id }
  const short = (s: string) => (s.length > 34 ? `${s.slice(0, 33)}…` : s)
  return (
    <div className="stack-sm">
      <div className="toolbar">
        <label>Show <select className="input input-sm" value={n} onChange={(e) => setN(Number(e.target.value))}>{[10, 20, 30, 50].map((k) => <option key={k} value={k}>top {k} consensus terms</option>)}</select></label>
        <span className="muted small">y = rank of the term within each run; points at the bottom line (“not found”) mean the run did not report it.</span>
        <span className="toolbar-spacer" />
        <CopyButton label="Copy ranked names" text={top.map((r) => `${r.consensus}\t${name(r.termId)}`).join('\n')} />
      </div>
      <Plot height={Math.max(420, 22 * n + 120)} dragmode="zoom"
        onSelect={(keys, add) => keys.length && onSelect([keys[0]], add)}
        data={top.map((r, i) => {
          const t = byId.get(r.termId)
          const sel = selected.has(r.termId)
          return {
            type: 'scatter', mode: 'lines+markers', name: `#${r.consensus} ${short(name(r.termId))}`,
            x: runs.map((x) => x.label), y: ids.map((id) => r.ranks[id] ?? missing),
            customdata: ids.map(() => r.termId),
            text: ids.map((id) => { const h = t ? hit(t, id) : null; return h ? `rank ${r.ranks[id]} · p = ${formatP(h.p)} · ${h.genes} genes` : 'not reported by this run' }),
            hovertemplate: `<b>${name(r.termId).replace(/</g, '&lt;')}</b> (${t?.source ?? ''})<br>%{x}: %{text}<br>consensus #${r.consensus} (RRA ${r.rra.toExponential(1)})<extra></extra>`,
            line: { width: sel ? 4 : 2, color: pal[i % pal.length], dash: i < pal.length ? 'solid' : 'dot' }, opacity: selected.size && !sel ? 0.35 : 0.9,
            marker: { size: ids.map((id) => (r.ranks[id] == null ? 6 : 9)), symbol: ids.map((id) => (r.ranks[id] == null ? 'x' : 'circle')) },
          } as never
        })}
        layout={{ yaxis: { type: 'log', autorange: 'reversed', title: { text: 'rank in run (1 = most significant, log scale)' }, zeroline: false,
          tickvals: [1, ...[5, 10, 20, 50, 100, 200, 500].filter((v) => v < missing), missing], ticktext: ['1', ...[5, 10, 20, 50, 100, 200, 500].filter((v) => v < missing).map(String), 'not found'] },
          xaxis: { type: 'category' }, legend: { font: { size: 11 } }, margin: { l: 70, r: 10, t: 10, b: 60 } }} />
      <p className="muted small">Consensus order: Robust Rank Aggregation of the per-run ranks (Kolde et al. 2012, Bioinformatics 28:573). Each line is one term (colours repeat with dotted lines after ten). Click a line or point to select the term.</p>
    </div>
  )
}
