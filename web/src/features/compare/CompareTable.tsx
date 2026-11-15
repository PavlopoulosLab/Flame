import { useMemo, useState } from 'react'
import { saveText } from '../../api/client'
import type { ComparisonRunMeta, ComparisonTerm } from '../../api/types'
import { CopyButton } from '../../components/ui'
import { ToolLogo } from '../../components/ToolLogo'
import { formatP, toCsv } from '../../lib/format'
import { aggregateRanks } from '../../lib/rankAgg'

type ByRun = Record<string, { p: number; genes: number } | null | Record<string, never>>
export type Row = ComparisonTerm & { byRun?: ByRun }
export const hit = (t: Row, r: string) => { const x = t.byRun?.[r]; return x && typeof (x as { p?: number }).p === 'number' ? (x as { p: number; genes: number }) : null }

/**
 * Combined results as one table: every term with the runs (tools, lists or
 * strategies) that found it, each run's own p-value and gene count, the
 * agreement and Fisher's combined p-value.
 */
export function CompareTable({ terms, runs, colors, selected, onSelect, labels }: {
  terms: Row[]; runs: ComparisonRunMeta[]; colors: Record<string, string>
  selected: Set<string>; onSelect: (ids: string[], additive: boolean) => void; labels: 'Function' | 'Term_ID'
}) {
  const [sort, setSort] = useState<{ key: string; dir: 1 | -1 }>({ key: 'rank', dir: 1 })
  const [cells, setCells] = useState<'p' | 'rank'>('p')
  const [only, setOnly] = useState<'all' | 'consensus' | 'unique'>('all')
  const [limit, setLimit] = useState(300)
  const ids = runs.map((r) => r.id)
  const found = (t: Row) => ids.filter((r) => hit(t, r))
  const agg = useMemo(() => aggregateRanks(terms.map((t) => t.id), ids, (() => { const byId = new Map(terms.map((t) => [t.id, t])); return (tid: string, r: string) => { const t = byId.get(tid); return t ? hit(t, r)?.p ?? null : null } })()),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [terms, ids.join()])
  const rows = useMemo(() => {
    let out = terms.filter((t) => (only === 'all' ? true : only === 'consensus' ? found(t).length === ids.length : found(t).length === 1))
    const val = (t: Row): number | string => {
      if (sort.key === 'comb') return t.combPvalue ?? 1
      if (sort.key === 'rank') return agg.get(t.id)?.consensus ?? 1e9
      if (sort.key === 'mean') return agg.get(t.id)?.meanRank ?? 1e9
      if (sort.key === 'agree') return found(t).length
      if (sort.key === 'name') return t.name.toLowerCase()
      if (sort.key === 'source') return t.source
      if (sort.key === 'genes') return t.intersectionCount ?? 0
      return cells === 'rank' ? agg.get(t.id)?.ranks[sort.key] ?? 1e9 : hit(t, sort.key)?.p ?? 2
    }
    out = [...out].sort((a, b) => { const x = val(a), y = val(b); return (x < y ? -1 : x > y ? 1 : 0) * sort.dir })
    return out
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [terms, sort, only, ids.join(), agg, cells])
  const head = (key: string, label: React.ReactNode, defDir: 1 | -1 = 1) => (
    <th className="sortable" aria-sort={sort.key === key ? (sort.dir === 1 ? 'ascending' : 'descending') : 'none'}
      onClick={() => setSort((s) => (s.key === key ? { key, dir: (s.dir * -1) as 1 | -1 } : { key, dir: defDir }))}>
      {label}{sort.key === key ? (sort.dir === 1 ? ' ▲' : ' ▼') : ''}
    </th>
  )
  const exportRows = rows.map((t) => {
    const r: Record<string, unknown> = { term_id: t.id, term: t.name, source: t.source, found_by: found(t).map((x) => runs.find((y) => y.id === x)?.label).join('; '),
      agreement: `${found(t).length}/${ids.length}`, consensus_rank: agg.get(t.id)?.consensus, rra_score: agg.get(t.id)?.rra, mean_rank: agg.get(t.id)?.meanRank, combined_p: t.combPvalue, shared_genes: t.intersectionCount, all_genes: t.unionCount }
    runs.forEach((run) => { const h = hit(t, run.id); r[`p_${run.label}`] = h ? h.p : ''; r[`rank_${run.label}`] = agg.get(t.id)?.ranks[run.id] ?? ''; r[`genes_${run.label}`] = h ? h.genes : '' })
    return r
  })
  const exportCols = Object.keys(exportRows[0] ?? {}).map((k) => ({ key: k, label: k }))
  return (
    <div className="stack-sm">
      <div className="toolbar">
        <label>Show <select className="input input-sm" value={only} onChange={(e) => setOnly(e.target.value as typeof only)}>
          <option value="all">all terms</option><option value="consensus">found by every run</option><option value="unique">found by one run only</option>
        </select></label>
        <label>Run columns show <select className="input input-sm" value={cells} onChange={(e) => setCells(e.target.value as 'p' | 'rank')}>
          <option value="p">p-value · genes</option><option value="rank">rank in that run</option>
        </select></label>
        <span className="muted small">{rows.length.toLocaleString()} terms · click a header to sort, a row to select (Shift adds)</span>
        <span className="toolbar-spacer" />
        <CopyButton label="Copy" text={toCsv(exportRows, exportCols).replace(/,/g, '\t')} />
        <button className="btn btn-ghost btn-sm" onClick={() => saveText(toCsv(exportRows, exportCols), 'flame_comparison_table.csv', 'text/csv')}>CSV</button>
      </div>
      <div className="table-scroll compare-table">
        <table className="table">
          <thead>
            <tr>
              {head('name', 'Term')}
              {head('source', 'Source')}
              <th>Found by</th>
              {head('rank', <span title="Consensus rank across runs: Robust Rank Aggregation of each run's p-value ranks (Kolde et al. 2012)">Consensus rank</span>)}
              {head('agree', 'Agreement', -1)}
              {head('comb', 'Combined p')}
              {runs.map((r) => (
                <th key={r.id} className="sortable run-col" onClick={() => setSort((s) => (s.key === r.id ? { key: r.id, dir: (s.dir * -1) as 1 | -1 } : { key: r.id, dir: 1 }))}
                  style={{ borderTop: `3px solid ${colors[r.id]}` }} title={`${r.label} on ${r.listName}: p-value and genes found`}>
                  <span className="run-col-head"><ToolLogo tool={r.tool} size={16} /> {r.label}{sort.key === r.id ? (sort.dir === 1 ? ' ▲' : ' ▼') : ''}</span>
                  <span className="muted small">{r.listName}</span>
                </th>
              ))}
              {head('genes', 'Shared genes', -1)}
            </tr>
          </thead>
          <tbody>
            {rows.slice(0, limit).map((t) => {
              const f = found(t)
              return (
                <tr key={t.id} className={`clickable${selected.has(t.id) ? ' selected' : ''}`} onClick={(e) => onSelect([t.id], e.shiftKey || e.metaKey || e.ctrlKey)}>
                  <td className="term-cell" title={`${t.name} (${t.id})`}>{labels === 'Term_ID' ? t.id : t.name}<span className="muted small"> {labels === 'Term_ID' ? t.name : t.id}</span></td>
                  <td>{t.source}</td>
                  <td><span className="found-by">{runs.map((r) => (
                    <span key={r.id} className={`found-chip${f.includes(r.id) ? ' on' : ''}`} style={f.includes(r.id) ? { borderColor: colors[r.id], background: `color-mix(in srgb, ${colors[r.id]} 14%, transparent)` } : undefined}
                      title={`${r.label}: ${f.includes(r.id) ? 'found' : 'not found'}`}><ToolLogo tool={r.tool} size={14} /></span>
                  ))}</span></td>
                  <td title={`RRA score ${agg.get(t.id)?.rra.toExponential(2)} · mean rank ${agg.get(t.id)?.meanRank.toFixed(1)}`}><strong>#{agg.get(t.id)?.consensus}</strong></td>
                  <td><span className={`agree${f.length === ids.length ? ' all' : f.length === 1 ? ' one' : ''}`}>{f.length}/{ids.length}</span></td>
                  <td>{f.length > 1 ? formatP(t.combPvalue) : <span className="muted">{formatP(t.combPvalue)}</span>}</td>
                  {runs.map((r) => { const h = hit(t, r.id); return <td key={r.id} className="run-col">{h ? (cells === 'rank' ? <>#{agg.get(t.id)?.ranks[r.id]}<span className="muted small"> · {formatP(h.p)}</span></> : <>{formatP(h.p)}<span className="muted small"> · {h.genes}</span></>) : <span className="muted">—</span>}</td> })}
                  <td>{t.intersectionCount ?? 0}<span className="muted small"> / {t.unionCount ?? 0}</span></td>
                </tr>
              )
            })}
          </tbody>
        </table>
      </div>
      {rows.length > limit && <button className="btn btn-ghost btn-sm" onClick={() => setLimit((l) => l + 500)}>Show 500 more ({(rows.length - limit).toLocaleString()} left)</button>}
      <p className="muted small">Consensus rank: each run ranks its terms by p-value; Robust Rank Aggregation (Kolde et al. 2012) combines the ranks, so terms ranked high by several runs come first (a run that did not report a term counts it as last). Per-run columns: that run's p-value · genes it found in the term (— = not reported). Combined p: Fisher's method over the runs that found the term (grey when only one run did). Shared genes: found by every run that reported the term / by any of them.</p>
    </div>
  )
}
