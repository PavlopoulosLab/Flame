import { useEffect, useMemo, useRef, useState } from 'react'
import { api } from '../../api/client'
import { Plot } from '../../components/charts'
import { BusyBar, ErrorNote, Field, Note } from '../../components/ui'
import type { TestId } from './benchTests'

interface GroupNorm {
  group: string; n: number; shapiro: number | null; dagostino: number | null; anderson: number | null; ks: number | null
  lognormal: number | null; passed: boolean | null; qq: { theoretical: number[]; sample: number[] } | null
}
interface NormResult {
  groups: string[]; log: boolean; warnings: string[]
  rows: { target: string; groups: GroupNorm[]; equalSD?: { name: string; p: number } | null }[]
  recommendation: { test: TestId; log: boolean; label: string; why: string[] }
}
export interface Recommendation { test: TestId; log: boolean }

const fmt = (p: number | null | undefined) => (p == null || !Number.isFinite(p) ? '—' : p < 0.0001 ? '< 0.0001' : p < 0.01 ? p.toPrecision(2) : p.toFixed(3))
const pass = (p: number | null | undefined) => (p == null || !Number.isFinite(p) ? '' : p >= 0.05 ? ' ✓' : ' ✗')

/**
 * "Which test?": GraphPad Prism's normality tests for every group (and row), the equal-SD check, QQ plots,
 * and the test they point to, which the user can then run in the experimental analysis.
 */
export function BenchNormality({ text, fileName, groupsBody, colors, auto, onUse }: {
  text: string; fileName: string; groupsBody: string | null; colors: (g: string, i: number) => string; auto: boolean
  onUse: (r: Recommendation) => void
}) {
  const [log, setLog] = useState(false)
  const [matched, setMatched] = useState(false)
  const [result, setResult] = useState<{ r: NormResult; key: string; data: string } | null>(null)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [row, setRow] = useState(0)
  const data = JSON.stringify([text, groupsBody])
  const key = JSON.stringify([data, log, matched])
  const run = () => {
    const k = key, dt = data
    setBusy(true); setErr(null)
    api<NormResult>('/inputs/bench/normality', { body: { text, fileName, log, matched, ...(groupsBody ? { groups: JSON.parse(groupsBody) } : {}) } })
      .then((r) => { setResult({ r, key: k, data: dt }); setRow(0) }).catch((e) => { setErr(e); setResult(null) }).finally(() => setBusy(false))
  }
  // Examples are tested straight away (once).
  const autoRan = useRef(false)
  useEffect(() => { if (auto && !autoRan.current) { autoRan.current = true; void Promise.resolve().then(run) } }, [auto]) // eslint-disable-line react-hooks/exhaustive-deps
  // Results belong to the table and groups they were run on; other options only mark them out of date.
  const res = result && result.data === data ? result.r : null
  const stale = !!result && !!res && result.key !== key
  const r = res?.rows[row]
  const qq = useMemo(() => {
    if (!r) return null
    const data = r.groups.flatMap((g, i) => {
      if (!g.qq) return []
      const { theoretical: x, sample: y } = g.qq
      const n = y.length, m = y.reduce((a, b) => a + b, 0) / n, sd = Math.sqrt(y.reduce((a, b) => a + (b - m) ** 2, 0) / (n - 1))
      const c = colors(g.group, i)
      return [
        { type: 'scatter', mode: 'markers', name: g.group, x, y, marker: { color: c, size: 8 }, hovertemplate: `${g.group}: %{y:.3g}<extra></extra>` },
        { type: 'scatter', mode: 'lines', name: `${g.group} (Gaussian)`, showlegend: false, x: [x[0], x[n - 1]], y: [m + sd * x[0], m + sd * x[n - 1]], line: { color: c, dash: 'dot', width: 1.5 }, hoverinfo: 'skip' },
      ]
    })
    return { data, layout: { xaxis: { title: { text: 'Expected (Gaussian quantile)' }, zeroline: false }, yaxis: { title: { text: res?.log ? 'Actual (log2)' : 'Actual' }, zeroline: false }, legend: { orientation: 'h', y: -0.2 } } }
  }, [r, res, colors])

  return (
    <div className="card stack-sm bench-normality">
      <h3 className="small">Normality: which test should I use?</h3>
      <div className="row wrap">
        <label className="check small"><input type="checkbox" checked={log} onChange={(e) => setLog(e.target.checked)} /> Test the log of the values (fold changes, ratios, concentrations)</label>
        <label className="check small" title="The same experiment, donor, blot or plate in every group"><input type="checkbox" checked={matched} onChange={(e) => setMatched(e.target.checked)} /> The samples are matched (the same experiment or donor in every group)</label>
        <button className="btn btn-primary btn-sm" disabled={busy} onClick={run}>{res ? 'Run again' : 'Run normality tests'}</button>
      </div>
      <p className="muted small">GraphPad Prism's normality tests for each group: D'Agostino–Pearson (n ≥ 8, the one Prism suggests), Anderson–Darling (n ≥ 8), Shapiro–Wilk and Kolmogorov–Smirnov. p ≥ 0.05 (✓): no evidence against a Gaussian distribution. Plus the equal-SD check and, for positive values, how likely a lognormal distribution is.</p>
      {busy && <BusyBar label="Testing normality" expect={2} />}
      <ErrorNote error={err} />
      {stale && <Note kind="info">The options changed: press <strong>Run again</strong>.</Note>}
      {res && (
        <>
          <div className="note note-success stack-sm" role="status">
            <div><strong>Suggested test: {res.recommendation.label}{res.recommendation.log ? ', on a log2 scale' : ''}.</strong></div>
            {res.recommendation.why.map((w) => <div key={w} className="small">{w}</div>)}
            <div className="row"><button className="btn btn-primary btn-sm" onClick={() => onUse({ test: res.recommendation.test, log: res.recommendation.log || res.log })}>Analyse with this test</button>
              <span className="small muted">Opens the experimental analysis with this test; you can still pick another.</span></div>
          </div>
          {res.warnings.map((w) => <Note key={w} kind="warning">{w}</Note>)}
          <div className="table-scroll"><table className="table compact">
            <thead><tr><th>Gene / readout</th><th>Group</th><th className="num">n</th><th className="num">D'Agostino–Pearson</th><th className="num">Anderson–Darling</th>
              <th className="num">Shapiro–Wilk</th><th className="num">Kolmogorov–Smirnov</th>{!res.log && <th className="num">P(lognormal)</th>}<th>Gaussian?</th><th className="num">Equal SDs p</th></tr></thead>
            <tbody>{res.rows.flatMap((rw) => rw.groups.map((g, i) => (
              <tr key={`${rw.target}-${g.group}`}>
                <td>{i === 0 ? <strong>{rw.target}</strong> : ''}</td><td>{g.group}</td><td className="num">{g.n}</td>
                <td className="num">{fmt(g.dagostino)}{pass(g.dagostino)}</td><td className="num">{fmt(g.anderson)}{pass(g.anderson)}</td>
                <td className="num">{fmt(g.shapiro)}{pass(g.shapiro)}</td><td className="num">{fmt(g.ks)}{pass(g.ks)}</td>
                {!res.log && <td className="num">{g.lognormal == null ? '—' : `${Math.round(g.lognormal * 100)}%`}</td>}
                <td>{g.passed == null ? 'n too small' : g.passed ? 'Yes' : 'No'}</td>
                <td className="num">{i === 0 && rw.equalSD ? `${fmt(rw.equalSD.p)}${pass(rw.equalSD.p)} (${rw.equalSD.name})` : ''}</td>
              </tr>
            )))}</tbody>
          </table></div>
          {qq && (
            <div className="stack-sm">
              <div className="row wrap">
                <strong className="small">QQ plot</strong>
                {res.rows.length > 1 && <Field label="Row"><select className="input input-sm" aria-label="QQ plot row" value={row} onChange={(e) => setRow(Number(e.target.value))}>
                  {res.rows.map((rw, i) => <option key={rw.target} value={i}>{rw.target}</option>)}</select></Field>}
                <span className="muted small">Points close to their dotted line look Gaussian; a curve or a lone far point does not.</span>
              </div>
              <Plot data={qq.data as never} layout={qq.layout as never} height={340} tools={false} exportName="flame_qq_plot" />
            </div>
          )}
        </>
      )}
    </div>
  )
}
