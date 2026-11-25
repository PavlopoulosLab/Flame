import { useEffect, useMemo, useRef, useState } from 'react'
import { API_BASE, api } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { Plot } from '../../components/charts'
import { BusyBar, ErrorNote, Field, FileDrop, Note, Segmented } from '../../components/ui'
import { categorical, roleColors, usePaletteMode } from '../../lib/palette'
import { GenePicker, type GeneResult } from './FetchInput'

export interface ClinicalTable { text: string; fileName: string }
interface SurvGene { gene: string; z: number; hr: number; lo: number; hi: number; p: number; q: number }
interface SurvResult {
  genes: SurvGene[]; samples: number; events: number; unmatched: number; excluded: number; medianTime: number; strata: number
  tested: number; significant: number; timeColumn: string; eventColumn: string; strataColumn?: string | null; note: string
}
type Pick = 'poor' | 'good' | 'both' | 'all'

const fmt = (x: number) => (x < 0.001 ? x.toExponential(1) : x.toPrecision(2))

interface KmGroup { group: string; n: number; events: number; median: number | null; time: number[]; surv: number[]; lower: (number | null)[]; upper: (number | null)[]
  censorTime: number[]; censorSurv: number[]; atRisk: number[] }
interface KmResult { by: 'gene' | 'column'; gene?: string | null; column?: string | null; splitLabel?: string | null; cut?: number | null; samples: number; events: number
  groups: KmGroup[]; ticks: number[]; logrank: { chisq: number; df: number; p: number }; hr?: { hr: number; lo: number; hi: number; p: number; of: string; vs: string } | null; note?: string | null }
type Split = 'median' | 'tertile' | 'quartile' | 'percentile' | 'optimal'
export interface ExpressionInfo { fileName: string; genes: number; samples: number }

/** "p = 0.012" or "p < 0.0001". */
const pIs = (p: number | null | undefined) => (p == null || !Number.isFinite(p) ? 'p = —' : p < 0.0001 ? 'p < 0.0001' : `p = ${p < 0.01 ? p.toPrecision(2) : p.toFixed(3)}`)

/** One input table: its drop box (and paste box), or the loaded file with Replace. */
function TableBox({ step, title, what, loaded, onText, placeholder, maxBytes }: {
  step: number; title: string; what: string; loaded: string | null; onText: (t: string, n: string) => void; placeholder: string; maxBytes: number
}) {
  const [replacing, setReplacing] = useState(false)
  const show = !loaded || replacing
  return (
    <div className={`card stack-sm survival-box${loaded ? ' loaded' : ''}`}>
      <div><strong>{step}. {title}</strong> <span className="muted small">{what}</span></div>
      {loaded && !replacing && (
        <div className="row wrap small"><span className="survival-ok">✓</span> <span>{loaded}</span>
          <button type="button" className="btn btn-ghost btn-sm" onClick={() => setReplacing(true)}>Replace</button></div>
      )}
      {show && (
        <>
          <FileDrop accept=".txt,.csv,.tsv,.tab" maxBytes={maxBytes} onText={(t, n) => { onText(t, n); setReplacing(false) }} />
          <textarea className="input mono" rows={3} placeholder={placeholder} aria-label={`Paste the ${title.toLowerCase()}`}
            onChange={(e) => { if (e.target.value.trim()) { onText(e.target.value, ''); setReplacing(false) } }} />
          {replacing && <button type="button" className="btn btn-ghost btn-sm" onClick={() => setReplacing(false)}>Keep the current table</button>}
        </>
      )}
    </div>
  )
}

/**
 * Survival: an expression table (genes × samples) and a survival table (samples × follow-up time, event, other
 * clinical columns), each dropped in its own box. Kaplan–Meier curves of the samples split by a gene's expression
 * or grouped by a clinical column (log-rank test, median survival, numbers at risk, hazard ratio), and one Cox
 * model per gene (hazard ratio per standard deviation), optionally stratified by a clinical column such as stage.
 */
export function SurvivalPanel({ text, fileName, log, clinical, setClinical, onCreated, expression, setExpression, onExample, exampleLoading }: {
  text: string; fileName: string; log: boolean | null; clinical: ClinicalTable | null
  setClinical: (c: ClinicalTable | null) => void; onCreated: (l: AnalyteList) => void
  /** The expression table loaded (null: none yet). */ expression: ExpressionInfo | null
  setExpression: (text: string, fileName: string) => void
  /** Load both example tables. */ onExample: () => void; exampleLoading?: boolean
}) {
  const [cols, setCols] = useState<{ columns: string[]; samples: number; timeColumn: string | null; eventColumn: string | null } | null>(null)
  const [timeCol, setTimeCol] = useState('')
  const [eventCol, setEventCol] = useState('')
  const [strataCol, setStrataCol] = useState('')
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [res, setRes] = useState<SurvResult | null>(null)
  const [pick, setPick] = useState<Pick>('poor')
  const [maxQ, setMaxQ] = useState(0.05)
  const pm = usePaletteMode()
  // Kaplan–Meier
  const [kmBy, setKmBy] = useState<'gene' | 'column'>('gene')
  const [kmGene, setKmGene] = useState('')
  const [split, setSplit] = useState<Split>('median')
  const [percent, setPercent] = useState(25)
  const [kmCol, setKmCol] = useState('')
  const [showCI, setShowCI] = useState(false)
  const [km, setKm] = useState<{ r: KmResult; key: string } | null>(null)
  const [kmErr, setKmErr] = useState<unknown>(null)
  const [kmBusy, setKmBusy] = useState(false)
  const kmKey = JSON.stringify([text.length, text.slice(0, 200), clinical?.text.length, timeCol, eventCol, kmBy, kmBy === 'gene' ? [kmGene.trim(), split, split === 'percentile' ? percent : 0] : kmCol, log])
  const kmReady = !!text && !!clinical && !!timeCol && !!eventCol && (kmBy === 'gene' ? !!kmGene.trim() : !!kmCol)
  const seq = useRef(0)
  // The curves follow the choices (after a short pause while typing a gene).
  useEffect(() => {
    if (!kmReady || !clinical) return
    const id = ++seq.current, key = kmKey
    const tm = setTimeout(() => {
      setKmBusy(true); setKmErr(null)
      api<KmResult>('/inputs/expression/km', { body: { text, fileName, log, clinical: clinical.text, clinicalFileName: clinical.fileName, timeColumn: timeCol, eventColumn: eventCol,
        by: kmBy, gene: kmGene.trim(), split, percent, column: kmCol } })
        .then((r) => { if (id === seq.current) setKm({ r, key }) }).catch((e) => { if (id === seq.current) { setKmErr(e); setKm(null) } })
        .finally(() => { if (id === seq.current) setKmBusy(false) })
    }, 450)
    return () => clearTimeout(tm)
  }, [kmKey, kmReady]) // eslint-disable-line react-hooks/exhaustive-deps
  const kmRes = km && kmReady ? km.r : null

  useEffect(() => {
    setCols(null); setRes(null)
    if (!clinical?.text.trim()) return
    setErr(null)
    api<{ columns: string[]; samples: number; timeColumn: string | null; eventColumn: string | null }>('/inputs/expression/survival-columns',
      { body: { clinical: clinical.text, clinicalFileName: clinical.fileName } })
      .then((c) => { setCols(c); setTimeCol(c.timeColumn ?? ''); setEventCol(c.eventColumn ?? ''); setStrataCol('') })
      .catch(setErr)
  }, [clinical])
  useEffect(() => { setRes(null) }, [text, timeCol, eventCol, strataCol])

  const run = () => {
    if (!clinical) return
    setBusy(true); setErr(null)
    api<SurvResult>('/inputs/expression/survival', { body: { text, fileName, log, clinical: clinical.text, clinicalFileName: clinical.fileName, timeColumn: timeCol, eventColumn: eventCol, strataColumn: strataCol || null } })
      .then((r) => { setRes(r); if (!kmGene.trim() && r.genes[0]) { setKmBy('gene'); setKmGene(r.genes[0].gene) } }).catch(setErr).finally(() => setBusy(false))
  }

  const result: GeneResult | null = useMemo(() => {
    if (!res) return null
    const keep = pick === 'all' ? res.genes : res.genes.filter((g) => g.q <= maxQ && (pick === 'both' || (pick === 'poor' ? g.hr > 1 : g.hr < 1)))
    const sorted = pick === 'all' ? [...keep].sort((a, b) => b.z - a.z) : keep
    const what = { poor: 'poor-prognosis genes', good: 'good-prognosis genes', both: 'survival-linked genes', all: 'genes ranked by Cox z' }[pick]
    return {
      genes: sorted.map((g) => ({ gene: g.gene, value: g.z, note: `HR ${g.hr.toFixed(2)} (${g.lo.toFixed(2)}–${g.hi.toFixed(2)}) · q = ${fmt(g.q)}` })),
      organismTaxId: 9606, ranked: true, keepAll: pick === 'all', defaultTop: 500,
      valueLabel: 'Cox z-score (+ = shorter survival)', term: `Survival: ${what}`, source: 'survival analysis',
      citation: `${res.note} ${res.samples} samples, ${res.events} events${res.strataColumn ? `, stratified by ${res.strataColumn} (${res.strata} strata)` : ''}.`,
    }
  }, [res, pick, maxQ])

  const volcano = useMemo(() => {
    if (!res) return null
    const c = roleColors(pm)
    const pts = res.genes.filter((g) => g.hr > 0)
    const col = (g: SurvGene) => (g.q <= maxQ ? (g.hr > 1 ? c.up : c.down) : c.ns)
    return [{
      type: pts.length > 5000 ? 'scattergl' : 'scatter', mode: 'markers', x: pts.map((g) => Math.log2(g.hr)), y: pts.map((g) => -Math.log10(Math.max(g.p, 1e-300))),
      text: pts.map((g) => `${g.gene}<br>HR ${g.hr.toFixed(2)} (${g.lo.toFixed(2)}–${g.hi.toFixed(2)})<br>p ${fmt(g.p)} · q ${fmt(g.q)}`),
      hoverinfo: 'text', marker: { size: 5, color: pts.map(col), opacity: 0.8 }, customdata: pts.map((g) => g.gene),
    }]
  }, [res, pm, maxQ])

  const kmFig = useMemo(() => {
    if (!kmRes) return null
    const rc = roleColors(pm), cat = categorical(pm)
    const color = (g: string, i: number) => (kmRes.by === 'gene' ? (g === 'High' ? rc.up : rc.down) : cat[i % cat.length])
    const label = (g: KmGroup) => `${kmRes.by === 'gene' ? `${kmRes.gene} ${g.group.toLowerCase()}` : g.group} (n = ${g.n})`
    const data = kmRes.groups.flatMap((g, i) => {
      const c = color(g.group, i)
      // Start at time 0, survival 1; steps after each time (as a Kaplan–Meier curve is drawn).
      const x0 = g.time[0] > 0 ? [0, ...g.time] : g.time, pad = <T,>(a: T[], v: T) => (g.time[0] > 0 ? [v, ...a] : a)
      const out: Record<string, unknown>[] = []
      if (showCI) {
        out.push({ type: 'scatter', mode: 'lines', x: x0, y: pad(g.upper, 1), line: { shape: 'hv', width: 0 }, hoverinfo: 'skip', showlegend: false, legendgroup: g.group },
          { type: 'scatter', mode: 'lines', x: x0, y: pad(g.lower, 1), line: { shape: 'hv', width: 0 }, fill: 'tonexty', fillcolor: c + '26', hoverinfo: 'skip', showlegend: false, legendgroup: g.group })
      }
      out.push({ type: 'scatter', mode: 'lines', name: label(g), legendgroup: g.group, x: x0, y: pad(g.surv, 1), line: { shape: 'hv', color: c, width: 2.2 },
        hovertemplate: `${g.group}: %{y:.2f} at %{x}<extra></extra>` })
      if (g.censorTime.length) out.push({ type: 'scatter', mode: 'markers', name: `${g.group} censored`, legendgroup: g.group, showlegend: false, x: g.censorTime, y: g.censorSurv,
        marker: { symbol: 'line-ns-open', size: 8, color: c, line: { width: 1.5, color: c } }, hovertemplate: `${g.group}: censored at %{x}<extra></extra>` })
      return out
    })
    const stat = [`Log-rank ${pIs(kmRes.logrank.p)}`, kmRes.hr ? `HR (${kmRes.hr.of} vs ${kmRes.hr.vs}) = ${kmRes.hr.hr.toFixed(2)} (95% CI ${kmRes.hr.lo.toFixed(2)}–${kmRes.hr.hi.toFixed(2)})` : null].filter(Boolean).join('<br>')
    return { data, layout: {
      xaxis: { title: { text: `Time (${timeCol})` }, range: [0, Math.max(...kmRes.ticks, ...kmRes.groups.map((g) => g.time[g.time.length - 1] ?? 0)) * 1.02], tickvals: kmRes.ticks, zeroline: false, showline: true, showgrid: false, ticks: 'outside' },
      yaxis: { title: { text: 'Survival probability' }, range: [0, 1.04], zeroline: false, showline: true, showgrid: false, ticks: 'outside', tickformat: '.1f' },
      legend: { orientation: 'v', x: 0.99, xanchor: 'right', y: 0.99, yanchor: 'top', bgcolor: 'rgba(255,255,255,0.6)' },
      annotations: [{ xref: 'paper', yref: 'paper', x: 0.01, y: 0.02, xanchor: 'left', yanchor: 'bottom', align: 'left', showarrow: false, text: stat, font: { size: 12 } }],
      margin: { l: 60, r: 15, t: 10, b: 50 },
    }, color }
  }, [kmRes, pm, showCI, timeCol])

  return (
    <div className="stack-sm">
      <p className="muted small">Survival analysis needs two tables. Drop each in its own box: the <strong>expression table</strong> (genes in rows, samples in columns) and the <strong>survival table</strong> (samples in rows, with the same sample names as the expression columns, then a follow-up time column, e.g. months, an event column, 1/0, dead/alive or 1:DECEASED, and any other clinical columns such as stage).</p>
      <div className="survival-boxes">
        <TableBox step={1} title="Expression table" what="genes × samples" maxBytes={20 * 1024 * 1024} onText={setExpression}
          loaded={expression ? `${expression.fileName || 'pasted table'}: ${expression.genes.toLocaleString()} genes × ${expression.samples} samples` : text ? 'reading…' : null}
          placeholder={'gene\tT1\tT2\tT3\nCDK1\t5.2\t7.9\t6.1\nALB\t14.1\t12.3\t13.8'} />
        <TableBox step={2} title="Survival table" what="samples × time, event (and clinical columns)" maxBytes={2 * 1024 * 1024} onText={(t, n) => setClinical({ text: t, fileName: n })}
          loaded={clinical ? `${clinical.fileName || 'pasted table'}${cols ? `: ${cols.samples} samples, ${cols.columns.length} columns` : ''}` : null}
          placeholder={'sample\tOS_MONTHS\tOS_STATUS\tSTAGE\nT1\t23.8\t1:DECEASED\tIV\nT2\t53.4\t0:LIVING\tI'} />
      </div>
      <div className="row wrap small">
        <button className="btn btn-ghost btn-sm" onClick={onExample} disabled={exampleLoading}>{exampleLoading ? 'Loading…' : 'Load the example (365 liver tumours, TCGA)'}</button>
        <a className="btn btn-ghost btn-sm" href={`${API_BASE}/inputs/examples/survival/file`}>Example expression table</a>
        <a className="btn btn-ghost btn-sm" href={`${API_BASE}/inputs/examples/survival_clinical/file`}>Example survival table</a>
      </div>
      {cols && (
        <div className="row wrap">
          <Field label="Time"><select className="input" value={timeCol} onChange={(e) => setTimeCol(e.target.value)}>
            <option value="">choose…</option>{cols.columns.map((c) => <option key={c}>{c}</option>)}</select></Field>
          <Field label="Event"><select className="input" value={eventCol} onChange={(e) => setEventCol(e.target.value)}>
            <option value="">choose…</option>{cols.columns.map((c) => <option key={c}>{c}</option>)}</select></Field>
          <Field label="Stratify by (optional)" hint="e.g. stage or batch: compared only within each group">
            <select className="input" value={strataCol} onChange={(e) => setStrataCol(e.target.value)}>
              <option value="">none</option>{cols.columns.filter((c) => c !== timeCol && c !== eventCol).map((c) => <option key={c}>{c}</option>)}</select></Field>
        </div>
      )}
      {cols && !text && <Note kind="info">Now add the expression table (box 1): its sample columns are matched to the rows of the survival table.</Note>}
      {cols && text && (
        <div className="card stack-sm survival-km">
          <h3 className="small">Kaplan–Meier curves</h3>
          <div className="row wrap">
            <Segmented label="Group the samples by" value={kmBy} onChange={setKmBy} options={[{ value: 'gene', label: "A gene's expression" }, { value: 'column', label: 'A clinical column' }]} />
            {kmBy === 'gene' ? (<>
              <Field label="Gene"><input className="input input-sm" list="survival-genes" value={kmGene} placeholder="e.g. CDK1" aria-label="Gene for the Kaplan–Meier curves" onChange={(e) => setKmGene(e.target.value)} style={{ width: 140 }} />
                <datalist id="survival-genes">{(res?.genes ?? []).slice(0, 300).map((g) => <option key={g.gene} value={g.gene} />)}</datalist></Field>
              <Field label="High vs low">
                <select className="input input-sm" aria-label="Split" value={split} onChange={(e) => setSplit(e.target.value as Split)}>
                  <option value="median">At the median</option><option value="tertile">Top vs bottom third</option><option value="quartile">Top vs bottom quarter</option>
                  <option value="percentile">Top … % vs the rest</option><option value="optimal">Best cut-off (smallest p)</option>
                </select></Field>
              {split === 'percentile' && <Field label="Top %"><input className="input input-sm input-narrow" type="number" min={5} max={95} step={5} value={percent} onChange={(e) => setPercent(Math.min(95, Math.max(5, Number(e.target.value) || 25)))} /></Field>}
            </>) : (
              <Field label="Column"><select className="input input-sm" aria-label="Clinical column" value={kmCol} onChange={(e) => setKmCol(e.target.value)}>
                <option value="">choose…</option>{cols.columns.filter((c) => c !== timeCol && c !== eventCol).map((c) => <option key={c}>{c}</option>)}</select></Field>
            )}
            <label className="check small align-end"><input type="checkbox" checked={showCI} onChange={(e) => setShowCI(e.target.checked)} /> 95% confidence bands</label>
          </div>
          {!kmReady && <p className="muted small">{kmBy === 'gene' ? 'Type a gene, or run the Cox analysis below and click a gene in its plot.' : 'Choose a clinical column, e.g. stage.'}</p>}
          {kmBusy && <BusyBar label="Drawing the curves" expect={2} />}
          <ErrorNote error={kmErr} />
          {kmRes && kmFig && (
            <>
              <Plot tools={false} height={400} data={kmFig.data as never} layout={kmFig.layout as never} exportName={`flame_km_${kmRes.gene ?? kmRes.column ?? 'groups'}`} />
              <div className="table-scroll"><table className="table compact km-risk">
                <thead><tr><th>Number at risk</th>{kmRes.ticks.map((t) => <th key={t} className="num">{t}</th>)}</tr></thead>
                <tbody>{kmRes.groups.map((g, i) => <tr key={g.group}><td><span className="km-swatch" style={{ background: kmFig.color(g.group, i) }} />{g.group}</td>{g.atRisk.map((n, j) => <td key={j} className="num">{n}</td>)}</tr>)}</tbody>
              </table></div>
              <div className="table-scroll"><table className="table compact">
                <thead><tr><th>Group</th><th className="num">Samples</th><th className="num">Events</th><th className="num">Median survival</th></tr></thead>
                <tbody>{kmRes.groups.map((g) => <tr key={g.group}><td>{g.group}</td><td className="num">{g.n}</td><td className="num">{g.events}</td><td className="num">{g.median == null ? 'not reached' : g.median}</td></tr>)}</tbody>
              </table></div>
              <p className="small"><strong>Log-rank test:</strong> χ² = {kmRes.logrank.chisq?.toFixed(2)}, df = {kmRes.logrank.df}, {pIs(kmRes.logrank.p)}
                {kmRes.hr && <>; <strong>hazard ratio</strong> {kmRes.hr.of} vs {kmRes.hr.vs} = {kmRes.hr.hr.toFixed(2)} (95% CI {kmRes.hr.lo.toFixed(2)}–{kmRes.hr.hi.toFixed(2)}), Cox {pIs(kmRes.hr.p)}</>}.
                {' '}{kmRes.samples} samples, {kmRes.events} events{kmRes.by === 'gene' ? `; ${kmRes.gene} split ${kmRes.splitLabel === 'a percentile' ? `top ${percent}% vs the rest` : `at ${kmRes.splitLabel}`}${kmRes.cut != null ? ` (cut-off ${kmRes.cut})` : ''}` : ''}. Ticks: censored samples.</p>
              {kmRes.note && <Note kind="warning">{kmRes.note}</Note>}
            </>
          )}
        </div>
      )}
      {cols && text && (
        <div className="row wrap">
          <h3 className="small">Genes linked to survival</h3>
          <button className="btn btn-primary" disabled={!timeCol || !eventCol || busy} onClick={run}>Run the Cox analysis of every gene</button>
        </div>
      )}
      {cols && (/os|overall/i.test(timeCol) || /os|overall/i.test(eventCol)) && cols.columns.some((c) => /^PF[IS]/i.test(c)) && (
        <p className="muted small">Tip: for some TCGA cancer types (e.g. breast, prostate, thyroid, brain lower-grade glioma) progression-free interval is the more reliable endpoint (Liu et al. 2018, Cell 173:400).</p>
      )}
      {busy && <BusyBar label="Fitting a Cox model for every gene" expect={8} />}
      <ErrorNote error={err} />
      {res && (
        <>
          <p className="muted small">Click a gene in the plot to see its Kaplan–Meier curves above.</p>
          <p className="small"><strong>{res.samples}</strong> samples with <strong>{res.events}</strong> events (median follow-up {res.medianTime.toFixed(1)}){res.excluded ? `, ${res.excluded} left out (no usable time or event${res.strataColumn ? ' or stratum' : ''})` : ''}{res.unmatched ? `, ${res.unmatched} expression columns not in the clinical table` : ''}. <strong>{res.significant.toLocaleString()}</strong> of {res.tested.toLocaleString()} genes with q ≤ 0.05{res.strataColumn ? `; stratified by ${res.strataColumn} (${res.strata} strata)` : ''}.</p>
          {volcano && (
            <Plot tools={false} height={320} data={volcano as never} onPointClick={(g) => { setKmBy('gene'); setKmGene(g) }}
              layout={{ xaxis: { title: { text: 'log2 hazard ratio per SD (→ shorter survival)' }, zeroline: true }, yaxis: { title: { text: '−log10 p' } }, margin: { l: 55, r: 10, t: 10, b: 45 } }} />
          )}
          <div className="row wrap">
            <Segmented label="Genes to keep" value={pick} onChange={setPick} options={[
              { value: 'poor', label: 'Poor prognosis (HR > 1)' }, { value: 'good', label: 'Good prognosis (HR < 1)' },
              { value: 'both', label: 'Both' }, { value: 'all', label: 'All, ranked (GSEA)' }]} />
            {pick !== 'all' && <Field label="q-value at most"><input className="input input-narrow" type="number" min={0} max={1} step={0.01} value={maxQ} onChange={(e) => setMaxQ(Number(e.target.value))} /></Field>}
          </div>
          <p className="muted small">In many cancers the strongest prognostic genes are proliferation genes, so cell cycle terms often dominate an enrichment of poor-prognosis genes. Stratifying by stage, or looking at the good-prognosis genes, shows what else is there.</p>
          {result && result.genes.length > 0 && <GenePicker r={result} mode="survival" onCreated={onCreated} />}
          {result && result.genes.length === 0 && <Note kind="info">No gene passes these cut-offs.</Note>}
        </>
      )}
    </div>
  )
}
