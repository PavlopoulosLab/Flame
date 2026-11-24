import { useEffect, useMemo, useState } from 'react'
import { API_BASE, api } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { Plot } from '../../components/charts'
import { BusyBar, ErrorNote, Field, FileDrop, Note, Segmented } from '../../components/ui'
import { roleColors, usePaletteMode } from '../../lib/palette'
import { GenePicker, type GeneResult } from './FetchInput'

export interface ClinicalTable { text: string; fileName: string }
interface SurvGene { gene: string; z: number; hr: number; lo: number; hi: number; p: number; q: number }
interface SurvResult {
  genes: SurvGene[]; samples: number; events: number; unmatched: number; excluded: number; medianTime: number; strata: number
  tested: number; significant: number; timeColumn: string; eventColumn: string; strataColumn?: string | null; note: string
}
type Pick = 'poor' | 'good' | 'both' | 'all'

const fmt = (x: number) => (x < 0.001 ? x.toExponential(1) : x.toPrecision(2))

/**
 * Genes linked to survival: a clinical table (sample, follow-up time, event) next to the
 * expression matrix; one Cox model per gene (hazard ratio per standard deviation),
 * optionally stratified by a clinical column such as stage.
 */
export function SurvivalPanel({ text, fileName, log, clinical, setClinical, onCreated }: {
  text: string; fileName: string; log: boolean | null; clinical: ClinicalTable | null
  setClinical: (c: ClinicalTable | null) => void; onCreated: (l: AnalyteList) => void
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
      .then(setRes).catch(setErr).finally(() => setBusy(false))
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
      hoverinfo: 'text', marker: { size: 5, color: pts.map(col), opacity: 0.8 },
    }]
  }, [res, pm, maxQ])

  const loadExample = () => {
    api<{ clinical: string; clinicalFileName: string }>('/inputs/examples/survival')
      .then((r) => setClinical({ text: r.clinical, fileName: r.clinicalFileName })).catch(setErr)
  }

  return (
    <div className="stack-sm">
      <p className="muted small">Which genes are linked to outcome? Give the follow-up of each sample in a <strong>clinical table</strong>: first column the sample names (as in the expression table), then a time column (e.g. months) and an event column (1/0, dead/alive, 1:DECEASED…). Each gene gets a Cox proportional-hazards model on its standardized expression: a hazard ratio per standard deviation, above 1 when higher expression goes with shorter survival.</p>
      {!clinical ? (
        <div className="row wrap">
          <FileDrop accept=".txt,.csv,.tsv,.tab" maxBytes={1024 * 1024} onText={(t, n) => setClinical({ text: t, fileName: n })} />
          <button className="btn btn-ghost btn-sm" onClick={loadExample}>Example clinical table</button>
          <a className="btn btn-ghost btn-sm" href={`${API_BASE}/inputs/examples/survival_clinical/file`}>Download example</a>
        </div>
      ) : (
        <div className="row wrap small">
          <span><span className="muted">Clinical table:</span> <strong>{clinical.fileName || 'pasted'}</strong>{cols ? ` · ${cols.samples} samples` : ''}</span>
          <button className="btn btn-ghost btn-sm" onClick={() => setClinical(null)}>Replace</button>
        </div>
      )}
      {!clinical && <textarea className="input mono" rows={3} placeholder={'sample\tOS_MONTHS\tOS_STATUS\tSTAGE\nT1\t23.8\t1:DECEASED\tIV\nT2\t53.4\t0:LIVING\tI'}
        onChange={(e) => e.target.value.trim() && setClinical({ text: e.target.value, fileName: '' })} />}
      {cols && (
        <div className="row wrap">
          <Field label="Time"><select className="input" value={timeCol} onChange={(e) => setTimeCol(e.target.value)}>
            <option value="">choose…</option>{cols.columns.map((c) => <option key={c}>{c}</option>)}</select></Field>
          <Field label="Event"><select className="input" value={eventCol} onChange={(e) => setEventCol(e.target.value)}>
            <option value="">choose…</option>{cols.columns.map((c) => <option key={c}>{c}</option>)}</select></Field>
          <Field label="Stratify by (optional)" hint="e.g. stage or batch: compared only within each group">
            <select className="input" value={strataCol} onChange={(e) => setStrataCol(e.target.value)}>
              <option value="">none</option>{cols.columns.filter((c) => c !== timeCol && c !== eventCol).map((c) => <option key={c}>{c}</option>)}</select></Field>
          <button className="btn btn-primary" disabled={!timeCol || !eventCol || busy} onClick={run}>Run survival analysis</button>
        </div>
      )}
      {cols && (/os|overall/i.test(timeCol) || /os|overall/i.test(eventCol)) && cols.columns.some((c) => /^PF[IS]/i.test(c)) && (
        <p className="muted small">Tip: for some TCGA cancer types (e.g. breast, prostate, thyroid, brain lower-grade glioma) progression-free interval is the more reliable endpoint (Liu et al. 2018, Cell 173:400).</p>
      )}
      {busy && <BusyBar label="Fitting a Cox model for every gene" expect={8} />}
      <ErrorNote error={err} />
      {res && (
        <>
          <p className="small"><strong>{res.samples}</strong> samples with <strong>{res.events}</strong> events (median follow-up {res.medianTime.toFixed(1)}){res.excluded ? `, ${res.excluded} left out (no usable time or event${res.strataColumn ? ' or stratum' : ''})` : ''}{res.unmatched ? `, ${res.unmatched} expression columns not in the clinical table` : ''}. <strong>{res.significant.toLocaleString()}</strong> of {res.tested.toLocaleString()} genes with q ≤ 0.05{res.strataColumn ? `; stratified by ${res.strataColumn} (${res.strata} strata)` : ''}.</p>
          {volcano && (
            <Plot tools={false} height={320} data={volcano as never}
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
