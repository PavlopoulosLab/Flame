import { useEffect, useMemo, useState } from 'react'
import { api } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { Plot } from '../../components/charts'
import { BusyBar, CopyButton, ErrorNote, Field, FileDrop, Modal, Note } from '../../components/ui'
import { categorical } from '../../lib/palette'
import { saveXlsx } from '../../lib/xlsx'
import { SaveList } from './InputModal'
import { autoBreak, nice } from '../../lib/axisBreak'

type Helper = 'none' | 'qpcr' | 'western' | 'facs'
interface Comparison { a: string; b: string; diff: number; lo: number | null; hi: number | null; p: number; stars: string; fold?: number }
interface Target {
  target: string; error?: string
  values: { group: string; sample: string; pair: number; value: number }[]
  summary: { group: string; n: number; mean: number; sd: number | null; sem: number | null }[]
  omnibus?: { name: string; p: number; pAcross?: number }
  posthoc?: string | null
  comparisons?: Comparison[]
}
interface Result {
  layout: 'column' | 'grouped'; helper: Helper; groups: string[]; control: string; paired: boolean; logScale: boolean; shownAs: string
  targets: Target[]; sentence: string; methods: string; warnings: string[]; rowNames: string[]
}

const EXAMPLES: { kind: string; label: string; helper: Helper; refs?: string[]; loading?: string; paired?: boolean }[] = [
  { kind: 'bench_qpcr', label: 'qPCR (Ct)', helper: 'qpcr', refs: ['GAPDH', 'ACTB'], paired: true },
  { kind: 'bench_western', label: 'Western bands', helper: 'western', loading: 'ACTB' },
  { kind: 'bench_facs', label: 'FACS cell cycle', helper: 'facs' },
  { kind: 'bench_column', label: 'Simple table', helper: 'none' },
]
const REF_GENES = ['GAPDH', 'ACTB', 'B2M', 'HPRT1', 'RPLP0', 'TBP', '18S', 'RN18S1', 'PPIA', 'YWHAZ', 'UBC', 'GUSB', 'TUBB', 'Gapdh', 'Actb']
const LOADING = /^(ACTB|GAPDH|TUBB|TUBA1A|VCL|vinculin|tubulin|actin|β-actin|beta-actin|total protein|HSP90|LMNB1|lamin)/i
const PHASES = /^(G0\/G1|G1|S|G2\/M|G2|M|sub-?G1)$/i
const fmtP = (p: number | null | undefined) => (p == null || !Number.isFinite(p) ? '' : p < 0.0001 ? p.toExponential(1) : p < 0.01 ? p.toPrecision(2) : p.toFixed(3))
const num = (x: number | null | undefined, d = 2) => (x == null || !Number.isFinite(x) ? '' : x.toFixed(d))

/**
 * Bench data: paste a small table (qPCR Ct, band intensities, FACS percentages or any measurements
 * per group), get a sensible test and a clean figure, and send the genes to Flame. A quick, generic
 * tool, not a replacement for Prism, qbase+ or FlowJo.
 */
export function BenchInput({ title, example, onClose, onCreated }: { title: string; example?: boolean; onClose: () => void; onCreated: (l: AnalyteList) => void }) {
  const [text, setText] = useState('')
  const [fileName, setFileName] = useState('')
  const [source, setSource] = useState<string | null>(null)
  const [helper, setHelper] = useState<Helper>('none')
  const [refs, setRefs] = useState<string[]>([])
  const [loading, setLoading] = useState('')
  const [log, setLog] = useState(false)
  const [control, setControl] = useState('')
  const [paired, setPaired] = useState(false)
  const [nonpar, setNonpar] = useState(false)
  const [allPairs, setAllPairs] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [busy, setBusy] = useState(false)
  const [res, setRes] = useState<Result | null>(null)
  const [errBar, setErrBar] = useState<'sd' | 'sem'>('sd')
  const [figStyle, setFigStyle] = useState<FigStyle>('color')
  const [brk, setBrk] = useState<BreakMode>('off')
  const [brkFrom, setBrkFrom] = useState('')
  const [brkTo, setBrkTo] = useState('')
  const [organism, setOrganism] = useState(9606)

  const loadExample = async (ex: (typeof EXAMPLES)[number]) => {
    try {
      const r = await api<{ text: string; fileName: string; source: string }>(`/inputs/examples/${ex.kind}`)
      setRes(null); setControl(''); setHelper(ex.helper); setRefs(ex.refs ?? []); setLoading(ex.loading ?? ''); setPaired(!!ex.paired)
      setNonpar(false); setAllPairs(false); setLog(false); setFileName(r.fileName); setSource(r.source); setText(r.text)
    } catch (e) { setErr(e) }
  }
  useEffect(() => { if (example) loadExample(EXAMPLES[0]) // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  // Guess the helper from the row names the first time a table is analysed.
  const guess = (rows: string[]) => {
    if (rows.some((r) => PHASES.test(r)) && rows.length <= 5) return { helper: 'facs' as Helper }
    const refRows = rows.filter((r) => REF_GENES.includes(r))
    const lc = rows.find((r) => LOADING.test(r))
    return refRows.length ? { helper: 'qpcr' as Helper, refs: refRows } : lc ? { helper: 'western' as Helper, loading: lc } : null
  }

  useEffect(() => {
    if (!text.trim()) { setRes(null); return }
    if (helper === 'qpcr' && !refs.length) return
    if (helper === 'western' && !loading) return
    const t = setTimeout(() => {
      setBusy(true); setErr(null)
      api<Result>('/inputs/bench/analyse', { body: { text, fileName, helper, refs, loading, control: control || null, paired, nonparametric: nonpar, allPairs, log } })
        .then((r) => {
          setRes(r)
          if (!control) setControl(r.control)
          if (helper === 'none' && !source && r.layout === 'grouped') {
            const g = guess(r.rowNames)
            if (g) { setHelper(g.helper); if (g.refs) setRefs(g.refs); if (g.loading) setLoading(g.loading) }
          }
        })
        .catch((e) => { setErr(e); setRes(null) })
        .finally(() => setBusy(false))
    }, 250)
    return () => clearTimeout(t)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [text, fileName, helper, refs, loading, control, paired, nonpar, allPairs, log])

  const colors = categorical('flame')
  const figure = useMemo(() => (res ? benchFigure(res, { errBar, style: figStyle, brk, from: brkFrom ? Number(brkFrom) : undefined, to: brkTo ? Number(brkTo) : undefined }, colors) : null),
    [res, errBar, figStyle, brk, brkFrom, brkTo, colors])
  const tested = res?.targets.filter((t) => t.comparisons?.length) ?? []
  const genesLike = !!res && res.layout === 'grouped' && res.helper !== 'facs'

  return (
    <Modal title={title} size="xl" onClose={onClose}
      subtitle="Paste a small table (qPCR Ct, band intensities, FACS percentages or any values per group): Flame picks a test and draws the figure. For a full analysis use Prism, qbase+ or FlowJo.">
      <div className="stack">
        <div className="row wrap">
          <FileDrop accept=".txt,.csv,.tsv,.tab" onText={(t, n) => { setSource(null); setRes(null); setControl(''); setHelper('none'); setText(t); setFileName(n) }} />
          <span className="muted small">Examples:</span>
          {EXAMPLES.map((ex) => <button key={ex.kind} className="btn btn-ghost btn-sm" onClick={() => loadExample(ex)}>{ex.label}</button>)}
        </div>
        <textarea className="input mono" rows={5} value={text} aria-label="Table"
          placeholder={'Column table — one column per group:\nControl\tDrug A\tDrug B\n100\t71.4\t95.2\n96.8\t65.9\t101.3\n\nGrouped table — genes in rows, replicates under each group:\nGene\tControl 1\tControl 2\tDex 1\tDex 2\nGAPDH\t18.1\t18.3\t18.2\t18.0\nFKBP5\t27.1\t27.4\t23.6\t23.4'}
          onChange={(e) => { setSource(null); setText(e.target.value); setFileName('') }} />
        {source && <p className="example-source"><strong>Example data:</strong> {source}</p>}
        {res && (
          <div className="row wrap bench-options">
            <Field label="Helper">
              <select className="input input-sm" value={helper} onChange={(e) => setHelper(e.target.value as Helper)}>
                <option value="none">None: values as given</option>
                <option value="qpcr" disabled={res.layout !== 'grouped'}>qPCR: Ct → ΔΔCt, fold change</option>
                <option value="western" disabled={res.layout !== 'grouped'}>Western: band ÷ loading control</option>
                <option value="facs">FACS: phase percentages</option>
              </select>
            </Field>
            {helper === 'qpcr' && (
              <Field label="Reference gene(s)">
                <div className="row wrap">{res.rowNames.map((r) => (
                  <label key={r} className="check small"><input type="checkbox" checked={refs.includes(r)}
                    onChange={(e) => setRefs((cur) => (e.target.checked ? [...cur, r] : cur.filter((x) => x !== r)))} /> {r}</label>
                ))}</div>
              </Field>
            )}
            {helper === 'western' && (
              <Field label="Loading control">
                <select className="input input-sm" value={loading} onChange={(e) => setLoading(e.target.value)}>
                  <option value="">choose…</option>{res.rowNames.map((r) => <option key={r}>{r}</option>)}
                </select>
              </Field>
            )}
            {helper === 'none' && <label className="check small"><input type="checkbox" checked={log} onChange={(e) => setLog(e.target.checked)} /> Ratios: test on log2 scale</label>}
            <Field label="Control group">
              <select className="input input-sm" value={control} onChange={(e) => setControl(e.target.value)}>
                {res.groups.map((g) => <option key={g}>{g}</option>)}
              </select>
            </Field>
            <label className="check small" title="Each replicate (row, or replicate number) is the same experiment, blot or donor in every group"><input type="checkbox" checked={paired} onChange={(e) => setPaired(e.target.checked)} /> Rows are paired</label>
            <label className="check small"><input type="checkbox" checked={nonpar} onChange={(e) => setNonpar(e.target.checked)} /> Non-parametric</label>
            {res.groups.length > 2 && <label className="check small"><input type="checkbox" checked={allPairs} onChange={(e) => setAllPairs(e.target.checked)} /> All pairs (not only vs control)</label>}
          </div>
        )}
        {helper === 'qpcr' && res && !refs.length && <Note kind="info">Tick the reference gene(s) to compute ΔΔCt.</Note>}
        {helper === 'western' && res && !loading && <Note kind="info">Choose the loading control row.</Note>}
        {busy && <BusyBar label="Testing" expect={2} />}
        <ErrorNote error={err} />
        {res && (
          <>
            <p className="bench-sentence"><strong>Test:</strong> {res.sentence}</p>
            {res.warnings.map((w) => <Note key={w} kind="warning">{w}</Note>)}
            {figure && (
              <div className="stack-sm">
                <div className="row wrap small">
                  {res.helper !== 'facs' && <><span className="muted">Error bars</span>
                    <select className="input input-sm" value={errBar} onChange={(e) => setErrBar(e.target.value as 'sd' | 'sem')} aria-label="Error bars">
                      <option value="sd">mean ± SD</option><option value="sem">mean ± SEM</option></select></>}
                  <span className="muted">Style</span>
                  <select className="input input-sm" value={figStyle} onChange={(e) => setFigStyle(e.target.value as FigStyle)} aria-label="Figure style">
                    <option value="color">Colour</option><option value="bw">Black &amp; white</option></select>
                  {res.helper !== 'facs' && <><span className="muted">Axis break</span>
                    <select className="input input-sm" value={brk} onChange={(e) => setBrk(e.target.value as BreakMode)} aria-label="Axis break">
                      <option value="off">None</option><option value="auto">Automatic</option><option value="custom">From … to …</option></select>
                    {brk === 'custom' && <>
                      <input className="input input-sm input-narrow" type="number" step="any" min={0} value={brkFrom} placeholder="from" aria-label="Break from" onChange={(e) => setBrkFrom(e.target.value)} />
                      <input className="input input-sm input-narrow" type="number" step="any" min={0} value={brkTo} placeholder="to" aria-label="Break to" onChange={(e) => setBrkTo(e.target.value)} />
                    </>}
                    {brk !== 'off' && !figure.broken && <span className="muted">{brk === 'auto' ? 'no break needed: the bars are of similar height' : 'set from < to, both below the tallest bar'}</span>}
                    {figure.broken && <span className="muted">axis broken from {figure.broken[0]} to {figure.broken[1]}</span>}
                  </>}
                </div>
                <div className="row wrap small">
                  <span className="muted">Dots = replicates; stars vs {res.control}: * p &lt; 0.05, ** &lt; 0.01, *** &lt; 0.001.</span>
                </div>
                <Plot data={figure.data as never} layout={figure.layout as never} height={figure.height} tools={false} exportName={`flame_bench_${res.helper}`} />
              </div>
            )}
            <div className="table-scroll fetch-table"><table className="table compact">
              <thead><tr><th>{res.layout === 'grouped' ? (res.helper === 'facs' ? 'Phase' : 'Gene / target') : 'Target'}</th><th>Comparison</th>
                <th className="num">{res.logScale ? 'Fold change' : 'Difference'}</th>{res.logScale && <th className="num">log2 FC</th>}<th className="num">95% CI</th>
                <th className="num">p</th>{tested.length > 1 && <th className="num">p (Holm, all targets)</th>}<th /></tr></thead>
              <tbody>{tested.flatMap((t) => t.comparisons!.map((c, i) => (
                <tr key={`${t.target}-${c.a}-${c.b}`}>
                  <td>{i === 0 ? <strong>{t.target}</strong> : ''}</td><td className="small">{c.a} vs {c.b}</td>
                  <td className="num">{res.logScale ? num(c.fold) : num(c.diff)}</td>{res.logScale && <td className="num">{num(c.diff)}</td>}
                  <td className="num small">{c.lo != null && Number.isFinite(c.lo) ? (res.logScale ? `${num(2 ** c.lo)} to ${num(2 ** c.hi!)}` : `${num(c.lo)} to ${num(c.hi)}`) : '—'}</td>
                  <td className="num">{fmtP(c.p)}</td>{tested.length > 1 && <td className="num">{i === 0 ? fmtP(t.omnibus?.pAcross) : ''}</td>}<td><strong>{c.stars}</strong></td>
                </tr>
              )))}</tbody>
            </table></div>
            {res.targets.filter((t) => t.error).map((t) => <Note key={t.target} kind="info">{t.target}: {t.error}</Note>)}
            <div className="row wrap">
              <span className="small"><strong>Methods:</strong> {res.methods}</span>
              <CopyButton label="Copy methods" text={res.methods} />
              <button className="btn btn-ghost btn-sm" onClick={() => saveXlsx([{ name: 'Results', rows: tested.flatMap((t) => t.comparisons!.map((c) => ({
                Target: t.target, Comparison: `${c.a} vs ${c.b}`, ...(res.logScale ? { 'Fold change': c.fold, log2FC: c.diff } : { Difference: c.diff }),
                'CI low': c.lo, 'CI high': c.hi, p: c.p, 'p (Holm, all targets)': t.omnibus?.pAcross ?? '', Test: `${t.omnibus?.name ?? ''}${t.posthoc ? `; ${t.posthoc}` : ''}` }))) },
                { name: 'Values', rows: res.targets.flatMap((t) => t.values.map((v) => ({ Target: t.target, Group: v.group, Sample: v.sample, [res.shownAs]: v.value }))) }], 'flame_bench_results.xlsx')}>Excel</button>
            </div>
            {genesLike && tested.length > 0 && <BenchGenes res={res} tested={tested} organism={organism} setOrganism={setOrganism} onCreated={onCreated} />}
          </>
        )}
      </div>
    </Modal>
  )
}

/** Save the genes (rows) of one comparison as a Flame list, with log2 fold changes as scores. */
function BenchGenes({ res, tested, organism, setOrganism, onCreated }: { res: Result; tested: Target[]; organism: number; setOrganism: (t: number) => void; onCreated: (l: AnalyteList) => void }) {
  const comps = [...new Set(tested.flatMap((t) => t.comparisons!.map((c) => `${c.a} vs ${c.b}`)))]
  const [comp, setComp] = useState(comps[0])
  const [which, setWhich] = useState<'sig' | 'up' | 'down' | 'all'>('sig')
  const rows = tested.map((t) => ({ t, c: t.comparisons!.find((c) => `${c.a} vs ${c.b}` === comp) })).filter((r) => r.c)
  const keep = rows.filter(({ c }) => which === 'all' || (c!.p < 0.05 && (which === 'sig' || (which === 'up' ? c!.diff > 0 : c!.diff < 0))))
  return (
    <details className="bench-genes" open>
      <summary><strong>Save genes as a Flame list</strong></summary>
      <div className="row wrap">
        <Field label="Comparison"><select className="input input-sm" value={comp} onChange={(e) => setComp(e.target.value)}>{comps.map((c) => <option key={c}>{c}</option>)}</select></Field>
        <Field label="Genes"><select className="input input-sm" value={which} onChange={(e) => setWhich(e.target.value as typeof which)}>
          <option value="sig">Changed (p &lt; 0.05)</option><option value="up">Up (p &lt; 0.05)</option><option value="down">Down (p &lt; 0.05)</option><option value="all">All tested</option></select></Field>
        <span className="small muted">{keep.map(({ t }) => t.target).join(', ') || 'none'}</span>
      </div>
      {keep.length > 0 && keep.length < 15 && <p className="muted small">Saved as a plain list: Flame keeps fold changes as scores (a ranked list for GSEA) from 15 genes up.</p>}
      {keep.length > 0 && <SaveList ids={keep.map(({ t }) => t.target)} scores={keep.length >= 15 ? keep.map(({ c }) => Number(c!.diff.toFixed(4))) : undefined} defaultName={`Bench: ${comp}`.slice(0, 70)}
        organism={organism} setOrganism={setOrganism} idFormat="USERINPUT" meta={{ method: 'bench', comparison: comp, helper: res.helper }} onCreated={onCreated} />}
    </details>
  )
}

type FigStyle = 'color' | 'bw'
type BreakMode = 'off' | 'auto' | 'custom'
interface FigOpts { errBar: 'sd' | 'sem'; style: FigStyle; brk: BreakMode; from?: number; to?: number }
const BW_FILLS = ['#ffffff', '#bdbdbd', '#636363', '#000000', '#e6e6e6', '#8c8c8c', '#404040', '#d0d0d0']
/** Dot-and-bar figure (or stacked bars for FACS phases), Prism style, in colour or black & white, with an optional axis break. */
function benchFigure(res: Result, o: FigOpts, colors: string[]) {
  const ts = res.targets.filter((t) => !t.error)
  const groups = res.groups
  const bw = o.style === 'bw'
  const ink = '#000000'
  const fill = (i: number) => (bw ? BW_FILLS[i % BW_FILLS.length] : colors[i % colors.length])
  const darkFill = (i: number) => bw && ['#636363', '#000000', '#404040', '#8c8c8c'].includes(fill(i))
  const yTitle = res.logScale ? `Fold change vs ${res.control}` : res.helper === 'facs' ? '% of cells' : 'Value'
  const axisStyle = bw ? { showline: true, linecolor: ink, linewidth: 1.2, ticks: 'outside', tickcolor: ink, showgrid: false, zeroline: false, color: ink } : {}
  const page = bw ? { paper_bgcolor: '#ffffff', plot_bgcolor: '#ffffff', font: { color: ink, family: 'Arial, Helvetica, sans-serif' } } : {}
  if (res.helper === 'facs') {
    const data = ts.map((t, i) => ({
      type: 'bar', name: t.target, x: groups, y: groups.map((g) => t.summary.find((s) => s.group === g)?.mean ?? null),
      marker: { color: fill(i), line: bw ? { color: ink, width: 1.2 } : undefined }, hovertemplate: `${t.target}: %{y:.1f}%<extra>%{x}</extra>`,
      text: groups.map((g) => { const c = t.comparisons?.find((x) => x.a === g && x.b === res.control); return c && c.stars !== 'ns' ? c.stars : '' }), textposition: 'inside',
      textfont: { color: bw ? (darkFill(i) ? '#ffffff' : ink) : '#ffffff' },
    }))
    return { data, layout: { ...page, barmode: 'stack', xaxis: { ...axisStyle }, yaxis: { ...axisStyle, title: { text: yTitle }, range: [0, 100] }, legend: { orientation: 'h', y: -0.15 }, margin: { l: 60, r: 10, t: 10, b: 70 } }, height: 380, broken: null }
  }
  const k = groups.length, w = Math.min(0.8 / k, 0.28)
  const pos = (ti: number, gi: number) => ti + (gi - (k - 1) / 2) * w
  const err = (t: Target, g: string) => { const s = t.summary.find((x) => x.group === g); return (o.errBar === 'sd' ? s?.sd : s?.sem) ?? 0 }
  const mean = (t: Target, g: string) => t.summary.find((x) => x.group === g)?.mean ?? NaN
  const vals = (t: Target, g: string) => t.values.filter((v) => v.group === g).map((v) => v.value)
  const top = (t: Target, g: string) => Math.max(mean(t, g) + err(t, g), ...vals(t, g))
  const bottom = (t: Target, g: string) => Math.min(mean(t, g) - err(t, g), ...vals(t, g))
  const ymax = Math.max(...ts.flatMap((t) => groups.map((g) => top(t, g))), res.logScale ? 1 : 0)
  // Axis break: automatic or set by the user; null = one continuous axis.
  let broken: [number, number] | null = null
  if (o.brk === 'auto') broken = autoBreak(ts.flatMap((t) => groups.map((g) => ({ top: top(t, g), bottom: bottom(t, g) }))))
  if (o.brk === 'custom' && o.from != null && o.to != null && o.from > 0 && o.to > o.from && o.to < ymax) broken = [o.from, o.to]
  const upTop = nice(ymax * 1.12)
  const axes = broken ? ['y', 'y2'] : ['y']
  const traces = axes.flatMap((ax, ai) => [
    ...groups.map((g, gi) => ({
      type: 'bar', name: g, showlegend: ai === 0, legendgroup: g, yaxis: ax, x: ts.map((_, ti) => pos(ti, gi)), width: w * 0.85,
      y: ts.map((t) => mean(t, g)),
      error_y: { type: 'data', array: ts.map((t) => err(t, g)), visible: true, color: bw ? ink : '#374151', thickness: 1.2, width: 4 },
      marker: bw ? { color: fill(gi), line: { color: ink, width: 1.2 } } : { color: fill(gi), opacity: 0.35, line: { color: fill(gi), width: 1.5 } },
      hovertemplate: `${g}: %{y:.3g}<extra></extra>`,
    })),
    ...groups.map((g, gi) => {
      const jitter = (s: string) => { let h = 0; for (const ch of s) h = (h * 31 + ch.charCodeAt(0)) >>> 0; return ((h % 1000) / 1000 - 0.5) * w * 0.45 }
      const pts = ts.flatMap((t, ti) => t.values.filter((v) => v.group === g).map((v) => ({ x: pos(ti, gi) + jitter(v.sample), y: v.value, s: v.sample })))
      return { type: 'scatter', mode: 'markers', name: `${g} (replicates)`, showlegend: false, legendgroup: g, yaxis: ax, x: pts.map((p) => p.x), y: pts.map((p) => p.y), text: pts.map((p) => p.s),
        marker: bw ? { color: darkFill(gi) ? '#ffffff' : ink, size: 7, line: { color: darkFill(gi) ? ink : '#ffffff', width: 1 } } : { color: fill(gi), size: 8, line: { color: '#fff', width: 1 } },
        hovertemplate: '%{text}: %{y:.3g}<extra></extra>' }
    }),
  ])
  const lowTop = broken ? broken[0] : ymax * 1.18
  const span = (y: number) => (broken && y > broken[0] ? upTop - broken[1] : lowTop)
  const annotations: Record<string, unknown>[] = ts.flatMap((t, ti) => groups.map((g, gi) => {
    const c = t.comparisons?.find((x) => (x.a === g && x.b === res.control) || (x.b === g && x.a === res.control))
    if (!c || g === res.control) return null
    const y0 = top(t, g)
    return { x: pos(ti, gi), y: y0 + span(y0) * 0.04, yref: broken && y0 > broken[0] ? 'y2' : 'y', text: c.stars, showarrow: false, font: { size: c.stars === 'ns' ? 10 : 15, color: bw ? ink : undefined }, yanchor: 'bottom' }
  }).filter((a): a is NonNullable<typeof a> => !!a))
  const shapes: Record<string, unknown>[] = res.logScale ? [{ type: 'line', xref: 'paper', yref: 'y', x0: 0, x1: 1, y0: 1, y1: 1, line: { color: bw ? '#7a7a7a' : '#9ca3af', width: 1, dash: 'dot' } }] : []
  const low = 0.6, high = 0.68                                         // paper heights where the lower axis ends and the upper one starts
  if (broken) {
    for (const y of [low, high]) shapes.push({ type: 'line', xref: 'paper', yref: 'paper', x0: -0.012, x1: 0.012, y0: y - 0.014, y1: y + 0.014, line: { color: bw ? ink : '#6b7280', width: 1.5 } })
    annotations.push({ xref: 'paper', yref: 'paper', x: -0.075, y: 0.5, text: yTitle, textangle: -90, showarrow: false, font: { size: 13, color: bw ? ink : undefined } })
  }
  const xaxis = { ...axisStyle, tickvals: ts.map((_, i) => i), ticktext: ts.map((t) => t.target), range: [-0.6, ts.length - 0.4], zeroline: false, showgrid: false, anchor: 'y' }
  const layout = broken
    ? { ...page, barmode: 'overlay', bargap: 0, xaxis,
        yaxis: { ...axisStyle, domain: [0, low], range: [0, broken[0]] },
        yaxis2: { ...axisStyle, domain: [high, 1], range: [broken[1], upTop], anchor: 'x' },
        annotations, shapes, legend: { orientation: 'h', y: -0.15 }, margin: { l: 70, r: 10, t: 10, b: 70 } }
    : { ...page, barmode: 'overlay', bargap: 0, xaxis, yaxis: { ...axisStyle, title: { text: yTitle }, rangemode: 'tozero', range: [0, ymax * 1.18] },
        annotations, shapes, legend: { orientation: 'h', y: -0.15 }, margin: { l: 60, r: 10, t: 10, b: 70 } }
  return { data: traces, layout, height: broken ? 440 : 400, broken }
}
