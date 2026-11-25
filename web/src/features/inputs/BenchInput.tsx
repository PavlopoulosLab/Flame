import { useEffect, useMemo, useState } from 'react'
import { api, API_BASE } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { Plot } from '../../components/charts'
import { BusyBar, CopyButton, ErrorNote, Field, FileDrop, Modal, Note, Segmented } from '../../components/ui'
import { roleColors, usePaletteMode } from '../../lib/palette'
import { PaletteSelect } from '../../components/PaletteSelect'
import { benchFigure, DEFAULT_PLOT, type PlotOpts } from './benchPlot'
import { PlotControls } from './BenchPlotControls'
import { saveXlsx } from '../../lib/xlsx'
import { SampleGroupPicker, SaveList } from './InputModal'
import { plural } from '../../lib/format'

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
  layout: 'column' | 'grouped'; helper: Helper; groups: string[]; control: string; paired: boolean; allPairs?: boolean; logScale: boolean; shownAs: string
  targets: Target[]; sentence: string; methods: string; warnings: string[]; rowNames: string[]
  /** Grouped tables: the sample columns and the group of each ('' = not used). */
  samples?: string[]; sampleGroups?: string[]; customGroups?: boolean
  /** The column names give no usable groups: the user has to group the samples first. */
  needsGroups?: boolean
}
/** Sample groups set by the user: group names, and each sample column's group index (-1 = not used). */
interface GroupSetup { names: string[]; assign: Record<string, number>; /** Colour picked per group; empty = from the palette. */ colors?: string[] }
/** Step 1 (/inputs/bench/samples): the table's sample columns and the groups guessed from their names. */
interface Parsed { layout: 'column' | 'grouped'; samples: string[]; guess: string[]; rowNames: string[]; maxGroups: number }
/** Groups guessed from the column names; two empty groups when the names give none. */
const setupFrom = (p: Parsed): GroupSetup => {
  const found = [...new Set(p.guess.filter(Boolean))]
  const names = found.length ? found : ['Group A', 'Group B']
  return { names, assign: Object.fromEntries(p.samples.map((smp, i) => [smp, names.indexOf(p.guess[i] ?? '')])) }
}

type Analysis = 'generic' | 'ddct'
/** Example experiments: each loads its table and the settings that suit it. */
const EXAMPLES: { kind: string; analysis: Analysis; label: string; what: string; refs?: string[]; loading?: string; log?: boolean; paired?: boolean; plot?: Partial<PlotOpts> }[] = [
  { kind: 'bench_qpcr_fold', analysis: 'generic', label: 'qPCR: glucocorticoid target genes in airway cells, fold changes from Excel (dexamethasone ± RU486)', log: true, paired: true,
    what: '2^−ΔΔCt fold changes already calculated per sample; tested on a log2 scale.', plot: { kind: 'bar' } },
  { kind: 'bench_western', analysis: 'generic', label: 'Western blot: AKT phosphorylation and apoptosis after a drug ± PI3K inhibitor', loading: 'ACTB',
    what: 'Raw band intensities (e.g. from ImageJ), normalised here to the β-actin row (optional step).', plot: { kind: 'bar' } },
  { kind: 'bench_facs', analysis: 'generic', label: 'Flow cytometry: cell-cycle arrest in G2/M after nocodazole',
    what: '% of cells in G0/G1, S and G2/M, shown as stacked bars.', plot: { kind: 'stacked' } },
  { kind: 'bench_viability', analysis: 'generic', label: 'MTT assay: cancer cell viability across cisplatin doses',
    what: 'Viability (% of vehicle) at 4 doses, each dose against the vehicle (Dunnett).', plot: { kind: 'bar' } },
  { kind: 'bench_elisa', analysis: 'generic', label: 'ELISA: cytokines secreted by macrophages after LPS ± dexamethasone', log: true,
    what: 'IL-6, TNF and IL-1β in pg/mL, tested as log2 ratios, on a log axis.', plot: { kind: 'dots', logY: true } },
  { kind: 'bench_luciferase', analysis: 'generic', label: 'Luciferase reporter: NF-κB activity after TNF ± IKK inhibitor', loading: 'Renilla luciferase (control)',
    what: 'Firefly reporter, normalised here to the Renilla row (dual-luciferase).', plot: { kind: 'bar' } },
  { kind: 'bench_qpcr', analysis: 'ddct', label: 'qPCR: glucocorticoid target genes in airway cells, raw Ct values (dexamethasone ± RU486)', refs: ['GAPDH', 'ACTB'], paired: true,
    what: 'Ct values of 6 genes and 2 reference genes → ΔCt → ΔΔCt → fold change, paired by experiment.', plot: { kind: 'bar' } },
]
const REF_GENES = ['GAPDH', 'ACTB', 'B2M', 'HPRT1', 'RPLP0', 'TBP', '18S', 'RN18S1', 'PPIA', 'YWHAZ', 'UBC', 'GUSB', 'TUBB', 'Gapdh', 'Actb']
const PHASES = /^(G0\/G1|G1|S|G2\/M|G2|M|sub-?G1)$/i
const fmtP = (p: number | null | undefined) => (p == null || !Number.isFinite(p) ? '' : p < 0.0001 ? p.toExponential(1) : p < 0.01 ? p.toPrecision(2) : p.toFixed(3))
const num = (x: number | null | undefined, d = 2) => (x == null || !Number.isFinite(x) ? '' : x.toFixed(d))

/**
 * Experimental analysis (bench data): paste a small table (qPCR Ct, band intensities, FACS percentages or any measurements
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
  const [plot, setPlot] = useState<PlotOpts>(DEFAULT_PLOT)
  const [exampleKind, setExampleKind] = useState('')
  const [organism, setOrganism] = useState(9606)
  // Step 1 -> 2: the sample columns, read as soon as a table is pasted or uploaded.
  const [parsed, setParsed] = useState<Parsed | null>(null)
  // Step 2: which samples form each group (grouped tables). Step 3 starts once the user confirms.
  const [groupSetup, setGroupSetup] = useState<GroupSetup | null>(null)
  const [confirmed, setConfirmed] = useState(false)
  const grouped = parsed?.layout === 'grouped'
  const groupNamesOk = !groupSetup || (groupSetup.names.every((n) => n.trim()) && new Set(groupSetup.names.map((n) => n.trim())).size === groupSetup.names.length)
  const groupSizes = groupSetup ? groupSetup.names.map((_, i) => Object.values(groupSetup.assign).filter((g) => g === i).length) : []
  const groupsReady = !grouped || (!!groupSetup && groupNamesOk && groupSizes.filter((n) => n > 0).length >= 2)
  const groupsBody = useMemo(() => (!grouped || !groupSetup || !groupNamesOk ? null : JSON.stringify(Object.fromEntries(groupSetup.names.map((n, i) => [n.trim(),
    Object.keys(groupSetup.assign).filter((smp) => groupSetup.assign[smp] === i)]).filter(([, m]) => (m as string[]).length)))), [grouped, groupSetup, groupNamesOk])
  // Groups as they will be tested (grouped tables: the editor's groups with samples; column tables: the columns).
  const groupNames = useMemo(() => (grouped && groupSetup ? groupSetup.names.map((n) => n.trim()).filter((_, i) => groupSizes[i] > 0)
    : [...new Set(parsed?.guess ?? [])]), [grouped, groupSetup, parsed, groupSizes.join(',')]) // eslint-disable-line react-hooks/exhaustive-deps
  const effControl = groupNames.includes(control) ? control : groupNames[0] ?? ''

  // The options are only a draft until "Generate plot": the tests and the figure use the applied copy.
  const pmode = usePaletteMode()
  const palette = useMemo(() => roleColors(pmode).series, [pmode])
  const groupColors = useMemo(() => (groupSetup ? groupSetup.names.map((_, i) => groupSetup.colors?.[i] || palette[i % palette.length]) : palette), [groupSetup, palette])
  const draft = useMemo(() => ({
    helper, refs, loading, log, control: effControl, paired, nonpar, allPairs, groupsBody, plot,
    order: grouped && groupSetup ? groupSetup.names.map((n) => n.trim()) : groupNames,
    colors: groupColors,
  }), [helper, refs, loading, log, effControl, paired, nonpar, allPairs, groupsBody, plot, grouped, groupSetup, groupNames, groupColors])
  type Applied = typeof draft
  const [applied, setApplied] = useState<Applied | null>(null)
  const [autoGen, setAutoGen] = useState(false)
  const optionsReady = !!parsed && confirmed && groupsReady && groupNames.length >= 2
    && !(helper === 'qpcr' && !refs.length) && !(helper === 'western' && !loading)
  const dirty = !!applied && JSON.stringify(draft) !== JSON.stringify(applied)
  const generate = () => { if (optionsReady) setApplied(draft) }
  // Examples are drawn straight away.
  useEffect(() => { if (autoGen && optionsReady) { setApplied(draft); setAutoGen(false) } }, [autoGen, optionsReady, draft])

  const loadExample = async (ex: (typeof EXAMPLES)[number]) => {
    try {
      const r = await api<{ text: string; fileName: string; source: string }>(`/inputs/examples/${ex.kind}`)
      setRes(null); setApplied(null); setControl(''); setHelper(ex.analysis === 'ddct' ? 'qpcr' : ex.loading ? 'western' : 'none'); setRefs(ex.refs ?? []); setLoading(ex.loading ?? ''); setPaired(!!ex.paired)
      setNonpar(false); setAllPairs(false); setLog(!!ex.log); setFileName(r.fileName); setSource(r.source); setText(r.text); setExampleKind(ex.kind)
      setPlot((p) => ({ ...p, kind: 'bar', logY: false, ...ex.plot }))
    } catch (e) { setErr(e) }
  }
  useEffect(() => { if (example) loadExample(EXAMPLES[0]) // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  // Step 1: read the sample columns of a new table (no statistics yet).
  useEffect(() => {
    setParsed(null); setGroupSetup(null); setConfirmed(false); setRes(null); setApplied(null)
    if (!text.trim()) return
    const t = setTimeout(() => {
      setBusy(true); setErr(null)
      api<Parsed>('/inputs/bench/samples', { body: { text, fileName } })
        .then((p) => {
          setParsed(p)
          setGroupSetup(p.layout === 'grouped' ? setupFrom(p) : null)
          setConfirmed(p.layout === 'column' || !!source)            // column tables: each column is a group; examples are ready
          if (source) setAutoGen(true)
          // Own tables: never switch the analysis; only pre-tick reference genes (ΔΔCt) or suggest stacked bars (cell-cycle phases).
          if (!source && p.layout === 'grouped') {
            if (helper === 'qpcr' && !refs.length) setRefs(p.rowNames.filter((r) => REF_GENES.includes(r)))
            if (helper !== 'qpcr' && p.rowNames.some((r) => PHASES.test(r)) && p.rowNames.length <= 5) setPlot((x) => ({ ...x, kind: 'stacked' }))
          }
        })
        .catch((e) => setErr(e))
        .finally(() => setBusy(false))
    }, 250)
    return () => clearTimeout(t)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [text, fileName])

  // Step 3: the tests, run with the applied options (plot-only changes redraw without testing again).
  const testKey = applied ? JSON.stringify({ ...applied, plot: undefined, colors: undefined, order: undefined }) : ''
  useEffect(() => {
    if (!applied || !parsed) { setRes(null); return }
    const a = applied
    setBusy(true); setErr(null)
    api<Result>('/inputs/bench/analyse', { body: { text, fileName, helper: a.helper, refs: a.refs, loading: a.loading, control: a.control || null, paired: a.paired,
      nonparametric: a.nonpar, allPairs: a.allPairs, log: a.log, ...(a.groupsBody ? { groups: JSON.parse(a.groupsBody) } : {}) } })
      .then(setRes)
      .catch((e) => { setErr(e); setRes(null) })
      .finally(() => setBusy(false))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [testKey])

  // The figure: applied plot options, the editor's group order and colours (the tests put the control first).
  const colorOf = (g: string, i: number) => {
    const at = applied ? applied.order.indexOf(g) : -1
    return at >= 0 ? applied!.colors[at % applied!.colors.length] : palette[i % palette.length]
  }
  const plotGroups = useMemo(() => (!res ? [] : applied ? [...res.groups].sort((a, b) => applied.order.indexOf(a) - applied.order.indexOf(b)) : res.groups), [res, applied])
  // Stacked bars that add up to ~100 in every group (e.g. cell-cycle phases) are percentages.
  const sumsTo100 = !!res && res.groups.every((g) => Math.abs(res.targets.reduce((a, t) => a + (t.summary.find((x) => x.group === g)?.mean ?? 0), 0) - 100) < 5)
  const defaultYTitle = !res || !applied ? '' : res.logScale ? `Fold change vs ${res.control}` : applied.helper === 'western' ? `Ratio to ${applied.loading}` : applied.plot.kind === 'stacked' && sumsTo100 ? 'Percentage' : 'Value'
  const figure = useMemo(() => (res && applied ? benchFigure({ ...res, groups: plotGroups }, applied.plot, colorOf, defaultYTitle) : null),
    [res, plotGroups, applied, defaultYTitle]) // eslint-disable-line react-hooks/exhaustive-deps
  const smallViolin = !!res && applied?.plot.kind === 'violin' && res.targets.some((t) => t.summary.some((x) => x.n > 0 && x.n < 5))
  const tested = res?.targets.filter((t) => t.comparisons?.length) ?? []
  const genesLike = !!res && res.layout === 'grouped' && res.helper !== 'facs'

  return (
    <Modal title={title} size="xl" onClose={onClose}
      subtitle="Statistics and figures for small lab experiments (qPCR, ELISA, westerns, viability, reporter assays, flow cytometry): upload a table with samples in columns and genes or readouts in rows, group the samples, choose the test and the plot.">
      <div className="stack">
        <Segmented label="Analysis" value={helper === 'qpcr' ? 'ddct' : 'generic'}
          onChange={(v) => { setExampleKind(''); setApplied(null); if (v === 'ddct') { setHelper('qpcr'); setLog(false); setLoading(''); if (!refs.length && parsed) setRefs(parsed.rowNames.filter((r) => REF_GENES.includes(r))) } else { setHelper('none') } }}
          options={[{ value: 'generic', label: 'Experimental analysis (any assay, values ready)' }, { value: 'ddct', label: 'qPCR ΔΔCt (from raw Ct values)' }]} />
        <p className="muted small">{helper === 'qpcr'
          ? 'Raw Ct values, genes in rows and samples in columns, including one or more reference genes: Flame computes ΔCt, ΔΔCt and the fold changes (2^−ΔΔCt).'
          : 'Values you already have, e.g. processed in Excel: fold changes (including qPCR 2^−ΔΔCt), normalised band intensities, % viability, pg/mL, reporter ratios, % of cells. Samples in columns, genes or readouts in rows. Normalising to a row is optional.'}</p>
        <div className="row wrap">
          <FileDrop accept=".txt,.csv,.tsv,.tab" onText={(t, n) => { setSource(null); setExampleKind(''); setRes(null); setControl(''); setText(t); setFileName(n) }} />
          <Field label="Or try an example experiment">
            <select className="input input-sm" aria-label="Example experiment" value={exampleKind} style={{ maxWidth: 560 }}
              onChange={(e) => { const ex = EXAMPLES.find((x) => x.kind === e.target.value); if (ex) loadExample(ex) }}>
              <option value="">Choose an experiment…</option>
              {EXAMPLES.filter((ex) => ex.analysis === (helper === 'qpcr' ? 'ddct' : 'generic')).map((ex) => <option key={ex.kind} value={ex.kind}>{ex.label}</option>)}
            </select>
          </Field>
        </div>
        <textarea className="input mono" rows={5} value={text} aria-label="Table"
          placeholder={helper === 'qpcr'
            ? 'Raw Ct values: genes in rows (with a reference gene), one column per sample:\nGene\tControl 1\tControl 2\tDex 1\tDex 2\nGAPDH\t18.1\t18.3\t18.2\t18.0\nFKBP5\t27.1\t27.4\t23.6\t23.4'
            : 'Values ready to compare: genes or readouts in rows, one column per sample:\nGene\tControl 1\tControl 2\tDex 1\tDex 2\nFKBP5\t1.00\t0.95\t11.7\t12.6\nIL6\t1.00\t0.96\t0.31\t0.33\n\nOr one column per group, one row per replicate:\nVehicle\tCisplatin\n100\t49.8\n96.8\t45.2'}
          onChange={(e) => { setSource(null); setExampleKind(''); setText(e.target.value); setFileName('') }} />
        {source && (() => { const ex = EXAMPLES.find((x) => x.kind === exampleKind); return (
          <p className="example-source"><strong>Example data:</strong> {source}{ex ? <> <em>{ex.what}</em></> : null}{' '}
            {ex && <a href={`${API_BASE}/inputs/examples/${ex.kind}/file`} download>Download the table</a>} to see the layout.</p>
        ) })()}
        {parsed && (
          <ol className="progress-steps" aria-label="Steps">
            <li className="done"><span>1</span> Table: {plural(parsed.samples.length, grouped ? 'sample' : 'group')}{grouped ? `, ${plural(parsed.rowNames.length, 'row')}` : ''}</li>
            <li className={!grouped || confirmed ? 'done' : 'current'}><span>2</span> {grouped ? 'Group the samples' : 'Groups: one per column'}</li>
            <li className={confirmed ? 'current' : ''}><span>3</span> Tests and figure</li>
          </ol>
        )}
        {grouped && groupSetup && parsed && (
          <BenchGroups samples={parsed.samples} setup={groupSetup} setSetup={setGroupSetup} initial={setupFrom(parsed)} maxGroups={parsed.maxGroups}
            helper={helper} colors={groupColors} palette={palette} />
        )}
        {grouped && !groupNamesOk && <Note kind="warning">Give each group a different, non-empty name.</Note>}
        {grouped && groupNamesOk && !groupsReady && <Note kind="info">Put samples in at least two groups.</Note>}
        {grouped && groupsReady && groupSizes.some((n) => n === 1) && <Note kind="info">A group with one sample has no spread: put 2 or more samples in each group to test it.</Note>}
        {grouped && !confirmed && (
          <div className="row">
            <button className="btn btn-primary" disabled={!groupsReady} onClick={() => setConfirmed(true)}>
              Next: test and plot options ({plural(groupSizes.filter((n) => n > 0).length, 'group')})</button>
            <span className="muted small">You can still change the groups afterwards.</span>
          </div>
        )}
        {parsed && confirmed && (
          <div className="card stack-sm bench-setup">
          <h3 className="small">Test</h3>
          <div className="row wrap bench-options">
            {helper === 'qpcr' ? (
              <Field label="Reference gene(s)">
                <div className="row wrap">{parsed.rowNames.map((r) => (
                  <label key={r} className="check small"><input type="checkbox" checked={refs.includes(r)}
                    onChange={(e) => setRefs((cur) => (e.target.checked ? [...cur, r] : cur.filter((x) => x !== r)))} /> {r}</label>
                ))}</div>
              </Field>
            ) : (
              <>
                <Field label="Normalise to a row (optional)">
                  <select className="input input-sm" value={helper === 'western' ? loading : ''} disabled={!grouped}
                    onChange={(e) => { const r = e.target.value; setLoading(r); setHelper(r ? 'western' : 'none') }}>
                    <option value="">No: values are ready</option>
                    {parsed.rowNames.map((r) => <option key={r} value={r}>{r}</option>)}
                  </select>
                </Field>
                <label className="check small align-end" title="Fold changes and ratios are compared on a log2 scale, so that 2× up and 2× down are equally far from 1">
                  <input type="checkbox" checked={helper === 'western' || log} disabled={helper === 'western'} onChange={(e) => setLog(e.target.checked)} /> Values are ratios or fold changes: test on log2 scale</label>
              </>
            )}
            <Field label="Control group">
              <select className="input input-sm" value={effControl} onChange={(e) => setControl(e.target.value)}>
                {groupNames.map((g) => <option key={g}>{g}</option>)}
              </select>
            </Field>
            <label className="check small" title="Each replicate (row, or replicate number) is the same experiment, blot or donor in every group"><input type="checkbox" checked={paired} onChange={(e) => setPaired(e.target.checked)} /> Rows are paired</label>
            <label className="check small"><input type="checkbox" checked={nonpar} onChange={(e) => setNonpar(e.target.checked)} /> Non-parametric</label>
            {groupNames.length > 2 && <label className="check small"><input type="checkbox" checked={allPairs} onChange={(e) => setAllPairs(e.target.checked)} /> All pairs (not only vs control)</label>}
          </div>
          {helper === 'qpcr' && !refs.length && <Note kind="info">Tick the reference gene(s) to compute ΔΔCt.</Note>}
          {helper === 'qpcr' && !grouped && <Note kind="warning">ΔΔCt needs genes in rows (including a reference gene) and samples in columns.</Note>}
          <h3 className="small">Plot</h3>
          <PlotControls o={plot} set={setPlot} hasComparisons canStack={parsed.rowNames.filter((r) => !(helper === 'qpcr' && refs.includes(r)) && r !== loading).length > 1} />
          <div className="toolbar">
            <span className="small muted">Colours</span>
            <PaletteSelect />
            {grouped && <span className="small muted">or pick one per group in the group editor above</span>}
          </div>
          <div className="row">
            <button className="btn btn-primary" disabled={!optionsReady || (!!applied && !dirty) || busy} onClick={generate}>{applied ? 'Update plot' : 'Generate plot'}</button>
            {dirty && <span className="small">Options changed: press <strong>Update plot</strong> to apply them.</span>}
            {!optionsReady && confirmed && <span className="small muted">{!groupsReady || groupNames.length < 2 ? 'Put samples in at least two groups first.' : 'Complete the options above first.'}</span>}
          </div>
          </div>
        )}
        {busy && <BusyBar label={applied ? 'Testing and drawing' : 'Reading the table'} expect={2} />}
        <ErrorNote error={err} />
        {res && !res.needsGroups && (
          <>
            <p className="bench-sentence"><strong>Test:</strong> {res.sentence}</p>
            {res.warnings.map((w) => <Note key={w} kind="warning">{w}</Note>)}
            {figure && applied && (() => { const ap = applied.plot; return (
              <div className="stack-sm bench-figure">
                {dirty && <Note kind="info">This figure uses the previous options. Press <strong>Update plot</strong> above to apply your changes.</Note>}
                {smallViolin && <Note kind="info">Some groups have fewer than 5 values: a violin then shows the smoothing more than the data. A scatter dot plot or box plot is clearer for small n.</Note>}
                {ap.brk !== 'off' && !figure.broken && ap.kind !== 'stacked' && <p className="small muted">{ap.brk === 'auto' ? 'No axis break needed: the values are of similar height.' : 'Axis break: set from < to, both below the top of the axis.'}</p>}
                {figure.broken && <p className="small muted">Axis broken from {figure.broken[0]} to {figure.broken[1]}.</p>}
                <div style={ap.figW ? { width: ap.figW, maxWidth: '100%' } : undefined}>
                  <Plot data={figure.data as never} layout={figure.layout as never} height={ap.figH} tools={false} exportName={`flame_bench_${ap.kind}`} />
                </div>
                <p className="muted small">{ap.points && ap.kind !== 'stacked' ? 'Points = replicates. ' : ''}{tested.length > 0 && ap.pvals !== 'off'
                  ? `${ap.kind === 'stacked' ? `P-values in each segment: vs ${res.control}` : `Brackets: ${res.allPairs ? 'all pairs of groups' : `each group vs ${res.control}`}`}${ap.pvals === 'stars' ? `; * p < 0.05, ** < 0.01, *** < 0.001${ap.hideNs ? '' : ', ns = not significant'}` : ''}${tested[0]?.posthoc ? ` (${tested[0].posthoc})` : ''}.` : ''}</p>
              </div>
            ) })()}
            <div className="table-scroll fetch-table"><table className="table compact">
              <thead><tr><th>{res.layout === 'grouped' ? 'Gene / target' : 'Target'}</th><th>Comparison</th>
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

/**
 * Step 2: which sample columns form each group, set as in the expression boxplots: the number of groups,
 * a name and colour per group, then tick samples and send them to a group. Starts from the groups
 * guessed from the column names; columns in no group are left out of the tests.
 */
function BenchGroups({ samples, setup, setSetup, initial, maxGroups, helper, colors, palette }: {
  samples: string[]; setup: GroupSetup; setSetup: (s: GroupSetup) => void; initial: GroupSetup; maxGroups: number; helper: Helper; colors: string[]; palette: string[]
}) {
  const edit = (f: (v: GroupSetup) => GroupSetup) => setSetup(f(setup))
  const letter = (i: number) => (i < 26 ? String.fromCharCode(65 + i) : String(i + 1))
  // More groups: add "Group C", "Group D"...; fewer: the last groups go and their samples become unused.
  const setCount = (n: number) => edit((v) => {
    const names = v.names.slice(0, n)
    for (let i = names.length; i < n; i++) { let nm = `Group ${letter(i)}`, k = i; while (names.includes(nm)) nm = `Group ${letter(++k)}`; names.push(nm) }
    return { names, colors: (v.colors ?? []).slice(0, n), assign: Object.fromEntries(Object.entries(v.assign).map(([smp, g]) => [smp, g < n ? g : -1])) }
  })
  const changed = JSON.stringify({ ...setup, colors: undefined }) !== JSON.stringify({ ...initial, colors: undefined })
  const custom = (setup.colors ?? []).some(Boolean)
  return (
    <SampleGroupPicker samples={samples} names={setup.names} colors={colors}
      groupOf={(smp) => setup.assign[smp] ?? -1}
      onAssign={(ss, g) => edit((v) => ({ ...v, assign: { ...v.assign, ...Object.fromEntries(ss.map((x) => [x, g])) } }))}>
      <div className="row wrap">
        <Field label="Groups">
          <select className="input input-sm" aria-label="Number of groups" value={setup.names.length} onChange={(e) => setCount(Number(e.target.value))}>
            {Array.from({ length: maxGroups - 1 }, (_, i) => i + 2).map((n) => <option key={n} value={n}>{n} groups</option>)}
          </select>
        </Field>
        {changed && <button type="button" className="btn btn-ghost btn-sm align-end" title="Group the samples by their column names again" onClick={() => setSetup({ ...initial, colors: setup.colors })}>Reset to column names</button>}
        {custom && <button type="button" className="btn btn-ghost btn-sm align-end" title="Drop the colours picked per group and use the palette again" onClick={() => edit((v) => ({ ...v, colors: [] }))}>Reset to palette</button>}
      </div>
      <div className="row wrap">
        {setup.names.map((n, i) => (
          <Field key={i} label={`Group ${i + 1}: name and colour`}>
            <span className="row">
              <input className="input input-sm" value={n} maxLength={60} aria-label={`Name of group ${i + 1}`}
                onChange={(e) => edit((v) => ({ ...v, names: v.names.map((x, j) => (j === i ? e.target.value : x)) }))} />
              <input type="color" aria-label={`Colour of ${n}`} value={colors[i] ?? palette[i % palette.length]} style={{ width: 32, height: 28, padding: 0, border: 'none', background: 'none', cursor: 'pointer' }}
                onChange={(e) => { const c = e.target.value; edit((v) => { const cs = [...(v.colors ?? [])]; cs[i] = c; return { ...v, colors: cs } }) }} />
            </span>
          </Field>
        ))}
      </div>
      <p className="muted small">Up to {maxGroups} groups. Samples in no group are left out of the tests{helper === 'qpcr' ? ' (they are still normalised to the reference genes)' : ''}. With <em>Rows are paired</em>, the first sample of each group is paired with the first of the others, and so on, in table order.</p>
    </SampleGroupPicker>
  )
}
