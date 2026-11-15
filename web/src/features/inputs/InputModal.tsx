import { useEffect, useMemo, useState } from 'react'
import { api, saveText } from '../../api/client'
import { useCreateList } from '../../api/hooks'
import type { AnalyteList } from '../../api/types'
import { Plot } from '../../components/charts'
import { CopyButton, ErrorNote, Field, FileDrop, IdFormatSelect, Modal, Note, OrganismPicker, Segmented, Spinner } from '../../components/ui'
import { plural } from '../../lib/format'
import { categorical, roleColors, usePaletteMode } from '../../lib/palette'
import { METHODS, type InputKind } from './MethodPicker'

interface Props {
  kind: InputKind
  example?: boolean
  lists: AnalyteList[]
  onClose: () => void
  onCreated: (list: AnalyteList) => void
  /** Pre-filled table (e.g. a two-group comparison handed to the volcano selector). */
  initialText?: string
  initialFileName?: string
  /** Open the volcano selector with a table (used by the expression input). */
  onOpenVolcano?: (text: string, fileName: string) => void
}

export function InputModal(props: Props) {
  const title = METHODS.find((m) => m.kind === props.kind)?.title ?? 'Add a list'
  switch (props.kind) {
    case 'upload':
      return <UploadPaste {...props} title={title} />
    case 'snps':
      return <Snps {...props} title={title} />
    case 'textmining':
      return <TextMining {...props} title={title} />
    case 'volcano':
    case 'reduction':
      return <TableInput {...props} title={title} />
    case 'upset':
      return <Upset {...props} title={title} />
    case 'expression':
      return <ExpressionInput {...props} title={title} />
    case 'mirna':
      return <MirnaInput {...props} title={title} />
  }
}

type P = Props & { title: string }

function useExample(kind: string, enabled: boolean | undefined, apply: (text: string, fileName?: string) => void) {
  const [loading, setLoading] = useState(false)
  const load = () => {
    setLoading(true)
    api<{ text: string; fileName?: string }>(`/inputs/examples/${kind}`)
      .then((ex) => apply(ex.text, ex.fileName))
      .finally(() => setLoading(false))
  }
  useEffect(() => {
    if (enabled) load()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])
  return { load, loading }
}

/** Shared "save as list" footer: name, organism, ID format. */
function SaveList({
  ids,
  scores,
  defaultName,
  organism,
  setOrganism,
  idFormat,
  setIdFormat,
  showOrganism = true,
  showFormat = true,
  meta,
  onCreated,
  extra,
}: {
  ids: string[]
  scores?: number[]
  defaultName: string
  organism: number
  setOrganism?: (t: number) => void
  idFormat: string
  setIdFormat?: (f: string) => void
  showOrganism?: boolean
  showFormat?: boolean
  meta?: Record<string, unknown>
  onCreated: (l: AnalyteList) => void
  extra?: React.ReactNode
}) {
  const [name, setName] = useState(defaultName)
  const create = useCreateList()
  useEffect(() => setName(defaultName), [defaultName])
  return (
    <div className="save-list">
      <div className="save-list-fields">
        <Field label="List name">
          <input className="input" value={name} maxLength={80} onChange={(e) => setName(e.target.value)} />
        </Field>
        {showOrganism && setOrganism && <OrganismPicker value={organism} onChange={(t) => setOrganism(t)} />}
        {showFormat && setIdFormat && <IdFormatSelect value={idFormat} onChange={setIdFormat} label="ID type" />}
        {extra}
      </div>
      <ErrorNote error={create.error} />
      <div className="row end">
        <span className="muted small">{plural(ids.length, 'ID')}{scores ? ' with scores (ranked list, for GSEA)' : ''} will be saved</span>
        <button
          className="btn btn-primary"
          disabled={!ids.length || !name.trim() || create.isPending}
          onClick={() =>
            create.mutate(
              { name: name.trim(), ids, scores, organismTaxId: organism, idFormat, meta, uniqueName: false },
              { onSuccess: onCreated },
            )
          }
        >
          {create.isPending ? 'Saving…' : 'Save list and continue'}
        </button>
      </div>
    </div>
  )
}

// ---- Paste / upload ---------------------------------------------------------

function UploadPaste({ example, onClose, onCreated, title }: P) {
  const [text, setText] = useState('')
  const [skip, setSkip] = useState(false)
  const [ranked, setRanked] = useState(false)
  const [organism, setOrganism] = useState(9606)
  const [idFormat, setIdFormat] = useState('USERINPUT')
  const [parsed, setParsed] = useState<{ ids: string[]; scores?: number[]; unique: number; duplicates: number; invalid: number; invalidExamples: string[] } | null>(null)
  const [err, setErr] = useState<unknown>(null)
  const ex = useExample('upload', example, (t) => setText(t))
  useEffect(() => {
    if (!text.trim()) {
      setParsed(null)
      return
    }
    const h = setTimeout(() => {
      api<typeof parsed>('/inputs/upload/parse', { body: { text, skipFirstRow: skip, ranked } }).then(setParsed).catch(setErr)
    }, 300)
    return () => clearTimeout(h)
  }, [text, skip, ranked])
  return (
    <Modal title={title} onClose={onClose} size="lg" subtitle="One ID per line, or separated by commas, tabs or semicolons.">
      <div className="row between">
        <span className="muted small">Gene symbols, Ensembl, Entrez, UniProt and 15 other formats are accepted.</span>
        <button className="btn btn-ghost btn-sm" onClick={ex.load} disabled={ex.loading}>Load example</button>
      </div>
      <textarea className="input mono" rows={10} value={text} onChange={(e) => setText(e.target.value)} placeholder={'TP53\nBRCA1\nEGFR'} aria-label="IDs" />
      <FileDrop accept=".txt,.csv,.tsv" onText={(t) => setText(t)} />
      <div className="row wrap">
        <label className="check"><input type="checkbox" checked={skip} onChange={(e) => setSkip(e.target.checked)} /> First row is a header</label>
        <label className="check" title="Two columns: ID and a numeric score (e.g. log2 fold change or a signed statistic). Needed for GSEA.">
          <input type="checkbox" checked={ranked} onChange={(e) => setRanked(e.target.checked)} /> IDs with scores (ranked list, for GSEA)
        </label>
      </div>
      {parsed && (
        <Note kind={parsed.invalid ? 'warning' : 'success'}>
          {plural(parsed.unique, 'unique ID')}
          {parsed.duplicates ? ` · ${parsed.duplicates} duplicates removed` : ''}
          {parsed.invalid ? ` · ${parsed.invalid} skipped (e.g. ${parsed.invalidExamples.slice(0, 3).join(', ')})` : ''}
        </Note>
      )}
      <ErrorNote error={err} />
      <SaveList ids={parsed?.ids ?? []} scores={ranked ? parsed?.scores : undefined} defaultName={ranked ? 'My ranked genes' : 'My genes'} organism={organism} setOrganism={setOrganism}
        idFormat={idFormat} setIdFormat={setIdFormat} meta={{ method: 'upload' }} onCreated={onCreated} />
    </Modal>
  )
}

// ---- SNPs -------------------------------------------------------------------

interface SnpResult { rows: Record<string, string>[]; stats: Record<string, number>; genes: { USERINPUT: string[]; ENSG: string[] } }

function Snps({ example, onClose, onCreated, title }: P) {
  const [text, setText] = useState('')
  const [res, setRes] = useState<SnpResult | null>(null)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [fmt, setFmt] = useState<'USERINPUT' | 'ENSG'>('USERINPUT')
  const ex = useExample('snps', example, (t) => setText(t))
  const map = () => {
    setBusy(true)
    setErr(null)
    api<SnpResult>('/inputs/snps/map', { body: { text } }).then(setRes).catch(setErr).finally(() => setBusy(false))
  }
  return (
    <Modal title={title} onClose={onClose} size="lg" subtitle="Human dbSNP rsIDs (e.g. rs1130233) are mapped to genes with g:SNPense.">
      <div className="row between">
        <span className="muted small">One rsID per line.</span>
        <button className="btn btn-ghost btn-sm" onClick={ex.load}>Load example</button>
      </div>
      <textarea className="input mono" rows={6} value={text} onChange={(e) => setText(e.target.value)} placeholder="rs1130233" aria-label="SNP IDs" />
      <div className="row end"><button className="btn btn-secondary" disabled={!text.trim() || busy} onClick={map}>{busy ? 'Mapping…' : 'Map to genes'}</button></div>
      <ErrorNote error={err} />
      {res && (
        <>
          <Note kind="success">{plural(res.rows.length, 'mapping')} found.</Note>
          <SimpleTable rows={res.rows} max={200} />
          <SaveList ids={res.genes[fmt] ?? []} defaultName="Genes from SNPs" organism={9606} idFormat={fmt}
            showOrganism={false} showFormat={false} meta={{ method: 'snps' }} onCreated={onCreated}
            extra={<Field label="Save genes as"><Segmented label="Output ID type" value={fmt} onChange={setFmt}
              options={[{ value: 'USERINPUT', label: 'Gene symbols' }, { value: 'ENSG', label: 'Ensembl IDs' }]} /></Field>} />
        </>
      )}
    </Modal>
  )
}

// ---- Text mining ------------------------------------------------------------

interface ExtractRow { rowId?: string; name: string; type?: string; species?: string; id: string; idUrl?: string; ambiguous?: boolean; selected?: boolean }

function TextMining({ example, onClose, onCreated, title }: P) {
  const [text, setText] = useState('')
  const [organism, setOrganism] = useState(9606)
  const [rows, setRows] = useState<ExtractRow[] | null>(null)
  const [chosen, setChosen] = useState<Set<number>>(new Set())
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const ex = useExample('textmining', example, (t) => setText(t))
  const extract = () => {
    setBusy(true)
    setErr(null)
    api<{ rows: ExtractRow[] }>('/inputs/textmining/extract', { body: { text, organismTaxId: organism } })
      .then((r) => {
        setRows(r.rows)
        setChosen(new Set(r.rows.map((x, i) => (x.selected !== false && !x.ambiguous ? i : -1)).filter((i) => i >= 0)))
      })
      .catch(setErr)
      .finally(() => setBusy(false))
  }
  const ids = rows ? [...new Set([...chosen].map((i) => rows[i].id).filter(Boolean))] : []
  const names = rows ? [...new Set(rows.map((r) => r.name).filter(Boolean))] : []
  return (
    <Modal title={title} onClose={onClose} size="xl" subtitle="Gene and protein names are recognised with the EXTRACT tagger (JensenLab).">
      <div className="row between">
        <OrganismPicker value={organism} onChange={(t) => setOrganism(t)} label="Organism of the text" />
        <button className="btn btn-ghost btn-sm" onClick={ex.load}>Load example</button>
      </div>
      <textarea className="input" rows={7} value={text} maxLength={500000} onChange={(e) => setText(e.target.value)} placeholder="Paste an abstract or any text…" aria-label="Text" />
      <div className="row between">
        <span className="muted small">{text.length.toLocaleString()} characters</span>
        <button className="btn btn-secondary" disabled={text.trim().length < 3 || busy} onClick={extract}>{busy ? 'Finding genes…' : 'Find genes'}</button>
      </div>
      <ErrorNote error={err} />
      {rows && (
        <div className="two-col-tight">
          <div>
            <h3>Highlighted text</h3>
            <div className="annotated">{highlight(text, names)}</div>
          </div>
          <div>
            <div className="row between">
              <h3>{plural(rows.length, 'mention')}</h3>
              <span className="row">
                <button className="btn btn-ghost btn-sm" onClick={() => setChosen(new Set(rows.map((_, i) => i)))}>Select all</button>
                <button className="btn btn-ghost btn-sm" onClick={() => setChosen(new Set())}>Clear</button>
              </span>
            </div>
            <div className="table-scroll small-table">
              <table className="table">
                <thead><tr><th /><th>Name</th><th>ID</th></tr></thead>
                <tbody>
                  {rows.map((r, i) => (
                    <tr key={i} className={r.ambiguous ? 'muted' : ''}>
                      <td><input type="checkbox" checked={chosen.has(i)} aria-label={`Use ${r.name}`}
                        onChange={() => setChosen((s) => { const n = new Set(s); if (n.has(i)) n.delete(i); else n.add(i); return n })} /></td>
                      <td>{r.name}{r.ambiguous ? ' (ambiguous)' : ''}</td>
                      <td>{r.idUrl && /^https?:\/\//.test(r.idUrl) ? <a href={r.idUrl} target="_blank" rel="noopener noreferrer">{r.id}</a> : r.id}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      )}
      {rows && <SaveList ids={ids} defaultName="Genes from text" organism={organism} idFormat="USERINPUT" showOrganism={false}
        showFormat={false} meta={{ method: 'textmining' }} onCreated={onCreated} />}
    </Modal>
  )
}

/** Highlight names in text using React text nodes only (no HTML injection). */
function highlight(text: string, names: string[]) {
  if (!names.length) return text
  const escaped = names.filter((n) => n.length > 1).sort((a, b) => b.length - a.length).map((n) => n.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'))
  if (!escaped.length) return text
  const re = new RegExp(`\\b(${escaped.join('|')})\\b`, 'g')
  const out: React.ReactNode[] = []
  let last = 0
  for (const m of text.matchAll(re)) {
    if (m.index! > last) out.push(text.slice(last, m.index))
    out.push(<mark key={m.index}>{m[0]}</mark>)
    last = m.index! + m[0].length
  }
  out.push(text.slice(last))
  return out
}

// ---- Volcano / 2D reduction ---------------------------------------------------

interface Prepared { rowCount: number; columns: { all: string[]; numeric: string[] }; defaults: Record<string, string> }
interface VolcanoPoint { pointId: string; id: string; logfc: number; pvalue: number; negLog10p: number; status: 'up' | 'down' | 'ns' }
interface ReductionPoint { pointId: string; id: string; x: number; y: number; color?: string | number; size?: number }

function TableInput({ kind, example, onClose, onCreated, title, initialText, initialFileName }: P) {
  const isVolcano = kind === 'volcano'
  const [text, setText] = useState(initialText ?? '')
  const [fileName, setFileName] = useState(initialFileName ?? '')
  const [prep, setPrep] = useState<Prepared | null>(null)
  const [mapping, setMapping] = useState<Record<string, string>>({})
  const [p, setP] = useState(0.05)
  const [lfc, setLfc] = useState(1)
  const [points, setPoints] = useState<{ points: (VolcanoPoint | ReductionPoint)[]; plotted: number; total: number; counts?: Record<string, number>; colorType?: string } | null>(null)
  const [sel, setSel] = useState<Set<string>>(new Set())
  const [organism, setOrganism] = useState(9606)
  const [idFormat, setIdFormat] = useState('USERINPUT')
  const [err, setErr] = useState<unknown>(null)
  const [busy, setBusy] = useState(false)
  const ex = useExample(kind, example, (t, f) => { setText(t); setFileName(f ?? '') })
  const pm = usePaletteMode()
  const rc = roleColors(pm)

  useEffect(() => {
    if (!text.trim()) return
    setErr(null)
    api<Prepared>('/inputs/table/prepare', { body: { text, fileName, kind } })
      .then((r) => { setPrep(r); setMapping(r.defaults); setPoints(null); setSel(new Set()) })
      .catch((e) => { setPrep(null); setErr(e) })
  }, [text, fileName, kind])

  const draw = () => {
    setBusy(true)
    setErr(null)
    const path = isVolcano ? '/inputs/volcano/points' : '/inputs/reduction/points'
    api<NonNullable<typeof points>>(path, { body: { text, fileName, mapping, pThreshold: p, lfcThreshold: lfc } })
      .then((r) => { setPoints(r); setSel(new Set()) })
      .catch(setErr)
      .finally(() => setBusy(false))
  }

  const byId = useMemo(() => new Map((points?.points ?? []).map((pt) => [pt.pointId, pt])), [points])
  const selectedIds = [...new Set([...sel].map((pid) => byId.get(pid)?.id).filter((x): x is string => !!x))]

  const plotData = useMemo(() => {
    if (!points) return []
    if (isVolcano) {
      const groups: Record<string, { name: string; color: string }> = {
        up: { name: 'Up-regulated', color: rc.up }, down: { name: 'Down-regulated', color: rc.down }, ns: { name: 'Not significant', color: rc.ns },
      }
      return Object.entries(groups).map(([k, g]) => {
        const pts = (points.points as VolcanoPoint[]).filter((x) => x.status === k)
        return {
          type: 'scatter' as const, mode: 'markers' as const, name: g.name,
          x: pts.map((x) => x.logfc), y: pts.map((x) => x.negLog10p), customdata: pts.map((x) => x.pointId),
          text: pts.map((x) => `${x.id}<br>log2FC ${x.logfc.toFixed(2)}<br>p ${x.pvalue.toExponential(2)}`),
          hoverinfo: 'text' as const,
          marker: { color: g.color, size: 6, opacity: 0.75, line: { width: pts.map((x) => (sel.has(x.pointId) ? 2 : 0)), color: '#f97316' } },
        }
      })
    }
    const pts = points.points as ReductionPoint[]
    const cats = points.colorType === 'categorical' ? [...new Set(pts.map((x) => String(x.color)))] : ['all']
    return cats.map((c) => {
      const sub = points.colorType === 'categorical' ? pts.filter((x) => String(x.color) === c) : pts
      return {
        type: 'scatter' as const, mode: 'markers' as const, name: points.colorType === 'categorical' ? c : 'points',
        x: sub.map((x) => x.x), y: sub.map((x) => x.y), customdata: sub.map((x) => x.pointId),
        text: sub.map((x) => `${x.id}${x.color != null ? `<br>${mapping.color}: ${x.color}` : ''}`), hoverinfo: 'text' as const,
        marker: {
          size: sub.map((x) => (x.size != null ? Math.max(4, Math.min(18, x.size)) : 7)), opacity: 0.8,
          ...(points.colorType === 'continuous' ? { color: sub.map((x) => Number(x.color)), colorscale: 'Viridis', showscale: true } : {}),
          line: { width: sub.map((x) => (sel.has(x.pointId) ? 2 : 0)), color: '#f97316' },
        },
      }
    })
  }, [points, isVolcano, sel, mapping.color, rc])

  const quick = (status: 'sig' | 'up' | 'down') => {
    const pts = (points?.points ?? []) as VolcanoPoint[]
    setSel(new Set(pts.filter((x) => (status === 'sig' ? x.status !== 'ns' : x.status === status)).map((x) => x.pointId)))
  }

  const roles = isVolcano
    ? [{ key: 'id', label: 'Gene ID column', numeric: false }, { key: 'logfc', label: 'log2 fold change', numeric: true }, { key: 'pvalue', label: 'p-value (or adjusted)', numeric: true }]
    : [{ key: 'id', label: 'Gene ID column', numeric: false }, { key: 'x', label: 'X axis', numeric: true }, { key: 'y', label: 'Y axis', numeric: true },
       { key: 'color', label: 'Colour (optional)', numeric: false, optional: true }, { key: 'size', label: 'Size (optional)', numeric: true, optional: true }]

  return (
    <Modal title={title} onClose={onClose} size="xl"
      subtitle={isVolcano ? 'A table with a gene column, a log2 fold change column and a p-value column (CSV or TSV).' : 'A table with a gene column and two coordinate columns (CSV or TSV).'}>
      <div className="row between">
        <span className="muted small">Step 1 · Provide the table</span>
        <button className="btn btn-ghost btn-sm" onClick={ex.load}>Load example</button>
      </div>
      <FileDrop accept=".csv,.tsv,.txt" onText={(t, n) => { setText(t); setFileName(n) }} />
      <details>
        <summary className="small">…or paste the table</summary>
        <textarea className="input mono" rows={6} value={text} onChange={(e) => { setText(e.target.value); setFileName('') }} aria-label="Table" />
      </details>
      <ErrorNote error={err} />
      {prep && (
        <>
          <p className="muted small">Step 2 · Check the columns ({prep.rowCount.toLocaleString()} rows detected)</p>
          <div className="grid-fields">
            {roles.map((r) => (
              <Field key={r.key} label={r.label}>
                <select className="input" value={mapping[r.key] ?? ''} onChange={(e) => setMapping({ ...mapping, [r.key]: e.target.value })}>
                  {'optional' in r && r.optional && <option value="">None</option>}
                  {(r.numeric ? prep.columns.numeric : prep.columns.all).map((c) => <option key={c} value={c}>{c}</option>)}
                </select>
              </Field>
            ))}
            {isVolcano && (
              <>
                <Field label="p-value below" hint="Significance cut-off"><input className="input" type="number" step="0.001" min="0" max="1" value={p} onChange={(e) => setP(Number(e.target.value))} /></Field>
                <Field label="|log2FC| above" hint="Effect-size cut-off"><input className="input" type="number" step="0.1" min="0" value={lfc} onChange={(e) => setLfc(Number(e.target.value))} /></Field>
              </>
            )}
          </div>
          <div className="row end"><button className="btn btn-secondary" onClick={draw} disabled={busy}>{busy ? 'Drawing…' : points ? 'Redraw plot' : 'Draw plot'}</button></div>
        </>
      )}
      {points && (
        <>
          <p className="muted small">Step 3 · Select genes: drag a box on the plot (or pick the lasso in the plot toolbar){isVolcano ? ', or use a quick selection' : ''}. Shift-click adds points.</p>
          {isVolcano && points.counts && (
            <div className="row">
              <button className="btn btn-secondary btn-sm" onClick={() => quick('sig')}>All significant ({points.counts.significant})</button>
              <button className="btn btn-secondary btn-sm" onClick={() => quick('up')}>Up ({points.counts.up})</button>
              <button className="btn btn-secondary btn-sm" onClick={() => quick('down')}>Down ({points.counts.down})</button>
              <button className="btn btn-ghost btn-sm" onClick={() => setSel(new Set())}>Clear</button>
            </div>
          )}
          {!isVolcano && <div className="row"><button className="btn btn-ghost btn-sm" onClick={() => setSel(new Set())}>Clear selection</button></div>}
          <Plot data={plotData} height={420} dragmode="select"
            layout={{ xaxis: { title: { text: isVolcano ? 'log2 fold change' : mapping.x } }, yaxis: { title: { text: isVolcano ? '−log10 p-value' : mapping.y } },
              shapes: isVolcano ? [
                { type: 'line', xref: 'paper', x0: 0, x1: 1, y0: -Math.log10(p), y1: -Math.log10(p), line: { color: '#dc2626', dash: 'dot', width: 1 } },
                { type: 'line', yref: 'paper', y0: 0, y1: 1, x0: lfc, x1: lfc, line: { color: '#dc2626', dash: 'dot', width: 1 } },
                { type: 'line', yref: 'paper', y0: 0, y1: 1, x0: -lfc, x1: -lfc, line: { color: '#dc2626', dash: 'dot', width: 1 } },
              ] : [] }}
            onSelect={(ids, add) => setSel((s) => new Set(add ? [...s, ...ids] : ids))} />
          <p className="muted small">Plotted {points.plotted.toLocaleString()} of {points.total.toLocaleString()} rows · {plural(selectedIds.length, 'gene')} selected{selectedIds.length > 3000 ? ' (a list holds at most 3,000 IDs)' : ''}</p>
          <SaveList ids={selectedIds.slice(0, 3000)} defaultName={isVolcano ? 'Volcano selection' : 'Cluster selection'} organism={organism}
            setOrganism={setOrganism} idFormat={idFormat} setIdFormat={setIdFormat} meta={{ method: kind }} onCreated={onCreated} />
          {isVolcano && <RankedFromVolcano points={points.points as VolcanoPoint[]} organism={organism} idFormat={idFormat} onCreated={onCreated} />}
        </>
      )}
    </Modal>
  )
}

/** Whole volcano table as a ranked list: score = sign(log2FC) × −log10 p (for GSEA). */
function RankedFromVolcano({ points, organism, idFormat, onCreated }: { points: VolcanoPoint[]; organism: number; idFormat: string; onCreated: (l: AnalyteList) => void }) {
  const create = useCreateList()
  const best = new Map<string, number>()
  points.forEach((p) => {
    const sc = Math.sign(p.logfc) * p.negLog10p
    const prev = best.get(p.id)
    if (prev === undefined || Math.abs(sc) > Math.abs(prev)) best.set(p.id, sc)
  })
  return (
    <div className="note note-info">
      <strong>Want to use every gene instead of a selection?</strong> Save the whole table as a ranked list (score = sign(log2FC) × −log10 p) and run
      GSEA on it — no cut-offs needed.
      <div className="row end">
        <button className="btn btn-secondary btn-sm" disabled={create.isPending} onClick={() =>
          create.mutate({ name: 'Volcano ranked (GSEA)', ids: [...best.keys()], scores: [...best.values()], organismTaxId: organism, idFormat, uniqueName: true, meta: { method: 'volcano-ranked' } },
            { onSuccess: onCreated })}>
          Save whole table as ranked list ({best.size.toLocaleString()} genes)
        </button>
      </div>
      <ErrorNote error={create.error} />
    </div>
  )
}

// ---- UpSet ------------------------------------------------------------------

interface Combo { id: string; label: string; sourceNames: string[]; degree: number; count: number; ids: string[] }
interface UpsetResult { mode: string; modeLabel: string; sets: { name: string; size: number }[]; combinations: Combo[]; organismTaxId: number; idFormat: string }

function Upset({ lists, onClose, onCreated, title }: P) {
  const [picked, setPicked] = useState<string[]>(() => lists.slice(0, 2).map((l) => l.id))
  const [mode, setMode] = useState<'intersection' | 'distinct' | 'union'>('distinct')
  const [res, setRes] = useState<UpsetResult | null>(null)
  const [active, setActive] = useState<Combo | null>(null)
  const [err, setErr] = useState<unknown>(null)
  const first = lists.find((l) => l.id === picked[0])
  const compatible = (l: AnalyteList) => !first || (l.organismTaxId === first.organismTaxId && l.idFormat === first.idFormat && l.analyteType === first.analyteType)
  // Lists can arrive after the dialog opens (e.g. the UpSet example creates two):
  // pre-select the two newest compatible lists until the user has two picked.
  useEffect(() => {
    if (picked.length >= 2 || lists.length < 2) return
    const base = lists[0]
    const same = lists.filter((l) => l.organismTaxId === base.organismTaxId && l.idFormat === base.idFormat && l.analyteType === base.analyteType)
    if (same.length >= 2) setPicked(same.slice(0, 2).map((l) => l.id))
  }, [lists, picked.length])
  useEffect(() => {
    if (picked.length < 2) { setRes(null); return }
    setErr(null)
    api<UpsetResult>('/inputs/upset', { body: { listIds: picked, mode } }).then((r) => { setRes(r); setActive(r.combinations[0] ?? null) }).catch(setErr)
  }, [picked, mode])
  const max = Math.max(1, ...(res?.combinations.map((c) => c.count) ?? [1]))
  return (
    <Modal title={title} onClose={onClose} size="xl" subtitle="Pick 2–6 lists with the same organism and ID type, then click a bar to use that combination.">
      <div className="grid-two">
        <div>
          <h3>Lists</h3>
          <ul className="plain-list">
            {lists.map((l) => (
              <li key={l.id}>
                <label className={`check${!compatible(l) && !picked.includes(l.id) ? ' muted' : ''}`}>
                  <input type="checkbox" checked={picked.includes(l.id)} disabled={!compatible(l) && !picked.includes(l.id)}
                    onChange={() => setPicked((s) => (s.includes(l.id) ? s.filter((x) => x !== l.id) : s.length >= 6 ? s : [...s, l.id]))} />
                  {l.name} <span className="muted small">({l.size})</span>
                </label>
              </li>
            ))}
          </ul>
          <Field label="Show">
            <Segmented label="Set operation" value={mode} onChange={setMode}
              options={[{ value: 'distinct', label: 'Only in' }, { value: 'intersection', label: 'Shared by' }, { value: 'union', label: 'Union' }]} />
          </Field>
        </div>
        <div>
          <ErrorNote error={err} />
          {!res && <p className="muted">Select at least two lists.</p>}
          {res && (
            <div className="upset" role="list">
              {res.combinations.map((c) => (
                <button key={c.id} role="listitem" className={`upset-row${active?.id === c.id ? ' active' : ''}`} onClick={() => setActive(c)}>
                  <span className="upset-dots" aria-hidden="true">
                    {res.sets.map((s) => <span key={s.name} className={c.sourceNames.includes(s.name) ? 'on' : ''} />)}
                  </span>
                  <span className="upset-label">{c.label}</span>
                  <span className="upset-bar"><span style={{ width: `${(c.count / max) * 100}%` }} /></span>
                  <span className="upset-count">{c.count}</span>
                </button>
              ))}
              <p className="muted small">Dots, left to right: {res.sets.map((s) => s.name).join(', ')}</p>
              <div className="row wrap">
                <span className="muted small">All combinations:</span>
                <CopyButton label="Copy table" text={['combination\tcount\tids', ...res.combinations.map((c) => `${c.label}\t${c.count}\t${c.ids.join(',')}`)].join('\n')} />
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => saveText(['combination\tlists\tcount\tids', ...res.combinations.map((c) => `${c.label}\t${c.sourceNames.join(' + ')}\t${c.count}\t${c.ids.join(',')}`)].join('\n'), `flame_upset_${res.mode}.tsv`, 'text/tab-separated-values')}>Download (TSV)</button>
              </div>
            </div>
          )}
        </div>
      </div>
      {active && res && (
        <>
          <p className="small"><strong>{active.label}</strong>: {active.ids.slice(0, 30).join(', ')}{active.count > 30 ? ` … (+${active.count - 30})` : ''}</p>
          <div className="row wrap">
            <span className="muted small">{plural(active.count, 'ID')}:</span>
            <CopyButton label="Copy (one per line)" text={active.ids.join('\n')} />
            <CopyButton label="Copy (comma-separated)" text={active.ids.join(', ')} />
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => saveText(active.ids.join('\n'), `${active.label.replace(/[^\w.-]+/g, '_').slice(0, 60)}.txt`)}>Download (.txt)</button>
          </div>
          <SaveList ids={active.ids.slice(0, 3000)} defaultName={active.label.slice(0, 80)} organism={res.organismTaxId} idFormat={res.idFormat}
            showOrganism={false} showFormat={false} meta={{ method: 'upset', mode: res.mode, sourceLists: active.sourceNames }} onCreated={onCreated} />
        </>
      )}
    </Modal>
  )
}

export function SimpleTable({ rows, max = 100 }: { rows: Record<string, unknown>[]; max?: number }) {
  if (!rows.length) return null
  const cols = Object.keys(rows[0]).filter((c) => !/url$/i.test(c))
  return (
    <div className="table-scroll">
      <table className="table">
        <thead><tr>{cols.map((c) => <th key={c}>{c}</th>)}</tr></thead>
        <tbody>
          {rows.slice(0, max).map((r, i) => (
            <tr key={i}>{cols.map((c) => <td key={c}>{String(r[c] ?? '')}</td>)}</tr>
          ))}
        </tbody>
      </table>
      {rows.length > max && <p className="muted small">Showing {max} of {rows.length} rows.</p>}
    </div>
  )
}

export { Spinner }

// ---------------------------------------------------------------------------
// Gene expression matrix: clusters, gene map, two-group comparison
// ---------------------------------------------------------------------------
interface ExprGene { id: string; cluster: number; x: number; y: number; sd: number }
interface ExprOverview {
  samples: string[]; logged: boolean; totalGenes: number; shownGenes: number; dropped: number
  genes: ExprGene[]; heatmap: { genes: string[]; clusters: number[]; z: number[][] }; clusterSizes: Record<string, number>; variance: number[]
}

function ExpressionInput({ example, onClose, onCreated, title, onOpenVolcano }: P) {
  const [text, setText] = useState('')
  const [fileName, setFileName] = useState('')
  const [topN, setTopN] = useState(1000)
  const [k, setK] = useState(6)
  const [logMode, setLogMode] = useState<'auto' | 'yes' | 'no'>('auto')
  const [ov, setOv] = useState<ExprOverview | null>(null)
  const [view, setView] = useState<'clusters' | 'map' | 'groups'>('clusters')
  const [sel, setSel] = useState<Set<string>>(new Set())
  const [groups, setGroups] = useState<Record<string, 'A' | 'B' | ''>>({})
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [note, setNote] = useState<string | null>(null)
  const [organism, setOrganism] = useState(9606)
  const [idFormat, setIdFormat] = useState('USERINPUT')
  const pm = usePaletteMode()
  const ex = useExample('expression', example, (t, f) => { setText(t); setFileName(f ?? '') })
  const log = logMode === 'auto' ? null : logMode === 'yes'

  useEffect(() => {
    if (!text.trim()) return
    setErr(null); setBusy(true)
    api<ExprOverview>('/inputs/expression/overview', { body: { text, fileName, topN, k, log } })
      .then((r) => {
        setOv(r); setSel(new Set())
        // Guess two groups from sample names (e.g. control_1 / treated_1).
        const stem = (s: string) => s.replace(/[_.\- ]?(rep)?\d+$/i, '')
        const stems = [...new Set(r.samples.map(stem))]
        setGroups(Object.fromEntries(r.samples.map((s) => [s, stems.length === 2 ? (stem(s) === stems[1] ? 'A' : 'B') : ''])))
      })
      .catch((e) => { setOv(null); setErr(e) })
      .finally(() => setBusy(false))
  }, [text, fileName, topN, k, log])

  const clusterColor = (c: number) => categorical(pm)[(c - 1) % categorical(pm).length]
  const pickCluster = (c: number, add: boolean) => {
    const ids = ov!.genes.filter((g) => g.cluster === c).map((g) => g.id)
    setSel((cur) => (add ? new Set([...cur, ...ids]) : new Set(ids)))
  }
  const runDe = () => {
    const A = Object.entries(groups).filter(([, g]) => g === 'A').map(([s]) => s)
    const B = Object.entries(groups).filter(([, g]) => g === 'B').map(([s]) => s)
    setBusy(true); setErr(null)
    api<{ text: string; fileName: string; significant: number; n: number; note: string }>('/inputs/expression/de', { body: { text, fileName, groupA: A, groupB: B, log } })
      .then((r) => {
        setNote(`${r.significant} of ${r.n} genes differ between the groups (BH-adjusted p ≤ 0.05). ${r.note}`)
        if (onOpenVolcano) onOpenVolcano(r.text, r.fileName)
      })
      .catch(setErr).finally(() => setBusy(false))
  }

  return (
    <Modal title={title} size="xl" onClose={onClose}
      subtitle="One row per gene (first column: gene ID), one numeric column per sample. Counts or intensities are log2-transformed automatically.">
      <div className="stack">
        <div className="row wrap">
          <FileDrop accept=".txt,.csv,.tsv,.tab" maxBytes={2 * 1024 * 1024} onText={(t, n) => { setText(t); setFileName(n) }} />
          <button className="btn btn-ghost btn-sm" onClick={ex.load} disabled={ex.loading}>{ex.loading ? 'Loading…' : 'Load example'}</button>
          <a className="btn btn-ghost btn-sm" href="/api/inputs/examples/expression/file">Example file</a>
        </div>
        {!text && <textarea className="input mono" rows={5} placeholder={'gene\tcontrol_1\tcontrol_2\ttreated_1\ttreated_2\nTP53\t812\t790\t1530\t1610'} onChange={(e) => setText(e.target.value)} />}
        <ErrorNote error={err} />
        {busy && <Spinner label="Analysing the matrix" />}
        {ov && (
          <>
            <div className="row wrap small">
              <span><strong>{ov.totalGenes.toLocaleString()}</strong> genes × <strong>{ov.samples.length}</strong> samples{ov.dropped ? ` (${ov.dropped} rows skipped)` : ''} · {ov.logged ? 'log2(x + 1) transformed' : 'used as given'}</span>
              <label>Log2 <select className="input input-sm" value={logMode} onChange={(e) => setLogMode(e.target.value as 'auto' | 'yes' | 'no')}><option value="auto">auto</option><option value="yes">yes</option><option value="no">no</option></select></label>
              <label>Most variable <select className="input input-sm" value={topN} onChange={(e) => setTopN(Number(e.target.value))}>{[200, 500, 1000, 2000, 3000].map((n) => <option key={n} value={n}>{n} genes</option>)}</select></label>
              <label>Clusters <input className="input input-sm input-narrow" type="number" min={2} max={20} value={k} onChange={(e) => setK(Number(e.target.value))} /></label>
            </div>
            <Segmented label="View" value={view} onChange={setView}
              options={[{ value: 'clusters', label: 'Co-expression clusters' }, { value: 'map', label: 'Gene map (PCA)' }, { value: 'groups', label: 'Compare two groups' }]} />
            {view === 'clusters' && (
              <div className="expr-clusters">
                <Plot height={Math.min(560, 120 + ov.heatmap.genes.length * 0.5)} data={[{
                  type: 'heatmap', z: ov.heatmap.z, x: ov.samples, y: ov.heatmap.genes, colorscale: 'RdBu', reversescale: true, zmid: 0, zmin: -2.5, zmax: 2.5,
                  customdata: ov.heatmap.genes.map((_, i) => ov.samples.map(() => `c${ov.heatmap.clusters[i]}`)) as never,
                  hovertemplate: '%{y} · %{x}<br>z = %{z}<extra></extra>', colorbar: { title: { text: 'z-score' } },
                } as never]}
                  layout={{ yaxis: { showticklabels: false, autorange: 'reversed' }, margin: { l: 20, r: 20, t: 10, b: 90 } }}
                  onSelect={(keys, add) => { const c = Number((keys[0] ?? '').slice(1)); if (c) pickCluster(c, add) }} />
                <div className="stack-sm">
                  <p className="muted small">Genes with similar profiles across samples, clustered by correlation. Click a cluster (or the heatmap) to select its genes; Shift adds.</p>
                  {Object.entries(ov.clusterSizes).map(([c, n]) => (
                    <button key={c} type="button" className="example-mini" onClick={(e) => pickCluster(Number(c), e.shiftKey)}>
                      <i className="swatch" style={{ background: clusterColor(Number(c)) }} />
                      <span><strong>Cluster {c}</strong><span className="muted small">{n} genes</span></span>
                    </button>
                  ))}
                </div>
              </div>
            )}
            {view === 'map' && (
              <Plot height={460} dragmode="lasso" onSelect={(ids, add) => setSel((cur) => (add ? new Set([...cur, ...ids]) : new Set(ids)))}
                data={Object.keys(ov.clusterSizes).map((c) => {
                  const g = ov.genes.filter((x) => x.cluster === Number(c))
                  return { type: 'scatter', mode: 'markers', name: `Cluster ${c}`, x: g.map((p) => p.x), y: g.map((p) => p.y), customdata: g.map((p) => p.id), text: g.map((p) => p.id),
                    hovertemplate: '%{text}<extra>%{fullData.name}</extra>',
                    marker: { size: g.map((p) => (sel.has(p.id) ? 9 : 6)), color: clusterColor(Number(c)), opacity: 0.8, line: { width: g.map((p) => (sel.has(p.id) ? 1.5 : 0)), color: '#111827' } } }
                })}
                layout={{ xaxis: { title: { text: `PC1 (${ov.variance[0]}%)` } }, yaxis: { title: { text: `PC2 (${ov.variance[1] ?? 0}%)` } }, legend: { orientation: 'h', y: -0.2 } }} />
            )}
            {view === 'groups' && (
              <div className="stack-sm">
                <p className="muted small">Put samples into group A and group B (at least two each). Flame tests every gene (Welch t-test, Benjamini–Hochberg) and opens the result in the volcano selector, where you pick up- and down-regulated genes or save all genes as a ranked list for GSEA.</p>
                <table className="table compact">
                  <thead><tr><th>Sample</th><th>Group A</th><th>Group B</th><th>Not used</th></tr></thead>
                  <tbody>{ov.samples.map((s) => (
                    <tr key={s}><td>{s}</td>
                      {(['A', 'B', ''] as const).map((g) => (
                        <td key={g || 'none'}><input type="radio" name={`grp-${s}`} aria-label={`${s} in ${g ? `group ${g}` : 'no group'}`} checked={(groups[s] ?? '') === g} onChange={() => setGroups({ ...groups, [s]: g })} /></td>
                      ))}
                    </tr>
                  ))}</tbody>
                </table>
                <div className="row between wrap">
                  <span className="small">A = {Object.values(groups).filter((g) => g === 'A').length} samples, B = {Object.values(groups).filter((g) => g === 'B').length} samples · log2FC = A − B</span>
                  <button className="btn btn-primary" disabled={busy || Object.values(groups).filter((g) => g === 'A').length < 2 || Object.values(groups).filter((g) => g === 'B').length < 2} onClick={runDe}>Compare and open volcano</button>
                </div>
                {note && <Note kind="info">{note}</Note>}
              </div>
            )}
            {view !== 'groups' && (
              <>
                <div className="row wrap">
                  <span className="small"><strong>{plural(sel.size, 'gene')}</strong> selected</span>
                  {sel.size > 0 && <button className="btn btn-ghost btn-sm" onClick={() => setSel(new Set())}>Clear</button>}
                  {sel.size > 0 && <CopyButton label="Copy" text={[...sel].join('\n')} />}
                </div>
                {sel.size > 0 && (
                  <SaveList ids={[...sel].slice(0, 3000)} defaultName={`Expression ${view === 'map' ? 'selection' : 'cluster'}`} organism={organism} setOrganism={setOrganism}
                    idFormat={idFormat} setIdFormat={setIdFormat} meta={{ method: 'expression', view }} onCreated={onCreated} />
                )}
              </>
            )}
          </>
        )}
      </div>
    </Modal>
  )
}

// ---------------------------------------------------------------------------
// microRNAs -> predicted target genes (miRDB via MSigDB)
// ---------------------------------------------------------------------------
interface MirnaResult {
  organismTaxId: number; orthology: boolean; source: string; totalTargets: number; minMirnas: number
  mirnas: { input: string; matched: string[]; targets: number }[]; unmatched: string[]
  genes: { gene: string; mirnas: string[]; count: number }[]
}

function MirnaInput({ example, onClose, onCreated, title }: P) {
  const [text, setText] = useState('')
  const [organism, setOrganism] = useState(9606)
  const [res, setRes] = useState<MirnaResult | null>(null)
  const [minMirnas, setMinMirnas] = useState(2)
  const [only, setOnly] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const pm = usePaletteMode()
  const ex = useExample('mirna', example, (t) => setText(t))
  const run = () => {
    setBusy(true); setErr(null)
    api<MirnaResult>('/inputs/mirna/targets', { body: { text, organismTaxId: organism } })
      .then((r) => { setRes(r); setOnly(null); setMinMirnas(r.mirnas.filter((m) => m.targets).length > 2 ? 2 : 1) })
      .catch(setErr).finally(() => setBusy(false))
  }
  useEffect(() => { if (example && text) run() // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [text])
  const chosen = res ? res.genes.filter((g) => (only ? g.mirnas.includes(only) : g.count >= minMirnas)) : []
  const maxCount = res ? Math.max(1, ...res.genes.map((g) => g.count)) : 1
  return (
    <Modal title={title} size="xl" onClose={onClose}
      subtitle="Start from microRNAs — for example those changed in your experiment — and analyse the genes they are predicted to regulate.">
      <div className="stack">
        <div className="two-col-tight">
          <div className="stack-sm">
            <Field label="microRNAs" hint="miRBase names, one per line: hsa-miR-21-5p, hsa-let-7a-5p… A name without -5p/-3p (miR-21) uses both arms.">
              <textarea className="input mono" rows={8} value={text} onChange={(e) => setText(e.target.value)} placeholder={'hsa-miR-21-5p\nhsa-miR-155-5p\nhsa-let-7a-5p'} />
            </Field>
            <div className="row wrap">
              <button className="btn btn-ghost btn-sm" onClick={ex.load} disabled={ex.loading}>{ex.loading ? 'Loading…' : 'Load example'}</button>
              <a className="btn btn-ghost btn-sm" href="/api/inputs/examples/mirna/file">Example file</a>
            </div>
          </div>
          <div className="stack-sm">
            <OrganismPicker value={organism} onChange={setOrganism} />
            <button className="btn btn-primary" disabled={!text.trim() || busy} onClick={run}>{busy ? 'Finding targets…' : 'Find target genes'}</button>
            <p className="muted small">Targets are miRDB v6 predictions with MirTarget score &gt; 80 (MSigDB C3 MIR:MIRDB). For organisms other than human, MSigDB maps the human predictions through orthologs.</p>
          </div>
        </div>
        <ErrorNote error={err} />
        {res && (
          <>
            {res.unmatched.length > 0 && <Note kind="warning">Not found in miRDB: {res.unmatched.join(', ')}. Check the names (e.g. hsa-miR-21-5p).</Note>}
            {res.orthology && <Note kind="info">Targets for this organism are human miRDB predictions mapped through orthologs.</Note>}
            <div className="chart-grid">
              <div className="chart-card">
                <h3>Targets per microRNA</h3>
                <Plot height={Math.max(200, 26 * res.mirnas.length + 60)}
                  data={[{ type: 'bar', orientation: 'h', y: [...res.mirnas].reverse().map((m) => m.input), x: [...res.mirnas].reverse().map((m) => m.targets),
                    customdata: [...res.mirnas].reverse().map((m) => m.input), marker: { color: [...res.mirnas].reverse().map((m) => (m.input === only ? '#e11d48' : roleColors(pm).accent)) },
                    hovertemplate: '%{y}: %{x} targets — click to use only its targets<extra></extra>' }]}
                  layout={{ margin: { l: 140, r: 10, t: 6, b: 30 }, yaxis: { automargin: true } }}
                  onSelect={(keys) => setOnly(keys[0] && keys[0] !== only ? keys[0] : null)} />
              </div>
              <div className="chart-card">
                <h3>Targeted by how many of your microRNAs</h3>
                <Plot height={260} data={[{ type: 'bar', x: Array.from({ length: maxCount }, (_, i) => i + 1), y: Array.from({ length: maxCount }, (_, i) => res.genes.filter((g) => g.count === i + 1).length),
                  customdata: Array.from({ length: maxCount }, (_, i) => String(i + 1)), marker: { color: Array.from({ length: maxCount }, (_, i) => (i + 1 >= minMirnas && !only ? '#8b5cf6' : '#d4d4d8')) },
                  hovertemplate: '%{y} genes targeted by %{x} microRNAs — click to keep genes hit at least this often<extra></extra>' }]}
                  layout={{ xaxis: { title: { text: 'microRNAs targeting the gene' }, dtick: 1 }, yaxis: { title: { text: 'Genes' }, type: 'log' } }}
                  onSelect={(keys) => { if (keys[0]) { setOnly(null); setMinMirnas(Number(keys[0])) } }} />
              </div>
            </div>
            <div className="row wrap">
              {only ? (
                <span className="small">Targets of <strong>{only}</strong> <button className="link-btn" onClick={() => setOnly(null)}>(use all microRNAs)</button></span>
              ) : (
                <label className="small">Keep genes targeted by at least <input className="input input-sm input-narrow" type="number" min={1} max={maxCount} value={minMirnas} onChange={(e) => setMinMirnas(Math.max(1, Number(e.target.value)))} /> of your microRNAs</label>
              )}
              <span className="small"><strong>{plural(chosen.length, 'gene')}</strong> of {res.totalTargets.toLocaleString()} targets</span>
              <CopyButton label="Copy genes" text={chosen.map((g) => g.gene).join('\n')} />
            </div>
            <div className="table-scroll" style={{ maxHeight: 220 }}>
              <table className="table compact">
                <thead><tr><th>Gene</th><th>microRNAs</th><th>Targeted by</th></tr></thead>
                <tbody>{chosen.slice(0, 300).map((g) => <tr key={g.gene}><td>{g.gene}</td><td>{g.count}</td><td className="small">{g.mirnas.join(', ')}</td></tr>)}</tbody>
              </table>
            </div>
            {chosen.length > 0 && (
              <SaveList ids={chosen.slice(0, 3000).map((g) => g.gene)} defaultName={only ? `${only} targets` : `miRNA targets (≥${minMirnas} miRNAs)`}
                organism={res.organismTaxId} idFormat="USERINPUT" showOrganism={false} showFormat={false}
                meta={{ method: 'mirna', mirnas: res.mirnas.map((m) => m.input), minMirnas: only ? null : minMirnas, only, source: res.source }} onCreated={onCreated} />
            )}
          </>
        )}
      </div>
    </Modal>
  )
}
