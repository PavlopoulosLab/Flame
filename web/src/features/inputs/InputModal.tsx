import { useEffect, useMemo, useState } from 'react'
import { api } from '../../api/client'
import { useCreateList } from '../../api/hooks'
import type { AnalyteList } from '../../api/types'
import { Plot } from '../../components/charts'
import { ErrorNote, Field, FileDrop, IdFormatSelect, Modal, Note, OrganismPicker, Segmented, Spinner } from '../../components/ui'
import { plural } from '../../lib/format'
import { METHODS, type InputKind } from './MethodPicker'

interface Props {
  kind: InputKind
  example?: boolean
  lists: AnalyteList[]
  onClose: () => void
  onCreated: (list: AnalyteList) => void
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
        <span className="muted small">{plural(ids.length, 'ID')} will be saved</span>
        <button
          className="btn btn-primary"
          disabled={!ids.length || !name.trim() || create.isPending}
          onClick={() =>
            create.mutate(
              { name: name.trim(), ids, organismTaxId: organism, idFormat, meta, uniqueName: false },
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
  const [organism, setOrganism] = useState(9606)
  const [idFormat, setIdFormat] = useState('USERINPUT')
  const [parsed, setParsed] = useState<{ ids: string[]; unique: number; duplicates: number; invalid: number; invalidExamples: string[] } | null>(null)
  const [err, setErr] = useState<unknown>(null)
  const ex = useExample('upload', example, (t) => setText(t))
  useEffect(() => {
    if (!text.trim()) {
      setParsed(null)
      return
    }
    const h = setTimeout(() => {
      api<typeof parsed>('/inputs/upload/parse', { body: { text, skipFirstRow: skip } }).then(setParsed).catch(setErr)
    }, 300)
    return () => clearTimeout(h)
  }, [text, skip])
  return (
    <Modal title={title} onClose={onClose} size="lg" subtitle="One ID per line, or separated by commas, tabs or semicolons.">
      <div className="row between">
        <span className="muted small">Gene symbols, Ensembl, Entrez, UniProt and 15 other formats are accepted.</span>
        <button className="btn btn-ghost btn-sm" onClick={ex.load} disabled={ex.loading}>Load example</button>
      </div>
      <textarea className="input mono" rows={10} value={text} onChange={(e) => setText(e.target.value)} placeholder={'TP53\nBRCA1\nEGFR'} aria-label="IDs" />
      <FileDrop accept=".txt,.csv,.tsv" onText={(t) => setText(t)} />
      <label className="check"><input type="checkbox" checked={skip} onChange={(e) => setSkip(e.target.checked)} /> First row is a header</label>
      {parsed && (
        <Note kind={parsed.invalid ? 'warning' : 'success'}>
          {plural(parsed.unique, 'unique ID')}
          {parsed.duplicates ? ` · ${parsed.duplicates} duplicates removed` : ''}
          {parsed.invalid ? ` · ${parsed.invalid} skipped (e.g. ${parsed.invalidExamples.slice(0, 3).join(', ')})` : ''}
        </Note>
      )}
      <ErrorNote error={err} />
      <SaveList ids={parsed?.ids ?? []} defaultName="My genes" organism={organism} setOrganism={setOrganism}
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

function TableInput({ kind, example, onClose, onCreated, title }: P) {
  const isVolcano = kind === 'volcano'
  const [text, setText] = useState('')
  const [fileName, setFileName] = useState('')
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
        up: { name: 'Up-regulated', color: '#dc2626' }, down: { name: 'Down-regulated', color: '#2563eb' }, ns: { name: 'Not significant', color: '#a1a1aa' },
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
  }, [points, isVolcano, sel, mapping.color])

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
        </>
      )}
    </Modal>
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
            </div>
          )}
        </div>
      </div>
      {active && res && (
        <>
          <p className="small"><strong>{active.label}</strong>: {active.ids.slice(0, 30).join(', ')}{active.count > 30 ? ` … (+${active.count - 30})` : ''}</p>
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
