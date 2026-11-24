import { Fragment, useEffect, useMemo, useRef, useState } from 'react'
import { API_BASE, api, saveText } from '../../api/client'
import { useCreateList } from '../../api/hooks'
import type { AnalyteList } from '../../api/types'
import { Plot } from '../../components/charts'
import { BusyBar, CopyButton, ErrorNote, Field, FileDrop, IdFormatSelect, Modal, ModalPaletteContext, Note, OrganismPicker, Segmented, Spinner } from '../../components/ui'
import { plural, tsvCell } from '../../lib/format'
import { categorical, PaletteScope, roleColors, usePaletteKey, usePaletteMode } from '../../lib/palette'
import { PaletteSelect } from '../../components/PaletteSelect'
import { METHODS, type InputKind } from './MethodPicker'
import { FetchInput } from './FetchInput'
import { saveXlsx } from '../../lib/xlsx'
import { usePlainSelection } from '../../lib/imageExport'
import { ExportImageButton } from '../../components/ExportImage'
import { htmlSource } from '../../components/viz'
import { TcgaInput } from './TcgaInput'
import { SurvivalPanel, type ClinicalTable } from './SurvivalPanel'
import { PdfAnnotate } from './PdfAnnotate'
import { NutritionInput } from './NutritionInput'
import { BenchInput } from './BenchInput'
const NO_SET = new Set<string>()
const NO_BARS: string[] = []

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
  /** Expression input: open on a given view (e.g. 'activity'). */
  initialView?: string
  /** Combine lists: the lists ticked in the lists panel. */
  initialListIds?: string[]
}

export function InputModal(props: Props) {
  return <PaletteScope id={`input-${props.kind}`}><ModalPaletteContext.Provider value={false}><InputModalBody {...props} /></ModalPaletteContext.Provider></PaletteScope>
}

function InputModalBody(props: Props) {
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
    case 'disease':
    case 'pubmed':
    case 'geo':
    case 'clinvar':
      return <FetchInput mode={props.kind} title={title} example={props.example} onClose={props.onClose} onCreated={props.onCreated} />
    case 'bench':
      return <BenchInput title={title} example={props.example} onClose={props.onClose} onCreated={props.onCreated} />
    case 'nutrition':
      return <NutritionInput title={title} example={props.example} onClose={props.onClose} onCreated={props.onCreated} />
    case 'tcga':
      return <TcgaInput title={title} example={props.example} onClose={props.onClose} onCreated={props.onCreated} />
    case 'variants':
      return null // opens the Variants page instead of a dialog (see Workspace)
  }
}

type P = Props & { title: string }

function useExample(kind: string, enabled: boolean | undefined, apply: (text: string, fileName?: string) => void) {
  const [loading, setLoading] = useState(false)
  const [source, setSource] = useState<string | null>(null)
  const load = () => {
    setLoading(true)
    api<{ text: string; fileName?: string; source?: string }>(`/inputs/examples/${kind}`)
      .then((ex) => { apply(ex.text, ex.fileName); setSource(ex.source ?? null) })
      .finally(() => setLoading(false))
  }
  useEffect(() => {
    if (enabled) load()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])
  return { load, loading, source }
}

/** Where the loaded example data come from. */
function ExampleSource({ text, loading = false }: { text: string | null; loading?: boolean }) {
  if (loading) return <BusyBar label="Loading the example" expect={6} />
  return text ? <p className="example-source"><strong>Example data:</strong> {text}</p> : null
}

/** Shared "save as list" footer: name, organism, ID format. */
export function SaveList({
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
      {create.isPending && <BusyBar label="Saving the list and checking its IDs" expect={8} />}
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
          {create.isPending ? 'Saving…' : 'Save list'}
        </button>
      </div>
    </div>
  )
}

// ---- Paste / upload ---------------------------------------------------------

const MAX_PASTE_CHARS = 1024 * 1024

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
    // Larger than the server accepts: say so here, with what to do, instead of sending it.
    if (text.length > MAX_PASTE_CHARS) {
      setParsed(null)
      setErr(new Error(`This is ${(text.length / 1024 / 1024).toFixed(1)} MB of text; the limit is 1 MB. A list holds up to 3,000 IDs (30,000 with scores), so paste only the column of gene IDs. For a full expression table use “From expression data”, and for differential-expression results “From a volcano plot”.`))
      return
    }
    setErr(null)
    const h = setTimeout(() => {
      api<typeof parsed>('/inputs/upload/parse', { body: { text, skipFirstRow: skip, ranked } }).then(setParsed).catch((e) => { setParsed(null); setErr(e) })
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
          {parsed.invalid ? ` · ${parsed.invalid} skipped (e.g. ${[parsed.invalidExamples ?? []].flat().slice(0, 3).join(', ')})` : ''}
        </Note>
      )}
      <ExampleSource text={ex.source} loading={ex.loading} />
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
      {busy && <BusyBar label="Mapping SNPs to genes with g:SNPense" expect={10} />}
      <ExampleSource text={ex.source} loading={ex.loading} />
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
/** One protein found in the text, with every name that pointed to it (two names can map to the same protein). */
interface ExtractHit { id: string; idUrl?: string; names: { name: string; ambiguous: boolean }[]; ambiguous: boolean; preselect: boolean; order: number }
type HitSortKey = 'order' | 'name' | 'id' | 'note'
/** Sort weight of the Note column: duplicates first, then ambiguous, then clear matches. */
const noteRank = (h: ExtractHit) => (h.names.length > 1 ? 0 : h.ambiguous ? 1 : 2)

/** Group the tagger's name–ID matches by ID, so each protein is listed, counted and saved once. */
function groupById(rows: ExtractRow[]): ExtractHit[] {
  const byId = new Map<string, ExtractHit>()
  for (const r of rows) {
    if (!r.id) continue
    const h = byId.get(r.id) ?? { id: r.id, idUrl: r.idUrl, names: [], ambiguous: true, preselect: false, order: byId.size }
    if (!h.names.some((n) => n.name === r.name)) h.names.push({ name: r.name, ambiguous: !!r.ambiguous })
    // a protein is ambiguous only if every name that found it is ambiguous
    if (!r.ambiguous) h.ambiguous = false
    if (r.selected !== false && !r.ambiguous) h.preselect = true
    byId.set(r.id, h)
  }
  return [...byId.values()]
}

function TextMining({ example, onClose, onCreated, title }: P) {
  const [text, setText] = useState('')
  const [organism, setOrganism] = useState(9606)
  const [rows, setRows] = useState<ExtractRow[] | null>(null)
  const [hits, setHits] = useState<ExtractHit[]>([])
  const [chosen, setChosen] = useState<Set<string>>(new Set())
  const [sort, setSort] = useState<{ key: HitSortKey; dir: 1 | -1 }>({ key: 'order', dir: 1 })
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const ex = useExample('textmining', example, (t) => setText(t))
  const extract = () => {
    setBusy(true)
    setErr(null)
    api<{ rows: ExtractRow[] }>('/inputs/textmining/extract', { body: { text, organismTaxId: organism } })
      .then((r) => {
        const g = groupById(r.rows)
        setRows(r.rows)
        setHits(g)
        setChosen(new Set(g.filter((h) => h.preselect).map((h) => h.id)))
      })
      .catch(setErr)
      .finally(() => setBusy(false))
  }
  const ids = hits.filter((h) => chosen.has(h.id)).map((h) => h.id)
  const sorted = [...hits].sort((a, b) => {
    const v = (h: ExtractHit): number | string => sort.key === 'name' ? h.names[0].name.toLowerCase() : sort.key === 'id' ? h.id : sort.key === 'note' ? noteRank(h) : h.order
    const x = v(a), y = v(b)
    return (x < y ? -1 : x > y ? 1 : a.order - b.order) * sort.dir
  })
  const sortBy = (key: HitSortKey) => setSort((s) => (s.key === key ? { key, dir: (s.dir * -1) as 1 | -1 } : { key, dir: 1 }))
  const th = (key: HitSortKey, label: string) => (
    <th aria-sort={sort.key === key ? (sort.dir === 1 ? 'ascending' : 'descending') : 'none'}>
      <button className="th-btn" onClick={() => sortBy(key)}>{label}{sort.key === key ? (sort.dir === 1 ? ' ▲' : ' ▼') : ''}</button>
    </th>
  )
  const duplicates = hits.filter((h) => h.names.length > 1).length
  const names = rows ? [...new Set(rows.map((r) => r.name).filter(Boolean))] : []
  const [mode, setMode] = useState<'text' | 'pdf'>('text')
  return (
    <Modal title={title} onClose={onClose} size="xl" subtitle="Names are recognised with the EXTRACT tagger (JensenLab).">
      <Segmented label="Source" value={mode} onChange={setMode} options={[{ value: 'text', label: 'Paste text' }, { value: 'pdf', label: 'PDF document' }]} />
      {mode === 'pdf' && <PdfAnnotate onCreated={onCreated} />}
      {mode === 'text' && (<>
      <div className="row between">
        <OrganismPicker value={organism} onChange={(t) => setOrganism(t)} label="Organism of the text" />
        <button className="btn btn-ghost btn-sm" onClick={ex.load}>Load example</button>
      </div>
      <textarea className="input" rows={7} value={text} maxLength={500000} onChange={(e) => setText(e.target.value)} placeholder="Paste an abstract or any text…" aria-label="Text" />
      <div className="row between">
        <span className="muted small">{text.length.toLocaleString()} characters</span>
        <button className="btn btn-secondary" disabled={text.trim().length < 3 || busy} onClick={extract}>{busy ? 'Finding genes…' : 'Find genes'}</button>
      </div>
      {busy && <BusyBar label="Finding gene and protein names with the EXTRACT tagger" expect={12} />}
      <ExampleSource text={ex.source} loading={ex.loading} />
        <ErrorNote error={err} />
      {rows && (
        <div className="two-col-tight">
          <div>
            <h3>Highlighted text</h3>
            <div className="annotated">{highlight(text, names)}</div>
          </div>
          <div>
            <div className="row between">
              <h3>{plural(hits.length, 'protein')}{rows.length !== hits.length && <span className="muted small"> from {plural(rows.length, 'name match', 'name matches')}</span>}</h3>
              <span className="row">
                <button className="btn btn-ghost btn-sm" onClick={() => setChosen(new Set(hits.map((h) => h.id)))}>Select all</button>
                <button className="btn btn-ghost btn-sm" onClick={() => setChosen(new Set())}>Clear</button>
              </span>
            </div>
            {duplicates > 0 && <p className="muted small">{plural(duplicates, 'protein')} found under more than one name {duplicates === 1 ? 'is' : 'are'} marked <span className="tm-flag tm-dup">duplicate</span> and saved once.</p>}
            <div className="table-scroll small-table">
              <table className="table">
                <thead><tr><th aria-label="Use" />{th('name', 'Name in the text')}{th('id', 'ID')}{th('note', 'Note')}</tr></thead>
                <tbody>
                  {sorted.map((h) => (
                    <tr key={h.id} className={h.ambiguous ? 'muted' : ''}>
                      <td><input type="checkbox" checked={chosen.has(h.id)} aria-label={`Use ${h.names.map((n) => n.name).join(', ')}`}
                        onChange={() => setChosen((s) => { const n = new Set(s); if (n.has(h.id)) n.delete(h.id); else n.add(h.id); return n })} /></td>
                      <td>{h.names.map((n) => n.name).join(', ')}</td>
                      <td>{h.idUrl && /^https?:\/\//.test(h.idUrl) ? <a href={h.idUrl} target="_blank" rel="noopener noreferrer">{h.id}</a> : h.id}</td>
                      <td className="tm-notes">
                        {h.names.length > 1 && <span className="tm-flag tm-dup" title={`${h.names.map((n) => n.name).join(' and ')} are the same protein: listed and saved once`}>duplicate ×{h.names.length}</span>}
                        {h.ambiguous && <span className="tm-flag tm-amb" title="The name matches several proteins: check before saving">ambiguous</span>}
                      </td>
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
      </>)}
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
  const [replacing, setReplacing] = useState(false)
  const [fileName, setFileName] = useState(initialFileName ?? '')
  const [prep, setPrep] = useState<Prepared | null>(null)
  const [mapping, setMapping] = useState<Record<string, string>>({})
  // Thresholds are typed as text; half-typed values ("0.", "") keep the last valid number.
  const [pText, setPText] = useState('0.05')
  const [lfcText, setLfcText] = useState('1')
  const lastP = useRef(0.05)
  const lastLfc = useRef(1)
  const pNum = Number(pText)
  const lfcNum = Number(lfcText)
  if (Number.isFinite(pNum) && pNum > 0 && pNum <= 1) lastP.current = pNum
  if (Number.isFinite(lfcNum) && lfcNum >= 0) lastLfc.current = lfcNum
  const p = lastP.current
  const lfc = lastLfc.current
  const pInvalid = !(Number.isFinite(pNum) && pNum > 0 && pNum <= 1)
  const lfcInvalid = !(Number.isFinite(lfcNum) && lfcNum >= 0)
  const fmtP = (v: number) => (v >= 0.001 ? String(Number(v.toPrecision(3))) : v.toExponential(1))
  const [points, setPoints] = useState<{ points: (VolcanoPoint | ReductionPoint)[]; plotted: number; total: number; counts?: Record<string, number>; colorType?: string } | null>(null)
  const [sel, setSel] = useState<Set<string>>(new Set())
  const selV = usePlainSelection(sel, NO_SET) // none while an image is exported without highlighting
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
  // Up / down / not significant, from the current cut-offs (same rule as the server: p < cut, |log2FC| > cut).
  const statusOf = (x: VolcanoPoint): 'up' | 'down' | 'ns' => (x.pvalue < p && x.logfc > lfc ? 'up' : x.pvalue < p && x.logfc < -lfc ? 'down' : 'ns')
  const liveCounts = useMemo(() => {
    if (!isVolcano || !points) return null
    const c = { up: 0, down: 0, significant: 0 }
    ;(points.points as VolcanoPoint[]).forEach((x) => { const s = statusOf(x); if (s !== 'ns') { c.significant++; c[s]++ } })
    return c
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [points, isVolcano, p, lfc])
  const maxY = useMemo(() => (isVolcano && points ? Math.max(1, ...(points.points as VolcanoPoint[]).map((x) => x.negLog10p)) : 1), [points, isVolcano])
  const yTop = Math.max(maxY, -Math.log10(p)) * 1.06
  // p-value ticks for the right axis: decades (thinned on tall plots) plus the current cut-off.
  const pTicks = useMemo(() => {
    const step = Math.max(1, Math.ceil(yTop / 8))
    const ticks: { y: number; label: string }[] = []
    for (let k = 0; k <= yTop; k += step) ticks.push({ y: k, label: k === 0 ? '1' : k <= 3 ? String(10 ** -k) : `1e-${k}` })
    const cut = -Math.log10(p)
    if (!ticks.some((t) => Math.abs(t.y - cut) < yTop / 30)) ticks.push({ y: cut, label: fmtP(p) })
    return ticks
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [yTop, p])
  const selectedIds = [...new Set([...sel].map((pid) => byId.get(pid)?.id).filter((x): x is string => !!x))]

  const plotData = useMemo(() => {
    if (!points) return []
    if (isVolcano) {
      const groups: Record<string, { name: string; color: string }> = {
        up: { name: 'Up-regulated', color: rc.up }, down: { name: 'Down-regulated', color: rc.down }, ns: { name: 'Not significant', color: rc.ns },
      }
      return Object.entries(groups).map(([k, g]) => {
        const pts = (points.points as VolcanoPoint[]).filter((x) => statusOf(x) === k)
        return {
          type: 'scatter' as const, mode: 'markers' as const, name: g.name,
          x: pts.map((x) => x.logfc), y: pts.map((x) => x.negLog10p), customdata: pts.map((x) => x.pointId),
          text: pts.map((x) => `${x.id}<br>log2FC ${x.logfc.toFixed(2)}<br>p ${x.pvalue.toExponential(2)}`),
          hoverinfo: 'text' as const,
          marker: { color: g.color, size: 6, opacity: 0.75, line: { width: pts.map((x) => (selV.has(x.pointId) ? 2 : 0)), color: '#f97316' } },
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
          line: { width: sub.map((x) => (selV.has(x.pointId) ? 2 : 0)), color: '#f97316' },
        },
      }
    })
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [points, isVolcano, selV, mapping.color, rc, p, lfc])

  const quick = (status: 'sig' | 'up' | 'down') => {
    const pts = (points?.points ?? []) as VolcanoPoint[]
    setSel(new Set(pts.filter((x) => (status === 'sig' ? statusOf(x) !== 'ns' : statusOf(x) === status)).map((x) => x.pointId)))
  }

  const roles = isVolcano
    ? [{ key: 'id', label: 'Gene ID column', numeric: false }, { key: 'logfc', label: 'log2 fold change', numeric: true }, { key: 'pvalue', label: 'p-value (or adjusted)', numeric: true }]
    : [{ key: 'id', label: 'Gene ID column', numeric: false }, { key: 'x', label: 'X axis', numeric: true }, { key: 'y', label: 'Y axis', numeric: true },
       { key: 'color', label: 'Colour (optional)', numeric: false, optional: true }, { key: 'size', label: 'Size (optional)', numeric: true, optional: true }]

  return (
    <Modal title={title} onClose={onClose} size="xl"
      subtitle={isVolcano && initialText ? 'Every gene of your expression table compared between group A and group B (Welch t-test, log2FC = A − B). Pick up- or down-regulated genes, or save all genes as a ranked list.' : isVolcano ? 'A table with a gene column, a log2 fold change column and a p-value column (CSV or TSV).' : 'A table with a gene column and two coordinate columns (CSV or TSV).'}>
      {prep && !replacing ? (
        // A table is loaded: Step 1 shrinks to one line, so the columns and the plot come first.
        <div className="row between wrap">
          <span className="small"><span className="muted">Table:</span> <strong>{initialText && text === initialText ? 'two-group comparison from your expression data' : fileName || 'pasted table'}</strong> · {plural(prep.rowCount, 'row')}</span>
          <button className="btn btn-ghost btn-sm" onClick={() => setReplacing(true)}>Replace table</button>
        </div>
      ) : (
        <>
          <div className="row between">
            <span className="muted small">Step 1 · Provide the table</span>
            <span className="row">
              <button className="btn btn-ghost btn-sm" onClick={() => { setReplacing(false); ex.load() }}>Load example</button>
              {prep && <button className="btn btn-ghost btn-sm" onClick={() => setReplacing(false)}>Keep current table</button>}
            </span>
          </div>
          <FileDrop accept=".csv,.tsv,.txt" onText={(t, n) => { setText(t); setFileName(n); setReplacing(false) }} />
          <details>
            <summary className="small">…or paste the table</summary>
            <textarea className="input mono" rows={6} value={text} onChange={(e) => { setText(e.target.value); setFileName('') }} aria-label="Table" />
          </details>
        </>
      )}
      <ExampleSource text={ex.source} loading={ex.loading} />
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
                <Field label="p-value below" hint={pInvalid ? 'Enter a number between 0 and 1 (e.g. 0.05 or 1e-3)' : 'Significance cut-off — updates the plot at once'}>
                  <div className="threshold-input">
                    <input className={`input${pInvalid ? ' invalid' : ''}`} inputMode="decimal" value={pText} onChange={(e) => setPText(e.target.value.trim())} aria-label="p-value cut-off" aria-invalid={pInvalid} />
                    <input type="range" min={-10} max={0} step={0.1} value={Math.max(-10, Math.log10(p))} aria-label="p-value cut-off (log scale)"
                      onChange={(e) => setPText(fmtP(10 ** Number(e.target.value)))} />
                  </div>
                </Field>
                <Field label="|log2FC| above" hint={lfcInvalid ? 'Enter a number of 0 or more' : 'Effect-size cut-off — updates the plot at once'}>
                  <div className="threshold-input">
                    <input className={`input${lfcInvalid ? ' invalid' : ''}`} inputMode="decimal" value={lfcText} onChange={(e) => setLfcText(e.target.value.trim())} aria-label="log2 fold-change cut-off" aria-invalid={lfcInvalid} />
                    <input type="range" min={0} max={5} step={0.1} value={Math.min(5, lfc)} aria-label="log2 fold-change cut-off (slider)" onChange={(e) => setLfcText(e.target.value)} />
                  </div>
                </Field>
              </>
            )}
          </div>
          <div className="row end"><button className="btn btn-secondary" onClick={draw} disabled={busy}>{busy ? 'Drawing…' : points ? 'Redraw plot' : 'Draw plot'}</button></div>
          {busy && <BusyBar label={`Reading ${prep.rowCount.toLocaleString()} rows and drawing the plot`} expect={6} />}
        </>
      )}
      {points && (
        <>
          <p className="muted small">Step 3 · Select genes: drag a box on the plot (or pick the lasso in the plot toolbar){isVolcano ? ', or use a quick selection' : ''}. Shift-click adds points.</p>
          {isVolcano && liveCounts && (
            <div className="row wrap">
              <button className="btn btn-secondary btn-sm" onClick={() => quick('sig')}>All significant ({liveCounts.significant})</button>
              <button className="btn btn-secondary btn-sm" onClick={() => quick('up')}>Up ({liveCounts.up})</button>
              <button className="btn btn-secondary btn-sm" onClick={() => quick('down')}>Down ({liveCounts.down})</button>
              <span className="muted small">p &lt; {fmtP(p)}, |log2FC| &gt; {lfc} · drag the red dotted lines to change the cut-offs</span>
              <button className="btn btn-ghost btn-sm" onClick={() => setSel(new Set())}>Clear</button>
            </div>
          )}
          {!isVolcano && <div className="row"><button className="btn btn-ghost btn-sm" onClick={() => setSel(new Set())}>Clear selection</button></div>}
          <Plot data={isVolcano ? [...plotData, { type: 'scatter' as const, mode: 'markers' as const, x: [0], y: [0], yaxis: 'y2', showlegend: false, hoverinfo: 'skip' as const, marker: { opacity: 0 } }] as never : plotData} height={420} dragmode="select"
            onShapesMoved={isVolcano ? (u) => {
              const y = u['shapes[0].y0'] ?? u['shapes[0].y1']
              if (y !== undefined && y > 0) setPText(fmtP(Math.min(1, 10 ** -y)))
              const x = u['shapes[1].x0'] ?? u['shapes[1].x1'] ?? u['shapes[2].x0'] ?? u['shapes[2].x1']
              if (x !== undefined) setLfcText(String(Math.round(Math.abs(x) * 100) / 100))
            } : undefined}
            layout={{ xaxis: { title: { text: isVolcano ? 'log2 fold change' : mapping.x } },
              yaxis: { title: { text: isVolcano ? '−log10 p-value' : mapping.y }, ...(isVolcano ? { range: [0, yTop] } : {}) },
              ...(isVolcano ? {
                // Right-hand axis: the same scale labelled with the original p-values.
                yaxis2: { overlaying: 'y', side: 'right', matches: 'y', range: [0, yTop], showgrid: false, zeroline: false,
                  tickvals: pTicks.map((t) => t.y), ticktext: pTicks.map((t) => t.label), title: { text: 'p-value' } },
                margin: { l: 60, r: 70, t: 20, b: 50 },
              } : {}),
              shapes: isVolcano ? [
                { type: 'line', xref: 'paper', x0: 0, x1: 1, y0: -Math.log10(p), y1: -Math.log10(p), line: { color: '#dc2626', dash: 'dot', width: 1 } },
                { type: 'line', yref: 'paper', y0: 0, y1: 1, x0: lfc, x1: lfc, line: { color: '#dc2626', dash: 'dot', width: 1 } },
                { type: 'line', yref: 'paper', y0: 0, y1: 1, x0: -lfc, x1: -lfc, line: { color: '#dc2626', dash: 'dot', width: 1 } },
              ] : [] }}
            onSelect={(ids, add) => setSel((s) => new Set(add ? [...s, ...ids] : ids))} />
          <p className="muted small">Plotted {points.plotted.toLocaleString()} of {points.total.toLocaleString()} rows · {plural(selectedIds.length, 'gene')} selected{selectedIds.length > 3000 ? ' (a list holds at most 3,000 IDs)' : ''}</p>
          <SelectedGenes isVolcano={isVolcano} colorLabel={mapping.color || undefined}
            points={[...sel].map((pid) => byId.get(pid)).filter((x): x is VolcanoPoint | ReductionPoint => !!x)}
            onRemove={(geneId) => setSel((cur) => new Set([...cur].filter((pid) => byId.get(pid)?.id !== geneId)))}
            onClear={() => setSel(new Set())} />
          <h3 className="save-heading">Save the selected genes as a list</h3>
          <SaveList ids={selectedIds.slice(0, 3000)} defaultName={isVolcano ? 'Volcano selection' : 'Cluster selection'} organism={organism}
            setOrganism={setOrganism} idFormat={idFormat} setIdFormat={setIdFormat} meta={{ method: kind }} onCreated={onCreated} />
          {isVolcano && <RankedFromVolcano points={points.points as VolcanoPoint[]} organism={organism} idFormat={idFormat} onCreated={onCreated} />}
        </>
      )}
    </Modal>
  )
}

/** The genes currently selected on the plot, so the user sees exactly what will be saved. */
function SelectedGenes({ points, isVolcano, colorLabel, onRemove, onClear }: {
  points: (VolcanoPoint | ReductionPoint)[]; isVolcano: boolean; colorLabel?: string; onRemove: (geneId: string) => void; onClear: () => void
}) {
  const MAX_ROWS = 300
  // One row per gene (a table can hold the same gene on several rows): the most significant one for a volcano.
  const rows = useMemo(() => {
    const best = new Map<string, VolcanoPoint | ReductionPoint>()
    points.forEach((pt) => {
      const prev = best.get(pt.id)
      if (!prev || (isVolcano && (pt as VolcanoPoint).pvalue < (prev as VolcanoPoint).pvalue)) best.set(pt.id, pt)
    })
    const out = [...best.values()]
    if (isVolcano) (out as VolcanoPoint[]).sort((a, b) => a.pvalue - b.pvalue)
    else out.sort((a, b) => a.id.localeCompare(b.id))
    return out
  }, [points, isVolcano])
  const up = isVolcano ? (rows as VolcanoPoint[]).filter((r) => r.logfc > 0).length : 0
  return (
    <section className="selected-genes" aria-label="Selected genes">
      <div className="row between">
        <h3>Selected genes <span className="count">{rows.length.toLocaleString()}</span>
          {isVolcano && rows.length > 0 && <span className="muted small"> · {up.toLocaleString()} up, {(rows.length - up).toLocaleString()} down</span>}</h3>
        {rows.length > 0 && (
          <span className="row">
            <CopyButton text={rows.map((r) => r.id).join('\n')} label="Copy genes" />
            <button type="button" className="btn btn-ghost btn-sm" onClick={onClear}>Clear</button>
          </span>
        )}
      </div>
      {rows.length === 0
        ? <p className="muted small">No genes selected yet. Drag a box or lasso on the plot{isVolcano ? ', or use a quick selection above' : ''}; the genes you pick appear here.</p>
        : (
          <div className="selected-genes-scroll">
            <table className="table">
              <thead>
                <tr>
                  <th>Gene</th>
                  {isVolcano ? <><th className="num">log2FC</th><th className="num">p-value</th><th>Change</th></> : colorLabel ? <th>{colorLabel}</th> : null}
                  <th aria-label="Remove" />
                </tr>
              </thead>
              <tbody>
                {rows.slice(0, MAX_ROWS).map((r) => (
                  <tr key={r.id}>
                    <td className="mono">{r.id}</td>
                    {isVolcano
                      ? <><td className="num">{(r as VolcanoPoint).logfc.toFixed(2)}</td><td className="num">{(r as VolcanoPoint).pvalue.toExponential(2)}</td><td>{(r as VolcanoPoint).logfc > 0 ? 'Up' : 'Down'}</td></>
                      : colorLabel ? <td>{String((r as ReductionPoint).color ?? '')}</td> : null}
                    <td className="num"><button type="button" className="icon-btn" title={`Remove ${r.id} from the selection`} aria-label={`Remove ${r.id} from the selection`} onClick={() => onRemove(r.id)}>×</button></td>
                  </tr>
                ))}
              </tbody>
            </table>
            {rows.length > MAX_ROWS && <p className="muted small">Showing the first {MAX_ROWS} of {rows.length.toLocaleString()} selected genes; all of them will be saved.</p>}
          </div>
        )}
    </section>
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

type UpsetMode = 'intersection' | 'distinct' | 'union'
const UPSET_MODES: { value: UpsetMode; label: string; help: string }[] = [
  { value: 'distinct', label: 'Only in', help: 'Each bar: IDs in exactly these lists and in no other (the parts never overlap, so any set operation is a choice of bars).' },
  { value: 'intersection', label: 'Shared by', help: 'Each bar: IDs shared by these lists, whether or not other lists have them too.' },
  { value: 'union', label: 'Union of', help: 'Each bar: IDs found in at least one of these lists.' },
]

/**
 * Combine lists with an interactive UpSet plot. Bars are selectable (one or
 * more); the IDs of the selected bars are listed and can be saved as a new list.
 * Opened from "Compare selected" in the lists panel and from "Combine lists".
 */
function Upset({ lists, onClose, onCreated, title, initialListIds }: P) {
  const [picked, setPicked] = useState<string[]>(() => {
    const init = (initialListIds ?? []).filter((id) => lists.some((l) => l.id === id)).slice(0, 6)
    return init.length >= 2 ? init : lists.slice(0, 2).map((l) => l.id)
  })
  const [mode, setMode] = useState<UpsetMode>('distinct')
  const [res, setRes] = useState<UpsetResult | null>(null)
  const [selIds, setSelIds] = useState<string[]>([])   // ids of the selected bars
  // A quick operation chosen in another mode is applied once the exclusive ("only in") bars have loaded.
  const pending = useRef<((r: UpsetResult) => string[]) | null>(null)
  const selShown = usePlainSelection(selIds, NO_BARS) // none while an image is exported without highlighting
  const upsetRef = useRef<HTMLDivElement | null>(null)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [opNote, setOpNote] = useState<string | null>(null)
  const pm = usePaletteMode()
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
    if (picked.length < 2) { setRes(null); setSelIds([]); return }
    setErr(null); setBusy(true)
    let stale = false
    api<UpsetResult>('/inputs/upset', { body: { listIds: picked, mode } })
      .then((r) => {
        if (stale) return
        setRes(r)
        const quick = pending.current; pending.current = null
        setSelIds(quick ? quick(r) : [])
      })
      .catch((e) => { if (!stale) { setRes(null); setErr(e) } })
      .finally(() => { if (!stale) setBusy(false) })
    return () => { stale = true }
  }, [picked, mode])

  const combos = res?.combinations ?? []
  const nSets = res?.sets.length ?? 0
  const max = Math.max(1, ...combos.map((c) => c.count))
  const maxSize = Math.max(1, ...(res?.sets.map((x) => x.size) ?? [1]))
  const colors = categorical(pm)
  const toggle = (id: string) => { setOpNote(null); setSelIds((cur) => (cur.includes(id) ? cur.filter((x) => x !== id) : [...cur, id])) }
  // Set operations across all picked lists, as a choice of exclusive bars.
  const quickPick = (label: string, choose: (r: UpsetResult) => string[]) => {
    const apply = (r: UpsetResult) => { const ids = choose(r); setOpNote(ids.length ? null : `No IDs match "${label}" for these lists.`); return ids }
    if (res && res.mode === 'distinct') setSelIds(apply(res))
    else { pending.current = apply; setMode('distinct') }
  }
  const byDegree = (test: (degree: number, n: number) => boolean) => (r: UpsetResult) => r.combinations.filter((c) => test(c.degree, r.sets.length)).map((c) => c.id)

  const selected = combos.filter((c) => selShown.includes(c.id))
  const chosen = combos.filter((c) => selIds.includes(c.id))
  // Each ID once, with the lists it belongs to (exact in "only in" mode; otherwise the lists of the bars it came from).
  const genes = useMemo(() => {
    const m = new Map<string, Set<string>>()
    chosen.forEach((c) => c.ids.forEach((id) => { const cur = m.get(id) ?? new Set<string>(); c.sourceNames.forEach((n) => cur.add(n)); m.set(id, cur) }))
    return [...m.entries()].map(([id, names]) => ({ id, lists: res ? res.sets.map((x) => x.name).filter((n) => names.has(n)) : [] }))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [selIds, res])
  const MAX_ROWS = 300
  const defaultName = chosen.length === 1 ? chosen[0].label.slice(0, 80) : chosen.length > 1 ? `Combined: ${plural(chosen.length, 'part')} of ${nSets} lists` : 'Combined list'

  return (
    <Modal title={title} onClose={onClose} size="xl" subtitle="Pick 2–6 lists with the same organism and ID type. Click one or more bars of the UpSet plot to select their IDs, then save them as a new list.">
      <div className="upset-layout">
        <div className="stack-sm">
          <h3>Lists</h3>
          <ul className="plain-list">
            {lists.map((l) => (
              <li key={l.id}>
                <label className={`check${!compatible(l) && !picked.includes(l.id) ? ' muted' : ''}`}>
                  <input type="checkbox" checked={picked.includes(l.id)} disabled={!compatible(l) && !picked.includes(l.id)}
                    onChange={() => setPicked((cur) => (cur.includes(l.id) ? cur.filter((x) => x !== l.id) : cur.length >= 6 ? cur : [...cur, l.id]))} />
                  {l.name} <span className="muted small">({l.size})</span>
                </label>
              </li>
            ))}
          </ul>
          <Field label="Each bar shows" hint={UPSET_MODES.find((m) => m.value === mode)?.help}>
            <Segmented label="What each bar shows" value={mode} onChange={(m) => { pending.current = null; setOpNote(null); setMode(m) }} options={UPSET_MODES.map(({ value, label }) => ({ value, label }))} />
          </Field>
          {res && (
            <div className="stack-sm" data-no-export="">
              <span className="field-label">Set operations on all {nSets} lists</span>
              <div className="row wrap">
                <button type="button" className="btn btn-secondary btn-sm" title="IDs present in every list" onClick={() => quickPick('Intersection (in every list)', byDegree((d, n) => d === n))}>Intersection</button>
                <button type="button" className="btn btn-secondary btn-sm" title="IDs present in at least one list" onClick={() => quickPick('Union (in any list)', byDegree(() => true))}>Union</button>
                <button type="button" className="btn btn-secondary btn-sm" title="Exclusive or: IDs present in exactly one list" onClick={() => quickPick('XOR (in exactly one list)', byDegree((d) => d === 1))}>XOR (in one list only)</button>
                {nSets > 2 && <button type="button" className="btn btn-secondary btn-sm" title="IDs present in two or more lists" onClick={() => quickPick('In 2 or more lists', byDegree((d) => d >= 2))}>In 2 or more</button>}
                {nSets > 2 && <button type="button" className="btn btn-secondary btn-sm" title="IDs present in some lists but not in all of them" onClick={() => quickPick('Not in all lists', byDegree((d, n) => d < n))}>Not in all</button>}
              </div>
            </div>
          )}
        </div>
        <div className="stack-sm">
          <ErrorNote error={err} />
          {busy && <BusyBar label="Comparing the lists" expect={4} />}
          {!res && !busy && !err && <p className="muted">Select at least two lists.</p>}
          {res && (
            <>
              <div className="upset-plot upset-pick" ref={upsetRef}>
                <div className="upset-sets">
                  {res.sets.map((x, i) => (
                    <div key={x.name} className="upset-set-row">
                      <span className="upset-set-bar"><span style={{ width: `${(x.size / maxSize) * 100}%`, background: colors[i % colors.length] }} /></span>
                      <span className="upset-set-label" title={x.name}>{x.name}</span>
                      <span className="upset-count">{x.size}</span>
                    </div>
                  ))}
                </div>
                <div className="upset-combos" role="group" aria-label="Combinations — click to select">
                  {combos.map((c) => {
                    const on = selShown.includes(c.id)
                    return (
                      <button key={c.id} type="button" aria-pressed={on} className={`upset-col${on ? ' active' : ''}`} onClick={() => toggle(c.id)}
                        title={`${c.label}: ${plural(c.count, 'ID')} — click to ${on ? 'remove from' : 'add to'} the selection`}>
                        <span className="upset-col-count">{c.count}</span>
                        <span className="upset-col-bar"><span style={{ height: `${(c.count / max) * 100}%` }} /></span>
                        <span className="upset-col-dots">{res.sets.map((x, i) => {
                          const inCombo = c.sourceNames.includes(x.name)
                          return <i key={x.name} className={inCombo ? 'on' : ''} style={inCombo ? { background: colors[i % colors.length] } : undefined} />
                        })}</span>
                      </button>
                    )
                  })}
                </div>
              </div>
              <div className="row wrap" data-no-export="">
                <span className="small"><strong>{plural(selected.length, 'bar')}</strong> selected</span>
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => setSelIds(combos.map((c) => c.id))}>Select all bars</button>
                <button type="button" className="btn btn-ghost btn-sm" disabled={!selIds.length} onClick={() => setSelIds([])}>Clear</button>
                <ExportImageButton source={() => htmlSource(upsetRef.current, `flame_upset_${res.mode}`)} />
                <CopyButton label="Copy table" text={['combination\tcount\tids', ...combos.map((c) => [c.label, c.count, c.ids.join(',')].map(tsvCell).join('\t'))].join('\n')} />
                <button type="button" className="btn btn-ghost btn-sm" title="Excel workbook: a summary and one sheet per combination" onClick={() => saveXlsx([
                  { name: 'Combinations', rows: combos.map((c) => ({ Combination: c.label, Lists: c.sourceNames.join(' + '), Count: c.count, IDs: c.ids.join(', ') })) },
                  ...combos.slice(0, 60).map((c) => ({ name: c.label, rows: c.ids.map((i) => ({ ID: i })) })),
                ], `flame_upset_${res.mode}.xlsx`)}>Excel</button>
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => saveText(['combination\tlists\tcount\tids', ...combos.map((c) => [c.label, c.sourceNames.join(' + '), c.count, c.ids.join(',')].map(tsvCell).join('\t'))].join('\n'), `flame_upset_${res.mode}.tsv`, 'text/tab-separated-values')}>TSV</button>
              </div>
            </>
          )}
        </div>
      </div>
      {res && (
        <section className="selected-genes" aria-label="Selected IDs">
          <div className="row between wrap">
            <h3>Selected IDs <span className="count">{genes.length.toLocaleString()}</span>
              {chosen.length > 1 && <span className="muted small"> · from {plural(chosen.length, 'bar')}, each ID counted once</span>}</h3>
            {genes.length > 0 && (
              <span className="row">
                <CopyButton label="Copy IDs" text={genes.map((g) => g.id).join('\n')} />
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => saveText(genes.map((g) => g.id).join('\n'), 'flame_combined_ids.txt')}>Download (.txt)</button>
              </span>
            )}
          </div>
          {chosen.length === 0 && opNote && <Note kind="info">{opNote}</Note>}
          {chosen.length === 0 && !opNote && <p className="muted small">Nothing selected yet. Click one or more bars, or use a set operation on the left; the IDs appear here.</p>}
          {chosen.length > 0 && (
            <div className="upset-parts">
              {chosen.map((c) => (
                <details key={c.id} className="upset-part">
                  <summary><strong>{c.label}</strong> <span className="muted small">{plural(c.count, 'ID')}</span>
                    <button type="button" className="icon-btn" title={`Remove "${c.label}" from the selection`} aria-label={`Remove ${c.label} from the selection`} onClick={(e) => { e.preventDefault(); toggle(c.id) }}>×</button></summary>
                  <p className="mono small">{c.ids.join(', ')}</p>
                </details>
              ))}
            </div>
          )}
          {genes.length > 0 && (
            <div className="selected-genes-scroll">
              <table className="table">
                <thead><tr><th>ID</th><th>{res.mode === 'distinct' ? 'Found in' : 'From bars of'}</th><th className="num">Lists</th></tr></thead>
                <tbody>
                  {genes.slice(0, MAX_ROWS).map((g) => (
                    <tr key={g.id}><td className="mono">{g.id}</td><td>{g.lists.join(' + ')}</td><td className="num">{g.lists.length}</td></tr>
                  ))}
                </tbody>
              </table>
              {genes.length > MAX_ROWS && <p className="muted small">Showing the first {MAX_ROWS} of {genes.length.toLocaleString()} selected IDs; all of them will be saved.</p>}
            </div>
          )}
          <h3 className="save-heading">Save the selected IDs as a new list</h3>
          <SaveList ids={genes.map((g) => g.id).slice(0, 3000)} defaultName={defaultName} organism={res.organismTaxId} idFormat={res.idFormat}
            showOrganism={false} showFormat={false}
            meta={{ method: 'upset', mode: res.mode, sourceLists: [...new Set(chosen.flatMap((c) => c.sourceNames))] }} onCreated={onCreated} />
        </section>
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

function ExpressionInput({ example, onClose, onCreated, title, onOpenVolcano, initialView }: P) {
  const [text, setText] = useState('')
  const [fileName, setFileName] = useState('')
  const [topN, setTopN] = useState(1000)
  const [k, setK] = useState(6)
  const [logMode, setLogMode] = useState<'auto' | 'yes' | 'no'>('auto')
  const [ov, setOv] = useState<ExprOverview | null>(null)
  const [view, setView] = useState<'clusters' | 'map' | 'groups' | 'correlation' | 'boxplots' | 'activity' | 'survival'>(initialView === 'activity' ? 'activity' : initialView === 'survival' ? 'survival' : 'clusters')
  const [clinical, setClinical] = useState<ClinicalTable | null>(null)
  const [sel, setSel] = useState<Set<string>>(new Set())
  const selV = usePlainSelection(sel, NO_SET) // none while an image is exported without highlighting
  const [groups, setGroups] = useState<Record<string, 'A' | 'B' | ''>>({})
  const [box, setBox] = useState<BoxSetup | null>(null)
  const [replacing, setReplacing] = useState(false)
  usePaletteKey(`expression-${view}`) // each tab keeps its own palette
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [note, setNote] = useState<string | null>(null)
  const [organism, setOrganism] = useState(9606)
  const [idFormat, setIdFormat] = useState('USERINPUT')
  const pm = usePaletteMode()
  const ex = useExample('expression', example, (t, f) => { setText(t); setFileName(f ?? ''); setClinical(null) })
  // Survival example: 365 liver tumours (TCGA LIHC) with their expression and follow-up.
  const [survEx, setSurvEx] = useState<{ loading: boolean; source: string | null }>({ loading: false, source: null })
  const loadSurvivalExample = () => {
    setSurvEx({ loading: true, source: null }); setErr(null)
    api<{ text: string; fileName: string; clinical: string; clinicalFileName: string; source: string }>('/inputs/examples/survival')
      .then((r) => { setText(r.text); setFileName(r.fileName); setClinical({ text: r.clinical, fileName: r.clinicalFileName }); setView('survival'); setReplacing(false); setSurvEx({ loading: false, source: r.source }) })
      .catch((e) => { setErr(e); setSurvEx({ loading: false, source: null }) })
  }
  useEffect(() => { if (initialView === 'survival') loadSurvivalExample() // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])
  const log = logMode === 'auto' ? null : logMode === 'yes'

  useEffect(() => {
    if (!text.trim()) return
    setErr(null); setBusy(true)
    api<ExprOverview>('/inputs/expression/overview', { body: { text, fileName, topN, k, log } })
      .then((r) => {
        setOv(r); setSel(new Set())
        setBox((b) => (b && r.samples.every((s) => s in b.assign) ? b : null))
        // Guess two groups from sample names (e.g. control_1 / treated_1).
        const stem = (s: string) => s.replace(/[_.\- ]?(rep)?\d+$/i, '')
        const stems = [...new Set(r.samples.map(stem))]
        setGroups(Object.fromEntries(r.samples.map((s) => [s, stems.length === 2 ? (stem(s) === stems[1] ? 'A' : 'B') : ''])))
      })
      .catch((e) => { setOv(null); setErr(e) })
      .finally(() => setBusy(false))
  }, [text, fileName, topN, k, log])

  const clusterColor = (c: number) => categorical(pm)[(c - 1) % categorical(pm).length]
  const clusterIds = (c: number) => ov!.genes.filter((g) => g.cluster === c).map((g) => g.id)
  // How much of a cluster is in the selection: all of it, part of it (e.g. after a lasso) or none.
  const clusterState = (c: number): 'all' | 'some' | 'none' => {
    const ids = clusterIds(c)
    const n = ids.filter((id) => sel.has(id)).length
    return n === 0 ? 'none' : n === ids.length ? 'all' : 'some'
  }
  // Clusters add up: clicking one adds its genes to the selection, clicking it again takes them out.
  const toggleCluster = (c: number) => {
    const ids = clusterIds(c)
    const remove = clusterState(c) === 'all'
    setSel((cur) => { const next = new Set(cur); ids.forEach((id) => (remove ? next.delete(id) : next.add(id))); return next })
  }
  const pickedClusters = ov ? Object.keys(ov.clusterSizes).map(Number).filter((c) => clusterState(c) === 'all') : []
  const geneById = useMemo(() => new Map((ov?.genes ?? []).map((g) => [g.id, g])), [ov])
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
        {(!ov || replacing) && (
          <div className="row wrap">
            <FileDrop accept=".txt,.csv,.tsv,.tab" maxBytes={20 * 1024 * 1024} onText={(t, n) => { setText(t); setFileName(n); setReplacing(false) }} />
            <button className="btn btn-ghost btn-sm" onClick={() => { setReplacing(false); ex.load() }} disabled={ex.loading}>{ex.loading ? 'Loading…' : 'Load example'}</button>
            <button className="btn btn-ghost btn-sm" onClick={loadSurvivalExample} disabled={survEx.loading}>{survEx.loading ? 'Loading…' : 'Survival example'}</button>
            <a className="btn btn-ghost btn-sm" href={`${API_BASE}/inputs/examples/expression/file`}>Example file</a>
            {ov && <button className="btn btn-ghost btn-sm" onClick={() => setReplacing(false)}>Keep current table</button>}
          </div>
        )}
        {!text && <textarea className="input mono" rows={5} placeholder={'gene\tcontrol_1\tcontrol_2\ttreated_1\ttreated_2\nTP53\t812\t790\t1530\t1610'} onChange={(e) => setText(e.target.value)} />}
        <ExampleSource text={survEx.source ?? ex.source} loading={ex.loading || survEx.loading} />
        <ErrorNote error={err} />
        {busy && <BusyBar label="Analysing the matrix: clustering genes and computing the gene map" expect={12} />}
        {ov && (
          <>
            <div className="row wrap small">
              <span><span className="muted">Table:</span> <strong>{fileName || (ex.source ? 'example' : 'pasted table')}</strong> · <strong>{ov.totalGenes.toLocaleString()}</strong> genes × <strong>{ov.samples.length}</strong> samples{ov.dropped ? ` (${ov.dropped} rows skipped)` : ''} · {ov.logged ? 'log2(x + 1) transformed' : 'used as given'}</span>
              <label>Log2 <select className="input input-sm" value={logMode} onChange={(e) => setLogMode(e.target.value as 'auto' | 'yes' | 'no')}><option value="auto">auto</option><option value="yes">yes</option><option value="no">no</option></select></label>
              <span className="toolbar-spacer" />
              {!replacing && <button className="btn btn-ghost btn-sm" onClick={() => setReplacing(true)}>Replace table</button>}
            </div>
            <Segmented label="View" value={view} onChange={setView}
              options={[{ value: 'clusters', label: 'Co-expression clusters' }, { value: 'map', label: 'Gene map (PCA)' }, { value: 'groups', label: 'Compare two groups' }, { value: 'correlation', label: 'Correlation' }, { value: 'boxplots', label: 'Boxplots' }, { value: 'activity', label: 'Pathway activity' }, { value: 'survival', label: 'Survival' }]} />
            {(view === 'clusters' || view === 'map') && (
              <div className="row wrap small">
                <label>Most variable <select className="input input-sm" value={topN} onChange={(e) => setTopN(Number(e.target.value))}>{[200, 500, 1000, 2000, 3000].map((n) => <option key={n} value={n}>{n} genes</option>)}</select></label>
                <label>Clusters <input className="input input-sm input-narrow" type="number" min={2} max={20} value={k} onChange={(e) => setK(Number(e.target.value))} /></label>
              </div>
            )}
            {view === 'clusters' && (
              <div className="expr-clusters">
                <Plot height={Math.min(560, 120 + ov.heatmap.genes.length * 0.5)} data={[{
                  type: 'heatmap', z: ov.heatmap.z, x: ov.samples, y: ov.heatmap.genes, colorscale: 'RdBu', reversescale: true, zmid: 0, zmin: -2.5, zmax: 2.5,
                  customdata: ov.heatmap.genes.map((_, i) => ov.samples.map(() => `c${ov.heatmap.clusters[i]}`)) as never,
                  hovertemplate: '%{y} · %{x}<br>z = %{z}<extra></extra>', colorbar: { title: { text: 'z-score' } },
                } as never]}
                  layout={{ yaxis: { showticklabels: false, autorange: 'reversed' }, margin: { l: 20, r: 20, t: 10, b: 90 } }}
                  onSelect={(keys) => { const c = Number((keys[0] ?? '').slice(1)); if (c) toggleCluster(c) }} />
                <div className="stack-sm">
                  <p className="muted small">Genes with similar profiles across samples, clustered by correlation. Tick one or more clusters (or click them on the heatmap) to select their genes; tick again to remove them.</p>
                  {Object.entries(ov.clusterSizes).map(([c, n]) => {
                    const state = clusterState(Number(c))
                    return (
                      <label key={c} className={`example-mini cluster-pick${state !== 'none' ? ' picked' : ''}`}>
                        <input type="checkbox" checked={state === 'all'} ref={(el) => { if (el) el.indeterminate = state === 'some' }} onChange={() => toggleCluster(Number(c))} />
                        <i className="swatch" style={{ background: clusterColor(Number(c)) }} />
                        <span><strong>Cluster {c}</strong><span className="muted small">{n} genes</span></span>
                      </label>
                    )
                  })}
                  <div className="row">
                    <button type="button" className="btn btn-ghost btn-sm" onClick={() => setSel(new Set(ov.genes.map((g) => g.id)))}>All clusters</button>
                    <button type="button" className="btn btn-ghost btn-sm" onClick={() => setSel(new Set())}>None</button>
                  </div>
                </div>
              </div>
            )}
            {view === 'map' && (
              <Plot height={460} dragmode="lasso" onSelect={(ids, add) => setSel((cur) => (add ? new Set([...cur, ...ids]) : new Set(ids)))}
                data={Object.keys(ov.clusterSizes).map((c) => {
                  const g = ov.genes.filter((x) => x.cluster === Number(c))
                  return { type: 'scatter', mode: 'markers', name: `Cluster ${c}`, x: g.map((p) => p.x), y: g.map((p) => p.y), customdata: g.map((p) => p.id), text: g.map((p) => p.id),
                    hovertemplate: '%{text}<extra>%{fullData.name}</extra>',
                    marker: { size: g.map((p) => (selV.has(p.id) ? 9 : 6)), color: clusterColor(Number(c)), opacity: 0.8, line: { width: g.map((p) => (selV.has(p.id) ? 1.5 : 0)), color: '#111827' } } }
                })}
                layout={{ xaxis: { title: { text: `PC1 (${ov.variance[0]}%)` } }, yaxis: { title: { text: `PC2 (${ov.variance[1] ?? 0}%)` } }, legend: { orientation: 'h', y: -0.2 } }} />
            )}
            {view === 'groups' && (
              <div className="stack-sm">
                <p className="muted small">Put samples into group A and group B (at least two each). Flame tests every gene (Welch t-test, Benjamini–Hochberg) and opens the result in the volcano selector, where you pick up- and down-regulated genes or save all genes as a ranked list for GSEA. The <em>Boxplots</em> tab starts from these groups.</p>
                <SampleGroupPicker samples={ov.samples} names={['Group A', 'Group B']} colors={roleColors(pm).series}
                  groupOf={(s) => (groups[s] === 'A' ? 0 : groups[s] === 'B' ? 1 : -1)}
                  onAssign={(ss, g) => setGroups((cur) => ({ ...cur, ...Object.fromEntries(ss.map((x) => [x, g === 0 ? 'A' : g === 1 ? 'B' : ''])) }))} />
                <div className="row between wrap">
                  <span className="small">A = {Object.values(groups).filter((g) => g === 'A').length} samples, B = {Object.values(groups).filter((g) => g === 'B').length} samples · log2FC = A − B</span>
                  <button className="btn btn-primary" disabled={busy || Object.values(groups).filter((g) => g === 'A').length < 2 || Object.values(groups).filter((g) => g === 'B').length < 2} onClick={runDe}>Compare and open volcano</button>
                </div>
                {note && <Note kind="info">{note}</Note>}
              </div>
            )}
            {view === 'correlation' && <CorrelationView text={text} fileName={fileName} log={log} groups={groups}
              organism={organism} setOrganism={setOrganism} idFormat={idFormat} setIdFormat={setIdFormat} onCreated={onCreated}
              onBoxplots={(g) => { setBox((b) => ({ ...(b ?? initialBoxSetup(ov.samples, groups)), genes: g })); setView('boxplots') }} />}
            {view === 'boxplots' && <BoxplotsView text={text} fileName={fileName} log={log} samples={ov.samples}
              setup={box ?? initialBoxSetup(ov.samples, groups)} setSetup={(f) => setBox((b) => f(b ?? initialBoxSetup(ov.samples, groups)))} />}
            {view === 'activity' && <PathwayActivity text={text} fileName={fileName} log={log} organism={organism} />}
            {view === 'survival' && <SurvivalPanel text={text} fileName={fileName} log={log} clinical={clinical} setClinical={setClinical} onCreated={onCreated} />}
            {(view === 'clusters' || view === 'map') && (
              <>
                <SelectedGenes isVolcano={false} colorLabel="Cluster"
                  points={[...selV].map((id) => geneById.get(id)).filter((g): g is NonNullable<typeof g> => !!g)
                    .map((g) => ({ pointId: g.id, id: g.id, x: g.x, y: g.y, color: `Cluster ${g.cluster}` }))}
                  onRemove={(id) => setSel((cur) => { const next = new Set(cur); next.delete(id); return next })}
                  onClear={() => setSel(new Set())} />
                <h3 className="save-heading">Save the selected genes as a list</h3>
                <SaveList ids={[...sel].slice(0, 3000)}
                  defaultName={pickedClusters.length && pickedClusters.reduce((n, c) => n + ov.clusterSizes[String(c)], 0) === sel.size
                    ? `Expression cluster${pickedClusters.length > 1 ? 's' : ''} ${pickedClusters.join('+')}` : 'Expression selection'}
                  organism={organism} setOrganism={setOrganism}
                  idFormat={idFormat} setIdFormat={setIdFormat} meta={{ method: 'expression', view, clusters: pickedClusters }} onCreated={onCreated} />
              </>
            )}
          </>
        )}
      </div>
    </Modal>
  )
}

// ---- Correlation with a gene of interest (Pearson, Spearman, Kendall) ----
type CorrMethod = 'pearson' | 'spearman' | 'kendall'
const CORR_METHODS: CorrMethod[] = ['pearson', 'spearman', 'kendall']
const CORR_LABEL: Record<CorrMethod, string> = { pearson: 'Pearson r', spearman: 'Spearman ρ', kendall: 'Kendall τ' }
interface CorrResult {
  gene: string; n: number; samples: string[]; methods: CorrMethod[]; genes: string[]; tested: number; constant: number; logged: boolean
  values: number[]; results: Partial<Record<CorrMethod, { r: number[]; p: number[]; padj: number[] }>>; note: string
}
interface GeneValues {
  samples: string[]; groups: Record<string, string[]>; logged: boolean; missing: string[]; test: string; testName: string
  genes: { gene: string; values: number[]; pvalue: number | null; means: Record<string, number>; pairwise: { a: string; b: string; log2FC: number; pvalue: number | null; padj: number | null }[] }[]
}
type Consensus = 'pos' | 'neg' | 'ns'
const fmtP = (p: number | null | undefined) => (p == null || !Number.isFinite(p) ? '–' : p < 1e-3 ? p.toExponential(1) : p.toFixed(3))
const MAX_PLOTTED_OTHER = 6000

/** Gene IDs of an expression table (its first column), for suggestions. */
function useTableGeneIds(text: string) {
  return useMemo(() => text.split(/\r?\n/).slice(1).map((l) => l.split(/[\t,;]/)[0]?.trim().replace(/^"|"$/g, '')).filter((x): x is string => !!x), [text])
}
function GeneSuggest({ id, ids, value }: { id: string; ids: string[]; value: string }) {
  const q = value.trim().toUpperCase()
  const opts = q ? ids.filter((g) => g.toUpperCase().startsWith(q)).slice(0, 30) : []
  return <datalist id={id}>{opts.map((g) => <option key={g} value={g} />)}</datalist>
}

interface PairResult {
  geneA: string; geneB: string; n: number; samples: string[]; logged: boolean; a: number[]; b: number[]; note: string
  stats: { method: CorrMethod; r: number; p: number; ciLow: number | null; ciHigh: number | null }[]
}

/** The “diagonal”: expression of two genes across the samples, with each method's correlation (cor.test). */
function PairPlot({ text, fileName, log, geneA, geneB, methods, groups, fdr }: {
  text: string; fileName: string; log: boolean | null; geneA: string; geneB: string; methods: CorrMethod[]
  groups: Record<string, 'A' | 'B' | ''>; fdr?: Partial<Record<CorrMethod, number>>
}) {
  const [res, setRes] = useState<PairResult | null>(null)
  const [err, setErr] = useState<unknown>(null)
  const [labels, setLabels] = useState(true)
  const pm = usePaletteMode()
  const rc = roleColors(pm)
  useEffect(() => {
    setErr(null)
    api<PairResult>('/inputs/expression/pair', { body: { text, fileName, log, geneA, geneB, methods } })
      .then(setRes).catch((e) => { setRes(null); setErr(e) })
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [text, fileName, log, geneA, geneB, methods.join(',')])

  const unit = res?.logged ? 'log2(x + 1)' : 'expression'
  const traces = useMemo(() => {
    if (!res) return []
    const hasGroups = res.samples.some((s) => groups[s] === 'A') && res.samples.some((s) => groups[s] === 'B')
    const cats: [string, string, string][] = hasGroups ? [['A', 'Group A', rc.series[0]], ['B', 'Group B', rc.series[1]], ['', 'Not grouped', '#9CA3AF']] : [['*', 'Samples', rc.accent]]
    const out: Record<string, unknown>[] = cats.map(([k, name, color]) => {
      const idx = res.samples.map((s, i) => [s, i] as const).filter(([s]) => k === '*' || (groups[s] ?? '') === k)
      return { type: 'scatter', mode: labels ? 'markers+text' : 'markers', name, x: idx.map(([, i]) => res.a[i]), y: idx.map(([, i]) => res.b[i]), text: idx.map(([s]) => s),
        textposition: 'top center', textfont: { size: 9 }, marker: { size: 10, color, line: { width: 1, color: '#fff' } },
        hovertemplate: `%{text}<br>${res.geneA}: %{x:.2f}<br>${res.geneB}: %{y:.2f}<extra></extra>` }
    }).filter((t) => (t.x as number[]).length)
    const n = res.a.length; const mx = res.a.reduce((u, v) => u + v, 0) / n; const my = res.b.reduce((u, v) => u + v, 0) / n
    const sxx = res.a.reduce((u, v) => u + (v - mx) ** 2, 0); const sxy = res.a.reduce((u, v, i) => u + (v - mx) * (res.b[i] - my), 0)
    const slope = sxx ? sxy / sxx : 0; const lo = Math.min(...res.a); const hi = Math.max(...res.a)
    out.push({ type: 'scatter', mode: 'lines', name: 'Linear fit', x: [lo, hi], y: [my + slope * (lo - mx), my + slope * (hi - mx)], hoverinfo: 'skip', line: { dash: 'dash', color: '#6B7280', width: 1.5 } })
    return out
  }, [res, groups, labels, rc])

  return (
    <div className="stack-sm">
      <ErrorNote error={err} />
      {!res && !err && <Spinner label={`Loading ${geneA} and ${geneB}`} />}
      {res && (
        <>
          <div className="row wrap small">
            <strong>{res.geneA} vs {res.geneB}</strong><span className="muted">{res.n} samples</span>
            <span className="toolbar-spacer" />
            <PaletteSelect />
            <label className="row"><input type="checkbox" checked={labels} onChange={(e) => setLabels(e.target.checked)} /> Sample labels</label>
          </div>
          <table className="table compact">
            <thead><tr><th>Method</th><th className="num">Coefficient</th><th className="num">95% CI</th><th className="num">p-value</th>{fdr && <th className="num">FDR (vs all genes)</th>}</tr></thead>
            <tbody>{res.stats.map((s) => (
              <tr key={s.method}><td>{CORR_LABEL[s.method]}</td><td className="num"><strong>{s.r.toFixed(3)}</strong></td>
                <td className="num">{s.ciLow != null && Number.isFinite(s.ciLow) ? `${s.ciLow.toFixed(2)} to ${s.ciHigh!.toFixed(2)}` : '–'}</td>
                <td className="num">{fmtP(s.p)}</td>{fdr && <td className="num">{fmtP(fdr[s.method])}</td>}</tr>
            ))}</tbody>
          </table>
          <Plot height={420} exportName={`flame_correlation_${res.geneA}_${res.geneB}`} data={traces as never}
            layout={{ xaxis: { title: { text: `${res.geneA} (${unit})` } }, yaxis: { title: { text: `${res.geneB} (${unit})` } }, legend: { orientation: 'h', y: -0.2 } }} />
          <p className="muted small">Each point is a sample. p-values from <code>cor.test</code> for this pair (exact for Spearman and Kendall in small samples without ties); 95% CI for Pearson only.{fdr ? ' The FDR column comes from testing this gene against every gene.' : ''} {res.note}</p>
        </>
      )}
    </div>
  )
}

function CorrelationView({ text, fileName, log, groups, organism, setOrganism, idFormat, setIdFormat, onCreated, onBoxplots }: {
  text: string; fileName: string; log: boolean | null; groups: Record<string, 'A' | 'B' | ''>
  organism: number; setOrganism: (t: number) => void; idFormat: string; setIdFormat: (f: string) => void; onCreated: (l: AnalyteList) => void
  onBoxplots: (genes: string[]) => void
}) {
  const [mode, setMode] = useState<'all' | 'pair'>('all')
  const [gene, setGene] = useState('')
  const [gene2, setGene2] = useState('')
  const [pairAsked, setPairAsked] = useState<[string, string] | null>(null)
  const [methods, setMethods] = useState<CorrMethod[]>(CORR_METHODS)
  const [res, setRes] = useState<CorrResult | null>(null)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [minR, setMinR] = useState(0.7)
  const [maxQ, setMaxQ] = useState(0.05)
  const [plotMethod, setPlotMethod] = useState<CorrMethod>('spearman')
  const [sel, setSel] = useState<Set<string>>(new Set())
  const selV = usePlainSelection(sel, NO_SET)
  const [partner, setPartner] = useState('')
  const pm = usePaletteMode()
  const rc = roleColors(pm)
  const create = useCreateList()
  const ids = useTableGeneIds(text)
  const pairRef = useRef<HTMLDivElement>(null)

  const run = () => {
    setBusy(true); setErr(null)
    api<CorrResult>('/inputs/expression/correlate', { body: { text, fileName, log, gene, methods } })
      .then((r) => { setRes(r); setPartner(''); if (!r.methods.includes(plotMethod)) setPlotMethod(r.methods[0]) })
      .catch(setErr).finally(() => setBusy(false))
  }
  const showPartner = (g: string) => {
    setPartner(g)
    setTimeout(() => pairRef.current?.scrollIntoView({ behavior: 'smooth', block: 'nearest' }), 50)
  }

  // Per gene: consensus = every chosen method passes |r| and FDR cut-offs with the same sign.
  const rows = useMemo(() => {
    if (!res) return []
    return res.genes.map((g, i) => {
      const per = res.methods.map((m) => ({ m, r: res.results[m]!.r[i], padj: res.results[m]!.padj[i] }))
      const pass = per.map((x) => Math.abs(x.r) >= minR && x.padj <= maxQ)
      const all = pass.every(Boolean)
      const consensus: Consensus = all && per.every((x) => x.r > 0) ? 'pos' : all && per.every((x) => x.r < 0) ? 'neg' : 'ns'
      return { gene: g, i, per, consensus, meanR: per.reduce((a, x) => a + x.r, 0) / per.length, passCount: pass.filter(Boolean).length }
    })
  }, [res, minR, maxQ])
  const byGene = useMemo(() => new Map(rows.map((r) => [r.gene, r])), [rows])
  const counts = useMemo(() => ({ pos: rows.filter((r) => r.consensus === 'pos').length, neg: rows.filter((r) => r.consensus === 'neg').length }), [rows])
  // A new result or new cut-offs select the consensus genes.
  useEffect(() => { setSel(new Set(rows.filter((r) => r.consensus !== 'ns').map((r) => r.gene))) }, [rows])
  const pick = (which: Consensus | 'both') => setSel(new Set(rows.filter((r) => (which === 'both' ? r.consensus !== 'ns' : r.consensus === which)).map((r) => r.gene)))

  // r against −log10 FDR (plain SVG, no WebGL); the non-consensus cloud is thinned when it is very large.
  const { plotData, thinned } = useMemo(() => {
    const pr = res?.results[plotMethod]
    if (!res || !pr) return { plotData: [], thinned: 0 }
    const y = (i: number) => Math.min(50, -Math.log10(Math.max(pr.padj[i], 1e-300)))
    let thinned = 0
    const traces = ([['ns', 'Other', rc.ns], ['pos', 'Consensus positive', rc.up], ['neg', 'Consensus negative', rc.down]] as const).map(([k, name, color]) => {
      let rs = rows.filter((r) => r.consensus === k)
      const total = rs.length
      if (k === 'ns' && rs.length > MAX_PLOTTED_OTHER) { const step = Math.ceil(rs.length / MAX_PLOTTED_OTHER); rs = rs.filter((_, j) => j % step === 0 || selV.has(rs[j].gene) || rs[j].gene === partner); thinned = total - rs.length }
      return { type: 'scatter', mode: 'markers', name: `${name} (${total.toLocaleString()})`, x: rs.map((r) => pr.r[r.i]), y: rs.map((r) => y(r.i)),
        customdata: rs.map((r) => r.gene), text: rs.map((r) => r.gene), hovertemplate: '%{text}<br>r = %{x:.3f}<br>−log10 FDR = %{y:.2f}<br><i>click: scatter with ' + res.gene + '</i><extra></extra>',
        marker: { size: rs.map((r) => (r.gene === partner ? 13 : selV.has(r.gene) ? 9 : k === 'ns' ? 4 : 6)), color, opacity: k === 'ns' ? 0.45 : 0.85,
          line: { width: rs.map((r) => (r.gene === partner ? 2.5 : selV.has(r.gene) ? 1.5 : 0)), color: '#111827' } } }
    })
    return { plotData: traces, thinned }
  }, [res, rows, plotMethod, selV, rc, partner])
  const cut = { dash: 'dot', color: rc.up, width: 1 }

  const tableRows = useMemo(() => {
    const src = sel.size ? rows.filter((r) => sel.has(r.gene)) : rows
    return [...src].sort((a, b) => Math.abs(b.meanR) - Math.abs(a.meanR)).slice(0, 200)
  }, [rows, sel])
  const fullTable = () => rows.map((r) => ({ gene: r.gene, ...Object.fromEntries(r.per.flatMap((x) => [[`${x.m}_r`, x.r], [`${x.m}_p`, res!.results[x.m]!.p[r.i]], [`${x.m}_padj`, x.padj]])), consensus: r.consensus === 'pos' ? 'positive' : r.consensus === 'neg' ? 'negative' : '' }))
  const tsv = () => {
    const t = fullTable()
    const cols = Object.keys(t[0] ?? { gene: '' })
    return [cols.join('\t'), ...t.map((r) => cols.map((c) => tsvCell((r as Record<string, unknown>)[c] ?? '')).join('\t'))].join('\n')
  }
  const methodBoxes = (
    <Field label="Methods">
      <div className="row">
        {CORR_METHODS.map((m) => (
          <label key={m} className="row"><input type="checkbox" checked={methods.includes(m)}
            onChange={(e) => setMethods((cur) => (e.target.checked ? CORR_METHODS.filter((x) => x === m || cur.includes(x)) : cur.filter((x) => x !== m)))} /> {CORR_LABEL[m].split(' ')[0]}</label>
        ))}
      </div>
    </Field>
  )
  const partnerRow = partner ? byGene.get(partner) : undefined

  return (
    <div className="stack-sm">
      <Segmented label="Correlate" value={mode} onChange={setMode}
        options={[{ value: 'all', label: 'One gene vs all genes' }, { value: 'pair', label: 'Two genes' }]} />
      {mode === 'pair' && (
        <>
          <p className="muted small">Correlation of two genes across the samples, by each method, with a scatter plot of their expression.</p>
          <div className="row wrap">
            <Field label="Gene 1 (x)">
              <input className="input" list="pair-genes-1" value={gene} placeholder="e.g. FKBP5" onChange={(e) => setGene(e.target.value)} />
              <GeneSuggest id="pair-genes-1" ids={ids} value={gene} />
            </Field>
            <Field label="Gene 2 (y)">
              <input className="input" list="pair-genes-2" value={gene2} placeholder="e.g. PER1" onChange={(e) => setGene2(e.target.value)}
                onKeyDown={(e) => { if (e.key === 'Enter' && gene.trim() && gene2.trim()) setPairAsked([gene.trim(), gene2.trim()]) }} />
              <GeneSuggest id="pair-genes-2" ids={ids} value={gene2} />
            </Field>
            {methodBoxes}
            <button className="btn btn-primary align-end" disabled={!gene.trim() || !gene2.trim() || !methods.length} onClick={() => setPairAsked([gene.trim(), gene2.trim()])}>Correlate</button>
          </div>
          {pairAsked && <PairPlot text={text} fileName={fileName} log={log} geneA={pairAsked[0]} geneB={pairAsked[1]} methods={methods} groups={groups} />}
        </>
      )}
      {mode === 'all' && (
        <>
          <p className="muted small">Correlate one gene with every other gene across the samples, with up to three methods. Genes that pass the cut-offs with all chosen methods, in the same direction, are the <strong>consensus</strong> co-expressed genes. <strong>Click a gene</strong> on the plot or in the table to see the two genes' expression across the samples.</p>
          <div className="row wrap">
            <Field label="Gene of interest">
              <input className="input" list="corr-genes" value={gene} placeholder="e.g. FKBP5" onChange={(e) => setGene(e.target.value)} onKeyDown={(e) => { if (e.key === 'Enter' && gene.trim() && methods.length) run() }} />
              <GeneSuggest id="corr-genes" ids={ids} value={gene} />
            </Field>
            {methodBoxes}
            <button className="btn btn-primary align-end" disabled={busy || !gene.trim() || !methods.length} onClick={run}>{busy ? 'Correlating…' : 'Correlate'}</button>
          </div>
          <ErrorNote error={err} />
          {busy && <Spinner label={`Correlating ${gene} with every gene`} />}
          {res && (
            <>
              <div className="row wrap small">
                <span><strong>{res.gene}</strong> vs {res.tested.toLocaleString()} genes · {res.n} samples{res.constant ? ` · ${res.constant} constant genes skipped` : ''}</span>
                <label>|r| ≥ <input className="input input-sm input-narrow" type="number" min={0} max={1} step={0.05} value={minR} onChange={(e) => setMinR(Math.min(1, Math.max(0, Number(e.target.value))))} /></label>
                <label>FDR ≤ <select className="input input-sm" value={maxQ} onChange={(e) => setMaxQ(Number(e.target.value))}>{[0.01, 0.05, 0.1, 0.25, 1].map((q) => <option key={q} value={q}>{q}</option>)}</select></label>
                <span className="toolbar-spacer" />
                <PaletteSelect />
                {res.methods.length > 1 && (
                  <label>Plot <select className="input input-sm" value={plotMethod} onChange={(e) => setPlotMethod(e.target.value as CorrMethod)}>{res.methods.map((m) => <option key={m} value={m}>{CORR_LABEL[m]}</option>)}</select></label>
                )}
              </div>
              <Note kind="info">{res.note}</Note>
              <div className="row wrap">
                <span className="small"><strong>Consensus</strong> ({res.methods.map((m) => CORR_LABEL[m].split(' ')[0]).join(' + ')}): <span style={{ color: rc.up }}>{counts.pos.toLocaleString()} positive</span>, <span style={{ color: rc.down }}>{counts.neg.toLocaleString()} negative</span></span>
                <span className="toolbar-spacer" />
                <button className="btn btn-ghost btn-sm" onClick={() => pick('pos')}>Positive</button>
                <button className="btn btn-ghost btn-sm" onClick={() => pick('neg')}>Negative</button>
                <button className="btn btn-ghost btn-sm" onClick={() => pick('both')}>Both</button>
                <button className="btn btn-ghost btn-sm" onClick={() => setSel(new Set())}>Clear</button>
              </div>
              <Plot height={420} dragmode="select" exportName={`flame_correlation_${res.gene}`} data={plotData as never}
                onSelect={(sid, add) => setSel((cur) => (add ? new Set([...cur, ...sid]) : new Set(sid)))}
                onPointClick={showPartner}
                layout={{ xaxis: { title: { text: `${CORR_LABEL[plotMethod]} with ${res.gene}` }, range: [-1.05, 1.05] }, yaxis: { title: { text: '−log10 FDR' } }, legend: { orientation: 'h', y: -0.2 },
                  shapes: [...[-minR, minR].map((x) => ({ type: 'line', x0: x, x1: x, yref: 'paper', y0: 0, y1: 1, line: cut })),
                    ...(maxQ < 1 ? [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: -Math.log10(maxQ), y1: -Math.log10(maxQ), line: cut }] : [])] } as never} />
              <p className="muted small">Click a point to see that gene against {res.gene}; drag a box or lasso (or Shift-click) to select genes.{thinned > 0 ? ` ${thinned.toLocaleString()} non-consensus genes are not drawn, to keep the plot fast; all are in the table and downloads.` : ''}</p>
              <div ref={pairRef}>
                {partner && (
                  <div className="card stack-sm">
                    <div className="row"><h3 className="save-heading">{res.gene} vs {partner}</h3><span className="toolbar-spacer" />
                      <button className="btn btn-ghost btn-sm" onClick={() => onBoxplots([res.gene, partner])}>Boxplots →</button>
                      <button className="btn btn-ghost btn-sm" aria-label="Close" onClick={() => setPartner('')}>×</button></div>
                    <PairPlot text={text} fileName={fileName} log={log} geneA={res.gene} geneB={partner} methods={res.methods} groups={groups}
                      fdr={partnerRow ? Object.fromEntries(partnerRow.per.map((x) => [x.m, x.padj])) : undefined} />
                  </div>
                )}
              </div>
              <div className="toolbar">
                <span className="small"><strong>{sel.size ? `${sel.size.toLocaleString()} selected` : 'All genes'}</strong>, strongest first{tableRows.length === 200 ? ' (top 200 shown)' : ''}</span>
                <span className="toolbar-spacer" />
                <button className="btn btn-secondary btn-sm" disabled={!sel.size} title="Open these genes, with the gene of interest, in the Boxplots tab"
                  onClick={() => onBoxplots([res.gene, ...tableRows.filter((r) => sel.has(r.gene)).slice(0, 11).map((r) => r.gene)])}>Boxplots of top selected →</button>
                <CopyButton label="Copy genes" text={(sel.size ? [...sel] : rows.map((r) => r.gene)).join('\n')} />
                <button className="btn btn-ghost btn-sm" onClick={() => saveXlsx([{ name: `Correlation ${res.gene}`.slice(0, 31), rows: fullTable() }], `flame_correlation_${res.gene}.xlsx`)}>Excel</button>
                <button className="btn btn-ghost btn-sm" onClick={() => saveText(tsv(), `flame_correlation_${res.gene}.tsv`, 'text/tab-separated-values')}>TSV</button>
              </div>
              <div className="table-scroll" style={{ maxHeight: 320, overflow: 'auto' }}>
                <table className="table compact">
                  <thead><tr><th>Gene</th>{res.methods.map((m) => <th key={m} className="num" colSpan={2}>{CORR_LABEL[m]} · FDR</th>)}<th>Consensus</th></tr></thead>
                  <tbody>{tableRows.map((r) => (
                    <tr key={r.gene} className={partner === r.gene ? 'selected' : undefined}>
                      <td><button type="button" className="link-btn" onClick={() => showPartner(r.gene)}>{r.gene}</button></td>
                      {r.per.map((x) => <Fragment key={x.m}><td className="num">{x.r.toFixed(3)}</td><td className="num muted">{fmtP(x.padj)}</td></Fragment>)}
                      <td style={{ color: r.consensus === 'pos' ? rc.up : r.consensus === 'neg' ? rc.down : undefined }}>{r.consensus === 'pos' ? 'Positive' : r.consensus === 'neg' ? 'Negative' : `${r.passCount}/${r.per.length} methods`}</td>
                    </tr>
                  ))}</tbody>
                </table>
              </div>
              <h3 className="save-heading">Save the selected genes as a list</h3>
              <SaveList ids={[...sel].slice(0, 3000)} defaultName={`Co-expressed with ${res.gene}`} organism={organism} setOrganism={setOrganism}
                idFormat={idFormat} setIdFormat={setIdFormat} meta={{ method: 'expression-correlation', gene: res.gene, methods: res.methods, minR, maxQ }} onCreated={onCreated} />
              <div className="note note-info">
                <strong>Use every gene instead of a selection?</strong> Save all genes ranked by their {CORR_LABEL[plotMethod]} with {res.gene} and run GSEA on it — no cut-offs needed.
                <div className="row end">
                  <button className="btn btn-secondary btn-sm" disabled={create.isPending} onClick={() =>
                    create.mutate({ name: `Correlation with ${res.gene} (${plotMethod})`, ids: res.genes, scores: res.results[plotMethod]!.r, organismTaxId: organism, idFormat, uniqueName: true,
                      meta: { method: 'expression-correlation-ranked', gene: res.gene, correlation: plotMethod } }, { onSuccess: onCreated })}>
                    Save as ranked list ({res.genes.length.toLocaleString()} genes)
                  </button>
                </div>
                <ErrorNote error={create.error} />
              </div>
            </>
          )}
        </>
      )}
    </div>
  )
}

/**
 * Assign samples to groups: tick rows (Shift-click for a range) or select by name, then send the
 * ticked samples to a group in one go; each row also has its own group menu.
 */
function SampleGroupPicker({ samples, names, colors, groupOf, onAssign, children }: {
  samples: string[]; names: string[]; colors: string[]
  /** Group index of a sample (-1 = not used). */
  groupOf: (s: string) => number
  onAssign: (samples: string[], group: number) => void
  /** Extra controls shown above the list (e.g. group names). */
  children?: React.ReactNode
}) {
  const [show, setShow] = useState(true)
  const [checked, setChecked] = useState<Set<string>>(new Set())
  const [match, setMatch] = useState('')
  const lastTick = useRef<number | null>(null)
  const counts = names.map((_, i) => samples.filter((s) => groupOf(s) === i).length)
  const unused = samples.length - counts.reduce((a, b) => a + b, 0)
  // Tick one sample, or with Shift every sample between it and the last one ticked.
  const tick = (idx: number, range: boolean) => {
    setChecked((cur) => {
      const next = new Set(cur)
      if (range && lastTick.current != null) {
        const [lo, hi] = [Math.min(lastTick.current, idx), Math.max(lastTick.current, idx)]
        for (let k = lo; k <= hi; k++) next.add(samples[k])
      } else if (cur.has(samples[idx])) next.delete(samples[idx])
      else next.add(samples[idx])
      return next
    })
    lastTick.current = idx
  }
  const assign = (g: number) => { onAssign([...checked], g); setChecked(new Set()); setMatch('') }
  return (
    <div className="card stack-sm">
      <div className="row wrap small">
        <strong>Samples in groups</strong>
        {names.map((n, i) => <span key={i}><i className="swatch" style={{ background: colors[i % colors.length] }} /> {n}: {counts[i]}</span>)}
        <span className="muted">Not used: {unused}</span>
        <span className="toolbar-spacer" />
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => setShow((v) => !v)}>{show ? 'Hide samples' : 'Show samples'}</button>
      </div>
      {show && (
        <>
          {children}
          <div className="row wrap">
            <input className="input input-sm" style={{ minWidth: 200 }} value={match} placeholder="Select samples containing… (e.g. dex)" aria-label="Select samples whose name contains this text"
              onChange={(e) => { const q = e.target.value; setMatch(q); setChecked(q.trim() ? new Set(samples.filter((s) => s.toLowerCase().includes(q.trim().toLowerCase()))) : new Set()) }} />
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => setChecked(new Set(samples))}>All</button>
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => { setChecked(new Set()); setMatch('') }}>None</button>
            <span className="small muted">{checked.size} ticked → put in</span>
            {names.map((n, i) => (
              <button key={i} type="button" className="btn btn-secondary btn-sm" disabled={!checked.size} style={{ borderColor: colors[i % colors.length] }} onClick={() => assign(i)}>{n}</button>
            ))}
            <button type="button" className="btn btn-ghost btn-sm" disabled={!checked.size} onClick={() => assign(-1)}>Not used</button>
          </div>
          <div className="table-scroll" style={{ maxHeight: 300, overflow: 'auto' }}>
            <table className="table compact">
              <thead><tr>
                <th><input type="checkbox" aria-label="Tick all samples" checked={checked.size === samples.length && samples.length > 0}
                  ref={(el) => { if (el) el.indeterminate = checked.size > 0 && checked.size < samples.length }}
                  onChange={(e) => setChecked(e.target.checked ? new Set(samples) : new Set())} /></th>
                <th>Sample</th><th>Group</th>
              </tr></thead>
              <tbody>{samples.map((s, idx) => {
                const g = groupOf(s)
                return (
                  <tr key={s} className={checked.has(s) ? 'selected' : undefined} style={{ cursor: 'pointer' }} onClick={(e) => tick(idx, e.shiftKey)}>
                    <td><input type="checkbox" aria-label={`Tick ${s}`} checked={checked.has(s)} onClick={(e) => e.stopPropagation()} onChange={(e) => tick(idx, (e.nativeEvent as MouseEvent).shiftKey)} /></td>
                    <td>{s}</td>
                    <td onClick={(e) => e.stopPropagation()}>
                      <i className="swatch" style={{ background: g >= 0 ? colors[g % colors.length] : 'transparent', border: g >= 0 ? undefined : '1px dashed #9CA3AF' }} />{' '}
                      <select className="input input-sm" aria-label={`Group of ${s}`} value={g} onChange={(e) => onAssign([s], Number(e.target.value))}>
                        {names.map((n, i) => <option key={i} value={i}>{n}</option>)}<option value={-1}>Not used</option>
                      </select>
                    </td>
                  </tr>
                )
              })}</tbody>
            </table>
          </div>
          <p className="muted small">Tick samples (click a row; Shift-click ticks a range) or type part of their names, then press a group button. Each sample's menu changes it alone.</p>
        </>
      )}
    </div>
  )
}

// ---- Boxplots of genes by 2-3 sample groups, with tests ----
interface BoxSetup { genes: string[]; nGroups: 2 | 3; names: string[]; assign: Record<string, number>; test: 'parametric' | 'rank'
  /** Colours chosen per group; empty = from the palette. */
  colors?: string[] }

/** Initial boxplot groups: the A/B groups of “Compare two groups”, else guessed from sample-name stems. */
function initialBoxSetup(samples: string[], groups: Record<string, 'A' | 'B' | ''>): BoxSetup {
  const fromVolcano = samples.some((s) => groups[s] === 'A') && samples.some((s) => groups[s] === 'B')
  if (fromVolcano) return { genes: [], nGroups: 2, names: ['Group A', 'Group B', 'Group C'], test: 'parametric', assign: Object.fromEntries(samples.map((s) => [s, groups[s] === 'A' ? 0 : groups[s] === 'B' ? 1 : -1])) }
  const stem = (s: string) => s.replace(/[_.\- ]?(rep)?\d+$/i, '')
  const stems = [...new Set(samples.map(stem))]
  const ok = stems.length >= 2 && stems.length <= 3
  return { genes: [], nGroups: stems.length === 3 ? 3 : 2, test: 'parametric', names: ok ? [...stems, 'Group C'].slice(0, 3) : ['Group A', 'Group B', 'Group C'],
    assign: Object.fromEntries(samples.map((s) => [s, ok ? stems.indexOf(stem(s)) : -1])) }
}

function BoxplotsView({ text, fileName, log, samples, setup, setSetup }: {
  text: string; fileName: string; log: boolean | null; samples: string[]; setup: BoxSetup; setSetup: (f: (s: BoxSetup) => BoxSetup) => void
}) {
  const [draft, setDraft] = useState('')
  const [vals, setVals] = useState<GeneValues | null>(null)
  const [err, setErr] = useState<unknown>(null)
  const [busy, setBusy] = useState(false)
  const pm = usePaletteMode()
  const palette = useMemo(() => roleColors(pm).series, [pm])
  const ids = useTableGeneIds(text)
  const { genes, nGroups, names, assign, test } = setup
  const series = useMemo(() => palette.map((c, i) => setup.colors?.[i] || c), [palette, setup.colors])
  const custom = (setup.colors ?? []).some(Boolean)
  const groupNames = names.slice(0, nGroups).map((n, i) => n.trim() || `Group ${'ABC'[i]}`)
  const members = groupNames.map((_, i) => samples.filter((s) => assign[s] === i))
  const sizesOk = members.every((m) => m.length >= 2)
  const dupNames = new Set(groupNames.map((n) => n.toLowerCase())).size < groupNames.length

  const addGenes = (raw: string) => {
    const add = raw.split(/[\s,;]+/).map((g) => g.trim()).filter(Boolean)
    if (add.length) setSetup((s) => ({ ...s, genes: [...new Set([...s.genes, ...add])].slice(0, 24) }))
    setDraft('')
  }
  const key = JSON.stringify([genes, groupNames, members, test, log])
  useEffect(() => {
    if (!genes.length || dupNames) { setVals(null); return }
    setBusy(true); setErr(null)
    const body = { text, fileName, log, genes, test, groups: Object.fromEntries(groupNames.map((n, i) => [n, members[i]]).filter(([, m]) => (m as string[]).length)) }
    api<GeneValues>('/inputs/expression/genes', { body }).then(setVals).catch((e) => { setVals(null); setErr(e) }).finally(() => setBusy(false))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, text, fileName])

  const shownGroups = vals ? Object.keys(vals.groups) : []
  const boxData = useMemo(() => {
    if (!vals) return []
    const sets: [string, string[]][] = shownGroups.length ? shownGroups.map((g) => [g, vals.groups[g]]) : [['All samples', vals.samples]]
    return sets.map(([g, ss], j) => {
      const xs: string[] = []; const ys: number[] = []; const tx: string[] = []
      vals.genes.forEach((gn) => vals.samples.forEach((s, si) => { if (ss.includes(s)) { xs.push(gn.gene); ys.push(gn.values[si]); tx.push(s) } }))
      return { type: 'box', name: `${g} (n = ${ss.length})`, x: xs, y: ys, text: tx, boxpoints: 'all', jitter: 0.4, pointpos: 0,
        marker: { color: series[j % series.length], size: 6 }, line: { width: 1.5 }, hovertemplate: `%{text}<br>%{x}: %{y:.2f}<extra>${g}</extra>` }
    })
  }, [vals, shownGroups.join('|'), series]) // eslint-disable-line react-hooks/exhaustive-deps
  const annotations = useMemo(() => (!vals || !vals.testName ? [] : vals.genes.map((g) => ({
    x: g.gene, y: 1, yref: 'paper', yanchor: 'bottom', showarrow: false, font: { size: 11 }, text: `p = ${fmtP(g.pvalue)}` }))), [vals])
  const pairs = vals?.genes[0]?.pairwise ?? []
  const tableRows = (vals?.genes ?? []).map((g) => ({
    Gene: g.gene, ...Object.fromEntries(shownGroups.map((n) => [`Mean ${n}`, g.means[n]])),
    [shownGroups.length > 2 ? 'Overall p' : 'p-value']: g.pvalue,
    ...(shownGroups.length > 2 ? Object.fromEntries(g.pairwise.flatMap((p) => [[`${p.a} vs ${p.b} log2FC`, p.log2FC], [`${p.a} vs ${p.b} p (Holm)`, p.padj]])) : { log2FC: g.pairwise[0]?.log2FC }),
  }))
  const unit = vals?.logged ? 'log2(x + 1)' : 'expression'

  return (
    <div className="stack-sm">
      <p className="muted small">Compare genes between two or three groups of samples. Each box is one group; points are samples. Two groups: Welch t-test (or Wilcoxon). Three groups: an overall test (Welch ANOVA or Kruskal–Wallis) plus every pair of groups, Holm-adjusted.</p>
      <div className="row wrap">
        <Field label="Genes (up to 24)">
          <input className="input" list="box-genes" value={draft} placeholder="Type a gene and press Enter, or paste several" onChange={(e) => setDraft(e.target.value)}
            onKeyDown={(e) => { if (e.key === 'Enter') addGenes(draft) }} onPaste={(e) => { const t = e.clipboardData.getData('text'); if (/[\s,;]/.test(t.trim())) { e.preventDefault(); addGenes(t) } }} />
          <GeneSuggest id="box-genes" ids={ids} value={draft} />
        </Field>
        <button className="btn btn-secondary align-end" disabled={!draft.trim()} onClick={() => addGenes(draft)}>Add</button>
        <Field label="Groups">
          <Segmented label="Number of groups" value={String(nGroups)} onChange={(v) => setSetup((s) => ({ ...s, nGroups: Number(v) as 2 | 3, assign: Number(v) === 2 ? Object.fromEntries(Object.entries(s.assign).map(([k, g]) => [k, g === 2 ? -1 : g])) : s.assign }))}
            options={[{ value: '2', label: '2 groups' }, { value: '3', label: '3 groups' }]} />
        </Field>
        <Field label="Test">
          <select className="input" value={test} onChange={(e) => setSetup((s) => ({ ...s, test: e.target.value as BoxSetup['test'] }))}>
            <option value="parametric">{nGroups === 3 ? 'Welch ANOVA + Welch t-tests' : 'Welch t-test'}</option>
            <option value="rank">{nGroups === 3 ? 'Kruskal–Wallis + Wilcoxon' : 'Wilcoxon rank-sum'}</option>
          </select>
        </Field>
      </div>
      {genes.length > 0 && (
        <div className="row wrap small">{genes.map((g) => (
          <span key={g} className="chip">{g} <button type="button" className="btn btn-ghost btn-sm" aria-label={`Remove ${g}`} onClick={() => setSetup((s) => ({ ...s, genes: s.genes.filter((x) => x !== g) }))}>×</button></span>
        ))}<button type="button" className="btn btn-ghost btn-sm" onClick={() => setSetup((s) => ({ ...s, genes: [] }))}>Clear genes</button></div>
      )}
      <SampleGroupPicker samples={samples} names={groupNames} colors={series}
        groupOf={(s) => { const g = assign[s] ?? -1; return g < nGroups ? g : -1 }}
        onAssign={(ss, g) => setSetup((st) => ({ ...st, assign: { ...st.assign, ...Object.fromEntries(ss.map((x) => [x, g])) } }))}>
        <div className="row wrap">
          {groupNames.map((_, i) => (
            <Field key={i} label={`Group ${i + 1}: name and colour`}>
              <span className="row">
                <input className="input input-sm" value={names[i]} maxLength={40} onChange={(e) => setSetup((st) => ({ ...st, names: st.names.map((x, j) => (j === i ? e.target.value : x)) }))} />
                <input type="color" aria-label={`Colour of ${groupNames[i]}`} value={series[i]} style={{ width: 32, height: 28, padding: 0, border: 'none', background: 'none', cursor: 'pointer' }}
                  onChange={(e) => { const c = e.target.value; setSetup((st) => { const cs = [...(st.colors ?? [])]; cs[i] = c; return { ...st, colors: cs } }) }} />
              </span>
            </Field>
          ))}
        </div>
      </SampleGroupPicker>
      {dupNames && <Note kind="warning">Give each group a different name.</Note>}
      {!sizesOk && <Note kind="info">Put at least two samples in each group to get p-values.</Note>}
      <ErrorNote error={err} />
      {!genes.length && <p className="muted small">Add genes above, or send them from the <em>Correlation</em> tab.</p>}
      {busy && !vals && <Spinner label="Loading expression values" />}
      {vals && (
        <>
          {vals.missing.length > 0 && <Note kind="warning">Not in the table: {vals.missing.join(', ')}</Note>}
          <div className="toolbar">
            <span className="small muted">Colours</span>
            <PaletteSelect />
            {custom && <button type="button" className="btn btn-ghost btn-sm" title="Drop the colours picked per group and use the palette again" onClick={() => setSetup((st) => ({ ...st, colors: [] }))}>Reset to palette</button>}
            <span className="muted small">or pick a colour per group under <em>Samples in groups</em></span>
          </div>
          <Plot height={400} exportName={`flame_boxplots_${vals.genes.map((g) => g.gene).slice(0, 3).join('_')}`} data={boxData as never}
            layout={{ boxmode: 'group', yaxis: { title: { text: unit } }, legend: { orientation: 'h', y: -0.15 }, margin: { l: 60, r: 20, t: vals.testName ? 30 : 10, b: 60 }, annotations: annotations as never }} />
          {vals.testName && <p className="muted small">p above each gene: {shownGroups.length > 2 ? (vals.test === 'rank' ? 'Kruskal–Wallis test' : 'Welch one-way ANOVA') : vals.testName} ({shownGroups.join(' vs ')}), not adjusted across genes.{members.some((m) => m.length < 4) && vals.test === 'rank' ? ' With so few samples per group, rank tests cannot reach small p-values (3 vs 3: p ≥ 0.1).' : ''}</p>}
          {vals.testName && (
            <>
              <div className="toolbar">
                <span className="small"><strong>Tests</strong> · {vals.testName}</span>
                <span className="toolbar-spacer" />
                <button className="btn btn-ghost btn-sm" onClick={() => saveXlsx([{ name: 'Boxplot tests', rows: tableRows }], 'flame_boxplot_tests.xlsx')}>Excel</button>
              </div>
              <div className="table-scroll">
                <table className="table compact">
                  <thead><tr><th>Gene</th>{shownGroups.map((n) => <th key={n} className="num">Mean {n}</th>)}<th className="num">{shownGroups.length > 2 ? 'Overall p' : 'p-value'}</th>
                    {shownGroups.length > 2 ? pairs.map((p) => <th key={`${p.a}|${p.b}`} className="num">{p.a} vs {p.b}<div className="muted small">log2FC · p (Holm)</div></th>) : <th className="num">log2FC ({shownGroups[0]} − {shownGroups[1]})</th>}</tr></thead>
                  <tbody>{vals.genes.map((g) => (
                    <tr key={g.gene}><td>{g.gene}</td>{shownGroups.map((n) => <td key={n} className="num">{g.means[n]?.toFixed(2)}</td>)}
                      <td className="num"><strong>{fmtP(g.pvalue)}</strong></td>
                      {shownGroups.length > 2 ? g.pairwise.map((p) => <td key={`${p.a}|${p.b}`} className="num">{p.log2FC.toFixed(2)} · {fmtP(p.padj)}</td>) : <td className="num">{g.pairwise[0]?.log2FC.toFixed(2)}</td>}
                    </tr>
                  ))}</tbody>
                </table>
              </div>
            </>
          )}
        </>
      )}
    </div>
  )
}

interface ActivityResult { method: string; citation: string; samples: string[]; pathways: string[]; scores: number[][]; totalPathways: number }

/** Per-sample pathway scores (ssGSEA, combined z-score or PROGENy) as an interactive heatmap. */
function PathwayActivity({ text, fileName, log, organism }: { text: string; fileName: string; log: boolean | null; organism: number }) {
  const [method, setMethod] = useState<'ssgsea' | 'zscore' | 'progeny'>('ssgsea')
  const [source, setSource] = useState('HALLMARK')
  const [res, setRes] = useState<ActivityResult | null>(null)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [scale, setScale] = useState(true)
  const [sel, setSel] = useState<string[]>([])
  const run = () => {
    setBusy(true); setErr(null); setSel([])
    api<ActivityResult>('/inputs/expression/activity', { body: { text, fileName, log, method, sources: [source], organismTaxId: organism } })
      .then(setRes).catch(setErr).finally(() => setBusy(false))
  }
  const z = useMemo(() => {
    if (!res) return []
    if (!scale) return res.scores
    return res.scores.map((row) => {
      const m = row.reduce((a, b) => a + b, 0) / row.length
      const sd = Math.sqrt(row.reduce((a, b) => a + (b - m) ** 2, 0) / Math.max(1, row.length - 1)) || 1
      return row.map((v) => Math.round(((v - m) / sd) * 100) / 100)
    })
  }, [res, scale])
  const tsv = res ? [['pathway', ...res.samples].map(tsvCell).join('\t'), ...res.pathways.map((p, i) => [p, ...res.scores[i]].map(tsvCell).join('\t'))].join('\n') : ''
  return (
    <div className="stack-sm">
      <p className="muted small">Scores each pathway in each sample, so you can see which programmes are on or off per sample or group — no comparison needed.</p>
      <div className="row wrap">
        <Field label="Method">
          <select className="input" value={method} onChange={(e) => setMethod(e.target.value as typeof method)}>
            <option value="ssgsea">ssGSEA (rank-based)</option><option value="zscore">Combined z-score</option><option value="progeny">PROGENy (14 signalling pathways)</option>
          </select>
        </Field>
        {method !== 'progeny' && (
          <Field label="Gene sets">
            <select className="input" value={source} onChange={(e) => setSource(e.target.value)}>
              <option value="HALLMARK">MSigDB Hallmark (50)</option><option value="KEGG">KEGG</option><option value="REAC">Reactome</option><option value="WP">WikiPathways</option><option value="GO:BP">GO biological process</option>
            </select>
          </Field>
        )}
        <button className="btn btn-primary align-end" disabled={busy} onClick={run}>{busy ? 'Scoring…' : res ? 'Score again' : 'Score pathways'}</button>
      </div>
      <ErrorNote error={err} />
      {busy && <Spinner label={method === 'progeny' ? 'Scoring 14 pathways' : 'Scoring gene sets in every sample'} />}
      {res && (
        <>
          <div className="toolbar">
            <label className="row"><input type="checkbox" checked={scale} onChange={(e) => setScale(e.target.checked)} /> Scale each pathway across samples</label>
            <span className="muted small">{res.pathways.length} most variable of {res.totalPathways} pathways · click rows to select</span>
            <span className="toolbar-spacer" />
            <CopyButton label="Copy names" text={(sel.length ? sel : res.pathways).join('\n')} />
            <button className="btn btn-ghost btn-sm" onClick={() => saveXlsx([{ name: `Activity (${res.method})`, rows: res.pathways.map((pw, i) => ({ Pathway: pw, ...Object.fromEntries(res.samples.map((sm, j) => [sm, res.scores[i][j]])) })) }],
              `flame_pathway_activity_${res.method}.xlsx`)}>Excel</button>
            <button className="btn btn-ghost btn-sm" onClick={() => saveText(tsv, `flame_pathway_activity_${res.method}.tsv`, 'text/tab-separated-values')}>TSV</button>
          </div>
          <Plot height={Math.max(320, res.pathways.length * 16 + 120)} data={[{
            type: 'heatmap', x: res.samples, y: res.pathways.map((p) => (p.length > 48 ? `${p.slice(0, 47)}…` : p)), z,
            customdata: res.pathways.map((p) => res.samples.map(() => p)) as never, colorscale: 'RdBu', reversescale: true, zmid: 0,
            hovertemplate: '%{customdata}<br>%{x}: %{z}<extra></extra>', colorbar: { title: { text: scale ? 'z-score' : 'score' } },
          } as never]} layout={{ yaxis: { autorange: 'reversed', automargin: true }, margin: { l: 10, r: 10, t: 10, b: 100 } }}
            onSelect={(keys, add) => setSel((c) => (add ? [...new Set([...c, ...keys])] : keys.slice(0, 1)))} />
          {sel.length > 0 && <p className="small"><strong>Selected:</strong> {sel.join(' · ')} <button className="btn btn-ghost btn-sm" onClick={() => setSel([])}>Clear</button></p>}
          <p className="muted small">{res.citation}</p>
        </>
      )}
    </div>
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
              <a className="btn btn-ghost btn-sm" href={`${API_BASE}/inputs/examples/mirna/file`}>Example file</a>
            </div>
          </div>
          <div className="stack-sm">
            <OrganismPicker value={organism} onChange={setOrganism} />
            <button className="btn btn-primary" disabled={!text.trim() || busy} onClick={run}>{busy ? 'Finding targets…' : 'Find target genes'}</button>
            <p className="muted small">Targets are miRDB v6 predictions with MirTarget score &gt; 80 (MSigDB C3 MIR:MIRDB). For organisms other than human, MSigDB maps the human predictions through orthologs.</p>
          </div>
        </div>
        <ExampleSource text={ex.source} loading={ex.loading} />
        <ErrorNote error={err} />
        {busy && <BusyBar label="Finding the genes these microRNAs target (miRDB)" expect={25}
          hint="The first search for an organism downloads the miRDB target sets, so it takes longer; later searches are quick." />}
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
