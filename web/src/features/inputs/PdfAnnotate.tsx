import { useEffect, useMemo, useRef, useState } from 'react'
import type { PDFDocumentProxy } from 'pdfjs-dist'
import workerUrl from 'pdfjs-dist/build/pdf.worker.min.mjs?url'
import { api, saveText } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { BusyBar, ErrorNote, Note, OrganismPicker } from '../../components/ui'
import { toCsv } from '../../lib/format'
import { annotatePdf, findMentions, locate, mergeBoxes, textLayerString, type PdfMark, type RGB } from '../../lib/pdfText'
import { SaveList } from './InputModal'

type Kind = 'genes' | 'chemicals' | 'organisms' | 'process' | 'component' | 'function' | 'tissues' | 'diseases' | 'environments' | 'phenotypes'
interface Entity { name: string; kind: Kind; id: string; url: string }

const KINDS: { id: Kind; label: string; color: string }[] = [
  { id: 'genes', label: 'Genes / proteins', color: '#2563eb' },
  { id: 'chemicals', label: 'Chemicals', color: '#16a34a' },
  { id: 'diseases', label: 'Diseases', color: '#dc2626' },
  { id: 'tissues', label: 'Tissues', color: '#0891b2' },
  { id: 'organisms', label: 'Organisms', color: '#a16207' },
  { id: 'process', label: 'GO processes', color: '#9333ea' },
  { id: 'component', label: 'GO components', color: '#c026d3' },
  { id: 'function', label: 'GO functions', color: '#db2777' },
  { id: 'phenotypes', label: 'Phenotypes', color: '#ea580c' },
  { id: 'environments', label: 'Environments', color: '#65a30d' },
]
const KIND = Object.fromEntries(KINDS.map((k) => [k.id, k])) as Record<Kind, (typeof KINDS)[number]>
const rgb = (hex: string): RGB => [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16) / 255) as RGB
const MAX_PAGES = 150
const MAX_FILES = 10
const MAX_BYTES = 50 * 1024 * 1024
const BASE = `${import.meta.env.BASE_URL}pdfjs/`

/** One highlighted mention on a page: its boxes (CSS px of the shown page) and the same in PDF points. */
interface Hit { key: string; kind: Kind; name: string; boxes: { left: number; top: number; width: number; height: number }[]; pdf: [number, number, number, number][] }

/**
 * Annotate a PDF with EXTRACT: the pages are drawn exactly as in the file (PDF.js), every gene,
 * chemical, disease, tissue, organism, GO term, phenotype and environment the tagger recognises is
 * highlighted on top, and the annotated PDF (the original plus highlight notes) can be downloaded.
 * The file stays in the browser; only its text is sent to the tagger.
 */
function PdfDocument({ source, organism, kinds, active, rerun, onCreated, onStatus }: {
  source: File; organism: number; kinds: Set<Kind>; active: boolean; rerun: number
  onCreated: (l: AnalyteList) => void; onStatus: (s: { busy: boolean; entities: number | null; error: boolean }) => void
}) {
  const [file, setFile] = useState<{ name: string; bytes: Uint8Array } | null>(null)
  const [pdf, setPdf] = useState<PDFDocumentProxy | null>(null)
  const [pageTexts, setPageTexts] = useState<string[] | null>(null)
  const [entities, setEntities] = useState<Entity[] | null>(null)
  const [busy, setBusy] = useState<string | null>(null)
  const [err, setErr] = useState<unknown>(null)
  const [shown, setShown] = useState<Set<Kind>>(new Set(KINDS.map((k) => k.id)))
  const [hits, setHits] = useState<Record<number, Hit[]>>({})
  const [popup, setPopup] = useState<{ page: number; x: number; y: number; key: string } | null>(null)
  const [chosen, setChosen] = useState<Set<string>>(new Set())
  const [width, setWidth] = useState(860)
  const wrap = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const el = wrap.current
    if (!el) return
    const ro = new ResizeObserver(() => setWidth(Math.max(320, Math.min(1100, el.clientWidth - 2))))
    ro.observe(el)
    return () => ro.disconnect()
  }, [pdf, active])

  const open = async (f: File) => {
    setErr(null); setEntities(null); setHits({}); setPdf(null); setPageTexts(null); setPopup(null)
    if (f.size > MAX_BYTES) { setErr(new Error('The PDF is larger than the 50 MB limit.')); return }
    setBusy('Reading the PDF')
    try {
      const bytes = new Uint8Array(await f.arrayBuffer())
      const pdfjs = await import('pdfjs-dist')
      pdfjs.GlobalWorkerOptions.workerSrc = workerUrl
      const doc = await pdfjs.getDocument({ data: bytes.slice(), cMapUrl: `${BASE}cmaps/`, cMapPacked: true, standardFontDataUrl: `${BASE}standard_fonts/`,
        wasmUrl: `${BASE}wasm/`, iccUrl: `${BASE}iccs/` }).promise
      const texts: string[] = []
      for (let i = 1; i <= Math.min(doc.numPages, MAX_PAGES); i++) {
        const tc = await (await doc.getPage(i)).getTextContent()
        texts.push(tc.items.map((it) => ('str' in it ? it.str + (it.hasEOL ? '\n' : '') : '')).join(''))
      }
      setFile({ name: f.name, bytes }); setPdf(doc); setPageTexts(texts)
    } catch (e) {
      setErr(e instanceof Error && /password/i.test(e.message) ? new Error('This PDF is password-protected.') : e)
    } finally { setBusy(null) }
  }

  const chars = pageTexts?.reduce((s, t) => s + t.length, 0) ?? 0
  const annotate = async () => {
    if (!pageTexts) return
    setBusy('Recognising entities with the EXTRACT tagger'); setErr(null); setHits({}); setPopup(null)
    try {
      const r = await api<{ entities: Entity[] }>('/inputs/textmining/annotate', { body: { text: pageTexts.join('\n\n').slice(0, 500000), organismTaxId: organism, kinds: [...kinds] } })
      setEntities(r.entities)
      setShown(new Set(kinds))
      const genes = r.entities.filter((e) => e.kind === 'genes')
      const amb = (n: string) => new Set(genes.filter((g) => g.name.toLowerCase() === n.toLowerCase()).map((g) => g.id)).size > 1
      setChosen(new Set(genes.filter((g) => !amb(g.name)).map((g) => g.id)))
    } catch (e) { setErr(e) } finally { setBusy(null) }
  }
  useEffect(() => { if (pageTexts && chars >= 20) annotate() // annotate as soon as a PDF with text is open
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [pageTexts])
  useEffect(() => { open(source) // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [source])
  useEffect(() => { if (rerun && pageTexts && chars >= 20) annotate() // "Annotate again" with new organism or types
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [rerun])
  // Free the document's memory when its tab is closed.
  const pdfRef = useRef<PDFDocumentProxy | null>(null); pdfRef.current = pdf
  useEffect(() => () => { pdfRef.current?.loadingTask.destroy().catch(() => {}) }, [])
  const onStatusRef = useRef(onStatus); onStatusRef.current = onStatus
  useEffect(() => { onStatusRef.current({ busy: !!busy, entities: entities ? entities.length : null, error: !!err }) }, [busy, entities, err])

  // Entities by kind + name (a name can have several IDs: ambiguous).
  const byKey = useMemo(() => {
    const m = new Map<string, Entity[]>()
    for (const e of entities ?? []) { const k = `${e.kind}\u0001${e.name}`; m.set(k, [...(m.get(k) ?? []), e]) }
    return m
  }, [entities])
  const mentionCount = useMemo(() => {
    const c = new Map<string, number>()
    Object.values(hits).flat().forEach((h) => c.set(h.key, (c.get(h.key) ?? 0) + 1))
    return c
  }, [hits])
  const countByKind = useMemo(() => {
    const c = new Map<Kind, number>()
    Object.values(hits).flat().forEach((h) => c.set(h.kind, (c.get(h.kind) ?? 0) + 1))
    return c
  }, [hits])

  const geneRows = useMemo(() => {
    const m = new Map<string, { id: string; url: string; names: Set<string> }>()
    for (const e of entities ?? []) if (e.kind === 'genes') {
      const r = m.get(e.id) ?? { id: e.id, url: e.url, names: new Set<string>() }
      r.names.add(e.name); m.set(e.id, r)
    }
    return [...m.values()].map((r) => ({ ...r, names: [...r.names], ambiguous: [...r.names].every((n) => (byKey.get(`genes\u0001${n}`)?.length ?? 0) > 1) }))
  }, [entities, byKey])

  const tableRows = () => [...byKey.entries()].flatMap(([key, es]) => es.map((e) => ({
    Name: e.name, Type: KIND[e.kind].label, ID: e.id, Ambiguous: es.length > 1 ? 'yes' : '', Mentions: mentionCount.get(key) ?? 0, Link: e.url,
  })))
  const downloadPdf = async () => {
    if (!file) return
    setBusy('Writing the annotated PDF'); setErr(null)
    try {
      const marks = new Map<number, PdfMark[]>()
      for (const [p, hs] of Object.entries(hits)) {
        marks.set(Number(p) - 1, hs.filter((h) => shown.has(h.kind)).flatMap((h) => {
          const es = byKey.get(h.key) ?? []
          const note = `${h.name} (${KIND[h.kind].label})\n${es.map((e) => `${e.id}  ${e.url}`).join('\n')}${es.length > 1 ? '\nAmbiguous: the name matches several entries.' : ''}`
          return h.pdf.map((rect) => ({ rect, color: rgb(KIND[h.kind].color), text: note }))
        }))
      }
      const out = await annotatePdf(file.bytes, marks)
      const blob = new Blob([out as BlobPart], { type: 'application/pdf' })
      const a = document.createElement('a')
      a.href = URL.createObjectURL(blob); a.download = file.name.replace(/\.pdf$/i, '') + '_annotated.pdf'
      document.body.appendChild(a); a.click(); a.remove(); setTimeout(() => URL.revokeObjectURL(a.href), 5000)
    } catch (e) { setErr(e) } finally { setBusy(null) }
  }

  const pages = pdf ? Math.min(pdf.numPages, MAX_PAGES) : 0
  const pop = popup ? byKey.get(popup.key) ?? [] : []
  return (
    <div className="stack-sm pdfdoc-doc" hidden={!active}>
      {busy && <BusyBar label={busy} expect={busy.startsWith('Recognising') ? Math.min(60, 6 + chars / 8000) : 6} />}
      <ErrorNote error={err} />
      {pdf && chars < 20 && <Note kind="warning">This PDF has no text layer (probably a scan), so there is no text to annotate.</Note>}
      {entities && (
        <div className="row wrap pdfdoc-bar">
          <span className="small"><strong>{file?.name}</strong> · {pdf?.numPages} page{pdf?.numPages === 1 ? '' : 's'}{(pdf?.numPages ?? 0) > MAX_PAGES ? ` (first ${MAX_PAGES} shown)` : ''} · {entities.length.toLocaleString()} entities</span>
          <span className="toolbar-spacer" />
          <button className="btn btn-primary btn-sm" disabled={!!busy || !Object.keys(hits).length} onClick={downloadPdf}>Download annotated PDF</button>
          <button className="btn btn-ghost btn-sm" onClick={() => saveText(toCsv(tableRows(), ['Name', 'Type', 'ID', 'Ambiguous', 'Mentions', 'Link'].map((k) => ({ key: k, label: k }))), (file?.name ?? 'document').replace(/\.pdf$/i, '') + '_entities.csv', 'text/csv')}>Entities (CSV)</button>
        </div>
      )}
      {entities && (
        <div className="row wrap pdfdoc-legend" role="group" aria-label="Highlighted types">
          {KINDS.filter((k) => kinds.has(k.id)).map((k) => (
            <label key={k.id} className="check small">
              <input type="checkbox" checked={shown.has(k.id)} onChange={(e) => setShown((s) => { const n = new Set(s); if (e.target.checked) n.add(k.id); else n.delete(k.id); return n })} />
              <span className="pdfdoc-swatch" style={{ background: k.color }} /> {k.label} <span className="muted">({countByKind.get(k.id) ?? 0})</span>
            </label>
          ))}
        </div>
      )}
      {pdf && active && (
        <div className="pdfdoc-pages" ref={wrap} onClick={(e) => { if (!(e.target as Element).closest('.pdfdoc-hl, .pdfdoc-pop')) setPopup(null) }}>
          {Array.from({ length: pages }, (_, i) => (
            <PdfPage key={`${file?.name}-${i}`} pdf={pdf} pageNumber={i + 1} width={width} entities={entities} shown={shown}
              onHits={(h) => setHits((cur) => ({ ...cur, [i + 1]: h }))}
              onPick={(key, x, y) => setPopup({ page: i + 1, x, y, key })}
              popup={popup?.page === i + 1 ? (
                <div className="pdfdoc-pop" style={{ left: Math.min(popup.x, width - 300), top: popup.y + 6 }} role="group" aria-label="Entity">
                  <button className="pdfdoc-pop-x" aria-label="Close" onClick={() => setPopup(null)}>×</button>
                  <strong>{pop[0]?.name}</strong> <span className="muted small">{pop[0] && KIND[pop[0].kind].label}</span>
                  {pop.length > 1 && <div className="small muted">Ambiguous: {pop.length} entries share this name</div>}
                  <ul className="small">{pop.map((e) => <li key={e.id}>{e.url ? <a href={e.url} target="_blank" rel="noopener noreferrer">{e.id} ↗</a> : e.id}</li>)}</ul>
                  <div className="small muted">{mentionCount.get(popup.key) ?? 0} mention{mentionCount.get(popup.key) === 1 ? '' : 's'} in the document</div>
                </div>
              ) : null} />
          ))}
        </div>
      )}
      {geneRows.length > 0 && (
        <details className="pdfdoc-genes" open>
          <summary><strong>{geneRows.length} genes / proteins</strong> <span className="muted small">— save them as a list</span></summary>
          <div className="row"><button className="btn btn-ghost btn-sm" onClick={() => setChosen(new Set(geneRows.map((g) => g.id)))}>Select all</button>
            <button className="btn btn-ghost btn-sm" onClick={() => setChosen(new Set())}>None</button></div>
          <div className="table-scroll fetch-table"><table className="table compact">
            <thead><tr><th aria-label="Use" /><th>Name in the document</th><th>ID</th><th>Mentions</th><th>Note</th></tr></thead>
            <tbody>{geneRows.map((g) => (
              <tr key={g.id} className={chosen.has(g.id) ? 'selected' : ''}>
                <td><input type="checkbox" checked={chosen.has(g.id)} aria-label={`Use ${g.names.join(', ')}`} onChange={() => setChosen((s) => { const n = new Set(s); if (n.has(g.id)) n.delete(g.id); else n.add(g.id); return n })} /></td>
                <td>{g.names.join(', ')}</td>
                <td>{g.url ? <a href={g.url} target="_blank" rel="noopener noreferrer">{g.id}</a> : g.id}</td>
                <td className="num">{g.names.reduce((s, n) => s + (mentionCount.get(`genes\u0001${n}`) ?? 0), 0)}</td>
                <td className="small">{g.ambiguous && <span className="tm-flag tm-amb">ambiguous</span>}</td>
              </tr>
            ))}</tbody>
          </table></div>
          {chosen.size > 0 && <SaveList ids={[...chosen]} defaultName={`Genes from ${(file?.name ?? 'PDF').replace(/\.pdf$/i, '')}`.slice(0, 70)} organism={organism} idFormat="USERINPUT" showOrganism={false}
            meta={{ method: 'textmining-pdf', source: file?.name ?? 'PDF' }} onCreated={onCreated} />}
        </details>
      )}
    </div>
  )
}


interface Doc { id: string; file: File }

/**
 * Annotate PDFs with EXTRACT, one tab per document: the pages are drawn exactly as in the file
 * (PDF.js), every gene, chemical, disease, tissue, organism, GO term, phenotype and environment the
 * tagger recognises is highlighted on top, and each annotated PDF (the original plus highlight notes)
 * can be downloaded. The files stay in the browser; only their text is sent to the tagger.
 */
export function PdfAnnotate({ onCreated }: { onCreated: (l: AnalyteList) => void }) {
  const [organism, setOrganism] = useState(9606)
  const [kinds, setKinds] = useState<Set<Kind>>(new Set(KINDS.map((k) => k.id)))
  const [docs, setDocs] = useState<Doc[]>([])
  const [active, setActive] = useState<string | null>(null)
  const [rerun, setRerun] = useState(0)
  const [status, setStatus] = useState<Record<string, { busy: boolean; entities: number | null; error: boolean }>>({})
  const [note, setNote] = useState<string | null>(null)
  const add = (files: FileList | null) => {
    if (!files?.length) return
    const pdfs = Array.from(files).filter((f) => /\.pdf$/i.test(f.name) || f.type === 'application/pdf')
    const room = MAX_FILES - docs.length
    const taken = pdfs.slice(0, Math.max(0, room)).map((file, i) => ({ id: `${Date.now()}-${i}-${file.name}`, file }))
    setNote(pdfs.length < files.length ? 'Only PDF files can be annotated here; the others were skipped.'
      : pdfs.length > room ? `At most ${MAX_FILES} PDFs at a time; ${pdfs.length - Math.max(0, room)} were not opened.` : null)
    if (!taken.length) return
    setDocs((d) => [...d, ...taken])
    setActive(taken[0].id)
  }
  const close = (id: string) => {
    setDocs((d) => {
      const i = d.findIndex((x) => x.id === id)
      const next = d.filter((x) => x.id !== id)
      if (active === id) setActive(next[Math.min(i, next.length - 1)]?.id ?? null)
      return next
    })
    setStatus((s) => { const n = { ...s }; delete n[id]; return n })
  }
  return (
    <div className="stack-sm pdfdoc">
      <div className="row wrap between">
        <OrganismPicker value={organism} onChange={setOrganism} label="Organism (for gene names)" />
        <label className="btn btn-secondary btn-sm pdfdoc-pick">
          {docs.length ? 'Add PDFs' : 'Choose PDFs'}
          <input type="file" multiple accept="application/pdf,.pdf" aria-label="PDF files" disabled={docs.length >= MAX_FILES}
            onChange={(e) => { add(e.target.files); e.target.value = '' }} />
        </label>
      </div>
      <div className="row wrap pdfdoc-kinds" role="group" aria-label="Entity types to recognise">
        {KINDS.map((k) => (
          <label key={k.id} className="check small">
            <input type="checkbox" checked={kinds.has(k.id)} onChange={(e) => setKinds((s) => { const n = new Set(s); if (e.target.checked) n.add(k.id); else n.delete(k.id); return n })} />
            <span className="pdfdoc-swatch" style={{ background: k.color }} /> {k.label}
          </label>
        ))}
        {docs.length > 0 && <button className="btn btn-ghost btn-sm" disabled={!kinds.size} onClick={() => setRerun((x) => x + 1)}>Annotate again{docs.length > 1 ? ' (all)' : ''}</button>}
      </div>
      {!docs.length && <p className="muted small">Choose one or more PDFs (up to {MAX_FILES}); each opens in its own tab. The pages are shown exactly as in the file, with every recognised name highlighted on top; click a highlight for its database entry. The PDFs stay in your browser: only their text is sent to the EXTRACT tagger (JensenLab). Up to {MAX_PAGES} pages and 50 MB per file; scanned PDFs without a text layer are not supported yet.</p>}
      {note && <Note kind="info">{note}</Note>}
      {docs.length > 0 && (
        <div className="pdfdoc-tabs" role="tablist" aria-label="Documents">
          {docs.map((d) => {
            const st = status[d.id]
            return (
              <span key={d.id} className={`pdfdoc-tab${d.id === active ? ' active' : ''}`}>
                <button role="tab" aria-selected={d.id === active} title={d.file.name} onClick={() => setActive(d.id)}>
                  <span className="pdfdoc-tab-name">{d.file.name.replace(/\.pdf$/i, '')}</span>
                  <span className="muted small">{st?.busy ? ' …' : st?.error ? ' !' : st?.entities != null ? ` ${st.entities}` : ''}</span>
                </button>
                <button className="pdfdoc-tab-x" aria-label={`Close ${d.file.name}`} onClick={() => close(d.id)}>×</button>
              </span>
            )
          })}
        </div>
      )}
      {docs.map((d) => (
        <PdfDocument key={d.id} source={d.file} organism={organism} kinds={kinds} active={d.id === active} rerun={rerun} onCreated={onCreated}
          onStatus={(st) => setStatus((s) => (s[d.id]?.busy === st.busy && s[d.id]?.entities === st.entities && s[d.id]?.error === st.error ? s : { ...s, [d.id]: st }))} />
      ))}
    </div>
  )
}

/** One page: canvas (exact look), PDF.js text layer, and the highlights computed on it. */
function PdfPage({ pdf, pageNumber, width, entities, shown, onHits, onPick, popup }: {
  pdf: PDFDocumentProxy; pageNumber: number; width: number; entities: Entity[] | null; shown: Set<Kind>
  onHits: (h: Hit[]) => void; onPick: (key: string, x: number, y: number) => void; popup: React.ReactNode
}) {
  const box = useRef<HTMLDivElement>(null)
  const canvas = useRef<HTMLCanvasElement>(null)
  const layer = useRef<HTMLDivElement>(null)
  const [size, setSize] = useState<{ w: number; h: number } | null>(null)
  const [ready, setReady] = useState(0)
  const [hits, setHits] = useState<Hit[]>([])
  const view = useRef<import('pdfjs-dist').PageViewport | null>(null)
  const onHitsRef = useRef(onHits); onHitsRef.current = onHits

  // Draw the page and its text layer (again when the width changes).
  useEffect(() => {
    let cancelled = false
    let task: { cancel: () => void } | null = null
    ;(async () => {
      const pdfjs = await import('pdfjs-dist')
      const page = await pdf.getPage(pageNumber)
      const base = page.getViewport({ scale: 1 })
      const viewport = page.getViewport({ scale: width / base.width })
      if (cancelled || !canvas.current || !layer.current || !box.current) return
      view.current = viewport
      setSize({ w: viewport.width, h: viewport.height })
      const dpr = Math.min(window.devicePixelRatio || 1, 2)
      const c = canvas.current
      c.width = Math.floor(viewport.width * dpr); c.height = Math.floor(viewport.height * dpr)
      c.style.width = `${viewport.width}px`; c.style.height = `${viewport.height}px`
      const r = page.render({ canvas: c, viewport, transform: dpr !== 1 ? [dpr, 0, 0, dpr, 0, 0] : undefined })
      r.promise.catch(() => {}) // cancelled when the tab is hidden or the width changes
      task = r
      box.current.style.setProperty('--scale-factor', String(viewport.scale))
      box.current.style.setProperty('--total-scale-factor', String(viewport.scale))
      layer.current.replaceChildren()
      const tl = new pdfjs.TextLayer({ textContentSource: await page.getTextContent(), container: layer.current, viewport })
      if (cancelled) return
      await tl.render().catch(() => {})
      await r.promise.catch(() => {})
      if (!cancelled) setReady((x) => x + 1)
    })().catch(() => {})
    return () => { cancelled = true; task?.cancel() }
  }, [pdf, pageNumber, width])

  // Find the entities in the text layer and measure where they are.
  useEffect(() => {
    if (!ready || !entities || !layer.current || !box.current || !view.current) { setHits([]); return }
    const { text, nodes } = textLayerString(layer.current)
    const keyOf = new Map<string, string>()
    const kindOf = new Map<string, Kind>()
    for (const e of entities) { const k = `${e.kind}\u0001${e.name}`; if (!keyOf.has(e.name)) { keyOf.set(e.name, k); kindOf.set(e.name, e.kind) } }
    const mentions = findMentions(text, [...keyOf.keys()])
    const origin = box.current.getBoundingClientRect()
    const vp = view.current
    const out: Hit[] = []
    for (const m of mentions) {
      const a = locate(nodes, m.start), b = locate(nodes, m.end, true)
      if (!a || !b) continue
      const range = document.createRange()
      try { range.setStart(a.node, a.offset); range.setEnd(b.node, b.offset) } catch { continue }
      const boxes = mergeBoxes(Array.from(range.getClientRects()).filter((r) => r.width > 0.5 && r.height > 0.5)
        .map((r) => ({ left: r.left - origin.left, top: r.top - origin.top, width: r.width, height: r.height })))
      if (!boxes.length) continue
      const pdfBoxes = boxes.map((r) => {
        const [x1, y1] = vp.convertToPdfPoint(r.left, r.top + r.height), [x2, y2] = vp.convertToPdfPoint(r.left + r.width, r.top)
        return [Math.min(x1, x2), Math.min(y1, y2), Math.max(x1, x2), Math.max(y1, y2)] as [number, number, number, number]
      })
      out.push({ key: keyOf.get(m.name)!, kind: kindOf.get(m.name)!, name: m.name, boxes, pdf: pdfBoxes })
    }
    setHits(out)
    onHitsRef.current(out)
  }, [ready, entities])

  return (
    <div className="pdfdoc-page" ref={box} style={size ? { width: size.w, height: size.h } : { width, height: width * 1.3 }} data-page={pageNumber}>
      <canvas ref={canvas} />
      <div className="pdfdoc-hls" aria-hidden="true">
        {hits.filter((h) => shown.has(h.kind)).flatMap((h, i) => h.boxes.map((r, j) => (
          <div key={`${i}-${j}`} className="pdfdoc-hl" title={`${h.name} — ${KIND[h.kind].label}`}
            style={{ left: r.left - 1, top: r.top - 1, width: r.width + 2, height: r.height + 2, background: KIND[h.kind].color }}
            onClick={(e) => { e.stopPropagation(); onPick(h.key, r.left, r.top + r.height) }} />
        )))}
      </div>
      <div className="textLayer" ref={layer} />
      {popup}
      <span className="pdfdoc-pageno">{pageNumber}</span>
    </div>
  )
}
