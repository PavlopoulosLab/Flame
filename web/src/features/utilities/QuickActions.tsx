import { useQuery } from '@tanstack/react-query'
import { useMemo, useRef, useState } from 'react'
import { api, asset, getWorkspaceId, saveText } from '../../api/client'
import { useConfig, useCreateList } from '../../api/hooks'
import type { AnalyteList } from '../../api/types'
import { NetworkGraph, type NetworkHandle } from '../../components/charts'
import { LayoutSelect, NetworkOptions } from '../../components/NetworkOptions'
import { useNetworkStyle } from '../../lib/networkStyle'
import { Arena3DIcon } from '../../components/ExternalIcons'
import { BusyBar, CopyButton, ErrorNote, Field, Modal, Note, OpenExternalButton, OrganismPicker, Segmented } from '../../components/ui'
import { plural } from '../../lib/format'
import { categorical, PaletteScope, roleColors, usePaletteMode } from '../../lib/palette'
import { ModuleList } from '../analyses/Analyses'
import { saveXlsx } from '../../lib/xlsx'
import { startTask, useTask, type ModulesResult } from '../analyses/tasks'
import { ProgressBar } from '../../components/Progress'

export type QuickAction = 'convert' | 'orthologs' | 'ppi' | 'annotate' | 'clean' | 'filter'

const TITLES: Record<QuickAction, [string, string]> = {
  convert: ['Convert IDs', 'Translate a list to another identifier type with g:Convert (g:Profiler).'],
  orthologs: ['Find orthologs', 'Map a list to the corresponding genes in another organism with g:Orth (g:Profiler).'],
  ppi: ['Protein interaction network', 'Known and predicted protein–protein interactions from STRING.'],
  annotate: ['Annotate a list', 'Current symbol, name, gene type, location and database IDs of every gene (MyGene.info).'],
  clean: ['Check and clean a list', 'Find IDs that are not recognised, outdated symbols and duplicates, and save a cleaned list.'],
  filter: ['Filter a list', 'Keep genes by type (e.g. protein-coding) or chromosome and save the result as a new list.'],
}

export function QuickActionModal(props: { action: QuickAction; lists: AnalyteList[]; initialListId?: string; onClose: () => void }) {
  return <PaletteScope id={`quick-${props.action}`}><QuickActionBody {...props} /></PaletteScope>
}

function QuickActionBody({ action, lists, initialListId, onClose }: { action: QuickAction; lists: AnalyteList[]; initialListId?: string; onClose: () => void }) {
  const [listId, setListId] = useState(initialListId ?? lists[0]?.id ?? '')
  const list = lists.find((l) => l.id === listId)
  const [title, subtitle] = TITLES[action]
  return (
    <Modal title={title} subtitle={subtitle} onClose={onClose} size={action === 'ppi' || action === 'annotate' ? 'xl' : 'lg'} palette={action === 'ppi'}>
      <Field label="List">
        <select className="input" value={listId} onChange={(e) => setListId(e.target.value)}>
          {lists.map((l) => (
            <option key={l.id} value={l.id} disabled={action === 'ppi' && l.size > 500}>
              {l.name} ({l.size} IDs){action === 'ppi' && l.size > 500 ? ' — over the 500-ID STRING limit' : ''}
            </option>
          ))}
        </select>
      </Field>
      {list && action === 'convert' && <Convert key={list.id} list={list} onDone={onClose} />}
      {list && action === 'orthologs' && <Orthologs key={list.id} list={list} onDone={onClose} />}
      {list && action === 'ppi' && <Ppi key={list.id} list={list} onDone={onClose} />}
      {list && action === 'annotate' && <Annotate key={list.id} list={list} />}
      {list && action === 'clean' && <CleanList key={list.id} list={list} onDone={onClose} />}
      {list && action === 'filter' && <FilterList key={list.id} list={list} onDone={onClose} />}
    </Modal>
  )
}

interface MapRow { rowId: string; original: string; converted?: string; symbol?: string; ortholog?: string; orthologEnsg?: string; originalEnsg?: string; description?: string }

function MappingTable({ rows, targetKey, chosen, setChosen }: { rows: MapRow[]; targetKey: 'converted' | 'ortholog'; chosen: Set<string>; setChosen: (s: Set<string>) => void }) {
  const valid = rows.filter((r) => r[targetKey])
  return (
    <>
      <div className="row between">
        <span className="muted small">{plural(valid.length, 'mapping')} · {chosen.size} selected</span>
        <span className="row">
          <button className="btn btn-ghost btn-sm" onClick={() => setChosen(new Set(valid.map((r) => r.rowId)))}>Select all</button>
          <button className="btn btn-ghost btn-sm" onClick={() => setChosen(new Set())}>Clear</button>
        </span>
      </div>
      <div className="table-scroll">
        <table className="table">
          <thead>
            <tr><th /><th>Original</th><th>{targetKey === 'converted' ? 'Converted' : 'Ortholog'}</th>{targetKey === 'converted' ? <th>Symbol</th> : <th>Ensembl</th>}<th>Description</th></tr>
          </thead>
          <tbody>
            {rows.slice(0, 1000).map((r) => (
              <tr key={r.rowId} className={r[targetKey] ? '' : 'muted'}>
                <td><input type="checkbox" disabled={!r[targetKey]} checked={chosen.has(r.rowId)} aria-label={`Use ${r[targetKey] ?? r.original}`}
                  onChange={() => { const n = new Set(chosen); if (n.has(r.rowId)) n.delete(r.rowId); else n.add(r.rowId); setChosen(n) }} /></td>
                <td>{r.original}</td>
                <td>{r[targetKey] || '—'}</td>
                <td>{targetKey === 'converted' ? r.symbol : r.orthologEnsg}</td>
                <td className="truncate" title={r.description}>{r.description}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </>
  )
}

export function SaveDerived({ ids, defaultName, taxid, idFormat, meta, onDone }: { ids: string[]; defaultName: string; taxid: number; idFormat: string; meta: Record<string, unknown>; onDone: () => void }) {
  const [name, setName] = useState(defaultName)
  const create = useCreateList()
  return (
    <div className="save-list">
      <div className="row">
        <Field label="New list name"><input className="input" value={name} maxLength={80} onChange={(e) => setName(e.target.value)} /></Field>
      </div>
      <ErrorNote error={create.error} />
      <div className="row end">
        <button className="btn btn-primary" disabled={!ids.length || !name.trim()}
          onClick={() => create.mutate({ name: name.trim(), ids, organismTaxId: taxid, idFormat, meta, uniqueName: true }, { onSuccess: onDone })}>
          Save {plural(ids.length, 'ID')} as a new list
        </button>
      </div>
    </div>
  )
}

// ---- Annotate / Check and clean / Filter (one MyGene.info lookup, shared) ------

interface AnnRow { input: string; status: 'ok' | 'alias' | 'notfound'; symbol?: string; name?: string; type?: string; chr?: string; start?: number; end?: number; band?: string; ensembl?: string; entrez?: string; ambiguous?: boolean; duplicateOf?: string }
interface AnnResult { rows: AnnRow[]; stats: { total: number; ok: number; alias: number; notfound: number; duplicates: number; types: { type: string; n: number }[] }; source: string }

/** The annotation of a list; fetched once and shared by the three tools. */
function useAnnotation(list: AnalyteList) {
  return useQuery({
    queryKey: ['gene-annotation', getWorkspaceId(), list.id, list.size],
    queryFn: () => api<AnnResult>('/utilities/annotate', { body: { listId: list.id } }),
    staleTime: 10 * 60_000, retry: false,
  })
}
function AnnotationState({ q }: { q: ReturnType<typeof useAnnotation> }) {
  return <>{q.isFetching && <BusyBar label="Looking up the genes at MyGene.info" expect={8} />}<ErrorNote error={q.error} /></>
}
const typeLabel = (t?: string) => (!t ? 'unknown' : t)
const CHR_ORDER = [...Array.from({ length: 22 }, (_, i) => String(i + 1)), 'X', 'Y', 'MT']
const chrSort = (a: string, b: string) => { const i = CHR_ORDER.indexOf(a), j = CHR_ORDER.indexOf(b); return (i < 0 ? 99 : i) - (j < 0 ? 99 : j) || a.localeCompare(b, undefined, { numeric: true }) }
// Placeholder and non-gene-like names (LOC…, antisense, readthrough, lncRNA placeholders).
const PLACEHOLDER = /^(LOC[0-9]+|LINC[0-9]+|MIR[0-9]+HG|C[0-9XY]+orf[0-9]+|FAM[0-9]+[A-Z]?[0-9]*|KIAA[0-9]+|FLJ[0-9]+)$|-(AS|IT|OT|DT)[0-9]*$/i

function Annotate({ list }: { list: AnalyteList }) {
  const q = useAnnotation(list)
  const [find, setFind] = useState('')
  const rows = q.data?.rows ?? []
  const shown = useMemo(() => {
    const f = find.trim().toLowerCase()
    return f ? rows.filter((r) => `${r.input} ${r.symbol ?? ''} ${r.name ?? ''} ${r.type ?? ''} ${r.ensembl ?? ''} ${r.entrez ?? ''} ${r.chr ? `chr${r.chr}` : ''} ${r.band ?? ''}`.toLowerCase().includes(f)) : rows
  }, [rows, find])
  const table = (rs: AnnRow[]) => rs.map((r) => ({ Input: r.input, Status: r.status === 'ok' ? 'recognised' : r.status === 'alias' ? 'other symbol' : 'not found', Symbol: r.symbol ?? '', Name: r.name ?? '',
    'Gene type': r.type ?? '', Chromosome: r.chr ?? '', Start: r.start ?? '', End: r.end ?? '', Band: r.band ?? '', 'Ensembl gene': r.ensembl ?? '', 'Entrez gene': r.entrez ?? '' }))
  return (
    <div className="stack-sm">
      <AnnotationState q={q} />
      {q.data && (
        <>
          <p className="small"><strong>{q.data.stats.ok + q.data.stats.alias}</strong> of {q.data.stats.total} IDs recognised
            {q.data.stats.alias > 0 && <> · {q.data.stats.alias} under another current symbol</>}{q.data.stats.notfound > 0 && <> · {q.data.stats.notfound} not found</>}
            {' · '}{q.data.stats.types.filter((t) => t.type !== 'not found').sort((a, b) => b.n - a.n).map((t) => `${t.n} ${t.type}`).join(', ')}</p>
          <div className="row between wrap">
            <input className="input list-filter" type="search" value={find} placeholder="Find a gene, name, type or chr17" aria-label="Find in the annotation" onChange={(e) => setFind(e.target.value)} />
            <span className="row">
              <span className="muted small">{shown.length === rows.length ? `${rows.length} genes` : `${shown.length} of ${rows.length}`}</span>
              <CopyButton label="Copy symbols" text={[...new Set(shown.map((r) => r.symbol).filter(Boolean))].join('\n')} />
              <button type="button" className="btn btn-ghost btn-sm" onClick={() => saveXlsx([{ name: 'Annotation', rows: table(rows) }], `${list.name}_annotation.xlsx`)}>Excel</button>
              <button type="button" className="btn btn-ghost btn-sm" onClick={() => { const t = table(rows); saveText([Object.keys(t[0] ?? {}).join('\t'), ...t.map((r) => Object.values(r).join('\t'))].join('\n'), `${list.name}_annotation.tsv`, 'text/tab-separated-values') }}>TSV</button>
            </span>
          </div>
          <div className="table-scroll ann-table">
            <table className="table compact">
              <thead><tr><th>Your ID</th><th>Symbol</th><th>Name</th><th>Gene type</th><th>Location</th><th>Ensembl</th><th>Entrez</th></tr></thead>
              <tbody>
                {shown.slice(0, 1000).map((r) => (
                  <tr key={r.input} className={r.status === 'notfound' ? 'muted' : ''}>
                    <td className="mono">{r.input}</td>
                    <td>{r.symbol ? <a href={`https://www.genecards.org/cgi-bin/carddisp.pl?gene=${encodeURIComponent(r.symbol)}`} target="_blank" rel="noopener noreferrer">{r.symbol}</a> : <span className="muted">not found</span>}{r.status === 'alias' && <span className="tag-ranked" title="Your ID is an older or alternative name of this gene"> current symbol</span>}</td>
                    <td className="small">{r.name}</td><td className="small">{r.type}</td>
                    <td className="small">{r.chr ? `chr${r.chr}${r.start ? `:${r.start.toLocaleString()}` : ''}` : ''}{r.band ? <span className="muted"> {r.band}</span> : null}</td>
                    <td className="mono small">{r.ensembl && <a href={`https://www.ensembl.org/id/${r.ensembl}`} target="_blank" rel="noopener noreferrer">{r.ensembl}</a>}</td>
                    <td className="mono small">{r.entrez && <a href={`https://www.ncbi.nlm.nih.gov/gene/${r.entrez}`} target="_blank" rel="noopener noreferrer">{r.entrez}</a>}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          {shown.length > 1000 && <p className="muted small">Showing the first 1,000 rows; the Excel and TSV files hold all of them.</p>}
          <p className="muted small">{q.data.source}</p>
        </>
      )}
    </div>
  )
}

function CleanList({ list, onDone }: { list: AnalyteList; onDone: () => void }) {
  const q = useAnnotation(list)
  const [dropUnknown, setDropUnknown] = useState(true)
  const [useCurrent, setUseCurrent] = useState(true)
  const [mergeDup, setMergeDup] = useState(true)
  const rows = q.data?.rows ?? []
  const symbols = list.idFormat === 'USERINPUT' || /SYMBOL|HGNC/i.test(list.idFormat)
  const problems = rows.filter((r) => r.status !== 'ok' || r.duplicateOf)
  const cleaned = useMemo(() => {
    const out: string[] = []; const seen = new Set<string>()
    rows.forEach((r) => {
      if (r.status === 'notfound') { if (!dropUnknown) out.push(r.input); return }
      const id = useCurrent && symbols && r.status === 'alias' && r.symbol ? r.symbol : r.input
      const key = (r.symbol ?? id).toUpperCase()
      if (mergeDup && seen.has(key)) return
      seen.add(key); out.push(id)
    })
    return [...new Set(out)]
  }, [rows, dropUnknown, useCurrent, mergeDup, symbols])
  return (
    <div className="stack-sm">
      <AnnotationState q={q} />
      {q.data && (
        <>
          <div className="stat-tiles">
            <div className="stat-tile"><strong>{q.data.stats.ok}</strong><span className="small">recognised as given</span></div>
            <div className="stat-tile"><strong>{q.data.stats.alias}</strong><span className="small">older or alternative names</span></div>
            <div className="stat-tile"><strong>{q.data.stats.notfound}</strong><span className="small">not found</span></div>
            <div className="stat-tile"><strong>{q.data.stats.duplicates}</strong><span className="small">duplicates of another ID</span></div>
          </div>
          {problems.length === 0 ? <Note kind="success">Every ID is a current, unique gene identifier. Nothing to clean.</Note> : (
            <>
              <div className="stack-sm">
                <label className="check"><input type="checkbox" checked={dropUnknown} onChange={(e) => setDropUnknown(e.target.checked)} /> Remove the {q.data.stats.notfound} IDs that were not found</label>
                <label className={`check${symbols ? '' : ' muted'}`}><input type="checkbox" checked={useCurrent && symbols} disabled={!symbols} onChange={(e) => setUseCurrent(e.target.checked)} /> Replace the {q.data.stats.alias} older or alternative names with the current symbol{!symbols && ' (only for lists of gene symbols)'}</label>
                <label className="check"><input type="checkbox" checked={mergeDup} onChange={(e) => setMergeDup(e.target.checked)} /> Keep one ID per gene (merge the {q.data.stats.duplicates} duplicates)</label>
              </div>
              <div className="table-scroll ann-table">
                <table className="table compact">
                  <thead><tr><th>Your ID</th><th>Problem</th><th>Current symbol</th><th>Name</th></tr></thead>
                  <tbody>
                    {problems.slice(0, 500).map((r) => (
                      <tr key={r.input}>
                        <td className="mono">{r.input}</td>
                        <td className="small">{r.status === 'notfound' ? 'Not found' : [r.status === 'alias' ? `Older or alternative name${r.ambiguous ? ' (also used for other genes)' : ''}` : '', r.duplicateOf ? `Same gene as ${r.duplicateOf}` : ''].filter(Boolean).join(' · ')}</td>
                        <td>{r.symbol}</td><td className="small">{r.name}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
              {problems.length > 500 && <p className="muted small">Showing the first 500 of {problems.length} IDs with a problem.</p>}
            </>
          )}
          <p className="small"><strong>{plural(cleaned.length, 'ID')}</strong> in the cleaned list (from {rows.length}).{list.ranked ? ' Scores are not carried over: the cleaned list is a plain list.' : ''}</p>
          {problems.length > 0 && <SaveDerived ids={cleaned} defaultName={`${list.name} (cleaned)`} taxid={list.organismTaxId} idFormat={list.idFormat} meta={{ method: 'clean', derivedFrom: list.name }} onDone={onDone} />}
          <p className="muted small">{q.data.source}</p>
        </>
      )}
    </div>
  )
}

function FilterList({ list, onDone }: { list: AnalyteList; onDone: () => void }) {
  const q = useAnnotation(list)
  const rows = q.data?.rows ?? []
  const types = useMemo(() => { const m = new Map<string, number>(); rows.filter((r) => r.status !== 'notfound').forEach((r) => m.set(typeLabel(r.type), (m.get(typeLabel(r.type)) ?? 0) + 1)); return [...m.entries()].sort((a, b) => b[1] - a[1]) }, [rows])
  const chrs = useMemo(() => { const m = new Map<string, number>(); rows.forEach((r) => { if (r.chr) m.set(r.chr, (m.get(r.chr) ?? 0) + 1) }); return [...m.entries()].sort((a, b) => chrSort(a[0], b[0])) }, [rows])
  const [offTypes, setOffTypes] = useState<string[]>([])
  const [offChrs, setOffChrs] = useState<string[]>([])
  const [keepUnknown, setKeepUnknown] = useState(false)
  const [dropPlaceholders, setDropPlaceholders] = useState(false)
  const kept = useMemo(() => rows.filter((r) => {
    if (r.status === 'notfound') return keepUnknown
    if (offTypes.includes(typeLabel(r.type))) return false
    if (r.chr && offChrs.includes(r.chr)) return false
    if (dropPlaceholders && PLACEHOLDER.test(r.symbol ?? r.input)) return false
    return true
  }), [rows, offTypes, offChrs, keepUnknown, dropPlaceholders])
  const placeholders = rows.filter((r) => r.status !== 'notfound' && PLACEHOLDER.test(r.symbol ?? r.input)).length
  const notFound = q.data?.stats.notfound ?? 0
  const toggle = (cur: string[], set: (v: string[]) => void, v: string) => set(cur.includes(v) ? cur.filter((x) => x !== v) : [...cur, v])
  return (
    <div className="stack-sm">
      <AnnotationState q={q} />
      {q.data && (
        <>
          <div className="stack-sm">
            <div className="row between wrap">
              <span className="field-label">Gene types to keep</span>
              <span className="row">
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => setOffTypes(types.map(([t]) => t).filter((t) => t !== 'protein-coding'))}>Protein-coding only</button>
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => setOffTypes([])}>All</button>
              </span>
            </div>
            <div className="row wrap">
              {types.map(([t, n]) => <label key={t} className="check small"><input type="checkbox" checked={!offTypes.includes(t)} onChange={() => toggle(offTypes, setOffTypes, t)} /> {t} <span className="muted">({n})</span></label>)}
            </div>
            {chrs.length > 0 && (
              <>
                <div className="row between wrap">
                  <span className="field-label">Chromosomes to keep</span>
                  <span className="row">
                    <button type="button" className="btn btn-ghost btn-sm" onClick={() => setOffChrs([])}>All</button>
                    <button type="button" className="btn btn-ghost btn-sm" onClick={() => setOffChrs(chrs.map(([c]) => c))}>None</button>
                  </span>
                </div>
                <div className="chip-wrap">
                  {chrs.map(([c, n]) => <button key={c} type="button" className={`chip${offChrs.includes(c) ? '' : ' on'}`} aria-pressed={!offChrs.includes(c)} title={`${n} genes on chromosome ${c}`} onClick={() => toggle(offChrs, setOffChrs, c)}>{c} <span className="muted small">{n}</span></button>)}
                </div>
              </>
            )}
            <label className="check"><input type="checkbox" checked={dropPlaceholders} onChange={(e) => setDropPlaceholders(e.target.checked)} /> Remove placeholder and antisense names such as LOC…, LINC…, C1orf… and …-AS1 <span className="muted small">({placeholders} in this list)</span></label>
            {notFound > 0 && <label className="check"><input type="checkbox" checked={keepUnknown} onChange={(e) => setKeepUnknown(e.target.checked)} /> Keep the {notFound} IDs that were not found (their type and chromosome are unknown)</label>}
          </div>
          <p className="small"><strong>{plural(kept.length, 'ID')}</strong> kept of {rows.length}.{list.ranked ? ' Scores are not carried over: the filtered list is a plain list.' : ''}</p>
          <details className="small"><summary>Genes kept ({kept.length})</summary><p className="mono small">{kept.slice(0, 600).map((r) => r.input).join(', ')}{kept.length > 600 ? ' …' : ''}</p></details>
          <SaveDerived ids={kept.map((r) => r.input)} defaultName={`${list.name} (filtered)`} taxid={list.organismTaxId} idFormat={list.idFormat} meta={{ method: 'filter', derivedFrom: list.name }} onDone={onDone} />
          <p className="muted small">{q.data.source}</p>
        </>
      )}
    </div>
  )
}

function Convert({ list, onDone }: { list: AnalyteList; onDone: () => void }) {
  const { data: config } = useConfig()
  const [target, setTarget] = useState('ENTREZGENE_ACC')
  const [mode, setMode] = useState<'all' | 'best'>('best')
  const [res, setRes] = useState<{ rows: MapRow[]; stats: Record<string, string | number> } | null>(null)
  const [chosen, setChosen] = useState<Set<string>>(new Set())
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const run = () => {
    setBusy(true); setErr(null)
    api<NonNullable<typeof res>>('/utilities/convert', { body: { listId: list.id, targetFormat: target, mappingMode: mode } })
      .then((r) => { setRes(r); setChosen(new Set(r.rows.filter((x) => x.converted).map((x) => x.rowId))) })
      .catch(setErr).finally(() => setBusy(false))
  }
  const ids = res ? [...new Set(res.rows.filter((r) => chosen.has(r.rowId)).map((r) => r.converted!))] : []
  return (
    <>
      <div className="row">
        <Field label="Convert to">
          <select className="input" value={target} onChange={(e) => setTarget(e.target.value)}>
            {(config?.idFormats ?? []).filter((f) => f.value !== 'USERINPUT').map((f) => <option key={f.value} value={f.value}>{f.label}</option>)}
          </select>
        </Field>
        <Field label="Mappings" hint="Best keeps one target per input ID">
          <Segmented label="Mapping mode" value={mode} onChange={setMode} options={[{ value: 'best', label: 'Best only' }, { value: 'all', label: 'All' }]} />
        </Field>
        <button className="btn btn-secondary align-end" onClick={run} disabled={busy}>{busy ? 'Converting…' : 'Convert'}</button>
      </div>
      {busy && <BusyBar label="Converting IDs with g:Convert" expect={10} />}
      <ErrorNote error={err} />
      {res && (
        <>
          <Note kind="success">{res.stats.mappedInputs} of {res.stats.inputCount} IDs converted to {res.stats.targetLabel}.</Note>
          <MappingTable rows={res.rows} targetKey="converted" chosen={chosen} setChosen={setChosen} />
          <SaveDerived ids={ids} defaultName={`${list.name} (${res.stats.targetLabel})`} taxid={list.organismTaxId} idFormat={target}
            meta={{ method: 'convert', derivedFrom: list.name }} onDone={onDone} />
        </>
      )}
    </>
  )
}

function Orthologs({ list, onDone }: { list: AnalyteList; onDone: () => void }) {
  const [target, setTarget] = useState(list.organismTaxId === 10090 ? 9606 : 10090)
  const [mode, setMode] = useState<'all' | 'best'>('all')
  const [res, setRes] = useState<{ rows: MapRow[]; stats: Record<string, string | number>; targetTaxId: number } | null>(null)
  const [chosen, setChosen] = useState<Set<string>>(new Set())
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const run = () => {
    setBusy(true); setErr(null)
    api<NonNullable<typeof res>>('/utilities/orthologs', { body: { listId: list.id, targetTaxId: target, mappingMode: mode } })
      .then((r) => { setRes(r); setChosen(new Set(r.rows.filter((x) => x.ortholog).map((x) => x.rowId))) })
      .catch(setErr).finally(() => setBusy(false))
  }
  const ids = res ? [...new Set(res.rows.filter((r) => chosen.has(r.rowId)).map((r) => r.ortholog!))] : []
  return (
    <>
      <div className="row">
        <OrganismPicker value={target} onChange={(t) => setTarget(t)} label="Target organism" />
        <Field label="Mappings">
          <Segmented label="Mapping mode" value={mode} onChange={setMode} options={[{ value: 'all', label: 'All' }, { value: 'best', label: 'Best only' }]} />
        </Field>
        <button className="btn btn-secondary align-end" onClick={run} disabled={busy}>{busy ? 'Searching…' : 'Find orthologs'}</button>
      </div>
      {busy && <BusyBar label="Finding orthologs with g:Orth" expect={10} />}
      <ErrorNote error={err} />
      {res && (
        <>
          <Note kind="success">{res.stats.mappedInputs} of {res.stats.inputCount} genes have orthologs in {String(res.stats.targetOrganism).replace(/ \[NCBI.*$/, '')}.</Note>
          <MappingTable rows={res.rows} targetKey="ortholog" chosen={chosen} setChosen={setChosen} />
          <SaveDerived ids={ids} defaultName={`${list.name} orthologs`} taxid={res.targetTaxId} idFormat="USERINPUT"
            meta={{ method: 'orthologs', derivedFrom: list.name }} onDone={onDone} />
        </>
      )}
    </>
  )
}

interface PpiResult {
  nodes: { id: string; label: string; query: string; degree: number }[]
  edges: { from: string; to: string; score: number; [k: string]: number | string }[]
  link: string | null
  tsv: string
  stats: { nodes: number; edges: number; averageDegree: number; unmapped: number }
}

const EVIDENCE: [string, string, string][] = [
  ['escore', 'Experiments', '#d946ef'], ['dscore', 'Databases', '#06b6d4'], ['tscore', 'Text mining', '#84cc16'],
  ['ascore', 'Co-expression', '#1e3a8a'], ['nscore', 'Neighbourhood', '#16a34a'], ['fscore', 'Gene fusion', '#dc2626'], ['pscore', 'Co-occurrence', '#2563eb'],
]

function Ppi({ list, onDone }: { list: AnalyteList; onDone: () => void }) {
  const [type, setType] = useState<'functional' | 'physical'>('functional')
  const [edgesMode, setEdgesMode] = useState<'evidence' | 'confidence'>('evidence')
  const [hidden, setHidden] = useState<Set<string>>(new Set())
  const [score, setScore] = useState('400')
  const [res, setRes] = useState<PpiResult | null>(null)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const net = useRef<NetworkHandle | null>(null)
  const ppiStyle = useNetworkStyle('ppi', { engine: 'barnesHut', labelSize: 12 })
  const [picked, setPicked] = useState<string[]>([])
  const [modTask, setModTask] = useState<string | null>(null)
  const [modPick, setModPick] = useState<number | null>(null)
  const mod = useTask<ModulesResult>(modTask)
  const modules = mod.data?.status === 'complete' ? mod.data.result ?? null : null
  const modColor = (k: number) => categorical(pm)[(k - 1) % categorical(pm).length]
  const pm = usePaletteMode()
  const evidence = EVIDENCE.map(([k, label, c], i) => [k, label, pm === 'flame' ? c : categorical(pm)[i % categorical(pm).length]] as [string, string, string])
  const run = () => {
    setBusy(true); setErr(null)
    api<PpiResult>('/utilities/ppi', { body: { listId: list.id, networkType: type, networkEdges: edgesMode, score: Number(score) } })
      .then(setRes).catch(setErr).finally(() => setBusy(false))
  }
  // A direct link to the same network on the STRING website, built from the list's IDs.
  const ids = useQuery({ queryKey: ['list-ids', getWorkspaceId(), list.id], queryFn: () => api<{ ids: string[] }>(`/lists/${list.id}`), staleTime: 60_000 })
  const stringUrl = ids.data?.ids?.length && list.size <= 2000
    ? `https://string-db.org/cgi/network?${new URLSearchParams({ identifiers: ids.data.ids.join('\r'), species: String(list.organismTaxId),
        required_score: score, network_flavor: edgesMode === 'evidence' ? 'evidence' : 'confidence', network_type: type === 'physical' ? 'physical' : 'functional' })}`
    : null
  const graph = useMemo(() => {
    if (!res) return { nodes: [], edges: [] }
    const nodes = res.nodes.map((n) => ({ id: n.id, label: n.label, title: `${n.label}${n.query && n.query !== n.label ? ` (${n.query})` : ''} · ${n.degree} interactions`, color: modules ? (modules.modules.some((m) => m.id === modules.membership[n.id]) ? modColor(modules.membership[n.id]) : '#a1a1aa') : roleColors(pm).accent, shape: 'dot', size: 8 + Math.min(16, n.degree) }))
    // Multi-edge network: one edge per STRING evidence channel, drawn as parallel curves
    // (like STRING's evidence view); single-edge view: one edge per pair, width = combined score.
    const edges = edgesMode === 'evidence'
      ? res.edges.flatMap((e, i) => {
        const ch = evidence.filter(([k]) => !hidden.has(k) && Number(e[k] ?? 0) > 0)
        return ch.map(([k, label, c], j) => {
          const offset = j - (ch.length - 1) / 2
          return { id: `${i}-${k}`, from: e.from, to: e.to, value: Number(e[k]), width: 0.6 + 2.4 * Number(e[k]), color: c,
            smooth: ch.length === 1 ? false as const : { enabled: true, type: offset >= 0 ? 'curvedCW' : 'curvedCCW', roundness: Math.min(0.6, Math.abs(offset) * 0.18 + (ch.length % 2 ? 0 : 0.09)) },
            title: `${e.from} – ${e.to} · ${label} ${Number(e[k]).toFixed(3)} (combined ${e.score})` }
        })
      })
      : res.edges.map((e, i) => ({ id: i, from: e.from, to: e.to, value: e.score, width: 0.5 + 4 * e.score, color: '#71717a',
        title: `${e.from} – ${e.to} · combined score ${e.score}` }))
    return { nodes, edges }
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [res, edgesMode, pm, modules, hidden])
  return (
    <>
      <div className="row wrap">
        <Field label="Interactions"><Segmented label="Network type" value={type} onChange={setType} options={[{ value: 'functional', label: 'All (functional)' }, { value: 'physical', label: 'Physical only' }]} /></Field>
        <Field label="Edges"><Segmented label="Edge meaning" value={edgesMode} onChange={setEdgesMode} options={[{ value: 'evidence', label: 'One per evidence (multi-edge)' }, { value: 'confidence', label: 'One per pair (confidence)' }]} /></Field>
        <Field label="Minimum score">
          <select className="input" value={score} onChange={(e) => setScore(e.target.value)}>
            <option value="900">Highest (0.900)</option><option value="700">High (0.700)</option><option value="400">Medium (0.400)</option><option value="150">Low (0.150)</option>
          </select>
        </Field>
        <button className="btn btn-secondary align-end" onClick={run} disabled={busy}>{busy ? 'Building…' : res ? 'Rebuild network' : 'Build network'}</button>
        {stringUrl && <a className="btn btn-ghost align-end string-link" href={stringUrl} target="_blank" rel="noopener noreferrer"
          title="Open this list on the STRING website with the same settings"><img src={asset('/logos/string-icon.png')} alt="" width={18} height={18} /> Open in STRING ↗</a>}
      </div>
      {busy && <BusyBar label="Building the protein network from STRING" expect={15} />}
      {err != null && (
        <div className="row wrap">
          <ErrorNote error={err} />
          <button className="btn btn-secondary btn-sm" onClick={run} disabled={busy}>Try again</button>
          {stringUrl && <span className="muted small">or open the network directly in STRING.</span>}
        </div>
      )}
      {res && (
        <>
          <p className="small">{plural(res.stats.nodes, 'protein')} · {plural(res.stats.edges, 'interaction')}{edgesMode === 'evidence' ? ` (${graph.edges.length} evidence edges)` : ''} · average degree {res.stats.averageDegree}{res.stats.unmapped ? ` · ${res.stats.unmapped} IDs not found in STRING` : ''}</p>
          {edgesMode === 'evidence' && (
            <div className="legend legend-toggle" role="group" aria-label="Evidence channels">
              {evidence.map(([k, label, c]) => {
                const n = res.edges.filter((e) => Number(e[k] ?? 0) > 0).length
                const on = !hidden.has(k)
                return <button key={k} type="button" className={`chip${on ? ' on' : ''}`} aria-pressed={on} disabled={!n}
                  title={`${n} interactions supported by ${label.toLowerCase()} — click to ${on ? 'hide' : 'show'}`}
                  onClick={() => setHidden((h) => { const x = new Set(h); if (x.has(k)) x.delete(k); else x.add(k); return x })}>
                  <i style={{ background: c, opacity: on ? 1 : 0.3 }} />{label} <span className="muted small">{n}</span></button>
              })}
              <span className="muted small">One edge per evidence channel; click a channel to hide or show it.</span>
            </div>
          )}
          <div className="toolbar">
            <label>Layout <LayoutSelect style={ppiStyle.style} onChange={ppiStyle.setStyle} /></label>
            <NetworkOptions style={ppiStyle.style} onChange={ppiStyle.setStyle} onReset={ppiStyle.reset} handle={net} />
            <span className="toolbar-spacer" />
            <button className="btn btn-secondary btn-sm" disabled={!!modTask && mod.data?.status === 'running'} title="Split the network into modules (Louvain) and run enrichment on each"
              onClick={() => { setModPick(null); startTask({ type: 'modules', listId: list.id, score: Number(score) }).then((r) => setModTask(r.id)) }}>
              {modules ? 'Recompute modules' : 'Find modules'}
            </button>
          </div>
          {modTask && mod.data?.status !== 'complete' && (mod.data?.status === 'error' ? <ErrorNote error={new Error(mod.data.error ?? 'Module search failed')} /> : <ProgressBar progress={mod.data?.progress} status="running" />)}
          <NetworkGraph exportName={`${list.name}_string_network`} nodes={graph.nodes} edges={graph.edges} handleRef={net} style={ppiStyle.style} height={480} selected={picked}
            onSelect={(ids, add) => setPicked((cur) => (add ? [...new Set([...cur, ...ids])] : ids))} />
          {picked.length > 0 && (
            <div className="row wrap">
              <span className="small"><strong>{plural(picked.length, 'protein')}</strong> selected</span>
              <button className="btn btn-ghost btn-sm" onClick={() => setPicked([])}>Clear selection</button>
            </div>
          )}
          {modules && <ModuleList r={modules} colorOf={modColor} picked={modPick} taxid={list.organismTaxId} onDone={onDone} details={false}
            onPick={(k) => { setModPick(k); setPicked(k ? modules.modules.find((m) => m.id === k)?.genes ?? [] : []) }} />}
          {picked.length > 0 && <SaveDerived ids={picked} defaultName={`${list.name} selected proteins`} taxid={list.organismTaxId} idFormat="USERINPUT"
            meta={{ method: 'ppi-selection', derivedFrom: list.name }} onDone={onDone} />}
          <div className="row wrap">
            {(res.link || stringUrl) && <a className="btn btn-ghost" href={res.link ?? stringUrl ?? undefined} target="_blank" rel="noopener noreferrer">Open in STRING ↗</a>}
            <button className="btn btn-ghost" title="Excel workbook: proteins, interactions with every evidence channel, and the modules if found" onClick={() => saveXlsx([
              { name: 'Proteins', rows: res.nodes.map((n) => ({ Protein: n.label, Input: n.query, Interactions: n.degree, ...(modules ? { Module: modules.membership[n.id] ?? '' } : {}) })) },
              { name: 'Interactions', rows: res.edges.map((e) => ({ From: e.from, To: e.to, 'Combined score': e.score, ...Object.fromEntries(EVIDENCE.map(([k, label]) => [label, Number(e[k] ?? 0)])) })) },
              ...(modules ? [{ name: 'Modules', rows: modules.modules.map((m) => ({ Module: m.id, Label: m.label, Proteins: m.size, Genes: m.genes.join(', ') })) },
                { name: 'Module terms', rows: modules.modules.flatMap((m) => m.terms.map((t) => ({ Module: m.id, Source: t.source, 'Term ID': t.id, Term: t.name, 'Adjusted p': t.p, Genes: t.genes ?? '' }))) }] : []),
            ], `${list.name}_string_network.xlsx`)}>Download Excel</button>
            <button className="btn btn-ghost" onClick={() => saveText(res.tsv, `${list.name}_string_network.tsv`, 'text/tab-separated-values')}>Download TSV</button>
            <button className="btn btn-ghost" onClick={() => net.current?.exportImage(`${list.name}_string_network`)}>Save image…</button>
            <OpenExternalButton className="btn btn-secondary" path="/utilities/ppi/arena3d" label="Arena3D" caption="evidence types as 3D channels" icon={<Arena3DIcon />}
              body={{ listId: list.id, networkType: type, networkEdges: edgesMode, score: Number(score) }}
              title="Open in Arena3D (3D): each STRING evidence type becomes its own edge channel" />
          </div>
          <SaveDerived ids={res.nodes.map((n) => n.label)} defaultName={`${list.name} network`} taxid={list.organismTaxId} idFormat="USERINPUT"
            meta={{ method: 'ppi', derivedFrom: list.name }} onDone={onDone} />
        </>
      )}
    </>
  )
}
