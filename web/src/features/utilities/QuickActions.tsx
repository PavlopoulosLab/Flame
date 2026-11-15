import { useMemo, useRef, useState } from 'react'
import { api, saveText } from '../../api/client'
import { useConfig, useCreateList } from '../../api/hooks'
import type { AnalyteList } from '../../api/types'
import { NetworkGraph, type NetworkHandle } from '../../components/charts'
import { ErrorNote, Field, Modal, Note, OrganismPicker, Segmented } from '../../components/ui'
import { plural } from '../../lib/format'

export type QuickAction = 'convert' | 'orthologs' | 'ppi'

const TITLES: Record<QuickAction, [string, string]> = {
  convert: ['Convert IDs', 'Translate a list to another identifier type with g:Convert (g:Profiler).'],
  orthologs: ['Find orthologs', 'Map a list to the corresponding genes in another organism with g:Orth (g:Profiler).'],
  ppi: ['Protein interaction network', 'Known and predicted protein–protein interactions from STRING.'],
}

export function QuickActionModal({ action, lists, initialListId, onClose }: { action: QuickAction; lists: AnalyteList[]; initialListId?: string; onClose: () => void }) {
  const [listId, setListId] = useState(initialListId ?? lists[0]?.id ?? '')
  const list = lists.find((l) => l.id === listId)
  const [title, subtitle] = TITLES[action]
  return (
    <Modal title={title} subtitle={subtitle} onClose={onClose} size={action === 'ppi' ? 'xl' : 'lg'}>
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

function SaveDerived({ ids, defaultName, taxid, idFormat, meta, onDone }: { ids: string[]; defaultName: string; taxid: number; idFormat: string; meta: Record<string, unknown>; onDone: () => void }) {
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
  const [score, setScore] = useState('400')
  const [res, setRes] = useState<PpiResult | null>(null)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const net = useRef<NetworkHandle | null>(null)
  const run = () => {
    setBusy(true); setErr(null)
    api<PpiResult>('/utilities/ppi', { body: { listId: list.id, networkType: type, networkEdges: edgesMode, score: Number(score) } })
      .then(setRes).catch(setErr).finally(() => setBusy(false))
  }
  const graph = useMemo(() => {
    if (!res) return { nodes: [], edges: [] }
    const nodes = res.nodes.map((n) => ({ id: n.id, label: n.label, title: `${n.label}${n.query && n.query !== n.label ? ` (${n.query})` : ''} · ${n.degree} interactions`, color: '#f97316', shape: 'dot', size: 8 + Math.min(16, n.degree) }))
    const edges = res.edges.map((e, i) => {
      const top = EVIDENCE.map(([k, , c]) => [Number(e[k] ?? 0), c] as const).sort((a, b) => b[0] - a[0])[0]
      return { id: i, from: e.from, to: e.to, width: edgesMode === 'confidence' ? 0.5 + 4 * e.score : 1.5,
        color: edgesMode === 'evidence' ? top?.[1] ?? '#9ca3af' : '#71717a', title: `${e.from} – ${e.to} · score ${e.score}` }
    })
    return { nodes, edges }
  }, [res, edgesMode])
  return (
    <>
      <div className="row wrap">
        <Field label="Interactions"><Segmented label="Network type" value={type} onChange={setType} options={[{ value: 'functional', label: 'All (functional)' }, { value: 'physical', label: 'Physical only' }]} /></Field>
        <Field label="Edges show"><Segmented label="Edge meaning" value={edgesMode} onChange={setEdgesMode} options={[{ value: 'evidence', label: 'Evidence type' }, { value: 'confidence', label: 'Confidence' }]} /></Field>
        <Field label="Minimum score">
          <select className="input" value={score} onChange={(e) => setScore(e.target.value)}>
            <option value="900">Highest (0.900)</option><option value="700">High (0.700)</option><option value="400">Medium (0.400)</option><option value="150">Low (0.150)</option>
          </select>
        </Field>
        <button className="btn btn-secondary align-end" onClick={run} disabled={busy}>{busy ? 'Building…' : res ? 'Rebuild network' : 'Build network'}</button>
      </div>
      <ErrorNote error={err} />
      {res && (
        <>
          <p className="small">{plural(res.stats.nodes, 'protein')} · {plural(res.stats.edges, 'interaction')} · average degree {res.stats.averageDegree}{res.stats.unmapped ? ` · ${res.stats.unmapped} IDs not found in STRING` : ''}</p>
          {edgesMode === 'evidence' && (
            <div className="legend">{EVIDENCE.map(([, label, c]) => <span key={label}><i style={{ background: c }} />{label}</span>)}</div>
          )}
          <NetworkGraph nodes={graph.nodes} edges={graph.edges} handleRef={net} physics height={480} />
          <div className="row wrap">
            {res.link && <a className="btn btn-ghost" href={res.link} target="_blank" rel="noopener noreferrer">Open in STRING ↗</a>}
            <button className="btn btn-ghost" onClick={() => saveText(res.tsv, `${list.name}_string_network.tsv`, 'text/tab-separated-values')}>Download TSV</button>
            <button className="btn btn-ghost" onClick={() => net.current?.toPng(`${list.name}_string_network.png`)}>Download PNG</button>
          </div>
          <SaveDerived ids={res.nodes.map((n) => n.label)} defaultName={`${list.name} network`} taxid={list.organismTaxId} idFormat="USERINPUT"
            meta={{ method: 'ppi', derivedFrom: list.name }} onDone={onDone} />
        </>
      )}
    </>
  )
}
