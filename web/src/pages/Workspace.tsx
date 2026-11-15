import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { Link, useNavigate, useSearchParams } from 'react-router-dom'
import { api, downloadFromApi, getWorkspaceId } from '../api/client'
import { useConfig, useDeleteList, useDeleteRun, useInvalidateWorkspace, useRenameList, useWorkspace } from '../api/hooks'
import type { AnalyteList, Run } from '../api/types'
import { CopyButton, EmptyState, ErrorNote, Modal, Note, Spinner } from '../components/ui'
import { useUndoToast } from '../components/Undo'
import { MethodPicker, type InputKind } from '../features/inputs/MethodPicker'
import { InputModal } from '../features/inputs/InputModal'
import { NewRunPanel } from '../features/NewRunPanel'
import { QuickActionModal, type QuickAction } from '../features/utilities/QuickActions'
import { formatNum, plural } from '../lib/format'
import { ConceptIcon, IconLabel } from '../components/Icons'
import { SessionModal, SessionStatusPill, useSaveShortcut, useSessionStatus, type Tab as SessionTab } from '../features/session/Session'

type Dialog =
  | { type: 'picker' }
  | { type: 'input'; kind: InputKind; example?: boolean; text?: string; fileName?: string }
  | { type: 'quick'; action: QuickAction; listId?: string }
  | { type: 'newRun'; listId?: string }
  | { type: 'gmt' }
  | { type: 'session'; tab?: SessionTab }
  | null

export default function WorkspacePage() {
  const ws = getWorkspaceId()!
  const { data, error, isLoading } = useWorkspace(ws)
  const { data: config } = useConfig()
  const [dialog, setDialog] = useState<Dialog>(null)
  const sessionStatus = useSessionStatus(ws, data)
  useSaveShortcut(ws, useCallback(() => { sessionStatus.refetch() }, [sessionStatus]))
  const [params, setParams] = useSearchParams()
  const [selected, setSelected] = useState<string[]>([])
  const [toolFilter, setToolFilter] = useState<string>('all')
  const [listFilter, setListFilter] = useState<string>('all')
  const navigate = useNavigate()
  const invalidate = useInvalidateWorkspace()
  const exampleStarted = useRef(false)
  const [hiddenRuns, setHiddenRuns] = useState<Set<string>>(new Set())
  const undoToast = useUndoToast()
  const delRun = useDeleteRun()
  const removeRun = (r: Run) => {
    setHiddenRuns((h) => new Set(h).add(r.id))
    setSelected((s) => s.filter((x) => x !== r.id))
    undoToast(`Removed ${r.title}.`,
      () => setHiddenRuns((h) => { const n = new Set(h); n.delete(r.id); return n }),
      () => delRun.mutate(r.id))
  }
  const [exampleNote, setExampleNote] = useState<string | null>(null)

  // Preloaded examples, one per analysis option. Started from the Home tab
  // (?example=<kind>) or the Examples menu in the rail.
  const runExample = useCallback(async (kind: string) => {
    setExampleNote(null)
    const SOURCES = ['GO:BP', 'GO:MF', 'GO:CC', 'KEGG', 'REAC']
    const exampleList = async (name = 'Example genes') => {
      const ex = await api<{ text: string }>('/inputs/examples/upload')
      return api<AnalyteList>('/lists', { body: { name, text: ex.text, organismTaxId: 9606, uniqueName: true, meta: { source: 'example' } } })
    }
    const rankedList = async () => {
      const ex = await api<{ text: string }>('/inputs/examples/ranked')
      return api<AnalyteList>('/lists', { body: { name: 'Example ranked genes (DE)', text: ex.text, ranked: true, skipFirstRow: true, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'volcano-ranked' } } })
    }
    const deLists = async () => {
      const d = await api<{ up: string[]; down: string[] }>('/inputs/examples/de_lists')
      const up = await api<AnalyteList>('/lists', { body: { name: 'Example up-regulated', ids: d.up, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'volcano' } } })
      const down = await api<AnalyteList>('/lists', { body: { name: 'Example down-regulated', ids: d.down, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'volcano' } } })
      return { up, down, both: [...d.up, ...d.down] }
    }
    const startRuns = (body: Record<string, unknown>) => api<{ created: string[] }>('/runs', { body: { threshold: 0.05, ...body } })
    const preselect = (ids: string[]) => setSelected(ids)
    switch (kind) {
      case 'demo': {
        const list = await exampleList()
        await startRuns({ listId: list.id, tools: ['gprofiler', 'string'], datasources: SOURCES })
        setExampleNote('Example loaded: 200 human genes are being analysed with g:Profiler and STRING. Open a run when it finishes, or select both and press Compare.')
        break
      }
      case 'upset': {
        await exampleList()
        await exampleList('Example genes B')
        setDialog({ type: 'input', kind: 'upset' })
        break
      }
      case 'gsea': {
        const list = await rankedList()
        await startRuns({ listId: list.id, paradigm: 'GSEA', tools: ['clusterprofiler'], datasources: ['HALLMARK', 'KEGG', 'REAC', 'GO:BP'] })
        setExampleNote('GSEA example: about 6,000 genes from a differential-expression table, ranked by sign(log2FC) × −log10 p, are tested against MSigDB Hallmark, KEGG, Reactome and GO with clusterProfiler/fgsea. Positive NES = up-regulated sets, negative = down-regulated.')
        break
      }
      case 'compare-tools': {
        const list = await exampleList()
        const r = await startRuns({ listId: list.id, tools: ['gprofiler', 'string', 'enrichr', 'panther', 'clusterprofiler'], datasources: ['GO:BP', 'KEGG', 'REAC'] })
        preselect(r.created)
        setExampleNote(`Compare tools: the same 200 genes run through ${r.created.length} tools on GO:BP, KEGG and Reactome. The runs are preselected — press Compare when they finish, then open Statistics to see how far the tools agree.`)
        break
      }
      case 'compare-strategies': {
        const ranked = await rankedList()
        const { up, down, both } = await deLists()
        const sig = await api<AnalyteList>('/lists', { body: { name: 'Example significant (up + down)', ids: both, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'volcano' } } })
        const g = await startRuns({ listId: ranked.id, paradigm: 'GSEA', tools: ['clusterprofiler'], datasources: ['HALLMARK', 'KEGG', 'REAC'] })
        const o = await startRuns({ listId: sig.id, tools: ['clusterprofiler'], datasources: ['HALLMARK', 'KEGG', 'REAC'] })
        preselect([...g.created, ...o.created])
        void up; void down
        setExampleNote('Compare strategies: the same experiment analysed two ways with clusterProfiler on Hallmark, KEGG and Reactome — GSEA on all genes ranked by their change, and over-representation (ORA) on the significant genes only (|log2FC| ≥ 1, p ≤ 0.05). Press Compare when both finish.')
        break
      }
      case 'compare-lists': {
        const { up, down } = await deLists()
        const a = await startRuns({ listId: up.id, tools: ['gprofiler'], datasources: SOURCES })
        const b = await startRuns({ listId: down.id, tools: ['gprofiler'], datasources: SOURCES })
        preselect([...a.created, ...b.created])
        setExampleNote('Compare lists: up- and down-regulated genes of the example experiment, each analysed with g:Profiler. Press Compare when both finish to see which functions go up, which go down and which are shared.')
        break
      }
      case 'gmt': {
        const gmt = await api<{ text: string; name: string }>('/inputs/examples/gmt')
        const src = await api<{ id: string }>('/sources', { body: { name: gmt.name, text: gmt.text } })
        const list = await exampleList()
        await startRuns({ listId: list.id, tools: [], sourceIds: [src.id], datasources: [] })
        setExampleNote('Custom gene sets: the MSigDB Hallmark collection was uploaded as a GMT file and tested on 200 example genes (hypergeometric test, Benjamini–Hochberg). Upload your own GMT file under Custom sources the same way.')
        break
      }
      case 'ppi': {
        const list = await exampleList()
        invalidate()
        setDialog({ type: 'quick', action: 'ppi', listId: list.id })
        break
      }
      case 'variants':
      case 'gwas':
        navigate(`/w/${ws}/variants?example=${kind}`)
        return
      default:
        if (['upload', 'snps', 'textmining', 'volcano', 'reduction', 'expression', 'mirna'].includes(kind)) setDialog({ type: 'input', kind: kind as InputKind, example: true })
        return
    }
    invalidate()
  }, [invalidate, navigate, ws])

  useEffect(() => {
    const kind = params.get('example')
    if (!kind || !data || exampleStarted.current) return
    exampleStarted.current = true
    setParams({}, { replace: true })
    runExample(kind).catch((e) => setExampleNote(`The example could not be loaded: ${e.message}`))
  }, [params, data, setParams, runExample])

  const runs = (data?.runs ?? []).filter((r) => !hiddenRuns.has(r.id))
  const lists = data?.lists ?? []
  const shownRuns = runs.filter((r) => (toolFilter === 'all' || r.toolKey === toolFilter) && (listFilter === 'all' || r.listName === listFilter))
  const selectableRuns = selected.filter((id) => runs.find((r) => r.id === id && r.status === 'complete' && (r.resultCount ?? 0) > 0))
  const pending = runs.filter((r) => r.status === 'queued' || r.status === 'running').length

  const step = lists.length === 0 ? 1 : runs.length === 0 ? 2 : 3

  if (isLoading) return <div className="page-center"><Spinner label="Opening workspace" /></div>
  if (error) {
    return (
      <div className="page-center stack">
        <ErrorNote error={error} />
        <Link className="btn btn-primary" to="/start?new=1">
          Start a new workspace
        </Link>
      </div>
    )
  }

  return (
    <div className="workspace">
      <aside className="rail" aria-label="Lists and tools">
        <ListsSection lists={lists} max={config?.limits.maxLists ?? 10} onAdd={() => setDialog({ type: 'picker' })}
          onRun={(id) => setDialog({ type: 'newRun', listId: id })} onQuick={(action, id) => setDialog({ type: 'quick', action, listId: id })} />
        <section className="rail-section">
          <div className="rail-head">
            <h2>Custom sources</h2>
            <button className="btn btn-ghost btn-sm" onClick={() => setDialog({ type: 'gmt' })} title="Upload gene sets as a GMT file"><IconLabel icon="gmt">+ GMT</IconLabel></button>
          </div>
          {(data?.sources ?? []).length === 0 ? (
            <p className="muted small">Upload your own gene sets (GMT) to test them like any other database.</p>
          ) : (
            <ul className="plain-list">
              {data!.sources.map((s) => (
                <li key={s.id} className="rail-item">
                  <span>{s.name}</span>
                  <span className="muted small">{s.termCount ?? s.term_count} sets</span>
                  <button className="icon-btn" aria-label={`Delete ${s.name}`} onClick={() => api(`/sources/${s.id}`, { method: 'DELETE' }).then(invalidate)}>×</button>
                </li>
              ))}
            </ul>
          )}
        </section>
        <section className="rail-section">
          <h2>Variants and GWAS</h2>
          <p className="muted small">Annotate SNPs, VCF variants or GWAS results: consequences, genes, frequencies, ClinVar and trait associations.</p>
          <Link className="btn btn-secondary btn-block" to={`/w/${ws}/variants`}><IconLabel icon="variants">Variant analysis</IconLabel></Link>
        </section>
        <section className="rail-section">
          <h2>Quick actions</h2>
          <div className="stack-sm">
            <button className="btn btn-secondary btn-block" disabled={!lists.length} onClick={() => setDialog({ type: 'quick', action: 'convert' })}><IconLabel icon="convert">Convert IDs</IconLabel></button>
            <button className="btn btn-secondary btn-block" disabled={!lists.length} onClick={() => setDialog({ type: 'quick', action: 'orthologs' })}><IconLabel icon="orthologs">Find orthologs</IconLabel></button>
            <button className="btn btn-secondary btn-block" disabled={!lists.length} onClick={() => setDialog({ type: 'quick', action: 'ppi' })}><IconLabel icon="ppi">Protein network (STRING)</IconLabel></button>
          </div>
          {!lists.length && <p className="muted small">Add a list first, or <button className="link-btn" onClick={() => runExample('ppi').catch((e) => setExampleNote(`The example could not be loaded: ${e.message}`))}>load an example list</button>.</p>}
        </section>
        <ExamplesSection onPick={(k) => runExample(k).catch((e) => setExampleNote(`The example could not be loaded: ${e.message}`))} />
        <section className="rail-section">
          <button className="btn btn-ghost btn-block" onClick={() => setDialog({ type: 'session', tab: 'status' })}><IconLabel icon="session">Session: save, load, status…</IconLabel></button>
        </section>
      </aside>

      <div className="workspace-main">
        <ol className="progress-steps" aria-label="Progress">
          <li className={step >= 1 ? (step > 1 ? 'done' : 'current') : ''}>
            <span>1</span> Add a list
          </li>
          <li className={step >= 2 ? (step > 2 ? 'done' : 'current') : ''}>
            <span>2</span> Run enrichment
          </li>
          <li className={step >= 3 ? 'current' : ''}>
            <span>3</span> Explore and compare
          </li>
        </ol>

        <ShareBanner ws={ws} expires={data?.expires} />
        <SessionStatusPill status={sessionStatus.data} onOpen={(tab) => setDialog({ type: 'session', tab })} />
        {exampleNote && <Note kind="success">{exampleNote}</Note>}

        <div className="page-header">
          <div>
            <h1>Enrichment runs</h1>
            <p className="muted">
              {runs.length ? `${plural(runs.length, 'run')}${pending ? ` · ${pending} in progress` : ''}` : 'Results of each tool appear here as cards.'}
            </p>
          </div>
          <div className="row">
            <button
              className="btn btn-secondary"
              disabled={selectableRuns.length < 2}
              title={selectableRuns.length < 2 ? 'Select two or more finished runs to compare them' : ''}
              onClick={() => navigate(`/w/${ws}/compare?runs=${selectableRuns.join(',')}`)}
            >
              <IconLabel icon="compare">Compare{selectableRuns.length ? ` (${selectableRuns.length})` : ''}</IconLabel>
            </button>
            <button className="btn btn-primary" disabled={!lists.length} onClick={() => setDialog({ type: 'newRun' })}>
              <IconLabel icon="run">New run</IconLabel>
            </button>
          </div>
        </div>

        {lists.length === 0 ? (
          <EmptyState title="Start by adding a gene list">
            <p>Paste IDs, upload a file, or pick genes from a plot. Not sure? Load an example and see how it works.</p>
            <div className="row center">
              <button className="btn btn-primary" onClick={() => setDialog({ type: 'picker' })}>Add a list</button>
              <button className="btn btn-secondary" onClick={() => setDialog({ type: 'input', kind: 'upload', example: true })}>Use example genes</button>
            </div>
          </EmptyState>
        ) : runs.length === 0 ? (
          <EmptyState title="Your list is ready — now run an enrichment">
            <p>Choose one or more tools. The defaults (GO and KEGG, FDR &lt; 0.05) are a good start.</p>
            <button className="btn btn-primary" onClick={() => setDialog({ type: 'newRun' })}>Run enrichment</button>
          </EmptyState>
        ) : (
          <>
            <div className="filters-row" role="group" aria-label="Filter runs">
              <label>
                Tool{' '}
                <select className="input input-sm" value={toolFilter} onChange={(e) => setToolFilter(e.target.value)}>
                  <option value="all">All tools</option>
                  {[...new Set(runs.map((r) => r.toolKey))].map((k) => (
                    <option key={k} value={k}>{runs.find((r) => r.toolKey === k)?.tool}</option>
                  ))}
                </select>
              </label>
              <label>
                List{' '}
                <select className="input input-sm" value={listFilter} onChange={(e) => setListFilter(e.target.value)}>
                  <option value="all">All lists</option>
                  {[...new Set(runs.map((r) => r.listName))].map((n) => (
                    <option key={n} value={n}>{n}</option>
                  ))}
                </select>
              </label>
              {selected.length > 0 && (
                <button className="btn btn-ghost btn-sm" onClick={() => setSelected([])}>Clear selection</button>
              )}
              <span className="muted small">Tip: tick two or more runs to compare tools or lists.</span>
            </div>
            <div className="run-grid">
              {shownRuns.map((r) => (
                <RunCard key={r.id} run={r} onRemove={() => removeRun(r)} selected={selected.includes(r.id)}
                  onToggle={() => setSelected((s) => (s.includes(r.id) ? s.filter((x) => x !== r.id) : [...s, r.id]))}
                  onOpen={() => navigate(`/w/${ws}/runs/${r.id}`)} />
              ))}
            </div>
          </>
        )}
      </div>

      {dialog?.type === 'picker' && (
        <MethodPicker lists={lists} onClose={() => setDialog(null)} onPick={(kind) => setDialog({ type: 'input', kind })} />
      )}
      {dialog?.type === 'input' && (
        <InputModal key={`${dialog.kind}-${dialog.fileName ?? ''}`} kind={dialog.kind} example={dialog.example} lists={lists} onClose={() => setDialog(null)}
          initialText={dialog.text} initialFileName={dialog.fileName}
          onOpenVolcano={(text, fileName) => setDialog({ type: 'input', kind: 'volcano', text, fileName })}
          onCreated={(list) => setDialog({ type: 'newRun', listId: list.id })} />
      )}
      {dialog?.type === 'quick' && (
        <QuickActionModal action={dialog.action} lists={lists} initialListId={dialog.listId} onClose={() => setDialog(null)} />
      )}
      {dialog?.type === 'newRun' && (
        <NewRunPanel lists={lists} sources={data?.sources ?? []} initialListId={dialog.listId} onClose={() => setDialog(null)} />
      )}
      {dialog?.type === 'gmt' && <GmtModal onClose={() => setDialog(null)} />}
      {dialog?.type === 'session' && <SessionModal ws={ws} summary={data} status={sessionStatus.data} initialTab={dialog.tab} onClose={() => { setDialog(null); sessionStatus.refetch() }} />}
    </div>
  )
}

function ShareBanner({ ws, expires }: { ws: string; expires?: number }) {
  const url = `${window.location.origin}/w/${ws}`
  const days = expires ? Math.max(1, Math.round((expires * 1000 - Date.now()) / 86_400_000)) : null
  return (
    <div className="share-banner">
      <span>
        <strong>Bookmark this link</strong> to come back to your lists and results{days ? ` (kept for ${days} more days)` : ''}. Anyone with the link can view this workspace.
      </span>
      <code className="share-url">{url}</code>
      <CopyButton text={url} label="Copy link" />
    </div>
  )
}

function ListsSection({ lists, max, onAdd, onRun, onQuick }: {
  lists: AnalyteList[]; max: number; onAdd: () => void; onRun: (id: string) => void; onQuick: (a: QuickAction, id: string) => void
}) {
  const rename = useRenameList()
  const del = useDeleteList()
  const undoToast = useUndoToast()
  const invalidate = useInvalidateWorkspace()
  const deleteWithUndo = async (l: AnalyteList) => {
    const full = await api<AnalyteList & { scores?: number[] }>(`/lists/${l.id}`)
    await del.mutateAsync(l.id)
    undoToast(`Deleted list "${l.name}".`, () =>
      api('/lists', { body: { name: full.name, ids: full.ids, scores: full.scores, organismTaxId: full.organismTaxId,
        analyteType: full.analyteType, idFormat: full.idFormat, meta: full.meta, uniqueName: true } }).then(invalidate))
  }
  const [editing, setEditing] = useState<string | null>(null)
  const [name, setName] = useState('')
  const [menu, setMenu] = useState<string | null>(null)
  return (
    <section className="rail-section">
      <div className="rail-head">
        <h2>Lists <span className="count">{lists.length}/{max}</span></h2>
      </div>
      <button className="btn btn-primary btn-block" onClick={onAdd} disabled={lists.length >= max}>+ Add list</button>
      {lists.length >= max && <p className="muted small">List limit reached. Delete a list to add another.</p>}
      <ErrorNote error={rename.error ?? del.error} />
      <ul className="plain-list">
        {lists.map((l) => (
          <li key={l.id} className="rail-item list-item">
            {editing === l.id ? (
              <form className="row" onSubmit={(e) => {
                e.preventDefault()
                const old = l.name
                rename.mutate({ id: l.id, name }, { onSuccess: (nl) => {
                  setEditing(null)
                  undoToast(`Renamed to "${nl.name}".`, () => rename.mutateAsync({ id: l.id, name: old }))
                } })
              }}>
                <input className="input input-sm" autoFocus value={name} onChange={(e) => setName(e.target.value)}
                  onKeyDown={(e) => e.key === 'Escape' && setEditing(null)} aria-label="New list name" maxLength={80} />
                <button className="btn btn-sm btn-primary">Save</button>
              </form>
            ) : (
              <>
                <div className="list-text">
                  <span className="list-name" title={l.name}>{l.name}</span>
                  <span className="muted small">{formatNum(l.size)} IDs · {l.organismName.replace(/ \[NCBI.*$/, '')}{l.ranked && <> · <span className="tag-ranked" title="IDs with scores: can be used for GSEA">ranked</span></>}</span>
                </div>
                <div className="menu-wrap">
                  <button className="icon-btn" aria-label={`Actions for ${l.name}`} aria-expanded={menu === l.id} onClick={() => setMenu(menu === l.id ? null : l.id)}>⋯</button>
                  {menu === l.id && (
                    <div className="menu" onMouseLeave={() => setMenu(null)}>
                      <button onClick={() => { setMenu(null); onRun(l.id) }}>Run enrichment</button>
                      <button onClick={() => { setMenu(null); setEditing(l.id); setName(l.name) }}>Rename</button>
                      <button onClick={() => { setMenu(null); downloadFromApi(`/lists/${l.id}/csv`, `${l.name}.csv`) }}>Download CSV</button>
                      <button onClick={() => { setMenu(null); onQuick('convert', l.id) }}>Convert IDs</button>
                      <button onClick={() => { setMenu(null); onQuick('orthologs', l.id) }}>Find orthologs</button>
                      <button onClick={() => { setMenu(null); onQuick('ppi', l.id) }}>Protein network</button>
                      <button className="danger" onClick={() => { setMenu(null); deleteWithUndo(l) }}>Delete</button>
                    </div>
                  )}
                </div>
              </>
            )}
          </li>
        ))}
      </ul>
    </section>
  )
}

function RunCard({ run, selected, onToggle, onOpen, onRemove }: { run: Run; selected: boolean; onToggle: () => void; onOpen: () => void; onRemove: () => void }) {
  const busy = run.status === 'queued' || run.status === 'running'
  const empty = run.status === 'complete' && !run.resultCount
  const canSelect = run.status === 'complete' && !!run.resultCount
  const sources = Array.isArray(run.params?.datasources) ? run.params.datasources : [run.params?.datasources].filter(Boolean)
  return (
    <article className={`run-card${selected ? ' selected' : ''}${run.status === 'error' ? ' error' : ''}`} style={{ borderLeftColor: `var(--color-${run.toolKey}, var(--accent-color))` }}>
      <header className="run-card-head">
        <label className="run-check" title={canSelect ? 'Select to compare' : 'Only finished runs with results can be compared'}>
          <input type="checkbox" checked={selected} disabled={!canSelect} onChange={onToggle} aria-label={`Select ${run.title} for comparison`} />
        </label>
        <h3>{run.title}</h3>
        <button className="icon-btn" aria-label={`Remove ${run.title}`} title="Remove (you can undo)" onClick={onRemove}>×</button>
      </header>
      <p className="muted small">{run.listName} · {run.organismName.replace(/ \[NCBI.*$/, '')}</p>
      <p className="small">{sources.slice(0, 4).join(', ')}{sources.length > 4 ? ` +${sources.length - 4}` : ''} · {run.params?.metric} &lt; {run.params?.threshold}</p>
      <div className="run-card-foot">
        {busy && <Spinner label={run.status === 'queued' ? 'Waiting…' : 'Running…'} />}
        {run.status === 'error' && <span className="text-danger small">{run.error ?? 'The tool did not respond.'}</span>}
        {empty && <span className="muted small">No significant terms{run.messages?.length ? ` — ${run.messages[0]}` : ''}</span>}
        {canSelect && (
          <button className="btn btn-primary btn-sm" onClick={onOpen}>
            View {run.resultCount} terms →
          </button>
        )}
      </div>
    </article>
  )
}

function GmtModal({ onClose }: { onClose: () => void }) {
  const [name, setName] = useState('My gene sets')
  const [text, setText] = useState('')
  const [err, setErr] = useState<unknown>(null)
  const invalidate = useInvalidateWorkspace()
  const sets = useMemo(() => text.split(/\r?\n/).filter((l) => l.split('\t').length >= 3).length, [text])
  return (
    <Modal title="Upload custom gene sets (GMT)" onClose={onClose}
      subtitle="Each line: set ID ⇥ description ⇥ gene ⇥ gene … (tab-separated). Gene IDs must match your list's IDs."
      footer={<>
        <button className="btn btn-ghost" onClick={onClose}>Cancel</button>
        <button className="btn btn-primary" disabled={!sets} onClick={() =>
          api('/sources', { body: { name, text } }).then(() => { invalidate(); onClose() }).catch(setErr)}>Add source ({sets} sets)</button>
      </>}>
      <label className="field"><span className="field-label">Name</span>
        <input className="input" value={name} maxLength={80} onChange={(e) => setName(e.target.value)} /></label>
      <label className="field"><span className="field-label">GMT file</span>
        <input type="file" accept=".gmt,.txt,.tsv" onChange={(e) => e.target.files?.[0]?.text().then(setText)} /></label>
      <textarea className="input mono" rows={8} value={text} onChange={(e) => setText(e.target.value)} placeholder={'HALLMARK_APOPTOSIS\tApoptosis\tCASP3\tCASP8\tBAX\t…'} aria-label="GMT content" />
      <ErrorNote error={err} />
    </Modal>
  )
}


export const WORKSPACE_EXAMPLES: { kind: string; title: string; text: string }[] = [
  { kind: 'demo', title: 'Gene list → two tools', text: '200 genes through g:Profiler and STRING' },
  { kind: 'gsea', title: 'Ranked list (GSEA)', text: 'All genes of an experiment, ranked by change' },
  { kind: 'compare-tools', title: 'Compare tools', text: 'One list, five enrichment tools' },
  { kind: 'compare-strategies', title: 'Compare strategies', text: 'GSEA on all genes vs ORA on significant genes' },
  { kind: 'compare-lists', title: 'Compare lists', text: 'Up- vs down-regulated genes' },
  { kind: 'gmt', title: 'Custom gene sets', text: 'MSigDB Hallmark uploaded as GMT' },
  { kind: 'ppi', title: 'Protein network', text: 'STRING interactions of an example list' },
  { kind: 'volcano', title: 'Volcano plot', text: 'Select up- and down-regulated genes' },
  { kind: 'reduction', title: 'UMAP / 2D plot', text: 'Lasso a cluster on a UMAP of single cells' },
  { kind: 'expression', title: 'Expression matrix', text: 'Clusters, gene map, two-group comparison' },
  { kind: 'mirna', title: 'microRNAs', text: 'From miRNAs to their target genes' },
  { kind: 'variants', title: 'Variants', text: 'rsIDs and VCF lines → genes and traits' },
  { kind: 'gwas', title: 'GWAS results', text: 'Summary statistics → loci, genes, traits' },
]

function ExamplesSection({ onPick }: { onPick: (kind: string) => Promise<void> | void }) {
  const [busy, setBusy] = useState<string | null>(null)
  return (
    <section className="rail-section">
      <h2>Examples</h2>
      <p className="muted small">Load ready-made data for any analysis and see what it does.</p>
      <div className="example-list">
        {WORKSPACE_EXAMPLES.map((e) => (
          <button key={e.kind} type="button" className="example-mini" disabled={!!busy}
            onClick={async () => { setBusy(e.kind); try { await onPick(e.kind) } finally { setBusy(null) } }}>
            <ConceptIcon name={e.kind} />
            <span><strong>{busy === e.kind ? 'Loading…' : e.title}</strong><span className="muted small">{e.text}</span></span>
          </button>
        ))}
      </div>
    </section>
  )
}
