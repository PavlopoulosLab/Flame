import { useEffect, useMemo, useRef, useState } from 'react'
import { Link, useNavigate, useSearchParams } from 'react-router-dom'
import { api, downloadFromApi, getWorkspaceId, saveText } from '../api/client'
import { useConfig, useDeleteList, useDeleteRun, useInvalidateWorkspace, useRenameList, useWorkspace } from '../api/hooks'
import type { AnalyteList, Run } from '../api/types'
import { CopyButton, EmptyState, ErrorNote, Modal, Note, Spinner } from '../components/ui'
import { MethodPicker, type InputKind } from '../features/inputs/MethodPicker'
import { InputModal } from '../features/inputs/InputModal'
import { NewRunPanel } from '../features/NewRunPanel'
import { QuickActionModal, type QuickAction } from '../features/utilities/QuickActions'
import { formatNum, plural } from '../lib/format'

type Dialog =
  | { type: 'picker' }
  | { type: 'input'; kind: InputKind; example?: boolean }
  | { type: 'quick'; action: QuickAction; listId?: string }
  | { type: 'newRun'; listId?: string }
  | { type: 'gmt' }
  | { type: 'session' }
  | null

export default function WorkspacePage() {
  const ws = getWorkspaceId()!
  const { data, error, isLoading } = useWorkspace(ws)
  const { data: config } = useConfig()
  const [dialog, setDialog] = useState<Dialog>(null)
  const [params, setParams] = useSearchParams()
  const [selected, setSelected] = useState<string[]>([])
  const [toolFilter, setToolFilter] = useState<string>('all')
  const [listFilter, setListFilter] = useState<string>('all')
  const navigate = useNavigate()
  const invalidate = useInvalidateWorkspace()
  const exampleStarted = useRef(false)
  const [exampleNote, setExampleNote] = useState<string | null>(null)

  // Examples from the Home tab: ?example=<kind>
  useEffect(() => {
    const kind = params.get('example')
    if (!kind || !data || exampleStarted.current) return
    exampleStarted.current = true
    setParams({}, { replace: true })
    const run = async () => {
      if (kind === 'demo' || kind === 'upset') {
        const ex1 = await api<{ text: string }>('/inputs/examples/upload')
        const list = await api<AnalyteList>('/lists', {
          body: { name: 'Example genes', text: ex1.text, organismTaxId: 9606, uniqueName: true, meta: { source: 'example' } },
        })
        if (kind === 'upset') {
          const ex2 = await api<{ text: string }>('/inputs/examples/upload')
          await api('/lists', { body: { name: 'Example genes B', text: ex2.text, organismTaxId: 9606, uniqueName: true } })
          invalidate()
          setDialog({ type: 'input', kind: 'upset' })
          return
        }
        await api('/runs', {
          body: { listId: list.id, tools: ['gprofiler', 'string'], datasources: ['GO:BP', 'GO:MF', 'GO:CC', 'KEGG', 'REAC'], threshold: 0.05 },
        })
        setExampleNote('Example loaded: 200 human genes are being analysed with g:Profiler and STRING. Open a run when it finishes, or select both and press Compare.')
        invalidate()
      } else if (['upload', 'snps', 'textmining', 'volcano', 'reduction'].includes(kind)) {
        setDialog({ type: 'input', kind: kind as InputKind, example: true })
      }
    }
    run().catch((e) => setExampleNote(`The example could not be loaded: ${e.message}`))
  }, [params, data, setParams, invalidate])

  const runs = data?.runs ?? []
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
            <button className="btn btn-ghost btn-sm" onClick={() => setDialog({ type: 'gmt' })}>+ GMT</button>
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
          <h2>Quick actions</h2>
          <div className="stack-sm">
            <button className="btn btn-secondary btn-block" disabled={!lists.length} onClick={() => setDialog({ type: 'quick', action: 'convert' })}>Convert IDs</button>
            <button className="btn btn-secondary btn-block" disabled={!lists.length} onClick={() => setDialog({ type: 'quick', action: 'orthologs' })}>Find orthologs</button>
            <button className="btn btn-secondary btn-block" disabled={!lists.length} onClick={() => setDialog({ type: 'quick', action: 'ppi' })}>Protein network (STRING)</button>
          </div>
          {!lists.length && <p className="muted small">Add a list first.</p>}
        </section>
        <section className="rail-section">
          <button className="btn btn-ghost btn-block" onClick={() => setDialog({ type: 'session' })}>Save, load or clear session…</button>
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
              Compare{selectableRuns.length ? ` (${selectableRuns.length})` : ''}
            </button>
            <button className="btn btn-primary" disabled={!lists.length} onClick={() => setDialog({ type: 'newRun' })}>
              + New run
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
                <RunCard key={r.id} run={r} selected={selected.includes(r.id)}
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
        <InputModal kind={dialog.kind} example={dialog.example} lists={lists} onClose={() => setDialog(null)}
          onCreated={(list) => setDialog({ type: 'newRun', listId: list.id })} />
      )}
      {dialog?.type === 'quick' && (
        <QuickActionModal action={dialog.action} lists={lists} initialListId={dialog.listId} onClose={() => setDialog(null)} />
      )}
      {dialog?.type === 'newRun' && (
        <NewRunPanel lists={lists} sources={data?.sources ?? []} initialListId={dialog.listId} onClose={() => setDialog(null)} />
      )}
      {dialog?.type === 'gmt' && <GmtModal onClose={() => setDialog(null)} />}
      {dialog?.type === 'session' && <SessionModal hasEmail={!!data?.hasEmail} onClose={() => setDialog(null)} />}
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
                rename.mutate({ id: l.id, name }, { onSuccess: () => setEditing(null) })
              }}>
                <input className="input input-sm" autoFocus value={name} onChange={(e) => setName(e.target.value)}
                  onKeyDown={(e) => e.key === 'Escape' && setEditing(null)} aria-label="New list name" maxLength={80} />
                <button className="btn btn-sm btn-primary">Save</button>
              </form>
            ) : (
              <>
                <div className="list-text">
                  <span className="list-name" title={l.name}>{l.name}</span>
                  <span className="muted small">{formatNum(l.size)} IDs · {l.organismName.replace(/ \[NCBI.*$/, '')}</span>
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
                      <button className="danger" onClick={() => { setMenu(null); if (confirm(`Delete the list "${l.name}"? Runs made from it are kept.`)) del.mutate(l.id) }}>Delete</button>
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

function RunCard({ run, selected, onToggle, onOpen }: { run: Run; selected: boolean; onToggle: () => void; onOpen: () => void }) {
  const del = useDeleteRun()
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
        <button className="icon-btn" aria-label={`Remove ${run.title}`} onClick={() => confirm(`Remove ${run.title}?`) && del.mutate(run.id)}>×</button>
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

function SessionModal({ hasEmail, onClose }: { hasEmail: boolean; onClose: () => void }) {
  const { data: config } = useConfig()
  const invalidate = useInvalidateWorkspace()
  const navigate = useNavigate()
  const [err, setErr] = useState<unknown>(null)
  const [msg, setMsg] = useState<string | null>(null)
  const [email, setEmail] = useState('')
  return (
    <Modal title="Session" onClose={onClose} subtitle="Save everything to a file, load it later (or on another server), or clear your data.">
      <div className="stack">
        <section>
          <h3>Save session</h3>
          <p className="muted small">Downloads your lists, custom sources and finished results as one JSON file.</p>
          <button className="btn btn-secondary" onClick={() =>
            api<unknown>('/workspace/export').then((d) => saveText(JSON.stringify(d), 'flame-session.json', 'application/json')).catch(setErr)}>Download session file</button>
        </section>
        <section>
          <h3>Load session</h3>
          <p className="muted small">Adds the lists and results from a session file to this workspace.</p>
          <input type="file" accept=".json,application/json" aria-label="Session file" onChange={(e) => {
            const f = e.target.files?.[0]
            if (!f) return
            f.text().then((t) => api<{ lists: number; runs: number }>('/workspace/import', { body: JSON.parse(t) }))
              .then((r) => { setMsg(`Loaded ${plural(r.lists, 'list')} and ${plural(r.runs, 'run')}.`); invalidate() })
              .catch(setErr)
          }} />
        </section>
        {config?.emailNotifications && (
          <section>
            <h3>Email when runs finish (optional)</h3>
            <p className="muted small">{hasEmail ? 'An email address is set.' : 'Leave an address to get a single email when all runs are done. It is only used for this.'}</p>
            <div className="row">
              <input className="input" type="email" value={email} onChange={(e) => setEmail(e.target.value)} placeholder="you@example.org (optional)" aria-label="Email (optional)" />
              <button className="btn btn-secondary" onClick={() => api('/workspace/email', { method: 'PUT', body: { email } }).then(() => setMsg('Saved.')).catch(setErr)}>Save</button>
            </div>
          </section>
        )}
        <section>
          <h3>Clear or delete</h3>
          <div className="row">
            <button className="btn btn-secondary" onClick={() => confirm('Remove all lists, runs and sources from this workspace? This cannot be undone.') &&
              api('/workspace/data', { method: 'DELETE' }).then(() => { invalidate(); onClose() }).catch(setErr)}>Clear all data</button>
            <button className="btn btn-danger" onClick={() => confirm('Permanently delete this workspace and its link? This cannot be undone.') &&
              api('/workspace', { method: 'DELETE' }).then(() => { try { sessionStorage.removeItem('flame-last-workspace') } catch { /* ignore */ } navigate('/') }).catch(setErr)}>Delete workspace</button>
          </div>
        </section>
        {msg && <Note kind="success">{msg}</Note>}
        <ErrorNote error={err} />
      </div>
    </Modal>
  )
}
