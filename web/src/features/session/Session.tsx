import { CHART_TEXT_KEY, getChartText, hasSavedChartText, sanitizeChartText, setChartText } from '../../lib/chartText'
import { useQuery } from '@tanstack/react-query'
import { useEffect, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { api, saveBlob, setWorkspaceId } from '../../api/client'
import { useConfig, useInvalidateWorkspace } from '../../api/hooks'
import type { Run, WorkspaceSummary } from '../../api/types'
import { BusyBar, CopyButton, ErrorNote, Modal, Note, Segmented } from '../../components/ui'
import { useUndoToast } from '../../components/Undo'
import { IconLabel } from '../../components/Icons'
import { PALETTES, getCustomPalette, hasSavedCustomPalette, setCustomPalette, setPaletteMode, type CustomPalette, type PaletteMode } from '../../lib/palette'
import { formatNum, plural } from '../../lib/format'

export interface SessionStatus {
  created: number; lastActive: number; expires: number; retentionDays: number; lastSaved: number | null
  state: 'empty' | 'unsaved' | 'saved' | 'changed'
  counts: { lists: number; maxLists: number; sources: number; runsComplete: number; runsActive: number; runsFailed: number; variantAnalyses: number; variantActive: number; analyses?: number; analysesActive?: number }
  storageBytes: number; maxSessionBytes: number
}

interface SessionFile {
  format: string; version?: number; flameVersion?: string; exported?: string; complete?: boolean
  contents?: { lists: number; sources: number; runs: number; variants: number; analyses?: number }
  lists?: { name: string; size?: number; ids?: unknown[]; ranked?: boolean; organismName?: string }[]
  sources?: { name: string }[]; runs?: { title?: string; tool?: string; listName?: string }[]; variants?: { name: string }[]; analyses?: { label: string }[]
  prefs?: Record<string, string>
}

type Part = 'lists' | 'sources' | 'runs' | 'variants' | 'analyses'
const PARTS: { key: Part; label: string }[] = [
  { key: 'lists', label: 'Gene lists' }, { key: 'sources', label: 'Custom gene sets' },
  { key: 'runs', label: 'Finished enrichment results' }, { key: 'variants', label: 'Variant / GWAS analyses' },
  { key: 'analyses', label: 'Regulator, drug, module and robustness analyses' },
]
const PREF_SKIP = new Set(['flame-last-workspace'])

export const ago = (t: number | null | undefined) => {
  if (!t) return ''
  const s = Math.max(0, Date.now() / 1000 - t)
  if (s < 60) return 'just now'
  if (s < 3600) return `${Math.round(s / 60)} min ago`
  if (s < 86400) return `${Math.round(s / 3600)} h ago`
  return `${Math.round(s / 86400)} days ago`
}
const date = (t: number) => new Date(t * 1000).toLocaleDateString(undefined, { day: 'numeric', month: 'short', year: 'numeric' })
const mb = (b: number) => (b < 1048576 ? `${Math.max(1, Math.round(b / 1024))} kB` : `${(b / 1048576).toFixed(1)} MB`)

export function useSessionStatus(ws: string, summary?: WorkspaceSummary) {
  return useQuery({
    queryKey: ['session-status', ws, summary?.lists.length, summary?.runs.map((r) => r.status).join(), summary?.sources.length],
    queryFn: () => api<SessionStatus>('/workspace/status', { workspace: ws }),
    refetchInterval: 60_000,
  })
}

const STATE_LABEL: Record<SessionStatus['state'], { text: string; cls: string; hint: string }> = {
  empty: { text: 'Empty session', cls: 'idle', hint: 'Nothing to save yet' },
  unsaved: { text: 'Not saved to a file', cls: 'warn', hint: 'Your work is kept on the server for a while; download a session file to keep it for good' },
  saved: { text: 'Saved', cls: 'ok', hint: 'Everything is in your last session file' },
  changed: { text: 'Changes since last save', cls: 'warn', hint: 'You added or changed lists, runs or analyses after your last session file' },
}

/** Compact status shown in the workspace header; opens the Session dialog. */
export function SessionStatusPill({ status, onOpen }: { status?: SessionStatus; onOpen: (tab: Tab) => void }) {
  if (!status) return null
  const st = STATE_LABEL[status.state]
  const active = status.counts.runsActive + status.counts.variantActive + (status.counts.analysesActive ?? 0)
  return (
    <div className="session-pill" role="status">
      <button type="button" className={`session-state ${st.cls}`} title={st.hint} onClick={() => onOpen('status')}>
        <i aria-hidden="true" />{st.text}{status.state === 'saved' || status.state === 'changed' ? ` · ${ago(status.lastSaved)}` : ''}
      </button>
      {active > 0 && <span className="session-active" title="Jobs still running">⟳ {plural(active, 'job')} running</span>}
      <span className="muted small" title={`Workspaces are deleted after ${status.retentionDays} days without use`}>Kept until {date(status.expires)}</span>
      <button type="button" className="btn btn-secondary btn-sm" onClick={() => onOpen('save')} title="Save session (Ctrl/⌘+S)"><IconLabel icon="save">Save</IconLabel></button>
      <button type="button" className="btn btn-ghost btn-sm" onClick={() => onOpen('load')}><IconLabel icon="load">Load</IconLabel></button>
    </div>
  )
}

async function gzip(text: string): Promise<Blob> {
  const stream = new Blob([text]).stream().pipeThrough(new CompressionStream('gzip'))
  return new Response(stream).blob()
}
async function readSessionFile(f: File): Promise<string> {
  const head = new Uint8Array(await f.slice(0, 2).arrayBuffer())
  if (head[0] === 0x1f && head[1] === 0x8b) {
    if (typeof DecompressionStream === 'undefined') throw new Error('This browser cannot open compressed session files.')
    return new Response(f.stream().pipeThrough(new DecompressionStream('gzip'))).text()
  }
  return f.text()
}

function collectPrefs(): Record<string, string> {
  const out: Record<string, string> = {}
  try {
    for (let i = 0; i < sessionStorage.length; i++) {
      const k = sessionStorage.key(i)
      if (k && k.startsWith('flame-') && !PREF_SKIP.has(k)) out[k] = sessionStorage.getItem(k) ?? ''
    }
  } catch { /* storage unavailable */ }
  if (hasSavedCustomPalette()) out['flame-custom-palette'] = JSON.stringify(getCustomPalette())   // "My colours"
  if (hasSavedChartText()) out[CHART_TEXT_KEY] = JSON.stringify(getChartText())                    // text of the charts
  return out
}
function applyPrefs(prefs: Record<string, string>) {
  try {
    Object.entries(prefs).forEach(([k, v]) => {
      if (k.startsWith('flame-') && !PREF_SKIP.has(k) && typeof v === 'string' && v.length < 20_000) sessionStorage.setItem(k, v)
    })
  } catch { /* storage unavailable */ }
  if (PALETTES.some((p) => p.value === prefs['flame-palette'])) setPaletteMode(prefs['flame-palette'] as PaletteMode)   // only known modes from a session file
  if (prefs['flame-custom-palette']) {
    try { setCustomPalette(JSON.parse(prefs['flame-custom-palette']) as CustomPalette) } catch { /* not a palette */ }
  }
  if (prefs[CHART_TEXT_KEY]) {
    try { setChartText(sanitizeChartText(JSON.parse(prefs[CHART_TEXT_KEY]))) } catch { /* not a text style */ }
  }
  if (prefs['flame-theme'] === 'dark') document.documentElement.setAttribute('data-theme', 'dark')
  else if (prefs['flame-theme'] === 'light') document.documentElement.removeAttribute('data-theme')
}

const stamp = () => new Date().toISOString().slice(0, 10)

/** Save everything with default options (Ctrl/⌘+S). */
export async function quickSave(ws: string) {
  const d = await api<SessionFile>('/workspace/export', { workspace: ws })
  const text = JSON.stringify({ ...d, prefs: collectPrefs() })
  saveBlob(new Blob([text], { type: 'application/json' }), `flame-session-${stamp()}.json`)
}

export type Tab = 'status' | 'save' | 'load' | 'clear'

export function SessionModal({ ws, summary, status, initialTab = 'status', onClose }: {
  ws: string; summary?: WorkspaceSummary; status?: SessionStatus; initialTab?: Tab; onClose: () => void
}) {
  const [tab, setTab] = useState<Tab>(initialTab)
  return (
    <Modal title="Session" size="lg" onClose={onClose} palette={false}
      subtitle="Your workspace lives on this server for a while. Save it as a file to keep it, share it or move it to another Flame server.">
      <div className="stack">
        <Segmented label="Session section" value={tab} onChange={setTab}
          options={[{ value: 'status', label: 'Status' }, { value: 'save', label: 'Save' }, { value: 'load', label: 'Load' }, { value: 'clear', label: 'Clear or delete' }]} />
        {tab === 'status' && <StatusPanel ws={ws} status={status} onSave={() => setTab('save')} />}
        {tab === 'save' && <SavePanel ws={ws} runs={summary?.runs ?? []} status={status} />}
        {tab === 'load' && <LoadPanel ws={ws} onDone={onClose} />}
        {tab === 'clear' && <ClearPanel hasEmail={!!summary?.hasEmail} onClose={onClose} />}
      </div>
    </Modal>
  )
}

function StatusPanel({ ws, status, onSave }: { ws: string; status?: SessionStatus; onSave: () => void }) {
  if (!status) return <p className="muted">Loading…</p>
  const st = STATE_LABEL[status.state]
  const c = status.counts
  const url = `${window.location.origin}${import.meta.env.BASE_URL}w/${ws}`
  const daysLeft = Math.max(0, Math.round((status.expires * 1000 - Date.now()) / 86_400_000))
  return (
    <div className="stack">
      <div className={`session-banner ${st.cls}`}>
        <strong>{st.text}</strong>
        <span>{status.lastSaved ? `Last session file: ${new Date(status.lastSaved * 1000).toLocaleString()}.` : 'No session file downloaded yet.'} {st.hint}.</span>
        {status.state !== 'saved' && status.state !== 'empty' && <button className="btn btn-primary btn-sm" onClick={onSave}>Save now</button>}
      </div>
      <dl className="details session-details">
        <dt>Workspace link</dt><dd className="row wrap"><code className="share-url">{url}</code><CopyButton text={url} label="Copy" /></dd>
        <dt>Created</dt><dd>{new Date(status.created * 1000).toLocaleString()}</dd>
        <dt>Last active</dt><dd>{ago(status.lastActive)}</dd>
        <dt>Kept until</dt><dd>{date(status.expires)} ({plural(daysLeft, 'day')} left; the clock restarts whenever you use the workspace)</dd>
        <dt>Gene lists</dt><dd>{c.lists} of {c.maxLists}</dd>
        <dt>Enrichment runs</dt><dd>{c.runsComplete} finished{c.runsActive ? `, ${c.runsActive} running` : ''}{c.runsFailed ? `, ${c.runsFailed} failed` : ''}</dd>
        <dt>Variant analyses</dt><dd>{c.variantAnalyses}{c.variantActive ? ` (${c.variantActive} running)` : ''}</dd>
        <dt>Other analyses</dt><dd>{c.analyses ?? 0}{c.analysesActive ? ` (${c.analysesActive} running)` : ''}</dd>
        <dt>Custom gene sets</dt><dd>{c.sources}</dd>
        <dt>Stored data</dt><dd>about {mb(status.storageBytes)} (session files load up to {mb(status.maxSessionBytes)})</dd>
      </dl>
      <p className="muted small">Running jobs are not saved in session files; save again when they finish.</p>
    </div>
  )
}

function SavePanel({ ws, runs, status }: { ws: string; runs: Run[]; status?: SessionStatus }) {
  const finished = runs.filter((r) => r.status === 'complete')
  const [parts, setParts] = useState<Set<Part>>(new Set(['lists', 'sources', 'runs', 'variants', 'analyses']))
  const [runIds, setRunIds] = useState<Set<string>>(new Set(finished.map((r) => r.id)))
  const [prefs, setPrefs] = useState(true)
  const [compress, setCompress] = useState(typeof CompressionStream !== 'undefined')
  const [name, setName] = useState(`flame-session-${stamp()}`)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [done, setDone] = useState<string | null>(null)
  const toggle = <T,>(set: Set<T>, v: T) => { const n = new Set(set); if (n.has(v)) n.delete(v); else n.add(v); return n }
  const allRuns = runIds.size === finished.length
  const save = async () => {
    setBusy(true); setErr(null); setDone(null)
    try {
      const q = new URLSearchParams({ parts: [...parts].join(',') })
      if (parts.has('runs') && !allRuns) q.set('runIds', [...runIds].join(',') || '__none__')
      const d = await api<SessionFile>(`/workspace/export?${q}`, { workspace: ws })
      const text = JSON.stringify(prefs ? { ...d, prefs: collectPrefs() } : d)
      const base = name.trim().replace(/[^\w.-]+/g, '_').replace(/\.json(\.gz)?$/i, '') || 'flame-session'
      if (compress) saveBlob(await gzip(text), `${base}.json.gz`)
      else saveBlob(new Blob([text], { type: 'application/json' }), `${base}.json`)
      const c = d.contents
      setDone(`Saved ${c ? `${plural(c.lists, 'list')}, ${plural(c.runs, 'run')}, ${plural(c.variants, 'variant analysis', 'variant analyses')} and ${plural(c.sources, 'gene set collection')}` : 'the session'}${d.complete ? '' : ' (partial save: the status still counts your last full save)'}.`)
    } catch (e) { setErr(e) } finally { setBusy(false) }
  }
  return (
    <div className="stack">
      <fieldset className="plain-fieldset">
        <legend>Include</legend>
        {PARTS.map((p) => (
          <label key={p.key} className="check"><input type="checkbox" checked={parts.has(p.key)} onChange={() => setParts(toggle(parts, p.key))} /> {p.label}</label>
        ))}
        <label className="check"><input type="checkbox" checked={prefs} onChange={(e) => setPrefs(e.target.checked)} /> Display settings (theme, network style, visible result tabs)</label>
      </fieldset>
      {parts.has('runs') && finished.length > 0 && (
        <fieldset className="plain-fieldset">
          <legend>Results to include <button type="button" className="btn btn-ghost btn-sm" onClick={() => setRunIds(allRuns ? new Set() : new Set(finished.map((r) => r.id)))}>{allRuns ? 'None' : 'All'}</button></legend>
          <div className="session-run-list">
            {finished.map((r) => (
              <label key={r.id} className="check"><input type="checkbox" checked={runIds.has(r.id)} onChange={() => setRunIds(toggle(runIds, r.id))} />
                {r.title} · {r.listName} <span className="muted small">({formatNum(r.resultCount ?? 0)} terms)</span></label>
            ))}
          </div>
        </fieldset>
      )}
      <div className="row wrap">
        <label className="grow">File name <input className="input" value={name} onChange={(e) => setName(e.target.value)} aria-label="File name" /></label>
        <label className="check" title={typeof CompressionStream === 'undefined' ? 'Not supported by this browser' : 'Usually 5–10× smaller'}>
          <input type="checkbox" checked={compress} disabled={typeof CompressionStream === 'undefined'} onChange={(e) => setCompress(e.target.checked)} /> Compress (.json.gz)
        </label>
      </div>
      <div className="row between wrap">
        <span className="muted small">{status ? `About ${mb(status.storageBytes)} of data before compression.` : ''} Shortcut: Ctrl/⌘+S saves everything.</span>
        <button className="btn btn-primary" disabled={busy || parts.size === 0} onClick={save}>{busy ? 'Preparing…' : 'Download session file'}</button>
      </div>
      {busy && <BusyBar label="Collecting lists and results into a session file" expect={10} />}
      {done && <Note kind="success">{done}</Note>}
      <ErrorNote error={err} />
    </div>
  )
}

function LoadPanel({ ws, onDone }: { ws: string; onDone: () => void }) {
  const invalidate = useInvalidateWorkspace()
  const navigate = useNavigate()
  const undoToast = useUndoToast()
  const [file, setFile] = useState<{ name: string; data: SessionFile; text: string } | null>(null)
  const [parts, setParts] = useState<Set<Part>>(new Set(['lists', 'sources', 'runs', 'variants', 'analyses']))
  const [prefs, setPrefs] = useState(true)
  const [mode, setMode] = useState<'add' | 'replace' | 'new'>('add')
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [drag, setDrag] = useState(false)
  const [done, setDone] = useState<string | null>(null)
  const pick = async (f?: File | null) => {
    if (!f) return
    setErr(null); setFile(null); setDone(null)
    try {
      const text = await readSessionFile(f)
      const data = JSON.parse(text) as SessionFile
      if (data?.format !== 'flame-session') throw new Error('This is not a Flame session file.')
      setFile({ name: f.name, data, text })
      setPrefs(!!data.prefs)
    } catch (e) { setErr(e instanceof SyntaxError ? new Error('The file is not valid JSON.') : e) }
  }
  const counts = file && {
    lists: file.data.lists?.length ?? 0, sources: file.data.sources?.length ?? 0,
    runs: file.data.runs?.length ?? 0, variants: file.data.variants?.length ?? 0, analyses: file.data.analyses?.length ?? 0,
  }
  const load = async () => {
    if (!file) return
    setBusy(true); setErr(null)
    try {
      const q = new URLSearchParams({ mode: mode === 'replace' ? 'replace' : 'add', parts: [...parts].join(',') })
      let target = ws
      let snapshot: unknown = null
      if (mode === 'new') {
        target = (await api<{ id: string }>('/workspace', { method: 'POST', workspace: null })).id
      } else if (mode === 'replace') {
        snapshot = await api<unknown>('/workspace/export', { workspace: ws })
      }
      const body = JSON.parse(file.text)
      const r = await api<{ lists: number; runs: number; sources: number; variants: number; analyses?: number; skipped: string[] }>(`/workspace/import?${q}`, { body, workspace: target })
      if (prefs && file.data.prefs) applyPrefs(file.data.prefs)
      const msg = `Loaded ${plural(r.lists, 'list')}, ${plural(r.runs, 'run')}, ${plural(r.variants, 'variant analysis', 'variant analyses')}, ${plural(r.analyses ?? 0, 'other analysis', 'other analyses')} and ${plural(r.sources, 'gene set collection')}${r.skipped.length ? ` (${r.skipped.length} skipped: ${r.skipped.slice(0, 3).join('; ')})` : ''}.`
      if (mode === 'new') {
        setWorkspaceId(target)
        onDone()
        navigate(`/w/${target}`)
        return
      }
      invalidate()
      if (mode === 'replace' && snapshot) {
        onDone()
        undoToast(msg, () => api(`/workspace/import?mode=replace`, { body: snapshot, workspace: ws }).then(invalidate), undefined, 15000)
      } else { setDone(msg); setFile(null) }
    } catch (e) { setErr(e) } finally { setBusy(false) }
  }
  return (
    <div className="stack">
      <label className={`drop-zone${drag ? ' over' : ''}`}
        onDragOver={(e) => { e.preventDefault(); setDrag(true) }} onDragLeave={() => setDrag(false)}
        onDrop={(e) => { e.preventDefault(); setDrag(false); pick(e.dataTransfer.files?.[0]) }}>
        <input type="file" accept=".json,.gz,application/json,application/gzip" className="sr-only" onChange={(e) => pick(e.target.files?.[0])} />
        <strong>{file ? file.name : 'Choose a session file'}</strong>
        <span className="muted small">or drop it here · .json or .json.gz</span>
      </label>
      {file && counts && (
        <>
          <div className="session-banner idle">
            <strong>{file.data.complete === false ? 'Partial session file' : 'Session file'}</strong>
            <span>{file.data.exported ? `Saved ${new Date(file.data.exported).toLocaleString()}` : 'Saved with an earlier Flame version'}{file.data.flameVersion ? ` · Flame ${file.data.flameVersion}` : ''}</span>
          </div>
          <fieldset className="plain-fieldset">
            <legend>Load</legend>
            {PARTS.map((p) => (
              <label key={p.key} className="check"><input type="checkbox" checked={parts.has(p.key)} disabled={!counts[p.key]}
                onChange={() => { const n = new Set(parts); if (n.has(p.key)) n.delete(p.key); else n.add(p.key); setParts(n) }} />
                {p.label} <span className="muted small">({counts[p.key]})</span></label>
            ))}
            {file.data.prefs && <label className="check"><input type="checkbox" checked={prefs} onChange={(e) => setPrefs(e.target.checked)} /> Display settings saved with the file</label>}
            {!!file.data.lists?.length && <p className="muted small">Lists: {file.data.lists.slice(0, 6).map((l) => l.name).join(', ')}{file.data.lists.length > 6 ? '…' : ''}</p>}
          </fieldset>
          <fieldset className="plain-fieldset">
            <legend>Where</legend>
            <label className="check"><input type="radio" name="load-mode" checked={mode === 'add'} onChange={() => setMode('add')} /> Add to this workspace</label>
            <label className="check"><input type="radio" name="load-mode" checked={mode === 'replace'} onChange={() => setMode('replace')} /> Replace this workspace's contents <span className="muted small">(can be undone for a few seconds)</span></label>
            <label className="check"><input type="radio" name="load-mode" checked={mode === 'new'} onChange={() => setMode('new')} /> Open in a new workspace <span className="muted small">(this one stays as it is)</span></label>
          </fieldset>
          <div className="row between">
            <span className="muted small">Lists over the list limit or with names already in use are renamed or skipped and reported.</span>
            <button className="btn btn-primary" disabled={busy || parts.size === 0} onClick={load}>{busy ? 'Loading…' : 'Load session'}</button>
          </div>
          {busy && <BusyBar label="Restoring lists and results from the session file" expect={15} />}
        </>
      )}
      {done && <Note kind="success">{done}</Note>}
      <ErrorNote error={err} />
    </div>
  )
}

function ClearPanel({ hasEmail, onClose }: { hasEmail: boolean; onClose: () => void }) {
  const { data: config } = useConfig()
  const undoToast = useUndoToast()
  const invalidate = useInvalidateWorkspace()
  const navigate = useNavigate()
  const [err, setErr] = useState<unknown>(null)
  const [msg, setMsg] = useState<string | null>(null)
  const [email, setEmail] = useState('')
  return (
    <div className="stack">
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
        <h3>Clear all data</h3>
        <p className="muted small">Removes every list, run, variant analysis and custom gene set. The workspace link stays valid. You can undo this for a few seconds.</p>
        <button className="btn btn-secondary" onClick={async () => {
          try {
            const snapshot = await api<unknown>('/workspace/export')
            await api('/workspace/data', { method: 'DELETE' })
            invalidate(); onClose()
            undoToast('Cleared all lists, runs, analyses and gene sets.', () => api('/workspace/import', { body: snapshot }).then(invalidate), undefined, 15000)
          } catch (e) { setErr(e) }
        }}>Clear all data</button>
      </section>
      <section>
        <h3>Delete workspace</h3>
        <p className="muted small">Deletes everything and the link itself, permanently. Save a session file first if you may need it again.</p>
        <button className="btn btn-danger" onClick={() => confirm('Permanently delete this workspace and its link? This cannot be undone.') &&
          api('/workspace', { method: 'DELETE' }).then(() => { try { sessionStorage.removeItem('flame-last-workspace') } catch { /* ignore */ } navigate('/') }).catch(setErr)}>Delete workspace</button>
      </section>
      {msg && <Note kind="success">{msg}</Note>}
      <ErrorNote error={err} />
    </div>
  )
}

/** Ctrl/⌘+S saves the whole session from anywhere in the workspace. */
export function useSaveShortcut(ws: string, onSaved: () => void) {
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === 's') {
        e.preventDefault()
        quickSave(ws).then(onSaved).catch(() => { /* reported by the dialog when used */ })
      }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [ws, onSaved])
}
