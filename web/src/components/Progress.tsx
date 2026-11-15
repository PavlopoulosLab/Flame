import { useEffect, useRef, useState } from 'react'
import { Link } from 'react-router-dom'
import type { Run } from '../api/types'
import { ToolLogo } from './ToolLogo'

export interface JobProgress { fraction: number; message: string; elapsed: number }

export const formatElapsed = (s: number) => {
  const t = Math.max(0, Math.round(s))
  return t < 60 ? `${t} s` : `${Math.floor(t / 60)}:${String(t % 60).padStart(2, '0')} min`
}

/**
 * Progress of a server job. The server reports steps (fraction + message);
 * between reports the bar keeps creeping slowly towards the next step so it
 * never looks frozen, but it never goes backwards or reaches 100 % early.
 */
export function ProgressBar({ progress, status, compact = false }: { progress?: JobProgress | null; status?: string; compact?: boolean }) {
  const reported = status === 'complete' ? 1 : progress?.fraction ?? (status === 'queued' ? 0.02 : 0.05)
  const [shown, setShown] = useState(reported)
  const since = useRef({ value: reported, at: Date.now() })
  if (since.current.value !== reported) since.current = { value: reported, at: Date.now() }
  const [elapsed, setElapsed] = useState(progress?.elapsed ?? 0)
  useEffect(() => { if (progress) setElapsed(progress.elapsed) }, [progress?.elapsed])
  useEffect(() => {
    if (status === 'complete' || status === 'error') { setShown(status === 'complete' ? 1 : shown); return }
    const id = window.setInterval(() => {
      const dt = (Date.now() - since.current.at) / 1000
      // Ease toward 95 % while a step runs: fast at first, slower later, never reaching it early.
      const target = reported + (0.95 - reported) * (1 - Math.exp(-dt / 30)) * 0.9
      setShown((cur) => Math.max(cur, cur + (target - cur) * 0.15))
      setElapsed((e) => e + 0.25)
    }, 250)
    return () => window.clearInterval(id)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [reported, status])
  const pct = Math.round(shown * 100)
  const message = status === 'queued' && !progress ? 'Waiting for a free worker' : progress?.message ?? (status === 'complete' ? 'Done' : 'Starting')
  return (
    <div className={`job-progress${compact ? ' compact' : ''}${status === 'error' ? ' failed' : ''}`}>
      <div className="job-progress-bar" role="progressbar" aria-valuemin={0} aria-valuemax={100} aria-valuenow={pct} aria-label={message}>
        <span style={{ width: `${pct}%` }} />
      </div>
      <div className="job-progress-text">
        <span className="job-progress-msg">{message}</span>
        <span className="job-progress-meta">{pct}% · {formatElapsed(elapsed)}</span>
      </div>
    </div>
  )
}

/**
 * Floating window that lists running analyses with their progress. It opens
 * when jobs start, can be minimised, and stays briefly after they finish so the
 * user sees the outcome.
 */
export function JobsWindow({ runs, ws }: { runs: Run[]; ws: string }) {
  const active = runs.filter((r) => r.status === 'queued' || r.status === 'running')
  const [watch, setWatch] = useState<Set<string>>(new Set())
  const [minimised, setMinimised] = useState(false)
  const [closed, setClosed] = useState(false)
  // Keep every job that ran while the window was open, so finished ones show their outcome.
  useEffect(() => {
    if (active.length) {
      setWatch((w) => { const n = new Set(w); active.forEach((r) => n.add(r.id)); return n })
      setClosed(false)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [active.map((r) => r.id).join()])
  const shown = runs.filter((r) => watch.has(r.id))
  const allDone = shown.length > 0 && active.length === 0
  // When everything has finished, shrink to the one-line summary (the results are on the cards)
  // and leave after a few seconds, so the window never covers the run cards for long.
  useEffect(() => {
    if (!allDone) return
    setMinimised(true)
    const t = window.setTimeout(() => { setWatch(new Set()); setMinimised(false) }, 8000)
    return () => window.clearTimeout(t)
  }, [allDone])
  if (closed || !shown.length) return null
  const done = shown.filter((r) => r.status === 'complete').length
  const failed = shown.filter((r) => r.status === 'error').length
  const overall = shown.reduce((acc, r) => acc + (r.status === 'complete' || r.status === 'error' ? 1 : r.progress?.fraction ?? 0.03), 0) / shown.length
  return (
    <aside className={`jobs-window${minimised ? ' min' : ''}`} role="status" aria-live="polite" aria-label="Running analyses">
      <header>
        <strong>{allDone ? (failed ? `${done} finished, ${failed} failed` : `All ${shown.length} analyses finished`) : `Running ${active.length} of ${shown.length} analyses`}</strong>
        <span className="row">
          <button type="button" className="icon-btn" aria-label={minimised ? 'Expand' : 'Minimise'} onClick={() => setMinimised(!minimised)}>{minimised ? '▴' : '▾'}</button>
          <button type="button" className="icon-btn" aria-label="Close" onClick={() => { setClosed(true); if (allDone) setWatch(new Set()) }}>×</button>
        </span>
      </header>
      <div className="jobs-overall"><span style={{ width: `${Math.round(overall * 100)}%` }} /></div>
      {!minimised && (
        <ul>
          {shown.map((r) => (
            <li key={r.id}>
              <div className="row between">
                <span className="jobs-title"><ToolLogo tool={r.toolKey} size={18} /> {r.title} <span className="muted small">· {r.listName}</span></span>
                {r.status === 'complete' && <Link className="small" to={`/w/${ws}/runs/${r.id}`}>{r.resultCount ?? 0} terms →</Link>}
                {r.status === 'error' && <span className="small danger">failed</span>}
              </div>
              {r.status !== 'complete' && r.status !== 'error' && <ProgressBar progress={r.progress} status={r.status} compact />}
              {r.status === 'error' && <p className="small muted">{r.error}</p>}
            </li>
          ))}
        </ul>
      )}
    </aside>
  )
}
