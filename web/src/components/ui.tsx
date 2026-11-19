import { createContext, useContext, useEffect, useId, useRef, useState, type ReactNode } from 'react'
import { api } from '../api/client'
import { useConfig, useOrganismSearch } from '../api/hooks'
import type { OrganismOption } from '../api/types'
import { PaletteSelect } from './PaletteSelect'
import { geneCardsUrl } from '../lib/links'
import { saveXlsx, type Sheet } from '../lib/xlsx'
import { Link } from 'react-router-dom'

/**
 * Progress bar for a request that takes a while. The server does not report how
 * far along it is, so the bar fills steadily towards (never reaching) the end
 * and the elapsed time is shown instead of a percentage.
 */
export function BusyBar({ label, hint, expect = 15 }: { label: string; hint?: ReactNode; expect?: number }) {
  const [elapsed, setElapsed] = useState(0)
  useEffect(() => {
    const start = Date.now()
    const id = window.setInterval(() => setElapsed((Date.now() - start) / 1000), 250)
    return () => window.clearInterval(id)
  }, [])
  const shown = 0.04 + 0.92 * (1 - Math.exp(-elapsed / expect))
  const secs = Math.floor(elapsed)
  return (
    <div className="job-progress busy-bar" role="status" aria-live="polite">
      <div className="job-progress-bar" aria-hidden="true"><span style={{ width: `${Math.round(shown * 100)}%` }} /></div>
      <div className="job-progress-text">
        <span className="job-progress-msg">{label}…</span>
        <span className="job-progress-meta">{secs < 60 ? `${secs} s` : `${Math.floor(secs / 60)}:${String(secs % 60).padStart(2, '0')} min`}</span>
      </div>
      {hint && elapsed > 4 && <span className="muted small">{hint}</span>}
    </div>
  )
}

/** A named wait shows a progress bar with the elapsed time; an unnamed one a small spinner. */
export function Spinner({ label }: { label?: string }) {
  if (label) return <BusyBar label={label.replace(/[….]+$/, '')} />
  label = 'Loading'
  return (
    <span className="spinner" role="status" aria-live="polite">
      <span className="spinner-dot" aria-hidden="true" />
      {label}
    </span>
  )
}

export function ErrorNote({ error }: { error: unknown }) {
  if (!error) return null
  const msg = error instanceof Error ? error.message : String(error)
  return (
    <div className="note note-error" role="alert">
      {msg}
    </div>
  )
}

export function Note({ kind = 'info', children }: { kind?: 'info' | 'success' | 'warning'; children: ReactNode }) {
  return <div className={`note note-${kind}`}>{children}</div>
}

/** Dialogs show the colour-palette selector in their header unless a parent turns it off (the input dialogs do). */
export const ModalPaletteContext = createContext(true)

export function Modal({
  title,
  subtitle,
  onClose,
  children,
  footer,
  size = 'md',
  palette,
}: {
  title: string
  subtitle?: ReactNode
  onClose: () => void
  children: ReactNode
  footer?: ReactNode
  size?: 'md' | 'lg' | 'xl'
  /** Show the colour-palette selector in the header. Set it only for dialogs that draw a chart or network; unset follows the surrounding context. */
  palette?: boolean
}) {
  const ref = useRef<HTMLDivElement>(null)
  const titleId = useId()
  const inherited = useContext(ModalPaletteContext)
  const showPalette = palette ?? inherited
  useEffect(() => {
    const prev = document.activeElement as HTMLElement | null
    ref.current?.focus()
    // Only the top-most dialog reacts to Escape (dialogs can open on top of each other).
    const onKey = (e: KeyboardEvent) => {
      const all = document.querySelectorAll('.modal')
      if (e.key === 'Escape' && all[all.length - 1] === ref.current) onClose()
    }
    document.addEventListener('keydown', onKey)
    document.body.classList.add('modal-open')
    return () => {
      document.removeEventListener('keydown', onKey)
      if (!document.querySelector('.modal-backdrop')) document.body.classList.remove('modal-open')
      prev?.focus?.()
    }
  }, [onClose])
  return (
    <div className="modal-backdrop" onMouseDown={(e) => e.target === e.currentTarget && onClose()}>
      <div className={`modal modal-${size}`} role="dialog" aria-modal="true" aria-labelledby={titleId} tabIndex={-1} ref={ref}>
        <header className="modal-header">
          <div>
            <h2 id={titleId}>{title}</h2>
            {subtitle && <p className="muted">{subtitle}</p>}
          </div>
          <span className="row modal-actions">
            {showPalette && <PaletteSelect />}
            <button className="icon-btn" onClick={onClose} aria-label="Close">
              ×
            </button>
          </span>
        </header>
        <div className="modal-body">{children}</div>
        {footer && <footer className="modal-footer">{footer}</footer>}
      </div>
    </div>
  )
}

export function Field({ label, hint, children }: { label: string; hint?: ReactNode; children: ReactNode }) {
  return (
    <label className="field">
      <span className="field-label">{label}</span>
      {children}
      {hint && <span className="field-hint">{hint}</span>}
    </label>
  )
}

export function Segmented<T extends string>({
  value,
  options,
  onChange,
  label,
}: {
  value: T
  options: { value: T; label: string; disabled?: boolean }[]
  onChange: (v: T) => void
  label: string
}) {
  return (
    <div className="segmented" role="radiogroup" aria-label={label}>
      {options.map((o) => (
        <button
          key={o.value}
          type="button"
          role="radio"
          aria-checked={value === o.value}
          disabled={o.disabled}
          className={value === o.value ? 'active' : ''}
          onClick={() => onChange(o.value)}
        >
          {o.label}
        </button>
      ))}
    </div>
  )
}

/** Organism selector: common organisms + server-side search across all organisms. */
export function OrganismPicker({
  value,
  onChange,
  tool,
  label = 'Organism',
}: {
  value: number
  onChange: (taxid: number, label: string) => void
  tool?: string
  label?: string
}) {
  const { data: config } = useConfig()
  const [query, setQuery] = useState('')
  const [open, setOpen] = useState(false)
  const search = useOrganismSearch(query, tool)
  const common = config?.commonOrganisms ?? []
  const options: OrganismOption[] = query.length >= 2 ? search.data ?? [] : common
  const current = [...common, ...(search.data ?? [])].find((o) => Number(o.value) === value)
  const [selectedLabel, setSelectedLabel] = useState<string>('')
  const shown = current?.label ?? selectedLabel ?? String(value)
  return (
    <div className="field">
      <span className="field-label">{label}</span>
      <div className="combo">
        <button type="button" className="combo-button" onClick={() => setOpen((o) => !o)} aria-expanded={open}>
          {shown || `NCBI taxonomy ${value}`}
          <span aria-hidden="true">▾</span>
        </button>
        {open && (
          <div className="combo-panel">
            <input
              autoFocus
              className="input"
              placeholder="Search 12,800+ organisms by name or taxonomy ID"
              value={query}
              onChange={(e) => setQuery(e.target.value)}
              aria-label="Search organisms"
            />
            <ul className="combo-list" role="listbox">
              {search.isFetching && <li className="muted">Searching…</li>}
              {options.map((o) => (
                <li key={o.value}>
                  <button
                    type="button"
                    role="option"
                    aria-selected={Number(o.value) === value}
                    onClick={() => {
                      setSelectedLabel(o.label)
                      onChange(Number(o.value), o.label)
                      setOpen(false)
                      setQuery('')
                    }}
                  >
                    {o.label}
                  </button>
                </li>
              ))}
              {query.length >= 2 && !search.isFetching && options.length === 0 && <li className="muted">No organisms found.</li>}
            </ul>
          </div>
        )}
      </div>
    </div>
  )
}

export function IdFormatSelect({ value, onChange, label = 'ID format' }: { value: string; onChange: (v: string) => void; label?: string }) {
  const { data: config } = useConfig()
  return (
    <Field label={label}>
      <select className="input" value={value} onChange={(e) => onChange(e.target.value)}>
        {(config?.idFormats ?? []).map((f) => (
          <option key={f.value} value={f.value}>
            {f.label}
            {f.example ? ` (e.g. ${f.example})` : ''}
          </option>
        ))}
      </select>
    </Field>
  )
}

/** Read a text file in the browser (size-checked before reading). */
export function FileDrop({ accept, onText, maxBytes = 1024 * 1024 }: { accept: string; onText: (text: string, name: string) => void; maxBytes?: number }) {
  const [error, setError] = useState<string | null>(null)
  const [drag, setDrag] = useState(false)
  const read = (file: File | undefined) => {
    if (!file) return
    if (file.size > maxBytes) {
      setError(`The file is larger than ${Math.round(maxBytes / 1024)} KB.`)
      return
    }
    setError(null)
    file.text().then((t) => onText(t, file.name))
  }
  return (
    <div
      className={`dropzone${drag ? ' drag' : ''}`}
      onDragOver={(e) => {
        e.preventDefault()
        setDrag(true)
      }}
      onDragLeave={() => setDrag(false)}
      onDrop={(e) => {
        e.preventDefault()
        setDrag(false)
        read(e.dataTransfer.files[0])
      }}
    >
      <span>Drop a file here or </span>
      <label className="link-btn">
        browse
        <input type="file" accept={accept} hidden onChange={(e) => read(e.target.files?.[0])} />
      </label>
      <span className="muted"> ({accept}, up to {Math.round(maxBytes / 1024)} KB)</span>
      {error && <div className="note note-error">{error}</div>}
    </div>
  )
}

export function CopyButton({ text, label = 'Copy', title, compact = false }: { text: string; label?: string; title?: string; compact?: boolean }) {
  const [done, setDone] = useState(false)
  return (
    <button
      type="button"
      title={title ?? label}
      aria-label={title ?? label}
      className={compact ? 'icon-btn copy-mini' : 'btn btn-ghost btn-sm'}
      onClick={() =>
        navigator.clipboard.writeText(text).then(() => {
          setDone(true)
          setTimeout(() => setDone(false), 1500)
        })
      }
    >
      {compact ? (done ? '✓' : '⧉') : done ? 'Copied' : label}
    </button>
  )
}

export function EmptyState({ title, children }: { title: string; children?: ReactNode }) {
  return (
    <div className="empty">
      <h3>{title}</h3>
      {children && <div className="muted">{children}</div>}
    </div>
  )
}

/**
 * Sends the current data to an external viewer (Arena3D, NORMA) through the API
 * and opens the returned link in a new tab. Only http(s) links are opened.
 */
export function OpenExternalButton({ path, body, label, title, className = 'btn btn-secondary btn-sm', icon, caption }: {
  path: string; body: unknown; label: string; title: string; className?: string
  /** Optional picture of what the viewer does, with a short caption under the name. */
  icon?: ReactNode; caption?: string
}) {
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  return (
    <>
      <button type="button" className={className} disabled={busy} title={title} aria-busy={busy}
        onClick={() => {
          setBusy(true); setErr(null)
          // Open the tab synchronously (popup blockers), then point it at the link.
          const win = window.open('', '_blank')
          api<{ url: string }>(path, { body })
            .then((r) => {
              if (!/^https?:\/\//i.test(r.url)) throw new Error('The viewer returned an unexpected link.')
              if (win) { win.opener = null; win.location.href = r.url } else window.open(r.url, '_blank', 'noopener')
            })
            .catch((e) => { win?.close(); setErr(e) })
            .finally(() => setBusy(false))
        }}>
        {icon ? (
          <span className="ext-btn">
            {icon}
            <span className="ext-btn-text"><strong>{busy ? 'Sending…' : label}</strong>{caption && <span>{caption}</span>}</span>
            <span aria-hidden="true" className="ext-btn-arrow">↗</span>
          </span>
        ) : busy ? 'Sending…' : label}
      </button>
      {err ? <ErrorNote error={err} /> : null}
    </>
  )
}

/** Small "open in…" menu of external links, grouped. */
export function LinkMenu({ links, label, title = 'Open in genome browsers and databases' }: {
  links: { label: string; url: string; group?: string }[]; label: ReactNode; title?: string
}) {
  if (!links.length) return <>{label}</>
  const groups = [...new Set(links.map((l) => l.group ?? ''))]
  return (
    <details className="link-menu">
      <summary title={title}>{label}<span aria-hidden="true" className="link-menu-icon">↗</span></summary>
      <div className="link-menu-panel" role="menu">
        {groups.map((g) => (
          <div key={g}>
            {g && <div className="link-menu-group">{g}</div>}
            {links.filter((l) => (l.group ?? '') === g).map((l) => (
              <a key={l.url} role="menuitem" href={l.url} target="_blank" rel="noopener noreferrer">{l.label}</a>
            ))}
          </div>
        ))}
      </div>
    </details>
  )
}

/** A gene name that opens its GeneCards page (human genes; plain text otherwise). */
export function GeneCardsLink({ gene, taxid = 9606, className }: { gene: string; taxid?: number; className?: string }) {
  const url = geneCardsUrl(gene, taxid)
  if (!url) return <span className={className}>{gene}</span>
  return <a className={`genecards-link${className ? ` ${className}` : ''}`} href={url} target="_blank" rel="noopener noreferrer" title={`${gene} in GeneCards`}>{gene}</a>
}

/** Genes as small chips, each linking to GeneCards (human), with "+N more". */
export function GeneChips({ genes, taxid = 9606, max = 60 }: { genes: string[]; taxid?: number; max?: number }) {
  return (
    <span className="gene-chips">
      {genes.slice(0, max).map((g) => <GeneCardsLink key={g} gene={g} taxid={taxid} className="gene" />)}
      {genes.length > max && <span className="muted small">+{genes.length - max} more (copy to get all)</span>}
    </span>
  )
}

/** The way back from a results page: a clear button with the workspace as destination. */
export function BackToWorkspace({ ws }: { ws: string }) {
  return (
    <Link to={`/w/${ws}`} className="btn btn-secondary back-to-workspace" title="Back to your lists, runs and analyses">
      <svg width="16" height="16" viewBox="0 0 24 24" aria-hidden="true"><path d="M15 5l-7 7 7 7" fill="none" stroke="currentColor" strokeWidth="2.2" strokeLinecap="round" strokeLinejoin="round" /></svg>
      Back to workspace
    </Link>
  )
}

/** Excel export button: `sheets` is called when clicked. */
export function ExcelButton({ sheets, filename, label = 'Excel', className = 'btn btn-ghost btn-sm', title }: {
  sheets: () => Sheet[]; filename: string; label?: string; className?: string; title?: string
}) {
  return <button type="button" className={className} title={title ?? 'Download as an Excel workbook'} onClick={() => saveXlsx(sheets(), filename)}>{label}</button>
}

/** A term's genes: the first few, with "+N more" to expand the full list and "less" to collapse it. */
export function GeneList({ genes, max = 8 }: { genes: string[] | string; max?: number }) {
  const [open, setOpen] = useState(false)
  const list = Array.isArray(genes) ? genes : genes.split(/[,;]\s*/).map((g) => g.trim()).filter(Boolean)
  if (!list.length) return <span className="muted">—</span>
  const shown = open ? list : list.slice(0, max)
  return (
    <span className={`gene-list${open ? ' open' : ''}`}>
      <span className="gene-list-names">{shown.join(', ')}</span>
      {list.length > max && (
        <button type="button" className="gene-list-toggle" aria-expanded={open}
          onClick={(e) => { e.stopPropagation(); setOpen(!open) }}>
          {open ? 'show less' : `+${list.length - max} more`}
        </button>
      )}
    </span>
  )
}
