import { useEffect, useRef, useState } from 'react'
import type { ChartColours, ColourRole } from '../lib/chartColors'
import { ColorPicker } from './ChartTextControls'
import { usePopoverPlacement } from './usePopoverPlacement'

/** One-click colours for chart series: Flame's, the Okabe–Ito colour-blind safe set, greys and black. */
const SERIES_PRESETS = ['#dc2626', '#2563eb', '#16a34a', '#f97316', '#8b5cf6', '#0891b2', '#db2777', '#eab308', '#D55E00', '#0072B2', '#009E73', '#a1a1aa', '#525252']

/**
 * "Colours" next to a chart's Aa Text button: a colour picker (presets, any colour, R G B and hex) for each
 * colour of the chart — each series (up / down / not significant, each source…), each colour painted point by
 * point, and the low / middle / high of colour scales. Kept for this chart in this browser.
 */
export function ChartColorButton({ roles, colours, setColours }: { roles: ColourRole[]; colours: ChartColours; setColours: (c: ChartColours) => void }) {
  const [open, setOpen] = useState(false)
  const ref = useRef<HTMLDivElement>(null)
  const popRef = useRef<HTMLDivElement>(null)
  usePopoverPlacement(ref, popRef, open)
  useEffect(() => {
    if (!open) return
    const close = (e: MouseEvent) => { if (ref.current && !ref.current.contains(e.target as Node)) setOpen(false) }
    const esc = (e: KeyboardEvent) => { if (e.key === 'Escape') setOpen(false) }
    document.addEventListener('mousedown', close); document.addEventListener('keydown', esc)
    return () => { document.removeEventListener('mousedown', close); document.removeEventListener('keydown', esc) }
  }, [open])
  if (!roles.length) return null
  const set = (key: string, c: string) => {
    const next = { ...colours }
    if (c) next[key] = c; else delete next[key]
    setColours(next)
  }
  const changed = roles.some((r) => colours[r.key])
  // Series and point colours one per row; the stops of a colour scale together, with a preview.
  const rows: (ColourRole | ColourRole[])[] = []
  roles.forEach((r) => {
    const last = rows[rows.length - 1]
    if (r.group && Array.isArray(last) && last[0].group === r.group) last.push(r)
    else rows.push(r.group ? [r] : r)
  })
  const picker = (r: ColourRole, label: string) => (
    <ColorPicker label={label} value={colours[r.key] ?? ''} fallback={r.color} defaultName="Default" presets={SERIES_PRESETS}
      onChange={(c) => set(r.key, c)} />
  )
  return (
    <div className="chart-text-button" ref={ref}>
      <button type="button" className="btn btn-ghost btn-sm" data-no-export="" aria-expanded={open} title="Colours of this chart: each series, and the ends of colour scales"
        onClick={() => setOpen((v) => !v)}>
        <span className="chart-colors-icon" aria-hidden="true" style={{ background: `conic-gradient(${roles.slice(0, 4).map((r) => colours[r.key] || r.color).join(', ')}, ${colours[roles[0].key] || roles[0].color})` }} />
        Colours
      </button>
      {open && (
        <div ref={popRef} className="chart-text-pop chart-colors-pop card stack-sm" role="dialog" aria-label="Chart colours">
          <div className="chart-colors-list">
            {rows.map((row) => {
              if (!Array.isArray(row)) return (
                <div key={row.key} className="chart-colors-row">
                  {picker(row, row.label)}
                  <span className="chart-colors-label">{row.label}{row.hint && <span className="muted small"> · {row.hint}</span>}</span>
                  {colours[row.key] && <code className="small muted">{colours[row.key]}</code>}
                </div>
              )
              const name = row[0].label.replace(/Low$/, '').replace(/ · $/, '')
              return (
                <div key={row[0].group} className="chart-colors-row">
                  <span className="chart-colors-scale">
                    {row.map((r) => <span key={r.key} className="chart-colors-stop">{picker(r, r.label)}<span className="small muted">{r.label.split(' · ').pop()}</span></span>)}
                  </span>
                  <span className="chart-colors-label">{name ? `${name} colour scale` : 'Colour scale'}
                    <span className="scale-preview" aria-hidden="true" style={{ background: `linear-gradient(90deg, ${row.map((r) => colours[r.key] || r.color).join(', ')})` }} />
                  </span>
                </div>
              )
            })}
          </div>
          <div className="row between">
            <span className="muted small">Kept for this chart in this browser; the image export uses them.</span>
            <button type="button" className="btn btn-ghost btn-sm" disabled={!changed} onClick={() => setColours({})}>Reset colours</button>
          </div>
        </div>
      )}
    </div>
  )
}
