import { useEffect, useRef, useState, type ReactNode } from 'react'
import { DEFAULT_CHART_TEXT, FONTS, TEXT_PARTS, TEXT_SIZES, TICK_ANGLES, setChartText, useChartText, type TextPart, type TextStyle } from '../lib/chartText'
import { Field } from './ui'

/** Bold, italic and size of one part of the chart text. */
export function TextStyleButtons({ part, label }: { part: TextPart; label: string }) {
  const t = useChartText()
  const s = t[part]
  const up = (v: Partial<TextStyle>) => setChartText((x) => ({ ...x, [part]: { ...x[part], ...v } }))
  return (
    <>
      <button type="button" className={`btn btn-sm ${s.bold ? 'btn-primary' : 'btn-secondary'}`} aria-pressed={s.bold} aria-label={`${label}: bold`} title="Bold"
        style={{ fontWeight: 700, minWidth: 30 }} onClick={() => up({ bold: !s.bold })}>B</button>
      <button type="button" className={`btn btn-sm ${s.italic ? 'btn-primary' : 'btn-secondary'}`} aria-pressed={s.italic} aria-label={`${label}: italic`} title="Italic"
        style={{ fontStyle: 'italic', minWidth: 30, fontFamily: 'Georgia, serif' }} onClick={() => up({ italic: !s.italic })}>I</button>
      <select className="input input-sm" aria-label={`${label}: size`} title="Text size" value={s.size} onChange={(e) => up({ size: Number(e.target.value) })} style={{ width: 'auto' }}>
        <option value={0}>Auto</option>
        {TEXT_SIZES.map((n) => <option key={n} value={n}>{n} pt</option>)}
      </select>
    </>
  )
}

export function FontSelect() {
  const t = useChartText()
  return (
    <Field label="Font">
      <select className="input input-sm" aria-label="Font" value={t.family} onChange={(e) => setChartText((x) => ({ ...x, family: e.target.value }))} style={{ width: 'auto' }}>
        {FONTS.map(([v, l]) => <option key={l} value={v} style={v ? { fontFamily: v } : undefined}>{l}</option>)}
      </select>
    </Field>
  )
}

export function TickAngleSelect() {
  const t = useChartText()
  return (
    <select className="input input-sm" aria-label="X tick label angle" title="Angle of the labels under the x axis" value={t.tickAngle}
      onChange={(e) => setChartText((x) => ({ ...x, tickAngle: Number(e.target.value) }))} style={{ width: 'auto' }}>
      {TICK_ANGLES.map(([a, l]) => <option key={a} value={a}>{l}</option>)}
    </select>
  )
}

/**
 * The text style of all charts: one row per part (an optional text box first, e.g. the title of the figure),
 * then the font. Changes apply to every chart in Flame.
 */
export function ChartTextControls({ inputs = {}, parts = TEXT_PARTS.map((p) => p.key), extra }: {
  /** A text box shown before a part's style buttons (e.g. the plot title being written). */
  inputs?: Partial<Record<TextPart, ReactNode>>
  parts?: TextPart[]
  /** More controls after the font (e.g. a text size or the legend position). */
  extra?: ReactNode
}) {
  return (
    <div className="row wrap chart-text" role="group" aria-label="Text of the charts">
      {TEXT_PARTS.filter((p) => parts.includes(p.key)).map((p) => (
        <Field key={p.key} label={p.label}>
          <span className="row" style={{ gap: 4 }}>
            {inputs[p.key]}
            <TextStyleButtons part={p.key} label={p.label} />
            {p.key === 'tick' && <TickAngleSelect />}
          </span>
        </Field>
      ))}
      <FontSelect />
      {extra}
    </div>
  )
}

/** "Aa Text" next to a chart's Image button: the text style of all charts, in a small panel. */
export function ChartTextButton() {
  const [open, setOpen] = useState(false)
  const ref = useRef<HTMLDivElement>(null)
  useEffect(() => {
    if (!open) return
    const close = (e: MouseEvent) => { if (ref.current && !ref.current.contains(e.target as Node)) setOpen(false) }
    const esc = (e: KeyboardEvent) => { if (e.key === 'Escape') setOpen(false) }
    document.addEventListener('mousedown', close); document.addEventListener('keydown', esc)
    return () => { document.removeEventListener('mousedown', close); document.removeEventListener('keydown', esc) }
  }, [open])
  return (
    <div className="chart-text-button" ref={ref}>
      <button type="button" className="btn btn-ghost btn-sm" data-no-export="" aria-expanded={open} title="Font, size, bold and italic of titles, axes and legends (all charts)" onClick={() => setOpen((v) => !v)}>Aa Text</button>
      {open && (
        <div className="chart-text-pop card stack-sm" role="dialog" aria-label="Text of all charts">
          <strong className="small">Text of all charts</strong>
          <ChartTextControls />
          <div className="row">
            <span className="muted small">Applies to every chart in Flame and is remembered in this browser. Auto keeps each chart's own size.</span>
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => setChartText(DEFAULT_CHART_TEXT)}>Reset</button>
          </div>
        </div>
      )}
    </div>
  )
}
