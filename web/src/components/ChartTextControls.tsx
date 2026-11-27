import { useEffect, useRef, useState, type PointerEvent as ReactPointerEvent, type ReactNode } from 'react'
import { usePopoverPlacement } from './usePopoverPlacement'
import { DEFAULT_CHART_TEXT, FONTS, NO_LABELS, TEXT_PARTS, TEXT_SIZES, TICK_ANGLES, setChartText, useChartText, type ChartLabels, type TextPart, type TextStyle, hex2, hexToHsv, hsvToHex, parseColor, toRgb } from '../lib/chartText'
import { Field } from './ui'

/** The grey charts use for text (shown in the colour picker until a colour is chosen). */
const DEFAULT_TEXT_COLOR = '#444444'

const PRESETS = ['#444444', '#000000', '#7f7f7f', '#c0392b', '#e67e22', '#d4a017', '#27ae60', '#16a085', '#2563eb', '#1f3a93', '#8e44ad', '#c2185b']
/**
 * Colour of one part of the text: preset colours, R G B and hex boxes, and the default grey, in a small panel
 * inside the page (the system colour window would close the Aa Text panel and lose the choice).
 */
/** Any colour: a saturation × brightness area (click or drag) and a hue slider, inside the page. */
function SpectrumPicker({ label, color, onChange }: { label: string; color: string; onChange: (c: string) => void }) {
  // The hue is kept here: greys and black have none, so it would jump back to red as the colour changes.
  const [hsv, setHsv] = useState<[number, number, number]>(() => hexToHsv(color))
  const area = useRef<HTMLDivElement>(null)
  const set = (h: number, s: number, v: number) => { setHsv([h, s, v]); onChange(hsvToHex(h, s, v)) }
  const at = (e: ReactPointerEvent) => {
    const r = area.current!.getBoundingClientRect()
    set(hsv[0], Math.min(1, Math.max(0, (e.clientX - r.left) / r.width)), 1 - Math.min(1, Math.max(0, (e.clientY - r.top) / r.height)))
  }
  return (
    <span className="spectrum">
      <div ref={area} className="spectrum-area" role="slider" tabIndex={0} aria-label={`${label}: saturation and brightness`} aria-valuetext={hsvToHex(...hsv)}
        style={{ background: `linear-gradient(to top, #000, transparent), linear-gradient(to right, #fff, transparent), hsl(${hsv[0]}, 100%, 50%)` }}
        onPointerDown={(e) => { e.currentTarget.setPointerCapture(e.pointerId); at(e) }} onPointerMove={(e) => { if (e.buttons) at(e) }}
        onKeyDown={(e) => {
          const d = e.shiftKey ? 0.1 : 0.02, [h, sat, v] = hsv
          if (e.key === 'ArrowRight') set(h, Math.min(1, sat + d), v); else if (e.key === 'ArrowLeft') set(h, Math.max(0, sat - d), v)
          else if (e.key === 'ArrowUp') set(h, sat, Math.min(1, v + d)); else if (e.key === 'ArrowDown') set(h, sat, Math.max(0, v - d)); else return
          e.preventDefault()
        }}>
        <span className="spectrum-dot" style={{ left: `${hsv[1] * 100}%`, top: `${(1 - hsv[2]) * 100}%`, background: hsvToHex(...hsv) }} />
      </div>
      <input type="range" className="spectrum-hue" min={0} max={359} value={Math.round(hsv[0])} aria-label={`${label}: hue`}
        onChange={(e) => set(Number(e.target.value), hsv[1] || 1, hsv[2] || 1)} />
    </span>
  )
}

export function ColorPicker({ label, value, onChange, fallback = DEFAULT_TEXT_COLOR, defaultName = 'Default grey', presets = PRESETS }: {
  label: string; value: string; onChange: (c: string) => void
  /** The colour used while none is chosen (value ''), and the name of that default. */ fallback?: string; defaultName?: string
  /** Colours offered as one-click swatches. */ presets?: string[]
}) {
  const [open, setOpen] = useState(false)
  const [spectrum, setSpectrum] = useState(false)
  const [hexText, setHexText] = useState('')
  const ref = useRef<HTMLSpanElement>(null)
  const popRef = useRef<HTMLSpanElement>(null)
  usePopoverPlacement(ref, popRef, open, 'left')
  const shown = value || fallback
  const rgb = toRgb(shown)
  useEffect(() => {
    if (!open) return
    const close = (e: MouseEvent) => { if (ref.current && !ref.current.contains(e.target as Node)) setOpen(false) }
    document.addEventListener('mousedown', close)
    return () => document.removeEventListener('mousedown', close)
  }, [open])
  const pick = (c: string) => { onChange(c === fallback ? '' : c); setHexText('') }
  return (
    <span className="text-color-wrap" ref={ref}>
      <button type="button" className="text-color" aria-label={`${label}: colour`} aria-expanded={open} title={value ? `Colour ${value}` : `Colour (${defaultName.toLowerCase()})`}
        style={{ background: shown }} onClick={() => setOpen((v) => !v)} />
      {open && (
        <span ref={popRef} className="text-color-pop card" role="dialog" aria-label={`${label} colour`}>
          <span className="text-color-presets">
            {presets.map((c) => <button key={c} type="button" className={`text-color-swatch${shown === c ? ' on' : ''}`} style={{ background: c }} aria-label={c === fallback ? defaultName : c}
              title={c === fallback ? defaultName : c} onClick={() => pick(c)} />)}
            <button type="button" className={`text-color-swatch text-color-multi${spectrum ? ' on' : ''}`} aria-label="More colours" aria-pressed={spectrum} title="Any colour"
              onClick={() => setSpectrum((v) => !v)} />
          </span>
          {spectrum && <SpectrumPicker key={label} label={label} color={shown} onChange={(c) => { onChange(c); setHexText('') }} />}
          <span className="row" style={{ gap: 4 }}>
            {(['R', 'G', 'B'] as const).map((ch, i) => (
              <label key={ch} className="small text-color-ch">{ch}
                <input className="input input-sm" type="number" min={0} max={255} aria-label={`${label}: ${ch}`} value={rgb[i]}
                  onChange={(e) => { const v = [...rgb]; v[i] = Number(e.target.value); onChange(`#${v.map(hex2).join('')}`) }} />
              </label>
            ))}
          </span>
          <span className="row" style={{ gap: 4 }}>
            <input className="input input-sm" aria-label={`${label}: hex or rgb`} placeholder={shown} value={hexText} style={{ width: 130 }}
              onChange={(e) => { setHexText(e.target.value); const c = parseColor(e.target.value); if (c) onChange(c) }} />
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => { onChange(''); setHexText('') }} disabled={!value}>{defaultName}</button>
          </span>
        </span>
      )}
    </span>
  )
}

/** Bold, italic, size and colour of one part of the chart text. */
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
      <ColorPicker label={label} value={s.color} onChange={(c) => up({ color: c })} />
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
            {p.key === 'xTick' && <TickAngleSelect />}
          </span>
        </Field>
      ))}
      <FontSelect />
      {extra}
    </div>
  )
}

/**
 * "Aa Text" next to a chart's Image button: this chart's title and axis titles (written by the user; empty = the
 * chart's own, shown greyed), and the font, size, bold and italic of titles, tick labels and legends of all charts.
 */
export function ChartTextButton({ labels, setLabels, own }: { labels: ChartLabels; setLabels: (l: ChartLabels) => void; own: ChartLabels }) {
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
  const box = (k: keyof ChartLabels, label: string, fallback: string, width: number) => (
    <input className="input input-sm" value={labels[k]} maxLength={160} aria-label={label} placeholder={own[k] || fallback} style={{ width }}
      onChange={(e) => setLabels({ ...labels, [k]: e.target.value })} />
  )
  return (
    <div className="chart-text-button" ref={ref}>
      <button type="button" className="btn btn-ghost btn-sm" data-no-export="" aria-expanded={open} title="Title, axis titles and the font, size, bold and italic of the text" onClick={() => setOpen((v) => !v)}>Aa Text</button>
      {open && (
        <div ref={popRef} className="chart-text-pop card stack-sm" role="dialog" aria-label="Chart text">
          <ChartTextControls inputs={{ title: box('title', 'Plot title', 'no title', 220), yTitle: box('yTitle', 'Y axis title', 'none', 170), xTitle: box('xTitle', 'X axis title', 'none', 170) }} />
          <div className="row">
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => setLabels(NO_LABELS)} disabled={!labels.title && !labels.xTitle && !labels.yTitle}>Clear titles</button>
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => setChartText(DEFAULT_CHART_TEXT)}>Reset style</button>
          </div>
        </div>
      )}
    </div>
  )
}
