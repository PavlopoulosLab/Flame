import { useSyncExternalStore } from 'react'

/**
 * Text style of every chart in Flame: the font, and the size, bold and italic of the plot title, the axis titles,
 * the tick labels and the legend (plus the angle of the x tick labels). Set once (from any chart's "Aa Text"
 * button or the Bench Data figure) and kept in this browser; a size of 0 (Auto) or bold / italic off
 * leave a chart's own style.
 */
/** color: '' = the chart's own (grey, or the theme's text colour). */
export interface TextStyle { size: number; bold: boolean; italic: boolean; color: string }
export type TextPart = 'title' | 'yTitle' | 'xTitle' | 'yTick' | 'xTick' | 'legend'
export interface ChartText extends Record<TextPart, TextStyle> {
  /** '' = the chart's own font. */
  family: string
  /** Angle of the x tick labels in degrees (0 = the chart's own: horizontal). */
  tickAngle: number
}

const plain: TextStyle = { size: 0, bold: false, italic: false, color: '' }
export const DEFAULT_CHART_TEXT: ChartText = {
  family: '', tickAngle: 0,
  title: { size: 0, bold: true, italic: false, color: '' }, yTitle: plain, xTitle: plain, yTick: plain, xTick: plain, legend: plain,
}
export const TEXT_PARTS: { key: TextPart; label: string }[] = [
  { key: 'title', label: 'Plot title' }, { key: 'yTitle', label: 'Y axis title' }, { key: 'xTitle', label: 'X axis title' },
  { key: 'yTick', label: 'Y tick labels' }, { key: 'xTick', label: 'X tick labels' }, { key: 'legend', label: 'Legend' },
]
export const FONTS: [string, string][] = [['', 'Default'], ['Arial, Helvetica, sans-serif', 'Arial'], ['Helvetica, Arial, sans-serif', 'Helvetica'],
  ['Calibri, Carlito, sans-serif', 'Calibri'], ['Verdana, sans-serif', 'Verdana'], ['"Times New Roman", Times, serif', 'Times New Roman'],
  ['Georgia, serif', 'Georgia'], ['"Courier New", monospace', 'Courier New']]
export const TEXT_SIZES = [9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22, 24, 28, 32]
export const TICK_ANGLES: [number, string][] = [[0, 'Horizontal'], [30, 'Slanted 30°'], [45, 'Slanted 45°'], [90, 'Vertical']]

export const CHART_TEXT_KEY = 'flame-chart-text'
const listeners = new Set<() => void>()

/** A stored or pasted value, checked field by field (unknown or broken values fall back to the defaults). */
export function sanitizeChartText(v: unknown): ChartText {
  const o = (v && typeof v === 'object' ? v : {}) as Record<string, unknown>
  const style = (k: TextPart): TextStyle => {
    // Settings saved before the x and y tick labels were separate: one "tick" style for both.
    const src = o[k] ?? ((k === 'xTick' || k === 'yTick') ? o.tick : undefined)
    const s = (src && typeof src === 'object' ? src : {}) as Record<string, unknown>
    const d = DEFAULT_CHART_TEXT[k]
    return { size: typeof s.size === 'number' && s.size >= 0 && s.size <= 72 ? s.size : d.size,
      bold: typeof s.bold === 'boolean' ? s.bold : d.bold, italic: typeof s.italic === 'boolean' ? s.italic : d.italic,
      color: typeof s.color === 'string' && /^#[0-9a-f]{6}$/i.test(s.color) ? s.color.toLowerCase() : '' }
  }
  return {
    family: typeof o.family === 'string' && FONTS.some(([f]) => f === o.family) ? o.family : '',
    tickAngle: typeof o.tickAngle === 'number' && TICK_ANGLES.some(([a]) => a === o.tickAngle) ? o.tickAngle : 0,
    title: style('title'), yTitle: style('yTitle'), xTitle: style('xTitle'), yTick: style('yTick'), xTick: style('xTick'), legend: style('legend'),
  }
}

let current: ChartText = (() => {
  try { const s = localStorage.getItem(CHART_TEXT_KEY); return s ? sanitizeChartText(JSON.parse(s)) : DEFAULT_CHART_TEXT } catch { return DEFAULT_CHART_TEXT }
})()

export function getChartText(): ChartText { return current }
export function hasSavedChartText(): boolean { try { return localStorage.getItem(CHART_TEXT_KEY) !== null } catch { return false } }
export function setChartText(t: ChartText | ((t: ChartText) => ChartText)) {
  current = sanitizeChartText(typeof t === 'function' ? t(current) : t)
  try { localStorage.setItem(CHART_TEXT_KEY, JSON.stringify(current)) } catch { /* storage unavailable */ }
  listeners.forEach((l) => l())
}
export function useChartText(): ChartText {
  return useSyncExternalStore((l) => { listeners.add(l); return () => listeners.delete(l) }, () => current)
}

type Obj = Record<string, unknown>
const asObj = (v: unknown): Obj => (v && typeof v === 'object' ? v as Obj : typeof v === 'string' ? { text: v } : {})
/** A font with this part's size / bold / italic laid over the chart's own (Auto and "off" keep the chart's). */
function font(base: unknown, s: TextStyle, family: string): Obj {
  return { ...asObj(base), ...(s.size ? { size: s.size } : {}), ...(s.bold ? { weight: 'bold' } : {}), ...(s.italic ? { style: 'italic' } : {}), ...(s.color ? { color: s.color } : {}), ...(family ? { family } : {}) }
}

/** Text written for one chart ('' = the chart's own). */
export interface ChartLabels { title: string; yTitle: string; xTitle: string }
export const NO_LABELS: ChartLabels = { title: '', yTitle: '', xTitle: '' }

/** The chart's own title and axis titles (shown as placeholders where the user can write their own). */
export function chartLabels(layout: Obj): ChartLabels {
  const txt = (v: unknown) => String(asObj(v).text ?? '')
  return { title: txt(layout.title), yTitle: txt(asObj(layout.yaxis).title), xTitle: txt(asObj(layout.xaxis).title) }
}

/** The layout with the user's title and axis titles for this chart (room is made above for a title). */
export function applyChartLabels<L extends Obj>(layout: L, l: ChartLabels, t: ChartText): L {
  const out: Obj = { ...layout }
  const title = l.title.trim(), y = l.yTitle.trim(), x = l.xTitle.trim()
  if (title) {
    const size = t.title.size || 17
    out.title = { ...asObj(out.title), text: title, x: 0.5, xanchor: 'center', y: 0.98, yanchor: 'top', font: font(asObj(asObj(out.title).font), { ...t.title, size }, t.family) }
    const m = asObj(out.margin)
    out.margin = { ...m, t: Math.max(Number(m.t ?? 20), size * 2 + 16) }
  }
  const axis = (k: 'xaxis' | 'yaxis', text: string, s: TextStyle) => {
    if (!text) return
    const ax = asObj(out[k]), ti = asObj(ax.title)
    out[k] = { ...ax, title: { ...ti, text, font: font(ti.font, s, t.family) } }
  }
  axis('yaxis', y, t.yTitle); axis('xaxis', x, t.xTitle)
  return out as L
}

/** The chart's Plotly layout with the text style applied to its title, every x / y axis and the legend. */
export function applyChartText<L extends Obj>(layout: L, t: ChartText): L {
  const out: Obj = { ...layout }
  if (t.family) out.font = { ...asObj(out.font), family: t.family }
  if (out.title != null) { const ti = asObj(out.title); out.title = { ...ti, font: font(ti.font, t.title, t.family) } }
  for (const k of Object.keys(out).filter((x) => /^[xy]axis\d*$/.test(x))) {
    const ax = asObj(out[k]), x = k.startsWith('x')
    const ti = ax.title == null ? null : asObj(ax.title)
    out[k] = { ...ax, tickfont: font(ax.tickfont, x ? t.xTick : t.yTick, t.family), ...(ti ? { title: { ...ti, font: font(ti.font, x ? t.xTitle : t.yTitle, t.family) } } : {}),
      ...(x && t.tickAngle ? { tickangle: -t.tickAngle, automargin: true } : {}) }
  }
  out.legend = { ...asObj(out.legend), font: font(asObj(out.legend).font, t.legend, t.family) }
  return out as L
}

// ---- Colours typed by the user ----
export const hex2 = (n: number) => Math.max(0, Math.min(255, Math.round(n || 0))).toString(16).padStart(2, '0')
export const toRgb = (h: string) => [1, 3, 5].map((i) => parseInt(h.slice(i, i + 2), 16))
/** "#abc", "#aabbcc", "aabbcc" or "rgb(10, 20, 30)" / "10 20 30" as #rrggbb (null if not a colour). */
export function parseColor(v: string): string | null {
  const t = v.trim().toLowerCase()
  let m = t.match(/^#?([0-9a-f]{6})$/)
  if (m) return `#${m[1]}`
  m = t.match(/^#?([0-9a-f])([0-9a-f])([0-9a-f])$/)
  if (m) return `#${m[1]}${m[1]}${m[2]}${m[2]}${m[3]}${m[3]}`
  m = t.match(/^(?:rgb\s*\()?\s*(\d{1,3})\s*[,\s]\s*(\d{1,3})\s*[,\s]\s*(\d{1,3})\s*\)?$/)
  if (m && [m[1], m[2], m[3]].every((x) => Number(x) <= 255)) return `#${hex2(+m[1])}${hex2(+m[2])}${hex2(+m[3])}`
  return null
}


/** #rrggbb as hue (0-360), saturation and value (0-1). */
export function hexToHsv(h: string): [number, number, number] {
  const [r, g, b] = toRgb(h).map((x) => x / 255)
  const max = Math.max(r, g, b), min = Math.min(r, g, b), d = max - min
  let hue = 0
  if (d) hue = max === r ? ((g - b) / d) % 6 : max === g ? (b - r) / d + 2 : (r - g) / d + 4
  return [((hue * 60) + 360) % 360, max ? d / max : 0, max]
}
/** Hue (0-360), saturation and value (0-1) as #rrggbb. */
export function hsvToHex(h: number, s: number, v: number): string {
  const f = (n: number) => { const k = (n + h / 60) % 6; return v - v * s * Math.max(0, Math.min(k, 4 - k, 1)) }
  return `#${[f(5), f(3), f(1)].map((x) => hex2(x * 255)).join('')}`
}
