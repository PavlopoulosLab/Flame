import { useSyncExternalStore } from 'react'
import { hex2 } from './chartText'

/**
 * Colours chosen by the user for one chart, over the chart's own (and its palette's). Every Plotly chart
 * offers them under its "Colours" button: one colour per series (each named trace, e.g. Up-regulated /
 * Down-regulated / Not significant of a volcano, or each source of a bar chart), one per distinct colour of
 * charts that colour points one by one (Sankey nodes, highlighted bars), and the low / middle / high ends of
 * colour scales (heatmaps, p-value shading).
 *
 * Keys: `t:<series>` for a series, `c:<#rrggbb>` for a colour used point by point, `s:<trace>:low|mid|high`
 * for a colour scale. Values are #rrggbb.
 */
export type ChartColours = Record<string, string>

export interface ColourRole {
  key: string
  label: string
  /** The chart's own colour (#rrggbb). */
  color: string
  /** Where the colour is used (e.g. the first bars or nodes painted with it). */
  hint?: string
  /** Roles of one colour scale share a group (shown on one row: low, middle, high). */
  group?: string
  /** The colour as the chart gives it (its opacity is kept for the chosen colour). */
  raw?: string
}

type Obj = Record<string, unknown>
const isObj = (v: unknown): v is Obj => !!v && typeof v === 'object' && !Array.isArray(v)

// ---- Colour strings ----

let ctx: CanvasRenderingContext2D | null | undefined
/** A CSS colour name ("grey", "steelblue") as #rrggbb, via a canvas (null outside the browser). */
function named(c: string): string | null {
  if (ctx === undefined) {
    try { ctx = typeof document !== 'undefined' ? document.createElement('canvas').getContext('2d') : null } catch { ctx = null }
  }
  if (!ctx) return null
  ctx.fillStyle = '#000001'
  ctx.fillStyle = c
  const out = String(ctx.fillStyle)
  return out === '#000001' ? null : out.startsWith('#') ? out : null
}

/** Any Plotly colour string as #rrggbb plus its opacity (null: not a colour, e.g. a number). */
export function parseCss(c: unknown): { hex: string; alpha: number } | null {
  if (typeof c !== 'string') return null
  const t = c.trim().toLowerCase()
  let m = t.match(/^#([0-9a-f]{6})([0-9a-f]{2})?$/)
  if (m) return { hex: `#${m[1]}`, alpha: m[2] ? parseInt(m[2], 16) / 255 : 1 }
  m = t.match(/^#([0-9a-f])([0-9a-f])([0-9a-f])$/)
  if (m) return { hex: `#${m[1]}${m[1]}${m[2]}${m[2]}${m[3]}${m[3]}`, alpha: 1 }
  m = t.match(/^rgba?\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*(?:,\s*([\d.]+)\s*)?\)$/)
  if (m) return { hex: `#${hex2(+m[1])}${hex2(+m[2])}${hex2(+m[3])}`, alpha: m[4] != null ? Math.min(1, +m[4]) : 1 }
  if (/^[a-z]+$/.test(t) && t !== 'transparent') { const h = named(t); return h ? { hex: h, alpha: 1 } : null }
  return null
}

/** `hex` with the opacity of `original` (rgba when it was see-through, so fills and links stay light). */
function withAlpha(hex: string, original: unknown): string {
  const a = parseCss(original)?.alpha ?? 1
  if (a >= 1) return hex
  const [r, g, b] = [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16))
  return `rgba(${r},${g},${b},${+a.toFixed(3)})`
}

// ---- Which colours a chart uses ----

type Scale = [number, string][]
/** Plotly's built-in colour scales, reduced to their low, middle and high colours (from plotly.js 3). */
const NAMED: Record<string, string[]> = {
  Greys: ['#000000', '#ffffff'], YlGnBu: ['#081d58', '#41b6c4', '#ffffd9'], Greens: ['#00441b', '#74c476', '#f7fcf5'],
  YlOrRd: ['#800026', '#fd8d3c', '#ffffcc'], Bluered: ['#0000ff', '#ff0000'], RdBu: ['#050aac', '#bebebe', '#b20a1c'],
  Reds: ['#dcdcdc', '#f5a069', '#b20a1c'], Blues: ['#050aac', '#4664f5', '#dcdcdc'], Picnic: ['#0000ff', '#ffffff', '#ff0000'],
  Rainbow: ['#96005a', '#2cff96', '#ff0000'], Portland: ['#0c3383', '#f2d338', '#d91e1e'], Jet: ['#000083', '#05ffff', '#800000'],
  Hot: ['#000000', '#ffd200', '#ffffff'], Blackbody: ['#000000', '#e6d200', '#a0c8ff'], Earth: ['#000082', '#e6e632', '#ffffff'],
  Electric: ['#000000', '#780064', '#fffadc'], Viridis: ['#440154', '#21918c', '#fde725'], Cividis: ['#00204c', '#757575', '#ffe945'],
}
/** A colour scale as stops: its own, a built-in name, or Plotly's default for the trace (null if unknown). */
function stopsOf(v: unknown, host: Obj): Scale | null {
  if (isScale(v)) return v
  const name = typeof v === 'string' ? v : host.colorscale == null ? (SCALE_TYPES.has(String(host.type)) ? 'RdBu' : 'Reds') : ''
  const n = Object.keys(NAMED).find((k) => k.toLowerCase() === name.toLowerCase())
  return n ? NAMED[n].map((c, i, a) => [i / (a.length - 1), c] as [number, string]) : null
}
const isScale = (v: unknown): v is Scale => Array.isArray(v) && v.length >= 2 && v.every((s) => Array.isArray(s) && s.length === 2 && typeof s[1] === 'string')
const SCALE_TYPES = new Set(['heatmap', 'contour', 'histogram2d', 'histogram2dcontour', 'densitymapbox', 'surface'])

/** The low, middle (scales of 3+ stops) and high colours of a scale as drawn (reversescale applied), as given. */
function scaleEnds(scale: Scale, reverse: boolean): { low: string; mid?: string; high: string } | null {
  const stops = reverse ? [...scale].reverse().map(([v, c]) => [1 - v, c] as [number, string]) : scale
  const low = stops[0][1], high = stops[stops.length - 1][1]
  if (!parseCss(low) || !parseCss(high)) return null
  if (stops.length < 3) return { low, high }
  const mid = stops.reduce((best, s) => (Math.abs(s[0] - 0.5) < Math.abs(best[0] - 0.5) ? s : best), stops[1])[1]
  return { low, mid: parseCss(mid) ? mid : undefined, high }
}

const lineOnly = (t: Obj) => String(t.mode ?? '').includes('lines') && !String(t.mode ?? '').includes('markers')
/** Where a trace keeps its colour: a colour scale (on the trace or its marker), or marker / line colours. */
function scaleHost(t: Obj): Obj | null {
  const type = String(t.type ?? 'scatter')
  if (SCALE_TYPES.has(type)) return t
  const mk = t.marker
  if (isObj(mk) && Array.isArray(mk.color) && mk.color.some((x) => typeof x === 'number')) return mk
  return null
}

const MAX_POINT_COLOURS = 24

/** A short label for the points painted with one colour: their categories or names. */
function pointNames(t: Obj, idx: number[]): string {
  const horizontal = t.orientation === 'h'
  const src = [t.labels, horizontal ? t.y : t.x, t.text].find((a) => Array.isArray(a) && a.some((x) => typeof x === 'string')) as unknown[] | undefined
  if (!src) return ''
  const names = [...new Set(idx.map((i) => String(src[i] ?? '').replace(/<[^>]+>/g, ' ').trim()).filter(Boolean))]
  return names.slice(0, 3).map((n) => (n.length > 24 ? `${n.slice(0, 23)}…` : n)).join(', ') + (names.length > 3 ? '…' : '')
}

/** A series' name: its legend group or name, or what it draws when it has neither. */
function seriesKey(t: Obj, i: number): string {
  return String(t.legendgroup || t.name || `#${i + 1}`)
}
function seriesLabel(t: Obj, i: number, n: number): string {
  if (t.legendgroup || t.name) return String(t.name || t.legendgroup).replace(/<[^>]+>/g, ' ').trim()
  if (n > 1) return `Series ${i + 1}`
  const type = String(t.type ?? 'scatter')
  return type === 'bar' ? 'Bars' : type === 'box' ? 'Boxes' : type === 'violin' ? 'Violins' : type === 'histogram' ? 'Bars'
    : lineOnly(t) ? 'Line' : String(t.fill ?? 'none') !== 'none' ? 'Area' : 'Points'
}

/** The single colour a series is drawn in (explicit, or Plotly's default from the colourway). */
function seriesColour(t: Obj, i: number, colorway: string[]): string | null {
  const mk = isObj(t.marker) ? t.marker : {}, ln = isObj(t.line) ? t.line : {}
  const first = lineOnly(t) ? [ln.color, mk.color, t.fillcolor] : [mk.color, ln.color, t.fillcolor]
  for (const c of first) { if (Array.isArray(c)) return null; const p = parseCss(c); if (p) return p.hex }
  return colorway.length ? parseCss(colorway[i % colorway.length])?.hex ?? null : null
}

/** Arrays of colours painted point by point (bars, nodes, links). */
function colourArrays(t: Obj): unknown[][] {
  const out: unknown[][] = []
  const mk = isObj(t.marker) ? t.marker : null
  if (mk && Array.isArray(mk.color)) out.push(mk.color)
  if (mk && Array.isArray(mk.colors)) out.push(mk.colors) // pie
  if (t.type === 'sankey') {
    if (isObj(t.node) && Array.isArray(t.node.color)) out.push(t.node.color)
    if (isObj(t.link) && Array.isArray(t.link.color)) out.push(t.link.color)
  }
  return out
}

/** Traces drawn invisibly (e.g. an empty trace that only sets up a second axis) have no colour to choose. */
function hidden(t: Obj): boolean {
  if (t.visible === false || t.opacity === 0 || (isObj(t.marker) && t.marker.opacity === 0 && !lineOnly(t))) return true
  return t.showlegend === false && t.hoverinfo === 'skip' && !t.name && !SCALE_TYPES.has(String(t.type))
}

/** The colour roles of a chart, in drawing order (`colorway`: the colours of series that have none). */
export function colourRoles(data: Obj[], colorway: string[]): ColourRole[] {
  const roles: ColourRole[] = []
  const seen = new Set<string>()
  const add = (r: ColourRole) => { if (!seen.has(r.key)) { seen.add(r.key); roles.push(r) } }
  const n = data.filter((t) => !hidden(t)).length
  data.forEach((t, i) => {
    if (hidden(t)) return
    const host = scaleHost(t)
    if (host) {
      const drawn = stopsOf(host.colorscale, host)
      const ends = drawn ? scaleEnds(drawn, !!host.reversescale) : null
      if (ends) {
        const group = `s:${seriesKey(t, i)}`
        const name = t.name || t.legendgroup ? `${seriesLabel(t, i, n)} · ` : ''
        const role = (part: 'low' | 'mid' | 'high', label: string, raw: string) => add({ key: `${group}:${part}`, label: `${name}${label}`, color: parseCss(raw)!.hex, raw, group })
        role('low', 'Low', ends.low)
        if (ends.mid) role('mid', 'Middle', ends.mid)
        role('high', 'High', ends.high)
      }
    }
    const arrays = colourArrays(t)
    let pointRoles = 0
    arrays.forEach((arr, ai) => {
      const byHex = new Map<string, number[]>()
      arr.forEach((c, j) => { const p = parseCss(c); if (p) byHex.set(p.hex, [...(byHex.get(p.hex) ?? []), j]) })
      if (byHex.size > MAX_POINT_COLOURS) return
      const named = t.type === 'sankey' && ai === 0 ? { ...t, labels: isObj(t.node) ? t.node.label : undefined } : t
      byHex.forEach((idx, hex) => { pointRoles++; add({ key: `c:${hex}`, label: hex, color: hex, hint: pointNames(named, idx) }) })
    })
    // Point-by-point colours and scales cover the trace; its outline / line colour is left as drawn.
    if (pointRoles || host || t.type === 'sankey' || t.type === 'pie') return
    const c = seriesColour(t, i, colorway)
    if (c) add({ key: `t:${seriesKey(t, i)}`, label: seriesLabel(t, i, n), color: c })
  })
  // Point colours are named after their order of appearance.
  let k = 0
  roles.forEach((r) => { if (r.key.startsWith('c:')) r.label = `Colour ${++k}` })
  return roles
}

// ---- Drawing a chart with the user's colours ----

/** Original → chosen colour for every overridden role (used for point colours and hand-drawn legends). */
function colourMap(roles: ColourRole[], chosen: ChartColours): Map<string, string> {
  const m = new Map<string, string>()
  roles.forEach((r) => { const c = chosen[r.key]; if (c && !r.group) m.set(r.color, c) })
  return m
}

const recolour = (c: unknown, m: Map<string, string>): unknown => {
  const p = parseCss(c)
  const to = p && m.get(p.hex)
  return to ? withAlpha(to, c) : c
}

/** The chart's traces and layout with the user's colours. Traces and layout are copied only where changed. */
export function applyChartColours<L extends Obj>(data: Obj[], layout: L, chosen: ChartColours, colorway: string[]): { data: Obj[]; layout: L } {
  if (!Object.keys(chosen).length) return { data, layout }
  const roles = colourRoles(data, colorway)
  const byKey = new Map(roles.map((r) => [r.key, r]))
  const pointMap = new Map<string, string>()
  roles.forEach((r) => { const c = chosen[r.key]; if (c && r.key.startsWith('c:')) pointMap.set(r.color, c) })
  const out = data.map((t, i) => {
    if (t.visible === false) return t
    let next: Obj = t
    const edit = (patch: Obj) => { next = { ...next, ...patch } }
    // Colour scale: replaced by the chosen low (middle) high stops.
    const host = scaleHost(t)
    if (host) {
      const group = `s:${seriesKey(t, i)}`
      const low = byKey.get(`${group}:low`), mid = byKey.get(`${group}:mid`), high = byKey.get(`${group}:high`)
      if (low && high && (chosen[low.key] || chosen[high.key] || (mid && chosen[mid.key]))) {
        const pick = (r: ColourRole) => (chosen[r.key] ? withAlpha(chosen[r.key], r.raw) : r.raw ?? r.color)
        const scale: Scale = mid ? [[0, pick(low)], [0.5, pick(mid)], [1, pick(high)]] : [[0, pick(low)], [1, pick(high)]]
        if (host === t) edit({ colorscale: scale, reversescale: false })
        else edit({ marker: { ...(next.marker as Obj), colorscale: scale, reversescale: false } })
      }
    }
    // Point-by-point colours.
    if (pointMap.size) {
      const mapArr = (a: unknown) => (Array.isArray(a) ? a.map((c) => recolour(c, pointMap)) : a)
      const mk = isObj(next.marker) ? next.marker : null
      if (mk && (Array.isArray(mk.color) || Array.isArray(mk.colors))) edit({ marker: { ...mk, color: mapArr(mk.color), ...(mk.colors ? { colors: mapArr(mk.colors) } : {}) } })
      if (t.type === 'sankey') {
        if (isObj(t.node)) edit({ node: { ...t.node, color: mapArr(t.node.color) } })
        if (isObj(t.link)) edit({ link: { ...t.link, color: mapArr(t.link.color) } })
      }
    }
    // One colour for the whole series: its markers, line and fill.
    const role = byKey.get(`t:${seriesKey(t, i)}`)
    const c = role && chosen[role.key]
    if (c) {
      const mk = isObj(next.marker) ? next.marker : {}, ln = isObj(next.line) ? next.line : {}
      const type = String(t.type ?? 'scatter')
      const boxy = type === 'box' || type === 'violin'
      const usesLine = lineOnly(t) || String(t.mode ?? '').includes('lines') || boxy
      edit({
        ...(!lineOnly(t) || mk.color != null ? { marker: { ...mk, color: withAlpha(c, mk.color) } } : {}),
        ...(usesLine || ln.color != null ? { line: { ...ln, color: withAlpha(c, ln.color) } } : {}),
        ...(next.fillcolor != null ? { fillcolor: withAlpha(c, next.fillcolor) } : {}),
      })
    }
    return next
  })
  // Hand-drawn legends (shapes / annotations named "legend…") follow the colours they show.
  const m = colourMap(roles, chosen)
  const isLegend = (x: unknown) => isObj(x) && String(x.name ?? '').startsWith('legend')
  let lay: Obj = layout
  if (m.size && Array.isArray(layout.shapes) && layout.shapes.some(isLegend)) {
    lay = { ...lay, shapes: layout.shapes.map((s) => (isLegend(s) ? { ...s, fillcolor: recolour(s.fillcolor, m), ...(isObj(s.line) ? { line: { ...s.line, color: recolour(s.line.color, m) } } : {}) } : s)) }
  }
  if (m.size && Array.isArray(layout.annotations) && layout.annotations.some(isLegend)) {
    lay = { ...lay, annotations: layout.annotations.map((a) => (isLegend(a) && isObj(a.font) ? { ...a, font: { ...a.font, color: recolour(a.font.color, m) } } : a)) }
  }
  return { data: out, layout: lay as L }
}

// ---- Kept per chart in this browser ----

const PREFIX = 'flame-chart-colours:'
const HEX = /^#[0-9a-f]{6}$/
const listeners = new Set<() => void>()
const cache = new Map<string, ChartColours>()
const EMPTY: ChartColours = {}

export function sanitizeChartColours(v: unknown): ChartColours {
  if (!isObj(v)) return EMPTY
  const out: ChartColours = {}
  Object.entries(v).forEach(([k, c]) => { if (typeof c === 'string' && HEX.test(c.toLowerCase()) && k.length <= 200) out[k] = c.toLowerCase() })
  return Object.keys(out).length ? out : EMPTY
}

/** The colours chosen for the chart `id` (its export name; '' = not kept). */
export function getChartColours(id: string): ChartColours {
  if (!id) return EMPTY
  if (!cache.has(id)) {
    let v: ChartColours = EMPTY
    try { v = sanitizeChartColours(JSON.parse(localStorage.getItem(PREFIX + id) ?? 'null')) } catch { /* storage unavailable */ }
    cache.set(id, v)
  }
  return cache.get(id)!
}
export function setChartColours(id: string, c: ChartColours) {
  const v = sanitizeChartColours(c)
  cache.set(id, v)
  try { if (Object.keys(v).length) localStorage.setItem(PREFIX + id, JSON.stringify(v)); else localStorage.removeItem(PREFIX + id) } catch { /* storage unavailable */ }
  listeners.forEach((l) => l())
}
export function useChartColours(id: string): ChartColours {
  return useSyncExternalStore((l) => { listeners.add(l); return () => listeners.delete(l) }, () => getChartColours(id))
}
