/**
 * Image export for every figure in Flame (Plotly charts, vis-network graphs, inline SVG and
 * HTML figures). A figure describes itself as an ImageSource; the export dialog
 * (components/ExportImage.tsx) asks for format, area, resolution, background and options and
 * calls exportImage().
 */
import { create } from 'zustand'
import { saveBlob } from '../api/client'

export type ImageFormat = 'png' | 'svg' | 'pdf' | 'jpeg' | 'webp'
export type Background = 'theme' | 'white' | 'transparent'
export type Area = 'all' | 'view'

export const FORMATS: { value: ImageFormat; label: string; text: string; ext: string; vector?: boolean; alpha?: boolean }[] = [
  { value: 'png', label: 'PNG', text: 'Sharp image; transparent background possible', ext: 'png', alpha: true },
  { value: 'svg', label: 'SVG', text: 'Vector: sharp at any size, editable in Illustrator or Inkscape', ext: 'svg', vector: true, alpha: true },
  { value: 'pdf', label: 'PDF', text: 'One page for documents and printing', ext: 'pdf' },
  { value: 'jpeg', label: 'JPEG', text: 'Smaller files; no transparency', ext: 'jpg' },
  { value: 'webp', label: 'WebP', text: 'Small, sharp images for the web', ext: 'webp', alpha: true },
]

export const SCALES = [
  { value: 1, label: '1× · 96 dpi (screen)' },
  { value: 2, label: '2× · 192 dpi (high resolution)' },
  { value: 3, label: '3× · 288 dpi (print)' },
  { value: 4, label: '4× · 384 dpi (poster)' },
]

export interface RenderOptions {
  /** Output pixels per on-screen pixel (raster formats). */
  scale: number
  /** CSS colour, or null for transparent. */
  background: string | null
  /** Draw text and lines in light-theme colours (white background while the app is dark). */
  light: boolean
  legend: boolean
  area: Area
  /** Leave out selection highlighting (figures that draw selections themselves, e.g. networks). */
  plain: boolean
}

export interface ImageSource {
  /** Default file name (no extension). */
  name: string
  /** Size of the exported image in CSS px at 1×. */
  size: (area: Area) => { w: number; h: number }
  /** Which options apply to this figure. */
  features: { legend?: boolean; area?: boolean; selection?: boolean }
  /** Vector rendering (also rasterised for PNG/JPEG/WebP/PDF unless `raster` is given). */
  svg: (o: RenderOptions) => Promise<string> | string
  /** Native raster rendering (canvas-based figures). */
  raster?: (o: RenderOptions) => Promise<HTMLCanvasElement> | HTMLCanvasElement
}

export interface ExportRequest extends Omit<RenderOptions, 'background' | 'light' | 'plain'> {
  format: ImageFormat
  backgroundMode: Background
  /** Leave out selection and search highlighting. */
  plain: boolean
  filename: string
}

// ---------------------------------------------------------------------------
// Shared state: the open dialog, and "plain" mode, during which views draw without their
// selection highlighting (see usePlainSelection).
// ---------------------------------------------------------------------------
export const useImageExport = create<{
  source: ImageSource | null
  /** Increases on every open, so the dialog starts fresh. */
  seq: number
  plain: boolean
  open: (s: ImageSource) => void
  close: () => void
  setPlain: (p: boolean) => void
}>((set) => ({
  source: null,
  seq: 0,
  plain: false,
  open: (source) => set((st) => ({ source, seq: st.seq + 1 })),
  close: () => set({ source: null }),
  setPlain: (plain) => set({ plain }),
}))

/** The selection to draw: empty while an image is exported without selection highlighting. */
export function usePlainSelection<T>(selection: T, empty: T): T {
  const plain = useImageExport((s) => s.plain)
  return plain ? empty : selection
}

export const openImageExport = (s: ImageSource) => useImageExport.getState().open(s)

// ---------------------------------------------------------------------------
// Theme helpers
// ---------------------------------------------------------------------------
export const isDarkTheme = () => document.documentElement.getAttribute('data-theme') === 'dark'
export const cssVar = (name: string, fallback: string) => getComputedStyle(document.documentElement).getPropertyValue(name).trim() || fallback

/** Run `fn` with the light theme applied (synchronously, so nothing flickers on screen). */
export function withLightTheme<T>(light: boolean, fn: () => T): T {
  const root = document.documentElement
  const prev = root.getAttribute('data-theme')
  if (!light || prev !== 'dark') return fn()
  root.removeAttribute('data-theme')
  try { return fn() } finally { root.setAttribute('data-theme', prev) }
}

export const LIGHT = { text: '#3f3f46', heading: '#18181b', grid: '#e4e4e7', surface: '#ffffff' }

// ---------------------------------------------------------------------------
// SVG from DOM: inline SVG figures and HTML figures (via foreignObject)
// ---------------------------------------------------------------------------
const SVG_PROPS = ['fill', 'fill-opacity', 'stroke', 'stroke-width', 'stroke-opacity', 'stroke-dasharray', 'opacity', 'font-family', 'font-size',
  'font-weight', 'font-style', 'text-anchor', 'dominant-baseline', 'visibility', 'display', 'color']
const XHTML = 'http://www.w3.org/1999/xhtml'
const SVGNS = 'http://www.w3.org/2000/svg'
const esc = (s: string) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')

/** Copy computed styles onto the clone, so the file looks the same outside the page. */
function inlineStyles(orig: Element, clone: Element, props: string[] | 'all') {
  const a = [orig, ...orig.querySelectorAll('*')]
  const b = [clone, ...clone.querySelectorAll('*')]
  a.forEach((el, i) => {
    const c = b[i] as HTMLElement | SVGElement | undefined
    if (!c || !('style' in c)) return
    const cs = getComputedStyle(el)
    if (props === 'all') {
      let text = ''
      for (let k = 0; k < cs.length; k++) {
        const p = cs[k]
        text += `${p}:${cs.getPropertyValue(p)};`
      }
      c.setAttribute('style', text)
    } else {
      props.forEach((p) => c.style.setProperty(p, cs.getPropertyValue(p)))
    }
  })
}

/** Elements marked with this attribute (toolbars, buttons) are left out of exported images. */
export const NO_EXPORT = 'data-no-export'

export interface LegendItem { label: string; color: string }

/** Rows of legend items under a figure of width `w` (px); returns markup and height. */
function legendRows(items: LegendItem[], w: number, textColor: string) {
  const parts: string[] = []
  let x = 8, y = 14
  items.forEach((it) => {
    const iw = 18 + it.label.length * 6.6 + 14
    if (x + iw > w && x > 8) { x = 8; y += 20 }
    parts.push(`<circle cx="${x + 5}" cy="${y}" r="5" fill="${esc(it.color)}"/><text x="${x + 15}" y="${y}" font-size="12" dominant-baseline="central" fill="${esc(textColor)}">${esc(it.label)}</text>`)
    x += iw
  })
  return { markup: parts.join(''), height: items.length ? y + 14 : 0 }
}

/** CSS colour (possibly var(...)) → the colour it resolves to inside `ctx`. */
function resolveColor(color: string, ctx: Element) {
  const probe = document.createElementNS(SVGNS, 'g')
  probe.style.color = color
  ctx.appendChild(probe)
  const out = getComputedStyle(probe).color || color
  probe.remove()
  return out
}

/** The figure's viewBox widened to everything drawn (labels can reach past it), plus px per unit. */
function svgFrame(svg: SVGSVGElement) {
  const box = svg.getBoundingClientRect()
  const vb = svg.viewBox.baseVal
  if (!vb || !vb.width) return { x: 0, y: 0, w: box.width, h: box.height, k: 1, hasViewBox: false }
  let x0 = vb.x, y0 = vb.y, x1 = vb.x + vb.width, y1 = vb.y + vb.height
  try {
    const bb = svg.getBBox()
    if (bb.width && bb.height) { x0 = Math.min(x0, bb.x - 4); y0 = Math.min(y0, bb.y - 4); x1 = Math.max(x1, bb.x + bb.width + 4); y1 = Math.max(y1, bb.y + bb.height + 4) }
  } catch { /* not rendered */ }
  return { x: x0, y: y0, w: x1 - x0, h: y1 - y0, k: box.width / vb.width, hasViewBox: true }
}

/** Size of an inline SVG figure (everything drawn), plus its legend rows. */
export function svgFigureSize(svg: SVGSVGElement, legend?: LegendItem[]) {
  const f = svgFrame(svg)
  const w = f.w * f.k, h = f.h * f.k
  return { w, h: h + (legend?.length ? legendRows(legend, w, '#000').height : 0) }
}

/** Inline <svg> element → standalone SVG markup (with an optional legend underneath). */
export function svgElementToString(svg: SVGSVGElement, o: RenderOptions, legend?: LegendItem[]): string {
  return withLightTheme(o.light, () => {
    const f = svgFrame(svg)
    const w = f.w * f.k, h = f.h * f.k
    const clone = svg.cloneNode(true) as SVGSVGElement
    inlineStyles(svg, clone, SVG_PROPS)
    clone.querySelectorAll(`[${NO_EXPORT}]`).forEach((n) => n.remove())
    clone.setAttribute('xmlns', SVGNS)
    clone.removeAttribute('style')
    clone.style.fontFamily = 'Inter, system-ui, sans-serif'
    clone.style.color = getComputedStyle(svg).color
    clone.style.overflow = 'visible'
    if (f.hasViewBox) clone.setAttribute('viewBox', `${f.x} ${f.y} ${f.w} ${f.h}`)
    clone.setAttribute('width', String(Math.round(w)))
    clone.setAttribute('height', String(Math.round(h)))
    if (o.background) {
      const r = document.createElementNS(SVGNS, 'rect')
      r.setAttribute('x', String(f.x)); r.setAttribute('y', String(f.y)); r.setAttribute('width', String(f.w)); r.setAttribute('height', String(f.h))
      r.setAttribute('fill', o.background)
      clone.insertBefore(r, clone.firstChild)
    }
    if (legend?.length && o.legend) {
      // Extend the canvas downwards (in viewBox units) and draw the legend there.
      const items = legend.map((it) => ({ ...it, color: resolveColor(it.color, svg) }))
      const rows = legendRows(items, w, getComputedStyle(svg).color)
      const g = document.createElementNS(SVGNS, 'g')
      g.setAttribute('transform', `translate(${f.x},${f.y + f.h}) scale(${1 / f.k})`)
      g.setAttribute('font-family', 'Inter, system-ui, sans-serif')
      g.innerHTML = (o.background ? `<rect width="${w}" height="${rows.height}" fill="${esc(o.background)}"/>` : '') + rows.markup
      clone.appendChild(g)
      clone.setAttribute('viewBox', `${f.x} ${f.y} ${f.w} ${f.h + rows.height / f.k}`)
      clone.setAttribute('height', String(Math.round(h + rows.height)))
    }
    return new XMLSerializer().serializeToString(clone)
  })
}

/** Size of an HTML figure without its NO_EXPORT parts (e.g. a button row at the bottom). */
export function htmlFigureSize(el: HTMLElement) {
  const top = el.getBoundingClientRect().top
  let bottom = top
  ;[...el.children].forEach((c) => { if (!c.hasAttribute(NO_EXPORT)) bottom = Math.max(bottom, c.getBoundingClientRect().bottom) })
  return { w: Math.ceil(el.getBoundingClientRect().width), h: Math.ceil(bottom - top) }
}

/** HTML element → SVG with a foreignObject (rasterises faithfully in browsers). */
export function htmlElementToString(el: HTMLElement, o: RenderOptions): string {
  return withLightTheme(o.light, () => {
    const { w, h } = htmlFigureSize(el)
    const clone = el.cloneNode(true) as HTMLElement
    inlineStyles(el, clone, 'all')
    clone.querySelectorAll(`[${NO_EXPORT}]`).forEach((n) => n.remove())
    clone.setAttribute('xmlns', XHTML)
    clone.style.margin = '0'
    clone.style.background = o.background ?? 'transparent'
    const body = new XMLSerializer().serializeToString(clone)
    return `<svg xmlns="${SVGNS}" width="${w}" height="${h}" viewBox="0 0 ${w} ${h}">` +
      (o.background ? `<rect width="100%" height="100%" fill="${esc(o.background)}"/>` : '') +
      `<foreignObject x="0" y="0" width="${w}" height="${h}">${body}</foreignObject></svg>`
  })
}

// ---------------------------------------------------------------------------
// Rasterising, encoding and saving
// ---------------------------------------------------------------------------
export function svgSize(svg: string) {
  const doc = new DOMParser().parseFromString(svg, 'image/svg+xml').documentElement
  return { w: parseFloat(doc.getAttribute('width') ?? '0'), h: parseFloat(doc.getAttribute('height') ?? '0') }
}

export async function rasterise(svg: string, w: number, h: number, scale: number, background: string | null): Promise<HTMLCanvasElement> {
  // A data: URL (not a blob: URL) keeps the canvas readable when the SVG holds HTML (foreignObject).
  const url = `data:image/svg+xml;charset=utf-8,${encodeURIComponent(svg)}`
  {
    const img = new Image()
    img.decoding = 'sync'
    await new Promise<void>((resolve, reject) => {
      img.onload = () => resolve()
      img.onerror = () => reject(new Error('The figure could not be drawn as an image.'))
      img.src = url
    })
    const canvas = document.createElement('canvas')
    canvas.width = Math.round(w * scale)
    canvas.height = Math.round(h * scale)
    const ctx = canvas.getContext('2d')!
    if (background) { ctx.fillStyle = background; ctx.fillRect(0, 0, canvas.width, canvas.height) }
    ctx.drawImage(img, 0, 0, canvas.width, canvas.height)
    return canvas
  }
}

/** Put a canvas on a solid background (for formats without transparency). */
function flatten(canvas: HTMLCanvasElement, background: string) {
  const out = document.createElement('canvas')
  out.width = canvas.width
  out.height = canvas.height
  const ctx = out.getContext('2d')!
  ctx.fillStyle = background
  ctx.fillRect(0, 0, out.width, out.height)
  ctx.drawImage(canvas, 0, 0)
  return out
}

const toBlob = (canvas: HTMLCanvasElement, type: string, quality?: number) =>
  new Promise<Blob>((resolve, reject) => canvas.toBlob((b) => (b ? resolve(b) : reject(new Error('Could not encode the image.'))), type, quality))

/** One-page PDF holding a JPEG; the page has the figure's on-screen size (96 px = 1 in). */
export async function pdfFromCanvas(canvas: HTMLCanvasElement, cssW: number, cssH: number): Promise<Blob> {
  const jpeg = new Uint8Array(await (await toBlob(canvas, 'image/jpeg', 0.95)).arrayBuffer())
  const W = +(cssW * 0.75).toFixed(2), H = +(cssH * 0.75).toFixed(2) // points
  const enc = new TextEncoder()
  const parts: Uint8Array[] = []
  const offsets: number[] = []
  let length = 0
  const push = (p: Uint8Array | string) => { const b = typeof p === 'string' ? enc.encode(p) : p; parts.push(b); length += b.length }
  const obj = (n: number, body: (Uint8Array | string)[]) => { offsets[n] = length; push(`${n} 0 obj\n`); body.forEach(push); push('\nendobj\n') }
  const content = `q ${W} 0 0 ${H} 0 0 cm /Im0 Do Q`
  push('%PDF-1.4\n%\xE2\xE3\xCF\xD3\n')
  obj(1, ['<< /Type /Catalog /Pages 2 0 R >>'])
  obj(2, ['<< /Type /Pages /Kids [3 0 R] /Count 1 >>'])
  obj(3, [`<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ${W} ${H}] /Resources << /XObject << /Im0 4 0 R >> >> /Contents 5 0 R >>`])
  obj(4, [`<< /Type /XObject /Subtype /Image /Width ${canvas.width} /Height ${canvas.height} /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /DCTDecode /Length ${jpeg.length} >>\nstream\n`, jpeg, '\nendstream'])
  obj(5, [`<< /Length ${content.length} >>\nstream\n${content}\nendstream`])
  const xref = length
  push(`xref\n0 6\n0000000000 65535 f \n${offsets.slice(1).map((o) => `${String(o).padStart(10, '0')} 00000 n \n`).join('')}`)
  push(`trailer\n<< /Size 6 /Root 1 0 R >>\nstartxref\n${xref}\n%%EOF\n`)
  return new Blob(parts as BlobPart[], { type: 'application/pdf' })
}

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms))
const nextFrame = () => new Promise((r) => requestAnimationFrame(() => r(null)))

export function backgroundColor(mode: Background, format: ImageFormat): { background: string | null; light: boolean } {
  const alpha = FORMATS.find((f) => f.value === format)?.alpha
  if (mode === 'transparent' && alpha) return { background: null, light: false }
  if (mode === 'theme') return { background: cssVar('--surface-color', '#ffffff'), light: false }
  return { background: '#ffffff', light: isDarkTheme() }
}

/** Render the figure and return the file (also used by tests through the dialog). */
export async function renderImage(src: ImageSource, req: ExportRequest): Promise<Blob> {
  const { background, light } = backgroundColor(req.backgroundMode, req.format)
  const o: RenderOptions = { scale: req.scale, background, light, legend: req.legend, area: req.area, plain: req.plain }
  const store = useImageExport.getState()
  if (req.plain && src.features.selection) {
    store.setPlain(true)
    await nextFrame(); await nextFrame(); await sleep(350) // let views redraw without highlighting
  }
  try {
    if (req.format === 'svg') return new Blob([await src.svg(o)], { type: 'image/svg+xml' })
    const { w, h } = src.size(req.area)
    let canvas: HTMLCanvasElement
    if (src.raster) canvas = await src.raster(o)
    else canvas = await rasterise(await src.svg(o), w, h, req.scale, background)
    const solid = background ?? '#ffffff'
    if (req.format === 'pdf') return pdfFromCanvas(flatten(canvas, solid), w, h)
    if (req.format === 'jpeg') return toBlob(flatten(canvas, solid), 'image/jpeg', 0.95)
    return toBlob(canvas, req.format === 'webp' ? 'image/webp' : 'image/png', 0.95)
  } finally {
    if (req.plain && src.features.selection) store.setPlain(false)
  }
}

export async function exportImage(src: ImageSource, req: ExportRequest) {
  const blob = await renderImage(src, req)
  const ext = FORMATS.find((f) => f.value === req.format)!.ext
  const base = (req.filename.trim() || src.name || 'flame_figure').replace(/[\\/:*?"<>|]+/g, '_').replace(/\.(png|svg|pdf|jpe?g|webp)$/i, '')
  saveBlob(blob, `${base}.${ext}`)
}

/** Readable size line for the dialog. */
export function describeSize(w: number, h: number, scale: number) {
  const W = Math.round(w * scale), H = Math.round(h * scale)
  const n = (x: number) => x.toLocaleString('en-US')
  const inch = (px: number, dpi: number) => px / dpi
  const f = (x: number) => (x >= 10 ? x.toFixed(0) : x.toFixed(1))
  const cm = (x: number) => (x * 2.54).toFixed(1)
  const sw = inch(W, 96 * scale), sh = inch(H, 96 * scale), pw = inch(W, 300), ph = inch(H, 300)
  return `${n(W)} × ${n(H)} px · ${96 * scale} dpi when printed at the on-screen size, ${f(sw)} × ${f(sh)} in (${cm(sw)} × ${cm(sh)} cm) · ` +
    `${f(pw)} × ${f(ph)} in (${cm(pw)} × ${cm(ph)} cm) at 300 dpi`
}
