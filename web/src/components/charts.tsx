import Plotly from 'plotly.js-dist-min'
import { useEffect, useRef, useState } from 'react'
import { DataSet } from 'vis-data/peer'
import { Network } from 'vis-network/peer'
import 'vis-network/styles/vis-network.css'
import { DEFAULT_STYLE, visOptions, type NetworkStyle } from '../lib/networkStyle'
import { BrushModes, pointInPolygon, useBrush, type BrushMode } from '../lib/brush'
import { categorical, usePaletteMode, type PaletteMode } from '../lib/palette'
import { ExportImageButton } from './ExportImage'
import { ChartTextButton } from './ChartTextControls'
import { applyChartText, useChartText } from '../lib/chartText'
import { LIGHT, openImageExport, type ImageSource, type RenderOptions } from '../lib/imageExport'
import { plotlySafeDeep } from '../lib/format'

type PlotData = Partial<Plotly.PlotData>[]

function themeLayout(pm: PaletteMode = 'flame'): Partial<Plotly.Layout> {
  const css = getComputedStyle(document.documentElement)
  const fg = css.getPropertyValue('--default-color').trim() || '#3f3f46'
  const grid = css.getPropertyValue('--border-color').trim() || '#e4e4e7'
  return {
    paper_bgcolor: 'rgba(0,0,0,0)',
    plot_bgcolor: 'rgba(0,0,0,0)',
    font: { family: 'Inter, system-ui, sans-serif', size: 12, color: fg },
    xaxis: { gridcolor: grid, zerolinecolor: grid },
    yaxis: { gridcolor: grid, zerolinecolor: grid },
    hoverlabel: { font: { family: 'Inter, system-ui, sans-serif' } },
    colorway: categorical(pm),
  }
}

export interface PlotHandle {
  /** Open the export dialog for this chart. */
  exportImage: (name?: string) => void
}

type Gd = HTMLElement & { data: Record<string, unknown>[]; layout: Record<string, unknown>; _fullData?: Record<string, unknown>[]; _fullLayout?: Record<string, unknown> }

/** Shapes and annotations whose `name` starts with "legend" belong to a hand-drawn legend. */
const isLegendItem = (x: unknown) => String((x as { name?: string })?.name ?? '').startsWith('legend')

function plotHasLegend(gd: Gd) {
  const full = gd._fullData ?? []
  return !!gd._fullLayout?.showlegend || full.some((t) => t.showscale || (t.marker as { showscale?: boolean } | undefined)?.showscale)
    || ((gd.layout.shapes as unknown[] | undefined) ?? []).some(isLegendItem)
}

/** The chart as an export figure: background, light text on white, legend on/off. */
function exportFigure(gd: Gd, o: RenderOptions) {
  const data = JSON.parse(JSON.stringify(gd.data)) as Record<string, unknown>[]
  const layout = JSON.parse(JSON.stringify(gd.layout)) as Record<string, unknown>
  delete layout.selections // the drag rectangle / lasso outline
  layout.paper_bgcolor = o.background ?? 'rgba(0,0,0,0)'
  layout.plot_bgcolor = 'rgba(0,0,0,0)'
  if (o.light) {
    layout.font = { ...(layout.font as object), color: LIGHT.text }
    Object.keys(layout).filter((k) => /^[xy]axis\d*$/.test(k)).forEach((k) => {
      layout[k] = { ...(layout[k] as object), gridcolor: LIGHT.grid, zerolinecolor: LIGHT.grid, linecolor: LIGHT.grid }
    })
  }
  if (!o.legend) {
    layout.showlegend = false
    data.forEach((t) => { t.showscale = false; if (t.marker) (t.marker as Record<string, unknown>).showscale = false })
    if (Array.isArray(layout.shapes)) layout.shapes = layout.shapes.filter((x) => !isLegendItem(x))
    if (Array.isArray(layout.annotations)) layout.annotations = layout.annotations.filter((x) => !isLegendItem(x))
  }
  return { data, layout }
}

/** Export source for a Plotly chart element. */
export function plotSource(el: HTMLElement, name: string, selectable: boolean): ImageSource {
  const gd = el as Gd
  return {
    name,
    size: () => ({ w: gd.clientWidth, h: gd.clientHeight }),
    features: { legend: plotHasLegend(gd), selection: selectable },
    svg: async (o) => {
      const url = await Plotly.toImage(exportFigure(gd, o) as never, { format: 'svg', width: gd.clientWidth, height: gd.clientHeight })
      return decodeURIComponent(url.replace(/^data:image\/svg\+xml,/, ''))
    },
  }
}

/**
 * Plotly chart. `onSelect` receives the customdata of clicked or lasso/box
 * selected points (we put item ids in customdata).
 */
export function Plot({
  data,
  layout,
  onSelect,
  onPointClick,
  height = 480,
  handleRef,
  dragmode,
  onShapesMoved,
  exportName = 'flame_plot',
  tools = true,
  textButton = true,
}: {
  data: PlotData
  layout?: Partial<Plotly.Layout>
  onSelect?: (ids: string[], additive: boolean) => void
  /** When set, a plain click on a point calls this instead of onSelect (Shift/Ctrl-click still adds to the selection). */
  onPointClick?: (id: string) => void
  height?: number
  handleRef?: { current: PlotHandle | null }
  dragmode?: 'zoom' | 'select' | 'lasso' | 'pan'
  /** Makes layout shapes draggable; receives Plotly's relayout update (e.g. "shapes[0].y0"). */
  onShapesMoved?: (update: Record<string, number>) => void
  /** Default file name when the chart is saved as an image. */
  exportName?: string
  /** false: no box/lasso/zoom/pan controls — the chart is only clicked (e.g. a bar chart of genes). */
  tools?: boolean
  /** false: no "Aa Text" button (the page has its own text controls); the text style still applies. */
  textButton?: boolean
}) {
  const ref = useRef<HTMLDivElement>(null)
  const text = useChartText()
  const selectRef = useRef(onSelect)
  selectRef.current = onSelect
  const clickRef = useRef(onPointClick)
  clickRef.current = onPointClick
  const shapesRef = useRef(onShapesMoved)
  shapesRef.current = onShapesMoved
  const nameRef = useRef(exportName)
  nameRef.current = exportName
  // Box / lasso selection works on point-like traces (scatter, bar, box, histogram).
  const selectable = !!onSelect && data.some((t) => ['scatter', 'scattergl', 'bar', 'box', 'histogram', 'violin', undefined].includes((t as { type?: string }).type)
    && !((t as { mode?: string }).mode ?? '').startsWith('lines') || (t as { mode?: string }).mode === 'lines+markers')
  const initial = dragmode ?? (selectable ? 'select' : 'zoom')
  const [mode, setMode] = useState<'zoom' | 'select' | 'lasso' | 'pan'>(initial)
  useEffect(() => setMode(initial), [initial])
  // Plotly.react() (e.g. when the selection highlight changes) clears the
  // drag-selection and emits an empty "selected" event; ignore events we cause.
  const redrawing = useRef(false)
  const cb = usePaletteMode()

  useEffect(() => {
    const el = ref.current
    if (!el) return
    const base = themeLayout(cb)
    // The text style of all charts (font, title, axis, tick and legend text) over the chart's own.
    const merged = applyChartText({
      ...base,
      height,
      margin: { l: 60, r: 20, t: 20, b: 50 },
      dragmode: tools ? mode : false,
      ...layout,
      xaxis: { ...base.xaxis, ...(layout?.xaxis ?? {}) },
      yaxis: { ...base.yaxis, ...(layout?.yaxis ?? {}) },
    } as Record<string, unknown>, text) as Partial<Plotly.Layout>
    redrawing.current = true
    Promise.resolve(
      Plotly.react(el, plotlySafeDeep(data) as Plotly.Data[], plotlySafeDeep(merged), {
        responsive: true,
        displaylogo: false,
        ...(tools ? {} : { displayModeBar: false }),
        ...(onShapesMoved ? { edits: { shapePosition: true } } : {}),
        modeBarButtonsToRemove: ['toImage'],
      }),
    ).finally(() => setTimeout(() => (redrawing.current = false), 50))
    const gd = el as unknown as Plotly.PlotlyHTMLElement
    const pick = (ev: Plotly.PlotMouseEvent | Plotly.PlotSelectionEvent | undefined) =>
      (ev?.points ?? []).map((p) => String((p as { customdata?: unknown }).customdata ?? '')).filter(Boolean)
    gd.removeAllListeners?.('plotly_click')
    gd.removeAllListeners?.('plotly_selected')
    gd.removeAllListeners?.('plotly_relayout')
    ;(gd.on as unknown as (event: string, cb: (ev: Record<string, unknown>) => void) => void).call(gd, 'plotly_relayout', (ev: Record<string, unknown>) => {
      const moved = Object.fromEntries(Object.entries(ev).filter(([k, v]) => k.startsWith('shapes[') && typeof v === 'number')) as Record<string, number>
      if (Object.keys(moved).length) shapesRef.current?.(moved)
    })
    gd.on('plotly_click', (ev: Plotly.PlotMouseEvent) => {
      const native = (ev as unknown as { event?: MouseEvent }).event
      const additive = !!(native && (native.shiftKey || native.metaKey || native.ctrlKey))
      const ids = pick(ev)
      if (clickRef.current && !additive) { if (ids[0]) clickRef.current(ids[0]); return }
      selectRef.current?.(ids, additive)
    })
    gd.on('plotly_selected', (ev: Plotly.PlotSelectionEvent) => {
      const ids = pick(ev)
      if (redrawing.current || !ids.length) return
      selectRef.current?.(ids, false)
    })
    if (handleRef) {
      handleRef.current = { exportImage: (name) => openImageExport(plotSource(el, name ?? nameRef.current, !!selectRef.current)) }
    }
  }, [data, layout, height, mode, handleRef, cb, tools, text])

  useEffect(() => {
    const el = ref.current
    return () => {
      if (el) Plotly.purge(el)
    }
  }, [])

  return (
    <>
      <div className="plot-tools">
        {selectable && tools && (
          <div className="plot-modes" role="radiogroup" aria-label="Mouse mode">
            {([['select', '▭ Box select'], ['lasso', '◌ Lasso'], ['zoom', '⌕ Zoom'], ['pan', '✥ Pan']] as const).map(([m, l]) => (
              <button key={m} type="button" role="radio" aria-checked={mode === m} className={mode === m ? 'on' : ''} onClick={() => setMode(m)}
                title={m === 'select' || m === 'lasso' ? 'Drag to select (Shift-click adds single items); double-click to clear' : m === 'zoom' ? 'Drag to zoom; double-click to reset' : 'Drag to move'}>{l}</button>
            ))}
          </div>
        )}
        {textButton && <ChartTextButton />}
        <ExportImageButton source={() => (ref.current ? plotSource(ref.current, exportName, !!onSelect) : null)} />
      </div>
      {/* reserve the plot height so following content never slides under the chart */}
      <div ref={ref} className="plot" style={{ minHeight: height }} />
    </>
  )
}

export interface NetworkHandle {
  toPng: (filename: string) => void
  fit: () => void
  /** vis-network clustering: collapse hubs or leaf nodes into cluster nodes, or open them again. */
  clusterHubs: () => number
  clusterOutliers: () => number
  openClusters: () => void
  /** Start or stop the physics simulation. */
  setPhysicsRunning: (on: boolean) => void
  /** Open the export dialog for this network. */
  exportImage: (name?: string) => void
}

// vis-network internals used to redraw a network as SVG.
interface VisNode { id: string; x: number; y: number; selected?: boolean; labelModule?: { size?: { left: number; top: number; width: number; height: number } }; options: { shape: string; size: number; label?: string; hidden?: boolean; borderWidth?: number;
  color: { background: string; border: string; highlight?: { border?: string } }; font: { color: string; size: number; face?: string; strokeWidth?: number; strokeColor?: string } } }
interface VisEdge { id: string; from: VisNode; to: VisNode; selected?: boolean; options: { width: number; hidden?: boolean; color: { color?: string; highlight?: string; opacity?: number; inherit?: string | boolean } };
  edgeType?: { via?: { x: number; y: number }; _getViaCoordinates?: () => { x?: number; y?: number } } }

const LABEL_INSIDE = new Set(['box', 'circle', 'ellipse', 'database', 'text'])

/** Rebuild a vis-network as SVG markup from its node and edge positions. */
function networkSvg(net: Network, el: HTMLElement, o: RenderOptions, w: number, h: number) {
  const body = (net as unknown as { body: { nodes: Record<string, VisNode>; edges: Record<string, VisEdge>; nodeIndices: string[]; edgeIndices: string[] } }).body
  const nodes = body.nodeIndices.map((id) => body.nodes[id]).filter((n) => n && !n.options.hidden)
  const edges = body.edgeIndices.map((id) => body.edges[id]).filter((e) => e && !e.options.hidden && e.from && e.to)
  const e2 = (s: string) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')
  let vb: { x: number; y: number; w: number; h: number }
  if (o.area === 'view') {
    const a = net.DOMtoCanvas({ x: 0, y: 0 }), b = net.DOMtoCanvas({ x: el.clientWidth, y: el.clientHeight })
    vb = { x: a.x, y: a.y, w: b.x - a.x, h: b.y - a.y }
  } else {
    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity
    nodes.forEach((n) => {
      const bb = net.getBoundingBox(n.id)
      x0 = Math.min(x0, bb.left); x1 = Math.max(x1, bb.right); y0 = Math.min(y0, bb.top); y1 = Math.max(y1, bb.bottom)
      const ls = n.options.label ? n.labelModule?.size : undefined // where vis drew the label
      if (ls && ls.width) { x0 = Math.min(x0, ls.left); x1 = Math.max(x1, ls.left + ls.width); y0 = Math.min(y0, ls.top); y1 = Math.max(y1, ls.top + ls.height) }
    })
    if (!isFinite(x0)) { x0 = 0; y0 = 0; x1 = w; y1 = h }
    const pad = 20
    vb = { x: x0 - pad, y: y0 - pad, w: x1 - x0 + 2 * pad, h: y1 - y0 + 2 * pad }
  }
  const textColor = (c: string) => (o.light ? LIGHT.heading : c)
  const out: string[] = []
  out.push(`<svg xmlns="http://www.w3.org/2000/svg" width="${w}" height="${h}" viewBox="${vb.x} ${vb.y} ${vb.w} ${vb.h}" preserveAspectRatio="xMidYMid meet" font-family="Inter, system-ui, sans-serif">`)
  if (o.background) out.push(`<rect x="${vb.x - vb.w}" y="${vb.y - vb.h}" width="${vb.w * 3}" height="${vb.h * 3}" fill="${e2(o.background)}"/>`)
  out.push('<g fill="none" stroke-linecap="round">')
  edges.forEach((e) => {
    const c = e.options.color
    const color = e.selected && c.highlight ? c.highlight : (c.inherit && !c.color ? e.from.options.color.border : c.color) ?? '#9ca3af'
    let via: { x?: number; y?: number } | undefined
    try { via = e.edgeType?.via ?? e.edgeType?._getViaCoordinates?.() } catch { via = undefined }
    const d = via && via.x !== undefined && via.y !== undefined
      ? `M${e.from.x},${e.from.y} Q${via.x},${via.y} ${e.to.x},${e.to.y}` : `M${e.from.x},${e.from.y} L${e.to.x},${e.to.y}`
    out.push(`<path d="${d}" stroke="${e2(color)}" stroke-width="${e.options.width ?? 1}" stroke-opacity="${c.opacity ?? 1}"/>`)
  })
  out.push('</g><g>')
  nodes.forEach((n) => {
    const { shape, size, color } = n.options
    const stroke = n.selected && color.highlight?.border ? color.highlight.border : color.border
    const sw = n.selected ? 3 : (n.options.borderWidth ?? 1)
    const paint = `fill="${e2(color.background)}" stroke="${e2(stroke)}" stroke-width="${sw}"`
    const bb = net.getBoundingBox(n.id)
    if (LABEL_INSIDE.has(shape) && shape !== 'circle') {
      if (shape !== 'text') out.push(`<rect x="${bb.left}" y="${bb.top}" width="${bb.right - bb.left}" height="${bb.bottom - bb.top}" rx="${shape === 'box' ? 4 : (bb.bottom - bb.top) / 2}" ${paint}/>`)
    } else if (shape === 'square') {
      out.push(`<rect x="${n.x - size}" y="${n.y - size}" width="${size * 2}" height="${size * 2}" ${paint}/>`)
    } else if (shape === 'diamond') {
      out.push(`<polygon points="${n.x},${n.y - size} ${n.x + size},${n.y} ${n.x},${n.y + size} ${n.x - size},${n.y}" ${paint}/>`)
    } else if (shape === 'triangle' || shape === 'triangleDown') {
      const k = shape === 'triangle' ? 1 : -1
      out.push(`<polygon points="${n.x},${n.y - k * size} ${n.x + size},${n.y + k * size * 0.6} ${n.x - size},${n.y + k * size * 0.6}" ${paint}/>`)
    } else if (shape === 'hexagon' || shape === 'star') {
      const k = shape === 'hexagon' ? 6 : 10
      const pts = Array.from({ length: k }, (_, i) => {
        const a = (Math.PI * 2 * i) / k - Math.PI / 2
        const rr = shape === 'star' && i % 2 ? size * 0.45 : size
        return `${n.x + rr * Math.cos(a)},${n.y + rr * Math.sin(a)}`
      }).join(' ')
      out.push(`<polygon points="${pts}" ${paint}/>`)
    } else {
      const r = shape === 'circle' ? (bb.right - bb.left) / 2 : size
      out.push(`<circle cx="${n.x}" cy="${n.y}" r="${r}" ${paint}/>`)
    }
    if (n.options.label) {
      const f = n.options.font
      const inside = LABEL_INSIDE.has(shape)
      const ls = n.labelModule?.size
      const y = inside ? n.y : ls && ls.height ? ls.top + f.size * 0.6 : bb.bottom + f.size * 0.9
      const outline = f.strokeWidth ? ` stroke="${e2(o.light ? '#ffffff' : f.strokeColor ?? '#ffffff')}" stroke-width="${f.strokeWidth}" paint-order="stroke"` : ''
      n.options.label.split('\n').forEach((line, i, all) => {
        out.push(`<text x="${n.x}" y="${y + (i - (inside ? (all.length - 1) / 2 : 0)) * f.size * 1.15}" font-size="${f.size}" fill="${e2(textColor(f.color))}" text-anchor="middle" dominant-baseline="central"${outline}>${e2(line)}</text>`)
      })
    }
  })
  out.push('</g></svg>')
  return out.join('')
}

export interface GraphNode {
  id: string
  label: string
  title?: string
  color?: string
  shape?: string
  x?: number
  y?: number
  size?: number
}
export interface GraphEdge {
  id: string | number
  from: string
  to: string
  width?: number
  /** Edge weight; used for width when the style scales edges by weight. */
  value?: number
  title?: string
  color?: string
  /** Per-edge curve (vis-network `smooth`), e.g. to draw parallel edges of a multi-edge network. */
  smooth?: false | { enabled: boolean; type: string; roundness: number }
}

const themeColors = () => {
  const css = getComputedStyle(document.documentElement)
  return { fg: css.getPropertyValue('--heading-color').trim() || '#18181b', bg: css.getPropertyValue('--surface-color').trim() || '#ffffff' }
}

/** Node/edge records for vis-network under a style; `positions` keeps the current layout. */
function graphData(nodes: GraphNode[], edges: GraphEdge[], style: NetworkStyle, positions?: Record<string, { x: number; y: number }> | null) {
  const degree = new Map<string, number>()
  edges.forEach((e) => { degree.set(e.from, (degree.get(e.from) ?? 0) + 1); degree.set(e.to, (degree.get(e.to) ?? 0) + 1) })
  const useCoords = style.engine === 'precomputed' || (style.engine !== 'hierarchical')
  // vis-network renders string titles as text (no HTML), which keeps external names inert.
  const n = nodes.map((nd) => {
    const p = positions?.[nd.id]
    const out: Record<string, unknown> = {
      id: nd.id, label: style.labels ? nd.label : undefined, title: nd.title ? String(nd.title) : nd.label, color: nd.color,
      shape: style.shape === 'auto' ? nd.shape ?? 'dot' : style.shape,
    }
    if (style.sizeBy === 'degree') out.value = degree.get(nd.id) ?? 0
    else if (nd.size) out.size = nd.size
    if (p) { out.x = p.x; out.y = p.y } else if (useCoords && nd.x !== undefined && nd.y !== undefined && style.engine !== 'hierarchical') { out.x = nd.x; out.y = nd.y }
    return out
  })
  const e = edges.map((ed) => {
    const out: Record<string, unknown> = { id: ed.id, from: ed.from, to: ed.to, title: ed.title ? String(ed.title) : undefined }
    if (ed.color) out.color = { color: ed.color, highlight: '#f97316', hover: '#8b5cf6', opacity: style.edgeOpacity }
    if (style.edgeWidth === 'weight' && ed.value !== undefined) out.value = ed.value
    else if (style.edgeWidth === 'auto' && ed.width) out.width = ed.width
    else out.width = 1
    if (ed.smooth !== undefined) out.smooth = ed.smooth
    return out
  })
  return { nodes: new DataSet(n as never[]), edges: new DataSet(e as never[]) }
}

/**
 * vis-network graph. Layout comes from precomputed coordinates or from one of
 * vis-network's engines (physics solvers or hierarchical), per `style`.
 * Style changes are applied in place; the current layout is kept unless the
 * layout engine changes.
 */
export function NetworkGraph({
  nodes,
  edges,
  selected,
  onSelect,
  height = 560,
  handleRef,
  style = DEFAULT_STYLE,
  exportName = 'flame_network',
}: {
  nodes: GraphNode[]
  edges: GraphEdge[]
  selected?: string[]
  onSelect?: (ids: string[], additive: boolean) => void
  height?: number
  handleRef?: { current: NetworkHandle | null }
  style?: NetworkStyle
  /** Default file name when the network is saved as an image. */
  exportName?: string
}) {
  const ref = useRef<HTMLDivElement>(null)
  const netRef = useRef<Network | null>(null)
  const selectRef = useRef(onSelect)
  selectRef.current = onSelect
  const styleRef = useRef(style)
  const engineRef = useRef(style.engine)
  const nameRef = useRef(exportName)
  nameRef.current = exportName
  // Export source: the canvas redrawn at the chosen resolution, or the network rebuilt as SVG.
  const sourceRef = useRef((name?: string): ImageSource | null => {
    if (!netRef.current || !ref.current) return null
    // Look the network up when saving: it is rebuilt whenever its data changes.
    const current = () => {
      const net = netRef.current, el = ref.current
      if (!net || !el) throw new Error('The network is no longer shown.')
      return { net, el }
    }
    const withExportState = <T,>(o: RenderOptions, fn: (net: Network, el: HTMLDivElement) => T): T => {
      const { net, el } = current()
      const sel = { nodes: net.getSelectedNodes(), edges: net.getSelectedEdges() }
      if (o.plain) net.unselectAll()
      const { fg, bg } = themeColors()
      if (o.light) net.setOptions({ nodes: { font: { color: LIGHT.heading, strokeColor: '#ffffff' } } })
      try { return fn(net, el) } finally {
        if (o.light) net.setOptions({ nodes: { font: visOptions(styleRef.current, fg, bg).nodes?.font as never } })
        if (o.plain) { try { net.setSelection(sel, { unselectAll: true, highlightEdges: false }) } catch { /* ids gone */ } }
        net.redraw()
      }
    }
    return {
      name: name ?? nameRef.current,
      size: () => { const el = ref.current; return { w: el?.clientWidth ?? 0, h: el?.clientHeight ?? 0 } },
      features: { area: true, selection: !!selectRef.current },
      svg: (o) => withExportState(o, (net, el) => networkSvg(net, el, o, el.clientWidth, el.clientHeight)),
      raster: (o) => withExportState(o, (net, el) => {
        const W = el.clientWidth, H = el.clientHeight, k = o.scale
        const view = { position: net.getViewPosition(), scale: net.getScale() }
        const prev = { w: el.style.width, h: el.style.height }
        el.style.width = `${W * k}px`; el.style.height = `${H * k}px`
        net.setSize(`${W * k}px`, `${H * k}px`)
        if (o.area === 'all') net.fit({ animation: false })
        else net.moveTo({ position: view.position, scale: view.scale * k, animation: false })
        net.redraw()
        const out = document.createElement('canvas')
        out.width = W * k; out.height = H * k
        const ctx = out.getContext('2d')!
        if (o.background) { ctx.fillStyle = o.background; ctx.fillRect(0, 0, out.width, out.height) }
        const c = el.querySelector('canvas')
        if (c) ctx.drawImage(c, 0, 0, out.width, out.height)
        el.style.width = prev.w; el.style.height = prev.h
        net.setSize(prev.w || '100%', prev.h || `${H}px`)
        net.moveTo({ position: view.position, scale: view.scale, animation: false })
        return out
      }),
    }
  })
  const [brushMode, setBrushMode] = useState<BrushMode>('click')
  const brush = useBrush(brushMode, (x, y) => {
    const r = ref.current!.getBoundingClientRect()
    return [x - r.left, y - r.top]
  }, (poly, additive) => {
    const net = netRef.current
    if (!net) return
    const pos = net.getPositions()
    const ids = Object.keys(pos).filter((id) => !net.isCluster(id) && pointInPolygon((({ x, y }) => [x, y] as [number, number])(net.canvasToDOM(pos[id])), poly))
    try { net.selectNodes(ids, false) } catch { /* ignore */ }
    selectRef.current?.(ids, additive)
  })

  useEffect(() => {
    if (!ref.current) return
    const st = styleRef.current
    const { fg, bg } = themeColors()
    const net = new Network(ref.current, graphData(nodes, edges, st), visOptions(st, fg, bg))
    netRef.current = net
    engineRef.current = st.engine
    net.on('click', (p: { nodes: string[]; event?: { srcEvent?: MouseEvent } }) => {
      const src = p.event?.srcEvent
      selectRef.current?.(p.nodes.map(String).filter((id) => !net.isCluster(id)), !!(src && (src.shiftKey || src.metaKey || src.ctrlKey)))
    })
    net.on('doubleClick', (p: { nodes: string[] }) => {
      const id = p.nodes[0]
      if (id !== undefined && net.isCluster(id)) net.openCluster(id)
    })
    net.on('stabilizationIterationsDone', () => {
      if (styleRef.current.freeze) net.setOptions({ physics: { enabled: false } })
    })
    net.once('afterDrawing', () => net.fit())
    if (handleRef) {
      let clusterSeq = 0
      const clusterNode = (label: string) => ({ shape: 'database', color: '#8b5cf6', font: { color: themeColors().fg }, label, borderWidth: 2 })
      handleRef.current = {
        fit: () => net.fit({ animation: { duration: 400, easingFunction: 'easeInOutQuad' } }),
        toPng: (filename) => {
          const canvas = ref.current?.querySelector('canvas')
          if (!canvas) return
          canvas.toBlob((blob) => {
            if (!blob) return
            const a = document.createElement('a')
            a.href = URL.createObjectURL(blob)
            a.download = filename.endsWith('.png') ? filename : `${filename}.png`
            a.click()
            setTimeout(() => URL.revokeObjectURL(a.href), 1000)
          })
        },
        clusterHubs: () => {
          const before = Object.keys(net.getPositions()).length
          net.clusterByHubsize(undefined, {
            processProperties: (cluster: Record<string, unknown>, nodesIn: unknown[]) => ({ ...cluster, ...clusterNode(`${nodesIn.length} nodes`), id: `cluster:hub:${++clusterSeq}` }),
          } as never)
          return before - Object.keys(net.getPositions()).length
        },
        clusterOutliers: () => {
          const before = Object.keys(net.getPositions()).length
          net.clusterOutliers({
            processProperties: (cluster: Record<string, unknown>, nodesIn: unknown[]) => ({ ...cluster, ...clusterNode(`+${nodesIn.length - 1}`), id: `cluster:leaf:${++clusterSeq}` }),
          } as never)
          return before - Object.keys(net.getPositions()).length
        },
        openClusters: () => {
          for (let i = 0; i < 20; i++) {
            const ids = Object.keys(net.getPositions()).filter((id) => net.isCluster(id))
            if (!ids.length) break
            ids.forEach((id) => net.openCluster(id))
          }
        },
        exportImage: (name) => { const s = sourceRef.current(name); if (s) openImageExport(s) },
        setPhysicsRunning: (on) => {
          if (styleRef.current.engine === 'precomputed' || styleRef.current.engine === 'hierarchical') return
          net.setOptions({ physics: { enabled: on } })
          if (on) net.startSimulation(); else net.stopSimulation()
        },
      }
    }
    return () => {
      net.destroy()
      netRef.current = null
    }
  }, [nodes, edges, handleRef])

  // Apply style changes without rebuilding the network.
  useEffect(() => {
    styleRef.current = style
    const net = netRef.current
    if (!net) return
    const sameEngine = engineRef.current === style.engine
    const positions = sameEngine ? net.getPositions() : null
    const { fg, bg } = themeColors()
    net.setOptions(visOptions(style, fg, bg))
    net.setData(graphData(nodes, edges, style, positions))
    engineRef.current = style.engine
    if (!sameEngine) net.once('afterDrawing', () => net.fit())
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [style])

  useEffect(() => {
    const net = netRef.current
    if (!net) return
    try {
      net.selectNodes(selected ?? [], false)
    } catch {
      /* ids not in this graph */
    }
  }, [selected, nodes, style])

  return (
    <>
      <div className="plot-tools">
        {onSelect && <BrushModes mode={brushMode} onChange={setBrushMode} />}
        <ExportImageButton source={() => sourceRef.current()} />
      </div>
      <div className="network-wrap" style={{ height }}>
        <div ref={ref} className="network" style={{ height }} />
        {brushMode !== 'click' && (
          <svg className="brush-overlay" {...brush.handlers}>
            {brush.path && <path d={brush.path} />}
          </svg>
        )}
      </div>
    </>
  )
}
