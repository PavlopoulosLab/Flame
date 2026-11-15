import Plotly from 'plotly.js-dist-min'
import { useEffect, useRef, useState } from 'react'
import { DataSet } from 'vis-data/peer'
import { Network } from 'vis-network/peer'
import 'vis-network/styles/vis-network.css'
import { DEFAULT_STYLE, visOptions, type NetworkStyle } from '../lib/networkStyle'
import { BrushModes, pointInPolygon, useBrush, type BrushMode } from '../lib/brush'
import { categorical, usePaletteMode, type PaletteMode } from '../lib/palette'

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
  toImage: (format: 'png' | 'svg', filename: string) => void
}

/**
 * Plotly chart. `onSelect` receives the customdata of clicked or lasso/box
 * selected points (we put item ids in customdata).
 */
export function Plot({
  data,
  layout,
  onSelect,
  height = 480,
  handleRef,
  dragmode,
  onShapesMoved,
}: {
  data: PlotData
  layout?: Partial<Plotly.Layout>
  onSelect?: (ids: string[], additive: boolean) => void
  height?: number
  handleRef?: { current: PlotHandle | null }
  dragmode?: 'zoom' | 'select' | 'lasso' | 'pan'
  /** Makes layout shapes draggable; receives Plotly's relayout update (e.g. "shapes[0].y0"). */
  onShapesMoved?: (update: Record<string, number>) => void
}) {
  const ref = useRef<HTMLDivElement>(null)
  const selectRef = useRef(onSelect)
  selectRef.current = onSelect
  const shapesRef = useRef(onShapesMoved)
  shapesRef.current = onShapesMoved
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
    const merged: Partial<Plotly.Layout> = {
      ...base,
      height,
      margin: { l: 60, r: 20, t: 20, b: 50 },
      dragmode: mode,
      ...layout,
      xaxis: { ...base.xaxis, ...(layout?.xaxis ?? {}) },
      yaxis: { ...base.yaxis, ...(layout?.yaxis ?? {}) },
    }
    redrawing.current = true
    Promise.resolve(
      Plotly.react(el, data as Plotly.Data[], merged, {
        responsive: true,
        displaylogo: false,
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
      selectRef.current?.(pick(ev), !!(native && (native.shiftKey || native.metaKey || native.ctrlKey)))
    })
    gd.on('plotly_selected', (ev: Plotly.PlotSelectionEvent) => {
      const ids = pick(ev)
      if (redrawing.current || !ids.length) return
      selectRef.current?.(ids, false)
    })
    if (handleRef) {
      handleRef.current = {
        toImage: (format, filename) => {
          Plotly.downloadImage(el, { format, filename, width: el.clientWidth, height: el.clientHeight })
        },
      }
    }
  }, [data, layout, height, mode, handleRef, cb])

  useEffect(() => {
    const el = ref.current
    return () => {
      if (el) Plotly.purge(el)
    }
  }, [])

  return (
    <>
      {selectable && (
        <div className="plot-modes" role="radiogroup" aria-label="Mouse mode">
          {([['select', '▭ Box select'], ['lasso', '◌ Lasso'], ['zoom', '⌕ Zoom'], ['pan', '✥ Pan']] as const).map(([m, l]) => (
            <button key={m} type="button" role="radio" aria-checked={mode === m} className={mode === m ? 'on' : ''} onClick={() => setMode(m)}
              title={m === 'select' || m === 'lasso' ? 'Drag to select (Shift-click adds single items); double-click to clear' : m === 'zoom' ? 'Drag to zoom; double-click to reset' : 'Drag to move'}>{l}</button>
          ))}
        </div>
      )}
      <div ref={ref} className="plot" />
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
}: {
  nodes: GraphNode[]
  edges: GraphEdge[]
  selected?: string[]
  onSelect?: (ids: string[], additive: boolean) => void
  height?: number
  handleRef?: { current: NetworkHandle | null }
  style?: NetworkStyle
}) {
  const ref = useRef<HTMLDivElement>(null)
  const netRef = useRef<Network | null>(null)
  const selectRef = useRef(onSelect)
  selectRef.current = onSelect
  const styleRef = useRef(style)
  const engineRef = useRef(style.engine)
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
      {onSelect && <BrushModes mode={brushMode} onChange={setBrushMode} />}
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
