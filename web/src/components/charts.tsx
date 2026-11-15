import Plotly from 'plotly.js-dist-min'
import { useEffect, useRef } from 'react'
import { DataSet } from 'vis-data/peer'
import { Network, type Options } from 'vis-network/peer'
import 'vis-network/styles/vis-network.css'

type PlotData = Partial<Plotly.PlotData>[]

function themeLayout(): Partial<Plotly.Layout> {
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
  dragmode = 'zoom',
}: {
  data: PlotData
  layout?: Partial<Plotly.Layout>
  onSelect?: (ids: string[], additive: boolean) => void
  height?: number
  handleRef?: { current: PlotHandle | null }
  dragmode?: 'zoom' | 'select' | 'lasso' | 'pan'
}) {
  const ref = useRef<HTMLDivElement>(null)
  const selectRef = useRef(onSelect)
  selectRef.current = onSelect

  useEffect(() => {
    const el = ref.current
    if (!el) return
    const base = themeLayout()
    const merged: Partial<Plotly.Layout> = {
      ...base,
      height,
      margin: { l: 60, r: 20, t: 20, b: 50 },
      dragmode,
      ...layout,
      xaxis: { ...base.xaxis, ...(layout?.xaxis ?? {}) },
      yaxis: { ...base.yaxis, ...(layout?.yaxis ?? {}) },
    }
    Plotly.react(el, data as Plotly.Data[], merged, {
      responsive: true,
      displaylogo: false,
      modeBarButtonsToRemove: ['toImage'],
    })
    const gd = el as unknown as Plotly.PlotlyHTMLElement
    const pick = (ev: Plotly.PlotMouseEvent | Plotly.PlotSelectionEvent | undefined) =>
      (ev?.points ?? []).map((p) => String((p as { customdata?: unknown }).customdata ?? '')).filter(Boolean)
    gd.removeAllListeners?.('plotly_click')
    gd.removeAllListeners?.('plotly_selected')
    gd.on('plotly_click', (ev: Plotly.PlotMouseEvent) => {
      const native = (ev as unknown as { event?: MouseEvent }).event
      selectRef.current?.(pick(ev), !!(native && (native.shiftKey || native.metaKey || native.ctrlKey)))
    })
    gd.on('plotly_selected', (ev: Plotly.PlotSelectionEvent) => {
      if (ev) selectRef.current?.(pick(ev), false)
    })
    if (handleRef) {
      handleRef.current = {
        toImage: (format, filename) => {
          Plotly.downloadImage(el, { format, filename, width: el.clientWidth, height: el.clientHeight })
        },
      }
    }
  }, [data, layout, height, dragmode, handleRef])

  useEffect(() => {
    const el = ref.current
    return () => {
      if (el) Plotly.purge(el)
    }
  }, [])

  return <div ref={ref} className="plot" />
}

export interface NetworkHandle {
  toPng: (filename: string) => void
  fit: () => void
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
  title?: string
  color?: string
}

/** vis-network graph using precomputed coordinates (physics off). Titles are plain text. */
export function NetworkGraph({
  nodes,
  edges,
  selected,
  onSelect,
  height = 560,
  handleRef,
  physics = false,
}: {
  nodes: GraphNode[]
  edges: GraphEdge[]
  selected?: string[]
  onSelect?: (ids: string[], additive: boolean) => void
  height?: number
  handleRef?: { current: NetworkHandle | null }
  physics?: boolean
}) {
  const ref = useRef<HTMLDivElement>(null)
  const netRef = useRef<Network | null>(null)
  const selectRef = useRef(onSelect)
  selectRef.current = onSelect

  useEffect(() => {
    if (!ref.current) return
    const css = getComputedStyle(document.documentElement)
    const fg = css.getPropertyValue('--heading-color').trim() || '#18181b'
    // vis-network renders string titles as text (no HTML), which keeps external names inert.
    const nodeSet = new DataSet(nodes.map((n) => ({ ...n, title: n.title ? String(n.title) : undefined, font: { color: fg, size: 14 } })))
    const edgeSet = new DataSet(edges.map((e) => ({ ...e, title: e.title ? String(e.title) : undefined })))
    const options: Options = {
      physics: physics ? { stabilization: { iterations: 200 } } : false,
      interaction: { hover: true, multiselect: true, navigationButtons: true, keyboard: false },
      nodes: { borderWidth: 1, size: 14 },
      edges: { color: { color: '#9ca3af', highlight: '#f97316' }, smooth: false },
      layout: { improvedLayout: physics },
    }
    const net = new Network(ref.current, { nodes: nodeSet, edges: edgeSet }, options)
    netRef.current = net
    net.on('click', (p: { nodes: string[]; event?: { srcEvent?: MouseEvent } }) => {
      const src = p.event?.srcEvent
      selectRef.current?.(p.nodes.map(String), !!(src && (src.shiftKey || src.metaKey || src.ctrlKey)))
    })
    net.once('afterDrawing', () => net.fit())
    if (handleRef) {
      handleRef.current = {
        fit: () => net.fit(),
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
      }
    }
    return () => {
      net.destroy()
      netRef.current = null
    }
  }, [nodes, edges, physics, handleRef])

  useEffect(() => {
    const net = netRef.current
    if (!net) return
    try {
      net.selectNodes(selected ?? [], false)
    } catch {
      /* ids not in this graph */
    }
  }, [selected, nodes])

  return <div ref={ref} className="network" style={{ height }} />
}
