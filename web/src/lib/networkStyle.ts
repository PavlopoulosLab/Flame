import { useState } from 'react'
import type { Options } from 'vis-network/peer'

/**
 * Display options for vis-network graphs: layout engine (precomputed, live
 * physics with one of vis-network's solvers, or hierarchical), node and edge
 * visuals, and interaction. Kept per browser session.
 */
export type NetworkEngine = 'precomputed' | 'barnesHut' | 'forceAtlas2Based' | 'repulsion' | 'hierarchicalRepulsion' | 'hierarchical'
export type EdgeCurve = 'straight' | 'dynamic' | 'continuous' | 'discrete' | 'curvedCW' | 'curvedCCW' | 'cubicBezier' | 'diagonalCross' | 'straightCross' | 'horizontal' | 'vertical'

export interface NetworkStyle {
  engine: NetworkEngine
  spacing: number               // 0.5–3: spring length / node distance multiplier
  freeze: boolean               // stop physics once the layout has settled
  direction: 'UD' | 'DU' | 'LR' | 'RL'
  sortMethod: 'hubsize' | 'directed'
  shape: 'auto' | 'dot' | 'ellipse' | 'circle' | 'box' | 'diamond' | 'star' | 'triangle' | 'triangleDown' | 'square' | 'hexagon' | 'database' | 'text'
  nodeSize: number              // base radius
  sizeBy: 'fixed' | 'degree'
  labels: boolean
  labelSize: number
  labelOutline: boolean
  curve: EdgeCurve
  edgeWidth: 'auto' | 'weight' | 'fixed'
  edgeOpacity: number           // 0.1–1
  dashes: boolean
  shadows: boolean
  navigation: boolean
  dragNodes: boolean
  hideEdgesOnDrag: boolean
}

export const ENGINES: { value: NetworkEngine; label: string; hint: string }[] = [
  { value: 'barnesHut', label: 'Force-directed (Barnes–Hut)', hint: 'Live physics; fast general-purpose layout' },
  { value: 'forceAtlas2Based', label: 'Force Atlas 2', hint: 'Live physics; separates communities well' },
  { value: 'repulsion', label: 'Repulsion', hint: 'Live physics; evenly spread nodes' },
  { value: 'hierarchicalRepulsion', label: 'Hierarchical repulsion', hint: 'Live physics tuned for layered graphs' },
  { value: 'hierarchical', label: 'Hierarchical (layered)', hint: 'Nodes in levels, hubs first; choose the direction' },
]

export const SHAPES: { value: NetworkStyle['shape']; label: string }[] = [
  { value: 'auto', label: 'By type (default)' }, { value: 'dot', label: 'Dot' }, { value: 'ellipse', label: 'Ellipse (label inside)' },
  { value: 'box', label: 'Box (label inside)' }, { value: 'circle', label: 'Circle (label inside)' }, { value: 'diamond', label: 'Diamond' },
  { value: 'square', label: 'Square' }, { value: 'hexagon', label: 'Hexagon' }, { value: 'star', label: 'Star' },
  { value: 'triangle', label: 'Triangle' }, { value: 'triangleDown', label: 'Triangle down' }, { value: 'database', label: 'Database' },
  { value: 'text', label: 'Text only' },
]

export const CURVES: { value: EdgeCurve; label: string }[] = [
  { value: 'straight', label: 'Straight' }, { value: 'dynamic', label: 'Dynamic' }, { value: 'continuous', label: 'Continuous' },
  { value: 'discrete', label: 'Discrete' }, { value: 'curvedCW', label: 'Curved (clockwise)' }, { value: 'curvedCCW', label: 'Curved (anticlockwise)' },
  { value: 'cubicBezier', label: 'Cubic Bézier' }, { value: 'diagonalCross', label: 'Diagonal cross' }, { value: 'straightCross', label: 'Straight cross' },
  { value: 'horizontal', label: 'Horizontal' }, { value: 'vertical', label: 'Vertical' },
]

export const DEFAULT_STYLE: NetworkStyle = {
  engine: 'precomputed', spacing: 1, freeze: true, direction: 'UD', sortMethod: 'hubsize',
  shape: 'auto', nodeSize: 14, sizeBy: 'fixed', labels: true, labelSize: 14, labelOutline: true,
  curve: 'straight', edgeWidth: 'auto', edgeOpacity: 0.8, dashes: false, shadows: false,
  navigation: true, dragNodes: true, hideEdgesOnDrag: false,
}

const KEY = 'flame-network-style'

function load(scope: string, defaults: NetworkStyle): NetworkStyle {
  try {
    const raw = sessionStorage.getItem(`${KEY}:${scope}`)
    return raw ? { ...defaults, ...JSON.parse(raw) } : defaults
  } catch {
    return defaults
  }
}

/** Style state for one kind of network ("results", "ppi"), remembered for the session. */
export function useNetworkStyle(scope: string, overrides: Partial<NetworkStyle> = {}) {
  const defaults = { ...DEFAULT_STYLE, ...overrides }
  const [style, setStyleState] = useState<NetworkStyle>(() => load(scope, defaults))
  const setStyle = (s: NetworkStyle) => {
    setStyleState(s)
    try { sessionStorage.setItem(`${KEY}:${scope}`, JSON.stringify(s)) } catch { /* storage unavailable */ }
  }
  return { style, setStyle, reset: () => setStyle(defaults), defaults }
}

/** vis-network options for a style. `fg`/`bg` are the theme's text and surface colours. */
export function visOptions(s: NetworkStyle, fg: string, bg: string): Options {
  const physics = s.engine !== 'precomputed' && s.engine !== 'hierarchical'
  const k = s.spacing
  const solvers = {
    barnesHut: { gravitationalConstant: -3000 * k, springLength: 120 * k, springConstant: 0.03, avoidOverlap: 0.2 },
    forceAtlas2Based: { gravitationalConstant: -60 * k, springLength: 100 * k, springConstant: 0.08, avoidOverlap: 0.3 },
    repulsion: { nodeDistance: 140 * k, springLength: 160 * k, centralGravity: 0.2 },
    hierarchicalRepulsion: { nodeDistance: 140 * k, springLength: 120 * k, avoidOverlap: 0.5 },
  }
  return {
    autoResize: true,
    physics: physics
      ? { enabled: true, solver: s.engine, [s.engine]: solvers[s.engine as keyof typeof solvers], stabilization: { enabled: true, iterations: 300, fit: true } }
      : { enabled: false },
    layout: {
      improvedLayout: physics,
      hierarchical: s.engine === 'hierarchical'
        ? { enabled: true, direction: s.direction, sortMethod: s.sortMethod, levelSeparation: 150 * k, nodeSpacing: 120 * k, treeSpacing: 200 * k, shakeTowards: 'roots' }
        : { enabled: false },
    },
    interaction: {
      hover: true, multiselect: true, navigationButtons: s.navigation, keyboard: { enabled: s.navigation, bindToWindow: false },
      dragNodes: s.dragNodes, hideEdgesOnDrag: s.hideEdgesOnDrag, tooltipDelay: 150,
    },
    nodes: {
      borderWidth: 1, size: s.nodeSize, shadow: s.shadows,
      scaling: { min: s.nodeSize * 0.6, max: s.nodeSize * 2.6, label: { enabled: false } },
      font: { color: fg, size: s.labelSize, strokeWidth: s.labelOutline ? 3 : 0, strokeColor: bg },
    },
    edges: {
      color: { color: '#9ca3af', highlight: '#f97316', hover: '#8b5cf6', opacity: s.edgeOpacity },
      smooth: s.curve === 'straight' ? false : { enabled: true, type: s.curve, roundness: 0.4 },
      dashes: s.dashes, shadow: s.shadows, selectionWidth: 2, hoverWidth: 1.5,
      scaling: { min: 1, max: 8, label: { enabled: false } },
    },
  }
}
