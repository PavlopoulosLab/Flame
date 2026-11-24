import { createContext, createElement, useContext, useEffect, useState, useSyncExternalStore, type ReactNode } from 'react'

/**
 * Chart palettes. Each visualization has its own palette (a PaletteScope,
 * chosen with PaletteSelect in its toolbar and remembered per view), so one
 * figure can be colour-blind safe or grayscale while the others keep theirs.
 */
export type PaletteMode = 'flame' | 'cb' | 'vivid' | 'pastel' | 'gray' | 'custom'

export const PALETTES: { value: PaletteMode; label: string; hint: string }[] = [
  { value: 'flame', label: 'Flame', hint: 'Default colours, matching the logo' },
  { value: 'cb', label: 'Colour-blind safe', hint: 'Okabe–Ito and Cividis: distinguishable with all common colour-vision deficiencies' },
  { value: 'vivid', label: 'Vivid', hint: 'Tableau 10 and Plasma: high contrast for slides' },
  { value: 'pastel', label: 'Pastel', hint: 'Soft colours for posters and backgrounds' },
  { value: 'gray', label: 'Grayscale (print)', hint: 'Shades of grey for black-and-white printing' },
  { value: 'custom', label: 'My colours', hint: 'Your own colours, set with Edit colours' },
]

const KEY = 'flame-palette'
const listeners = new Set<() => void>()
let mode: PaletteMode = (() => {
  try {
    const v = sessionStorage.getItem(KEY) as PaletteMode | null
    return v && PALETTES.some((p) => p.value === v) ? v : 'flame'
  } catch {
    return 'flame'
  }
})()

export function setPaletteMode(m: PaletteMode) {
  mode = m
  try { sessionStorage.setItem(KEY, m) } catch { /* storage unavailable */ }
  if (typeof document !== 'undefined') document.documentElement.setAttribute('data-palette', m)
  listeners.forEach((l) => l())
}
if (typeof document !== 'undefined') document.documentElement.setAttribute('data-palette', mode)

interface Scope { mode: PaletteMode; setMode: (m: PaletteMode) => void; setKey: (k: string) => void }
const ScopeContext = createContext<Scope | null>(null)
const readMode = (k: string): PaletteMode | null => {
  try { const v = localStorage.getItem(k) as PaletteMode | null; return v && PALETTES.some((p) => p.value === v) ? v : null } catch { return null }
}

/**
 * A visualization's own palette. `id` names the visualization (e.g. "results");
 * a child can refine it per view with usePaletteKey (e.g. the current tab).
 */
export function PaletteScope({ id, children }: { id: string; children: ReactNode }) {
  const [key, setKey] = useState('')
  const storageKey = `flame-palette:${id}${key ? `:${key}` : ''}`
  const [modes, setModes] = useState<Record<string, PaletteMode>>({})
  const current = modes[storageKey] ?? readMode(storageKey) ?? 'flame'
  const value: Scope = {
    mode: current,
    setMode: (m) => { setModes((x) => ({ ...x, [storageKey]: m })); try { localStorage.setItem(storageKey, m) } catch { /* storage unavailable */ } },
    setKey,
  }
  return createElement(ScopeContext.Provider, { value }, children)
}

/** Remember the palette separately for each view of a scope (e.g. each results tab). */
export function usePaletteKey(key: string) {
  const s = useContext(ScopeContext)
  const setKey = s?.setKey
  useEffect(() => { setKey?.(key) }, [key, setKey])
}

/** Palette of the nearest visualization scope (Flame colours outside any scope). */
export function usePaletteMode(): PaletteMode {
  const scope = useContext(ScopeContext)
  const global = useSyncExternalStore((l) => { listeners.add(l); return () => listeners.delete(l) }, () => mode)
  useCustomPalette() // re-draw when the user's own colours change
  return scope ? scope.mode : global
}

// ---- The user's own palette ("My colours"): kept in this browser, shared by every chart set to it ----

export interface CustomPalette {
  /** Colours of categories (sources, runs, groups…), in order. */
  colors: string[]
  /** Continuous scales (heatmaps, p-value colours) run from `low` to `high`. */
  low: string
  high: string
}
export const DEFAULT_CUSTOM_PALETTE: CustomPalette = {
  colors: ['#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02', '#a6761d', '#666666'],
  low: '#f7fbff', high: '#08306b',
}
export const MAX_CUSTOM_COLOURS = 12
const CUSTOM_KEY = 'flame-custom-palette'
const HEX = /^#[0-9a-f]{6}$/i
export const isHexColour = (c: string) => HEX.test(c)
const customListeners = new Set<() => void>()
let custom: CustomPalette = (() => {
  try {
    const v = JSON.parse(localStorage.getItem(CUSTOM_KEY) ?? 'null') as Partial<CustomPalette> | null
    const colors = (v?.colors ?? []).filter((c) => typeof c === 'string' && HEX.test(c)).slice(0, MAX_CUSTOM_COLOURS)
    if (!colors.length) return DEFAULT_CUSTOM_PALETTE
    return { colors, low: v?.low && HEX.test(v.low) ? v.low : DEFAULT_CUSTOM_PALETTE.low, high: v?.high && HEX.test(v.high) ? v.high : DEFAULT_CUSTOM_PALETTE.high }
  } catch {
    return DEFAULT_CUSTOM_PALETTE
  }
})()

export function getCustomPalette(): CustomPalette { return custom }
export function hasSavedCustomPalette(): boolean {
  try { return localStorage.getItem(CUSTOM_KEY) !== null } catch { return false }
}
export function setCustomPalette(p: CustomPalette) {
  const colors = p.colors.filter((c) => HEX.test(c)).slice(0, MAX_CUSTOM_COLOURS)
  custom = { colors: colors.length ? colors : DEFAULT_CUSTOM_PALETTE.colors, low: HEX.test(p.low) ? p.low : custom.low, high: HEX.test(p.high) ? p.high : custom.high }
  try { localStorage.setItem(CUSTOM_KEY, JSON.stringify(custom)) } catch { /* storage unavailable */ }
  customListeners.forEach((l) => l())
}
/** Hex codes found in pasted text ("#1b9e77, d95f02 …"). */
export function parseHexColours(text: string): string[] {
  return (text.match(/#?\b[0-9a-f]{6}\b/gi) ?? []).map((c) => (c.startsWith('#') ? c : `#${c}`).toLowerCase())
}
export function useCustomPalette(): CustomPalette {
  return useSyncExternalStore((l) => { customListeners.add(l); return () => customListeners.delete(l) }, () => custom)
}

/** Setter of the nearest scope (null outside any scope); used by PaletteSelect. */
export function usePaletteScope() {
  return useContext(ScopeContext)
}

/** Back-compat helper: true when the colour-blind safe palette is active. */
export const useColorBlindSafe = () => usePaletteMode() === 'cb'

export const OKABE_ITO = ['#E69F00', '#56B4E9', '#009E73', '#0072B2', '#D55E00', '#CC79A7', '#F0E442', '#999999']
const TABLEAU10 = ['#4E79A7', '#F28E2B', '#E15759', '#76B7B2', '#59A14F', '#EDC948', '#B07AA1', '#FF9DA7', '#9C755F', '#BAB0AC']
const PASTEL = ['#8DD3C7', '#FFED6F', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#CCEBC5', '#BC80BD']
const GRAYS = ['#252525', '#636363', '#969696', '#bdbdbd', '#525252', '#737373', '#a3a3a3', '#d9d9d9']
const FLAME = ['#f97316', '#8b5cf6', '#e11d48', '#6366f1', '#16a34a', '#0891b2', '#eab308', '#db2777', '#64748b', '#84cc16']

export function categorical(m: PaletteMode): string[] {
  if (m === 'custom') return custom.colors.length ? custom.colors : FLAME
  return { flame: FLAME, cb: OKABE_ITO, vivid: TABLEAU10, pastel: PASTEL, gray: GRAYS }[m]
}

function hash(s: string) {
  let h = 0
  for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) >>> 0
  return h
}

/** Colour for a category. In the Flame palette the configured colour (if any) wins. */
export function categoricalColor(m: PaletteMode | boolean, key: string, keys: string[], configured?: string): string {
  const pm: PaletteMode = typeof m === 'boolean' ? (m ? 'cb' : 'flame') : m
  if (pm === 'flame' && configured) return configured
  const pal = categorical(pm)
  const i = keys.indexOf(key)
  return pal[(i < 0 ? hash(key) : i) % pal.length]
}

export type ColourScale = string | [number, string][]

/** Continuous colour scale for the palette: a Plotly scale name, or low → high stops for "My colours".
 *  `flameDefault` is used in the Flame palette. */
export function sequentialScale(m: PaletteMode | boolean, flameDefault: string): ColourScale {
  const pm: PaletteMode = typeof m === 'boolean' ? (m ? 'cb' : 'flame') : m
  if (pm === 'custom') return [[0, custom.low], [1, custom.high]]
  return { flame: flameDefault, cb: 'Cividis', vivid: 'Plasma', pastel: 'Blues', gray: 'Greys' }[pm]
}

/** Whether a Plotly scale needs reversing so that larger = darker/stronger (never for explicit stops). */
export function reverseScale(name: ColourScale | undefined): boolean {
  return typeof name === 'string' && ['YlOrRd', 'Blues', 'Greys', 'Reds', 'Greens', 'YlGnBu', 'OrRd'].includes(name)
}

/** Fixed role colours (volcano, impact, Sankey layers, single-series charts). */
export function roleColors(m: PaletteMode | boolean) {
  const pm: PaletteMode = typeof m === 'boolean' ? (m ? 'cb' : 'flame') : m
  switch (pm) {
    case 'custom': {
      const c = custom.colors.length ? custom.colors : FLAME
      const at = (i: number) => c[i % c.length]
      return { up: at(0), down: at(1), ns: '#BBBBBB', high: at(0), moderate: at(1), low: at(2), modifier: '#BBBBBB',
        variant: at(3), gene: at(1), trait: at(0), accent: at(0), series: [at(0), at(1), at(2), at(3)] }
    }
    case 'cb':
      return { up: '#D55E00', down: '#0072B2', ns: '#BBBBBB', high: '#D55E00', moderate: '#E69F00', low: '#F0E442', modifier: '#999999',
        variant: '#CC79A7', gene: '#E69F00', trait: '#0072B2', accent: '#0072B2', series: ['#0072B2', '#E69F00', '#009E73', '#CC79A7'] }
    case 'vivid':
      return { up: '#E15759', down: '#4E79A7', ns: '#BAB0AC', high: '#E15759', moderate: '#F28E2B', low: '#EDC948', modifier: '#BAB0AC',
        variant: '#B07AA1', gene: '#F28E2B', trait: '#4E79A7', accent: '#4E79A7', series: ['#4E79A7', '#F28E2B', '#59A14F', '#E15759'] }
    case 'pastel':
      return { up: '#FB8072', down: '#80B1D3', ns: '#D9D9D9', high: '#FB8072', moderate: '#FDB462', low: '#FFED6F', modifier: '#D9D9D9',
        variant: '#BEBADA', gene: '#FDB462', trait: '#80B1D3', accent: '#80B1D3', series: ['#80B1D3', '#FDB462', '#B3DE69', '#BEBADA'] }
    case 'gray':
      return { up: '#252525', down: '#737373', ns: '#d9d9d9', high: '#252525', moderate: '#636363', low: '#969696', modifier: '#d9d9d9',
        variant: '#525252', gene: '#969696', trait: '#252525', accent: '#525252', series: ['#252525', '#737373', '#bdbdbd', '#525252'] }
    default:
      return { up: '#dc2626', down: '#2563eb', ns: '#a1a1aa', high: '#dc2626', moderate: '#f97316', low: '#eab308', modifier: '#a1a1aa',
        variant: '#8b5cf6', gene: '#f97316', trait: '#e11d48', accent: '#f97316', series: ['#f97316', '#2563eb', '#a1a1aa', '#8b5cf6'] }
  }
}
