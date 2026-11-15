import { useSyncExternalStore } from 'react'

/**
 * App-wide chart palette. Every chart reads it, so a figure can be switched
 * to a colour-blind safe, vivid, pastel or print (grayscale) scheme at once.
 */
export type PaletteMode = 'flame' | 'cb' | 'vivid' | 'pastel' | 'gray'

export const PALETTES: { value: PaletteMode; label: string; hint: string }[] = [
  { value: 'flame', label: 'Flame', hint: 'Default colours, matching the logo' },
  { value: 'cb', label: 'Colour-blind safe', hint: 'Okabe–Ito and Cividis: distinguishable with all common colour-vision deficiencies' },
  { value: 'vivid', label: 'Vivid', hint: 'Tableau 10 and Plasma: high contrast for slides' },
  { value: 'pastel', label: 'Pastel', hint: 'Soft colours for posters and backgrounds' },
  { value: 'gray', label: 'Grayscale (print)', hint: 'Shades of grey for black-and-white printing' },
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

export function usePaletteMode(): PaletteMode {
  return useSyncExternalStore((l) => { listeners.add(l); return () => listeners.delete(l) }, () => mode)
}

/** Back-compat helper: true when the colour-blind safe palette is active. */
export const useColorBlindSafe = () => usePaletteMode() === 'cb'

export const OKABE_ITO = ['#E69F00', '#56B4E9', '#009E73', '#0072B2', '#D55E00', '#CC79A7', '#F0E442', '#999999']
const TABLEAU10 = ['#4E79A7', '#F28E2B', '#E15759', '#76B7B2', '#59A14F', '#EDC948', '#B07AA1', '#FF9DA7', '#9C755F', '#BAB0AC']
const PASTEL = ['#8DD3C7', '#FFED6F', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#CCEBC5', '#BC80BD']
const GRAYS = ['#252525', '#636363', '#969696', '#bdbdbd', '#525252', '#737373', '#a3a3a3', '#d9d9d9']
const FLAME = ['#f97316', '#8b5cf6', '#e11d48', '#6366f1', '#16a34a', '#0891b2', '#eab308', '#db2777', '#64748b', '#84cc16']

export function categorical(m: PaletteMode): string[] {
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

/** Continuous colour scale (Plotly name) for the palette; `flameDefault` is used in the Flame palette. */
export function sequentialScale(m: PaletteMode | boolean, flameDefault: string): string {
  const pm: PaletteMode = typeof m === 'boolean' ? (m ? 'cb' : 'flame') : m
  return { flame: flameDefault, cb: 'Cividis', vivid: 'Plasma', pastel: 'Blues', gray: 'Greys' }[pm]
}

/** Whether a Plotly scale needs reversing so that larger = darker/stronger. */
export function reverseScale(name: string): boolean {
  return ['YlOrRd', 'Blues', 'Greys', 'Reds', 'Greens', 'YlGnBu', 'OrRd'].includes(name)
}

/** Fixed role colours (volcano, impact, Sankey layers, single-series charts). */
export function roleColors(m: PaletteMode | boolean) {
  const pm: PaletteMode = typeof m === 'boolean' ? (m ? 'cb' : 'flame') : m
  switch (pm) {
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
