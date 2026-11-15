import { useRef, useState, type PointerEvent as RPointerEvent } from 'react'

export type BrushMode = 'click' | 'box' | 'lasso'
export type Pt = [number, number]

export function pointInPolygon([x, y]: Pt, poly: Pt[]): boolean {
  let inside = false
  for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
    const [xi, yi] = poly[i], [xj, yj] = poly[j]
    if ((yi > y) !== (yj > y) && x < ((xj - xi) * (y - yi)) / (yj - yi + 1e-12) + xi) inside = !inside
  }
  return inside
}

export const boxPolygon = (a: Pt, b: Pt): Pt[] => [[a[0], a[1]], [b[0], a[1]], [b[0], b[1]], [a[0], b[1]]]

/** Mode switch shown above charts that support box/lasso selection. */
export function BrushModes({ mode, onChange }: { mode: BrushMode; onChange: (m: BrushMode) => void }) {
  return (
    <div className="plot-modes" role="radiogroup" aria-label="Mouse mode">
      {([['click', '☝ Click / drag'], ['box', '▭ Box select'], ['lasso', '◌ Lasso']] as const).map(([m, l]) => (
        <button key={m} type="button" role="radio" aria-checked={mode === m} className={mode === m ? 'on' : ''} onClick={() => onChange(m)}
          title={m === 'click' ? 'Click items to select (Shift adds); drag to move or pan' : 'Drag to select items (hold Shift to add to the selection)'}>{l}</button>
      ))}
    </div>
  )
}

/**
 * Drag-selection overlay. `toLocal` converts client coordinates to the space
 * the caller tests items in; `onDone` receives the polygon (box or lasso).
 */
export function useBrush(mode: BrushMode, toLocal: (x: number, y: number) => Pt, onDone: (poly: Pt[], additive: boolean) => void) {
  const [pts, setPts] = useState<Pt[] | null>(null)
  const start = useRef<Pt | null>(null)
  const poly = useRef<Pt[]>([])
  const handlers = mode === 'click' ? {} : {
    onPointerDown: (e: RPointerEvent) => {
      (e.target as Element).setPointerCapture?.(e.pointerId)
      const p = toLocal(e.clientX, e.clientY)
      start.current = p
      poly.current = [p]
      setPts([p])
      e.preventDefault()
    },
    onPointerMove: (e: RPointerEvent) => {
      const s0 = start.current
      if (!s0) return
      const p = toLocal(e.clientX, e.clientY)
      poly.current = mode === 'box' ? boxPolygon(s0, p) : [...poly.current, p]
      setPts(poly.current)
    },
    onPointerUp: (e: RPointerEvent) => {
      const done = poly.current
      start.current = null
      poly.current = []
      setPts(null)
      if (done.length >= 3) onDone(done, e.shiftKey || e.metaKey || e.ctrlKey)
    },
  }
  const path = pts && pts.length > 1 ? `M${pts.map((p) => p.join(',')).join('L')}Z` : ''
  return { handlers, path }
}
