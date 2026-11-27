import { useLayoutEffect, type RefObject } from 'react'
import { placePopover } from '../lib/popover'

/**
 * Keeps an open pop-up panel inside the window (see placePopover): fixed positioning, recomputed when the
 * window is resized, when anything scrolls (a dialog, the page) and when the panel itself changes size.
 */
export function usePopoverPlacement(anchorRef: RefObject<HTMLElement | null>, popRef: RefObject<HTMLElement | null>,
  open: boolean, align: 'left' | 'right' = 'right') {
  useLayoutEffect(() => {
    if (!open) return
    const place = () => {
      const a = anchorRef.current, p = popRef.current
      if (!a || !p) return
      // inside a dialog, keep to its sides (wider than the panel on most screens); otherwise to the window's
      const vw = document.documentElement.clientWidth, vh = window.innerHeight
      const dlg = a.closest('.modal, [role="dialog"]')?.getBoundingClientRect()
      const bounds = { left: Math.max(0, dlg?.left ?? 0), right: Math.min(vw, dlg?.right ?? vw), top: 0, bottom: vh }
      if (bounds.right - bounds.left < p.offsetWidth + 16) { bounds.left = 0; bounds.right = vw }   // too narrow: use the window
      const { left, top } = placePopover(a.getBoundingClientRect(), { width: p.offsetWidth, height: p.offsetHeight }, bounds, align)
      Object.assign(p.style, { position: 'fixed', left: `${left}px`, top: `${top}px`, right: 'auto', bottom: 'auto' })
    }
    place()
    const ro = typeof ResizeObserver !== 'undefined' ? new ResizeObserver(place) : null
    if (ro && popRef.current) ro.observe(popRef.current)
    window.addEventListener('resize', place)
    window.addEventListener('scroll', place, true)       // capture: scrolling inside a dialog moves the button too
    return () => { ro?.disconnect(); window.removeEventListener('resize', place); window.removeEventListener('scroll', place, true) }
  }, [open, align, anchorRef, popRef])
}
