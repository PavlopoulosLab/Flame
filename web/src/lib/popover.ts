/**
 * Where to put a pop-up panel next to the button that opened it, so that it stays inside the window:
 * below the button (above it when there is no room below and there is room above), aligned with the
 * button's right edge (or left edge), and slid sideways to keep a margin from the window's edges.
 * Panels are then drawn with position: fixed, so a narrow chart card or a scrolling dialog cannot clip them.
 */
export interface Box { left: number; right: number; top: number; bottom: number }

/** `bounds`: the area the panel must stay in (the window, or the dialog it was opened from). */
export function placePopover(anchor: Box, size: { width: number; height: number }, bounds: Box,
  align: 'left' | 'right' = 'right', margin = 8, gap = 4): { left: number; top: number } {
  const minLeft = bounds.left + margin
  const maxLeft = Math.max(minLeft, bounds.right - size.width - margin)
  const wanted = align === 'right' ? anchor.right - size.width : anchor.left
  const left = Math.min(Math.max(wanted, minLeft), maxLeft)
  const below = anchor.bottom + gap
  const above = anchor.top - gap - size.height
  let top = below
  if (below + size.height > bounds.bottom - margin && above >= bounds.top + margin) top = above
  // taller than the room on either side: keep its top inside
  top = Math.max(bounds.top + margin, Math.min(top, Math.max(bounds.top + margin, bounds.bottom - size.height - margin)))
  return { left: Math.round(left), top: Math.round(top) }
}
