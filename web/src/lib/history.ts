import { useCallback, useEffect, useRef, useState } from 'react'

/**
 * Undo/redo for a screen's view state.
 *
 * `snapshot` is the current state (any JSON-serialisable value); `apply` restores
 * a snapshot. Changes within `mergeMs` of each other (e.g. typing in a search box)
 * become one history step. Ctrl/⌘+Z undoes, Ctrl/⌘+Shift+Z or Ctrl+Y redoes —
 * except while typing in a text field, where the browser's own text undo applies.
 */
export function useHistory<T>(snapshot: T, apply: (s: T) => void, mergeMs = 600) {
  const past = useRef<string[]>([])
  const future = useRef<string[]>([])
  const current = useRef<string>(JSON.stringify(snapshot))
  const restoring = useRef(false)
  const lastPush = useRef(0)
  const [, force] = useState(0)

  const key = JSON.stringify(snapshot)
  useEffect(() => {
    if (key === current.current) return
    if (restoring.current) {
      restoring.current = false
      current.current = key
      return
    }
    const now = Date.now()
    // Merge rapid successive edits into the previous step.
    if (now - lastPush.current > mergeMs || past.current.length === 0) past.current.push(current.current)
    if (past.current.length > 100) past.current.shift()
    lastPush.current = now
    future.current = []
    current.current = key
    force((n) => n + 1)
  }, [key, mergeMs])

  const undo = useCallback(() => {
    const prev = past.current.pop()
    if (prev === undefined) return
    future.current.push(current.current)
    restoring.current = true
    lastPush.current = 0
    apply(JSON.parse(prev))
    force((n) => n + 1)
  }, [apply])

  const redo = useCallback(() => {
    const next = future.current.pop()
    if (next === undefined) return
    past.current.push(current.current)
    restoring.current = true
    lastPush.current = 0
    apply(JSON.parse(next))
    force((n) => n + 1)
  }, [apply])

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (!(e.metaKey || e.ctrlKey)) return
      const t = e.target as HTMLElement | null
      if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.isContentEditable)) return
      const k = e.key.toLowerCase()
      if (k === 'z' && !e.shiftKey) { e.preventDefault(); undo() }
      else if ((k === 'z' && e.shiftKey) || k === 'y') { e.preventDefault(); redo() }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [undo, redo])

  return { undo, redo, canUndo: past.current.length > 0, canRedo: future.current.length > 0 }
}
