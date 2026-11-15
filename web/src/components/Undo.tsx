import { createContext, useCallback, useContext, useEffect, useRef, useState, type ReactNode } from 'react'

interface UndoItem {
  id: number
  message: string
  undo: () => void | Promise<unknown>
  commit?: () => void | Promise<unknown>
}

const UndoCtx = createContext<(message: string, undo: UndoItem['undo'], commit?: UndoItem['commit'], ms?: number) => void>(() => {})

/** Toast with an Undo button. `commit` runs when the toast expires without being undone. */
export function UndoProvider({ children }: { children: ReactNode }) {
  const [items, setItems] = useState<UndoItem[]>([])
  const timers = useRef(new Map<number, number>())
  const seq = useRef(0)

  const finish = useCallback((id: number, undone: boolean) => {
    setItems((list) => {
      const item = list.find((i) => i.id === id)
      if (item) Promise.resolve(undone ? item.undo() : item.commit?.()).catch(() => {})
      return list.filter((i) => i.id !== id)
    })
    const t = timers.current.get(id)
    if (t) window.clearTimeout(t)
    timers.current.delete(id)
  }, [])

  const push = useCallback((message: string, undo: UndoItem['undo'], commit?: UndoItem['commit'], ms = 8000) => {
    const id = ++seq.current
    setItems((l) => [...l.slice(-2), { id, message, undo, commit }])
    timers.current.set(id, window.setTimeout(() => finish(id, false), ms))
  }, [finish])

  // Ctrl/⌘+Z undoes the most recent action while its toast is visible.
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (!(e.metaKey || e.ctrlKey) || e.key.toLowerCase() !== 'z' || e.shiftKey) return
      const t = e.target as HTMLElement | null
      if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.isContentEditable)) return
      const last = items[items.length - 1]
      if (last) { e.preventDefault(); e.stopImmediatePropagation(); finish(last.id, true) }
    }
    window.addEventListener('keydown', onKey, true)
    return () => window.removeEventListener('keydown', onKey, true)
  }, [items, finish])

  // Commit pending actions if the page is closed.
  useEffect(() => {
    const onUnload = () => items.forEach((i) => i.commit?.())
    window.addEventListener('pagehide', onUnload)
    return () => window.removeEventListener('pagehide', onUnload)
  }, [items])

  return (
    <UndoCtx.Provider value={push}>
      {children}
      <div className="toasts" role="status" aria-live="polite">
        {items.map((i) => (
          <div key={i.id} className="toast">
            <span>{i.message}</span>
            <button className="btn btn-sm btn-secondary" onClick={() => finish(i.id, true)}>Undo</button>
            <button className="icon-btn" aria-label="Dismiss" onClick={() => finish(i.id, false)}>×</button>
          </div>
        ))}
      </div>
    </UndoCtx.Provider>
  )
}

export const useUndoToast = () => useContext(UndoCtx)

export function UndoRedoButtons({ h }: { h: { undo: () => void; redo: () => void; canUndo: boolean; canRedo: boolean } }) {
  const mac = typeof navigator !== 'undefined' && /Mac/i.test(navigator.platform)
  return (
    <span className="undo-redo" role="group" aria-label="Undo and redo">
      <button className="icon-btn" onClick={h.undo} disabled={!h.canUndo} title={`Undo (${mac ? '⌘' : 'Ctrl'}+Z)`} aria-label="Undo">↶</button>
      <button className="icon-btn" onClick={h.redo} disabled={!h.canRedo} title={`Redo (${mac ? '⌘⇧' : 'Ctrl+Shift+'}Z)`} aria-label="Redo">↷</button>
    </span>
  )
}
