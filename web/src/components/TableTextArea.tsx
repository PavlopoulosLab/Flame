import { useEffect, useRef, useState, type ClipboardEvent, type KeyboardEvent } from 'react'
import { detectDelim, gridToText, parseGrid, pasteInto, type Grid } from '../lib/delimited'
import { plural } from '../lib/format'

const MAX_ROWS = 300   // read as cells; the rows after them stay text, written back unchanged

/**
 * A text box for tables. Pasting tab- or comma-separated text (e.g. cells copied from Excel) offers to show
 * it as an editable table; edits in the table are written back to the text, which stays the value.
 */
export function TableTextArea({ value, onChange, rows = 5, placeholder, ariaLabel = 'Table', className = 'input mono', defaultView = 'text', maxCols = 60, header = true }: {
  value: string; onChange: (text: string) => void; rows?: number; placeholder?: string; ariaLabel?: string; className?: string
  /** 'table': open loaded tables as cells (e.g. when editing a table already analysed). */ defaultView?: 'text' | 'table'
  /** First row shown as column names (false for GMT files). */ header?: boolean
  /** Columns drawn as cells (wide expression tables: the others stay editable in the text view). */ maxCols?: number
}) {
  const [view, setView] = useState<'text' | 'table'>(defaultView)
  const [offer, setOffer] = useState(false)
  const [grid, setGrid] = useState<Grid | null>(null)
  const emitted = useRef<string | null>(null)   // the text this component last wrote: not re-read into the grid
  const tableRef = useRef<HTMLTableElement>(null)
  const tabular = !!detectDelim(value)

  // A new value from outside (an example, a file, the text view): read it again as a grid.
  useEffect(() => {
    if (value === emitted.current) return
    emitted.current = null
    setGrid(parseGrid(value, undefined, MAX_ROWS))
  }, [value])
  // Not a table any more (cleared, or a plain list): back to the text box.
  useEffect(() => { if (!tabular) { setOffer(false); if (view === 'table' && !grid) setView('text') } }, [tabular, view, grid])

  const update = (g: Grid) => { setGrid(g); const t = gridToText(g); emitted.current = t; onChange(t) }
  const setCell = (r: number, c: number, v: string) => grid && update({ ...grid, rows: grid.rows.map((row, i) => (i === r ? row.map((x, j) => (j === c ? v : x)) : row)) })
  const addRow = () => grid && update({ ...grid, rows: [...grid.rows, Array(grid.rows[0]?.length ?? 1).fill('')] })
  const addCol = () => grid && update({ ...grid, rows: grid.rows.map((r) => [...r, '']) })
  const delRow = (r: number) => grid && grid.rows.length > 1 && update({ ...grid, rows: grid.rows.filter((_, i) => i !== r) })
  const delCol = (c: number) => grid && (grid.rows[0]?.length ?? 0) > 1 && update({ ...grid, rows: grid.rows.map((r) => r.filter((_, j) => j !== c)) })

  const focusCell = (r: number, c: number) => tableRef.current?.querySelector<HTMLInputElement>(`input[data-r="${r}"][data-c="${c}"]`)?.focus()
  const onCellKey = (e: KeyboardEvent<HTMLInputElement>, r: number, c: number) => {
    if (e.key === 'Enter' || e.key === 'ArrowDown') { e.preventDefault(); if (e.key === 'Enter' && grid && r === grid.rows.length - 1) addRow(); setTimeout(() => focusCell(r + 1, c)) }
    else if (e.key === 'ArrowUp') { e.preventDefault(); focusCell(r - 1, c) }
  }
  const onCellPaste = (e: ClipboardEvent<HTMLInputElement>, r: number, c: number) => {
    const t = e.clipboardData.getData('text')
    if (grid && /[\t\n]/.test(t.replace(/\r?\n$/, ''))) { e.preventDefault(); update(pasteInto(grid, r, c, t)) }
  }
  const onTextPaste = (e: ClipboardEvent<HTMLTextAreaElement>) => {
    if (detectDelim(e.clipboardData.getData('text'))) setOffer(true)
  }

  const shown = grid ? grid.rows : []
  const width = grid?.rows[0]?.length ?? 0
  const drawn = Math.min(width, maxCols)
  // each column as wide as its longest value (first 100 rows), within 6 to 28 characters
  const sizes = Array.from({ length: drawn }, (_, c) => Math.min(28, Math.max(6, ...shown.slice(0, 100).map((r) => (r[c] ?? '').length + 1))))
  const more = [grid?.rest ? `the first ${MAX_ROWS} rows` : '', width > drawn ? `the first ${drawn} of ${width} columns` : ''].filter(Boolean)
  return (
    <div className="table-text stack-sm">
      {(tabular || view === 'table') && (
        <div className="row between wrap">
          <div className="segmented segmented-sm" role="group" aria-label={`${ariaLabel} view`}>
            <button type="button" className={view === 'text' ? 'active' : ''} aria-pressed={view === 'text'} onClick={() => setView('text')}>Text</button>
            <button type="button" className={view === 'table' ? 'active' : ''} aria-pressed={view === 'table'} onClick={() => { setGrid(parseGrid(value, undefined, MAX_ROWS) ?? grid); setView('table'); setOffer(false) }}>Table</button>
          </div>
          {grid && <span className="muted small">{grid.rest ? `${MAX_ROWS}+ rows` : plural(grid.rows.length, 'row')} × {plural(width, 'column')}{grid.delim === '\t' ? ', tab-separated' : grid.delim === ',' ? ', comma-separated' : ', semicolon-separated'}</span>}
        </div>
      )}
      {offer && view === 'text' && grid && (
        <div className="table-text-offer" role="status">
          <span>This looks like a table ({grid.rest ? `${MAX_ROWS}+ rows` : plural(grid.rows.length, 'row')} × {plural(width, 'column')}). Show it as an editable table?</span>
          <span className="row">
            <button type="button" className="btn btn-primary btn-sm" onClick={() => { setView('table'); setOffer(false) }}>Show as table</button>
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => setOffer(false)}>Keep as text</button>
          </span>
        </div>
      )}
      {view === 'text' || !grid ? (
        <textarea className={className} rows={rows} value={value} aria-label={ariaLabel} placeholder={placeholder} wrap="off"
          onPaste={onTextPaste} onChange={(e) => onChange(e.target.value)} />
      ) : (
        <>
          <div className="table-text-scroll">
            <table className="table-text-grid" ref={tableRef} aria-label={ariaLabel}>
              <thead>
                <tr>
                  <th aria-hidden />
                  {Array.from({ length: drawn }, (_, c) => (
                    <th key={c}><button type="button" className="table-text-del" title="Delete this column" aria-label={`Delete column ${c + 1}`}
                      disabled={width < 2 || !!grid.rest} onClick={() => delCol(c)}>×</button></th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {shown.map((row, r) => (
                  <tr key={r} className={r === 0 && header ? 'table-text-head' : undefined}>
                    <th><button type="button" className="table-text-del" title="Delete this row" aria-label={`Delete row ${r + 1}`}
                      disabled={grid.rows.length < 2} onClick={() => delRow(r)}>×</button></th>
                    {row.slice(0, drawn).map((v, c) => (
                      <td key={c}><input value={v} size={sizes[c]} data-r={r} data-c={c} spellCheck={false} aria-label={`Row ${r + 1}, column ${c + 1}`}
                        onChange={(e) => setCell(r, c, e.target.value)} onKeyDown={(e) => onCellKey(e, r, c)} onPaste={(e) => onCellPaste(e, r, c)} /></td>
                    ))}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          <div className="row wrap">
            <button type="button" className="btn btn-ghost btn-sm" onClick={addRow}>+ Row</button>
            <button type="button" className="btn btn-ghost btn-sm" onClick={addCol} disabled={!!grid.rest}
              title={grid.rest ? 'A long table: add or delete columns in the text view, so every row gets them' : undefined}>+ Column</button>
            <span className="muted small">{more.length
              ? `Showing ${more.join(' and ')}: edit the rest in the text view.`
              : 'Edit any cell. Enter moves down, Tab moves right; paste cells from Excel into a cell to fill several.'}</span>
          </div>
        </>
      )}
    </div>
  )
}

/**
 * A table box for analyses that rerun when the table changes (expression, survival): edits stay a draft
 * until "Use this table", so the analysis does not restart at every keystroke.
 */
export function StagedTableText({ current, onApply, applyLabel = 'Use this table', ...box }: {
  current: string; onApply: (text: string) => void; applyLabel?: string
} & Omit<Parameters<typeof TableTextArea>[0], 'value' | 'onChange'>) {
  const [draft, setDraft] = useState(current)
  useEffect(() => { setDraft(current) }, [current])
  const changed = draft !== current && !!draft.trim()
  return (
    <div className="stack-sm">
      <TableTextArea value={draft} onChange={setDraft} {...box} />
      {changed && (
        <div className="row">
          <button type="button" className="btn btn-primary btn-sm" onClick={() => { onApply(draft); setDraft(current) }}>{applyLabel}</button>
          {current && <button type="button" className="btn btn-ghost btn-sm" onClick={() => setDraft(current)}>Discard changes</button>}
        </div>
      )}
    </div>
  )
}
