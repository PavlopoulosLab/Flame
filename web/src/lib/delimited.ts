/**
 * Pasted tables (from Excel, a TSV or a CSV file) as a grid of cells and back, so a text box can be shown
 * and edited as a table. Tabs win over commas and semicolons; quoted CSV fields may hold the separator.
 */
export type Delim = '\t' | ',' | ';'
/** `rest`: the lines after the first `maxRows` (large tables), kept as text and written back unchanged. */
export interface Grid { delim: Delim; rows: string[][]; rest?: string }

/** The first `max` non-empty lines, without splitting the whole text (expression tables can be 20 MB), and the text after them. */
export function headLines(text: string, max = Infinity): { lines: string[]; rest: string } {
  const lines: string[] = []
  let at = 0
  while (at < text.length && lines.length < max) {
    let end = text.indexOf('\n', at)
    if (end < 0) end = text.length
    const line = text.slice(at, end).replace(/\r$/, '')
    if (line.trim()) lines.push(line)
    at = end + 1
  }
  return { lines, rest: text.slice(at).trim() ? text.slice(at) : '' }
}
const nonEmptyLines = (text: string, max?: number) => headLines(text, max).lines

/** The separator of a pasted table, or null when the text is not a table (fewer than two rows or columns). */
export function detectDelim(text: string): Delim | null {
  const lines = nonEmptyLines(text, 50)
  if (lines.length < 2) return null
  for (const d of ['\t', ';', ','] as Delim[]) {
    const counts = lines.map((l) => splitRow(l, d).length)
    // a table: most rows split into the same number (2 or more) of cells
    const width = counts[0]
    if (width < 2) continue
    const same = counts.filter((n) => n === width).length
    if (d === '\t' ? counts.some((n) => n >= 2) : same >= Math.ceil(lines.length * 0.8)) return d
  }
  return null
}

/** One row split at the separator; double quotes group a field and "" is a literal quote. */
export function splitRow(line: string, delim: Delim): string[] {
  if (delim === '\t' || !line.includes('"')) return line.split(delim)
  const out: string[] = []
  let cur = '', q = false
  for (let i = 0; i < line.length; i++) {
    const ch = line[i]
    if (q) {
      if (ch === '"' && line[i + 1] === '"') { cur += '"'; i++ }
      else if (ch === '"') q = false
      else cur += ch
    } else if (ch === '"' && cur === '') q = true
    else if (ch === delim) { out.push(cur); cur = '' }
    else cur += ch
  }
  out.push(cur)
  return out
}

/**
 * The table in the text, every row padded to the widest one; null when the text is not a table.
 * Only the first `maxRows` rows become cells; the rest is kept as text.
 */
export function parseGrid(text: string, delim: Delim | null = detectDelim(text), maxRows = Infinity): Grid | null {
  if (!delim) return null
  const { lines, rest } = headLines(text, maxRows)
  const rows = lines.map((l) => splitRow(l, delim))
  const width = Math.max(...rows.map((r) => r.length))
  return { delim, rest, rows: rows.map((r) => (r.length < width ? [...r, ...Array(width - r.length).fill('')] : r)) }
}

function cell(v: string, delim: Delim): string {
  const clean = v.replace(/\r?\n/g, ' ')
  if (delim === '\t') return clean.replace(/\t/g, ' ')
  return clean.includes(delim) || clean.includes('"') ? `"${clean.replace(/"/g, '""')}"` : clean
}

/** Back to text with the same separator; empty rows, and empty cells at the end of a row, are left out. */
export function gridToText(grid: Grid): string {
  return grid.rows
    .filter((r) => r.some((v) => v.trim()))
    .map((r) => { let n = r.length; while (n > 1 && r[n - 1] === '') n--; return r.slice(0, n).map((v) => cell(v, grid.delim)).join(grid.delim) })
    .join('\n') + '\n' + (grid.rest ? grid.rest.replace(/^\r?\n+/, '') : '')
}

/** Cells pasted into one cell of the grid (several rows or columns from Excel) spread right and down from it. */
export function pasteInto(grid: Grid, row: number, col: number, text: string): Grid {
  const block = text.replace(/\r?\n$/, '').split(/\r?\n/).map((l) => l.split('\t'))
  const height = Math.max(grid.rows.length, row + block.length)
  const width = Math.max(grid.rows[0]?.length ?? 0, col + Math.max(...block.map((b) => b.length)))
  const rows = Array.from({ length: height }, (_, i) => Array.from({ length: width }, (_, j) => grid.rows[i]?.[j] ?? ''))
  block.forEach((b, i) => b.forEach((v, j) => { rows[row + i][col + j] = v }))
  return { delim: grid.delim, rows, rest: grid.rest }
}
