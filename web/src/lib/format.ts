export function formatP(p: number | null | undefined): string {
  if (p == null || !Number.isFinite(p)) return 'NA'
  if (p === 0) return '0'
  return p < 0.001 ? p.toExponential(2) : p.toPrecision(3)
}

export function formatNum(v: number | null | undefined, digits = 3): string {
  if (v == null || !Number.isFinite(v)) return 'NA'
  return Number.isInteger(v) ? String(v) : v.toPrecision(digits)
}

export function plural(n: number, word: string, pluralWord = `${word}s`) {
  return `${n} ${n === 1 ? word : pluralWord}`
}

export function splitGenes(genes: string | null | undefined): string[] {
  return (genes ?? '')
    .split(/,\s*/)
    .map((g) => g.trim())
    .filter(Boolean)
}

export const isHttpUrl = (u: unknown): u is string => typeof u === 'string' && /^https?:\/\//i.test(u.trim())

/** A text cell a spreadsheet would read as a formula (=, +, -, @, tab or carriage return first) gets a leading
 *  apostrophe. Numbers are left alone, so negative values stay numbers. */
const neutralise = (value: unknown): string => {
  const s = value == null ? '' : String(value)
  return typeof value !== 'number' && /^[=+\-@\t\r]/.test(s) ? `'${s}` : s
}

/** CSV cell: quote when needed and neutralise spreadsheet formulas. */
export function csvCell(value: unknown): string {
  const s = neutralise(value)
  return /[",\n\r]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s
}

/** TSV cell: tabs and line breaks become spaces, and spreadsheet formulas are neutralised. */
export function tsvCell(value: unknown): string {
  return neutralise(typeof value === 'string' ? value.replace(/[\t\r\n]+/g, ' ') : value)
}

/** Plotly draws a few HTML tags in labels and hover text. Keep the ones our charts use and show any
 *  other markup (from uploaded names or API data, e.g. a link) as plain text. */
export const plotlySafe = (s: string): string =>
  s.includes('<') ? s.replace(/<(?!\/?(?:br|b|i|extra)\s*\/?>)/gi, '&lt;') : s

/** plotlySafe applied to every string of traces or a layout, except identifiers used for selection. */
export function plotlySafeDeep<T>(v: T, key = ''): T {
  if (typeof v === 'string') return plotlySafe(v) as T
  if (Array.isArray(v)) return (key === 'customdata' || key === 'ids' ? v : v.map((x) => plotlySafeDeep(x))) as T
  if (v && typeof v === 'object' && Object.getPrototypeOf(v) === Object.prototype) {
    const out: Record<string, unknown> = {}
    for (const [k, x] of Object.entries(v as Record<string, unknown>)) out[k] = k === 'customdata' || k === 'ids' ? x : plotlySafeDeep(x, k)
    return out as T
  }
  return v
}

export function toCsv(rows: Record<string, unknown>[], columns: { key: string; label: string }[]): string {
  const head = columns.map((c) => csvCell(c.label)).join(',')
  const body = rows.map((r) => columns.map((c) => csvCell(r[c.key])).join(','))
  return [head, ...body].join('\n') + '\n'
}

export function escapeHtml(s: unknown): string {
  return String(s ?? '').replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]!)
}

/** Standalone HTML table document (export / print); every value escaped. */
export function toHtmlTable(title: string, rows: Record<string, unknown>[], columns: { key: string; label: string }[]): string {
  const th = columns.map((c) => `<th>${escapeHtml(c.label)}</th>`).join('')
  const trs = rows.map((r) => `<tr>${columns.map((c) => `<td>${escapeHtml(r[c.key])}</td>`).join('')}</tr>`).join('\n')
  return `<!doctype html><html><head><meta charset="utf-8"><title>${escapeHtml(title)}</title>
<style>body{font-family:system-ui,sans-serif;margin:24px;color:#18181b}table{border-collapse:collapse;font-size:12px}
th,td{border:1px solid #d4d4d8;padding:4px 8px;text-align:left;vertical-align:top}th{background:#f4f4f5}</style></head>
<body><h2>${escapeHtml(title)}</h2><table><thead><tr>${th}</tr></thead><tbody>${trs}</tbody></table></body></html>`
}

export function slug(s: string) {
  return s.replace(/[^A-Za-z0-9_.-]+/g, '_').replace(/^_+|_+$/g, '') || 'flame'
}
