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

/** CSV cell: quote when needed and neutralise spreadsheet formulas. */
export function csvCell(value: unknown): string {
  let s = value == null ? '' : String(value)
  if (/^[=+\-@]/.test(s)) s = `'${s}`
  return /[",\n\r]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s
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
