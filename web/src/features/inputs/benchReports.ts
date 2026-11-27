// Bench Data: the result tables and their export (Excel, CSV, TSV, TXT, Markdown, HTML, PDF, JSON).
import { strToU8, zipSync } from 'fflate'
import { buildXlsx } from '../../lib/xlsx'

/** What the export needs from a bench result. */
export interface ExportResult {
  groups: string[]; control: string; logScale: boolean; sentence: string; methods: string; hypothetical?: number
  across?: 'holm' | 'bh' | 'none'
  anova2?: { effect: string; F: number; df1: number; df2: number; p: number }[] | null
  targets: {
    target: string; error?: string; notes?: string[]
    values: { group: string; sample: string; value: number }[]
    summary: { group: string; n: number; mean: number; sd: number | null; sem: number | null }[]
    omnibus?: { name: string; p: number; pAcross?: number; detail?: string }
    posthoc?: string | null
    comparisons?: { a: string; b: string; diff: number; lo: number | null; hi: number | null; p: number; stars: string; fold?: number }[]
    assumptions?: { normality: { group: string; n: number; p: number | null }[]; equalSD?: { name: string; p: number } | null }
  }[]
}

export type Kind = 'text' | 'num' | 'p' | 'int'
export interface Col { key: string; label: string; kind: Kind }
export interface Table { id: string; title: string; cols: Col[]; rows: Record<string, string | number | null>[] }

const T975 = [12.706, 4.303, 3.182, 2.776, 2.571, 2.447, 2.365, 2.306, 2.262, 2.228, 2.201, 2.179, 2.160, 2.145, 2.131, 2.120, 2.110, 2.101, 2.093, 2.086,
  2.080, 2.074, 2.069, 2.064, 2.060, 2.056, 2.052, 2.048, 2.045, 2.042]
const t975 = (df: number) => (df < 1 ? NaN : df <= 30 ? T975[df - 1] : 1.96 + 2.4 / df)
const fin = (x: number | null | undefined) => (x == null || !Number.isFinite(x) ? null : x)

/** All the tables of a result: comparisons, descriptive statistics, values, two-way ANOVA and assumptions (when present). */
export function resultTables(res: ExportResult): Table[] {
  const tested = res.targets.filter((t) => t.comparisons?.length)
  const across = tested.some((t) => t.omnibus?.pAcross != null)
  // Rows (or groups) that could not be tested, with the reason: so a table is never silently empty.
  const notTested = res.targets.flatMap((t) => [...(t.error ? [t.error] : []), ...(t.error ? [] : t.notes ?? [])].map((note) => ({ target: t.target, note })))
  const acrossLabel = res.across === 'bh' ? 'q (BH FDR across rows)' : 'p (Holm across rows)'
  const comparisons: Table = {
    id: 'comparisons', title: 'Comparisons',
    cols: [{ key: 'target', label: 'Gene / readout', kind: 'text' }, { key: 'comparison', label: 'Comparison', kind: 'text' },
      ...(res.logScale ? [{ key: 'fold', label: 'Fold change', kind: 'num' as Kind }, { key: 'log2fc', label: 'log2 fold change', kind: 'num' as Kind }]
        : [{ key: 'diff', label: 'Difference', kind: 'num' as Kind }]),
      { key: 'lo', label: '95% CI low', kind: 'num' }, { key: 'hi', label: '95% CI high', kind: 'num' }, { key: 'p', label: 'p', kind: 'p' },
      { key: 'stars', label: 'Summary', kind: 'text' }, ...(across ? [{ key: 'across', label: acrossLabel, kind: 'p' as Kind }] : []),
      { key: 'test', label: 'Test', kind: 'text' }, { key: 'stat', label: 'Test statistic', kind: 'text' }, { key: 'testp', label: 'Test p', kind: 'p' }, { key: 'posthoc', label: 'Multiple comparisons', kind: 'text' },
      ...(notTested.length ? [{ key: 'note', label: 'Note', kind: 'text' as Kind }] : [])],
    rows: [...tested.flatMap((t) => t.comparisons!.map((c) => ({
      target: t.target, comparison: `${c.a} vs ${c.b === 'value' ? res.hypothetical : c.b}`,
      ...(res.logScale ? { fold: fin(c.fold), log2fc: fin(c.diff), lo: fin(c.lo == null ? null : 2 ** c.lo), hi: fin(c.hi == null ? null : 2 ** c.hi) }
        : { diff: fin(c.diff), lo: fin(c.lo), hi: fin(c.hi) }),
      p: fin(c.p), stars: c.stars, ...(across ? { across: fin(t.omnibus?.pAcross) } : {}),
      test: t.omnibus?.name ?? '', stat: t.omnibus?.detail ?? '', testp: fin(t.omnibus?.p), posthoc: t.posthoc ?? '',
    }))), ...notTested.map((x) => ({ target: x.target, comparison: 'Not tested', note: x.note }))],
  }
  const descriptive: Table = {
    id: 'descriptive', title: 'Descriptive statistics',
    cols: [{ key: 'target', label: 'Gene / readout', kind: 'text' }, { key: 'group', label: 'Group', kind: 'text' }, { key: 'n', label: 'n', kind: 'int' },
      { key: 'mean', label: 'Mean', kind: 'num' }, { key: 'sd', label: 'SD', kind: 'num' }, { key: 'sem', label: 'SEM', kind: 'num' },
      { key: 'cilo', label: '95% CI of mean, low', kind: 'num' }, { key: 'cihi', label: '95% CI of mean, high', kind: 'num' },
      { key: 'median', label: 'Median', kind: 'num' }, { key: 'min', label: 'Minimum', kind: 'num' }, { key: 'max', label: 'Maximum', kind: 'num' }],
    rows: res.targets.flatMap((t) => res.groups.map((g) => {
      const v = t.values.filter((x) => x.group === g).map((x) => x.value).sort((a, b) => a - b)
      const n = v.length, m = n ? v.reduce((a, b) => a + b, 0) / n : NaN
      const sd = n > 1 ? Math.sqrt(v.reduce((a, b) => a + (b - m) ** 2, 0) / (n - 1)) : NaN, sem = sd / Math.sqrt(n)
      const med = n ? (n % 2 ? v[(n - 1) / 2] : (v[n / 2 - 1] + v[n / 2]) / 2) : NaN
      return { target: t.target, group: g, n, mean: fin(m), sd: fin(sd), sem: fin(sem), cilo: fin(m - t975(n - 1) * sem), cihi: fin(m + t975(n - 1) * sem),
        median: fin(med), min: fin(v[0]), max: fin(v[n - 1]) }
    })),
  }
  const values: Table = {
    id: 'values', title: res.logScale ? `Values (fold change vs ${res.control})` : 'Values',
    cols: [{ key: 'target', label: 'Gene / readout', kind: 'text' }, { key: 'group', label: 'Group', kind: 'text' }, { key: 'sample', label: 'Sample', kind: 'text' },
      { key: 'value', label: res.logScale ? 'Fold change' : 'Value', kind: 'num' }],
    rows: res.targets.flatMap((t) => t.values.map((v) => ({ target: t.target, group: v.group, sample: v.sample, value: fin(v.value) }))),
  }
  const out = [comparisons, descriptive, values]
  if (res.anova2?.length) out.push({
    id: 'anova2', title: 'Two-way ANOVA',
    cols: [{ key: 'effect', label: 'Source of variation', kind: 'text' }, { key: 'F', label: 'F', kind: 'num' }, { key: 'df1', label: 'DFn', kind: 'int' },
      { key: 'df2', label: 'DFd', kind: 'int' }, { key: 'p', label: 'p', kind: 'p' }],
    rows: res.anova2.map((r) => ({ effect: r.effect, F: fin(r.F), df1: r.df1, df2: r.df2, p: fin(r.p) })),
  })
  const withA = res.targets.filter((t) => t.assumptions)
  if (withA.length) out.push({
    id: 'assumptions', title: 'Assumptions (normality and equal SDs)',
    cols: [{ key: 'target', label: 'Gene / readout', kind: 'text' }, { key: 'group', label: 'Group', kind: 'text' }, { key: 'n', label: 'n', kind: 'int' },
      { key: 'shapiro', label: 'Shapiro-Wilk p', kind: 'p' }, { key: 'sdtest', label: 'Equal-SD test', kind: 'text' }, { key: 'sdp', label: 'Equal-SD p', kind: 'p' }],
    rows: withA.flatMap((t) => t.assumptions!.normality.map((nm, i) => ({ target: t.target, group: nm.group, n: nm.n, shapiro: fin(nm.p),
      sdtest: i === 0 ? t.assumptions!.equalSD?.name ?? '' : '', sdp: i === 0 ? fin(t.assumptions!.equalSD?.p) : null }))),
  })
  return out
}

// ---- formatting ----
export const fmtP = (p: number) => (p < 0.0001 ? '<0.0001' : p < 0.01 ? p.toPrecision(2) : p.toFixed(4))
export function fmtCell(v: string | number | null | undefined, kind: Kind): string {
  if (v == null || v === '') return ''
  if (typeof v === 'string') return v
  if (!Number.isFinite(v)) return ''
  if (kind === 'p') return fmtP(v)
  if (kind === 'int') return String(Math.round(v))
  const a = Math.abs(v)
  return a !== 0 && (a < 0.001 || a >= 1e6) ? v.toExponential(3) : String(Number(v.toPrecision(4)))
}
/** Raw values for machine-readable formats (numbers at full useful precision). */
const raw = (v: string | number | null | undefined) => (v == null ? '' : typeof v === 'number' ? (Number.isFinite(v) ? String(Number(v.toPrecision(8))) : '') : v)

/** CSV (RFC 4180: quotes when needed) or TSV (tabs and line breaks inside a cell become spaces). */
export function delimited(t: Table, sep: ',' | '\t'): string {
  const cell = (s: string) => (sep === '\t' ? s.replace(/[\t\r\n]+/g, ' ') : /[",\r\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s)
  return [t.cols.map((c) => cell(c.label)).join(sep), ...t.rows.map((r) => t.cols.map((c) => cell(raw(r[c.key]))).join(sep))].join('\r\n') + '\r\n'
}

/** A fixed-width text table (for .txt and the clipboard as text). */
export function textTable(t: Table): string {
  const cells = t.rows.map((r) => t.cols.map((c) => fmtCell(r[c.key], c.kind)))
  const w = t.cols.map((c, j) => Math.max(c.label.length, ...cells.map((r) => r[j].length)))
  const line = (vals: string[]) => vals.map((v, j) => (t.cols[j].kind === 'text' ? v.padEnd(w[j]) : v.padStart(w[j]))).join('  ').trimEnd()
  return [line(t.cols.map((c) => c.label)), w.map((n) => '-'.repeat(n)).join('  '), ...cells.map(line)].join('\n')
}

export function textReport(res: ExportResult, tables: Table[]): string {
  return [`Bench Data (Flame), ${new Date().toISOString().slice(0, 10)}`, '', `Test: ${res.sentence}`, '', `Methods: ${res.methods}`, '',
    ...tables.flatMap((t) => [t.title.toUpperCase(), textTable(t), ''])].join('\n')
}

const mdCell = (s: string) => s.replace(/\|/g, '\\|')
export function markdownReport(res: ExportResult, tables: Table[]): string {
  const md = (t: Table) => [`| ${t.cols.map((c) => mdCell(c.label)).join(' | ')} |`, `| ${t.cols.map((c) => (c.kind === 'text' ? '---' : '---:')).join(' | ')} |`,
    ...t.rows.map((r) => `| ${t.cols.map((c) => mdCell(fmtCell(r[c.key], c.kind))).join(' | ')} |`)].join('\n')
  return [`# Bench Data`, '', `**Test:** ${res.sentence}`, '', `**Methods:** ${res.methods}`, '', ...tables.flatMap((t) => [`## ${t.title}`, '', md(t), ''])].join('\n')
}

const h = (s: string) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')
export function htmlReport(res: ExportResult, tables: Table[], figurePng?: string | null): string {
  const tbl = (t: Table) => `<h2>${h(t.title)}</h2><table><thead><tr>${t.cols.map((c) => `<th class="${c.kind === 'text' ? '' : 'n'}">${h(c.label)}</th>`).join('')}</tr></thead><tbody>${
    t.rows.map((r) => `<tr>${t.cols.map((c) => `<td class="${c.kind === 'text' ? '' : 'n'}">${h(fmtCell(r[c.key], c.kind))}</td>`).join('')}</tr>`).join('')}</tbody></table>`
  return `<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Bench Data</title><style>
body{font-family:Arial,Helvetica,sans-serif;margin:24px;color:#111}h1{font-size:20px}h2{font-size:15px;margin-top:24px}
table{border-collapse:collapse;font-size:12px}th,td{border-bottom:1px solid #ddd;padding:4px 8px;text-align:left}th{background:#f3f4f6}.n{text-align:right}
img{max-width:100%;border:1px solid #eee}p{max-width:900px;font-size:13px}</style></head><body>
<h1>Bench Data</h1><p class="muted">Flame, ${new Date().toISOString().slice(0, 10)}</p><p><b>Test:</b> ${h(res.sentence)}</p><p><b>Methods:</b> ${h(res.methods)}</p>
${figurePng ? `<img alt="Figure" src="${figurePng}">` : ''}${tables.map(tbl).join('')}</body></html>`
}

export function jsonReport(res: ExportResult, tables: Table[]): string {
  return JSON.stringify({ test: res.sentence, methods: res.methods, tables: Object.fromEntries(tables.map((t) => [t.id, t.rows])) }, null, 2)
}

export function xlsxBytes(res: ExportResult, tables: Table[]): Uint8Array {
  return buildXlsx([...tables.map((t) => ({ name: t.title.slice(0, 31), columns: t.cols.map((c) => c.label),
    rows: t.rows.map((r) => Object.fromEntries(t.cols.map((c) => [c.label, r[c.key] ?? '']))) })),
  { name: 'Methods', rows: [{ Item: 'Test', Text: res.sentence }, { Item: 'Methods', Text: res.methods }] }])
}

/** Several CSV / TSV files in one zip. */
export function zipTables(tables: Table[], sep: ',' | '\t'): Uint8Array {
  const ext = sep === ',' ? 'csv' : 'tsv'
  return zipSync(Object.fromEntries(tables.map((t) => [`flame_${t.id}.${ext}`, strToU8(delimited(t, sep))])), { level: 6 })
}

// ---- PDF (pdf-lib, standard Helvetica: WinAnsi text) ----
const WIN = new Set('€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ'.split(''))
const GREEK: Record<string, string> = { 'Δ': 'Delta', 'ε': 'eps', 'κ': 'kappa', 'β': 'beta', 'α': 'alpha', 'μ': 'u', '≥': '>=', '≤': '<=', '−': '-', '→': '->', '√': 'sqrt', '²': '2', 'ρ': 'rho', '≈': '~' }
/** Text the standard PDF fonts can draw. */
export const pdfSafe = (s: string) => [...s].map((ch) => GREEK[ch] ?? (ch.charCodeAt(0) <= 0xff || WIN.has(ch) ? ch : '?')).join('')

export async function pdfReport(res: ExportResult, tables: Table[], figurePng?: string | null): Promise<Uint8Array> {
  const { PDFDocument, StandardFonts, rgb } = await import('pdf-lib')
  const doc = await PDFDocument.create()
  const font = await doc.embedFont(StandardFonts.Helvetica), bold = await doc.embedFont(StandardFonts.HelveticaBold)
  const W = 842, H = 595, M = 36                                        // A4 landscape, 0.5 inch margins
  let page = doc.addPage([W, H]); let y = H - M
  const newPage = () => { page = doc.addPage([W, H]); y = H - M }
  const need = (dy: number) => { if (y - dy < M) newPage() }
  const text = (s: string, size: number, f = font, x = M) => { page.drawText(pdfSafe(s), { x, y, size, font: f, color: rgb(0.07, 0.07, 0.07) }) }
  const wrap = (s: string, size: number, width: number, f = font) => {
    const words = pdfSafe(s).split(/\s+/); const lines: string[] = []; let cur = ''
    for (const w of words) { const nxt = cur ? `${cur} ${w}` : w; if (f.widthOfTextAtSize(nxt, size) > width && cur) { lines.push(cur); cur = w } else cur = nxt }
    if (cur) lines.push(cur); return lines
  }
  const para = (label: string, s: string) => { for (const [i, l] of wrap(`${label} ${s}`, 9.5, W - 2 * M).entries()) { need(13); text(l, 9.5, i === 0 ? font : font); y -= 13 } y -= 4 }
  text('Bench Data', 16, bold); y -= 16; text(`Flame, ${new Date().toISOString().slice(0, 10)}`, 9); y -= 18
  para('Test:', res.sentence); para('Methods:', res.methods)
  if (figurePng) {
    const img = await doc.embedPng(figurePng)
    const sc = Math.min((W - 2 * M) / img.width, 300 / img.height)
    need(img.height * sc + 8); page.drawImage(img, { x: M, y: y - img.height * sc, width: img.width * sc, height: img.height * sc }); y -= img.height * sc + 14
  }
  for (const t of tables) {
    const cells = t.rows.map((r) => t.cols.map((c) => pdfSafe(fmtCell(r[c.key], c.kind))))
    let size = 8
    const widths = (sz: number) => t.cols.map((c, j) => Math.max(bold.widthOfTextAtSize(pdfSafe(c.label), sz), ...cells.map((r) => font.widthOfTextAtSize(r[j], sz))) + 8)
    let w = widths(size); while (w.reduce((a, b) => a + b, 0) > W - 2 * M && size > 5) { size -= 0.5; w = widths(size) }
    const total = w.reduce((a, b) => a + b, 0), scale = Math.min(1, (W - 2 * M) / total); w = w.map((x) => x * scale)
    const row = (vals: string[], f: typeof font) => {
      let x = M
      vals.forEach((v, j) => {
        const fit = v.length && f.widthOfTextAtSize(v, size) > w[j] - 4 ? v.slice(0, Math.max(1, Math.floor(v.length * (w[j] - 4) / f.widthOfTextAtSize(v, size)) - 1)) + '…' : v
        const tw = f.widthOfTextAtSize(pdfSafe(fit), size)
        page.drawText(pdfSafe(fit), { x: t.cols[j].kind === 'text' ? x + 2 : x + w[j] - 2 - tw, y, size, font: f, color: rgb(0.07, 0.07, 0.07) }); x += w[j]
      })
    }
    const header = () => { row(t.cols.map((c) => pdfSafe(c.label)), bold); page.drawLine({ start: { x: M, y: y - 3 }, end: { x: M + w.reduce((a, b) => a + b, 0), y: y - 3 }, thickness: 0.6, color: rgb(0.5, 0.5, 0.5) }); y -= size + 6 }
    need(40); text(t.title, 11, bold); y -= 16; header()
    for (const r of cells) { if (y - size < M) { newPage(); header() } row(r, font); y -= size + 4 }
    y -= 14
  }
  return doc.save()
}

/** Save bytes or text as a file. */
export function download(data: Uint8Array | string, filename: string, type: string) {
  const blob = new Blob([typeof data === 'string' ? data : (data as BlobPart)], { type })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a'); a.href = url; a.download = filename
  document.body.appendChild(a); a.click(); a.remove()
  setTimeout(() => URL.revokeObjectURL(url), 30_000)
}
