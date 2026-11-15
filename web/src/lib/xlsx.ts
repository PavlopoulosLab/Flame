import { strToU8, zipSync } from 'fflate'

/**
 * Minimal Excel (.xlsx) writer: several sheets of rows, bold frozen header,
 * numbers stored as numbers, column widths fitted to the content.
 */
export type Row = Record<string, unknown>
export interface Sheet { name: string; rows: Row[]; columns?: string[] }

const esc = (s: string) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')
  // remove characters XML 1.0 does not allow
  // eslint-disable-next-line no-control-regex
  .replace(/[\u0000-\u0008\u000B\u000C\u000E-\u001F]/g, '')

const colName = (i: number) => { let s = ''; i += 1; while (i > 0) { const m = (i - 1) % 26; s = String.fromCharCode(65 + m) + s; i = Math.floor((i - 1) / 26) } return s }

/** Sheet names: at most 31 characters, no []:*?/\ and unique within the workbook. */
function sheetNames(names: string[]) {
  const used = new Set<string>()
  return names.map((n) => {
    const base = (n.replace(/[[\]:*?/\\]/g, '_').trim() || 'Sheet').slice(0, 31)
    let name = base, k = 2
    while (used.has(name.toLowerCase())) { const suf = ` (${k++})`; name = base.slice(0, 31 - suf.length) + suf }
    used.add(name.toLowerCase())
    return name
  })
}

function cell(ref: string, v: unknown, style = 0) {
  const s = style ? ` s="${style}"` : ''
  if (v === null || v === undefined || v === '') return ''
  if (typeof v === 'number' && Number.isFinite(v)) return `<c r="${ref}"${s}><v>${v}</v></c>`
  if (typeof v === 'boolean') return `<c r="${ref}" t="b"${s}><v>${v ? 1 : 0}</v></c>`
  const text = Array.isArray(v) ? v.join(', ') : typeof v === 'object' ? JSON.stringify(v) : String(v)
  return `<c r="${ref}" t="inlineStr"${s}><is><t xml:space="preserve">${esc(text.slice(0, 32767))}</t></is></c>`
}

function sheetXml(sheet: Sheet) {
  const cols = sheet.columns ?? [...new Set(sheet.rows.flatMap((r) => Object.keys(r)))]
  const width = cols.map((c) => Math.min(60, Math.max(c.length, ...sheet.rows.slice(0, 200).map((r) => String(r[c] ?? '').length)) + 2))
  const head = `<row r="1">${cols.map((c, j) => cell(`${colName(j)}1`, c, 1)).join('')}</row>`
  const body = sheet.rows.map((r, i) => `<row r="${i + 2}">${cols.map((c, j) => cell(`${colName(j)}${i + 2}`, r[c])).join('')}</row>`).join('')
  return '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
    '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">' +
    '<sheetViews><sheetView workbookViewId="0"><pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/></sheetView></sheetViews>' +
    `<cols>${width.map((w, j) => `<col min="${j + 1}" max="${j + 1}" width="${w}" customWidth="1"/>`).join('')}</cols>` +
    `<sheetData>${head}${body}</sheetData>` +
    (cols.length ? `<autoFilter ref="A1:${colName(cols.length - 1)}${sheet.rows.length + 1}"/>` : '') +
    '</worksheet>'
}

/** Build the workbook as bytes. */
export function buildXlsx(sheets: Sheet[]): Uint8Array {
  const list = sheets.length ? sheets : [{ name: 'Sheet1', rows: [] }]
  const names = sheetNames(list.map((s) => s.name))
  const files: Record<string, Uint8Array> = {
    '[Content_Types].xml': strToU8('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">' +
      '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>' +
      '<Default Extension="xml" ContentType="application/xml"/>' +
      '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>' +
      '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>' +
      list.map((_, i) => `<Override PartName="/xl/worksheets/sheet${i + 1}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>`).join('') +
      '</Types>'),
    '_rels/.rels': strToU8('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>'),
    'xl/workbook.xml': strToU8('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets>' +
      names.map((n, i) => `<sheet name="${esc(n)}" sheetId="${i + 1}" r:id="rId${i + 1}"/>`).join('') + '</sheets></workbook>'),
    'xl/_rels/workbook.xml.rels': strToU8('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
      list.map((_, i) => `<Relationship Id="rId${i + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet${i + 1}.xml"/>`).join('') +
      `<Relationship Id="rId${list.length + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/></Relationships>`),
    'xl/styles.xml': strToU8('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">' +
      '<fonts count="2"><font><sz val="11"/><name val="Calibri"/></font><font><b/><sz val="11"/><name val="Calibri"/></font></fonts>' +
      '<fills count="2"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill></fills>' +
      '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>' +
      '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>' +
      '<cellXfs count="2"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/><xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/></cellXfs>' +
      '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>' +
      '</styleSheet>'),
  }
  list.forEach((s, i) => { files[`xl/worksheets/sheet${i + 1}.xml`] = strToU8(sheetXml(s)) })
  return zipSync(files, { level: 6 })
}

/** Download a workbook. */
export function saveXlsx(sheets: Sheet[], filename: string) {
  const bytes = buildXlsx(sheets)
  const blob = new Blob([bytes as BlobPart], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url; a.download = filename.endsWith('.xlsx') ? filename : `${filename}.xlsx`
  document.body.appendChild(a); a.click(); a.remove()
  setTimeout(() => URL.revokeObjectURL(url), 30_000)
}

/** An "All" sheet plus one sheet per category (e.g. annotation source: GO:BP, KEGG, REAC…). */
export function sheetsByCategory(rows: Row[], key: string, allName = 'All', columns?: string[]): Sheet[] {
  const cats = [...new Set(rows.map((r) => String(r[key] ?? 'Other')))]
  const sheets: Sheet[] = [{ name: allName, rows, columns }]
  if (cats.length > 1) cats.forEach((c) => sheets.push({ name: c, rows: rows.filter((r) => String(r[key] ?? 'Other') === c), columns }))
  return sheets
}

/** Spreadsheet rows for enrichment terms: every gene of each term in the "Genes" column. */
export function termSheetRows(terms: { source: string; id: string; name: string; pvalue: number; log10p?: number; termSize?: number; intersection?: number;
  coverage?: number; geneRatio?: number; foldEnrichment?: number | null; nes?: number | null; genes: string; link?: string | null }[]): Row[] {
  return terms.map((t) => ({
    Source: t.source, 'Term ID': t.id, Term: t.name, 'P-value': t.pvalue, '-log10 p': t.log10p,
    ...(t.nes !== undefined && t.nes !== null ? { NES: t.nes } : {}),
    'Term size': t.termSize, 'Genes found': t.intersection, 'Coverage %': t.coverage, 'Gene ratio': t.geneRatio,
    'Fold enrichment': t.foldEnrichment ?? '', Genes: (t.genes ?? '').split(/[,;]\s*/).filter(Boolean).join(', '), Link: t.link ?? '',
  }))
}
