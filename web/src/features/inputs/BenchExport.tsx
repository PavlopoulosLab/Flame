import Plotly from 'plotly.js-dist-min'
import { useMemo, useState } from 'react'
import { Field } from '../../components/ui'
import { delimited, download, fmtCell, htmlReport, jsonReport, markdownReport, pdfReport, resultTables, textReport, textTable, xlsxBytes, zipTables, type ExportResult } from './benchExport'

type Format = 'xlsx' | 'csv' | 'tsv' | 'txt' | 'md' | 'html' | 'pdf' | 'json'
const FORMATS: { id: Format; label: string; title: string }[] = [
  { id: 'xlsx', label: 'Excel', title: 'One sheet per table, plus the methods' },
  { id: 'csv', label: 'CSV', title: 'Comma-separated (one table, or a zip of all)' },
  { id: 'tsv', label: 'TSV', title: 'Tab-separated (one table, or a zip of all)' },
  { id: 'txt', label: 'TXT', title: 'Plain-text report with aligned tables' },
  { id: 'md', label: 'Markdown', title: 'Markdown tables (e.g. for an electronic lab notebook)' },
  { id: 'html', label: 'HTML', title: 'A web page with the figure and the tables' },
  { id: 'pdf', label: 'PDF', title: 'A report with the test, methods, figure and tables' },
  { id: 'json', label: 'JSON', title: 'Machine-readable, for scripts' },
]

/** The figure on screen as a PNG data URL (for the PDF and HTML reports). */
async function figurePng(): Promise<string | null> {
  const el = document.querySelector('.bench-figure .plot') as HTMLElement | null
  if (!el || !el.clientWidth) return null
  try { return await Plotly.toImage(el as unknown as Plotly.PlotlyHTMLElement, { format: 'png', width: el.clientWidth, height: el.clientHeight, scale: 2 }) } catch { return null }
}

/** Export the statistics as tables, in many formats, or copy one table to paste into Excel or Prism. */
export function BenchExport({ res, name = '' }: { res: ExportResult; /** One gene / readout shown: in the file names. */ name?: string }) {
  const tables = useMemo(() => resultTables(res), [res])
  const [which, setWhich] = useState('all')
  const [busy, setBusy] = useState<Format | null>(null)
  const [copied, setCopied] = useState(false)
  const chosen = which === 'all' ? tables : tables.filter((t) => t.id === which)
  const one = chosen.length === 1 ? chosen[0] : null
  const save = async (f: Format) => {
    setBusy(f)
    try {
      const base = `flame_experimental_${name ? `${name.replace(/[^\w.-]+/g, '_').slice(0, 40)}_` : ''}${which}`
      if (f === 'xlsx') download(xlsxBytes(res, chosen), `${base}.xlsx`, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
      else if (f === 'csv' || f === 'tsv') {
        const sep = f === 'csv' ? ',' : '\t'
        if (one) download('﻿' + delimited(one, sep), `${base}.${f}`, f === 'csv' ? 'text/csv;charset=utf-8' : 'text/tab-separated-values;charset=utf-8')
        else download(zipTables(chosen, sep), `${base}_${f}.zip`, 'application/zip')
      } else if (f === 'txt') download(textReport(res, chosen), `${base}.txt`, 'text/plain;charset=utf-8')
      else if (f === 'md') download(markdownReport(res, chosen), `${base}.md`, 'text/markdown;charset=utf-8')
      else if (f === 'json') download(jsonReport(res, chosen), `${base}.json`, 'application/json')
      else if (f === 'html') download(htmlReport(res, chosen, await figurePng()), `${base}.html`, 'text/html;charset=utf-8')
      else if (f === 'pdf') download(await pdfReport(res, chosen, await figurePng()), `${base}.pdf`, 'application/pdf')
    } finally { setBusy(null) }
  }
  const copy = async () => {
    const t = one ?? tables[0]
    try { await navigator.clipboard.writeText(delimited(t, '\t')); setCopied(true); setTimeout(() => setCopied(false), 1500) } catch { /* clipboard blocked */ }
  }
  return (
    <div className="card stack-sm bench-export">
      <div className="row wrap">
        <strong className="small">Export the results</strong>
        <Field label="Table">
          <select className="input input-sm" aria-label="Table to export" value={which} onChange={(e) => setWhich(e.target.value)}>
            <option value="all">Everything (all tables)</option>
            {tables.map((t) => <option key={t.id} value={t.id}>{t.title}</option>)}
          </select>
        </Field>
        <span className="row wrap" role="group" aria-label="Export format">
          {FORMATS.map((f) => (
            <button key={f.id} type="button" className="btn btn-secondary btn-sm" title={f.title} disabled={!!busy} onClick={() => save(f.id)}>{busy === f.id ? '…' : f.label}</button>
          ))}
        </span>
        <button type="button" className="btn btn-ghost btn-sm" title="Copy the table (tab-separated) to paste into Excel, Prism or a lab notebook" onClick={copy}>{copied ? 'Copied' : 'Copy as table'}</button>
      </div>
      <p className="muted small">{which === 'all' ? 'Everything: comparisons, descriptive statistics, values' : chosen[0]?.title}
        {which === 'all' && tables.length > 3 ? `, ${tables.slice(3).map((t) => t.title.toLowerCase()).join(', ')}` : ''}.
        {' '}CSV and TSV of everything come as a zip (one file per table); the PDF and HTML reports include the figure.{one ? '' : ' Copy as table copies the comparisons.'}</p>
      {copied && <span className="sr-only" role="status">Copied</span>}
      <details className="small">
        <summary>Preview as text</summary>
        <pre className="mono" style={{ overflowX: 'auto', fontSize: 11 }}>{(one ? [one] : chosen.slice(0, 2)).map((t) => `${t.title}\n${textTable(t)}`).join('\n\n')}</pre>
      </details>
    </div>
  )
}

/** One result table on screen (numbers right-aligned, p-values formatted as in the exports). */
export function TableView({ res, id }: { res: ExportResult; id: string }) {
  const t = useMemo(() => resultTables(res).find((x) => x.id === id), [res, id])
  if (!t || !t.rows.length) return null
  return (
    <div className="table-scroll fetch-table"><table className="table compact">
      <thead><tr>{t.cols.map((c) => <th key={c.key} className={c.kind === 'text' ? undefined : 'num'}>{c.label}</th>)}</tr></thead>
      <tbody>{t.rows.map((r, i) => <tr key={i}>{t.cols.map((c) => <td key={c.key} className={c.kind === 'text' ? undefined : 'num'}>{fmtCell(r[c.key], c.kind)}</td>)}</tr>)}</tbody>
    </table></div>
  )
}
