// Kaplan–Meier curves as tables: one row per time and group (at risk, events, censored, survival and its 95% CI),
// the numbers at risk at the axis ticks, and a summary per group with the tests. TSV, CSV or Excel.
import { buildXlsx } from '../../lib/xlsx'

export interface KmGroupData {
  group: string; n: number; events: number; median: number | null; time: number[]; surv: number[]; lower: (number | null)[]; upper: (number | null)[]
  nrisk?: number[]; nevent?: number[]; ncens?: number[]; atRisk: number[]
}
export interface KmData {
  groups: KmGroupData[]; ticks: number[]; logrank: { chisq: number; df: number; p: number }
  hr?: { hr: number; lo: number; hi: number; p: number; of: string; vs: string } | null
}
type Cell = string | number | null
export interface KmTable { name: string; columns: string[]; rows: Cell[][] }

/** The three tables: curves (long format), numbers at risk, summary. */
export function kmTables(r: KmData, timeLabel: string): KmTable[] {
  const curves: KmTable = {
    name: 'Curves', columns: ['Group', timeLabel, 'At risk', 'Events', 'Censored', 'Survival', '95% CI low', '95% CI high'],
    rows: r.groups.flatMap((g) => g.time.map((t, i) => [g.group, t, g.nrisk?.[i] ?? null, g.nevent?.[i] ?? null, g.ncens?.[i] ?? null, g.surv[i], g.lower[i], g.upper[i]])),
  }
  const risk: KmTable = { name: 'Number at risk', columns: ['Group', ...r.ticks.map((t) => `${timeLabel} ${t}`)], rows: r.groups.map((g) => [g.group, ...g.atRisk]) }
  const summary: KmTable = {
    name: 'Summary', columns: ['Group', 'Samples', 'Events', 'Median survival', 'Log-rank chi-square', 'df', 'Log-rank p', 'Hazard ratio', 'HR 95% CI low', 'HR 95% CI high', 'Cox p'],
    rows: r.groups.map((g, i) => [g.group, g.n, g.events, g.median ?? 'not reached',
      ...(i === 0 ? [r.logrank.chisq, r.logrank.df, r.logrank.p] : [null, null, null]),
      ...(r.hr && g.group === r.hr.of ? [r.hr.hr, r.hr.lo, r.hr.hi, r.hr.p] : r.hr && g.group === r.hr.vs ? ['reference', null, null, null] : [null, null, null, null])]),
  }
  return [curves, risk, summary]
}

const quote = (v: Cell, sep: string) => {
  const s = v == null ? '' : String(v)
  return sep === ',' && /[",\r\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : sep === '\t' ? s.replace(/[\t\r\n]+/g, ' ') : s
}
export const kmDelimited = (t: KmTable, sep: ',' | '\t') => [t.columns, ...t.rows].map((r) => r.map((v) => quote(v, sep)).join(sep)).join('\r\n') + '\r\n'

export function kmXlsx(tables: KmTable[]): Uint8Array {
  return buildXlsx(tables.map((t) => ({ name: t.name, columns: t.columns, rows: t.rows.map((r) => Object.fromEntries(t.columns.map((c, i) => [c, r[i] ?? '']))) })))
}
