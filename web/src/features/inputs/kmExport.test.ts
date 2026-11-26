import { strFromU8, unzipSync } from 'fflate'
import { describe, expect, it } from 'vitest'
import { kmDelimited, kmTables, kmXlsx, type KmData } from './kmExport'

const r: KmData = {
  ticks: [0, 20], logrank: { chisq: 10.5, df: 1, p: 0.0012 }, hr: { hr: 1.77, lo: 1.25, hi: 2.52, p: 0.0014, of: 'High, "fast"', vs: 'Low' },
  groups: [
    { group: 'Low', n: 3, events: 2, median: 12, time: [5, 12, 30], surv: [0.667, 0.333, 0.333], lower: [0.1, 0.01, 0.01], upper: [0.9, 0.8, 0.8], nrisk: [3, 2, 1], nevent: [1, 1, 0], ncens: [0, 0, 1], atRisk: [3, 1] },
    { group: 'High, "fast"', n: 2, events: 1, median: null, time: [8, 40], surv: [0.5, 0.5], lower: [0.05, 0.05], upper: [0.9, 0.9], nrisk: [2, 1], nevent: [1, 0], ncens: [0, 1], atRisk: [2, 1] },
  ],
}

describe('Kaplan–Meier curves as tables', () => {
  const [curves, risk, summary] = kmTables(r, 'Time (OS_MONTHS)')
  it('one row per time and group, with the counts and the CI', () => {
    expect(curves.columns).toEqual(['Group', 'Time (OS_MONTHS)', 'At risk', 'Events', 'Censored', 'Survival', '95% CI low', '95% CI high'])
    expect(curves.rows).toHaveLength(5)
    expect(curves.rows[2]).toEqual(['Low', 30, 1, 0, 1, 0.333, 0.01, 0.8])
    expect(risk.rows[0]).toEqual(['Low', 3, 1])
    expect(summary.rows[0]).toEqual(['Low', 3, 2, 12, 10.5, 1, 0.0012, 'reference', null, null, null])
    expect(summary.rows[1].slice(3)).toEqual(['not reached', null, null, null, 1.77, 1.25, 2.52, 0.0014])
  })
  it('CSV quotes, TSV and Excel files', () => {
    expect(kmDelimited(curves, ',')).toContain('"High, ""fast""",8,2,1,0,0.5')
    expect(kmDelimited(curves, '\t').split('\r\n')[1]).toBe('Low\t5\t3\t1\t0\t0.667\t0.1\t0.9')
    const x = unzipSync(kmXlsx([curves, risk, summary]))
    expect(Object.keys(x)).toContain('xl/worksheets/sheet3.xml')
    expect(strFromU8(x['xl/workbook.xml'])).toContain('Number at risk')
  })
})
