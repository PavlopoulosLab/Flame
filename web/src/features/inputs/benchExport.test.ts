import { unzipSync, strFromU8 } from 'fflate'
import { describe, expect, it } from 'vitest'
import { delimited, htmlReport, jsonReport, markdownReport, pdfReport, pdfSafe, resultTables, textReport, textTable, xlsxBytes, zipTables, type ExportResult } from './benchExport'

const res: ExportResult = {
  groups: ['Control', 'Dex'], control: 'Control', logScale: true, across: 'holm',
  sentence: '2 groups: unpaired t test; p-values Holm-adjusted across targets.', methods: 'Ct values were normalised to GAPDH (ΔCt)…',
  targets: [
    { target: 'FKBP5', values: [{ group: 'Control', sample: 'C 1', value: 1 }, { group: 'Control', sample: 'C 2', value: 0.9 }, { group: 'Control', sample: 'C 3', value: 1.1 },
      { group: 'Dex', sample: 'D 1', value: 11 }, { group: 'Dex', sample: 'D 2', value: 12 }, { group: 'Dex', sample: 'D 3', value: 10 }],
      summary: [], omnibus: { name: 'unpaired t test', p: 0.00012, pAcross: 0.00024, detail: 't = 18.2, df = 4' },
      comparisons: [{ a: 'Dex', b: 'Control', diff: 3.46, lo: 3.1, hi: 3.8, p: 0.00012, stars: '***', fold: 11 }] },
    { target: 'IL6, "secreted"', values: [{ group: 'Control', sample: 'C 1', value: 1 }, { group: 'Dex', sample: 'D 1', value: 0.3 }], summary: [],
      omnibus: { name: 'unpaired t test', p: 0.03, pAcross: 0.03 }, comparisons: [{ a: 'Dex', b: 'Control', diff: -1.7, lo: null, hi: null, p: 0.03, stars: '*', fold: 0.31 }] },
  ],
}

describe('experimental analysis export', () => {
  const tables = resultTables(res)
  it('builds comparisons, descriptive statistics and values tables', () => {
    expect(tables.map((t) => t.id)).toEqual(['comparisons', 'descriptive', 'values'])
    const c = tables[0]
    expect(c.cols.map((x) => x.label)).toContain('p (Holm across rows)')
    expect(c.rows[0]).toMatchObject({ target: 'FKBP5', comparison: 'Dex vs Control', fold: 11, p: 0.00012, stat: 't = 18.2, df = 4' })
    expect(c.rows[0].lo).toBeCloseTo(2 ** 3.1)                                                        // CI back on the fold-change scale
    const d = tables[1].rows.find((r) => r.target === 'FKBP5' && r.group === 'Dex')!
    expect(d).toMatchObject({ n: 3, mean: 11, sd: 1, median: 11, min: 10, max: 12 })
    expect(d.cihi as number).toBeCloseTo(11 + 4.303 / Math.sqrt(3))                                    // t(0.975, 2)
  })
  it('CSV quotes commas and quotes; TSV keeps one line per row', () => {
    const csv = delimited(tables[0], ',')
    expect(csv).toContain('"IL6, ""secreted"""')
    expect(csv.split('\r\n')[0].startsWith('Gene / readout,Comparison,Fold change')).toBe(true)
    const tsv = delimited(tables[0], '\t')
    expect(tsv.trim().split('\r\n')).toHaveLength(3)
    expect(tsv.split('\r\n')[1].split('\t')).toHaveLength(tables[0].cols.length)
  })
  it('text, Markdown, HTML and JSON reports', () => {
    const lines = textTable(tables[0]).split('\n')
    expect(lines[2].indexOf('Dex vs Control')).toBe(lines[0].indexOf('Comparison'))                   // columns line up
    expect(textReport(res, tables)).toContain('DESCRIPTIVE STATISTICS')
    expect(markdownReport(res, tables)).toContain('| Gene / readout | Comparison |')
    expect(htmlReport(res, tables, 'data:image/png;base64,AAAA')).toContain('<img alt="Figure"')
    expect(JSON.parse(jsonReport(res, tables)).tables.comparisons[0].target).toBe('FKBP5')
  })
  it('Excel, zipped CSV and PDF files are valid', async () => {
    const x = unzipSync(xlsxBytes(res, tables))
    expect(Object.keys(x)).toContain('xl/worksheets/sheet4.xml')                                     // 3 tables + Methods
    const z = unzipSync(zipTables(tables, ','))
    expect(Object.keys(z).sort()).toEqual(['flame_comparisons.csv', 'flame_descriptive.csv', 'flame_values.csv'])
    expect(strFromU8(z['flame_values.csv'])).toContain('Fold change')
    const pdf = await pdfReport(res, tables)
    expect(new TextDecoder().decode(pdf.slice(0, 5))).toBe('%PDF-')
    expect(pdfSafe('ΔΔCt ≥ 0.05, ε = 0.8, Šídák')).toBe('DeltaDeltaCt >= 0.05, eps = 0.8, Šídák')
  })
})
