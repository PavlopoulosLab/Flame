import { strFromU8, unzipSync } from 'fflate'
import { describe, expect, it } from 'vitest'
import { buildXlsx, sheetsByCategory, termSheetRows } from './xlsx'

const terms = [
  { source: 'GO:BP', id: 'GO:0030334', name: 'regulation of cell migration', pvalue: 2.59e-7, log10p: 6.59, termSize: 950, intersection: 3, coverage: 0.3, geneRatio: 0.015, foldEnrichment: 3.2, genes: 'TGFBR2,CXCL12,S1PR1', link: 'https://example.org' },
  { source: 'KEGG', id: 'KEGG:04110', name: 'Cell cycle <& "quotes">', pvalue: 0.01, log10p: 2, termSize: 120, intersection: 2, coverage: 1.7, geneRatio: 0.01, foldEnrichment: null, genes: 'CDKN1A,GADD45A', link: null },
]

describe('xlsx writer', () => {
  it('writes one sheet per category with all genes and valid XML', () => {
    const sheets = sheetsByCategory(termSheetRows(terms), 'Source', 'All terms')
    expect(sheets.map((s) => s.name)).toEqual(['All terms', 'GO:BP', 'KEGG'])
    const bytes = buildXlsx(sheets)
    const files = unzipSync(bytes)
    const wb = strFromU8(files['xl/workbook.xml'])
    expect(wb).toContain('name="All terms"')
    expect(wb).toContain('name="GO_BP"')            // ":" is not allowed in sheet names
    const s1 = strFromU8(files['xl/worksheets/sheet1.xml'])
    expect(s1).toContain('TGFBR2, CXCL12, S1PR1')
    expect(s1).toContain('Cell cycle &lt;&amp; &quot;quotes&quot;&gt;')
    expect(s1).toContain('<v>2.59e-7</v>')          // numbers stay numbers
  })
  it('makes sheet names unique and short', () => {
    const files = unzipSync(buildXlsx([{ name: 'x'.repeat(40), rows: [] }, { name: 'x'.repeat(40), rows: [] }]))
    const wb = strFromU8(files['xl/workbook.xml'])
    expect(wb).toContain(`name="${'x'.repeat(31)}"`)
    expect(wb).toContain(`name="${'x'.repeat(27)} (2)"`)
  })
})
