import { describe, expect, it } from 'vitest'
import { csvCell, plotlySafe, plotlySafeDeep, tsvCell } from './format'

describe('spreadsheet cells', () => {
  it('neutralises formulas but keeps numbers', () => {
    expect(csvCell('=HYPERLINK("x")')).toBe(`"'=HYPERLINK(""x"")"`)
    expect(csvCell('\tcmd')).toBe(`'\tcmd`)
    expect(csvCell(-1.25)).toBe('-1.25')
    expect(tsvCell('a\tb\nc')).toBe('a b c')
    expect(tsvCell('@SUM(A1)')).toBe(`'@SUM(A1)`)
    expect(tsvCell(-3)).toBe('-3')
  })
})

describe('Plotly labels', () => {
  it('keeps our formatting tags and shows other markup as text', () => {
    expect(plotlySafe('<b>FKBP5</b><br>HR 2<extra></extra>')).toBe('<b>FKBP5</b><br>HR 2<extra></extra>')
    expect(plotlySafe('<a href="https://evil.example">x</a>')).toBe('&lt;a href="https://evil.example">x&lt;/a>')
    expect(plotlySafe('<span style="font-size:90px">x')).toBe('&lt;span style="font-size:90px">x')
  })
  it('leaves selection identifiers untouched', () => {
    const t = plotlySafeDeep({ text: ['<a>'], customdata: ['<a>'], marker: { size: [1, 2] } })
    expect(t).toEqual({ text: ['&lt;a>'], customdata: ['<a>'], marker: { size: [1, 2] } })
  })
})
