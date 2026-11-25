import { describe, expect, it } from 'vitest'
import { applyChartLabels, applyChartText, chartLabels, DEFAULT_CHART_TEXT, NO_LABELS, parseColor, sanitizeChartText, type ChartText } from './chartText'

const t = (o: Partial<ChartText>): ChartText => ({ ...DEFAULT_CHART_TEXT, ...o })
type F = { size?: number; weight?: string; style?: string; family?: string; color?: string }
type Ax = { title: { text: string; font: F }; tickfont: F; tickangle?: number; gridcolor?: string }

describe('text of all charts', () => {
  it('styles the title, every x / y axis title and tick labels, and the legend', () => {
    const l = applyChartText({ title: 'Volcano', xaxis: { title: { text: 'log2 FC' }, gridcolor: '#eee' }, yaxis: { title: '−log10 p' }, yaxis2: { title: { text: 'Count', font: { color: 'red' } } } },
      t({ family: 'Arial', tickAngle: 45, xTitle: { size: 16, bold: true, italic: false, color: '#aa0000' }, yTitle: { size: 0, bold: false, italic: true, color: '' }, xTick: { size: 11, bold: false, italic: false, color: '' }, yTick: { size: 9, bold: true, italic: false, color: '#1f77b4' }, legend: { size: 14, bold: true, italic: false, color: '' } })) as unknown as
      { title: { text: string; font: F }; xaxis: Ax; yaxis: Ax; yaxis2: Ax; legend: { font: F }; font: F }
    expect(l.title).toMatchObject({ text: 'Volcano', font: { weight: 'bold', family: 'Arial' } })   // the title is bold by default
    expect(l.xaxis).toMatchObject({ title: { text: 'log2 FC', font: { size: 16, weight: 'bold' } }, tickfont: { size: 11 }, tickangle: -45, gridcolor: '#eee' })
    expect(l.yaxis.title).toMatchObject({ text: '−log10 p', font: { style: 'italic', family: 'Arial' } })
    expect(l.yaxis.title.font.size).toBeUndefined()                                                  // Auto keeps the chart's size
    expect(l.yaxis.tickangle).toBeUndefined()
    expect(l.yaxis.tickfont).toMatchObject({ size: 9, weight: 'bold', color: '#1f77b4' })           // y ticks: their own style and colour
    expect(l.xaxis.title.font.color).toBe('#aa0000')
    expect(l.xaxis.tickfont.color).toBeUndefined()                                                   // no colour chosen: the chart's own grey                                                        // the angle is for x labels only
    expect(l.yaxis2.title.font).toMatchObject({ color: 'red', style: 'italic' })                     // the chart's own font is kept underneath
    expect(l.legend.font).toMatchObject({ size: 14, weight: 'bold' })
    expect(l.font.family).toBe('Arial')
  })
  it('leaves a chart as drawn with the defaults (no axis gains a title it did not have)', () => {
    const l = applyChartText({ xaxis: { range: [0, 1] }, font: { size: 12 } }, DEFAULT_CHART_TEXT) as unknown as { xaxis: Record<string, unknown>; font: F; title?: unknown }
    expect(l.xaxis.title).toBeUndefined()
    expect(l.xaxis.tickfont).toEqual({})
    expect(l.font).toEqual({ size: 12 })
    expect(l.title).toBeUndefined()
  })
  it('stored or session values are checked field by field', () => {
    const s = sanitizeChartText({ family: 'Comic Sans', tickAngle: 33, title: { size: 500, bold: 'yes' }, tick: { size: 14, italic: true } })   // saved before x / y ticks were separate
    expect(s.family).toBe('')
    expect(s.tickAngle).toBe(0)
    expect(s.title).toEqual(DEFAULT_CHART_TEXT.title)
    expect(s.xTick).toEqual({ size: 14, bold: false, italic: true, color: '' })
    expect(s.yTick).toEqual({ size: 14, bold: false, italic: true, color: '' })
    expect(sanitizeChartText({ legend: { color: '#ABCDEF' } }).legend.color).toBe('#abcdef')
    expect(sanitizeChartText({ legend: { color: 'red; x' } }).legend.color).toBe('')
    expect(sanitizeChartText(null)).toEqual(DEFAULT_CHART_TEXT)
  })
  it("a chart's own title and axis titles can be replaced, and a title added where there was none", () => {
    const base = { xaxis: { title: { text: 'Gene ratio' } }, yaxis: {}, margin: { t: 10 } }
    expect(chartLabels(base)).toEqual({ title: '', yTitle: '', xTitle: 'Gene ratio' })
    const l = applyChartLabels(base, { title: ' GO enrichment ', yTitle: 'Term', xTitle: '' }, DEFAULT_CHART_TEXT) as unknown as
      { title: { text: string; font: F }; yaxis: { title: { text: string } }; xaxis: { title: { text: string } }; margin: { t: number } }
    expect(l.title).toMatchObject({ text: 'GO enrichment', font: { weight: 'bold', size: 17 } })
    expect(l.margin.t).toBeGreaterThan(40)
    expect(l.yaxis.title.text).toBe('Term')
    expect(l.xaxis.title.text).toBe('Gene ratio')                                                   // empty: the chart's own
    expect(applyChartLabels(base, NO_LABELS, DEFAULT_CHART_TEXT)).toEqual(base)
  })
  it('colours typed as hex or RGB', () => {
    expect(parseColor('#C0392B')).toBe('#c0392b')
    expect(parseColor('c0392b')).toBe('#c0392b')
    expect(parseColor('#f00')).toBe('#ff0000')
    expect(parseColor('rgb(37, 99, 235)')).toBe('#2563eb')
    expect(parseColor('37 99 235')).toBe('#2563eb')
    expect(parseColor('rgb(300, 0, 0)')).toBeNull()
    expect(parseColor('red')).toBeNull()
  })
})
