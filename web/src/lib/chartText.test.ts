import { describe, expect, it } from 'vitest'
import { applyChartText, DEFAULT_CHART_TEXT, sanitizeChartText, type ChartText } from './chartText'

const t = (o: Partial<ChartText>): ChartText => ({ ...DEFAULT_CHART_TEXT, ...o })
type F = { size?: number; weight?: string; style?: string; family?: string; color?: string }
type Ax = { title: { text: string; font: F }; tickfont: F; tickangle?: number; gridcolor?: string }

describe('text of all charts', () => {
  it('styles the title, every x / y axis title and tick labels, and the legend', () => {
    const l = applyChartText({ title: 'Volcano', xaxis: { title: { text: 'log2 FC' }, gridcolor: '#eee' }, yaxis: { title: '−log10 p' }, yaxis2: { title: { text: 'Count', font: { color: 'red' } } } },
      t({ family: 'Arial', tickAngle: 45, xTitle: { size: 16, bold: true, italic: false }, yTitle: { size: 0, bold: false, italic: true }, tick: { size: 11, bold: false, italic: false }, legend: { size: 14, bold: true, italic: false } })) as unknown as
      { title: { text: string; font: F }; xaxis: Ax; yaxis: Ax; yaxis2: Ax; legend: { font: F }; font: F }
    expect(l.title).toMatchObject({ text: 'Volcano', font: { weight: 'bold', family: 'Arial' } })   // the title is bold by default
    expect(l.xaxis).toMatchObject({ title: { text: 'log2 FC', font: { size: 16, weight: 'bold' } }, tickfont: { size: 11 }, tickangle: -45, gridcolor: '#eee' })
    expect(l.yaxis.title).toMatchObject({ text: '−log10 p', font: { style: 'italic', family: 'Arial' } })
    expect(l.yaxis.title.font.size).toBeUndefined()                                                  // Auto keeps the chart's size
    expect(l.yaxis.tickangle).toBeUndefined()                                                        // the angle is for x labels only
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
    const s = sanitizeChartText({ family: 'Comic Sans', tickAngle: 33, title: { size: 500, bold: 'yes' }, tick: { size: 14, italic: true } })
    expect(s.family).toBe('')
    expect(s.tickAngle).toBe(0)
    expect(s.title).toEqual(DEFAULT_CHART_TEXT.title)
    expect(s.tick).toEqual({ size: 14, bold: false, italic: true })
    expect(sanitizeChartText(null)).toEqual(DEFAULT_CHART_TEXT)
  })
})
