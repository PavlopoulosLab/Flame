import { describe, expect, it } from 'vitest'
import { applyChartColours, colourRoles, parseCss, sanitizeChartColours } from './chartColors'

const volcano = [
  { type: 'scatter', mode: 'markers', name: 'Up-regulated', x: [2], y: [5], marker: { color: '#dc2626', size: 6, line: { width: [0], color: '#f97316' } } },
  { type: 'scatter', mode: 'markers', name: 'Down-regulated', x: [-2], y: [5], marker: { color: '#2563eb', size: 6, line: { width: [0], color: '#f97316' } } },
  { type: 'scatter', mode: 'markers', name: 'Not significant', x: [0], y: [0.1], marker: { color: '#a1a1aa' } },
]

describe('parseCss', () => {
  it('reads hex, short hex and rgba', () => {
    expect(parseCss('#ABCDEF')).toEqual({ hex: '#abcdef', alpha: 1 })
    expect(parseCss('#abc')).toEqual({ hex: '#aabbcc', alpha: 1 })
    expect(parseCss('rgba(255, 0, 16, 0.4)')).toEqual({ hex: '#ff0010', alpha: 0.4 })
    expect(parseCss(3)).toBeNull()
  })
})

describe('colourRoles', () => {
  it('gives each volcano group its own colour', () => {
    const r = colourRoles(volcano, [])
    expect(r.map((x) => [x.key, x.label, x.color])).toEqual([
      ['t:Up-regulated', 'Up-regulated', '#dc2626'], ['t:Down-regulated', 'Down-regulated', '#2563eb'], ['t:Not significant', 'Not significant', '#a1a1aa'],
    ])
  })
  it('skips invisible helper traces', () => {
    expect(colourRoles([...volcano, { type: 'scatter', mode: 'markers', x: [0], y: [0], showlegend: false, hoverinfo: 'skip', marker: { opacity: 0 } }], []).length).toBe(3)
  })
  it('uses the colourway for series without a colour, and names a lone series by what it draws', () => {
    expect(colourRoles([{ type: 'bar', x: ['a'], y: [1] }], ['#111111'])).toEqual([{ key: 't:#1', label: 'Bars', color: '#111111' }])
  })
  it('gives a colour scale low / middle / high, reversed scales as drawn', () => {
    const r = colourRoles([{ type: 'heatmap', z: [[1]], colorscale: [[0, '#ffffff'], [1, '#000000']], reversescale: true }], [])
    expect(r.map((x) => [x.key, x.color])).toEqual([['s:#1:low', '#000000'], ['s:#1:high', '#ffffff']])
    const named = colourRoles([{ type: 'bar', name: 'Terms', x: [1, 2], marker: { color: [1, 2], colorscale: 'Viridis' } }], [])
    expect(named.map((x) => x.label)).toEqual(['Terms · Low', 'Terms · Middle', 'Terms · High'])
  })
  it('lists colours painted point by point, with the points they paint', () => {
    const r = colourRoles([{ type: 'bar', orientation: 'h', y: ['A', 'B', 'C'], x: [1, 2, 3], marker: { color: [1, '#e11d48', 3], colorscale: 'Viridis' } }], [])
    expect(r.find((x) => x.key === 'c:#e11d48')).toMatchObject({ label: 'Colour 1', hint: 'B' })
  })
})

describe('applyChartColours', () => {
  it('leaves the chart untouched without chosen colours', () => {
    const d = applyChartColours(volcano, {}, {}, [])
    expect(d.data).toBe(volcano)
  })
  it('recolours one series and keeps its selection outline', () => {
    const { data } = applyChartColours(volcano, {}, { 't:Up-regulated': '#00ff00' }, [])
    const up = data[0] as { marker: { color: string; line: { color: string } } }
    expect(up.marker.color).toBe('#00ff00')
    expect(up.marker.line.color).toBe('#f97316')
    expect(data[1]).toBe(volcano[1])
  })
  it('recolours lines, keeping a see-through fill see-through', () => {
    const km = [{ type: 'scatter', mode: 'lines', name: 'High', line: { color: '#dc2626' }, fill: 'tonexty', fillcolor: 'rgba(220,38,38,0.2)' }]
    const t = applyChartColours(km, {}, { 't:High': '#0000ff' }, []).data[0] as { line: { color: string }; fillcolor: string; marker?: unknown }
    expect(t.line.color).toBe('#0000ff')
    expect(t.fillcolor).toBe('rgba(0,0,255,0.2)')
    expect(t.marker).toBeUndefined()
  })
  it('replaces a colour scale, only the stops chosen', () => {
    const t = applyChartColours([{ type: 'heatmap', z: [[1]], colorscale: 'RdBu', reversescale: true }], {}, { 's:#1:high': '#ff0000' }, []).data[0] as { colorscale: unknown; reversescale: boolean }
    expect(t.colorscale).toEqual([[0, '#b20a1c'], [0.5, '#bebebe'], [1, '#ff0000']])
    expect(t.reversescale).toBe(false)
  })
  it('recolours Sankey nodes and their links, and hand-drawn legends', () => {
    const sankey = [{ type: 'sankey', node: { label: ['a', 'b'], color: ['#8b5cf6', '#f97316'] }, link: { source: [0], target: [1], value: [1], color: ['rgba(139,92,246,0.3)'] } }]
    const layout = { shapes: [{ name: 'legend-a', fillcolor: '#8b5cf6' }, { name: 'cut', fillcolor: '#8b5cf6' }] }
    const out = applyChartColours(sankey, layout, { 'c:#8b5cf6': '#000000' }, [])
    const t = out.data[0] as { node: { color: string[] }; link: { color: string[] } }
    expect(t.node.color).toEqual(['#000000', '#f97316'])
    expect(t.link.color).toEqual(['rgba(0,0,0,0.3)'])
    expect(out.layout.shapes.map((s) => s.fillcolor)).toEqual(['#000000', '#8b5cf6'])
  })
})

describe('sanitizeChartColours', () => {
  it('keeps hex colours only', () => {
    expect(sanitizeChartColours({ a: '#ABCDEF', b: 'red', c: 3 })).toEqual({ a: '#abcdef' })
    expect(sanitizeChartColours('x')).toEqual({})
  })
})
