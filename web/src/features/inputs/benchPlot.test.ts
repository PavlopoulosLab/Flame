import { describe, expect, it } from 'vitest'
import { autoFigWidth, benchFigure, DEFAULT_PLOT, type FigInput, type PlotOpts } from './benchPlot'

// Two targets, three groups of three replicates; FKBP5 up in Dex, IL6 down.
const vals = (target: string, g: string, xs: number[]) => xs.map((value, i) => ({ group: g, sample: `${g} ${i + 1}`, value, target }))
const summ = (g: string, xs: number[]) => {
  const n = xs.length, mean = xs.reduce((a, b) => a + b, 0) / n
  const sd = Math.sqrt(xs.reduce((a, b) => a + (b - mean) ** 2, 0) / (n - 1))
  return { group: g, n, mean, sd, sem: sd / Math.sqrt(n) }
}
const data: Record<string, Record<string, number[]>> = {
  FKBP5: { Control: [1, 1.1, 0.9], Dex: [11, 12, 10], RU486: [1.3, 1.2, 1.4] },
  IL6: { Control: [1, 0.95, 1.05], Dex: [0.3, 0.32, 0.28], RU486: [0.9, 0.85, 1] },
}
const res: FigInput = {
  groups: ['Control', 'Dex', 'RU486'], control: 'Control', logScale: true,
  targets: Object.entries(data).map(([t, gs]) => ({
    target: t,
    values: Object.entries(gs).flatMap(([g, xs]) => vals(t, g, xs)),
    summary: Object.entries(gs).map(([g, xs]) => summ(g, xs)),
    comparisons: [{ a: 'Dex', b: 'Control', p: 0.0004, stars: '***' }, { a: 'RU486', b: 'Control', p: 0.3, stars: 'ns' }],
  })),
}
const fig = (o: Partial<PlotOpts>) => benchFigure(res, { ...DEFAULT_PLOT, ...o }, (_g, i) => ['#111111', '#222222', '#333333'][i], 'Fold change')
const types = (o: Partial<PlotOpts>) => fig(o).data.map((t) => (t as { type: string }).type)

describe('bench figures', () => {
  it('draws every chart type', () => {
    expect(types({ kind: 'bar' })).toEqual(expect.arrayContaining(['bar', 'scatter']))
    expect(types({ kind: 'box' })).toContain('box')
    expect(types({ kind: 'violin' })).toContain('violin')
    expect(types({ kind: 'dots', points: false })).toEqual(['scatter', 'scatter', 'scatter'])        // mean ± error per group
    const st = fig({ kind: 'stacked' })
    expect(st.data).toHaveLength(2)                                                                  // one segment per target
    expect((st.layout as unknown as { barmode: string }).barmode).toBe('stack')
  })

  it('violins: one per group and target, never thinner than 4% of the axis, with a quartile box', () => {
    const v = fig({ kind: 'violin', points: false }).data as { type: string; bandwidth: number; box: { visible: boolean }; showlegend: boolean }[]
    expect(v).toHaveLength(3 * 2)
    expect(v.every((t) => t.type === 'violin' && t.box.visible)).toBe(true)
    expect(Math.min(...v.map((t) => t.bandwidth))).toBeGreaterThan(0.04 * 12 * 0.99)                 // axis spans ~0-12.4
    expect(v.filter((t) => t.showlegend)).toHaveLength(3)                                             // one legend entry per group
  })

  it('error bars: SD, SEM or 95% CI', () => {
    const err = (e: PlotOpts['errBar']) => ((fig({ kind: 'bar', errBar: e }).data[0] as { error_y: { array: number[] } }).error_y.array[0])
    const s = summ('Control', data.FKBP5.Control)
    expect(err('sd')).toBeCloseTo(s.sd)
    expect(err('sem')).toBeCloseTo(s.sem)
    expect(err('ci')).toBeCloseTo(s.sem * 4.303)                                                     // t(0.975, df = 2)
  })

  it('points: shape, a shape per group, hollow, size, hide', () => {
    const marker = (o: Partial<PlotOpts>, i = 1) => (fig(o).data[i] as { marker: { symbol: string; size: number } }).marker
    expect(marker({ shape: 'triangle-up' }).symbol).toBe('triangle-up')
    expect(marker({ shape: 'square', hollow: true, pointSize: 12 })).toMatchObject({ symbol: 'square-open', size: 12 })
    expect(marker({ shapePerGroup: true }, 3).symbol).toBe('triangle-up')                           // second group: the second shape
    expect(types({ points: false })).toEqual(['bar', 'bar', 'bar'])
  })

  it('p-value brackets: stars, exact p, hidden, hide ns', () => {
    const labels = (o: Partial<PlotOpts>) => ((fig(o).layout as unknown as { annotations: { text: string }[] }).annotations ?? []).map((a) => a.text)
    expect(labels({ pvals: 'stars' })).toEqual(['***', 'ns', '***', 'ns'])
    expect(labels({ pvals: 'stars', hideNs: true })).toEqual(['***', '***'])
    expect(labels({ pvals: 'exact' })[0]).toBe('p = 0.00040')
    expect(labels({ pvals: 'off' })).toEqual([])
    // each bracket is a line plus two end ticks; the 1-line at fold change 1 comes first
    expect((fig({ pvals: 'stars' }).layout as unknown as { shapes: unknown[] }).shapes).toHaveLength(1 + 4 * 3)
  })

  it('axes: titles, a fixed range, log scale, font size, axis break', () => {
    const l = fig({ yTitle: 'Relative mRNA', xTitle: 'Gene', yMin: '0', yMax: '20', font: 16 }).layout as unknown as Record<string, any>
    expect(l.yaxis.title.text).toBe('Relative mRNA')
    expect(l.xaxis.title.text).toBe('Gene')
    expect(l.yaxis.range).toEqual([0, 20])
    expect(l.font.size).toBe(16)
    expect((fig({ logY: true }).layout as unknown as Record<string, any>).yaxis.type).toBe('log')
    const b = fig({ brk: 'custom', brkFrom: '2', brkTo: '9' })
    expect(b.broken).toEqual([2, 9])
    expect((b.layout as unknown as Record<string, any>).yaxis2.range[0]).toBe(9)
  })

  it('log axis: brackets and range in log10 units, as Plotly expects', () => {
    const f = fig({ logY: true, pvals: 'stars' })
    const l = f.layout as unknown as { yaxis: { type: string; range: number[] }; annotations: { y: number }[] }
    expect(l.yaxis.type).toBe('log')
    expect(l.yaxis.range[1]).toBeLessThan(2)                                                           // log10 of ~12-20, not 10^98
    expect(Math.max(...l.annotations.map((a) => a.y))).toBeLessThan(l.yaxis.range[1])                 // the stars fit on the axis
    // bracket lines (shapes) are in data units: 10^(the label's height)
    const sh = (f.layout as unknown as { shapes: { y0: number; y1: number }[] }).shapes
    expect(sh[0].y1).toBeCloseTo(10 ** l.annotations[0].y, 6)
  })

  it('one readout that is not a fold change: its name is the y-axis title', () => {
    const raw = { ...res, logScale: false, targets: [{ ...res.targets[0], target: 'Viability (% of vehicle)' }] }
    const l = benchFigure(raw, DEFAULT_PLOT, () => '#000000', 'Value').layout as unknown as Record<string, any>
    expect(l.yaxis.title.text).toBe('Viability (% of vehicle)')
    expect(l.xaxis.title.text).toBe('')
  })

  it('one-sample tests: a label above each tested group, no bracket', () => {
    const vs = { ...res, targets: res.targets.map((t) => ({ ...t, comparisons: [{ a: 'Dex', b: 'value', p: 0.001, stars: '**' }] })) }
    const l = benchFigure(vs, DEFAULT_PLOT, () => '#000000', 'Fold change').layout as unknown as { annotations: { text: string }[]; shapes: unknown[] }
    expect(l.annotations.map((a) => a.text)).toEqual(['**', '**'])
    expect(l.shapes).toHaveLength(1)                                                                  // only the line at fold change 1
  })

  it('the gap between neighbouring bars is the same at every bar width', () => {
    const gapAt = (width: number) => {
      const bars = fig({ kind: 'bar', width, points: false }).data as { x: number[]; width: number }[]
      return (bars[1].x[0] - bars[0].x[0]) - bars[0].width                                          // centre distance minus bar width
    }
    expect(gapAt(0.4)).toBeCloseTo(gapAt(0.8), 10)
    expect(gapAt(0.25)).toBeCloseTo(gapAt(0.8), 10)
  })

  it('a title written by the user is drawn above the figure, with room for it', () => {
    const l = fig({ title: '  FKBP5 after dexamethasone ' }).layout as unknown as { title: { text: string }; margin: { t: number } }
    expect(l.title.text).toBe('FKBP5 after dexamethasone')
    expect(l.margin.t).toBeGreaterThan(30)
    expect((fig({}).layout as unknown as { title?: unknown }).title).toBeUndefined()
  })

  it('the plot and axis titles take their own font, size, bold and italic', () => {
    type F = { size: number; weight: string; style: string; family?: string }
    const l = fig({ title: 'T', yTitle: 'Relative mRNA', xTitle: 'Gene', family: 'Arial', titleSize: 24, titleItalic: true, yTitleBold: true, xTitleSize: 10, xTitleItalic: true })
      .layout as unknown as { title: { font: F }; yaxis: { title: { text: string; font: F } }; xaxis: { title: { font: F } }; font: { family: string } }
    expect(l.title.font).toMatchObject({ size: 24, weight: 'bold', style: 'italic', family: 'Arial' })
    expect(l.yaxis.title).toMatchObject({ text: 'Relative mRNA', font: { size: 13, weight: 'bold', style: 'normal' } })
    expect(l.xaxis.title.font).toMatchObject({ size: 10, weight: 'normal', style: 'italic' })
    expect(l.font.family).toBe('Arial')
  })

  it('the legend goes where the user puts it, in its own font, or is hidden', () => {
    type L = { legend: { orientation?: string; x?: number; y?: number; xanchor?: string; font: { size: number; weight: string; style: string } }; showlegend: boolean }
    const below = fig({}).layout as unknown as L
    expect(below).toMatchObject({ showlegend: true, legend: { orientation: 'h', y: -0.18 } })
    const right = fig({ legendPos: 'right', legendSize: 16, legendItalic: true }).layout as unknown as L
    expect(right.legend).toMatchObject({ orientation: 'v', x: 1.02, font: { size: 16, style: 'italic', weight: 'normal' } })
    expect((fig({ legendPos: 'inside-tr' }).layout as unknown as L).legend).toMatchObject({ x: 0.99, xanchor: 'right' })
    expect((fig({ legendPos: 'off' }).layout as unknown as L).showlegend).toBe(false)
    expect((fig({ kind: 'stacked', legendPos: 'top', legendBold: true }).layout as unknown as L).legend).toMatchObject({ y: 1.02, font: { weight: 'bold' } })
  })

  it('tick labels: their own size, bold, italic, and slanted x labels', () => {
    type A = { tickfont: { size: number; weight: string; style: string }; tickangle?: number }
    const l = fig({ tickSize: 15, tickBold: true, tickAngle: 45, yTickSize: 11 }).layout as unknown as { xaxis: A; yaxis: A }
    expect(l.xaxis).toMatchObject({ tickangle: -45, tickfont: { size: 15, weight: 'bold', style: 'normal' } })
    expect(l.yaxis.tickfont).toMatchObject({ size: 11, weight: 'normal' })                          // y ticks: their own style
    expect((fig({ yTickItalic: true }).layout as unknown as { yaxis: A }).yaxis.tickfont).toMatchObject({ size: 13, style: 'italic', weight: 'normal' })
    expect((fig({ kind: 'stacked', tickAngle: 90 }).layout as unknown as { xaxis: A }).xaxis.tickangle).toBe(-90)
  })

  it('one gene shown on its own gets a figure sized to its groups, not the whole window', () => {
    expect(autoFigWidth(1, 2, DEFAULT_PLOT)).toBe(373)
    expect(autoFigWidth(1, 3, { ...DEFAULT_PLOT, width: 0.3 })!).toBeLessThan(autoFigWidth(1, 3, DEFAULT_PLOT)! * 0.75)   // narrower bars, smaller figure
    expect(autoFigWidth(1, 5, DEFAULT_PLOT)).toBeGreaterThan(autoFigWidth(1, 2, DEFAULT_PLOT)!)
    expect(autoFigWidth(1, 2, { ...DEFAULT_PLOT, title: 'A long title for one gene shown on its own here' })).toBeGreaterThan(500)
    expect(autoFigWidth(6, 2, DEFAULT_PLOT)).toBeNull()                                          // all genes: the window's width
    expect(autoFigWidth(1, 2, { ...DEFAULT_PLOT, figW: 700 })).toBeNull()                         // the user's width wins
  })

  it('one target: the x axis hugs the groups, so narrow bars do not leave empty space at the sides', () => {
    const xr = (width: number) => (benchFigure({ ...res, targets: [res.targets[0]] }, { ...DEFAULT_PLOT, width }, () => '#000000', 'Value').layout as unknown as { xaxis: { range: number[] } }).xaxis.range
    expect(xr(0.3)[1]).toBeLessThan(xr(0.8)[1])
    const bars = benchFigure({ ...res, targets: [res.targets[0]] }, { ...DEFAULT_PLOT, width: 0.3 }, () => '#000000', 'Value').data.filter((t) => (t as { type: string }).type === 'bar') as unknown as { x: number[]; width: number }[]
    const right = Math.max(...bars.map((b) => b.x[0] + b.width / 2))
    expect(xr(0.3)[1] - right).toBeLessThan(right - Math.min(...bars.map((b) => b.x[0] - b.width / 2)))   // side space < the span of the bars
  })

  it('one target: each group is labelled under its bar', () => {
    const one = benchFigure({ ...res, targets: [res.targets[0]] }, DEFAULT_PLOT, () => '#000000', 'Value')
    expect((one.layout as unknown as Record<string, any>).xaxis.ticktext).toEqual(['Control', 'Dex', 'RU486'])
  })
})
