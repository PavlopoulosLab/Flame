import { describe, expect, it } from 'vitest'
import { benchFigure, DEFAULT_PLOT, type FigInput, type PlotOpts } from './benchPlot'

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

  it('one target: each group is labelled under its bar', () => {
    const one = benchFigure({ ...res, targets: [res.targets[0]] }, DEFAULT_PLOT, () => '#000000', 'Value')
    expect((one.layout as unknown as Record<string, any>).xaxis.ticktext).toEqual(['Control', 'Dex', 'RU486'])
  })
})
