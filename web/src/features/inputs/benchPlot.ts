// Bench data figures, GraphPad-style: bar, scatter-dot, box, violin or stacked bar charts of the
// values per group and target, with points (shape, size, jitter), error bars (SD / SEM / 95% CI),
// p-value brackets, axis titles and ranges, log scale, an optional axis break and the figure size.
import { autoBreak, nice } from '../../lib/axisBreak'

export type PlotKind = 'bar' | 'dots' | 'box' | 'violin' | 'stacked'
export type Shape = 'circle' | 'triangle-up' | 'square' | 'diamond' | 'cross' | 'x' | 'triangle-down' | 'star'
export interface PlotOpts {
  kind: PlotKind
  errBar: 'sd' | 'sem' | 'ci'
  /** Error-bar cap width (px). */
  cap: number
  /** Bar / box / violin width as a share of the space for one group (0.2–1). */
  width: number
  points: boolean
  shape: Shape
  /** A different shape for each group (in the order above), as Prism does. */
  shapePerGroup: boolean
  hollow: boolean
  pointSize: number
  /** Horizontal spread of the points (0 = in a line, 1 = across the bar). */
  jitter: number
  pvals: 'off' | 'stars' | 'exact'
  hideNs: boolean
  style: 'color' | 'bw'
  yTitle: string
  xTitle: string
  yMin: string
  yMax: string
  logY: boolean
  font: number
  brk: 'off' | 'auto' | 'custom'
  brkFrom: string
  brkTo: string
  /** Figure size on screen and in the exported image (px); width 0 = the width of the window. */
  figW: number
  figH: number
}
export const DEFAULT_PLOT: PlotOpts = {
  kind: 'bar', errBar: 'sd', cap: 6, width: 0.8, points: true, shape: 'circle', shapePerGroup: false, hollow: false, pointSize: 8, jitter: 0.5,
  pvals: 'stars', hideNs: false, style: 'color', yTitle: '', xTitle: '', yMin: '', yMax: '', logY: false, font: 13,
  brk: 'off', brkFrom: '', brkTo: '', figW: 0, figH: 420,
}
export const SHAPES: { value: Shape; label: string }[] = [
  { value: 'circle', label: '● Circle' }, { value: 'triangle-up', label: '▲ Triangle' }, { value: 'square', label: '■ Square' },
  { value: 'diamond', label: '◆ Diamond' }, { value: 'triangle-down', label: '▼ Triangle down' }, { value: 'cross', label: '✚ Cross' },
  { value: 'x', label: '✖ X' }, { value: 'star', label: '★ Star' },
]

/** What the figure needs from a bench result. */
export interface FigTarget {
  target: string; error?: string
  values: { group: string; sample: string; value: number }[]
  summary: { group: string; n: number; mean: number; sd: number | null; sem: number | null }[]
  comparisons?: { a: string; b: string; p: number; stars: string }[]
}
export interface FigInput { targets: FigTarget[]; groups: string[]; control: string; logScale: boolean }

const BW_FILLS = ['#ffffff', '#bdbdbd', '#636363', '#000000', '#e6e6e6', '#8c8c8c', '#404040', '#d0d0d0']
const DARK = new Set(['#636363', '#000000', '#404040', '#8c8c8c'])
const T975 = [12.706, 4.303, 3.182, 2.776, 2.571, 2.447, 2.365, 2.306, 2.262, 2.228, 2.201, 2.179, 2.160, 2.145, 2.131, 2.120, 2.110, 2.101, 2.093, 2.086,
  2.080, 2.074, 2.069, 2.064, 2.060, 2.056, 2.052, 2.048, 2.045, 2.042]
const t975 = (df: number) => (df < 1 ? NaN : df <= 30 ? T975[df - 1] : 1.96 + 2.4 / df)
const fmtP = (p: number) => (p < 0.0001 ? 'p < 0.0001' : `p = ${p < 0.01 ? p.toPrecision(2) : p.toFixed(3)}`)
const num = (s: string) => (s.trim() === '' ? null : Number.isFinite(Number(s)) ? Number(s) : null)
const hash = (s: string) => { let h = 0; for (const ch of s) h = (h * 31 + ch.charCodeAt(0)) >>> 0; return (h % 1000) / 1000 - 0.5 }

/** The Plotly figure for the chosen chart type and style; `colorOf` gives each group's colour. */
export function benchFigure(res: FigInput, o: PlotOpts, colorOf: (group: string, i: number) => string, defaultYTitle: string) {
  const ts = res.targets.filter((t) => !t.error && t.values.length)
  const groups = res.groups
  const bw = o.style === 'bw'
  const ink = '#000000'
  const fill = (gi: number) => (bw ? BW_FILLS[gi % BW_FILLS.length] : colorOf(groups[gi], gi))
  const font = { size: o.font, ...(bw ? { color: ink, family: 'Arial, Helvetica, sans-serif' } : {}) }
  const axisStyle = { showline: true, linecolor: bw ? ink : '#6b7280', linewidth: 1.2, ticks: 'outside', tickcolor: bw ? ink : '#6b7280', showgrid: false, zeroline: false }
  const page = { font, ...(bw ? { paper_bgcolor: '#ffffff', plot_bgcolor: '#ffffff' } : {}) }
  const yTitle = o.yTitle.trim() || defaultYTitle
  const legend = { orientation: 'h', y: -0.18 }

  // ---- stacked bars: one bar per group, one segment per target (e.g. cell-cycle phases) ----
  if (o.kind === 'stacked') {
    const data = ts.map((t, i) => ({
      type: 'bar', name: t.target, x: groups, y: groups.map((g) => t.summary.find((s) => s.group === g)?.mean ?? null), width: o.width * 0.8,
      marker: { color: bw ? BW_FILLS[i % BW_FILLS.length] : colorOf(t.target, i), line: { color: bw ? ink : '#ffffff', width: 1 } },
      hovertemplate: `${t.target}: %{y:.3g}<extra>%{x}</extra>`,
      text: o.pvals === 'off' ? undefined : groups.map((g) => { const c = t.comparisons?.find((x) => x.a === g && x.b === res.control); return c && (c.stars !== 'ns' || !o.hideNs) ? (o.pvals === 'exact' ? fmtP(c.p) : c.stars) : '' }),
      textposition: 'inside', insidetextanchor: 'middle',
      textfont: { color: bw ? (DARK.has(BW_FILLS[i % BW_FILLS.length]) ? '#ffffff' : ink) : '#ffffff', size: o.font - 1 },
    }))
    const yr = [num(o.yMin), num(o.yMax)]
    return { data, layout: { ...page, barmode: 'stack', xaxis: { ...axisStyle, title: { text: o.xTitle } },
      yaxis: { ...axisStyle, title: { text: yTitle }, type: o.logY ? 'log' : 'linear', ...(yr[0] != null && yr[1] != null ? { range: o.logY ? yr.map((v) => Math.log10(v!)) : yr } : { rangemode: 'tozero' }) },
      legend, margin: { l: 70, r: 10, t: 10, b: 80 } }, broken: null as [number, number] | null }
  }

  // ---- one slot per target, the groups side by side within it ----
  const k = groups.length
  const slot = Math.min(0.85 / k, 0.3)
  const one = ts.length === 1
  const pos = (ti: number, gi: number) => ti + (gi - (k - 1) / 2) * slot
  const stat = (t: FigTarget, g: string) => t.summary.find((x) => x.group === g)
  const mean = (t: FigTarget, g: string) => stat(t, g)?.mean ?? NaN
  const err = (t: FigTarget, g: string) => {
    const s = stat(t, g)
    if (!s) return 0
    return (o.errBar === 'sd' ? s.sd : o.errBar === 'sem' ? s.sem : s.sem != null ? s.sem * t975(s.n - 1) : null) ?? 0
  }
  const vals = (t: FigTarget, g: string) => t.values.filter((v) => v.group === g).map((v) => v.value)
  const showsBars = o.kind === 'bar' || o.kind === 'dots'
  const top = (t: FigTarget, g: string) => Math.max(showsBars ? mean(t, g) + err(t, g) : -Infinity, ...(o.points || !showsBars ? vals(t, g) : [-Infinity]))
  const bottom = (t: FigTarget, g: string) => Math.min(showsBars ? mean(t, g) - err(t, g) : Infinity, ...(o.points || !showsBars ? vals(t, g) : [Infinity]))
  const allTops = ts.flatMap((t) => groups.map((g) => top(t, g))).filter(Number.isFinite)
  const allBottoms = ts.flatMap((t) => groups.map((g) => bottom(t, g))).filter(Number.isFinite)
  let ymax = Math.max(...allTops, res.logScale ? 1 : 0)
  const ymin = Math.min(0, ...allBottoms)

  // p-value brackets: each comparison of a target as a bracket over its two groups, shortest lowest.
  type Bracket = { ti: number; x0: number; x1: number; y: number; label: string }
  const brackets: Bracket[] = []
  if (o.pvals !== 'off') {
    const step = (ymax - ymin || 1) * 0.075
    ts.forEach((t, ti) => {
      const cs = (t.comparisons ?? []).filter((c) => groups.includes(c.a) && groups.includes(c.b) && (!o.hideNs || c.stars !== 'ns'))
        .map((c) => ({ c, gi: [groups.indexOf(c.a), groups.indexOf(c.b)].sort((a, b) => a - b) }))
        .sort((a, b) => (a.gi[1] - a.gi[0]) - (b.gi[1] - b.gi[0]) || a.gi[0] - b.gi[0])
      const levels: number[] = []                                      // highest bracket so far over each group
      for (const { c, gi } of cs) {
        let base = -Infinity
        for (let g = gi[0]; g <= gi[1]; g++) base = Math.max(base, levels[g] ?? top(t, groups[g]))
        const y = base + step
        for (let g = gi[0]; g <= gi[1]; g++) levels[g] = y + step * 0.35
        brackets.push({ ti, x0: pos(ti, gi[0]), x1: pos(ti, gi[1]), y, label: o.pvals === 'exact' ? fmtP(c.p) : c.stars })
      }
    })
    if (brackets.length) ymax = Math.max(ymax, ...brackets.map((b) => b.y + step * 0.9))
  }

  // Axis break (linear axis only): automatic, or from ... to ... set by the user.
  let broken: [number, number] | null = null
  if (!o.logY && o.brk === 'auto') broken = autoBreak(ts.flatMap((t) => groups.map((g) => ({ top: top(t, g), bottom: bottom(t, g) }))))
  const bf = num(o.brkFrom), bt = num(o.brkTo)
  if (!o.logY && o.brk === 'custom' && bf != null && bt != null && bf > 0 && bt > bf && bt < ymax) broken = [bf, bt]
  const axes = broken ? ['y', 'y2'] : ['y']
  const ptSymbol = (gi: number) => { const s = o.shapePerGroup ? SHAPES[gi % SHAPES.length].value : o.shape; return o.hollow ? `${s}-open` : s }

  const traces = axes.flatMap((ax, ai) => groups.flatMap((g, gi) => {
    const out: Record<string, unknown>[] = []
    const xs = ts.map((_, ti) => pos(ti, gi))
    const w = slot * o.width
    const line = { color: bw ? ink : fill(gi), width: 1.5 }
    if (o.kind === 'bar') {
      out.push({ type: 'bar', name: g, showlegend: ai === 0 && !one, legendgroup: g, yaxis: ax, x: xs, y: ts.map((t) => mean(t, g)), width: w,
        error_y: { type: 'data', array: ts.map((t) => err(t, g)), visible: true, color: bw ? ink : '#374151', thickness: 1.3, width: o.cap },
        marker: bw ? { color: fill(gi), line: { color: ink, width: 1.2 } } : { color: fill(gi), opacity: o.points ? 0.35 : 0.8, line },
        hovertemplate: `${g}: %{y:.3g}<extra></extra>` })
    } else if (o.kind === 'dots') {
      out.push({ type: 'scatter', mode: 'markers', name: g, showlegend: ai === 0 && !one, legendgroup: g, yaxis: ax, x: xs, y: ts.map((t) => mean(t, g)),
        marker: { symbol: 'line-ew', size: Math.max(12, w * 260), line: { color: bw ? ink : fill(gi), width: 2.5 } },
        error_y: { type: 'data', array: ts.map((t) => err(t, g)), visible: true, color: bw ? ink : fill(gi), thickness: 1.5, width: o.cap },
        hovertemplate: `${g} mean: %{y:.3g}<extra></extra>` })
    } else if (o.kind === 'box') {
      const xv = ts.flatMap((t, ti) => vals(t, g).map(() => pos(ti, gi)))
      const yv = ts.flatMap((t) => vals(t, g))
      out.push({ type: 'box', name: g, showlegend: ai === 0 && !one, legendgroup: g, yaxis: ax, x: xv, y: yv, width: w, hoverinfo: 'y', line,
        boxpoints: false, fillcolor: bw ? fill(gi) : fill(gi) + '55', boxmean: false })
    } else {
      // One violin per target, so each gets its own smoothing. Bandwidth: Silverman's rule, but at least 4% of
      // the axis, so small groups (n = 3) or identical values still give a rounded shape instead of a flat line.
      ts.forEach((t, ti) => {
        const v = vals(t, g)
        if (!v.length) return
        const n = v.length, m = v.reduce((a, b) => a + b, 0) / n
        const sd = n > 1 ? Math.sqrt(v.reduce((a, b) => a + (b - m) ** 2, 0) / (n - 1)) : 0
        const bwd = Math.max(1.06 * sd * n ** -0.2, (ymax - ymin || 1) * 0.04)
        out.push({ type: 'violin', name: g, showlegend: ai === 0 && !one && ti === 0, legendgroup: g, yaxis: ax, x: v.map(() => pos(ti, gi)), y: v,
          width: w, hoverinfo: 'y', line: { color: bw ? ink : fill(gi), width: 1.3 }, fillcolor: bw ? fill(gi) : fill(gi) + '40',
          bandwidth: bwd, spanmode: 'soft', scalemode: 'width', points: false, meanline: { visible: false },
          // median and quartiles as a thin box inside, as in Prism
          box: { visible: true, width: 0.18, fillcolor: bw ? '#ffffff' : '#ffffff', line: { color: bw ? ink : fill(gi), width: 1.3 } } })
      })
    }
    if (o.points) {
      const pts = ts.flatMap((t, ti) => t.values.filter((v) => v.group === g).map((v) => ({ x: pos(ti, gi) + hash(v.sample + t.target) * w * o.jitter, y: v.value, s: v.sample })))
      const dark = bw && DARK.has(fill(gi))
      out.push({ type: 'scatter', mode: 'markers', name: `${g} (points)`, showlegend: false, legendgroup: g, yaxis: ax, x: pts.map((p) => p.x), y: pts.map((p) => p.y), text: pts.map((p) => p.s),
        marker: { symbol: ptSymbol(gi), size: o.pointSize,
          color: bw ? (dark ? '#ffffff' : ink) : fill(gi), line: { color: bw ? (dark ? ink : '#ffffff') : o.hollow ? fill(gi) : '#ffffff', width: o.hollow ? 1.6 : 1 } },
        hovertemplate: '%{text}: %{y:.3g}<extra></extra>' })
    }
    return out
  }))

  const upTop = nice(ymax * 1.06)
  const shapes: Record<string, unknown>[] = res.logScale && !o.logY ? [{ type: 'line', xref: 'paper', yref: 'y', x0: 0, x1: 1, y0: 1, y1: 1, line: { color: bw ? '#7a7a7a' : '#9ca3af', width: 1, dash: 'dot' } }] : []
  const annotations: Record<string, unknown>[] = []
  const yref = (y: number) => (broken && y > broken[0] ? 'y2' : 'y')
  for (const b of brackets) {
    let y = b.y
    if (broken && y > broken[0] && y < broken[1]) y = broken[1] + (upTop - broken[1]) * 0.05     // in the gap: just above it
    const r = yref(y), tick = (broken && r === 'y2' ? upTop - broken[1] : ymax - ymin) * 0.02
    const ln = { color: bw ? ink : '#374151', width: 1.2 }
    shapes.push({ type: 'line', xref: 'x', yref: r, x0: b.x0, x1: b.x1, y0: y, y1: y, line: ln },
      { type: 'line', xref: 'x', yref: r, x0: b.x0, x1: b.x0, y0: y - tick, y1: y, line: ln },
      { type: 'line', xref: 'x', yref: r, x0: b.x1, x1: b.x1, y0: y - tick, y1: y, line: ln })
    annotations.push({ x: (b.x0 + b.x1) / 2, y, yref: r, text: b.label, showarrow: false, yanchor: 'bottom',
      font: { size: b.label === 'ns' || o.pvals === 'exact' ? o.font - 2 : o.font + 2, color: bw ? ink : undefined } })
  }
  const low = 0.6, high = 0.68
  if (broken) {
    for (const y of [low, high]) shapes.push({ type: 'line', xref: 'paper', yref: 'paper', x0: -0.012, x1: 0.012, y0: y - 0.014, y1: y + 0.014, line: { color: bw ? ink : '#6b7280', width: 1.5 } })
    annotations.push({ xref: 'paper', yref: 'paper', x: -0.08, y: 0.5, text: yTitle, textangle: -90, showarrow: false, font: { size: o.font + 1, color: bw ? ink : undefined } })
  }
  // One target (e.g. a column table): label each group under its bar; else one label per target.
  const xaxis = { ...axisStyle, anchor: 'y', range: [-0.6, ts.length - 0.4], title: { text: o.xTitle || (one && ts[0].target !== 'value' ? ts[0].target : '') },
    ...(one ? { tickvals: groups.map((_, gi) => pos(0, gi)), ticktext: groups } : { tickvals: ts.map((_, i) => i), ticktext: ts.map((t) => t.target) }) }
  const yr = [num(o.yMin), num(o.yMax)]
  const userRange = yr[0] != null && yr[1] != null && yr[1]! > yr[0]! ? (o.logY ? yr.map((v) => Math.log10(Math.max(v!, 1e-12))) : yr) : null
  const pad = (ymax - ymin) * 0.04
  const autoRange = [ymin < 0 ? ymin - pad : 0, ymax + pad]
  const mode = o.kind === 'box' ? { boxmode: 'overlay' } : o.kind === 'violin' ? { violinmode: 'overlay' } : { barmode: 'overlay', bargap: 0 }
  const layout = broken
    ? { ...page, ...mode, xaxis, yaxis: { ...axisStyle, domain: [0, low], range: [autoRange[0], broken[0]] },
        yaxis2: { ...axisStyle, domain: [high, 1], range: [broken[1], userRange?.[1] ?? upTop], anchor: 'x' },
        annotations, shapes, legend, margin: { l: 75, r: 10, t: 10, b: 80 } }
    : { ...page, ...mode, xaxis, yaxis: { ...axisStyle, title: { text: yTitle }, type: o.logY ? 'log' : 'linear', ...(userRange ? { range: userRange } : o.logY ? { autorange: true } : { range: autoRange }) },
        annotations, shapes, legend, margin: { l: 70, r: 10, t: 10, b: 80 } }
  return { data: traces, layout, broken }
}

