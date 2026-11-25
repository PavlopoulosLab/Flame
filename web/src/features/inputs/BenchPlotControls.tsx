import { useState } from 'react'
import { Field } from '../../components/ui'
import { FontSelect, TextStyleButtons, TickAngleSelect } from '../../components/ChartTextControls'
import { DEFAULT_PLOT, LEGEND_POS, SHAPES, type PlotOpts } from './benchPlot'

/** The chart controls, grouped as in GraphPad: chart type, points, bars and error bars, p-values, axes and text. */
export function PlotControls({ o, set, hasComparisons, canStack }: { o: PlotOpts; set: (f: (o: PlotOpts) => PlotOpts) => void; hasComparisons: boolean; canStack: boolean }) {
  const [open, setOpen] = useState(false)
  const up = <K extends keyof PlotOpts>(k: K) => (v: PlotOpts[K]) => set((x) => ({ ...x, [k]: v }))
  const sel = <K extends keyof PlotOpts>(k: K, opts: [string, string][], label: string) => (
    <select className="input input-sm" aria-label={label} value={String(o[k])} onChange={(e) => up(k)(e.target.value as PlotOpts[K])}>
      {opts.map(([v, l]) => <option key={v} value={v}>{l}</option>)}
    </select>
  )
  const rng = (k: 'pointSize' | 'width' | 'jitter' | 'cap' | 'font', min: number, max: number, step: number, label: string, cls = '') => (
    <label className={`small row ${cls}`} style={{ gap: 6 }}>{label}
      <input type="range" min={min} max={max} step={step} value={o[k]} onChange={(e) => up(k)(Number(e.target.value))} style={{ width: 90 }} />
      <span className="muted">{k === 'width' || k === 'jitter' ? `${Math.round(o[k] * 100)}%` : o[k]}</span>
    </label>
  )
  const stacked = o.kind === 'stacked'
  // A title box, then its size, bold and italic: shared by all the charts in Flame.
  const titleBox = (k: 'title' | 'yTitle' | 'xTitle', label: string, placeholder: string, minWidth: number) => (
    <Field label={label}>
      <span className="row bench-title-box" style={{ gap: 4 }}>
        <input className="input input-sm" value={o[k]} maxLength={140} placeholder={placeholder} style={{ minWidth }} aria-label={label} onChange={(e) => up(k)(e.target.value)} />
        <TextStyleButtons part={k} label={label} />
      </span>
    </Field>
  )
  return (
    <div className="bench-plot-controls stack-sm">
      <div className="row wrap">
        <Field label="Chart">{sel('kind', [['bar', 'Bars (mean ± error)'], ['dots', 'Scatter dot plot (mean ± error)'], ['box', 'Box plot'], ['violin', 'Violin plot'],
          ...(canStack ? [['stacked', 'Stacked bars (targets stacked)'] as [string, string]] : [])], 'Chart type')}</Field>
        <Field label="Style">{sel('style', [['color', 'Colour'], ['bw', 'Black & white']], 'Style')}</Field>
        {(o.kind === 'bar' || o.kind === 'dots') && <Field label="Error bars">{sel('errBar', [['sd', 'Mean ± SD'], ['sem', 'Mean ± SEM'], ['ci', 'Mean ± 95% CI']], 'Error bars')}</Field>}
        {hasComparisons && <Field label="P-values on the plot">{sel('pvals', [['stars', 'Stars (* ** ***)'], ['exact', 'Exact p'], ['off', 'Hide']], 'P-values')}</Field>}
        {hasComparisons && o.pvals !== 'off' && <label className="check small align-end"><input type="checkbox" checked={o.hideNs} onChange={(e) => up('hideNs')(e.target.checked)} /> Hide ns</label>}
      </div>
      <div className="row wrap bench-titles" role="group" aria-label="Titles and text" title="Font, sizes, bold and italic apply to every chart in Flame">
        {titleBox('title', 'Plot title', 'e.g. FKBP5 is induced by dexamethasone', 240)}
        {titleBox('yTitle', 'Y axis title', 'automatic', 170)}
        {titleBox('xTitle', 'X axis title', 'none', 130)}
        <FontSelect />
        {rng('font', 9, 22, 1, 'Text size', 'align-end')}
        <Field label="Tick labels">
          <span className="row bench-title-box" style={{ gap: 4 }}>
            <TextStyleButtons part="tick" label="Tick labels" />
            <TickAngleSelect />
          </span>
        </Field>
        <Field label="Legend">
          <span className="row bench-title-box" style={{ gap: 4 }}>
            <select className="input input-sm" aria-label="Legend position" title="The groups (the targets in stacked bars). With one gene the groups are named under the bars instead." value={o.legendPos} onChange={(e) => up('legendPos')(e.target.value as PlotOpts['legendPos'])} style={{ width: 'auto' }}>
              {LEGEND_POS.map(([v, l]) => <option key={v} value={v}>{l}</option>)}
            </select>
            {o.legendPos !== 'off' && <TextStyleButtons part="legend" label="Legend" />}
          </span>
        </Field>
      </div>
      <div className="row">
        <button type="button" className={`btn btn-sm ${open ? 'btn-primary' : 'btn-secondary'}`} aria-expanded={open} onClick={() => setOpen((v) => !v)}>
          {open ? '▾' : '▸'} Customise the chart: points and shapes, bar width, error-bar caps, axis range and break, figure size
        </button>
      </div>
      {open && (
      <div className="small card stack-sm" style={{ padding: 12 }}>
        <div className="stack-sm" style={{ marginTop: 8 }}>
          {!stacked && (
            <div className="row wrap">
              <label className="check small"><input type="checkbox" checked={o.points} onChange={(e) => up('points')(e.target.checked)} /> Show points</label>
              {o.points && <>
                <Field label="Shape">{sel('shape', SHAPES.map((s) => [s.value, s.label]), 'Point shape')}</Field>
                <label className="check small align-end"><input type="checkbox" checked={o.shapePerGroup} onChange={(e) => up('shapePerGroup')(e.target.checked)} /> A shape per group</label>
                <label className="check small align-end"><input type="checkbox" checked={o.hollow} onChange={(e) => up('hollow')(e.target.checked)} /> Hollow</label>
                {rng('pointSize', 3, 18, 1, 'Size')}
                {rng('jitter', 0, 1, 0.05, 'Spread')}
              </>}
            </div>
          )}
          <div className="row wrap">
            {rng('width', 0.2, 1, 0.05, stacked ? 'Bar width' : o.kind === 'box' ? 'Box width' : o.kind === 'violin' ? 'Violin width' : 'Bar width')}
            {(o.kind === 'bar' || o.kind === 'dots') && rng('cap', 0, 16, 1, 'Error-bar cap')}
          </div>
          <div className="row wrap">
            <Field label="Y from"><input className="input input-sm input-narrow" type="number" step="any" value={o.yMin} placeholder="auto" onChange={(e) => up('yMin')(e.target.value)} /></Field>
            <Field label="Y to"><input className="input input-sm input-narrow" type="number" step="any" value={o.yMax} placeholder="auto" onChange={(e) => up('yMax')(e.target.value)} /></Field>
            <label className="check small align-end"><input type="checkbox" checked={o.logY} onChange={(e) => up('logY')(e.target.checked)} /> Log scale</label>
          </div>
          {!stacked && !o.logY && (
            <div className="row wrap">
              <Field label="Axis break">{sel('brk', [['off', 'None'], ['auto', 'Automatic'], ['custom', 'From … to …']], 'Axis break')}</Field>
              {o.brk === 'custom' && <>
                <Field label="Break from"><input className="input input-sm input-narrow" type="number" step="any" min={0} value={o.brkFrom} onChange={(e) => up('brkFrom')(e.target.value)} /></Field>
                <Field label="to"><input className="input input-sm input-narrow" type="number" step="any" min={0} value={o.brkTo} onChange={(e) => up('brkTo')(e.target.value)} /></Field>
              </>}
            </div>
          )}
          <div className="row wrap">
            <Field label="Figure width (px)"><input className="input input-sm input-narrow" type="number" min={0} step={10} value={o.figW || ''} placeholder="window" onChange={(e) => up('figW')(Math.max(0, Number(e.target.value) || 0))} /></Field>
            <Field label="Figure height (px)"><input className="input input-sm input-narrow" type="number" min={200} max={1600} step={10} value={o.figH} onChange={(e) => up('figH')(Math.min(1600, Math.max(200, Number(e.target.value) || 420)))} /></Field>
            <span className="muted small align-end">The exported image has this size (PNG at the resolution you pick, or SVG/PDF).</span>
            <button type="button" className="btn btn-ghost btn-sm align-end" onClick={() => set((x) => ({ ...DEFAULT_PLOT, kind: x.kind, title: x.title, yTitle: x.yTitle, xTitle: x.xTitle }))}>Reset style</button>
          </div>
        </div>
      </div>
      )}
    </div>
  )
}
