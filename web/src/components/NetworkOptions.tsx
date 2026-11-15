import { useState } from 'react'
import { CURVES, ENGINES, SHAPES, type NetworkEngine, type NetworkStyle } from '../lib/networkStyle'
import type { NetworkHandle } from './charts'

/** Layout picker: static layouts (computed on the server) plus vis-network's engines. */
export function LayoutSelect({ style, onChange, staticLayouts, staticLayout, onStaticLayout, className = 'input input-sm' }: {
  style: NetworkStyle; onChange: (s: NetworkStyle) => void
  staticLayouts?: { value: string; label: string }[]; staticLayout?: string; onStaticLayout?: (v: string) => void; className?: string
}) {
  const value = style.engine === 'precomputed' ? `static:${staticLayout ?? ''}` : style.engine
  return (
    <select className={className} value={value} aria-label="Network layout"
      title={ENGINES.find((e) => e.value === style.engine)?.hint ?? 'Computed once; drag nodes to adjust'}
      onChange={(e) => {
        const v = e.target.value
        if (v.startsWith('static:')) { onStaticLayout?.(v.slice(7)); onChange({ ...style, engine: 'precomputed' }) }
        else onChange({ ...style, engine: v as NetworkEngine })
      }}>
      {staticLayouts?.length ? (
        <optgroup label="Static (computed)">
          {staticLayouts.map((l) => <option key={l.value} value={`static:${l.value}`}>{l.label}</option>)}
        </optgroup>
      ) : null}
      <optgroup label="Interactive (vis-network)">
        {ENGINES.map((e) => <option key={e.value} value={e.value}>{e.label}</option>)}
      </optgroup>
    </select>
  )
}

/**
 * "Network style" panel: vis-network layout parameters, node and edge visuals,
 * interaction, and clustering. Changes apply immediately.
 */
export function NetworkOptions({ style, onChange, onReset, handle, layoutSelect }: {
  style: NetworkStyle; onChange: (s: NetworkStyle) => void; onReset: () => void
  handle?: { current: NetworkHandle | null }
  /** Optional layout select to show inside the panel (when the toolbar does not have one). */
  layoutSelect?: React.ReactNode
}) {
  const [msg, setMsg] = useState('')
  const set = <K extends keyof NetworkStyle>(k: K, v: NetworkStyle[K]) => onChange({ ...style, [k]: v })
  const physics = style.engine !== 'precomputed' && style.engine !== 'hierarchical'
  return (
    <details className="net-options">
      <summary className="btn btn-secondary btn-sm">Network style ▾</summary>
      <div className="net-options-panel" role="group" aria-label="Network style">
        <fieldset>
          <legend>Layout</legend>
          {layoutSelect && <label>Engine {layoutSelect}</label>}
          {style.engine !== 'precomputed' && (
            <label>Spacing <input type="range" min={0.5} max={3} step={0.1} value={style.spacing} onChange={(e) => set('spacing', Number(e.target.value))} /></label>
          )}
          {style.engine === 'hierarchical' && (
            <>
              <label>Direction <select className="input input-sm" value={style.direction} onChange={(e) => set('direction', e.target.value as NetworkStyle['direction'])}>
                <option value="UD">Top → bottom</option><option value="DU">Bottom → top</option><option value="LR">Left → right</option><option value="RL">Right → left</option>
              </select></label>
              <label>Levels by <select className="input input-sm" value={style.sortMethod} onChange={(e) => set('sortMethod', e.target.value as NetworkStyle['sortMethod'])}>
                <option value="hubsize">Hub size (most connected first)</option><option value="directed">Edge direction</option>
              </select></label>
            </>
          )}
          {physics && (
            <>
              <label className="check"><input type="checkbox" checked={style.freeze} onChange={(e) => set('freeze', e.target.checked)} /> Freeze when settled</label>
              <span className="row wrap">
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => handle?.current?.setPhysicsRunning(true)}>▶ Simulate</button>
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => handle?.current?.setPhysicsRunning(false)}>⏸ Stop</button>
              </span>
            </>
          )}
          {style.engine === 'precomputed' && <p className="muted small">Static layout; drag nodes to adjust. Pick an interactive engine for live physics or levels.</p>}
        </fieldset>

        <fieldset>
          <legend>Nodes</legend>
          <label>Shape <select className="input input-sm" value={style.shape} onChange={(e) => set('shape', e.target.value as NetworkStyle['shape'])}>
            {SHAPES.map((s) => <option key={s.value} value={s.value}>{s.label}</option>)}
          </select></label>
          <label>Size by <select className="input input-sm" value={style.sizeBy} onChange={(e) => set('sizeBy', e.target.value as NetworkStyle['sizeBy'])}>
            <option value="fixed">Same size</option><option value="degree">Number of connections</option>
          </select></label>
          <label>Size <input type="range" min={6} max={40} step={1} value={style.nodeSize} onChange={(e) => set('nodeSize', Number(e.target.value))} /></label>
          <label className="check"><input type="checkbox" checked={style.labels} onChange={(e) => set('labels', e.target.checked)} /> Labels</label>
          {style.labels && (
            <>
              <label>Label size <input type="range" min={8} max={32} step={1} value={style.labelSize} onChange={(e) => set('labelSize', Number(e.target.value))} /></label>
              <label className="check"><input type="checkbox" checked={style.labelOutline} onChange={(e) => set('labelOutline', e.target.checked)} /> Label outline</label>
            </>
          )}
          <label className="check"><input type="checkbox" checked={style.shadows} onChange={(e) => set('shadows', e.target.checked)} /> Shadows</label>
        </fieldset>

        <fieldset>
          <legend>Edges</legend>
          <label>Shape <select className="input input-sm" value={style.curve} onChange={(e) => set('curve', e.target.value as NetworkStyle['curve'])}>
            {CURVES.map((c) => <option key={c.value} value={c.value}>{c.label}</option>)}
          </select></label>
          <label>Width <select className="input input-sm" value={style.edgeWidth} onChange={(e) => set('edgeWidth', e.target.value as NetworkStyle['edgeWidth'])}>
            <option value="auto">Default</option><option value="weight">By weight</option><option value="fixed">Thin</option>
          </select></label>
          <label>Opacity <input type="range" min={0.1} max={1} step={0.05} value={style.edgeOpacity} onChange={(e) => set('edgeOpacity', Number(e.target.value))} /></label>
          <label className="check"><input type="checkbox" checked={style.dashes} onChange={(e) => set('dashes', e.target.checked)} /> Dashed</label>
        </fieldset>

        <fieldset>
          <legend>Interaction</legend>
          <label className="check"><input type="checkbox" checked={style.navigation} onChange={(e) => set('navigation', e.target.checked)} /> Zoom buttons and arrow keys</label>
          <label className="check"><input type="checkbox" checked={style.dragNodes} onChange={(e) => set('dragNodes', e.target.checked)} /> Drag nodes</label>
          <label className="check"><input type="checkbox" checked={style.hideEdgesOnDrag} onChange={(e) => set('hideEdgesOnDrag', e.target.checked)} /> Hide edges while panning (faster)</label>
        </fieldset>

        <fieldset>
          <legend>Clusters</legend>
          <span className="row wrap">
            <button type="button" className="btn btn-ghost btn-sm" title="Collapse each highly connected node together with its neighbours"
              onClick={() => { const n = handle?.current?.clusterHubs() ?? 0; setMsg(n ? `Collapsed ${n} nodes. Double-click a cluster to open it.` : 'No hubs to collapse.') }}>Group hubs</button>
            <button type="button" className="btn btn-ghost btn-sm" title="Collapse nodes with a single connection into their neighbour"
              onClick={() => { const n = handle?.current?.clusterOutliers() ?? 0; setMsg(n ? `Collapsed ${n} nodes. Double-click a cluster to open it.` : 'No single-link nodes to collapse.') }}>Group leaves</button>
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => { handle?.current?.openClusters(); setMsg('') }}>Open all</button>
          </span>
          {msg && <p className="muted small" role="status">{msg}</p>}
        </fieldset>

        <div className="row between">
          <button type="button" className="btn btn-ghost btn-sm" onClick={() => handle?.current?.fit()}>Fit to view</button>
          <button type="button" className="btn btn-ghost btn-sm" onClick={onReset}>Reset style</button>
        </div>
      </div>
    </details>
  )
}
