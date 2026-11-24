import { useState } from 'react'
import {
  DEFAULT_CUSTOM_PALETTE, MAX_CUSTOM_COLOURS, PALETTES, hasSavedCustomPalette, parseHexColours, setCustomPalette,
  useCustomPalette, usePaletteScope, type CustomPalette, type PaletteMode,
} from '../lib/palette'
import { Modal } from './ui'

/** Palette menu of the nearest visualization (renders nothing outside a PaletteScope). */
export function PaletteSelect() {
  const s = usePaletteScope()
  const [editing, setEditing] = useState(false)
  if (!s) return null
  const hint = PALETTES.find((p) => p.value === s.mode)?.hint
  return (
    <span className="palette-select-wrap">
      <label className="palette-select" title={`Colours of this visualization: ${hint}`}>
        <svg width="16" height="16" viewBox="0 0 24 24" aria-hidden="true">
          <circle cx="12" cy="12" r="9" fill="none" stroke="currentColor" strokeWidth="1.6" />
          <path d="M12 3a9 9 0 0 1 0 18z" fill="currentColor" />
        </svg>
        <select className="input input-sm" value={s.mode} aria-label="Colours of this visualization"
          onChange={(e) => {
            const m = e.target.value as PaletteMode
            s.setMode(m)
            if (m === 'custom' && !hasSavedCustomPalette()) setEditing(true)   // first time: choose the colours
          }}>
          {PALETTES.map((p) => <option key={p.value} value={p.value}>{p.label}</option>)}
        </select>
      </label>
      {s.mode === 'custom' && <button type="button" className="btn btn-ghost btn-sm" onClick={() => setEditing(true)}>Edit colours</button>}
      {editing && <CustomPaletteEditor onClose={() => setEditing(false)} />}
    </span>
  )
}

/** Editor of the user's own palette: category colours and the low/high ends of continuous scales. */
function CustomPaletteEditor({ onClose }: { onClose: () => void }) {
  const saved = useCustomPalette()
  const [p, setP] = useState<CustomPalette>(saved)
  const [paste, setPaste] = useState('')
  const set = (patch: Partial<CustomPalette>) => setP((x) => ({ ...x, ...patch }))
  const setColour = (i: number, c: string) => set({ colors: p.colors.map((x, j) => (j === i ? c : x)) })
  const pasted = parseHexColours(paste)
  return (
    <Modal title="My colours" subtitle="Used by every chart and network set to “My colours”; kept in this browser." onClose={onClose} palette={false}
      footer={<>
        <button type="button" className="btn btn-ghost" onClick={() => setP(DEFAULT_CUSTOM_PALETTE)}>Reset to defaults</button>
        <button type="button" className="btn btn-secondary" onClick={onClose}>Cancel</button>
        <button type="button" className="btn btn-primary" onClick={() => { setCustomPalette(p); onClose() }}>Apply</button>
      </>}>
      <div className="stack-sm custom-palette">
        <h3>Category colours</h3>
        <p className="muted small">In order: sources, runs, groups and series take the first colour, then the next. The first two are also used for up / down.</p>
        <div className="swatch-row">
          {p.colors.map((c, i) => (
            <span key={i} className="swatch-edit">
              <input type="color" value={c} aria-label={`Colour ${i + 1}`} onChange={(e) => setColour(i, e.target.value)} />
              <code className="small">{c}</code>
              <button type="button" className="link-btn small" aria-label={`Remove colour ${i + 1}`} disabled={p.colors.length <= 1}
                onClick={() => set({ colors: p.colors.filter((_, j) => j !== i) })}>×</button>
            </span>
          ))}
          {p.colors.length < MAX_CUSTOM_COLOURS && (
            <button type="button" className="btn btn-ghost btn-sm" onClick={() => set({ colors: [...p.colors, '#888888'] })}>+ Add colour</button>
          )}
        </div>
        <label className="small" htmlFor="palette-paste">Or paste hex codes (e.g. from ColorBrewer or a journal style guide)</label>
        <div className="row">
          <input id="palette-paste" className="input input-sm" value={paste} placeholder="#1b9e77, #d95f02, #7570b3" onChange={(e) => setPaste(e.target.value)} />
          <button type="button" className="btn btn-secondary btn-sm" disabled={!pasted.length}
            onClick={() => { set({ colors: pasted.slice(0, MAX_CUSTOM_COLOURS) }); setPaste('') }}>Use {pasted.length || ''} colours</button>
        </div>
        <h3>Continuous scale</h3>
        <p className="muted small">Heatmaps and p-value colours run from the low to the high colour.</p>
        <div className="row">
          <label className="swatch-edit small">Low <input type="color" value={p.low} onChange={(e) => set({ low: e.target.value })} /></label>
          <span className="scale-preview" style={{ background: `linear-gradient(90deg, ${p.low}, ${p.high})` }} aria-hidden="true" />
          <label className="swatch-edit small">High <input type="color" value={p.high} onChange={(e) => set({ high: e.target.value })} /></label>
        </div>
      </div>
    </Modal>
  )
}
