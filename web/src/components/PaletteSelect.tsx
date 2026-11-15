import { PALETTES, usePaletteScope, type PaletteMode } from '../lib/palette'

/** Palette menu of the nearest visualization (renders nothing outside a PaletteScope). */
export function PaletteSelect() {
  const s = usePaletteScope()
  if (!s) return null
  const hint = PALETTES.find((p) => p.value === s.mode)?.hint
  return (
    <label className="palette-select" title={`Colours of this visualization: ${hint}`}>
      <svg width="16" height="16" viewBox="0 0 24 24" aria-hidden="true">
        <circle cx="12" cy="12" r="9" fill="none" stroke="currentColor" strokeWidth="1.6" />
        <path d="M12 3a9 9 0 0 1 0 18z" fill="currentColor" />
      </svg>
      <select className="input input-sm" value={s.mode} onChange={(e) => s.setMode(e.target.value as PaletteMode)} aria-label="Colours of this visualization">
        {PALETTES.map((p) => <option key={p.value} value={p.value}>{p.label}</option>)}
      </select>
    </label>
  )
}
