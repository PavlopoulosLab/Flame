import { useState } from 'react'
import { Modal } from './ui'
import {
  FORMATS, SCALES, describeSize, exportImage, openImageExport, useImageExport,
  type Area, type Background, type ImageFormat, type ImageSource,
} from '../lib/imageExport'

// Choices are remembered for the session, so exporting several figures keeps the same settings.
const last: { format: ImageFormat; scale: number; background: Background; area: Area; legend: boolean; plain: boolean } =
  { format: 'png', scale: 2, background: 'theme', area: 'all', legend: true, plain: true }

/** The export dialog; mounted once in the app shell and opened with openImageExport(). */
export function ExportImageHost() {
  const source = useImageExport((s) => s.source)
  const seq = useImageExport((s) => s.seq)
  const close = useImageExport((s) => s.close)
  if (!source) return null
  return <ExportImageDialog key={seq} source={source} onClose={close} />
}

function ExportImageDialog({ source, onClose }: { source: ImageSource; onClose: () => void }) {
  const [format, setFormat] = useState<ImageFormat>(last.format)
  const [scale, setScale] = useState(last.scale)
  const [bgChoice, setBackground] = useState<Background>(last.background)
  const [area, setArea] = useState<Area>(source.features.area ? last.area : 'all')
  const [legend, setLegend] = useState(last.legend)
  const [plain, setPlain] = useState(last.plain)
  const [filename, setFilename] = useState(source.name)
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const fmt = FORMATS.find((f) => f.value === format)!
  const transparentOk = !!fmt.alpha
  // JPEG and PDF have no transparency: fall back to white.
  const background: Background = bgChoice === 'transparent' && !transparentOk ? 'white' : bgChoice
  const { w, h } = source.size(area)

  const save = async () => {
    Object.assign(last, { format, scale, background, area, legend, plain })
    setBusy(true); setError(null)
    try {
      await exportImage(source, { format, scale, backgroundMode: background, area, legend, plain, filename })
      onClose()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  return (
    <Modal title="Export image" subtitle="Saves the current view as it looks, including labels and legends." onClose={onClose}
      footer={<>
        <button className="btn btn-secondary" onClick={onClose}>Close</button>
        <button className="btn btn-primary" onClick={save} disabled={busy}>{busy ? 'Saving…' : 'Save image'}</button>
      </>}>
      <div className="export-image" data-testid="export-image">
        <fieldset className="export-formats">
          <legend>Format</legend>
          {FORMATS.map((f) => (
            <label key={f.value} className={`export-format${format === f.value ? ' on' : ''}`}>
              <input type="radio" name="export-format" value={f.value} checked={format === f.value} onChange={() => setFormat(f.value)} />
              <span><strong>{f.label}</strong><span className="small muted">{f.text}</span></span>
            </label>
          ))}
        </fieldset>
        <div className="export-grid">
          {source.features.area && <>
            <label htmlFor="export-area">Area</label>
            <select id="export-area" className="input" value={area} onChange={(e) => setArea(e.target.value as Area)}>
              <option value="all">Whole network</option>
              <option value="view">Current view (as zoomed on screen)</option>
            </select>
          </>}
          <label htmlFor="export-scale">Resolution</label>
          <div>
            <select id="export-scale" className="input" value={scale} onChange={(e) => setScale(Number(e.target.value))} disabled={fmt.vector}>
              {SCALES.map((s) => <option key={s.value} value={s.value}>{s.label}</option>)}
            </select>
            <p className="export-size small muted">{fmt.vector ? `Vector image · ${Math.round(w)} × ${Math.round(h)} at on-screen size; sharp at any size` : describeSize(w, h, scale)}</p>
          </div>
          <label htmlFor="export-bg">Background</label>
          <select id="export-bg" className="input" value={background} onChange={(e) => setBackground(e.target.value as Background)}>
            <option value="theme">Theme color</option>
            <option value="white">White</option>
            <option value="transparent" disabled={!transparentOk}>Transparent{transparentOk ? '' : ` (not available in ${fmt.label})`}</option>
          </select>
          <label htmlFor="export-name">File name</label>
          <span className="export-name">
            <input id="export-name" className="input" value={filename} onChange={(e) => setFilename(e.target.value)} />
            <span className="muted">.{fmt.ext}</span>
          </span>
        </div>
        {source.features.legend && (
          <label className="check"><input type="checkbox" checked={legend} onChange={(e) => setLegend(e.target.checked)} /> Include the legend and colour bar</label>
        )}
        {source.features.selection && (
          <label className="check"><input type="checkbox" checked={plain} onChange={(e) => setPlain(e.target.checked)} /> Leave out selection and search highlighting</label>
        )}
        {error && <p className="error small" role="alert">{error}</p>}
      </div>
    </Modal>
  )
}

/** Small button that opens the export dialog for a figure. */
export function ExportImageButton({ source, className = 'btn btn-ghost btn-sm' }: { source: () => ImageSource | null; className?: string }) {
  return (
    <button type="button" className={`${className} export-image-btn`} data-no-export="" title="Save this figure as PNG, SVG, PDF, JPEG or WebP"
      onClick={() => { const s = source(); if (s) openImageExport(s) }}>⤓ Image</button>
  )
}
