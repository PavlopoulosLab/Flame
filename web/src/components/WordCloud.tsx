import cloud from 'd3-cloud'
import { useEffect, useMemo, useRef, useState } from 'react'
import { categorical, usePaletteMode } from '../lib/palette'
import { ExportImageButton } from './ExportImage'
import { svgSource } from './viz'

export interface CloudWord { word: string; weight: number }
interface Placed { text: string; size: number; x: number; y: number; rotate: number; weight: number }

/** Small seeded PRNG so a cloud keeps the same layout between renders. */
function seeded(seed: number) {
  let s = seed >>> 0
  return () => { s = (s * 1664525 + 1013904223) >>> 0; return s / 4294967296 }
}

/**
 * Word cloud of term names weighted by significance (d3-cloud layout).
 * Click a word to act on it (e.g. select the terms that contain it).
 */
export function WordCloud({ words, width = 560, height = 260, onWord, active, svgRef, maxWords = 60, exportName = 'flame_wordcloud' }: {
  words: CloudWord[]; width?: number; height?: number; onWord?: (w: string) => void; active?: string | null
  svgRef?: { current: SVGSVGElement | null }; maxWords?: number; exportName?: string
}) {
  const own = useRef<SVGSVGElement | null>(null)
  const pm = usePaletteMode()
  const [placed, setPlaced] = useState<Placed[]>([])
  const key = useMemo(() => words.slice(0, maxWords).map((w) => `${w.word}:${w.weight}`).join('|'), [words, maxWords])
  const layoutRef = useRef<{ stop: () => unknown } | null>(null)
  useEffect(() => {
    const ws = words.slice(0, maxWords)
    if (!ws.length) { setPlaced([]); return }
    const max = Math.max(...ws.map((w) => w.weight)), min = Math.min(...ws.map((w) => w.weight))
    const size = (v: number) => 11 + (max === min ? 18 : ((Math.sqrt(v) - Math.sqrt(min)) / (Math.sqrt(max) - Math.sqrt(min))) * 34)
    const rnd = seeded(ws.length * 7919 + Math.round(max))
    layoutRef.current?.stop()
    const layout = cloud<Placed>()
      .size([width, height])
      .words(ws.map((w) => ({ text: w.word, size: size(w.weight), weight: w.weight, x: 0, y: 0, rotate: 0 })))
      .padding(2)
      .rotate(() => (rnd() < 0.18 ? 90 : 0))
      .font('Inter, system-ui, sans-serif')
      .fontWeight(600)
      .fontSize((d) => d.size)
      .random(rnd)
      .on('end', (out) => setPlaced(out as Placed[]))
    layoutRef.current = layout
    layout.start()
    return () => { layout.stop() }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, width, height])
  const pal = categorical(pm)
  if (!words.length) return <p className="muted small">No words to show.</p>
  return (
    <div className="figure">
    <div className="plot-tools"><ExportImageButton source={() => svgSource(own.current, exportName, { selection: !!onWord })} /></div>
    <svg ref={(el) => { own.current = el; if (svgRef) svgRef.current = el }} viewBox={`${-width / 2} ${-height / 2} ${width} ${height}`} width="100%" style={{ maxWidth: width }} role="img"
      aria-label={`Word cloud of ${words.length} words; larger words come from more significant terms`} className="word-cloud">
      {placed.map((w, i) => (
        <text key={w.text} transform={`translate(${w.x},${w.y}) rotate(${w.rotate})`} textAnchor="middle" fontSize={w.size} fontWeight={600}
          fontFamily="Inter, system-ui, sans-serif" fill={pal[i % pal.length]} opacity={active && active !== w.text ? 0.25 : 1}
          style={{ cursor: onWord ? 'pointer' : undefined }} onClick={() => onWord?.(w.text)}>
          <title>{`${w.text}: weight ${w.weight.toFixed(1)} (sum of −log10 p of the terms containing it)${onWord ? ' — click to select those terms' : ''}`}</title>
          {w.text}
        </text>
      ))}
    </svg>
    </div>
  )
}
