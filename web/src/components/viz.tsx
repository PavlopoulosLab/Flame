import { arc as d3arc, chord as d3chord, curveBundle, descending, line as d3line, ribbon as d3ribbon } from 'd3'
import { useMemo, useRef, useState } from 'react'
import { BrushModes, pointInPolygon, useBrush, type BrushMode, type Pt } from '../lib/brush'
import { saveBlob } from '../api/client'

/** Client → SVG user coordinates. */
const toSvgPoint = (svg: SVGSVGElement | null, x: number, y: number): Pt => {
  const m = svg?.getScreenCTM()
  if (!svg || !m) return [x, y]
  const p = new DOMPoint(x, y).matrixTransform(m.inverse())
  return [p.x, p.y]
}

/** Download an inline SVG element as an .svg file (fonts/colours inlined via computed style). */
export function downloadSvg(svg: SVGSVGElement | null, filename: string) {
  if (!svg) return
  const clone = svg.cloneNode(true) as SVGSVGElement
  clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg')
  const css = getComputedStyle(document.documentElement)
  clone.style.fontFamily = 'Inter, system-ui, sans-serif'
  clone.style.color = css.getPropertyValue('--heading-color')
  saveBlob(new Blob([new XMLSerializer().serializeToString(clone)], { type: 'image/svg+xml' }), filename.endsWith('.svg') ? filename : `${filename}.svg`)
}

// ---------------------------------------------------------------------------
// Chord diagram: arcs = items (terms or runs), ribbons = shared members.
// ---------------------------------------------------------------------------
export function ChordDiagram({ names, matrix, colors, onSelect, selected, size = 620, svgRef, unit = 'shared genes' }: {
  names: string[]; matrix: number[][]; colors: string[]; onSelect?: (i: number, j?: number) => void
  selected?: Set<number>; size?: number; svgRef?: { current: SVGSVGElement | null }; unit?: string
}) {
  const outer = size / 2 - 150
  const inner = outer - 14
  const chords = useMemo(() => d3chord().padAngle(0.03).sortSubgroups(descending)(matrix), [matrix])
  const arcGen = d3arc<{ startAngle: number; endAngle: number }>().innerRadius(inner).outerRadius(outer)
  const ribbonGen = d3ribbon<unknown, { startAngle: number; endAngle: number; radius?: number }>().radius(inner - 1)
  const anySel = !!selected && selected.size > 0
  return (
    <svg ref={svgRef ?? undefined} viewBox={`${-size / 2} ${-size / 2} ${size} ${size}`} width="100%" style={{ maxWidth: size }} role="img"
      aria-label={`Chord diagram of ${names.length} items; ribbons show ${unit}`}>
      <g>
        {chords.map((c, k) => {
          const on = !anySel || selected!.has(c.source.index) || selected!.has(c.target.index)
          return (
            <path key={k} d={ribbonGen(c as never) as unknown as string} fill={colors[c.source.index]} fillOpacity={on ? 0.6 : 0.08}
              stroke="none" style={{ cursor: onSelect ? 'pointer' : undefined }} onClick={() => onSelect?.(c.source.index, c.target.index)}>
              <title>{`${names[c.source.index]} ↔ ${names[c.target.index]}: ${c.source.value} ${unit}`}</title>
            </path>
          )
        })}
      </g>
      <g>
        {chords.groups.map((g) => {
          const angle = (g.startAngle + g.endAngle) / 2
          const flip = angle > Math.PI
          const label = names[g.index].length > 28 ? `${names[g.index].slice(0, 27)}…` : names[g.index]
          return (
            <g key={g.index} style={{ cursor: onSelect ? 'pointer' : undefined }} onClick={() => onSelect?.(g.index)}>
              <path d={arcGen(g) ?? ''} fill={colors[g.index]} stroke={selected?.has(g.index) ? 'currentColor' : 'none'} strokeWidth={2}>
                <title>{`${names[g.index]}: ${Math.round(g.value)} ${unit} in total`}</title>
              </path>
              <text transform={`rotate(${(angle * 180) / Math.PI - 90}) translate(${outer + 8},0)${flip ? ' rotate(180)' : ''}`}
                textAnchor={flip ? 'end' : 'start'} dominantBaseline="middle" fontSize={11} fill="currentColor">{label}</text>
            </g>
          )
        })}
      </g>
    </svg>
  )
}

// ---------------------------------------------------------------------------
// Hive plot: one axis per group (e.g. genes, and terms per annotation source);
// nodes are placed along their axis by rank; links are curved between axes.
// ---------------------------------------------------------------------------
export interface HiveNode { id: string; label: string; axis: string; rank: number; color: string }
export function HivePlot({ axes, nodes, links, onSelect, selected, size = 640, svgRef }: {
  axes: { name: string; color: string }[]; nodes: HiveNode[]; links: { source: string; target: string }[]
  onSelect?: (ids: string[]) => void; selected?: Set<string>; size?: number; svgRef?: { current: SVGSVGElement | null }
}) {
  const r0 = 40, r1 = size / 2 - 70
  const angleOf = (axis: string) => (2 * Math.PI * axes.findIndex((a) => a.name === axis)) / axes.length - Math.PI / 2
  const maxRank = useMemo(() => {
    const m = new Map<string, number>()
    nodes.forEach((n) => m.set(n.axis, Math.max(m.get(n.axis) ?? 0, n.rank)))
    return m
  }, [nodes])
  const pos = useMemo(() => {
    const p = new Map<string, [number, number]>()
    nodes.forEach((n) => {
      const a = angleOf(n.axis)
      const rr = r0 + ((r1 - r0) * (n.rank + 0.5)) / ((maxRank.get(n.axis) ?? 0) + 1)
      p.set(n.id, [Math.cos(a) * rr, Math.sin(a) * rr])
    })
    return p
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [nodes, axes, maxRank])
  const anySel = !!selected && selected.size > 0
  const nodeById = new Map(nodes.map((n) => [n.id, n]))
  const inner = useRef<SVGSVGElement | null>(null)
  const [mode, setMode] = useState<BrushMode>('click')
  const brush = useBrush(mode, (x, y) => toSvgPoint(inner.current, x, y), (poly, add) => {
    const ids = nodes.filter((n) => { const p = pos.get(n.id); return p && pointInPolygon(p, poly) }).map((n) => n.id)
    if (ids.length) onSelect?.(add ? [...(selected ?? []), ...ids] : ids)
  })
  return (
    <div className="circos">
    {onSelect && <BrushModes mode={mode} onChange={setMode} />}
    <svg ref={(el) => { inner.current = el; if (svgRef) svgRef.current = el }} viewBox={`${-size / 2} ${-size / 2} ${size} ${size}`} width="100%" style={{ maxWidth: size }} role="img"
      aria-label={`Hive plot with ${axes.length} axes and ${nodes.length} nodes`}>
      {axes.map((a) => {
        const ang = angleOf(a.name)
        return (
          <g key={a.name}>
            <line x1={Math.cos(ang) * r0} y1={Math.sin(ang) * r0} x2={Math.cos(ang) * r1} y2={Math.sin(ang) * r1} stroke={a.color} strokeWidth={3} strokeOpacity={0.6} />
            <text x={Math.cos(ang) * (r1 + 24)} y={Math.sin(ang) * (r1 + 24)} textAnchor="middle" dominantBaseline="middle" fontSize={12} fontWeight={600} fill="currentColor">{a.name}</text>
          </g>
        )
      })}
      <g fill="none">
        {links.map((l, i) => {
          const s = pos.get(l.source), t = pos.get(l.target)
          if (!s || !t) return null
          const on = !anySel || selected!.has(l.source) || selected!.has(l.target)
          // Quadratic curve bending through a point pulled toward the centre.
          const cx = (s[0] + t[0]) / 3, cy = (s[1] + t[1]) / 3
          return <path key={i} d={`M${s[0]},${s[1]} Q${cx},${cy} ${t[0]},${t[1]}`} stroke={nodeById.get(l.target)?.color ?? '#9ca3af'} strokeOpacity={on ? 0.35 : 0.04} strokeWidth={1} />
        })}
      </g>
      {nodes.map((n) => {
        const p = pos.get(n.id)!
        const on = selected?.has(n.id)
        return (
          <circle key={n.id} cx={p[0]} cy={p[1]} r={on ? 6 : 4} fill={n.color} stroke={on ? 'currentColor' : 'var(--surface-color)'} strokeWidth={1.5}
            style={{ cursor: onSelect ? 'pointer' : undefined }} onClick={(e) => onSelect?.(e.shiftKey ? [...(selected ?? []), n.id] : [n.id])}>
            <title>{n.label}</title>
          </circle>
        )
      })}
      {mode !== 'click' && <rect x={-size / 2} y={-size / 2} width={size} height={size} fill="transparent" style={{ cursor: 'crosshair', touchAction: 'none' }} {...brush.handlers} />}
      {brush.path && <path className="brush-path" d={brush.path} pointerEvents="none" />}
    </svg>
    </div>
  )
}

// ---------------------------------------------------------------------------
// UpSet plot of items shared between sets (exclusive intersections).
// ---------------------------------------------------------------------------
export function SetOverlapUpSet({ sets, membership, colors, onSelect, describe, itemNoun = 'terms' }: {
  sets: { id: string; label: string }[]; membership: { item: string; sets: string[] }[]; colors: string[]
  onSelect?: (items: string[]) => void
  /** Readable name for an item (e.g. term name), used when copying. */
  describe?: (item: string) => string; itemNoun?: string
}) {
  const [active, setActive] = useState<{ key: string; items: string[] } | null>(null)
  const comboName = (key: string) => sets.filter((_, i) => key[i] === '1').map((s) => s.label).join(' + ')
  const combos = useMemo(() => {
    const m = new Map<string, string[]>()
    membership.forEach(({ item, sets: s }) => {
      const key = sets.map((x) => (s.includes(x.id) ? '1' : '0')).join('')
      if (key.includes('1')) m.set(key, [...(m.get(key) ?? []), item])
    })
    return [...m.entries()].map(([key, items]) => ({ key, items })).sort((a, b) => b.items.length - a.items.length)
  }, [sets, membership])
  const sizes = sets.map((s) => membership.filter((m) => m.sets.includes(s.id)).length)
  const max = Math.max(1, ...combos.map((c) => c.items.length))
  const maxSize = Math.max(1, ...sizes)
  const ref = useRef<HTMLDivElement>(null)
  return (
    <div className="upset-plot" ref={ref}>
      <div className="upset-sets">
        {sets.map((s, i) => (
          <div key={s.id} className="upset-set-row">
            <span className="upset-set-bar"><span style={{ width: `${(sizes[i] / maxSize) * 100}%`, background: colors[i % colors.length] }} /></span>
            <span className="upset-set-label" title={s.label}>{s.label}</span>
            <span className="upset-count">{sizes[i]}</span>
          </div>
        ))}
      </div>
      <div className="upset-combos" role="list">
        {combos.map((c) => (
          <button key={c.key} role="listitem" className={`upset-col${active?.key === c.key ? ' active' : ''}`} onClick={() => { setActive(c); onSelect?.(c.items) }}
            title={`${c.items.length} terms found only by: ${sets.filter((_, i) => c.key[i] === '1').map((s) => s.label).join(' + ')} — click to select`}>
            <span className="upset-col-count">{c.items.length}</span>
            <span className="upset-col-bar"><span style={{ height: `${(c.items.length / max) * 100}%` }} /></span>
            <span className="upset-col-dots">{[...c.key].map((b, i) => <i key={i} className={b === '1' ? 'on' : ''} style={b === '1' ? { background: colors[i % colors.length] } : undefined} />)}</span>
          </button>
        ))}
      </div>
      <div className="upset-copy row wrap">
        {active ? (
          <>
            <span className="small"><strong>{active.items.length} {itemNoun}</strong> found only by {comboName(active.key)}:</span>
            <CopyText label="Copy IDs" text={active.items.join('\n')} />
            {describe && <CopyText label="Copy names" text={active.items.map(describe).join('\n')} />}
            {describe && <CopyText label="Copy table" text={['id\tname', ...active.items.map((i) => `${i}\t${describe(i)}`)].join('\n')} />}
          </>
        ) : <span className="muted small">Click a column to select its {itemNoun} and copy them.</span>}
        <CopyText label="Copy all combinations" text={['combination\tcount\tids', ...combos.map((c) => `${comboName(c.key)}\t${c.items.length}\t${c.items.join(',')}`)].join('\n')} />
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => saveBlob(new Blob([['combination\tcount\tid\tname', ...combos.flatMap((c) => c.items.map((i) => `${comboName(c.key)}\t${c.items.length}\t${i}\t${describe ? describe(i) : ''}`))].join('\n')], { type: 'text/tab-separated-values' }), 'flame_upset_combinations.tsv')}>Download (TSV)</button>
      </div>
    </div>
  )
}

function CopyText({ label, text }: { label: string; text: string }) {
  const [done, setDone] = useState(false)
  return (
    <button type="button" className="btn btn-ghost btn-sm" onClick={() => navigator.clipboard.writeText(text).then(() => { setDone(true); setTimeout(() => setDone(false), 1500) })}>
      {done ? 'Copied' : label}
    </button>
  )
}

// ---------------------------------------------------------------------------
// Circos-style plot with hierarchical edge bundling.
// Items sit on a circle grouped into coloured sectors; an outer track shows a
// value per item as bars; links are bundled through their sector hubs and the
// centre (Holten's hierarchical edge bundling via d3.curveBundle).
// ---------------------------------------------------------------------------
export interface CircosSector { id: string; label: string; color: string }
export interface CircosItem { id: string; label: string; sector: string; value?: number; color?: string; title?: string; /** angular share (default 1) */ weight?: number }
export interface CircosLink { source: string; target: string; color?: string; title?: string }

export function CircosBundle({ sectors, items, links, onSelect, selected, size = 760, svgRef, trackLabel, defaultLabels = true, linkLabel = 'links' }: {
  sectors: CircosSector[]; items: CircosItem[]; links: CircosLink[]
  onSelect?: (ids: string[], additive: boolean) => void; selected?: Set<string>
  size?: number; svgRef?: { current: SVGSVGElement | null }; trackLabel?: string; defaultLabels?: boolean; linkLabel?: string
}) {
  const [tension, setTension] = useState(0.85)
  const [labels, setLabels] = useState(defaultLabels && items.length <= 160)
  const [hover, setHover] = useState<string | null>(null)
  const [mode, setMode] = useState<BrushMode>('click')
  const inner = useRef<SVGSVGElement | null>(null)
  const margin = labels ? 190 : 64
  const R = size / 2 - margin
  const trackIn = R + 20, trackH = 34
  const layout = useMemo(() => {
    const used = sectors.filter((s) => items.some((i) => i.sector === s.id))
    const gap = used.length > 1 ? Math.min(0.08, 1.2 / used.length) : 0
    const totalW = items.reduce((acc, i) => acc + (i.weight ?? 1), 0)
    const unit = (2 * Math.PI - gap * used.length) / Math.max(1e-9, totalW)
    const angle = new Map<string, number>()
    const width = new Map<string, number>()
    const arcs: { s: CircosSector; a0: number; a1: number }[] = []
    let a = 0
    used.forEach((s) => {
      const a0 = a
      items.filter((i) => i.sector === s.id).forEach((it) => {
        const w = (it.weight ?? 1) * unit
        angle.set(it.id, a + w / 2); width.set(it.id, w); a += w
      })
      arcs.push({ s, a0, a1: a })
      a += gap
    })
    const max = Math.max(1e-9, ...items.map((i) => i.value ?? 0))
    return { angle, width, arcs, max }
  }, [sectors, items])
  const sectorOf = useMemo(() => new Map(items.map((i) => [i.id, i.sector])), [items])
  const colorOfSector = useMemo(() => new Map(sectors.map((s) => [s.id, s.color])), [sectors])
  const hubAngle = useMemo(() => new Map(layout.arcs.map((x) => [x.s.id, (x.a0 + x.a1) / 2])), [layout])
  const pt = (ang: number, r: number): [number, number] => [Math.sin(ang) * r, -Math.cos(ang) * r]
  const line = useMemo(() => d3line<[number, number]>().curve(curveBundle.beta(tension)), [tension])
  const paths = useMemo(() => links.map((l) => {
    const a = layout.angle.get(l.source), b = layout.angle.get(l.target)
    if (a === undefined || b === undefined) return null
    const sa = sectorOf.get(l.source)!, sb = sectorOf.get(l.target)!
    const pts: [number, number][] = [pt(a, R - 4)]
    if (sa === sb) pts.push(pt(hubAngle.get(sa)!, R * 0.6))
    else pts.push(pt(hubAngle.get(sa)!, R * 0.45), [0, 0], pt(hubAngle.get(sb)!, R * 0.45))
    pts.push(pt(b, R - 4))
    return { l, d: line(pts) ?? '' }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }), [links, layout, line, R, sectorOf, hubAngle])
  const focus = hover ? new Set([hover]) : selected && selected.size ? selected : null
  const brush = useBrush(mode, (x, y) => toSvgPoint(inner.current, x, y), (poly, add) => {
    const ids = items.filter((it) => { const a = layout.angle.get(it.id); return a !== undefined && pointInPolygon(pt(a, R - 4), poly) }).map((it) => it.id)
    if (ids.length) onSelect?.(ids, add)
  })
  const arc = d3arc<{ startAngle: number; endAngle: number }>()
  const deg = (r: number) => (r * 180) / Math.PI
  return (
    <div className="circos">
      <div className="toolbar">
        <label title="0 = straight lines, 1 = tightly bundled">Bundling <input type="range" min={0} max={1} step={0.05} value={tension} onChange={(e) => setTension(Number(e.target.value))} /></label>
        <label className="check"><input type="checkbox" checked={labels} onChange={(e) => setLabels(e.target.checked)} /> Labels</label>
        {onSelect && <BrushModes mode={mode} onChange={setMode} />}
        <span className="muted small">{items.length} items · {links.length} {linkLabel}{trackLabel ? ` · outer bars: ${trackLabel}` : ''} · hover to trace, click to select</span>
      </div>
      <svg ref={(el) => { inner.current = el; if (svgRef) svgRef.current = el }} viewBox={`${-size / 2} ${-size / 2} ${size} ${size}`} width="100%" style={{ maxWidth: size }} role="img"
        aria-label={`Circos plot: ${items.length} items in ${layout.arcs.length} sectors with ${links.length} bundled ${linkLabel}`}>
        <g fill="none">
          {paths.map((p, i) => {
            if (!p) return null
            const on = !focus || focus.has(p.l.source) || focus.has(p.l.target)
            return (
              <path key={i} d={p.d} stroke={p.l.color ?? colorOfSector.get(sectorOf.get(p.l.source)!) ?? '#9ca3af'}
                strokeOpacity={focus ? (on ? 0.85 : 0.04) : 0.35} strokeWidth={on && focus ? 1.6 : 1}>
                {p.l.title && <title>{p.l.title}</title>}
              </path>
            )
          })}
        </g>
        {layout.arcs.map(({ s, a0, a1 }) => (
          <path key={s.id} d={arc({ startAngle: a0, endAngle: a1, innerRadius: R + 2, outerRadius: R + 12 } as never) ?? ''} fill={s.color}
            style={{ cursor: onSelect ? 'pointer' : undefined }} onClick={(e) => onSelect?.(items.filter((i) => i.sector === s.id).map((i) => i.id), e.shiftKey)}>
            <title>{`${s.label}: ${items.filter((i) => i.sector === s.id).length} items`}</title>
          </path>
        ))}
        {trackLabel && <circle r={trackIn} fill="none" stroke="currentColor" strokeOpacity={0.12} />}
        {items.map((it) => {
          const a = layout.angle.get(it.id)!
          const on = selected?.has(it.id)
          const [x, y] = pt(a, R - 4)
          const len = it.value !== undefined ? Math.max(1, (trackH * it.value) / layout.max) : 0
          const w = Math.max(0.004, (layout.width.get(it.id) ?? 0.01) * 0.8)
          const flip = a > Math.PI
          const color = it.color ?? colorOfSector.get(it.sector) ?? '#9ca3af'
          return (
            <g key={it.id} style={{ cursor: onSelect ? 'pointer' : undefined }}
              onMouseEnter={() => setHover(it.id)} onMouseLeave={() => setHover(null)}
              onClick={(e) => onSelect?.([it.id], e.shiftKey || e.metaKey || e.ctrlKey)}>
              <title>{it.title ?? it.label}</title>
              {len > 0 && <path d={arc({ startAngle: a - w / 2, endAngle: a + w / 2, innerRadius: trackIn, outerRadius: trackIn + len } as never) ?? ''} fill={color} fillOpacity={0.85} />}
              <circle cx={x} cy={y} r={on ? 4.5 : 3} fill={color} stroke={on ? 'currentColor' : 'none'} strokeWidth={1.5} />
              {labels && (
                <text transform={`rotate(${deg(a) - 90}) translate(${trackIn + trackH + 6},0)${flip ? ' rotate(180)' : ''}`}
                  textAnchor={flip ? 'end' : 'start'} dominantBaseline="middle" fontSize={Math.max(7, Math.min(11, ((layout.width.get(it.id) ?? 0.01) * (trackIn + trackH)) * 0.9))}
                  fill="currentColor" fontWeight={on || hover === it.id ? 700 : 400}>
                  {it.label.length > 26 ? `${it.label.slice(0, 25)}…` : it.label}
                </text>
              )}
            </g>
          )
        })}
        {mode !== 'click' && <rect x={-size / 2} y={-size / 2} width={size} height={size} fill="transparent" style={{ cursor: 'crosshair', touchAction: 'none' }} {...brush.handlers} />}
        {brush.path && <path className="brush-path" d={brush.path} pointerEvents="none" />}
      </svg>
      <div className="legend">
        {layout.arcs.map(({ s }) => (
          <button key={s.id} type="button" className="legend-btn" onClick={(e) => onSelect?.(items.filter((i) => i.sector === s.id).map((i) => i.id), e.shiftKey)}
            title={`Select all of ${s.label}`}><i style={{ background: s.color }} />{s.label} ({items.filter((i) => i.sector === s.id).length})</button>
        ))}
      </div>
    </div>
  )
}
