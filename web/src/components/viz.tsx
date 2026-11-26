import { arc as d3arc, chord as d3chord, curveBundle, descending, line as d3line, ribbon as d3ribbon } from 'd3'
import { useMemo, useRef, useState } from 'react'
import { BrushModes, pointInPolygon, useBrush, type BrushMode, type Pt } from '../lib/brush'
import { saveBlob } from '../api/client'
import { ExportImageButton } from './ExportImage'
import { htmlElementToString, htmlFigureSize, svgElementToString, svgFigureSize, type ImageSource, type LegendItem } from '../lib/imageExport'
import { saveXlsx } from '../lib/xlsx'

/** Client → SVG user coordinates. */
const toSvgPoint = (svg: SVGSVGElement | null, x: number, y: number): Pt => {
  const m = svg?.getScreenCTM()
  if (!svg || !m) return [x, y]
  const p = new DOMPoint(x, y).matrixTransform(m.inverse())
  return [p.x, p.y]
}

/** Export source for an inline SVG figure (optionally with a legend drawn underneath). */
export function svgSource(svg: SVGSVGElement | null, name: string, opts: { selection?: boolean; legend?: LegendItem[] } = {}): ImageSource | null {
  if (!svg) return null
  return {
    name,
    size: () => svgFigureSize(svg, opts.legend),
    features: { selection: opts.selection ?? true, legend: !!opts.legend?.length },
    svg: (o) => svgElementToString(svg, o, opts.legend),
  }
}

/** Export source for an HTML figure (parts marked data-no-export are left out). */
export function htmlSource(el: HTMLElement | null, name: string, selection = true): ImageSource | null {
  if (!el) return null
  return { name, size: () => htmlFigureSize(el), features: { selection }, svg: (o) => htmlElementToString(el, o) }
}

// ---------------------------------------------------------------------------
// Chord diagram: arcs = items (terms or runs), ribbons = shared members.
// ---------------------------------------------------------------------------
export function ChordDiagram({ names, matrix, colors, onSelect, selected, size = 620, svgRef, unit = 'shared genes', exportName = 'flame_chord', title }: {
  names: string[]; matrix: number[][]; colors: string[]; onSelect?: (i: number, j?: number) => void
  selected?: Set<number>; size?: number; svgRef?: { current: SVGSVGElement | null }; unit?: string; exportName?: string
  /** Title drawn in the top-left corner of the diagram (and its exported image). */
  title?: string
}) {
  const own = useRef<SVGSVGElement | null>(null)
  const outer = size / 2 - 150
  const inner = outer - 14
  const chords = useMemo(() => d3chord().padAngle(0.03).sortSubgroups(descending)(matrix), [matrix])
  const arcGen = d3arc<{ startAngle: number; endAngle: number }>().innerRadius(inner).outerRadius(outer)
  const ribbonGen = d3ribbon<unknown, { startAngle: number; endAngle: number; radius?: number }>().radius(inner - 1)
  const anySel = !!selected && selected.size > 0
  return (
    <div className="figure">
    <div className="plot-tools"><ExportImageButton source={() => svgSource(own.current, exportName, { selection: !!onSelect })} /></div>
    <svg ref={(el) => { own.current = el; if (svgRef) svgRef.current = el }} viewBox={`${-size / 2} ${-size / 2} ${size} ${size}`} width="100%" style={{ maxWidth: size }} role="img"
      aria-label={`Chord diagram of ${names.length} items; ribbons show ${unit}`}>
      {title && <text x={-size / 2 + 8} y={-size / 2 + 22} fontSize={16} fontWeight={700} fill="currentColor">{title}</text>}
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
    </div>
  )
}

// ---------------------------------------------------------------------------
// Hive plot: one axis per group (e.g. genes, and terms per annotation source);
// nodes are placed along their axis by rank; links are curved between axes.
// ---------------------------------------------------------------------------
export interface HiveNode { id: string; label: string; axis: string; rank: number; color: string }
export function HivePlot({ axes, nodes, links, onSelect, selected, size = 640, svgRef, exportName = 'flame_hive' }: {
  axes: { name: string; color: string }[]; nodes: HiveNode[]; links: { source: string; target: string }[]
  onSelect?: (ids: string[]) => void; selected?: Set<string>; size?: number; svgRef?: { current: SVGSVGElement | null }; exportName?: string
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
    <div className="plot-tools">
      {onSelect && <BrushModes mode={mode} onChange={setMode} />}
      <ExportImageButton source={() => svgSource(inner.current, exportName, { selection: !!onSelect })} />
    </div>
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
type UpsetBarMode = 'distinct' | 'intersection' | 'union'
const UPSET_BAR_MODES: { value: UpsetBarMode; label: string; help: string }[] = [
  { value: 'distinct', label: 'Only in', help: 'found by exactly these sets and no other (the bars never overlap, so any set operation is a choice of bars)' },
  { value: 'intersection', label: 'Shared by', help: 'found by all of these sets, whether or not others found them too' },
  { value: 'union', label: 'Union of', help: 'found by at least one of these sets' },
]

export function SetOverlapUpSet({ sets, membership, colors, onSelect, describe, itemNoun = 'terms', exportName = 'flame_upset' }: {
  sets: { id: string; label: string }[]; membership: { item: string; sets: string[] }[]; colors: string[]
  onSelect?: (items: string[]) => void
  /** Readable name for an item (e.g. term name), used when copying. */
  describe?: (item: string) => string; itemNoun?: string; exportName?: string
}) {
  const [mode, setMode] = useState<UpsetBarMode>('distinct')
  const [picked, setPicked] = useState<string[]>([])   // keys ("1010") of the selected bars
  const n = sets.length
  const MAX_BARS = 48
  const comboName = (key: string, m: UpsetBarMode = mode) => {
    const names = sets.filter((_, i) => key[i] === '1').map((s) => s.label)
    return m === 'distinct' ? `Only ${names.join(' + ')}` : m === 'intersection' ? names.join(' ∩ ') : names.join(' ∪ ')
  }
  // Each item's membership as a bit string, e.g. "101" = found by the first and third set.
  const keyed = useMemo(() => membership.map(({ item, sets: s }) => ({ item, key: sets.map((x) => (s.includes(x.id) ? '1' : '0')).join('') })).filter((x) => x.key.includes('1')), [sets, membership])
  const exclusive = useMemo(() => {
    const m = new Map<string, string[]>()
    keyed.forEach(({ item, key }) => m.set(key, [...(m.get(key) ?? []), item]))
    return m
  }, [keyed])
  const { combos, hidden } = useMemo(() => {
    let out: { key: string; items: string[] }[]
    if (mode === 'distinct') out = [...exclusive.entries()].map(([key, items]) => ({ key, items }))
    else {
      // Every non-empty choice of sets (2 or more for unions: a union of one set is the set itself).
      out = []
      const total = n <= 12 ? (1 << n) : 0
      for (let mask = 1; mask < total; mask++) {
        const key = sets.map((_, i) => ((mask >> i) & 1 ? '1' : '0')).join('')
        const size = key.split('1').length - 1
        if (mode === 'union' && size < 2) continue
        const items = keyed.filter((x) => (mode === 'intersection'
          ? [...key].every((b, i) => b === '0' || x.key[i] === '1')
          : [...key].some((b, i) => b === '1' && x.key[i] === '1'))).map((x) => x.item)
        if (items.length) out.push({ key, items })
      }
    }
    out.sort((x, y) => y.items.length - x.items.length || x.key.localeCompare(y.key))
    return { combos: out.slice(0, MAX_BARS), hidden: Math.max(0, out.length - MAX_BARS) }
  }, [mode, exclusive, keyed, sets, n])
  const sizes = sets.map((_, i) => keyed.filter((x) => x.key[i] === '1').length)
  const max = Math.max(1, ...combos.map((c) => c.items.length))
  const maxSize = Math.max(1, ...sizes)
  const ref = useRef<HTMLDivElement>(null)

  const chosen = combos.filter((c) => picked.includes(c.key))
  const items = useMemo(() => [...new Set(chosen.flatMap((c) => c.items))], [chosen])
  const apply = (keys: string[], m: UpsetBarMode = mode) => {
    setPicked(keys)
    // Report the selected items to the page (computed here: state updates are not visible yet).
    const src = m === mode ? combos : [...exclusive.entries()].map(([key, its]) => ({ key, items: its }))
    onSelect?.([...new Set(src.filter((c) => keys.includes(c.key)).flatMap((c) => c.items))])
  }
  const toggle = (key: string) => apply(picked.includes(key) ? picked.filter((k) => k !== key) : [...picked, key])
  // Set operations across all sets = a choice of exclusive bars, by how many sets found the item.
  const quick = (test: (degree: number) => boolean) => {
    const keys = [...exclusive.keys()].filter((k) => test(k.split('1').length - 1))
    setMode('distinct'); apply(keys, 'distinct')
  }
  const changeMode = (m: UpsetBarMode) => { setMode(m); setPicked([]); onSelect?.([]) }

  return (
    <div className="upset-wrap">
      <div className="upset-controls" data-no-export="">
        <label className="small">Each bar shows{' '}
          <span className="segmented" role="radiogroup" aria-label="What each bar shows">
            {UPSET_BAR_MODES.map((m) => <button key={m.value} type="button" role="radio" aria-checked={mode === m.value} className={mode === m.value ? 'active' : ''} title={`The ${itemNoun} ${m.help}`} onClick={() => changeMode(m.value)}>{m.label}</button>)}
          </span>
        </label>
        <span className="row wrap">
          <span className="small muted">Across all {n} sets:</span>
          <button type="button" className="btn btn-secondary btn-sm" title={`The ${itemNoun} found by every set`} onClick={() => quick((d) => d === n)}>Intersection</button>
          <button type="button" className="btn btn-secondary btn-sm" title={`The ${itemNoun} found by at least one set`} onClick={() => quick(() => true)}>Union</button>
          <button type="button" className="btn btn-secondary btn-sm" title={`Exclusive or: the ${itemNoun} found by exactly one set`} onClick={() => quick((d) => d === 1)}>XOR (one set only)</button>
          {n > 2 && <button type="button" className="btn btn-secondary btn-sm" title={`The ${itemNoun} found by two or more sets`} onClick={() => quick((d) => d >= 2)}>In 2 or more</button>}
          {n > 2 && <button type="button" className="btn btn-secondary btn-sm" title={`The ${itemNoun} found by some sets but not by all`} onClick={() => quick((d) => d < n)}>Not in all</button>}
          {picked.length > 0 && <button type="button" className="btn btn-ghost btn-sm" onClick={() => apply([])}>Clear</button>}
        </span>
      </div>
      <div className="upset-plot upset-pick" ref={ref}>
        <div className="upset-sets">
          {sets.map((s, i) => (
            <div key={s.id} className="upset-set-row">
              <span className="upset-set-bar"><span style={{ width: `${(sizes[i] / maxSize) * 100}%`, background: colors[i % colors.length] }} /></span>
              <span className="upset-set-label" title={s.label}>{s.label}</span>
              <span className="upset-count">{sizes[i]}</span>
            </div>
          ))}
        </div>
        <div className="upset-combos" role="group" aria-label="Combinations — click to select, click again to remove">
          {combos.map((c) => {
            const on = picked.includes(c.key)
            return (
              <button key={c.key} type="button" aria-pressed={on} className={`upset-col${on ? ' active' : ''}`} onClick={() => toggle(c.key)}
                title={`${comboName(c.key)}: ${c.items.length} ${itemNoun} — click to ${on ? 'remove from' : 'add to'} the selection`}>
                <span className="upset-col-count">{c.items.length}</span>
                <span className="upset-col-bar"><span style={{ height: `${(c.items.length / max) * 100}%` }} /></span>
                <span className="upset-col-dots">{[...c.key].map((b, i) => <i key={i} className={b === '1' ? 'on' : ''} style={b === '1' ? { background: colors[i % colors.length] } : undefined} />)}</span>
              </button>
            )
          })}
        </div>
      </div>
      {hidden > 0 && <p className="muted small">Showing the {MAX_BARS} largest of {MAX_BARS + hidden} combinations.</p>}
      <div className="upset-copy stack-sm" data-no-export="">
        <div className="row wrap">
          <ExportImageButton source={() => htmlSource(ref.current, exportName)} />
          {chosen.length ? (
            <>
              <span className="small"><strong>{items.length} {itemNoun}</strong> selected from {chosen.length} bar{chosen.length === 1 ? '' : 's'}{chosen.length > 1 ? ' (each counted once)' : ''}:</span>
              <CopyText label="Copy IDs" text={items.join('\n')} />
              {describe && <CopyText label="Copy names" text={items.map(describe).join('\n')} />}
              {describe && <CopyText label="Copy table" text={['id\tname', ...items.map((i) => `${i}\t${describe(i)}`)].join('\n')} />}
            </>
          ) : <span className="muted small">Click one or more bars, or a set operation above, to select their {itemNoun}.</span>}
        </div>
        {chosen.length > 0 && (
          <div className="upset-parts">
            {chosen.map((c) => (
              <details key={c.key} className="upset-part">
                <summary><strong>{comboName(c.key)}</strong> <span className="muted small">{c.items.length} {itemNoun}</span>
                  <button type="button" className="icon-btn" title="Remove this bar from the selection" aria-label={`Remove ${comboName(c.key)} from the selection`} onClick={(e) => { e.preventDefault(); toggle(c.key) }}>×</button></summary>
                <p className="small">{c.items.map((i) => (describe ? `${describe(i)} (${i})` : i)).join('; ')}</p>
              </details>
            ))}
          </div>
        )}
        <div className="row wrap">
          <span className="muted small">All combinations:</span>
          <CopyText label="Copy" text={['combination\tcount\tids', ...combos.map((c) => `${comboName(c.key)}\t${c.items.length}\t${c.items.join(',')}`)].join('\n')} />
          <button type="button" className="btn btn-ghost btn-sm" title="Excel workbook: a summary of the combinations and one sheet per combination" onClick={() => saveXlsx([
            { name: 'Combinations', rows: combos.map((c) => ({ Combination: comboName(c.key), Count: c.items.length, Items: c.items.map((i) => (describe ? describe(i) : i)).join(', ') })) },
            ...combos.slice(0, 60).map((c) => ({ name: comboName(c.key), rows: c.items.map((i) => ({ ID: i, Name: describe ? describe(i) : '' })) })),
          ], `flame_upset_${mode}.xlsx`)}>Excel</button>
          <button type="button" className="btn btn-ghost btn-sm" onClick={() => saveBlob(new Blob([['combination\tcount\tid\tname', ...combos.flatMap((c) => c.items.map((i) => `${comboName(c.key)}\t${c.items.length}\t${i}\t${describe ? describe(i) : ''}`))].join('\n')], { type: 'text/tab-separated-values' }), `flame_upset_${mode}.tsv`)}>TSV</button>
        </div>
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

export function CircosBundle({ sectors, items, links, onSelect, selected, size = 760, svgRef, trackLabel, defaultLabels = true, linkLabel = 'links', exportName = 'flame_circos' }: {
  sectors: CircosSector[]; items: CircosItem[]; links: CircosLink[]
  onSelect?: (ids: string[], additive: boolean) => void; selected?: Set<string>
  size?: number; svgRef?: { current: SVGSVGElement | null }; trackLabel?: string; defaultLabels?: boolean; linkLabel?: string; exportName?: string
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
        <ExportImageButton source={() => svgSource(inner.current, exportName, {
          selection: !!onSelect,
          legend: layout.arcs.map(({ s }) => ({ label: `${s.label} (${items.filter((i) => i.sector === s.id).length})`, color: s.color })),
        })} />
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
