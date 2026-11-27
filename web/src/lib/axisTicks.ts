/** Axis ticks for a mirrored axis (the up & down figure), labelled with absolute values (|−3| shown as 3). */
export const absTicks = (max: number) => {
  const raw = max / 4, mag = 10 ** Math.floor(Math.log10(raw)), step = [1, 2, 5, 10].map((k) => k * mag).find((k) => k >= raw) ?? raw
  const vals: number[] = []
  // Each tick is k × step (not a running sum), so the middle one is exactly 0 and not e.g. 1.388e-17.
  const n = Math.floor(max / step + 1e-9)
  for (let k = -n; k <= n; k++) vals.push(k === 0 ? 0 : Number((k * step).toPrecision(10)))
  return { tickvals: vals, ticktext: vals.map((v) => String(Number(Math.abs(v).toPrecision(4)))) }
}
