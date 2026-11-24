/** Axis breaks for bar charts where one bar dwarfs the others. */

export const nice = (x: number) => { if (!(x > 0)) return x; const e = 10 ** Math.floor(Math.log10(x)); const m = x / e; return (m <= 1 ? 1 : m <= 1.5 ? 1.5 : m <= 2 ? 2 : m <= 2.5 ? 2.5 : m <= 3 ? 3 : m <= 4 ? 4 : m <= 5 ? 5 : m <= 6 ? 6 : m <= 8 ? 8 : 10) * e }

/**
 * Where to break the y axis so that one very tall bar does not flatten the others: the biggest jump
 * between bar heights, if the tallest bars are at least 2.5 times the rest. Returns [from, to] or null.
 */
export function autoBreak(bars: { top: number; bottom: number }[]): [number, number] | null {
  const tops = [...bars].sort((a, b) => b.top - a.top)
  if (tops.length < 2 || !(tops[tops.length - 1].top >= 0)) return null
  let best = -1, ratio = 0
  for (let i = 0; i < tops.length - 1; i++) {
    const r = tops[i + 1].top > 0 ? tops[i].top / tops[i + 1].top : Infinity
    if (r > ratio) { ratio = r; best = i }
  }
  if (ratio < 2.5 || best < 0) return null
  const from = nice(tops[best + 1].top * 1.15)
  const tallLow = Math.min(...tops.slice(0, best + 1).map((b) => b.bottom))
  const to = Math.floor(tallLow * 0.85 * 10) / 10
  return to > from * 1.05 ? [from, to] : null
}

