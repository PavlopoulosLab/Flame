/**
 * Rank aggregation across runs (tools, lists or strategies).
 *
 * Each run ranks the terms it found by p-value (1 = most significant; ties share
 * the best rank). The consensus uses Robust Rank Aggregation (Kolde et al. 2012,
 * Bioinformatics 28:573): with normalised ranks u = rank / N (N = distinct terms
 * considered, as in the RobustRankAggreg package; a run that did not report the
 * term: u = 1) sorted ascending, the score is
 * rho = min_k P(Beta(k, m − k + 1) ≤ u_k), Bonferroni-corrected by m. Small
 * rho = ranked higher than expected by chance in several runs. Ties on rho are
 * broken by the mean of u. R/core-compare.R (cmp_consensus) computes the same
 * score for the Top terms view: keep both in step.
 *
 * meanRank (shown in the table) is the mean raw rank, a missing term counting as
 * that run's worst rank + 1.
 */
export interface RankRow { termId: string; ranks: Record<string, number | null>; meanRank: number; meanU: number; rra: number; consensus: number; found: number }

/** P(Beta(k, m-k+1) <= x) = P(Binomial(m, x) >= k) */
function betaCdfOrder(k: number, m: number, x: number): number {
  if (x <= 0) return 0
  if (x >= 1) return 1
  let s = 0
  // sum_{j=k}^{m} C(m,j) x^j (1-x)^(m-j), in log space for stability
  const lx = Math.log(x), l1x = Math.log1p(-x)
  let lc = 0 // log C(m, 0)
  for (let j = 0; j <= m; j++) {
    if (j > 0) lc += Math.log((m - j + 1) / j)
    if (j >= k) s += Math.exp(lc + j * lx + (m - j) * l1x)
  }
  return Math.min(1, s)
}

export function rraScore(u: number[]): number {
  const v = [...u].sort((a, b) => a - b), m = v.length
  let best = 1
  for (let k = 1; k <= m; k++) best = Math.min(best, betaCdfOrder(k, m, v[k - 1]))
  return Math.min(1, best * m)
}

export function aggregateRanks(
  termIds: string[], runIds: string[], pOf: (termId: string, runId: string) => number | null,
): Map<string, RankRow> {
  const ranks: Record<string, Map<string, number>> = {}
  const n: Record<string, number> = {}
  runIds.forEach((r) => {
    const found = termIds.map((t) => [t, pOf(t, r)] as const).filter(([, p]) => p != null) as [string, number][]
    found.sort((a, b) => a[1] - b[1])
    const m = new Map<string, number>()
    // ties share the best rank
    let prev = -1, rank = 0
    found.forEach(([t, p], i) => { if (p !== prev) rank = i + 1; prev = p; m.set(t, rank) })
    ranks[r] = m; n[r] = found.length
  })
  const N = Math.max(1, termIds.filter((t) => runIds.some((r) => ranks[r].has(t))).length)
  const rows = termIds.map((t) => {
    const rr: Record<string, number | null> = {}
    const u: number[] = []
    let sum = 0, found = 0
    runIds.forEach((r) => {
      const k = ranks[r].get(t) ?? null
      rr[r] = k
      const eff = k ?? n[r] + 1
      sum += eff
      if (k != null) found++
      u.push(k != null ? Math.min(1, k / N) : 1)
    })
    const meanU = u.reduce((a, b) => a + b, 0) / Math.max(1, u.length)
    return { termId: t, ranks: rr, meanRank: sum / Math.max(1, runIds.length), meanU, rra: rraScore(u), consensus: 0, found }
  })
  rows.sort((a, b) => a.rra - b.rra || a.meanU - b.meanU)
  rows.forEach((r, i) => { r.consensus = i + 1 })
  return new Map(rows.map((r) => [r.termId, r]))
}
