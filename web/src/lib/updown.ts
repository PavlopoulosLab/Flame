/** Most IDs one list can hold (the server's maxIdsPerList). */
export const MAX_LIST_IDS = 3000

export interface ScoreSplit {
  /** Up genes (score > cut-off), strongest first, at most `maxPerDirection`. */
  up: string[]
  down: string[]
  /** How many genes passed the cut-off before the per-direction cap. */
  upTotal: number
  downTotal: number
}

/**
 * Split a ranked list (IDs with signed scores, e.g. log2 fold changes) into up- and
 * down-regulated genes: score > minAbs is up, score < −minAbs is down. Each direction is
 * ordered by |score| (strongest first) and cut to `maxPerDirection`. Duplicate IDs keep
 * their first score, as the server does for ranked lists.
 */
export function splitByScore(ids: string[], scores: number[], minAbs = 0, maxPerDirection = MAX_LIST_IDS): ScoreSplit {
  const cut = Math.max(0, Number.isFinite(minAbs) ? minAbs : 0)
  const cap = Math.max(1, Math.floor(maxPerDirection))
  const seen = new Set<string>()
  const up: [string, number][] = [], down: [string, number][] = []
  ids.forEach((id, i) => {
    const s = scores[i]
    const key = id.toLowerCase()
    if (!id || seen.has(key) || !Number.isFinite(s)) return
    seen.add(key)
    if (s > cut) up.push([id, s])
    else if (s < -cut) down.push([id, s])
  })
  up.sort((a, b) => b[1] - a[1])
  down.sort((a, b) => a[1] - b[1])
  return { up: up.slice(0, cap).map((x) => x[0]), down: down.slice(0, cap).map((x) => x[0]), upTotal: up.length, downTotal: down.length }
}
