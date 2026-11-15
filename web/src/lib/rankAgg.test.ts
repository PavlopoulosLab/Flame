import { describe, expect, it } from 'vitest'
import { aggregateRanks, rraScore } from './rankAgg'

describe('rank aggregation', () => {
  it('RRA: a term ranked first everywhere beats one ranked last', () => {
    expect(rraScore([0.01, 0.01, 0.01])).toBeLessThan(rraScore([0.9, 0.9, 0.9]))
    expect(rraScore([0.5])).toBeCloseTo(0.5, 6) // one run: P(U <= 0.5)
    expect(rraScore([1, 1])).toBe(1)
  })
  it('ranks within runs, missing terms count as last, consensus orders terms', () => {
    const p: Record<string, Record<string, number | null>> = {
      a: { r1: 1e-9, r2: 1e-6, r3: 1e-4 },
      b: { r1: 1e-3, r2: null, r3: 1e-8 },
      c: { r1: 1e-2, r2: 1e-2, r3: null },
    }
    const agg = aggregateRanks(['a', 'b', 'c'], ['r1', 'r2', 'r3'], (t, r) => p[t][r])
    expect(agg.get('a')!.ranks).toEqual({ r1: 1, r2: 1, r3: 2 })
    expect(agg.get('b')!.ranks.r2).toBeNull()
    expect(agg.get('a')!.consensus).toBe(1)
    expect(agg.get('c')!.consensus).toBe(3)
    expect(agg.get('a')!.found).toBe(3)
  })
})
