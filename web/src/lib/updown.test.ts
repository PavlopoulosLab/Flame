import { describe, expect, it } from 'vitest'
import { splitByScore } from './updown'

describe('splitByScore', () => {
  const ids = ['A', 'B', 'C', 'D', 'E', 'F']
  const scores = [2.5, -1.2, 0.3, -3, 0, 1.1]

  it('splits by sign, strongest first', () => {
    expect(splitByScore(ids, scores)).toEqual({ up: ['A', 'F', 'C'], down: ['D', 'B'], upTotal: 3, downTotal: 2 })
  })

  it('applies the |score| cut-off strictly', () => {
    const s = splitByScore(ids, scores, 1.2)
    expect(s.up).toEqual(['A'])
    expect(s.down).toEqual(['D'])
  })

  it('caps each direction but reports the totals', () => {
    const s = splitByScore(ids, scores, 0, 1)
    expect(s.up).toEqual(['A'])
    expect(s.down).toEqual(['D'])
    expect([s.upTotal, s.downTotal]).toEqual([3, 2])
  })

  it('skips missing scores and keeps the first of duplicate IDs', () => {
    const s = splitByScore(['A', 'a', 'B', ''], [1, -5, Number.NaN, 2])
    expect(s).toEqual({ up: ['A'], down: [], upTotal: 1, downTotal: 0 })
  })
})
