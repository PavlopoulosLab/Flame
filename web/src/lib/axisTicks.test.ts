import { describe, expect, it } from 'vitest'
import { absTicks } from './axisTicks'

describe('absTicks (up & down figure axis)', () => {
  it('labels the centre exactly 0, whatever the step', () => {
    for (const max of [0.35, 0.7, 1.388, 3.3, 7.9, 12, 47, 0.042]) {
      const { tickvals, ticktext } = absTicks(max)
      expect(tickvals).toContain(0)
      expect(ticktext[tickvals.indexOf(0)]).toBe('0')
      expect(ticktext.every((t) => !t.includes('e'))).toBe(true)
      expect(tickvals).toEqual([...tickvals].reverse().map((v) => -v || 0)) // symmetric
      expect(Math.max(...tickvals)).toBeLessThanOrEqual(max + 1e-9)
    }
  })
  it('shows absolute values on the down side', () => {
    expect(absTicks(0.35).ticktext).toEqual(['0.3', '0.2', '0.1', '0', '0.1', '0.2', '0.3'])
  })
})
