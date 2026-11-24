import { describe, expect, it } from 'vitest'
import { autoBreak, nice } from './axisBreak'

describe('autoBreak', () => {
  it('breaks the axis under one very tall bar', () => {
    const b = autoBreak([{ top: 1.1, bottom: 0.9 }, { top: 1.5, bottom: 1.2 }, { top: 1.3, bottom: 1.0 }, { top: 14.2, bottom: 11.8 }])
    expect(b).toEqual([2, 10])
  })
  it('keeps every bar wholly below or above the break', () => {
    const bars = [{ top: 1.1, bottom: 0.9 }, { top: 12.7, bottom: 10.3 }, { top: 1.5, bottom: 1.2 }, { top: 5.3, bottom: 4.3 }]
    const [from, to] = autoBreak(bars)!
    for (const b of bars) expect(b.top <= from || b.bottom >= to).toBe(true)
  })
  it('does not break bars of similar height', () => {
    expect(autoBreak([{ top: 1, bottom: 0.8 }, { top: 1.8, bottom: 1.5 }, { top: 2.2, bottom: 1.9 }])).toBeNull()
  })
  it('rounds to readable numbers', () => {
    expect(nice(6.1)).toBe(8)
    expect(nice(0.23)).toBeCloseTo(0.25)
  })
})
