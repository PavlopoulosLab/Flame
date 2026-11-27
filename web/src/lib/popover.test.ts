import { describe, expect, it } from 'vitest'
import { placePopover } from './popover'

const view = { left: 0, right: 1000, top: 0, bottom: 800 }
const size = { width: 460, height: 300 }

describe('pop-up panels stay inside the window', () => {
  it('aligns with the button and opens below it when there is room', () => {
    expect(placePopover({ left: 600, right: 700, top: 100, bottom: 130 }, size, view)).toEqual({ left: 240, top: 134 })
    expect(placePopover({ left: 300, right: 400, top: 100, bottom: 130 }, size, view, 'left')).toEqual({ left: 300, top: 134 })
  })
  it('slides right when the button is near the left edge (the clipped microRNA chart)', () => {
    expect(placePopover({ left: 150, right: 250, top: 100, bottom: 130 }, size, view).left).toBe(8)
  })
  it('slides left near the right edge', () => {
    expect(placePopover({ left: 900, right: 990, top: 100, bottom: 130 }, size, view, 'left').left).toBe(1000 - 460 - 8)
  })
  it('opens above the button when there is no room below', () => {
    expect(placePopover({ left: 600, right: 700, top: 700, bottom: 730 }, size, view).top).toBe(700 - 4 - 300)
  })
  it('stays inside a dialog when given its sides', () => {
    expect(placePopover({ left: 150, right: 250, top: 100, bottom: 130 }, size, { left: 80, right: 1200, top: 0, bottom: 800 }).left).toBe(88)
  })
  it('keeps a panel wider or taller than the window at the top left margin', () => {
    expect(placePopover({ left: 0, right: 50, top: 10, bottom: 40 }, { width: 2000, height: 2000 }, view)).toEqual({ left: 8, top: 8 })
  })
})
