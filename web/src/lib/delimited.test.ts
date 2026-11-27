import { describe, expect, it } from 'vitest'
import { detectDelim, gridToText, parseGrid, pasteInto, splitRow } from './delimited'

const QPCR = 'Gene\tControl 1\tControl 2\tDex 1\tDex 2\nGAPDH\t18.1\t18.3\t18.2\t18.0\nFKBP5\t27.1\t27.4\t23.6\t23.4\n'

describe('detectDelim', () => {
  it('finds tabs, commas and semicolons', () => {
    expect(detectDelim(QPCR)).toBe('\t')
    expect(detectDelim('gene,log2FC,p\nTP53,1.2,0.01\nEGFR,-0.4,0.2')).toBe(',')
    expect(detectDelim('gene;value\nTP53;1,2\nEGFR;0,4')).toBe(';')
  })
  it('is null for a plain list or a single row', () => {
    expect(detectDelim('TP53\nBRCA1\nEGFR')).toBeNull()
    expect(detectDelim('a\tb\tc')).toBeNull()
    expect(detectDelim('')).toBeNull()
  })
  it('accepts ragged tab-separated tables (one column per group)', () => {
    expect(detectDelim('Vehicle\tCisplatin\n100\t49.8\n96.8')).toBe('\t')
  })
})

describe('parseGrid and gridToText', () => {
  it('round-trips a tab-separated table', () => {
    const g = parseGrid(QPCR)!
    expect(g.rows).toHaveLength(3)
    expect(g.rows[2]).toEqual(['FKBP5', '27.1', '27.4', '23.6', '23.4'])
    expect(gridToText(g)).toBe(QPCR)
  })
  it('pads short rows and drops the padding when written back', () => {
    const g = parseGrid('Vehicle\tCisplatin\n100\t49.8\n96.8\n')!
    expect(g.rows[2]).toEqual(['96.8', ''])
    expect(gridToText(g)).toBe('Vehicle\tCisplatin\n100\t49.8\n96.8\n')
  })
  it('keeps quoted CSV fields together and quotes them again', () => {
    expect(splitRow('"Smith, J",2,"say ""hi"""', ',')).toEqual(['Smith, J', '2', 'say "hi"'])
    const g = parseGrid('name,n\n"Smith, J",2\nLee,3')!
    expect(g.rows[1]).toEqual(['Smith, J', '2'])
    expect(gridToText(g)).toBe('name,n\n"Smith, J",2\nLee,3\n')
  })
  it('leaves out empty rows added in the table', () => {
    const g = parseGrid('a\tb\n1\t2')!
    expect(gridToText({ ...g, rows: [...g.rows, ['', '']] })).toBe('a\tb\n1\t2\n')
  })
})

describe('long tables', () => {
  it('reads only the first rows as cells and writes the rest back unchanged', () => {
    const text = 'gene\ta\tb\n' + Array.from({ length: 1000 }, (_, i) => `G${i}\t${i}\t${i * 2}`).join('\n') + '\n'
    const g = parseGrid(text, undefined, 300)!
    expect(g.rows).toHaveLength(300)
    expect(gridToText(g)).toBe(text)
    const edited = { ...g, rows: g.rows.map((r, i) => (i === 1 ? ['TP53', r[1], r[2]] : r)) }
    const out = gridToText(edited)
    expect(out.split('\n')[1]).toBe('TP53\t0\t0')
    expect(out.split('\n')).toHaveLength(1002)
    expect(out.endsWith('G999\t999\t1998\n')).toBe(true)
  })
})

describe('pasteInto', () => {
  it('spreads cells copied from Excel from the cell pasted into, growing the table', () => {
    const g = parseGrid('a\tb\n1\t2')!
    const out = pasteInto(g, 1, 1, '5\t6\n7\t8\n')
    expect(out.rows).toEqual([['a', 'b', ''], ['1', '5', '6'], ['', '7', '8']])
  })
})
