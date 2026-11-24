import { describe, expect, it } from 'vitest'
import { parseMaf } from './maf'

const MAF = [
  '#version 2.4',
  'Hugo_Symbol\tEntrez_Gene_Id\tVariant_Classification\tTumor_Sample_Barcode',
  'TP53\t7157\tMissense_Mutation\tS1',
  'TP53\t7157\tNonsense_Mutation\tS1',
  'TP53\t7157\tMissense_Mutation\tS2',
  'PIK3CA\t5290\tMissense_Mutation\tS3',
  'TTN\t7273\tSilent\tS2',
  'Unknown\t0\tIGR\tS4',
].join('\n')

describe('parseMaf', () => {
  it('counts samples per gene, leaving out non-protein-changing mutations', () => {
    const r = parseMaf(MAF)
    expect(r.samples).toBe(4)
    expect(r.genes.map((g) => g.gene)).toEqual(['TP53', 'PIK3CA'])
    expect(r.genes[0]).toEqual({ gene: 'TP53', value: 50, note: '2 of 4 samples' })
    expect(r.mutations).toBe(4)
  })
  it('keeps silent mutations when asked', () => {
    expect(parseMaf(MAF, true).genes.map((g) => g.gene)).toContain('TTN')
  })
  it('rejects files without the MAF columns', () => {
    expect(() => parseMaf('gene\tsample\nTP53\tS1')).toThrow(/Not a MAF file/)
  })
})
