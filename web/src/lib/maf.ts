/** Mutated genes of a MAF (Mutation Annotation Format) file, counted per sample. Runs in the browser. */

/** Variant classes that do not change the protein (left out unless asked). */
const NON_PROTEIN = new Set(['Silent', 'Intron', "3'UTR", "5'UTR", "3'Flank", "5'Flank", 'IGR', 'RNA', 'lincRNA', 'Targeted_Region'])

export interface MafGene { gene: string; value: number; note: string }

export function parseMaf(text: string, includeSilent = false): { genes: MafGene[]; samples: number; mutations: number } {
  const lines = text.split(/\r?\n/).filter((l) => l && !l.startsWith('#'))
  if (!lines.length) throw new Error('The file is empty.')
  const head = lines[0].split('\t')
  const col = (n: string) => head.findIndex((h) => h.trim().toLowerCase() === n.toLowerCase())
  const iGene = col('Hugo_Symbol'), iSample = col('Tumor_Sample_Barcode'), iClass = col('Variant_Classification')
  if (iGene < 0 || iSample < 0) throw new Error('Not a MAF file: it needs the columns Hugo_Symbol and Tumor_Sample_Barcode (and Variant_Classification).')
  const bySample = new Map<string, Set<string>>()
  const allSamples = new Set<string>()
  let mutations = 0
  for (let i = 1; i < lines.length; i++) {
    const f = lines[i].split('\t')
    const gene = f[iGene]?.trim(), sample = f[iSample]?.trim()
    if (!sample) continue
    allSamples.add(sample)
    if (!gene || gene === 'Unknown') continue
    if (!includeSilent && iClass >= 0 && NON_PROTEIN.has(f[iClass]?.trim())) continue
    mutations++
    if (!bySample.has(gene)) bySample.set(gene, new Set())
    bySample.get(gene)!.add(sample)
  }
  const n = allSamples.size
  const genes = [...bySample.entries()].map(([gene, s]) => ({ gene, value: Math.round((10000 * s.size) / Math.max(n, 1)) / 100, note: `${s.size} of ${n} samples` }))
    .sort((a, b) => b.value - a.value || a.gene.localeCompare(b.gene))
  return { genes, samples: n, mutations }
}

