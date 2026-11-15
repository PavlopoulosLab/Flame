/**
 * Links out to genome browsers and gene / variant databases. Only https URLs
 * built from validated identifiers are produced (symbols, rsIDs, positions).
 */
export interface OutLink { label: string; url: string; group?: string }

const ENSEMBL_SPECIES: Record<number, string> = {
  9606: 'Homo_sapiens', 10090: 'Mus_musculus', 10116: 'Rattus_norvegicus', 7955: 'Danio_rerio', 7227: 'Drosophila_melanogaster',
  6239: 'Caenorhabditis_elegans', 4932: 'Saccharomyces_cerevisiae', 559292: 'Saccharomyces_cerevisiae', 9031: 'Gallus_gallus',
  9913: 'Bos_taurus', 9823: 'Sus_scrofa', 9615: 'Canis_lupus_familiaris', 9544: 'Macaca_mulatta', 9598: 'Pan_troglodytes',
  9796: 'Equus_caballus', 9685: 'Felis_catus', 8364: 'Xenopus_tropicalis', 9940: 'Ovis_aries',
}
const UCSC_DB: Record<number, string> = {
  9606: 'hg38', 10090: 'mm39', 10116: 'rn7', 7955: 'danRer11', 7227: 'dm6', 6239: 'ce11', 4932: 'sacCer3', 559292: 'sacCer3',
  9031: 'galGal6', 9913: 'bosTau9', 9823: 'susScr11', 9615: 'canFam6', 9544: 'rheMac10', 9598: 'panTro6', 8364: 'xenTro10',
}
const SPECIES_TAXID: Record<string, number> = Object.fromEntries(Object.entries(ENSEMBL_SPECIES).map(([t, s]) => [s.toLowerCase(), Number(t)]))

export const taxidOfSpecies = (species?: string) => (species ? SPECIES_TAXID[species.toLowerCase()] : undefined)

const SYMBOL = /^[A-Za-z0-9][A-Za-z0-9._:-]{0,40}$/
const RSID = /^rs\d{1,12}$/i
const CHR = /^(chr)?([0-9]{1,2}|X|Y|MT|M|[IVX]{1,4}|[23][LR]|4)$/i
const enc = encodeURIComponent

export function geneLinks(symbol: string, taxid = 9606): OutLink[] {
  if (!SYMBOL.test(symbol)) return []
  const s = enc(symbol)
  const sp = ENSEMBL_SPECIES[taxid]
  const ucsc = UCSC_DB[taxid]
  const out: OutLink[] = []
  if (sp) out.push({ label: 'Ensembl genome browser', url: `https://www.ensembl.org/${sp}/Gene/Summary?g=${s}`, group: 'Genome browsers' })
  if (ucsc) out.push({ label: 'UCSC Genome Browser', url: `https://genome.ucsc.edu/cgi-bin/hgTracks?db=${ucsc}&singleSearch=knownCanonical&position=${s}`, group: 'Genome browsers' })
  out.push({ label: 'NCBI Gene', url: `https://www.ncbi.nlm.nih.gov/gene/?term=${s}%5Bsym%5D+AND+${taxid}%5Btaxid%5D`, group: 'Gene' })
  out.push({ label: 'UniProt', url: `https://www.uniprot.org/uniprotkb?query=gene_exact:${s}+AND+organism_id:${taxid}`, group: 'Gene' })
  if (taxid === 9606) {
    out.push(
      { label: 'GeneCards', url: `https://www.genecards.org/cgi-bin/carddisp.pl?gene=${s}`, group: 'Gene' },
      { label: 'gnomAD (constraint, variants)', url: `https://gnomad.broadinstitute.org/gene/${s}?dataset=gnomad_r4`, group: 'Variation' },
      { label: 'ClinVar', url: `https://www.ncbi.nlm.nih.gov/clinvar/?term=${s}%5Bgene%5D`, group: 'Variation' },
      { label: 'GTEx (expression)', url: `https://gtexportal.org/home/gene/${s}`, group: 'Expression' },
      { label: 'Human Protein Atlas', url: `https://www.proteinatlas.org/search/${s}`, group: 'Expression' },
    )
  }
  return out
}

export function regionLinks(chr: string, pos: number, taxid = 9606, flank = 50): OutLink[] {
  const m = CHR.exec(chr)
  if (!m || !Number.isFinite(pos) || pos < 1) return []
  const c = m[2]
  const a = Math.max(1, Math.round(pos - flank)), b = Math.round(pos + flank)
  const out: OutLink[] = []
  const sp = ENSEMBL_SPECIES[taxid]
  const ucsc = UCSC_DB[taxid]
  if (sp) out.push({ label: 'Ensembl genome browser', url: `https://www.ensembl.org/${sp}/Location/View?r=${enc(c)}:${a}-${b}`, group: 'Genome browsers' })
  if (ucsc) out.push({ label: 'UCSC Genome Browser', url: `https://genome.ucsc.edu/cgi-bin/hgTracks?db=${ucsc}&position=chr${enc(c === 'MT' ? 'M' : c)}:${a}-${b}`, group: 'Genome browsers' })
  if (taxid === 9606) out.push({ label: 'NCBI Genome Data Viewer', url: `https://www.ncbi.nlm.nih.gov/genome/gdv/browser/genome/?id=GCF_000001405.40&chr=${enc(c)}&from=${a}&to=${b}`, group: 'Genome browsers' })
  return out
}

export function variantLinks(v: { id: string; chr?: string | null; pos?: number | null }, taxid = 9606): OutLink[] {
  const out = v.chr && v.pos ? regionLinks(v.chr, v.pos, taxid) : []
  if (RSID.test(v.id)) {
    const r = enc(v.id.toLowerCase())
    const sp = ENSEMBL_SPECIES[taxid]
    if (sp) out.unshift({ label: 'Ensembl variant page', url: `https://www.ensembl.org/${sp}/Variation/Explore?v=${r}`, group: 'Genome browsers' })
    out.push({ label: 'dbSNP', url: `https://www.ncbi.nlm.nih.gov/snp/${r}`, group: 'Variant' })
    if (taxid === 9606) {
      out.push(
        { label: 'gnomAD', url: `https://gnomad.broadinstitute.org/variant/${r}?dataset=gnomad_r4`, group: 'Variant' },
        { label: 'ClinVar', url: `https://www.ncbi.nlm.nih.gov/clinvar/?term=${r}`, group: 'Variant' },
        { label: 'GWAS Catalog', url: `https://www.ebi.ac.uk/gwas/variants/${r}`, group: 'Variant' },
        { label: 'LitVar (literature)', url: `https://www.ncbi.nlm.nih.gov/research/litvar2/docsum?variant=litvar%40${r}%23%23&query=${r}`, group: 'Variant' },
      )
    }
  }
  return out
}
