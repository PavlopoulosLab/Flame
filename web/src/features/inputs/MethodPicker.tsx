import type { AnalyteList } from '../../api/types'
import { ConceptIcon } from '../../components/Icons'

export type InputKind = 'upload' | 'snps' | 'textmining' | 'volcano' | 'reduction' | 'upset' | 'expression' | 'mirna' | 'disease' | 'pubmed' | 'geo' | 'clinvar'

export const METHODS: { kind: InputKind; title: string; text: string; needsLists?: number }[] = [
  { kind: 'upload', title: 'Paste or upload IDs', text: 'Gene or protein identifiers, one per line or separated by commas.' },
  { kind: 'volcano', title: 'From a volcano plot', text: 'Upload differential expression results and select the genes you care about.' },
  { kind: 'reduction', title: 'From a 2D plot', text: 'Upload UMAP, t-SNE or PCA coordinates and lasso a cluster.' },
  { kind: 'expression', title: 'From expression data', text: 'Upload a genes × samples matrix: pick co-expressed clusters, lasso a gene map, or compare two groups of samples.' },
  { kind: 'mirna', title: 'From microRNAs', text: 'Paste microRNAs (e.g. hsa-miR-21-5p) and get the genes they are predicted to target (miRDB).' },
  { kind: 'snps', title: 'From SNPs', text: 'Map dbSNP rsIDs (human) to the genes they affect.' },
  { kind: 'disease', title: 'From a disease', text: 'Genes of a disease: Open Targets scores, DisGeNET, OMIM, Orphanet, ClinVar, GWAS Catalog.' },
  { kind: 'pubmed', title: 'From PubMed articles', text: 'Genes linked to articles (PMIDs or a PubMed search), ranked by number of articles.' },
  { kind: 'geo', title: 'From a GEO dataset', text: 'Up- or down-regulated genes of a GEO series (RummaGEO signatures).' },
  { kind: 'clinvar', title: 'From ClinVar', text: 'A gene, a condition, rsIDs or ClinVar accessions → variants, their clinical significance and genes.' },
  { kind: 'textmining', title: 'From text', text: 'Find gene and protein names in an abstract or any free text.' },
  { kind: 'upset', title: 'Combine lists', text: 'Intersections, unique parts and unions of your lists (UpSet plot).', needsLists: 2 },
]

/** The ways to add a list, as a grid of cards (shown on the workspace's Input tab). */
export function MethodGrid({ lists, onPick, disabledAll = false }: { lists: AnalyteList[]; onPick: (k: InputKind) => void; disabledAll?: boolean }) {
  return (
    <div className="method-grid">
      {METHODS.map((m) => {
        const needsMore = !!m.needsLists && lists.length < m.needsLists
        return (
          <button key={m.kind} className="method-card" disabled={needsMore || disabledAll} onClick={() => onPick(m.kind)}>
            <ConceptIcon name={m.kind} size={34} />
            <h3>{m.title}</h3>
            <p>{needsMore ? 'Add at least two lists first.' : m.text}</p>
          </button>
        )
      })}
    </div>
  )
}
