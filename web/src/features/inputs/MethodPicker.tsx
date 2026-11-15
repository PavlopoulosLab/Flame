import type { AnalyteList } from '../../api/types'
import { Modal } from '../../components/ui'

export type InputKind = 'upload' | 'snps' | 'textmining' | 'volcano' | 'reduction' | 'upset'

export const METHODS: { kind: InputKind; title: string; text: string; needsLists?: number }[] = [
  { kind: 'upload', title: 'Paste or upload IDs', text: 'Gene or protein identifiers, one per line or separated by commas.' },
  { kind: 'volcano', title: 'From a volcano plot', text: 'Upload differential expression results and select the genes you care about.' },
  { kind: 'reduction', title: 'From a 2D plot', text: 'Upload UMAP, t-SNE or PCA coordinates and lasso a cluster.' },
  { kind: 'snps', title: 'From SNPs', text: 'Map dbSNP rsIDs (human) to the genes they affect.' },
  { kind: 'textmining', title: 'From text', text: 'Find gene and protein names in an abstract or any free text.' },
  { kind: 'upset', title: 'Combine lists', text: 'Intersections, unique parts and unions of your lists (UpSet plot).', needsLists: 2 },
]

export function MethodPicker({ lists, onClose, onPick }: { lists: AnalyteList[]; onClose: () => void; onPick: (k: InputKind) => void }) {
  return (
    <Modal title="Add a list" subtitle="How would you like to provide your genes?" onClose={onClose} size="lg">
      <div className="method-grid">
        {METHODS.map((m) => {
          const disabled = !!m.needsLists && lists.length < m.needsLists
          return (
            <button key={m.kind} className="method-card" disabled={disabled} onClick={() => onPick(m.kind)}>
              <h3>{m.title}</h3>
              <p>{disabled ? 'Add at least two lists first.' : m.text}</p>
            </button>
          )
        })}
      </div>
    </Modal>
  )
}
