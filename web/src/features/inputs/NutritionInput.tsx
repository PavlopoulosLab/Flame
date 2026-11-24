import { useEffect, useMemo, useState } from 'react'
import { api } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { BusyBar, ErrorNote, Field, Modal, Note, Segmented } from '../../components/ui'
import { GenePicker, type FGene, type GeneResult } from './FetchInput'

type Source = 'ctd' | 'chembl' | 'uniprot' | 'gwas'
interface NGene extends FGene { direction?: 'up' | 'down' | 'both' | 'other' }
interface NResult extends GeneResult { genes: NGene[]; matched?: string | null; chebi?: string }
interface Trait { id: string; name: string; description: string }

const SOURCES: { id: Source; label: string; what: string }[] = [
  { id: 'ctd', label: 'CTD', what: 'curated interactions: genes whose expression, activity or binding the compound or food changes' },
  { id: 'chembl', label: 'ChEMBL', what: 'human protein targets with measured activity of the compound' },
  { id: 'uniprot', label: 'UniProt', what: 'enzymes, transporters and binding proteins annotated with the compound (via ChEBI)' },
  { id: 'gwas', label: 'GWAS traits', what: 'diet and nutrient traits (intake, blood levels) and their GWAS Catalog genes' },
]
const EXAMPLES = ['curcumin', 'vitamin D', 'folic acid', 'quercetin', 'green tea', 'olive oil', 'omega-3 fatty acids', 'sulforaphane', 'genistein', 'coffee']

/**
 * Genes of a nutrient, food compound or food, from live APIs only: CTD (curated chemical–gene
 * interactions, also whole foods), ChEMBL (bioactivity), UniProt (enzymes and transporters via
 * ChEBI) and diet/nutrient traits of the GWAS Catalog (via Open Targets).
 */
export function NutritionInput({ title, example, onClose, onCreated }: { title: string; example?: boolean; onClose: () => void; onCreated: (l: AnalyteList) => void }) {
  const [term, setTerm] = useState(example ? 'curcumin' : '')
  const [sources, setSources] = useState<Set<Source>>(new Set(['ctd', 'chembl', 'uniprot', 'gwas']))
  const [organisms, setOrganisms] = useState<'human' | 'all'>('human')
  const [minP, setMinP] = useState(5)
  const [results, setResults] = useState<Partial<Record<Source, NResult | { error: string }>>>({})
  const [traits, setTraits] = useState<Trait[] | null>(null)
  const [trait, setTrait] = useState<{ t: Trait; r: NResult } | null>(null)
  const [tab, setTab] = useState<Source | 'all'>('ctd')
  const [busy, setBusy] = useState<string | null>(null)
  const [err, setErr] = useState<unknown>(null)
  const [dir, setDir] = useState<'any' | 'up' | 'down'>('any')
  const [searched, setSearched] = useState('')

  const search = async (q = term) => {
    const t = q.trim()
    if (t.length < 2) return
    setTerm(t); setSearched(t); setErr(null); setResults({}); setTraits(null); setTrait(null); setDir('any')
    setBusy(`Searching ${[...sources].map((s) => SOURCES.find((x) => x.id === s)!.label).join(', ')} for “${t}”`)
    const jobs = [...sources].map(async (s) => {
      try {
        if (s === 'gwas') {
          const r = await api<{ traits: Trait[] }>('/inputs/nutrition/traits', { body: { term: t } })
          setTraits(r.traits)
          return
        }
        const r = await api<NResult>('/inputs/nutrition/genes', { body: { term: t, source: s, organisms, minPchembl: minP } })
        setResults((cur) => ({ ...cur, [s]: r }))
      } catch (e) {
        const msg = e instanceof Error ? e.message : String(e)
        if (s === 'gwas') setTraits([]); else setResults((cur) => ({ ...cur, [s]: { error: msg } }))
      }
    })
    await Promise.all(jobs)
    setBusy(null)
    setTab((cur) => cur)
  }
  useEffect(() => { if (example) search('curcumin') // the example runs by itself
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])
  const openTrait = async (t: Trait) => {
    setBusy(`Fetching GWAS Catalog associations: ${t.name}`); setErr(null)
    try {
      const r = await api<NResult & { total: number }>('/inputs/disease/genes', { body: { source: 'gwas', id: t.id, size: 500 } })
      setTrait({ t, r: { ...r, organismTaxId: 9606, term: t.name, source: 'GWAS Catalog', valueLabel: 'variants (p ≤ 5e-8)', ranked: true } })
    } catch (e) { setErr(e) } finally { setBusy(null) }
  }

  const ok = (s: Source) => { const r = results[s]; return r && !('error' in r) ? r : null }
  // Genes found by several sources first (the more independent evidence, the higher).
  const combined: NResult | null = useMemo(() => {
    const parts = (['ctd', 'chembl', 'uniprot'] as Source[]).map((s) => [s, ok(s)] as const).filter(([, r]) => r && r.genes.length)
    if (trait) parts.push(['gwas', trait.r])
    if (parts.length < 2) return null
    const m = new Map<string, { gene: string; name?: string; from: string[] }>()
    for (const [s, r] of parts) for (const g of r!.genes) {
      const k = g.gene.toUpperCase()
      const e = m.get(k) ?? { gene: g.gene, name: g.name, from: [] }
      if (!e.name && g.name) e.name = g.name
      e.from.push(SOURCES.find((x) => x.id === s)!.label); m.set(k, e)
    }
    const genes = [...m.values()].sort((a, b) => b.from.length - a.from.length || a.gene.localeCompare(b.gene))
      .map((g) => ({ gene: g.gene, name: g.name, sources: g.from.length, from: g.from }))
    return { genes, organismTaxId: 9606, ranked: true, valueLabel: 'sources', term: `${searched}: all sources`, source: 'CTD, ChEMBL, UniProt and GWAS Catalog',
      citation: 'Genes ranked by how many sources link them to the query.' } as NResult
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [results, trait, searched])

  const shownResult = (s: Source | 'all'): NResult | null => {
    if (s === 'all') return combined
    if (s === 'gwas') return trait?.r ?? null
    const r = ok(s)
    if (!r) return null
    if (s === 'ctd' && dir !== 'any') {
      const genes = r.genes.filter((g) => g.direction === dir || g.direction === 'both')
      return { ...r, genes, term: `${r.term} (${dir === 'up' ? 'raises' : 'lowers'} expression)` }
    }
    return { ...r, term: `${r.term} (${s === 'ctd' ? 'CTD' : s === 'chembl' ? 'ChEMBL targets' : 'UniProt'})` }
  }
  const count = (s: Source) => (s === 'gwas' ? (trait ? trait.r.genes.length : traits ? `${traits.length} traits` : null) : ok(s)?.genes.length ?? (results[s] ? '—' : null))
  const anything = Object.keys(results).length > 0 || traits !== null
  const cur = shownResult(tab)
  const errOf = (s: Source) => { const r = results[s]; return r && 'error' in r ? r.error : null }

  return (
    <Modal title={title} size="xl" onClose={onClose}
      subtitle="Genes linked to a nutrient, food compound or food, from live databases: CTD, ChEMBL, UniProt and the GWAS Catalog.">
      <div className="stack">
        <div className="row">
          <input className="input" value={term} placeholder="A nutrient, food compound or food — e.g. curcumin, vitamin D, green tea" aria-label="Nutrient, compound or food"
            onChange={(e) => setTerm(e.target.value)} onKeyDown={(e) => { if (e.key === 'Enter') search() }} />
          <button className="btn btn-primary" disabled={term.trim().length < 2 || !!busy || !sources.size} onClick={() => search()}>Search</button>
        </div>
        <div className="row wrap small"><span className="muted">Try:</span>
          {EXAMPLES.map((x) => <button key={x} className="btn btn-ghost btn-sm" onClick={() => search(x)} disabled={!!busy}>{x}</button>)}
        </div>
        <div className="row wrap" role="group" aria-label="Sources">
          {SOURCES.map((s) => (
            <label key={s.id} className="check small" title={s.what}>
              <input type="checkbox" checked={sources.has(s.id)} onChange={(e) => setSources((cur) => { const n = new Set(cur); if (e.target.checked) n.add(s.id); else n.delete(s.id); return n })} />
              <strong>{s.label}</strong> <span className="muted">{s.what}</span>
            </label>
          ))}
        </div>
        <div className="row wrap">
          {sources.has('ctd') && <Field label="CTD genes of"><select className="input input-sm" value={organisms} onChange={(e) => setOrganisms(e.target.value as 'human' | 'all')}>
            <option value="human">human</option><option value="all">human, mouse and rat</option></select></Field>}
          {sources.has('chembl') && <Field label="ChEMBL: pChEMBL at least"><input className="input input-sm input-narrow" type="number" min={4} max={10} step={0.5} value={minP} onChange={(e) => setMinP(Number(e.target.value))} /></Field>}
        </div>
        {busy && <BusyBar label={busy} expect={25} />}
        <ErrorNote error={err} />
        {anything && !busy && (
          <>
            <nav className="view-tabs" aria-label="Sources found">
              {combined && <span className={`view-tab${tab === 'all' ? ' active' : ''}`}><button onClick={() => setTab('all')}>All sources ({combined.genes.length})</button></span>}
              {SOURCES.filter((s) => sources.has(s.id)).map((s) => (
                <span key={s.id} className={`view-tab${tab === s.id ? ' active' : ''}`}><button onClick={() => setTab(s.id)}>{s.label}{count(s.id) != null ? ` (${count(s.id)})` : ''}</button></span>
              ))}
            </nav>
            {tab !== 'all' && tab !== 'gwas' && errOf(tab) && <Note kind="info">{errOf(tab)}</Note>}
            {tab === 'ctd' && ok('ctd') && (
              <div className="row wrap small">
                {ok('ctd')!.matched && <span className="muted">Matched in CTD as <strong>{ok('ctd')!.matched}</strong>.</span>}
                <Segmented label="Effect on expression" value={dir} onChange={setDir}
                  options={[{ value: 'any', label: 'Any interaction' }, { value: 'up', label: 'Raises expression' }, { value: 'down', label: 'Lowers expression' }]} />
              </div>
            )}
            {tab === 'uniprot' && ok('uniprot') && <p className="small muted">ChEBI entry: <strong>{ok('uniprot')!.term}</strong> ({ok('uniprot')!.chebi}).</p>}
            {tab === 'gwas' && (
              <div className="stack-sm">
                {traits && !traits.length && <Note kind="info">No diet or nutrient trait matches “{searched}”. Try “… intake”, “… level” or “… consumption”.</Note>}
                {traits && traits.length > 0 && (
                  <div className="table-scroll fetch-table"><table className="table compact">
                    <thead><tr><th>Trait (Open Targets)</th><th>ID</th><th /></tr></thead>
                    <tbody>{traits.map((t) => (
                      <tr key={t.id} className={trait?.t.id === t.id ? 'selected' : ''}>
                        <td><button className="link-btn" onClick={() => openTrait(t)}><strong>{t.name}</strong></button><div className="muted small">{t.description}</div></td>
                        <td className="small">{t.id}</td>
                        <td><button className="btn btn-ghost btn-sm" onClick={() => openTrait(t)}>GWAS genes</button></td>
                      </tr>
                    ))}</tbody>
                  </table></div>
                )}
              </div>
            )}
            {cur && cur.genes.length > 0 && <GenePicker r={cur} mode="nutrition" onCreated={onCreated} />}
            {cur && cur.genes.length === 0 && <Note kind="info">No genes for this selection.</Note>}
            <p className="muted small">Sources: CTD (Davis et al. 2025), ChEMBL (Zdrazil et al. 2024), UniProt (2025), GWAS Catalog (Cerezo et al. 2025) via Open Targets. CTD's interactions come from cell, animal and human studies; links are not proof of a dietary effect at normal intake.</p>
          </>
        )}
      </div>
    </Modal>
  )
}
