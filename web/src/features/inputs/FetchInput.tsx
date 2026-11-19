import { useEffect, useMemo, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { api, getWorkspaceId } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { Plot } from '../../components/charts'
import { BusyBar, CopyButton, ErrorNote, Field, GeneCardsLink, GeneChips, Modal, Note, OrganismPicker } from '../../components/ui'
import { formatP, plural } from '../../lib/format'
import { SaveList } from './InputModal'
import { saveXlsx } from '../../lib/xlsx'

export type FetchMode = 'disease' | 'pubmed' | 'geo' | 'clinvar'

interface FGene { gene: string; name?: string; score?: number; articles?: number; variants?: number; sources?: number; from?: string[]; bestScore?: number; pmids?: string[]; evidence?: Record<string, number> }
interface GeneResult { genes: FGene[]; organismTaxId: number; ranked?: boolean; citation?: string; source?: string; disease?: string; term?: string; link?: string; total?: number }

const EXAMPLES: Record<FetchMode, string> = {
  disease: 'asthma',
  pubmed: 'glucocorticoid receptor airway smooth muscle',
  geo: 'GSE52778',
  clinvar: 'cystic fibrosis',
}
const PLACEHOLDER: Record<FetchMode, string> = {
  disease: 'A disease or trait, e.g. asthma, Alzheimer disease, cystic fibrosis',
  pubmed: 'PMIDs (24926665 36599824) or a PubMed search (glucocorticoid AND asthma)',
  geo: 'A GEO series (GSE52778) or keywords (dexamethasone airway)',
  clinvar: 'A gene (APOE), a condition (cystic fibrosis), rsIDs (rs429358 rs7412), accessions (VCV000017864) or a ClinVar search',
}

/**
 * Fetch a gene list from public resources: genes of a disease (Open Targets,
 * DisGeNET, OMIM, Orphanet, ClinVar…), genes linked to PubMed articles,
 * differential-expression signatures of GEO series, and ClinVar variants.
 */
export function FetchInput({ mode, title, example, onClose, onCreated }: {
  mode: FetchMode; title: string; example?: boolean; onClose: () => void; onCreated: (l: AnalyteList) => void
}) {
  const [query, setQuery] = useState(example ? EXAMPLES[mode] : '')
  const [busy, setBusy] = useState<string | null>(null)
  const [err, setErr] = useState<unknown>(null)
  const [result, setResult] = useState<GeneResult | null>(null)
  const [extra, setExtra] = useState<Record<string, unknown> | null>(null)
  const [taxid, setTaxid] = useState(9606)
  const [max, setMax] = useState(200)
  const [dbs, setDbs] = useState<string[]>(DISEASE_DBS.map((d) => d.id))
  const [significance, setSignificance] = useState<'any' | 'pathogenic' | 'pathogenic_likely'>(example && mode === 'clinvar' ? 'pathogenic' : 'any')
  const call = async <T,>(label: string, path: string, body: Record<string, unknown>) => {
    setBusy(label); setErr(null)
    try { return await api<T>(path, { body }) } catch (e) { setErr(e); return null } finally { setBusy(null) }
  }
  const search = async () => {
    setResult(null); setExtra(null)
    if (mode === 'disease') { const r = await call<Record<string, unknown>>(dbs.length === DISEASE_DBS.length ? 'Searching Open Targets and disease libraries' : `Searching ${DISEASE_DBS.filter((d) => dbs.includes(d.id)).map((d) => d.short).join(', ')}`, '/inputs/disease/search', { query, libraries: dbs }); if (r) setExtra(r) }
    if (mode === 'pubmed') { const r = await call<GeneResult & Record<string, unknown>>('Fetching articles and their genes from NCBI', '/inputs/pubmed/genes', { query, organismTaxId: taxid, max }); if (r) { setResult(r); setExtra(r) } }
    if (mode === 'geo') { const r = await call<Record<string, unknown>>('Searching GEO signatures', '/inputs/geo/search', { query }); if (r) setExtra(r) }
    if (mode === 'clinvar') { const r = await call<GeneResult & Record<string, unknown>>('Fetching variants from ClinVar', '/inputs/clinvar/lookup', { query, significance, max }); if (r) { setResult(r); setExtra(r) } }
  }
  useEffect(() => { if (example && query) search() // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])
  const subtitle = {
    disease: 'Genes associated with a disease: Open Targets (scored evidence), DisGeNET, OMIM, Orphanet, ClinVar, Jensen DISEASES and GWAS Catalog.',
    pubmed: 'Genes linked to PubMed articles in NCBI Gene (gene2pubmed), ranked by how many of the articles mention them.',
    geo: 'Up- or down-regulated genes of GEO series, from uniformly re-analysed data (RummaGEO).',
    clinvar: 'Variants from NCBI ClinVar with their clinical classification, conditions and genes.',
  }[mode]
  return (
    <Modal title={title} subtitle={subtitle} size="xl" onClose={onClose}>
      <div className="stack">
        <div className="row wrap fetch-bar">
          {mode === 'pubmed'
            ? <textarea className="input" rows={2} value={query} placeholder={PLACEHOLDER[mode]} onChange={(e) => setQuery(e.target.value)} />
            : <input className="input" value={query} placeholder={PLACEHOLDER[mode]} onChange={(e) => setQuery(e.target.value)} onKeyDown={(e) => { if (e.key === 'Enter') search() }} />}
          <button className="btn btn-primary" disabled={!query.trim() || !!busy || (mode === 'disease' && dbs.length === 0)} onClick={search}>{mode === 'pubmed' ? 'Fetch' : 'Search'}</button>
          {!query && <button className="btn btn-ghost btn-sm" onClick={() => setQuery(EXAMPLES[mode])}>Example</button>}
        </div>
        {mode === 'pubmed' && (
          <div className="row wrap">
            <OrganismPicker value={taxid} onChange={setTaxid} />
            <Field label="Articles (for a search)"><select className="input" value={max} onChange={(e) => setMax(Number(e.target.value))}>{[50, 100, 200, 500].map((n) => <option key={n} value={n}>{n} most relevant</option>)}</select></Field>
          </div>
        )}
        {mode === 'disease' && (
          <fieldset className="db-pick">
            <legend className="field-label">Databases to search</legend>
            <div className="row wrap">
              {DISEASE_DBS.map((d) => (
                <label key={d.id} className="check small" title={d.what}>
                  <input type="checkbox" checked={dbs.includes(d.id)} onChange={() => setDbs((cur) => (cur.includes(d.id) ? cur.filter((x) => x !== d.id) : [...cur, d.id]))} /> {d.short}
                </label>
              ))}
              <button type="button" className="btn btn-ghost btn-sm" disabled={dbs.length === DISEASE_DBS.length} onClick={() => setDbs(DISEASE_DBS.map((d) => d.id))}>Select all</button>
              <button type="button" className="btn btn-ghost btn-sm" disabled={dbs.length === 0} onClick={() => setDbs([])}>None</button>
            </div>
            {dbs.length === 0 && <p className="muted small">Select at least one database to search.</p>}
          </fieldset>
        )}
        {mode === 'clinvar' && (
          <div className="row wrap">
            <Field label="Clinical significance"><select className="input" value={significance} onChange={(e) => setSignificance(e.target.value as typeof significance)}>
              <option value="any">Any</option><option value="pathogenic">Pathogenic</option><option value="pathogenic_likely">Pathogenic or likely pathogenic</option></select></Field>
            <Field label="Variants to fetch"><select className="input" value={max} onChange={(e) => setMax(Number(e.target.value))}>{[100, 200, 500, 1000].map((n) => <option key={n} value={n}>up to {n}</option>)}</select></Field>
          </div>
        )}
        <ErrorNote error={err} />
        {busy && <BusyBar label={busy} expect={20} hint="External databases can take up to a minute to answer." />}
        {mode === 'disease' && extra && <DiseasePicker data={extra as never} onResult={setResult} call={call} showingGenes={!!result} />}
        {mode === 'geo' && extra && <GeoPicker data={extra as never} onResult={setResult} call={call} />}
        {mode === 'pubmed' && extra && <PubmedArticles data={extra as never} />}
        {mode === 'clinvar' && extra && <ClinvarVariants data={extra as never} />}
        {result && <GenePicker r={result} mode={mode} onCreated={onCreated} />}
      </div>
    </Modal>
  )
}

/** The server sends a one-element list as a bare value; always work with an array. */
const asArray = <T,>(v: T | T[] | null | undefined): T[] => (v == null ? [] : Array.isArray(v) ? v : [v])

/** Databases behind the disease search (ids as the API expects them). */
const DISEASE_DBS: { id: string; short: string; what: string }[] = [
  { id: 'OpenTargets', short: 'Open Targets', what: 'Diseases (EFO / MONDO) with genes scored by the strength of their evidence; also gives the GWAS variants of a disease' },
  { id: 'DisGeNET', short: 'DisGeNET', what: 'Curated and text-mined gene–disease associations' },
  { id: 'OMIM', short: 'OMIM', what: 'Mendelian disease genes (expanded)' },
  { id: 'Orphanet', short: 'Orphanet', what: 'Rare-disease genes' },
  { id: 'ClinVar', short: 'ClinVar', what: 'Genes with clinically interpreted variants for a condition' },
  { id: 'Jensen', short: 'Jensen DISEASES', what: 'Curated gene–disease associations (Jensen Lab)' },
  { id: 'GWAS', short: 'GWAS Catalog', what: 'Genes reported for a trait in genome-wide association studies' },
]

type Call = <T>(label: string, path: string, body: Record<string, unknown>) => Promise<T | null>

interface DiseaseHit { id: string; name: string; description: string }
interface DiseaseSet { source: string; label: string; term: string; size: number }
interface GwasResult { variants: { rsid: string; p: number; genes: string[]; study: string; trait: string }[]; genes: FGene[]; citation: string; total: number }
/** One selectable result: the scored genes of a disease (Open Targets) or a library gene set. */
interface DiseaseSource { key: string; title: string; from: string; body: Record<string, unknown> }

/**
 * Results of a disease search as two tables. Tick one or more rows and fetch
 * their genes (several rows are merged, ranked by how many of them list each
 * gene); "Back to results" returns to the tables with the ticks kept.
 */
function DiseasePicker({ data, onResult, call, showingGenes }: {
  data: { diseases: DiseaseHit[]; sets: DiseaseSet[]; queried?: string[] | string }
  onResult: (r: GeneResult | null) => void; call: Call; showingGenes: boolean
}) {
  const [ticked, setTicked] = useState<string[]>([])
  const [used, setUsed] = useState<DiseaseSource[]>([])
  const [gwas, setGwas] = useState<{ disease: DiseaseHit; r: GwasResult } | null>(null)
  const [filter, setFilter] = useState('')
  const [source, setSource] = useState('')
  const navigate = useNavigate()
  const diseaseSource = (d: DiseaseHit): DiseaseSource => ({ key: `ot:${d.id}`, title: d.name, from: 'Open Targets', body: { source: 'opentargets', id: d.id, size: 1000 } })
  const setSource_ = (x: DiseaseSet): DiseaseSource => ({ key: `${x.source}:${x.term}`, title: x.term, from: x.label, body: { source: x.source, term: x.term } })
  const all = useMemo(() => [...data.diseases.map(diseaseSource), ...data.sets.map(setSource_)], [data])
  const chosen = all.filter((x) => ticked.includes(x.key))
  const toggle = (key: string) => setTicked((cur) => (cur.includes(key) ? cur.filter((k) => k !== key) : [...cur, key]))
  const labels = [...new Set(data.sets.map((x) => x.label))]
  const queried = data.queried == null ? null : asArray(data.queried)
  const askedDiseases = !queried || queried.includes('OpenTargets')
  const askedSets = !queried || queried.some((x) => x !== 'OpenTargets')
  const sets = data.sets.filter((x) => (!source || x.label === source) && (!filter.trim() || x.term.toLowerCase().includes(filter.trim().toLowerCase())))

  // Genes of one or several results. One result keeps its own values (e.g. association scores);
  // several are merged and ranked by the number of selected results that list each gene.
  const fetchGenes = async (items: DiseaseSource[]) => {
    if (!items.length) return
    setGwas(null); onResult(null)
    const got: { item: DiseaseSource; r: GeneResult }[] = []
    for (const [i, item] of items.entries()) {
      const r = await call<GeneResult>(`Fetching genes: ${item.title} (${item.from})${items.length > 1 ? ` — ${i + 1} of ${items.length}` : ''}`, '/inputs/disease/genes', item.body)
      if (r) got.push({ item, r })
    }
    if (!got.length) return
    setUsed(got.map((g) => g.item))
    if (got.length === 1) { onResult(got[0].r); return }
    const merged = new Map<string, FGene & { sources: number; from: string[] }>()
    got.forEach(({ item, r }) => r.genes.forEach((g) => {
      const cur = merged.get(g.gene) ?? { gene: g.gene, name: g.name, sources: 0, from: [] }
      cur.sources += 1; cur.from.push(`${item.title} (${item.from})`); cur.name = cur.name ?? g.name
      if (g.score != null) cur.bestScore = Math.max(cur.bestScore ?? 0, g.score)
      merged.set(g.gene, cur)
    }))
    const genes = [...merged.values()].sort((x, y) => y.sources - x.sources || (y.bestScore ?? 0) - (x.bestScore ?? 0) || x.gene.localeCompare(y.gene))
    onResult({ genes, organismTaxId: got[0].r.organismTaxId, ranked: true, source: `${got.length} selected results`, disease: `Disease genes (${got.length} sources)`,
      citation: [...new Set(got.map((g) => g.r.citation).filter(Boolean))].join(' ') })
  }
  const fetchGwas = async (d: DiseaseHit) => {
    setGwas(null); onResult(null)
    const r = await call<GwasResult>(`Fetching GWAS Catalog associations: ${d.name}`, '/inputs/disease/genes', { source: 'gwas', id: d.id, size: 500 })
    if (!r) return
    setUsed([{ key: `gwas:${d.id}`, title: d.name, from: 'GWAS Catalog', body: {} }])
    setGwas({ disease: d, r })
    onResult({ ...(r as unknown as GeneResult), organismTaxId: 9606, ranked: true, source: 'GWAS Catalog' })
  }
  const annotate = async () => {
    if (!gwas) return
    const r = await api<{ id: string }>('/variants', { body: { name: `GWAS variants: ${gwas.disease.name}`, text: gwas.r.variants.map((v) => v.rsid).join('\n'), organismTaxId: 9606 } })
    navigate(`/w/${getWorkspaceId()}/variants/${r.id}`)
  }

  if (showingGenes) {
    return (
      <div className="stack-sm">
        <div className="fetch-back">
          <button className="btn btn-secondary btn-sm" onClick={() => { onResult(null); setGwas(null) }}>← Back to results</button>
          <span className="small">Genes from: {used.map((u, i) => <span key={u.key}>{i ? '; ' : ''}<strong>{u.title}</strong> <span className="muted">({u.from})</span></span>)}</span>
        </div>
        {gwas && (
          <div className="stack-sm">
            <div className="row between wrap">
              <h3>{plural(gwas.r.total, 'GWAS variant')} (p ≤ 5e-8)</h3>
              <span className="row"><CopyButton label="Copy rsIDs" text={gwas.r.variants.map((v) => v.rsid).join('\n')} />
                <button className="btn btn-secondary btn-sm" onClick={annotate} title="Annotate the variants with Ensembl VEP, dbSNP, gnomAD, ClinVar and traits">Annotate variants →</button></span>
            </div>
            <div className="table-scroll fetch-table"><table className="table compact">
              <thead><tr><th>Variant</th><th>p</th><th>Mapped genes</th><th>Reported trait</th><th>Study</th></tr></thead>
              <tbody>{gwas.r.variants.slice(0, 200).map((v) => (
                <tr key={v.rsid}><td><a href={`https://www.ncbi.nlm.nih.gov/snp/${v.rsid}`} target="_blank" rel="noopener noreferrer">{v.rsid}</a></td><td>{formatP(v.p)}</td>
                  <td>{asArray(v.genes).map((g, i) => <span key={g}>{i ? ', ' : ''}<GeneCardsLink gene={g} /></span>)}</td><td className="small">{v.trait}</td>
                  <td><a href={`https://www.ebi.ac.uk/gwas/studies/${v.study}`} target="_blank" rel="noopener noreferrer">{v.study}</a></td></tr>
              ))}</tbody>
            </table></div>
          </div>
        )}
      </div>
    )
  }

  return (
    <div className="stack-sm">
      <div className="fetch-back">
        <button className="btn btn-primary btn-sm" disabled={!chosen.length} onClick={() => fetchGenes(chosen)}>
          {chosen.length > 1 ? `Get genes of ${chosen.length} selected (merged)` : chosen.length === 1 ? 'Get genes of the selected result' : 'Get genes of selected'}
        </button>
        {chosen.length > 0 && <button className="btn btn-ghost btn-sm" onClick={() => setTicked([])}>Clear selection</button>}
        <span className="muted small">{chosen.length ? `${plural(chosen.length, 'result')} ticked.` : 'Tick one or more rows, or click a name to open it alone.'} Several results are merged and ranked by how many of them list each gene.</span>
      </div>
      {askedDiseases && <h3>Diseases <span className="muted small">Open Targets · EFO / MONDO — genes scored by the strength of their evidence</span></h3>}
      {!askedDiseases ? null : data.diseases.length === 0 ? <p className="muted small">No disease matches.</p> : (
        <div className="table-scroll fetch-table"><table className="table compact">
          <thead><tr><th aria-label="Select" /><th>Disease</th><th>Description</th><th>ID</th><th>Also</th></tr></thead>
          <tbody>{data.diseases.map((d) => {
            const src = diseaseSource(d); const on = ticked.includes(src.key)
            return (
              <tr key={d.id} className={on ? 'selected' : ''}>
                <td><input type="checkbox" checked={on} aria-label={`Select ${d.name}`} onChange={() => toggle(src.key)} /></td>
                <td><button className="link-btn" title="Open the scored genes of this disease" onClick={() => fetchGenes([src])}><strong>{d.name}</strong></button></td>
                <td className="small muted fetch-desc">{d.description}</td>
                <td className="small"><a href={`https://platform.opentargets.org/disease/${d.id}`} target="_blank" rel="noopener noreferrer">{d.id} ↗</a></td>
                <td><button className="btn btn-ghost btn-sm" onClick={() => fetchGwas(d)} title="Genome-wide significant variants of this trait and their genes (GWAS Catalog)">GWAS variants</button></td>
              </tr>
            )
          })}</tbody>
        </table></div>
      )}
      {askedSets && <div className="row between wrap">
        <h3>Gene sets <span className="muted small">curated disease gene sets from the libraries below</span></h3>
        {data.sets.length > 0 && (
          <span className="row wrap">
            <select className="input input-sm" value={source} aria-label="Library" onChange={(e) => setSource(e.target.value)}>
              <option value="">All libraries</option>{labels.map((l) => <option key={l} value={l}>{l}</option>)}
            </select>
            <input className="input input-sm" type="search" value={filter} placeholder="Filter gene sets" aria-label="Filter gene sets" onChange={(e) => setFilter(e.target.value)} />
          </span>
        )}
      </div>}
      {!askedSets ? null : data.sets.length === 0 ? <p className="muted small">No gene set matches.</p> : (
        <div className="table-scroll fetch-table"><table className="table compact">
          <thead><tr><th aria-label="Select" /><th>Gene set</th><th>Library</th><th className="num">Genes</th></tr></thead>
          <tbody>{sets.slice(0, 200).map((x) => {
            const src = setSource_(x); const on = ticked.includes(src.key)
            return (
              <tr key={src.key} className={on ? 'selected' : ''}>
                <td><input type="checkbox" checked={on} aria-label={`Select ${x.term} (${x.label})`} onChange={() => toggle(src.key)} /></td>
                <td><button className="link-btn" title="Open the genes of this set" onClick={() => fetchGenes([src])}><strong>{x.term}</strong></button></td>
                <td className="small">{x.label}</td><td className="num">{x.size}</td>
              </tr>
            )
          })}</tbody>
        </table></div>
      )}
      {sets.length === 0 && data.sets.length > 0 && <p className="muted small">No gene set matches this filter.</p>}
    </div>
  )
}

function GeoPicker({ data, onResult, call }: {
  data: { total: number; sets: { id: string; term: string; gse: string; title: string; size: number; species: string; direction: string; contrast?: { a: string; b: string } | null }[] }
  onResult: (r: GeneResult | null) => void; call: Call
}) {
  const [pick, setPick] = useState<string | null>(null)
  const open = async (id: string) => {
    setPick(id); onResult(null)
    const r = await call<GeneResult>('Fetching the signature', '/inputs/geo/genes', { id })
    if (r) onResult(r)
  }
  return (
    <div className="stack-sm">
      <p className="small">{plural(data.total, 'signature')} found{data.total > data.sets.length ? ` (first ${data.sets.length} shown)` : ''}. Each GEO series gives one up and one down signature per pair of sample groups.</p>
      <div className="table-scroll fetch-table"><table className="table compact">
        <thead><tr><th>Series</th><th>Comparison</th><th>Direction</th><th>Genes</th><th>Species</th><th /></tr></thead>
        <tbody>{data.sets.map((s) => (
          <tr key={s.id} className={pick === s.id ? 'selected' : ''}>
            <td><a href={`https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=${s.gse.split(',')[0]}`} target="_blank" rel="noopener noreferrer">{s.gse}</a><div className="muted small">{s.title}</div></td>
            <td className="small">{s.contrast ? <><strong>{s.contrast.b}</strong> vs {s.contrast.a}</> : s.term}</td>
            <td><span className={`dir-chip ${s.direction}`}>{s.direction === 'up' ? '▲ up' : s.direction === 'down' ? '▼ down' : '—'}</span></td>
            <td>{s.size}</td><td>{s.species}</td>
            <td><button className="btn btn-secondary btn-sm" onClick={() => open(s.id)}>Genes</button></td>
          </tr>
        ))}</tbody>
      </table></div>
    </div>
  )
}

function PubmedArticles({ data }: { data: { articles: number; searched?: { query: string; count: number } | null; titles: { pmid: string; title: string; year: string; journal: string }[]; otherOrganisms: number } }) {
  return (
    <details className="stack-sm" open={data.titles.length <= 5}>
      <summary className="small">{data.searched ? `${data.searched.count.toLocaleString()} articles match; the ${data.articles} most relevant were used` : plural(data.articles, 'article')}
        {data.otherOrganisms ? ` · ${data.otherOrganisms} linked genes of other organisms left out` : ''} — show titles</summary>
      <ul className="plain-list small">{data.titles.map((t) => (
        <li key={t.pmid}><a href={`https://pubmed.ncbi.nlm.nih.gov/${t.pmid}/`} target="_blank" rel="noopener noreferrer">{t.title || t.pmid}</a> <span className="muted">{t.journal} {t.year}</span></li>
      ))}</ul>
    </details>
  )
}

interface ClinvarVariant { uid: string; accession: string; title: string; type: string; genes: string[] | string; chrpos: string; consequence: string; clinical: string; review: string; conditions: string[] | string; conditionCount: number; rsid: string }

function ClinvarVariants({ data }: { data: { searched?: { query: string; term: string; count: number } | null; variants: ClinvarVariant[]; multiGeneVariants?: number } }) {
  const navigate = useNavigate()
  const rsids = [...new Set(data.variants.map((v) => v.rsid).filter(Boolean))]
  const annotate = async () => {
    const r = await api<{ id: string }>('/variants', { body: { name: `ClinVar variants: ${data.searched?.query ?? ''}`.slice(0, 80), text: rsids.slice(0, 2000).join('\n'), organismTaxId: 9606 } })
    navigate(`/w/${getWorkspaceId()}/variants/${r.id}`)
  }
  return (
    <div className="stack-sm">
      <div className="row between wrap">
        <span className="small">{plural(data.variants.length, 'variant')}{data.searched ? ` of ${data.searched.count.toLocaleString()} matching` : ''}{data.searched?.term && data.searched.term !== data.searched.query ? <span className="muted"> · searched as <code>{data.searched.term}</code></span> : null}</span>
        <span className="row">
          <CopyButton label="Copy accessions" text={data.variants.map((v) => v.accession).join('\n')} />
          {rsids.length > 0 && <CopyButton label="Copy rsIDs" text={rsids.join('\n')} />}
          {rsids.length > 0 && <button className="btn btn-secondary btn-sm" onClick={annotate} title={`Full annotation of the ${rsids.length} variants that have an rsID, with Ensembl VEP, gnomAD and GWAS traits`}>Annotate variants →</button>}
        </span>
      </div>
      <div className="table-scroll fetch-table"><table className="table compact">
        <thead><tr><th>Variant</th><th>Genes</th><th>Location (GRCh38)</th><th>Consequence</th><th>Classification</th><th>Review status</th><th>Conditions</th><th>rsID</th></tr></thead>
        <tbody>{data.variants.slice(0, 300).map((v) => {
          const genes = asArray(v.genes); const conditions = asArray(v.conditions)
          return (
            <tr key={v.uid}>
              <td><a href={`https://www.ncbi.nlm.nih.gov/clinvar/variation/${v.uid}/`} target="_blank" rel="noopener noreferrer" title={v.accession}>{v.title || v.accession}</a><div className="muted small">{v.type}</div></td>
              <td>{genes.slice(0, 5).map((g, i) => <span key={g}>{i ? ', ' : ''}<GeneCardsLink gene={g} /></span>)}{genes.length > 5 ? <span className="muted small"> +{genes.length - 5} more</span> : null}</td>
              <td>{v.chrpos}</td><td className="small">{v.consequence}</td><td className="small">{v.clinical}</td><td className="small muted">{v.review}</td>
              <td className="small">{conditions.join('; ')}{v.conditionCount > conditions.length ? ` (+${v.conditionCount - conditions.length})` : ''}</td>
              <td>{v.rsid && <a href={`https://www.ncbi.nlm.nih.gov/snp/${v.rsid}`} target="_blank" rel="noopener noreferrer">{v.rsid}</a>}</td>
            </tr>
          )
        })}</tbody>
      </table></div>
      {data.variants.length > 300 && <p className="muted small">Showing the first 300 variants; the gene list below uses all {data.variants.length}.</p>}
      {!!data.multiGeneVariants && <p className="muted small">{plural(data.multiGeneVariants, 'variant')} spanning more than five genes (large deletions or duplications) or no gene {data.multiGeneVariants === 1 ? 'is' : 'are'} listed above but not counted in the gene list below.</p>}
    </div>
  )
}

/** Ranked genes of the fetched source; choose how many and save them as a list. */
function GenePicker({ r, mode, onCreated }: { r: GeneResult; mode: FetchMode; onCreated: (l: AnalyteList) => void }) {
  const value = (g: FGene) => g.sources ?? g.score ?? g.articles ?? g.variants ?? null
  const valueLabel = r.genes.some((g) => g.sources != null) ? 'selected results listing the gene' : r.genes.some((g) => g.score != null) ? 'association score' : r.genes.some((g) => g.articles != null) ? 'articles' : r.genes.some((g) => g.variants != null) ? 'variants' : null
  const [top, setTop] = useState(Math.min(r.genes.length, valueLabel === 'association score' ? 200 : 500))
  const [withScores, setWithScores] = useState(false)
  const [organism, setOrganism] = useState(r.organismTaxId)
  const [idFormat, setIdFormat] = useState('USERINPUT')
  const [sel, setSel] = useState<string[]>([])
  useEffect(() => { setTop(Math.min(r.genes.length, valueLabel === 'association score' ? 200 : 500)); setOrganism(r.organismTaxId); setSel([]) }, [r, valueLabel])
  // Two ways to choose: the top N genes (slider), or hand-picked bars. Hand-picked genes win while there are any.
  const chosen = useMemo(() => (sel.length ? r.genes.filter((g) => sel.includes(g.gene)) : r.genes.slice(0, top)), [r, top, sel])
  const chosenSet = useMemo(() => new Set(chosen.map((g) => g.gene)), [chosen])
  const shown = r.genes.slice(0, 40)
  const TABLE_ROWS = 500
  const hasFrom = r.genes.some((g) => g.from?.length)
  const toggleGene = (gene: string) => setSel((c) => (c.includes(gene) ? c.filter((x) => x !== gene) : [...c, gene]))
  const minTop = Math.min(5, r.genes.length)
  const name = { disease: r.disease ?? r.term ?? 'Disease genes', pubmed: 'PubMed genes', geo: r.term ?? 'GEO signature', clinvar: 'ClinVar variant genes' }[mode]
  return (
    <div className="stack-sm fetch-genes">
      <div className="row between wrap">
        <h3>{plural(r.genes.length, 'gene')}{r.source ? ` · ${r.source}` : ''}{r.total && r.total > r.genes.length ? ` (top ${r.genes.length} of ${r.total.toLocaleString()})` : ''}</h3>
        <span className="row"><CopyButton label="Copy genes" text={chosen.map((g) => g.gene).join('\n')} />
          <button className="btn btn-ghost btn-sm" onClick={() => saveXlsx([{ name: (r.source ?? mode).slice(0, 31), rows: r.genes.map((g, i) => ({ Rank: i + 1, Gene: g.gene, Name: g.name ?? '',
            ...(valueLabel ? { [valueLabel]: value(g) ?? '' } : {}), ...(g.pmids ? { PMIDs: g.pmids.join(', ') } : {}), ...(g.from ? { 'Listed by': g.from.join('; ') } : {}),
            ...(g.evidence ? Object.fromEntries(Object.entries(g.evidence).map(([k, v]) => [`Evidence: ${k}`, v])) : {}) })) }], `flame_${mode}_genes.xlsx`)}>Excel</button>{r.link && <a className="btn btn-ghost btn-sm" href={r.link} target="_blank" rel="noopener noreferrer">Source ↗</a>}</span>
      </div>
      {r.genes.length > 1 && (
        <div className="keep-top">
          <label className="small" htmlFor="keep-top-range">Keep the top</label>
          <input className="input input-sm input-narrow" type="number" min={minTop} max={r.genes.length} value={top} disabled={sel.length > 0} aria-label="Number of top genes to keep"
            onChange={(e) => { const n = Math.round(Number(e.target.value)); if (Number.isFinite(n)) setTop(Math.max(1, Math.min(r.genes.length, n))) }} />
          <span className="small">of {r.genes.length.toLocaleString()} genes</span>
          <input id="keep-top-range" type="range" min={minTop} max={r.genes.length} step={1} value={Math.max(minTop, top)} disabled={sel.length > 0}
            onChange={(e) => setTop(Number(e.target.value))} />
          {sel.length > 0 && <span className="small"><strong>{plural(sel.length, 'hand-picked gene')}</strong> used instead <button className="btn btn-ghost btn-sm" onClick={() => setSel([])}>Back to top {top}</button></span>}
        </div>
      )}
      <div className="table-scroll fetch-table gene-table"><table className="table compact">
        <thead><tr><th aria-label="Hand-pick" /><th className="num">#</th><th>Gene</th><th>Name</th>{valueLabel && <th className="num">{valueLabel === 'selected results listing the gene' ? 'Results' : valueLabel[0].toUpperCase() + valueLabel.slice(1)}</th>}{hasFrom && <th>Listed by</th>}</tr></thead>
        <tbody>{r.genes.slice(0, TABLE_ROWS).map((g, i) => (
          <tr key={g.gene} className={chosenSet.has(g.gene) ? 'selected' : ''}>
            <td><input type="checkbox" checked={sel.includes(g.gene)} aria-label={`Hand-pick ${g.gene}`} onChange={() => toggleGene(g.gene)} /></td>
            <td className="num muted">{i + 1}</td><td><GeneCardsLink gene={g.gene} /></td><td className="small">{g.name}</td>
            {valueLabel && <td className="num">{typeof value(g) === 'number' && !Number.isInteger(value(g)) ? (value(g) as number).toFixed(3) : value(g)}</td>}
            {hasFrom && <td className="small muted">{(g.from ?? []).join('; ')}</td>}
          </tr>
        ))}</tbody>
      </table></div>
      <p className="muted small">Highlighted rows are the genes that will be saved{r.genes.length > TABLE_ROWS ? ` (the table shows the first ${TABLE_ROWS} of ${r.genes.length.toLocaleString()})` : ''}. Tick genes to hand-pick them instead of taking the top ones.</p>
      {valueLabel && (
        <details className="gene-chart" open={mode !== 'disease'}>
          <summary className="small">Bar chart of the first {shown.length} genes</summary>
        <Plot tools={false} height={Math.max(260, shown.length * 15 + 70)}
          onSelect={(ids) => ids.forEach(toggleGene)} data={[{
          type: 'bar', orientation: 'h', y: shown.map((g) => g.gene), x: shown.map((g) => value(g) ?? 0), customdata: shown.map((g) => g.gene),
          text: shown.map((g) => g.name ?? ''), textposition: 'none',
          marker: { color: shown.map((g) => (!chosenSet.has(g.gene) ? '#c7ccd3' : sel.length ? '#e11d48' : '#8b5cf6')) },
          hovertemplate: `%{y}: %{x} ${valueLabel}<br>%{text}<extra></extra>`,
        } as never]} layout={{ yaxis: { autorange: 'reversed', automargin: true }, xaxis: { title: { text: valueLabel } }, margin: { l: 80, r: 10, t: 10, b: 45 } }} />
          <p className="muted small">Coloured bars are the genes that will be saved; click a bar to hand-pick or remove that gene.</p>
        </details>
      )}
      {!valueLabel && <GeneChips genes={r.genes.map((g) => g.gene)} taxid={r.organismTaxId} max={80} />}
      <p className="small"><strong>{plural(chosen.length, 'gene')}</strong> will be saved{sel.length ? ' (hand-picked)' : r.genes.length > 1 ? ` (the top ${chosen.length} by ${valueLabel ?? 'rank'})` : ''}.</p>
      {valueLabel && <details className="small"><summary>Genes that will be saved ({chosen.length}) — click a gene for GeneCards</summary><GeneChips genes={chosen.map((g) => g.gene)} taxid={r.organismTaxId} max={200} /></details>}
      {r.citation && <p className="muted small">{r.citation}</p>}
      {chosen.length > 0 && (
        <SaveList ids={chosen.map((g) => g.gene)} scores={withScores && valueLabel ? chosen.map((g) => value(g) ?? 0) : undefined}
          defaultName={name.slice(0, 70)} organism={organism} setOrganism={setOrganism} idFormat={idFormat} setIdFormat={setIdFormat}
          meta={{ method: `fetch-${mode}`, source: r.source ?? mode, query: r.disease ?? r.term ?? '' }} onCreated={onCreated}
          extra={valueLabel ? <label className="check small"><input type="checkbox" checked={withScores} onChange={(e) => setWithScores(e.target.checked)} /> Keep {valueLabel} as scores (ranked list for GSEA)</label> : undefined} />
      )}
      {r.genes.length === 0 && <Note kind="info">No genes for this selection.</Note>}
    </div>
  )
}
