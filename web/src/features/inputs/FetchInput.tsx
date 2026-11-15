import { useEffect, useMemo, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { api, getWorkspaceId } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { Plot } from '../../components/charts'
import { CopyButton, ErrorNote, Field, GeneCardsLink, GeneChips, Modal, Note, OrganismPicker, Spinner } from '../../components/ui'
import { formatP, plural } from '../../lib/format'
import { SaveList } from './InputModal'
import { saveXlsx } from '../../lib/xlsx'

export type FetchMode = 'disease' | 'pubmed' | 'geo' | 'dbsnp'

interface FGene { gene: string; name?: string; score?: number; articles?: number; variants?: number; pmids?: string[]; evidence?: Record<string, number> }
interface GeneResult { genes: FGene[]; organismTaxId: number; ranked?: boolean; citation?: string; source?: string; disease?: string; term?: string; link?: string; total?: number }

const EXAMPLES: Record<FetchMode, string> = {
  disease: 'asthma',
  pubmed: 'glucocorticoid receptor airway smooth muscle',
  geo: 'GSE52778',
  dbsnp: 'APOE[gene] AND pathogenic[clinical_significance]',
}
const PLACEHOLDER: Record<FetchMode, string> = {
  disease: 'A disease or trait, e.g. asthma, Alzheimer disease, cystic fibrosis',
  pubmed: 'PMIDs (24926665 36599824) or a PubMed search (glucocorticoid AND asthma)',
  geo: 'A GEO series (GSE52778) or keywords (dexamethasone airway)',
  dbsnp: 'rsIDs (rs429358 rs7412) or a dbSNP search (APOE[gene] AND pathogenic[clinical_significance])',
}

/**
 * Fetch a gene list from public resources: genes of a disease (Open Targets,
 * DisGeNET, OMIM, Orphanet, ClinVar…), genes linked to PubMed articles,
 * differential-expression signatures of GEO series, and dbSNP variants.
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
  const call = async <T,>(label: string, path: string, body: Record<string, unknown>) => {
    setBusy(label); setErr(null)
    try { return await api<T>(path, { body }) } catch (e) { setErr(e); return null } finally { setBusy(null) }
  }
  const search = async () => {
    setResult(null); setExtra(null)
    if (mode === 'disease') { const r = await call<Record<string, unknown>>('Searching Open Targets and disease libraries', '/inputs/disease/search', { query }); if (r) setExtra(r) }
    if (mode === 'pubmed') { const r = await call<GeneResult & Record<string, unknown>>('Fetching articles and their genes from NCBI', '/inputs/pubmed/genes', { query, organismTaxId: taxid, max }); if (r) { setResult(r); setExtra(r) } }
    if (mode === 'geo') { const r = await call<Record<string, unknown>>('Searching GEO signatures', '/inputs/geo/search', { query }); if (r) setExtra(r) }
    if (mode === 'dbsnp') { const r = await call<GeneResult & Record<string, unknown>>('Fetching variants from dbSNP', '/inputs/dbsnp/lookup', { query }); if (r) { setResult(r); setExtra(r) } }
  }
  useEffect(() => { if (example && query) search() // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])
  const subtitle = {
    disease: 'Genes associated with a disease: Open Targets (scored evidence), DisGeNET, OMIM, Orphanet, ClinVar, Jensen DISEASES and GWAS Catalog.',
    pubmed: 'Genes linked to PubMed articles in NCBI Gene (gene2pubmed), ranked by how many of the articles mention them.',
    geo: 'Up- or down-regulated genes of GEO series, from uniformly re-analysed data (RummaGEO).',
    dbsnp: 'Variants from NCBI dbSNP with their genes, consequences, ClinVar significance and frequencies.',
  }[mode]
  return (
    <Modal title={title} subtitle={subtitle} size="xl" onClose={onClose}>
      <div className="stack">
        <div className="row wrap fetch-bar">
          {mode === 'pubmed' || mode === 'dbsnp'
            ? <textarea className="input" rows={2} value={query} placeholder={PLACEHOLDER[mode]} onChange={(e) => setQuery(e.target.value)} />
            : <input className="input" value={query} placeholder={PLACEHOLDER[mode]} onChange={(e) => setQuery(e.target.value)} onKeyDown={(e) => { if (e.key === 'Enter') search() }} />}
          <button className="btn btn-primary" disabled={!query.trim() || !!busy} onClick={search}>{mode === 'pubmed' || mode === 'dbsnp' ? 'Fetch' : 'Search'}</button>
          {!query && <button className="btn btn-ghost btn-sm" onClick={() => setQuery(EXAMPLES[mode])}>Example</button>}
        </div>
        {mode === 'pubmed' && (
          <div className="row wrap">
            <OrganismPicker value={taxid} onChange={setTaxid} />
            <Field label="Articles (for a search)"><select className="input" value={max} onChange={(e) => setMax(Number(e.target.value))}>{[50, 100, 200, 500].map((n) => <option key={n} value={n}>{n} most relevant</option>)}</select></Field>
          </div>
        )}
        <ErrorNote error={err} />
        {busy && <Spinner label={busy} />}
        {mode === 'disease' && extra && <DiseasePicker data={extra as never} onResult={setResult} call={call} />}
        {mode === 'geo' && extra && <GeoPicker data={extra as never} onResult={setResult} call={call} />}
        {mode === 'pubmed' && extra && <PubmedArticles data={extra as never} />}
        {mode === 'dbsnp' && extra && <DbsnpVariants data={extra as never} />}
        {result && <GenePicker r={result} mode={mode} onCreated={onCreated} />}
      </div>
    </Modal>
  )
}

type Call = <T>(label: string, path: string, body: Record<string, unknown>) => Promise<T | null>

function DiseasePicker({ data, onResult, call }: {
  data: { diseases: { id: string; name: string; description: string }[]; sets: { source: string; label: string; term: string; size: number }[] }
  onResult: (r: GeneResult | null) => void; call: Call
}) {
  const [pick, setPick] = useState<string | null>(null)
  const [gwas, setGwas] = useState<{ variants: { rsid: string; p: number; genes: string[]; study: string; trait: string }[]; genes: FGene[]; citation: string; total: number } | null>(null)
  const navigate = useNavigate()
  const open = async (key: string, body: Record<string, unknown>, label: string) => {
    setPick(key); setGwas(null); onResult(null)
    const r = await call<GeneResult>(label, '/inputs/disease/genes', body)
    if (r && body.source === 'gwas') { setGwas(r as never); onResult({ ...(r as GeneResult), organismTaxId: 9606, ranked: true, source: 'GWAS Catalog' }) } else if (r) onResult(r)
  }
  const annotate = async () => {
    if (!gwas) return
    const r = await api<{ id: string }>('/variants', { body: { name: `GWAS variants: ${data.diseases.find((d) => `ot:${d.id}` === pick || `gwas:${d.id}` === pick)?.name ?? 'disease'}`, text: gwas.variants.map((v) => v.rsid).join('\n'), organismTaxId: 9606 } })
    navigate(`/w/${getWorkspaceId()}/variants/${r.id}`)
  }
  return (
    <div className="fetch-grid">
      <div className="stack-sm">
        <h3>Diseases <span className="muted small">Open Targets · EFO / MONDO</span></h3>
        {data.diseases.length === 0 && <p className="muted small">No disease matches.</p>}
        <ul className="plain-list fetch-list">
          {data.diseases.map((d) => (
            <li key={d.id} className={pick?.endsWith(d.id) ? 'on' : ''}>
              <div><strong>{d.name}</strong> <a className="muted small" href={`https://platform.opentargets.org/disease/${d.id}`} target="_blank" rel="noopener noreferrer">{d.id} ↗</a></div>
              {d.description && <p className="muted small">{d.description}</p>}
              <span className="row wrap">
                <button className="btn btn-secondary btn-sm" onClick={() => open(`ot:${d.id}`, { source: 'opentargets', id: d.id, size: 1000 }, 'Fetching associated genes from Open Targets')}>Associated genes (scored)</button>
                <button className="btn btn-ghost btn-sm" onClick={() => open(`gwas:${d.id}`, { source: 'gwas', id: d.id, size: 500 }, 'Fetching GWAS Catalog associations')}>GWAS variants</button>
              </span>
            </li>
          ))}
        </ul>
      </div>
      <div className="stack-sm">
        <h3>Gene sets <span className="muted small">DisGeNET · OMIM · Orphanet · ClinVar · Jensen · GWAS</span></h3>
        {data.sets.length === 0 && <p className="muted small">No gene set matches.</p>}
        <ul className="plain-list fetch-list">
          {data.sets.slice(0, 60).map((s) => (
            <li key={`${s.source}:${s.term}`} className={pick === `${s.source}:${s.term}` ? 'on' : ''}>
              <button className="link-btn" onClick={() => open(`${s.source}:${s.term}`, { source: s.source, term: s.term }, `Fetching ${s.label}`)}><strong>{s.term}</strong></button>
              <span className="muted small"> {s.label} · {s.size} genes</span>
            </li>
          ))}
        </ul>
      </div>
      {gwas && (
        <div className="stack-sm fetch-wide">
          <div className="row between wrap">
            <h3>{plural(gwas.total, 'GWAS variant')} (p ≤ 5e-8)</h3>
            <span className="row"><CopyButton label="Copy rsIDs" text={gwas.variants.map((v) => v.rsid).join('\n')} />
              <button className="btn btn-secondary btn-sm" onClick={annotate} title="Annotate the variants with Ensembl VEP, dbSNP, gnomAD, ClinVar and traits">Annotate variants →</button></span>
          </div>
          <div className="table-scroll fetch-table"><table className="table compact">
            <thead><tr><th>Variant</th><th>p</th><th>Mapped genes</th><th>Reported trait</th><th>Study</th></tr></thead>
            <tbody>{gwas.variants.slice(0, 200).map((v) => (
              <tr key={v.rsid}><td><a href={`https://www.ncbi.nlm.nih.gov/snp/${v.rsid}`} target="_blank" rel="noopener noreferrer">{v.rsid}</a></td><td>{formatP(v.p)}</td>
                <td>{(v.genes ?? []).map((g, i) => <span key={g}>{i ? ', ' : ''}<GeneCardsLink gene={g} /></span>)}</td><td className="small">{v.trait}</td>
                <td><a href={`https://www.ebi.ac.uk/gwas/studies/${v.study}`} target="_blank" rel="noopener noreferrer">{v.study}</a></td></tr>
            ))}</tbody>
          </table></div>
        </div>
      )}
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

function DbsnpVariants({ data }: { data: { searched?: { query: string; count: number } | null; variants: { rsid: string; chrpos: string; genes: string[]; consequence: string; clinical: string; frequency: string }[] } }) {
  const navigate = useNavigate()
  const annotate = async () => {
    const r = await api<{ id: string }>('/variants', { body: { name: 'dbSNP variants', text: data.variants.map((v) => v.rsid).join('\n'), organismTaxId: 9606 } })
    navigate(`/w/${getWorkspaceId()}/variants/${r.id}`)
  }
  return (
    <div className="stack-sm">
      <div className="row between wrap">
        <span className="small">{plural(data.variants.length, 'variant')}{data.searched ? ` of ${data.searched.count.toLocaleString()} matching` : ''}</span>
        <span className="row"><CopyButton label="Copy rsIDs" text={data.variants.map((v) => v.rsid).join('\n')} />
          <button className="btn btn-secondary btn-sm" onClick={annotate} title="Full annotation with Ensembl VEP, gnomAD, ClinVar and GWAS traits">Annotate variants →</button></span>
      </div>
      <div className="table-scroll fetch-table"><table className="table compact">
        <thead><tr><th>Variant</th><th>Location (GRCh38)</th><th>Genes</th><th>Consequence</th><th>Clinical significance</th><th>Frequency</th></tr></thead>
        <tbody>{data.variants.slice(0, 300).map((v) => (
          <tr key={v.rsid}><td><a href={`https://www.ncbi.nlm.nih.gov/snp/${v.rsid}`} target="_blank" rel="noopener noreferrer">{v.rsid}</a></td><td>{v.chrpos}</td><td>{v.genes.map((g, i) => <span key={g}>{i ? ', ' : ''}<GeneCardsLink gene={g} /></span>)}</td>
            <td className="small">{v.consequence}</td><td className="small">{v.clinical}</td><td className="small">{v.frequency}</td></tr>
        ))}</tbody>
      </table></div>
    </div>
  )
}

/** Ranked genes of the fetched source; choose how many and save them as a list. */
function GenePicker({ r, mode, onCreated }: { r: GeneResult; mode: FetchMode; onCreated: (l: AnalyteList) => void }) {
  const value = (g: FGene) => g.score ?? g.articles ?? g.variants ?? null
  const valueLabel = r.genes.some((g) => g.score != null) ? 'association score' : r.genes.some((g) => g.articles != null) ? 'articles' : r.genes.some((g) => g.variants != null) ? 'variants' : null
  const [top, setTop] = useState(Math.min(r.genes.length, valueLabel === 'association score' ? 200 : 500))
  const [withScores, setWithScores] = useState(false)
  const [organism, setOrganism] = useState(r.organismTaxId)
  const [idFormat, setIdFormat] = useState('USERINPUT')
  const [sel, setSel] = useState<string[]>([])
  useEffect(() => { setTop(Math.min(r.genes.length, valueLabel === 'association score' ? 200 : 500)); setOrganism(r.organismTaxId); setSel([]) }, [r, valueLabel])
  const chosen = useMemo(() => (sel.length ? r.genes.filter((g) => sel.includes(g.gene)) : r.genes.slice(0, top)), [r, top, sel])
  const shown = r.genes.slice(0, 40)
  const name = { disease: r.disease ?? r.term ?? 'Disease genes', pubmed: 'PubMed genes', geo: r.term ?? 'GEO signature', dbsnp: 'dbSNP variant genes' }[mode]
  return (
    <div className="stack-sm fetch-genes">
      <div className="row between wrap">
        <h3>{plural(r.genes.length, 'gene')}{r.source ? ` · ${r.source}` : ''}{r.total && r.total > r.genes.length ? ` (top ${r.genes.length} of ${r.total.toLocaleString()})` : ''}</h3>
        <span className="row"><CopyButton label="Copy genes" text={chosen.map((g) => g.gene).join('\n')} />
          <button className="btn btn-ghost btn-sm" onClick={() => saveXlsx([{ name: (r.source ?? mode).slice(0, 31), rows: r.genes.map((g, i) => ({ Rank: i + 1, Gene: g.gene, Name: g.name ?? '',
            ...(valueLabel ? { [valueLabel]: value(g) ?? '' } : {}), ...(g.pmids ? { PMIDs: g.pmids.join(', ') } : {}),
            ...(g.evidence ? Object.fromEntries(Object.entries(g.evidence).map(([k, v]) => [`Evidence: ${k}`, v])) : {}) })) }], `flame_${mode}_genes.xlsx`)}>Excel</button>{r.link && <a className="btn btn-ghost btn-sm" href={r.link} target="_blank" rel="noopener noreferrer">Source ↗</a>}</span>
      </div>
      {valueLabel && (
        <Plot height={Math.max(260, shown.length * 15 + 70)} onSelect={(ids, add) => setSel((c) => (add ? [...new Set([...c, ...ids])] : ids))} data={[{
          type: 'bar', orientation: 'h', y: shown.map((g) => g.gene), x: shown.map((g) => value(g) ?? 0), customdata: shown.map((g) => g.gene),
          text: shown.map((g) => g.name ?? ''), marker: { color: shown.map((g) => (sel.includes(g.gene) ? '#e11d48' : '#8b5cf6')) },
          hovertemplate: `%{y}: %{x} ${valueLabel}<br>%{text}<extra></extra>`,
        } as never]} layout={{ yaxis: { autorange: 'reversed', automargin: true }, xaxis: { title: { text: valueLabel } }, margin: { l: 80, r: 10, t: 10, b: 45 } }} />
      )}
      {!valueLabel && <GeneChips genes={r.genes.map((g) => g.gene)} taxid={r.organismTaxId} max={80} />}
      {valueLabel && <p className="muted small">The first 40 genes by {valueLabel}. Box-select bars to keep only those genes; otherwise the top genes below are saved.</p>}
      {!sel.length && r.genes.length > 10 && (
        <label className="small">Keep the top <strong>{top}</strong> genes
          <input type="range" min={5} max={r.genes.length} value={top} onChange={(e) => setTop(Number(e.target.value))} aria-label="Number of genes to keep" />
        </label>
      )}
      {sel.length > 0 && <p className="small"><strong>{plural(sel.length, 'selected gene')}</strong> <button className="btn btn-ghost btn-sm" onClick={() => setSel([])}>Clear selection</button></p>}
      {valueLabel && <details className="small"><summary>Genes in order ({r.genes.length}) — click a gene for GeneCards</summary><GeneChips genes={(sel.length ? r.genes.filter((g) => sel.includes(g.gene)) : r.genes).map((g) => g.gene)} taxid={r.organismTaxId} max={200} /></details>}
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
