import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useEffect, useMemo, useRef, useState } from 'react'
import { Link, useNavigate, useParams, useSearchParams } from 'react-router-dom'
import { api, getWorkspaceId, saveText } from '../api/client'
import { useCreateList } from '../api/hooks'
import { ListsPanel } from '../components/ListsPanel'
import { Plot, type PlotHandle } from '../components/charts'
import { BackToWorkspace, EmptyState, ErrorNote, Field, FileDrop, LinkMenu, Note, OpenExternalButton, Segmented, Spinner } from '../components/ui'
import { geneLinks, variantLinks } from '../lib/links'
import { Arena3DIcon } from '../components/ExternalIcons'
import { formatP, plural, toCsv } from '../lib/format'
import { categorical, PaletteScope, roleColors, usePaletteMode } from '../lib/palette'
import { PaletteSelect } from '../components/PaletteSelect'
import { CircosBundle } from '../components/viz'
import { usePlainSelection } from '../lib/imageExport'
import { ProgressBar, type JobProgress } from '../components/Progress'
import { saveXlsx, sheetsByCategory } from '../lib/xlsx'

interface Analysis { id: string; name: string; organismTaxId: number; status: string; error?: string; inputCount: number; created: number; finished?: number; progress?: JobProgress | null }
interface VariantRow {
  input: string; id: string; location: string; chr: string; pos: number; alleles: string; consequence: string; impact: string
  gene: string | null; allGenes: string; biotype: string | null; aminoAcids: string | null; sift: string | null; polyphen: string | null
  gnomadAF: number | null; clinicalSignificance: string | null; pvalue: number | null
}
interface Phenotype { variant: string; trait: string; source: string; riskAllele: string | null; pvalue: number | null; study: string | null }
interface GeneRow { gene: string; variants: number; worstImpact: string; bestP: number | null; score: number | null }
interface Result {
  mode: 'variants' | 'gwas'; organismTaxId: number; inputCount: number; annotatedCount: number; skipped: number
  variants: VariantRow[]; phenotypes: Phenotype[]; genes: GeneRow[]
  summary: { consequences: { consequence: string; count: number }[]; impacts: { impact: string; count: number }[]
    chromosomes: { chromosome: string; count: number }[]; clinical: { significance: string; count: number }[]
    traits: { trait: string; variants: number; sources: string; bestP: number | null }[] }
}

function useImpactColors(): Record<string, string> {
  const rc = roleColors(usePaletteMode())
  return { HIGH: rc.high, MODERATE: rc.moderate, LOW: rc.low, MODIFIER: rc.modifier }
}
const CHR_ORDER = [...Array.from({ length: 22 }, (_, i) => String(i + 1)), 'X', 'Y', 'MT']

export default function VariantsPage() {
  return <PaletteScope id="variants"><VariantsPageBody /></PaletteScope>
}

function VariantsPageBody() {
  const ws = getWorkspaceId()!
  const { analysisId } = useParams()
  const navigate = useNavigate()
  const qc = useQueryClient()
  const [params, setParams] = useSearchParams()
  const list = useQuery({
    queryKey: ['variants', ws],
    queryFn: () => api<Analysis[]>('/variants'),
    refetchInterval: (q) => ((q.state.data ?? []).some((a) => a.status === 'queued' || a.status === 'running') ? 3000 : false),
  })
  const current = analysisId ?? list.data?.[0]?.id

  return (
    <div className="stack">
      <header className="results-head">
        <BackToWorkspace ws={ws} />
        <div>
          <h1>Variant analysis</h1>
          <p className="muted small">Start from SNPs, small variants or GWAS results: see their consequences, affected genes, population frequencies, clinical significance and known trait associations — then take the genes into enrichment.</p>
        </div>
        {analysisId && <PaletteSelect />}
      </header>
      <div className="variants-layout">
        <aside className="stack">
          <ListsPanel boxed />
          <NewAnalysis example={params.get('example')} onStarted={(id) => {
            setParams({}, { replace: true })
            qc.invalidateQueries({ queryKey: ['variants', ws] })
            navigate(`/w/${ws}/variants/${id}`)
          }} />
          <section className="rail-section">
            <h2>Analyses</h2>
            {list.isLoading && <Spinner />}
            {!list.data?.length && !list.isLoading && <p className="muted small">None yet.</p>}
            <ul className="plain-list">
              {(list.data ?? []).map((a) => (
                <li key={a.id}>
                  <Link className={`rail-item analysis-link${a.id === current ? ' active' : ''}`} to={`/w/${ws}/variants/${a.id}`}>
                    <span className="list-text"><span className="list-name">{a.name}</span>
                      <span className="muted small">{plural(a.inputCount, 'variant')} · {a.status === 'complete' ? 'ready' : a.status === 'error' ? 'failed' : 'annotating…'}</span></span>
                  </Link>
                </li>
              ))}
            </ul>
          </section>
        </aside>
        <section className="variants-main">
          {current ? <Report key={current} id={current} /> : (
            <EmptyState title="Paste variants or GWAS results to begin">
              <p>Supported: rsIDs (rs429358), VCF lines (chr pos id ref alt), chr:pos:ref:alt, HGVS (ENST…:c.803C&gt;T), or a GWAS summary-statistics table with SNP and P columns.</p>
            </EmptyState>
          )}
        </section>
      </div>
    </div>
  )
}

function NewAnalysis({ example, onStarted }: { example: string | null; onStarted: (id: string) => void }) {
  const [text, setText] = useState('')
  const [name, setName] = useState('')
  const [taxid, setTaxid] = useState(9606)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const species = useQuery({ queryKey: ['variant-species'], queryFn: () => api<{ taxid: number; name: string }[]>('/variants/species', { workspace: null }), staleTime: Infinity })
  const [exampleSource, setExampleSource] = useState<string | null>(null)
  const loadExample = (kind: 'variants' | 'gwas') => api<{ text: string; source?: string }>(`/variants/examples/${kind}`).then((r) => {
    setText(r.text)
    setName(kind === 'gwas' ? 'Alzheimer GWAS (Bellenguez 2022)' : 'Example clinical variants')
    setExampleSource(r.source ?? null)
  })
  const started = useRef(false)
  useEffect(() => {
    if ((example === 'variants' || example === 'gwas') && !started.current) {
      started.current = true
      loadExample(example)
    }
  }, [example])
  const lines = text.split(/\r?\n/).filter((l) => l.trim()).length
  return (
    <section className="rail-section">
      <h2>New analysis</h2>
      <Field label="Variants" hint="rsIDs, VCF lines, chr:pos:ref:alt, HGVS — or a GWAS table with SNP and P columns (up to 2,000 rows; GWAS keeps the most significant).">
        <textarea className="input mono" rows={8} value={text} onChange={(e) => setText(e.target.value)} placeholder={'rs429358\nrs7412\n7 140753336 . A T'} />
      </Field>
      {exampleSource && <p className="example-source"><strong>Example data:</strong> {exampleSource}</p>}
      <FileDrop accept=".vcf,.txt,.tsv,.csv" maxBytes={2 * 1024 * 1024} onText={(t, n) => { setText(t); if (!name) setName(n) }} />
      <div className="row wrap">
        <button className="btn btn-ghost btn-sm" onClick={() => loadExample('variants')}>Example: clinical variants</button>
        <button className="btn btn-ghost btn-sm" onClick={() => loadExample('gwas')}>Example: GWAS results</button>
      </div>
      <Field label="Name"><input className="input" value={name} maxLength={80} onChange={(e) => setName(e.target.value)} placeholder="optional" /></Field>
      <Field label="Organism" hint="Human uses GRCh38 coordinates.">
        <select className="input" value={taxid} onChange={(e) => setTaxid(Number(e.target.value))}>
          {(species.data ?? [{ taxid: 9606, name: 'Homo sapiens (Human)' }]).map((s) => <option key={s.taxid} value={s.taxid}>{s.name.replace(/ \[NCBI.*$/, '')}</option>)}
        </select>
      </Field>
      <ErrorNote error={err} />
      <button className="btn btn-primary" disabled={!lines || busy} onClick={() => {
        setBusy(true); setErr(null)
        api<{ id: string }>('/variants', { body: { text, name: name || undefined, organismTaxId: taxid } })
          .then((r) => { setText(''); setName(''); onStarted(r.id) }).catch(setErr).finally(() => setBusy(false))
      }}>{busy ? 'Starting…' : `Annotate ${lines ? plural(lines, 'line') : ''}`}</button>
    </section>
  )
}

function Report({ id }: { id: string }) {
  const q = useQuery({
    queryKey: ['variant', getWorkspaceId(), id],
    queryFn: () => api<Analysis & { result?: Result }>(`/variants/${id}`),
    refetchInterval: (qq) => (qq.state.data && ['queued', 'running'].includes(qq.state.data.status) ? 1200 : false),
  })
  const [traitFilter, setTraitFilter] = useState<string | null>(null)
  const [varSel, setVarSel] = useState<{ label: string; ids: Set<string> } | null>(null)
  // Drawn selection; none while an image is exported without highlighting.
  const shownSel = usePlainSelection(varSel, null)
  const IMPACT_COLOR = useImpactColors()
  const pmR = usePaletteMode()
  const series = roleColors(pmR).series
  if (q.isLoading) return <Spinner />
  if (q.error) return <ErrorNote error={q.error} />
  const a = q.data!
  if (a.status === 'error') return <ErrorNote error={`The analysis failed: ${a.error}`} />
  if (!a.result) return (
    <div className="page-center">
      <div className="progress-card">
        <h2>Analysing {plural(a.inputCount, 'variant')}</h2>
        <p className="muted small">Ensembl VEP annotates consequences, genes, frequencies and ClinVar; then trait associations are looked up. This takes about 1–2 seconds per variant.</p>
        <ProgressBar progress={a.progress} status={a.status} />
      </div>
    </div>
  )
  const r = a.result
  const pick = (label: string, ids: string[], additive = false) => {
    if (!ids.length) return
    setVarSel((cur) => (additive && cur ? { label: `${cur.label} + ${label}`, ids: new Set([...cur.ids, ...ids]) } : { label, ids: new Set(ids) }))
  }
  const byField = (label: string, test: (v: VariantRow) => boolean, additive: boolean) => pick(label, r.variants.filter(test).map((v) => v.id), additive)
  const high = r.summary.impacts.filter((i) => i.impact === 'HIGH' || i.impact === 'MODERATE').reduce((s, i) => s + i.count, 0)
  return (
    <div className="stack">
      <div className="row between wrap">
        <h2>{a.name}</h2>
        <span className="row wrap">
          <span className="muted small">{r.mode === 'gwas' ? 'GWAS summary statistics' : 'Variant list'} · annotated {r.annotatedCount} of {r.inputCount}{r.skipped ? ` · ${r.skipped} lines not recognised` : ''}</span>
          <OpenExternalButton path={`/variants/${encodeURIComponent(id)}/arena3d`} body={{}} label="Arena3D" caption="variants · genes · traits in 3D" icon={<Arena3DIcon />}
            title="Open in Arena3D as three 3D layers: variants (coloured by impact), the genes they affect, and their associated traits" />
        </span>
      </div>
      <p className="muted small">Click bars, Sankey nodes or Circos dots, or drag a box or lasso on the Manhattan plot, to select variants (Shift adds to the selection). The variants table below follows your selection.</p>
      <div className="pin-grid">
        <div className="pin-card"><span className="pin-value">{r.annotatedCount}</span><span className="pin-label">Variants annotated</span></div>
        <div className="pin-card"><span className="pin-value">{high}</span><span className="pin-label">High or moderate impact</span><span className="muted small">Change a protein or its splicing</span></div>
        <div className="pin-card"><span className="pin-value">{r.genes.length}</span><span className="pin-label">Genes affected</span></div>
        <div className="pin-card"><span className="pin-value">{r.summary.traits.length}</span><span className="pin-label">Associated traits</span><span className="muted small">GWAS Catalog, ClinVar, OMIM…</span></div>
      </div>

      {r.mode === 'gwas' && <Manhattan variants={r.variants} selected={shownSel?.ids} onSelect={(ids, add) => pick(`${ids.length} on the Manhattan plot`, ids, add)} />}

      <div className="chart-grid">
        <BarCard title="Impact" onPick={(l, add) => byField(`impact ${l}`, (v) => v.impact === l, add)} rows={r.summary.impacts.filter((i) => i.count).map((i) => ({ label: i.impact, value: i.count, color: IMPACT_COLOR[i.impact] }))} />
        <BarCard title="Consequence" onPick={(l, add) => byField(l, (v) => v.consequence.replace(/_/g, ' ') === l, add)} color={pmR === 'flame' ? '#8b5cf6' : series[1]} rows={r.summary.consequences.slice(0, 10).map((c) => ({ label: c.consequence.replace(/_/g, ' '), value: c.count }))} />
        <BarCard title="Clinical significance (ClinVar)" onPick={(l, add) => byField(`ClinVar ${l}`, (v) => (v.clinicalSignificance ?? '').replace(/_/g, ' ').split(/,\s*/).includes(l), add)} color={pmR === 'flame' ? '#e11d48' : series[2]} rows={r.summary.clinical.slice(0, 10).map((c) => ({ label: c.significance.replace(/_/g, ' '), value: c.count }))} />
        <BarCard title="Chromosome" onPick={(l, add) => byField(l, (v) => `chr${v.chr}` === l, add)} color={pmR === 'flame' ? '#6366f1' : series[0]} rows={[...r.summary.chromosomes].sort((x, y) => CHR_ORDER.indexOf(x.chromosome) - CHR_ORDER.indexOf(y.chromosome)).map((c) => ({ label: `chr${c.chromosome}`, value: c.count }))} vertical />
      </div>

      {r.phenotypes.length > 0 && <VariantSankey result={r} onPick={pick} />}
      {r.variants.some((v) => v.chr && v.pos) && <GenomeCircos result={r} traitFilter={traitFilter} onTrait={setTraitFilter} selected={shownSel?.ids} onPick={pick} />}

      <TraitsTable traits={r.summary.traits} selected={traitFilter} onSelect={setTraitFilter} />
      <VariantsTable variants={r.variants} phenotypes={r.phenotypes} traitFilter={traitFilter} onClearTrait={() => setTraitFilter(null)} name={a.name} taxid={r.organismTaxId}
        selection={varSel} onClearSelection={() => setVarSel(null)} />
      <GenesPanel result={r} name={a.name} />
    </div>
  )
}

function BarCard({ title, rows, vertical, color = '#f97316', onPick }: { title: string; rows: { label: string; value: number; color?: string }[]; vertical?: boolean; color?: string; onPick?: (label: string, additive: boolean) => void }) {
  if (!rows.length) return null
  const data = vertical
    ? [{ type: 'bar' as const, x: rows.map((r) => r.label), y: rows.map((r) => r.value), marker: { color }, customdata: rows.map((r) => r.label), hovertemplate: '%{x}: %{y} — click to select<extra></extra>' }]
    : [{ type: 'bar' as const, orientation: 'h' as const, y: [...rows].reverse().map((r) => r.label), x: [...rows].reverse().map((r) => r.value),
        customdata: [...rows].reverse().map((r) => r.label),
        marker: { color: [...rows].reverse().map((r) => r.color ?? color) }, hovertemplate: '%{y}: %{x} — click to select<extra></extra>' }]
  return (
    <div className="chart-card">
      <h3>{title}</h3>
      <Plot data={data} height={vertical ? 220 : Math.max(160, 26 * rows.length + 50)}
        onSelect={onPick ? (keys, add) => keys.forEach((k, i) => onPick(k, add || i > 0)) : undefined}
        layout={{ margin: { l: vertical ? 40 : 170, r: 10, t: 6, b: vertical ? 40 : 30 }, yaxis: { automargin: true } }} />
    </div>
  )
}

/** Manhattan plot for GWAS input: −log10 p by chromosome position, genome-wide line at 5e-8. */
function Manhattan({ variants, selected, onSelect }: { variants: VariantRow[]; selected?: Set<string>; onSelect: (ids: string[], additive: boolean) => void }) {
  const series = roleColors(usePaletteMode()).series
  const pts = variants.filter((v) => v.pvalue && v.chr && v.pos)
  const offsets = new Map<string, number>()
  let acc = 0
  CHR_ORDER.forEach((c) => {
    const max = Math.max(0, ...pts.filter((p) => p.chr === c).map((p) => p.pos))
    if (max) { offsets.set(c, acc); acc += max + 5e6 }
  })
  const data = CHR_ORDER.filter((c) => offsets.has(c)).map((c, i) => {
    const sub = pts.filter((p) => p.chr === c)
    return { type: 'scatter' as const, mode: 'markers' as const, name: `chr${c}`, showlegend: false,
      x: sub.map((p) => offsets.get(c)! + p.pos), y: sub.map((p) => -Math.log10(p.pvalue!)),
      text: sub.map((p) => `${p.id} (${p.gene ?? 'intergenic'})<br>chr${p.location}<br>p = ${formatP(p.pvalue)}`), hoverinfo: 'text' as const,
      customdata: sub.map((p) => p.id),
      marker: { size: sub.map((p) => (selected?.has(p.id) ? 12 : 8)), color: series[i % 2], line: { width: sub.map((p) => (selected?.has(p.id) ? 2 : 0)), color: '#111827' } } }
  })
  const ticks = CHR_ORDER.filter((c) => offsets.has(c))
  return (
    <div className="chart-card">
      <h3>Manhattan plot</h3>
      <Plot data={data} height={300} dragmode="lasso" onSelect={onSelect} layout={{
        xaxis: { tickvals: ticks.map((c) => offsets.get(c)! + 1), ticktext: ticks, title: { text: 'Chromosome' }, showgrid: false },
        yaxis: { title: { text: '−log10 p' } }, margin: { l: 50, r: 10, t: 10, b: 50 },
        shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: -Math.log10(5e-8), y1: -Math.log10(5e-8), line: { color: '#dc2626', dash: 'dot', width: 1 } }],
      }} />
      <p className="muted small">Red line: genome-wide significance (p = 5×10⁻⁸).</p>
    </div>
  )
}

/** Sankey: variants → genes → top traits. */
function VariantSankey({ result, onPick }: { result: Result; onPick: (label: string, ids: string[], additive?: boolean) => void }) {
  const [topN, setTopN] = useState(12)
  const plotRef = useRef<PlotHandle | null>(null)
  const rc = roleColors(usePaletteMode())
  const data = useMemo(() => {
    const traits = result.summary.traits.slice(0, topN).map((t) => t.trait.toLowerCase())
    const keep = new Set(traits)
    const links: [string, string][] = []
    const varGenes = new Map(result.variants.map((v) => [v.id, v.gene ?? 'intergenic']))
    result.phenotypes.forEach((p) => {
      if (!keep.has(p.trait.toLowerCase())) return
      const g = varGenes.get(p.variant)
      if (g) links.push([p.variant, `gene:${g}`], [`gene:${g}`, `trait:${p.trait.toLowerCase()}`])
    })
    const counts = new Map<string, number>()
    links.forEach(([s, t]) => counts.set(`${s}|${t}`, (counts.get(`${s}|${t}`) ?? 0) + 1))
    const nodes = [...new Set(links.flat())]
    const idx = new Map(nodes.map((n, i) => [n, i]))
    const label = (n: string) => (n.startsWith('gene:') ? n.slice(5) : n.startsWith('trait:') ? (result.summary.traits.find((t) => t.trait.toLowerCase() === n.slice(6))?.trait ?? n.slice(6)) : n)
    const color = (n: string) => (n.startsWith('gene:') ? rc.gene : n.startsWith('trait:') ? rc.trait : rc.variant)
    const entries = [...counts.entries()]
    return [{
      type: 'sankey' as const, arrangement: 'snap' as const,
      node: { label: nodes.map(label), color: nodes.map(color), pad: 12, thickness: 14, customdata: nodes },
      link: { source: entries.map(([k]) => idx.get(k.split('|')[0])!), target: entries.map(([k]) => idx.get(k.split('|')[1])!),
        value: entries.map(([, v]) => v), color: 'rgba(161,161,170,0.35)' },
    }]
  }, [result, topN, rc])
  return (
    <div className="chart-card">
      <div className="row between"><h3>Variants → genes → traits</h3>
        <label className="small">Top traits <select className="input input-sm" value={topN} onChange={(e) => setTopN(Number(e.target.value))}>{[8, 12, 20, 30].map((n) => <option key={n}>{n}</option>)}</select></label></div>
      <Plot exportName="flame_variants_sankey" data={data as never} height={Math.max(360, topN * 26)} handleRef={plotRef} layout={{ margin: { l: 10, r: 10, t: 10, b: 10 } }}
        onSelect={(keys, add) => keys.forEach((n, i) => {
          const additive = add || i > 0
          if (n.startsWith('gene:')) { const g = n.slice(5); onPick(`variants in ${g}`, result.variants.filter((v) => (v.gene ?? 'intergenic') === g || v.allGenes.split(',').includes(g)).map((v) => v.id), additive) }
          else if (n.startsWith('trait:')) { const t = n.slice(6); onPick(`variants linked to ${t}`, [...new Set(result.phenotypes.filter((p) => p.trait.toLowerCase() === t).map((p) => p.variant))], additive) }
          else onPick(n, [n], additive)
        })} />
      <p className="muted small">Left: variants · middle: genes · right: traits and diseases. Flow width = number of variant–trait associations. Click a node to select its variants.</p>
    </div>
  )
}

function TraitsTable({ traits, selected, onSelect }: { traits: Result['summary']['traits']; selected: string | null; onSelect: (t: string | null) => void }) {
  const [q, setQ] = useState('')
  const rows = traits.filter((t) => t.trait.toLowerCase().includes(q.toLowerCase()))
  if (!traits.length) return <Note kind="info">No trait or disease associations are recorded for these variants.</Note>
  return (
    <div className="chart-card">
      <div className="row between wrap"><h3>Associated traits and diseases ({traits.length})</h3>
        <input className="input input-sm" type="search" placeholder="Search traits" value={q} onChange={(e) => setQ(e.target.value)} /></div>
      <p className="muted small">Click a trait to show only its variants below. Traits shared by several of your variants are listed first.</p>
      <div className="table-scroll">
        <table className="table">
          <thead><tr><th>Trait / disease</th><th>Variants</th><th>Best p-value</th><th>Sources</th></tr></thead>
          <tbody>{rows.slice(0, 300).map((t) => (
            <tr key={t.trait} className={selected === t.trait ? 'selected' : ''} onClick={() => onSelect(selected === t.trait ? null : t.trait)} style={{ cursor: 'pointer' }}>
              <td>{t.trait}</td><td>{t.variants}</td><td>{t.bestP == null ? '—' : t.bestP === 0 ? '< 1e-300' : formatP(t.bestP)}</td><td className="small">{t.sources}</td>
            </tr>))}</tbody>
        </table>
      </div>
    </div>
  )
}

function VariantsTable({ variants, phenotypes, traitFilter, onClearTrait, name, taxid, selection, onClearSelection }: { variants: VariantRow[]; phenotypes: Phenotype[]; traitFilter: string | null; onClearTrait: () => void; name: string; taxid: number; selection: { label: string; ids: Set<string> } | null; onClearSelection: () => void }) {
  const create = useCreateList()
  const [saved, setSaved] = useState<string | null>(null)
  const IMPACT_COLOR = useImpactColors()
  const [impact, setImpact] = useState<'all' | 'HIGH' | 'MODERATE+'>('all')
  const traitVars = traitFilter ? new Set(phenotypes.filter((p) => p.trait === traitFilter).map((p) => p.variant)) : null
  const rows = variants.filter((v) => (!selection || selection.ids.has(v.id)) && (impact === 'all' || (impact === 'HIGH' ? v.impact === 'HIGH' : ['HIGH', 'MODERATE'].includes(v.impact))) && (!traitVars || traitVars.has(v.id)))
  const cols = [{ key: 'id', label: 'Variant' }, { key: 'location', label: 'Location' }, { key: 'alleles', label: 'Alleles' }, { key: 'consequence', label: 'Consequence' },
    { key: 'impact', label: 'Impact' }, { key: 'gene', label: 'Gene' }, { key: 'aminoAcids', label: 'Amino acids' }, { key: 'sift', label: 'SIFT' }, { key: 'polyphen', label: 'PolyPhen' },
    { key: 'gnomadAF', label: 'gnomAD AF' }, { key: 'clinicalSignificance', label: 'ClinVar' }, { key: 'pvalue', label: 'GWAS p' }]
  return (
    <div className="chart-card">
      <div className="row between wrap">
        <h3>Variants ({rows.length})</h3>
        <div className="row wrap">
          {traitFilter && <button className="chip on" onClick={onClearTrait}>{traitFilter} ×</button>}
          {selection && <button className="chip on" onClick={onClearSelection} title="Clear the selection">{selection.label} · {selection.ids.size} ×</button>}
          {selection && (() => {
            const genes = [...new Set(rows.flatMap((v) => v.allGenes.split(',')).filter(Boolean))]
            return genes.length > 0 && (
              <button className="btn btn-secondary btn-sm" disabled={create.isPending} onClick={() => create.mutate(
                { name: `${name} selected genes`, ids: genes, organismTaxId: taxid, uniqueName: true, meta: { method: 'variants', selection: selection.label } },
                { onSuccess: (l) => setSaved(`Saved "${l.name}" (${l.size} genes).`) })}>Save {plural(genes.length, 'gene')} as a list</button>
            )
          })()}
          <Segmented label="Impact filter" value={impact} onChange={setImpact} options={[{ value: 'all', label: 'All' }, { value: 'MODERATE+', label: 'Moderate+' }, { value: 'HIGH', label: 'High' }]} />
          <button className="btn btn-ghost btn-sm" title="Excel workbook: all variants, one sheet per impact, and the variant–trait associations" onClick={() => saveXlsx([
            ...sheetsByCategory(rows.map((v) => ({ Variant: v.id, Input: v.input, Location: v.location, Alleles: v.alleles, Consequence: v.consequence, Impact: v.impact,
              Gene: v.gene ?? '', 'All genes': v.allGenes, Biotype: v.biotype ?? '', 'Amino acids': v.aminoAcids ?? '', SIFT: v.sift ?? '', PolyPhen: v.polyphen ?? '',
              'gnomAD AF': v.gnomadAF ?? '', ClinVar: v.clinicalSignificance ?? '', 'P-value': v.pvalue ?? '' })), 'Impact', 'Variants'),
            { name: 'Traits', rows: phenotypes.map((ph) => ({ Variant: ph.variant, Trait: ph.trait, Source: ph.source, 'Risk allele': ph.riskAllele ?? '', 'P-value': ph.pvalue ?? '', Study: ph.study ?? '' })) },
          ], `${name}_variants.xlsx`)}>Excel</button>
          <button className="btn btn-ghost btn-sm" onClick={() => saveText(toCsv(rows as unknown as Record<string, unknown>[], cols), `${name}_variants.csv`, 'text/csv')}>CSV</button>
        </div>
      </div>
      {saved && <Note kind="success">{saved}</Note>}
      <div className="table-scroll">
        <table className="table">
          <thead><tr>{cols.map((c) => <th key={c.key}>{c.label}</th>)}</tr></thead>
          <tbody>{rows.map((v) => (
            <tr key={`${v.input}-${v.id}`}>
              <td><LinkMenu label={v.id} links={variantLinks(v, taxid)} title="Open this variant in genome browsers and variant databases" /></td>
              <td>{v.location}</td><td>{v.alleles}</td><td>{v.consequence.replace(/_/g, ' ')}</td>
              <td><span className="impact" style={{ background: IMPACT_COLOR[v.impact] }}>{v.impact}</span></td>
              <td>{v.gene ? <LinkMenu label={v.gene} links={geneLinks(v.gene, taxid)} /> : '—'}</td><td>{v.aminoAcids ?? ''}</td><td className="small">{v.sift ?? ''}</td><td className="small">{v.polyphen ?? ''}</td>
              <td>{v.gnomadAF != null ? v.gnomadAF.toPrecision(2) : '—'}</td><td className="small truncate" title={v.clinicalSignificance ?? ''}>{v.clinicalSignificance?.replace(/_/g, ' ') ?? ''}</td>
              <td>{v.pvalue != null ? formatP(v.pvalue) : ''}</td>
            </tr>))}</tbody>
        </table>
      </div>
    </div>
  )
}

function GenesPanel({ result, name }: { result: Result; name: string }) {
  const IMPACT_COLOR = useImpactColors()
  const ws = getWorkspaceId()!
  const create = useCreateList()
  const [rule, setRule] = useState<'moderate' | 'all' | 'gws'>(result.mode === 'gwas' ? 'gws' : 'moderate')
  const [done, setDone] = useState<string | null>(null)
  const genes = result.genes.filter((g) => rule === 'all' || (rule === 'moderate' ? ['HIGH', 'MODERATE'].includes(g.worstImpact) : (g.bestP ?? 1) <= 5e-8))
  const scored = result.genes.filter((g) => g.score != null && g.gene)
  return (
    <div className="chart-card">
      <h3>From variants to genes</h3>
      <p className="muted small">Use the affected genes as a list for functional enrichment{result.mode === 'gwas' ? ', or rank every gene by its strongest variant (−log10 p) and run GSEA' : ''}.</p>
      <div className="row wrap">
        <Segmented label="Which genes" value={rule} onChange={setRule} options={[
          { value: 'moderate', label: 'High/moderate impact' },
          ...(result.mode === 'gwas' ? [{ value: 'gws' as const, label: 'Genome-wide significant (p ≤ 5e-8)' }] : []),
          { value: 'all', label: 'All affected genes' }]} />
        <button className="btn btn-primary btn-sm" disabled={!genes.length || create.isPending} onClick={() =>
          create.mutate({ name: `${name} genes`, ids: genes.map((g) => g.gene), organismTaxId: result.organismTaxId, uniqueName: true, meta: { method: 'variants' } },
            { onSuccess: (l) => setDone(`Saved "${l.name}" (${l.size} genes).`) })}>
          Save {plural(genes.length, 'gene')} as a list
        </button>
        {result.mode === 'gwas' && scored.length >= 15 && (
          <button className="btn btn-secondary btn-sm" disabled={create.isPending} onClick={() =>
            create.mutate({ name: `${name} gene scores`, ids: scored.map((g) => g.gene), scores: scored.map((g) => g.score!), organismTaxId: result.organismTaxId, uniqueName: true, meta: { method: 'gwas-gene-scores' } },
              { onSuccess: (l) => setDone(`Saved the ranked list "${l.name}" (${l.size} genes) — run GSEA on it.`) })}>
            Save gene scores as ranked list (GSEA)
          </button>
        )}
      </div>
      {result.mode === 'gwas' && scored.length < 15 && <p className="muted small">A ranked list for GSEA needs at least 15 genes; upload more GWAS rows (e.g. all SNPs with p &lt; 10⁻³).</p>}
      <ErrorNote error={create.error} />
      {done && <Note kind="success">{done} <Link to={`/w/${ws}`}>Go to the workspace to run enrichment →</Link></Note>}
      <div className="table-scroll small-table">
        <table className="table">
          <thead><tr><th>Gene</th><th>Variants</th><th>Worst impact</th>{result.mode === 'gwas' && <th>Best p</th>}</tr></thead>
          <tbody>{genes.map((g) => (
            <tr key={g.gene}><td><LinkMenu label={g.gene} links={geneLinks(g.gene, result.organismTaxId)} /></td><td>{g.variants}</td><td><span className="impact" style={{ background: IMPACT_COLOR[g.worstImpact] }}>{g.worstImpact}</span></td>
              {result.mode === 'gwas' && <td>{g.bestP != null ? formatP(g.bestP) : '—'}</td>}</tr>))}</tbody>
        </table>
      </div>
    </div>
  )
}

/**
 * Genome Circos: chromosomes as sectors, variants in genomic order, outer bars =
 * −log10 p (GWAS) or impact; bundled links join variants associated with the
 * same trait (the most frequent traits, one colour each).
 */
function GenomeCircos({ result, traitFilter, onTrait, selected, onPick }: { result: Result; traitFilter: string | null; onTrait: (t: string | null) => void; selected?: Set<string>; onPick: (label: string, ids: string[], additive?: boolean) => void }) {
  const pm = usePaletteMode()
  const IMPACT_COLOR = useImpactColors()
  const svgRef = useRef<SVGSVGElement | null>(null)
  const [nTraits, setNTraits] = useState(8)
  const gwas = result.mode === 'gwas'
  const { sectors, items, links, traits } = useMemo(() => {
    const vs = result.variants.filter((v) => v.chr && v.pos && CHR_ORDER.includes(v.chr))
      .sort((a, b) => CHR_ORDER.indexOf(a.chr) - CHR_ORDER.indexOf(b.chr) || a.pos - b.pos)
    const ids = new Set(vs.map((v) => v.id))
    const chrs = CHR_ORDER.filter((c) => vs.some((v) => v.chr === c))
    const shade = pm === 'gray' ? ['#525252', '#a3a3a3'] : ['#6366f1', '#a78bfa']
    const sectors = chrs.map((c, i) => ({ id: c, label: `chr${c}`, color: shade[i % 2] }))
    const rank: Record<string, number> = { HIGH: 4, MODERATE: 3, LOW: 2, MODIFIER: 1 }
    const maxL = Math.max(1, ...vs.map((v) => (v.pvalue ? -Math.log10(v.pvalue) : 0)))
    const items = vs.map((v) => ({
      id: v.id, sector: v.chr, label: v.gene ? `${v.id} (${v.gene})` : v.id, color: IMPACT_COLOR[v.impact] ?? '#a1a1aa',
      value: gwas ? (v.pvalue ? -Math.log10(v.pvalue) / maxL : 0) : (rank[v.impact] ?? 1) / 4,
      title: `${v.id} · chr${v.location}\n${v.consequence.replace(/_/g, ' ')} · ${v.impact}${v.gene ? ` · ${v.gene}` : ''}${v.pvalue ? ` · p = ${formatP(v.pvalue)}` : ''}`,
    }))
    const palette = categorical(pm)
    const top = (traitFilter ? [{ trait: traitFilter }] : result.summary.traits.slice(0, nTraits)).map((t, i) => ({ trait: t.trait, color: palette[i % palette.length] }))
    const links = top.flatMap(({ trait, color }) => {
      const vars = [...new Set(result.phenotypes.filter((p) => p.trait === trait && ids.has(p.variant)).map((p) => p.variant))]
        .sort((a, b) => items.findIndex((x) => x.id === a) - items.findIndex((x) => x.id === b))
      // Small groups: every pair; large groups: a chain in genomic order (keeps the link count linear).
      const pairs = vars.length <= 8 ? vars.flatMap((a, i) => vars.slice(i + 1).map((b) => [a, b])) : vars.slice(1).map((b, i) => [vars[i], b])
      return pairs.map(([a, b]) => ({ source: a, target: b, color, title: `${trait}: ${a} — ${b}` }))
    })
    return { sectors, items, links, traits: top }
  }, [result, pm, IMPACT_COLOR, gwas, nTraits, traitFilter])
  return (
    <div className="chart-card">
      <div className="row between wrap">
        <h3>Genome Circos</h3>
        <span className="row wrap">
          {!traitFilter && (
            <label className="small">Traits <select className="input input-sm" value={nTraits} onChange={(e) => setNTraits(Number(e.target.value))}>
              {[4, 8, 12, 20].map((n) => <option key={n} value={n}>top {n}</option>)}</select></label>
          )}
        </span>
      </div>
      <p className="muted small">Chromosomes around the circle with your variants in genomic order (dots coloured by impact). Outer bars: {gwas ? '−log10 p' : 'impact (HIGH tallest)'}. Bundled links join variants associated with the same trait{traitFilter ? '' : '; click a trait to focus on it'}.</p>
      {items.length < 2 ? <Note kind="info">Not enough variants with genomic positions.</Note> : (
        <CircosBundle sectors={sectors} items={items} links={links} svgRef={svgRef} exportName="flame_genome_circos" size={680} defaultLabels={items.length <= 60} selected={selected}
          onSelect={(ids, add) => onPick(ids.length === 1 ? ids[0] : `${ids.length} variants on the Circos`, ids, add)}
          trackLabel={gwas ? '−log10 p' : 'impact'} linkLabel="shared-trait links" />
      )}
      <div className="legend">
        {traits.map((t) => (
          <button key={t.trait} type="button" className={`legend-btn${traitFilter === t.trait ? ' on' : ''}`} onClick={() => onTrait(traitFilter === t.trait ? null : t.trait)}
            title={traitFilter === t.trait ? 'Show all traits again' : 'Focus on this trait (also filters the variants table)'}>
            <i style={{ background: t.color }} />{t.trait.length > 40 ? `${t.trait.slice(0, 39)}…` : t.trait}
          </button>
        ))}
        {traitFilter && <button type="button" className="legend-btn" onClick={() => onTrait(null)}>Show all traits</button>}
      </div>
    </div>
  )
}
