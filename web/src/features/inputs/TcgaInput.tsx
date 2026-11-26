import { useEffect, useMemo, useState } from 'react'
import { api } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { BusyBar, ErrorNote, Field, Modal, Note, Segmented } from '../../components/ui'
import { GenePicker, type GeneResult } from './FetchInput'
import { parseMaf } from '../../lib/maf'

interface Study { id: string; code: string; name: string; samples: number; /** Adjacent normal tissue samples in the tumour-vs-normal table (0: not available). */ normals?: number }
interface Attribute { id: string; name: string; patient: boolean; values: { value: string; count: number }[] }
type Query = 'mutated' | 'cna' | 'coexpression' | 'de' | 'normal' | 'maf'
interface Group { type: 'clinical' | 'mutation'; attribute: string; values: string[]; gene: string; status: 'mutant' | 'wildtype' }

const QUERIES: { value: Query; label: string }[] = [
  { value: 'mutated', label: 'Mutated genes' },
  { value: 'cna', label: 'Copy-number changes' },
  { value: 'coexpression', label: 'Co-expressed with a gene' },
  { value: 'de', label: 'Compare two groups' },
  { value: 'normal', label: 'Tumour vs normal' },
  { value: 'maf', label: 'Import a MAF file' },
]

/**
 * Gene lists from TCGA (PanCancer Atlas, 32 cancer types, through the cBioPortal API): the most
 * mutated, amplified or deleted genes, genes co-expressed with a gene, genes differentially
 * expressed between two groups of tumours, or the mutated genes of a MAF file (read in the browser).
 */
export function TcgaInput({ title, example, onClose, onCreated }: { title: string; example?: boolean; onClose: () => void; onCreated: (l: AnalyteList) => void }) {
  const [studies, setStudies] = useState<Study[] | null>(null)
  const [study, setStudy] = useState('brca_tcga_pan_can_atlas_2018')
  const [query, setQuery] = useState<Query>(example ? 'de' : 'mutated')
  const [busy, setBusy] = useState<string | null>(null)
  const [err, setErr] = useState<unknown>(null)
  const [result, setResult] = useState<GeneResult | null>(null)
  const [cnaDir, setCnaDir] = useState<'amp' | 'del'>('amp')
  const [gene, setGene] = useState('ESR1')
  const [coDir, setCoDir] = useState<'positive' | 'negative' | 'both'>('positive')
  const [minCor, setMinCor] = useState(0.4)
  const [attrs, setAttrs] = useState<Attribute[] | null>(null)
  const [groupA, setGroupA] = useState<Group>({ type: 'clinical', attribute: '', values: [], gene: 'TP53', status: 'mutant' })
  const [groupB, setGroupB] = useState<Group>({ type: 'clinical', attribute: '', values: [], gene: 'TP53', status: 'wildtype' })
  const [deDir, setDeDir] = useState<'both' | 'up' | 'down'>('both')
  const [minFC, setMinFC] = useState(1)
  const [maxQ, setMaxQ] = useState(0.05)
  const [deAll, setDeAll] = useState(false)

  const call = async <T,>(label: string, path: string, body?: Record<string, unknown>) => {
    setBusy(label); setErr(null)
    try { return await api<T>(path, body ? { body } : {}) } catch (e) { setErr(e); return null } finally { setBusy(null) }
  }
  useEffect(() => { call<{ studies: Study[] }>('Loading the TCGA cancer types', '/inputs/tcga/studies').then((r) => r && setStudies(r.studies)) // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])
  // Clinical attributes of the chosen cancer type (for "compare two groups"); default: its first two subtypes.
  useEffect(() => {
    if (query !== 'de') return
    setAttrs(null)
    call<{ attributes: Attribute[] }>('Loading the clinical groups', '/inputs/tcga/groups', { study }).then((r) => {
      if (!r) return
      setAttrs(r.attributes)
      const a = r.attributes[0]
      if (a) {
        // The two largest groups; in the example, basal-like vs luminal A breast tumours.
        const has = (v: string) => a.values.some((x) => x.value === v)
        const [va, vb] = example && has('BRCA_Basal') && has('BRCA_LumA') ? ['BRCA_Basal', 'BRCA_LumA'] : [a.values[0]?.value, a.values[1]?.value]
        setGroupA((g) => ({ ...g, attribute: a.id, values: [va].filter(Boolean) as string[] }))
        setGroupB((g) => ({ ...g, attribute: a.id, values: [vb].filter(Boolean) as string[] }))
      }
    }) // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [study, query])
  useEffect(() => { setResult(null) }, [study, query])
  // Tumour vs normal exists only for the cancer types with enough normal tissue samples.
  const normalStudies = studies?.filter((s) => (s.normals ?? 0) > 0) ?? []
  useEffect(() => {
    if (query === 'normal' && normalStudies.length && !normalStudies.some((s) => s.id === study)) setStudy(normalStudies[0].id) // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [query, studies])

  const st = studies?.find((s) => s.id === study)
  const run = async () => {
    setResult(null)
    const base = { study }
    let r: GeneResult | null = null
    if (query === 'mutated') r = await call<GeneResult>(`Counting mutated genes in TCGA ${st?.code ?? ''}`, '/inputs/tcga/genes', { ...base, query })
    if (query === 'cna') r = await call<GeneResult>(`Counting ${cnaDir === 'amp' ? 'amplified' : 'deleted'} genes in TCGA ${st?.code ?? ''}`, '/inputs/tcga/genes', { ...base, query, direction: cnaDir })
    if (query === 'coexpression') r = await call<GeneResult>(`Correlating ${gene} with every gene across the tumours`, '/inputs/tcga/genes', { ...base, query, gene, direction: coDir, minCorrelation: minCor })
    if (query === 'de') {
      const g = (x: Group) => (x.type === 'mutation' ? { type: 'mutation', gene: x.gene, status: x.status } : { type: 'clinical', attribute: x.attribute, values: x.values })
      r = await call<GeneResult>('Comparing the expression of every gene between the two groups', '/inputs/tcga/genes',
        { ...base, query, groupA: g(groupA), groupB: g(groupB), direction: deDir, minLog2FC: minFC, maxQ, all: deAll })
    }
    if (query === 'normal') r = await call<GeneResult>(`Loading tumour vs normal genes for TCGA ${st?.code ?? ''}`, '/inputs/tcga/genes',
      { ...base, query, direction: deDir, minLog2FC: minFC, maxQ, all: deAll })
    if (r) setResult({ ...r, defaultTop: query === 'coexpression' || query === 'de' || query === 'normal' ? 500 : 200 })
  }
  useEffect(() => { if (example && query === 'de' && attrs && !result && !busy) run() // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [attrs])

  const ready = query === 'coexpression' ? gene.trim().length > 0
    : query === 'de' ? groupReady(groupA) && groupReady(groupB) : true
  return (
    <Modal title={title} size="xl" onClose={onClose}
      subtitle="Genes from The Cancer Genome Atlas (PanCancer Atlas, 32 cancer types) through cBioPortal, or from your own MAF file.">
      <div className="stack">
        <Segmented label="What to get" value={query} options={QUERIES} onChange={setQuery} />
        {query !== 'maf' && (
          <Field label="Cancer type">
            <select className="input" value={study} onChange={(e) => setStudy(e.target.value)} disabled={!studies}>
              {(query === 'normal' && normalStudies.length ? normalStudies : studies ?? [{ id: study, code: 'BRCA', name: 'Breast Invasive Carcinoma', samples: 0 }]).map((s) => (
                <option key={s.id} value={s.id}>{s.name} ({s.code}){query === 'normal' && s.normals ? ` · ${s.normals} normal samples` : s.samples ? ` · ${s.samples.toLocaleString()} samples` : ''}</option>
              ))}
            </select>
          </Field>
        )}
        {query === 'mutated' && <p className="muted small">Genes ranked by the share of sequenced tumours with a somatic mutation in them (protein-changing and splice mutations, as cBioPortal counts them). Long genes such as TTN and MUC16 are often mutated by chance.</p>}
        {query === 'cna' && (
          <>
            <Segmented label="Change" value={cnaDir} onChange={setCnaDir} options={[{ value: 'amp', label: 'Amplified' }, { value: 'del', label: 'Deeply deleted' }]} />
            <p className="muted small">Genes ranked by the share of tumours with a high-level amplification or a deep (likely two-copy) deletion (GISTIC calls).</p>
          </>
        )}
        {query === 'coexpression' && (
          <div className="row wrap">
            <Field label="Gene"><input className="input input-narrow" value={gene} onChange={(e) => setGene(e.target.value)} placeholder="e.g. ESR1" /></Field>
            <Field label="Correlation">
              <select className="input" value={coDir} onChange={(e) => setCoDir(e.target.value as typeof coDir)}>
                <option value="positive">Positive</option><option value="negative">Negative</option><option value="both">Both</option>
              </select>
            </Field>
            <Field label="|Spearman ρ| at least"><input className="input input-narrow" type="number" min={0.1} max={0.95} step={0.05} value={minCor} onChange={(e) => setMinCor(Number(e.target.value))} /></Field>
            <p className="muted small">Spearman correlation of RNA-seq expression across the tumours of this cancer type; genes with q ≤ 0.05.</p>
          </div>
        )}
        {query === 'de' && (
          <>
            {!attrs && !busy && <Note kind="info">Loading the clinical groups…</Note>}
            {attrs && (
              <div className="tcga-groups">
                <GroupEditor title="Group A" g={groupA} setG={setGroupA} attrs={attrs} />
                <span className="tcga-vs">vs</span>
                <GroupEditor title="Group B" g={groupB} setG={setGroupB} attrs={attrs} />
              </div>
            )}
            <div className="row wrap">
              <label className="check small"><input type="checkbox" checked={deAll} onChange={(e) => setDeAll(e.target.checked)} /> Every gene, ranked by fold change (a ranked list for GSEA)</label>
            </div>
            {!deAll && (
              <div className="row wrap">
                <Field label="Direction">
                  <select className="input" value={deDir} onChange={(e) => setDeDir(e.target.value as typeof deDir)}>
                    <option value="both">Up and down</option><option value="up">Higher in group A</option><option value="down">Lower in group A</option>
                  </select>
                </Field>
                <Field label="|log2 fold change| at least"><input className="input input-narrow" type="number" min={0} step={0.25} value={minFC} onChange={(e) => setMinFC(Number(e.target.value))} /></Field>
                <Field label="q-value at most"><input className="input input-narrow" type="number" min={0} max={1} step={0.01} value={maxQ} onChange={(e) => setMaxQ(Number(e.target.value))} /></Field>
              </div>
            )}
            <p className="muted small">Mean log2 RNA-seq expression of group A minus group B; p-values from cBioPortal (t-test), q-values by Benjamini–Hochberg. Tumours in both groups are left out.</p>
          </>
        )}
        {query === 'normal' && (
          <>
            <div className="row wrap">
              <label className="check small"><input type="checkbox" checked={deAll} onChange={(e) => setDeAll(e.target.checked)} /> Every gene, ranked by fold change (a ranked list for GSEA)</label>
            </div>
            {!deAll && (
              <div className="row wrap">
                <Field label="Direction">
                  <select className="input" value={deDir} onChange={(e) => setDeDir(e.target.value as typeof deDir)}>
                    <option value="both">Up and down</option><option value="up">Higher in tumours</option><option value="down">Lower in tumours</option>
                  </select>
                </Field>
                <Field label="|log2 fold change| at least"><input className="input input-narrow" type="number" min={0} step={0.25} value={minFC} onChange={(e) => setMinFC(Number(e.target.value))} /></Field>
                <Field label="q-value at most"><input className="input input-narrow" type="number" min={0} max={1} step={0.01} value={maxQ} onChange={(e) => setMaxQ(Number(e.target.value))} /></Field>
              </div>
            )}
            <p className="muted small">Primary tumours vs adjacent normal tissue from the same TCGA project (GDC STAR counts, limma-voom, Benjamini–Hochberg q-values), precomputed.
              Only the {normalStudies.length || 16} cancer types with at least 9 normal samples are listed; the others have (almost) none in TCGA.</p>
          </>
        )}
        {query === 'maf' ? <MafImport onResult={setResult} setErr={setErr} /> : (
          <div className="row"><button className="btn btn-primary" disabled={!ready || !!busy} onClick={run}>Get genes</button></div>
        )}
        {busy && <BusyBar label={busy} expect={query === 'de' ? 20 : 8} />}
        <ErrorNote error={err} />
        {result && (
          <>
            {result.note && <Note kind="info">{result.note}</Note>}
            {(result as GeneResult & { groups?: { a: { label: string; n: number }; b: { label: string; n: number } } }).groups && (() => {
              const g = (result as GeneResult & { groups: { a: { label: string; n: number }; b: { label: string; n: number } } }).groups
              return <p className="small">Group A: <strong>{g.a.label}</strong> ({g.a.n} tumours) · group B: <strong>{g.b.label}</strong> ({g.b.n} tumours)</p>
            })()}
            <GenePicker r={result} mode={query === 'maf' ? 'maf' : 'tcga'} onCreated={onCreated} />
          </>
        )}
      </div>
    </Modal>
  )
}

const groupReady = (g: Group) => (g.type === 'mutation' ? g.gene.trim().length > 0 : !!g.attribute && g.values.length > 0)

function GroupEditor({ title, g, setG, attrs }: { title: string; g: Group; setG: (f: (g: Group) => Group) => void; attrs: Attribute[] }) {
  const attr = attrs.find((a) => a.id === g.attribute)
  return (
    <fieldset className="tcga-group">
      <legend>{title}</legend>
      <Segmented label={`${title}: defined by`} value={g.type} onChange={(t) => setG((x) => ({ ...x, type: t }))}
        options={[{ value: 'clinical', label: 'Clinical data' }, { value: 'mutation', label: 'Mutation in a gene' }]} />
      {g.type === 'clinical' ? (
        <>
          <select className="input input-sm" value={g.attribute} aria-label={`${title}: clinical attribute`}
            onChange={(e) => setG((x) => ({ ...x, attribute: e.target.value, values: [] }))}>
            {attrs.map((a) => <option key={a.id} value={a.id}>{a.name}</option>)}
          </select>
          <div className="tcga-values">
            {(attr?.values ?? []).map((v) => (
              <label key={v.value} className="check small">
                <input type="checkbox" checked={g.values.includes(v.value)}
                  onChange={(e) => setG((x) => ({ ...x, values: e.target.checked ? [...x.values, v.value] : x.values.filter((y) => y !== v.value) }))} />
                {v.value} <span className="muted">({v.count})</span>
              </label>
            ))}
          </div>
        </>
      ) : (
        <div className="row wrap">
          <input className="input input-sm input-narrow" value={g.gene} aria-label={`${title}: gene`} placeholder="e.g. TP53" onChange={(e) => setG((x) => ({ ...x, gene: e.target.value }))} />
          <select className="input input-sm" value={g.status} aria-label={`${title}: mutation status`} onChange={(e) => setG((x) => ({ ...x, status: e.target.value as Group['status'] }))}>
            <option value="mutant">mutant tumours</option><option value="wildtype">wild-type tumours</option>
          </select>
        </div>
      )}
    </fieldset>
  )
}

// ---- MAF import (read in the browser; nothing is uploaded) --------------------------------

const MAF_MAX_BYTES = 300 * 1024 * 1024   // a whole-cohort MAF is far smaller; larger files would exhaust the tab

async function readText(file: File): Promise<string> {
  if (!/\.gz$/i.test(file.name)) {
    if (file.size > MAF_MAX_BYTES) throw new Error(`The file is larger than ${MAF_MAX_BYTES / 1024 / 1024} MB.`)
    return file.text()
  }
  // Decompress as a stream and stop at the limit, so a small .gz cannot expand into gigabytes.
  const reader = file.stream().pipeThrough(new DecompressionStream('gzip')).getReader()
  const parts: Uint8Array[] = []
  let total = 0
  for (;;) {
    const { done, value } = await reader.read()
    if (done) break
    total += value.byteLength
    if (total > MAF_MAX_BYTES) { await reader.cancel(); throw new Error(`The decompressed file is larger than ${MAF_MAX_BYTES / 1024 / 1024} MB.`) }
    parts.push(value)
  }
  return new TextDecoder().decode(await new Blob(parts as BlobPart[]).arrayBuffer())
}

function MafImport({ onResult, setErr }: { onResult: (r: GeneResult | null) => void; setErr: (e: unknown) => void }) {
  const [text, setText] = useState<string | null>(null)
  const [fileName, setFileName] = useState('')
  const [includeSilent, setIncludeSilent] = useState(false)
  const parsed = useMemo(() => {
    if (!text) return null
    try { return parseMaf(text, includeSilent) } catch (e) { return e as Error }
  }, [text, includeSilent])
  useEffect(() => {
    if (!parsed) { onResult(null); return }
    if (parsed instanceof Error) { setErr(parsed); onResult(null); return }
    setErr(null)
    onResult({ genes: parsed.genes, organismTaxId: 9606, ranked: true, valueLabel: '% of samples mutated', defaultTop: 200,
      source: `MAF file ${fileName}`, term: `Mutated genes (${fileName.replace(/\.(maf|txt|tsv)(\.gz)?$/i, '')})`,
      citation: `${parsed.samples.toLocaleString()} samples, ${parsed.mutations.toLocaleString()} ${includeSilent ? '' : 'protein-changing '}mutations, read in your browser (not uploaded).` })
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [parsed])
  return (
    <div className="stack-sm">
      <p className="muted small">A Mutation Annotation Format file, e.g. a masked somatic mutation MAF from the GDC Data Portal (portal.gdc.cancer.gov) or a cBioPortal download; .maf, .txt or .gz. Genes are ranked by the share of samples with a mutation. The file is read in your browser and not uploaded.</p>
      <input type="file" accept=".maf,.txt,.tsv,.gz" aria-label="MAF file" onChange={async (e) => {
        const f = e.target.files?.[0]
        if (!f) return
        setFileName(f.name)
        try { setText(await readText(f)) } catch (x) { setErr(x) }
      }} />
      <label className="check small"><input type="checkbox" checked={includeSilent} onChange={(e) => setIncludeSilent(e.target.checked)} /> Include silent, intronic, UTR and other non-protein-changing mutations</label>
    </div>
  )
}
