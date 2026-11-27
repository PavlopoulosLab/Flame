import { useMemo, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { API_BASE, api, getWorkspaceId, saveText } from '../../api/client'
import type { AnalyteList } from '../../api/types'
import { NetworkGraph, Plot } from '../../components/charts'
import { ConceptIcon } from '../../components/Icons'
import { ProgressBar } from '../../components/Progress'
import { CopyButton, EmptyState, ErrorNote, Field, GeneCardsLink, GeneChips, GeneList, Modal, Note, OrganismPicker, Segmented } from '../../components/ui'
import { formatP, plural, toCsv } from '../../lib/format'
import { categorical, PaletteScope, usePaletteKey, usePaletteMode } from '../../lib/palette'
import { SaveDerived } from '../utilities/QuickActions'
import { saveXlsx, sheetsByCategory } from '../../lib/xlsx'
import {
  startTask, TASK_INFO, useTask, useTasks, type DrugResult, type DrugRow, type ModulesResult, type RegulatorResult,
  type RobustnessResult, type TaskType,
} from './tasks'
import { usePlainSelection } from '../../lib/imageExport'
const NO_IDS: string[] = []

type Tile = TaskType | 'ppi'
const TILES: Tile[] = ['tf', 'kinase', 'drugs', 'ppi', 'robustness']
const PPI_INFO = { title: 'Protein network and modules', icon: 'ppi', tool: 'STRING', text: 'Known and predicted interactions among your proteins; find network modules and what each one does; select and save sub-networks.' }
const ROBUST_SOURCES = ['GO:BP', 'GO:MF', 'GO:CC', 'KEGG', 'REAC', 'WP', 'HALLMARK']

/**
 * "Regulators & drugs" section: analyses that go beyond term enrichment —
 * upstream regulators, drug signatures, cell types, network
 * modules and robustness checks. Each runs in the background with progress.
 */
export function AnalysesPanel({ lists, onPpi, onAddList, open, setOpen, onExample }: {
  lists: AnalyteList[]; onPpi: () => void; onAddList: () => void; open: string | null; setOpen: (id: string | null) => void; onExample: (kind: string) => Promise<void> | void
}) {
  const tasks = useTasks()
  const [dialog, setDialog] = useState<TaskType | null>(null)
  const [exBusy, setExBusy] = useState<string | null>(null)
  const items = tasks.data ?? []
  return (
    <section className="sec-panel sec-regulators" aria-label="Regulators, drugs and more">
      <div className="page-header">
        <div><h1>Regulators, drugs and more</h1><p className="muted">Ask what controls your genes, which compounds could reverse a response, how your proteins interact, and how solid the enrichment is.</p></div>
      </div>
      <div className="tool-grid-ws">
        {TILES.map((t) => {
          const info = t === 'ppi' ? PPI_INFO : TASK_INFO[t]
          const needsList = t !== 'metabolites'
          return (
            <div key={t} className="tool-tile-wrap">
              <button type="button" className="tool-tile" disabled={needsList && !lists.length}
                onClick={() => (t === 'ppi' ? onPpi() : setDialog(t))}>
                <ConceptIcon name={info.icon} size={36} />
                <strong>{info.title}</strong>
                <span className="muted small">{info.text}</span>
                <span className="tile-tool small">{info.tool}</span>
              </button>
              <button type="button" className="link-btn small tile-example" disabled={!!exBusy}
                onClick={async () => { setExBusy(t); try { await onExample(t) } finally { setExBusy(null) } }}>
                {exBusy === t ? 'Loading example…' : 'Try an example →'}
              </button>
            </div>
          )
        })}
      </div>
      {!lists.length && <p className="muted small">Most analyses start from one of your lists. <button className="link-btn" onClick={onAddList}>Add a list</button> first, or use <em>Try an example</em> under any analysis.</p>}

      <h2 className="sec-subhead">Your analyses</h2>
      {items.length === 0 ? (
        <EmptyState title="No analyses yet"><p>Pick an analysis above. Results are kept with your workspace and included when you save the session.</p></EmptyState>
      ) : (
        <div className="run-grid">
          {items.map((t) => (
            <article key={t.id} className={`run-card sec-card${t.status === 'error' ? ' error' : ''}`}>
              <header className="run-card-head"><h3 className="run-title"><ConceptIcon name={TASK_INFO[t.type]?.icon ?? 'run'} size={20} /> {t.label}</h3>
                <button className="icon-btn" aria-label={`Remove ${t.label}`} title="Remove this analysis"
                  onClick={() => api(`/analyses/${t.id}`, { method: 'DELETE' }).then(() => tasks.refetch())}>×</button></header>
              <p className="muted small">{TASK_INFO[t.type]?.title} · {TASK_INFO[t.type]?.tool} · {new Date(t.started * 1000).toLocaleTimeString()}</p>
              <div className="run-card-foot">
                {t.status === 'running' && <ProgressBar progress={t.progress} status="running" />}
                {t.status === 'error' && <span className="text-danger small">{t.error}</span>}
                {t.status === 'complete' && <button className="btn btn-primary btn-sm" onClick={() => setOpen(t.id)}>Open →</button>}
              </div>
            </article>
          ))}
        </div>
      )}
      {dialog && <StartAnalysisDialog type={dialog} lists={lists} onClose={() => setDialog(null)} onStarted={(id) => { setDialog(null); setOpen(id) }} />}
      {open && <PaletteScope id="analysis"><TaskResultModal id={open} lists={lists} onClose={() => setOpen(null)} /></PaletteScope>}
    </section>
  )
}

// ---------------------------------------------------------------------------
// Start dialog
// ---------------------------------------------------------------------------
function ListSelect({ lists, value, onChange, label, allowNone }: { lists: AnalyteList[]; value: string; onChange: (v: string) => void; label: string; allowNone?: boolean }) {
  return (
    <Field label={label}>
      <select className="input" value={value} onChange={(e) => onChange(e.target.value)}>
        {allowNone && <option value="">— none —</option>}
        {lists.map((l) => <option key={l.id} value={l.id}>{l.name} ({l.size} · {l.organismName})</option>)}
      </select>
    </Field>
  )
}

export function StartAnalysisDialog({ type, lists, initialListId, onClose, onStarted }: {
  type: TaskType; lists: AnalyteList[]; initialListId?: string; onClose: () => void; onStarted: (id: string) => void
}) {
  const info = TASK_INFO[type]
  const guessUp = lists.find((l) => /\bup\b/i.test(l.name))?.id, guessDown = lists.find((l) => /\bdown\b/i.test(l.name))?.id
  const [listId, setListId] = useState(initialListId ?? lists[0]?.id ?? '')
  const [up, setUp] = useState(guessUp ?? lists[0]?.id ?? '')
  const [down, setDown] = useState(guessDown ?? '')
  const [kind, setKind] = useState<'chemical' | 'crispr'>('chemical')
  const [score, setScore] = useState('700')
  const [sources, setSources] = useState<string[]>(['GO:BP', 'KEGG', 'REAC'])
  const [text, setText] = useState('')
  const [name, setName] = useState('Metabolites')
  const [organism, setOrganism] = useState(9606)
  const [busy, setBusy] = useState(false)
  const [err, setErr] = useState<unknown>(null)
  const [source, setSource] = useState<string | null>(null)
  const list = lists.find((l) => l.id === listId)
  const human = (id: string) => !id || lists.find((l) => l.id === id)?.organismTaxId === 9606
  const warn = (type === 'tf' || type === 'kinase') && list && list.organismTaxId !== 9606 ? 'This analysis uses human gene symbols. Map the list to human orthologs first (Gene tools → Find orthologs).'
    : type === 'drugs' && (!human(up) || !human(down)) ? 'LINCS L1000 signatures are human. Map the lists to human orthologs first.'
    : type === 'modules' && list && list.size > 500 ? 'STRING networks are limited to 500 proteins; use a shorter list.' : null
  const loadExample = () => api<{ text: string; source?: string }>('/inputs/examples/metabolites').then((r) => { setText(r.text); setName('Prostate cancer metabolites'); setSource(r.source ?? null) }).catch(setErr)
  const go = () => {
    setBusy(true); setErr(null)
    const body: Record<string, unknown> = { type }
    if (type === 'drugs') Object.assign(body, { upListId: up, downListId: down, kind })
    else if (type === 'metabolites') Object.assign(body, { text, name, organismTaxId: organism })
    else Object.assign(body, { listId })
    if (type === 'modules') body.score = Number(score)
    if (type === 'robustness') body.sources = sources
    startTask(body).then((r) => onStarted(r.id)).catch(setErr).finally(() => setBusy(false))
  }
  const ready = type === 'drugs' ? !!(up || down) && up !== down : type === 'metabolites' ? text.trim().split(/\n/).filter(Boolean).length >= 3 : !!listId
  return (
    <Modal title={info.title} subtitle={info.text} onClose={onClose} size="lg" palette={false}
      footer={<><button className="btn btn-ghost" onClick={onClose}>Cancel</button><button className="btn btn-primary" disabled={!ready || busy || !!warn} onClick={go}>{busy ? 'Starting…' : `Run ${info.tool}`}</button></>}>
      <div className="stack">
        {type === 'drugs' ? (
          <>
            <div className="row wrap">
              <ListSelect lists={lists} value={up} onChange={setUp} label="Up-regulated genes" allowNone />
              <ListSelect lists={lists} value={down} onChange={setDown} label="Down-regulated genes" allowNone />
            </div>
            <Field label="Signatures"><Segmented label="Signature type" value={kind} onChange={setKind} options={[{ value: 'chemical', label: 'Compounds' }, { value: 'crispr', label: 'CRISPR knock-outs' }]} /></Field>
            <p className="muted small">Give both directions for the best result (e.g. the up and down lists from a volcano plot). <strong>Reverse</strong>: perturbations whose signature goes the opposite way, possible treatments. <strong>Mimic</strong>: perturbations that cause a similar response.</p>
          </>
        ) : type === 'metabolites' ? (
          <>
            <Field label="Metabolites or lipids" hint="One per line: names (sarcosine, L-proline), KEGG compound IDs (C00213) or ChEBI IDs (CHEBI:15611).">
              <textarea className="input mono" rows={8} value={text} onChange={(e) => setText(e.target.value)} placeholder={'sarcosine\nuracil\nkynurenine'} />
            </Field>
            <div className="row wrap">
              <button className="btn btn-ghost btn-sm" onClick={loadExample}>Load example</button>
              <a className="btn btn-ghost btn-sm" href={`${API_BASE}/inputs/examples/metabolites/file`}>Example file</a>
            </div>
            {source && <p className="muted small example-source">Example: {source}</p>}
            <div className="row wrap">
              <Field label="Name"><input className="input" value={name} maxLength={80} onChange={(e) => setName(e.target.value)} /></Field>
              <OrganismPicker value={organism} onChange={setOrganism} />
            </div>
            <p className="muted small">Compounds are matched to KEGG, then tested against the organism's KEGG pathways (hypergeometric test, BH). The result opens as an ordinary run, so tables, plots, themes and comparisons all work.</p>
          </>
        ) : (
          <ListSelect lists={lists} value={listId} onChange={setListId} label="List" />
        )}
        {type === 'modules' && (
          <Field label="Minimum STRING score">
            <select className="input" value={score} onChange={(e) => setScore(e.target.value)}>
              <option value="900">Highest (0.900)</option><option value="700">High (0.700)</option><option value="400">Medium (0.400)</option>
            </select>
          </Field>
        )}
        {type === 'robustness' && (
          <Field label="Gene-set collections" hint="MSigDB versions of these databases, tested locally (fast).">
            <div className="row wrap">{ROBUST_SOURCES.map((s) => (
              <label key={s} className="chip-check"><input type="checkbox" checked={sources.includes(s)} onChange={() => setSources((c) => (c.includes(s) ? c.filter((x) => x !== s) : [...c, s]))} /> {s}</label>
            ))}</div>
          </Field>
        )}
        {(type === 'tf' || type === 'kinase') && <p className="muted small">{type === 'tf' ? 'ChEA3 combines ChIP-seq targets (ENCODE, ReMap, literature), co-expression (GTEx, ARCHS4) and TF–gene co-occurrence; the integrated mean rank orders the factors.' : 'KEA3 combines kinase–substrate databases, protein interactions and co-expression; the integrated mean rank orders the kinases.'} Lists with 20–500 genes work best.</p>}
        {warn && <Note kind="warning">{warn}</Note>}
        <ErrorNote error={err} />
      </div>
    </Modal>
  )
}

// ---------------------------------------------------------------------------
// Result modal
// ---------------------------------------------------------------------------
export function TaskResultModal({ id, lists, onClose }: { id: string; lists: AnalyteList[]; onClose: () => void }) {
  const q = useTask<unknown>(id)
  usePaletteKey(q.data?.type ?? '')
  const summary = useTasks().data?.find((t) => t.id === id)
  const navigate = useNavigate()
  const st = q.data
  const info = st ? TASK_INFO[st.type] : null
  const taxid = (st?.type === 'modules' || st?.type === 'robustness') ? lists.find((l) => l.id === summary?.listId)?.organismTaxId : 9606
  return (
    <Modal title={info?.title ?? 'Analysis'} subtitle={info ? `${info.tool} · ${info.text}` : undefined} size="xl" onClose={onClose} palette={st?.status === 'complete'}>
      {!st || st.status === 'running' ? (
        <div className="stack"><ProgressBar progress={st?.progress} status="running" /><p className="muted small">You can close this window; the analysis keeps running and appears under “Your analyses”.</p></div>
      ) : st.status === 'error' ? (
        <ErrorNote error={new Error(st.error ?? 'The analysis failed.')} />
      ) : st.runId ? (
        <div className="stack">
          <Note kind="success">The metabolite enrichment is ready and was added to your enrichment runs.</Note>
          <button className="btn btn-primary" onClick={() => navigate(`/w/${getWorkspaceId()}/runs/${st.runId}`)}>Open results →</button>
        </div>
      ) : st.type === 'tf' || st.type === 'kinase' ? <RegulatorsView r={st.result as RegulatorResult} onDone={onClose} />
        : st.type === 'drugs' ? <DrugsView r={st.result as DrugResult} />
        : st.type === 'modules' ? <ModulesView r={st.result as ModulesResult} taxid={taxid ?? 9606} onDone={onClose} />
        : st.type === 'robustness' ? <RobustnessView r={st.result as RobustnessResult} />
        : null}
    </Modal>
  )
}

function RegulatorsView({ r, onDone }: { r: RegulatorResult; onDone: () => void }) {
  const [sel, setSel] = useState<string[]>([])
  const selV = usePlainSelection(sel, NO_IDS) // none while an image is exported without highlighting
  const [limit, setLimit] = useState(25)
  const top = r.regulators.slice(0, limit)
  const genes = [...new Set(r.regulators.filter((x) => sel.includes(x.name)).flatMap((x) => x.overlap))]
  const what = r.kind === 'tf' ? 'transcription factor' : 'kinase'
  const rows = r.regulators.map((x) => ({ rank: x.rank, name: x.name, score: x.score, libraries: x.libraries.map((l) => `${l.library}:${l.rank}`).join('; '), genes: x.overlap.join(', ') }))
  const cols = ['rank', 'name', 'score', 'libraries', 'genes'].map((k) => ({ key: k, label: k }))
  return (
    <div className="stack">
      <p className="small">{r.note} {plural(r.queried, 'gene')} queried.</p>
      <div className="toolbar">
        <label>Show <select className="input input-sm" value={limit} onChange={(e) => setLimit(Number(e.target.value))}>{[15, 25, 40, 60].map((n) => <option key={n} value={n}>top {n}</option>)}</select></label>
        <span className="muted small">Drag a box over bars (or click) to select {what}s; their target genes from your list appear below.</span>
        <span className="toolbar-spacer" />
        <CopyButton label="Copy names" text={top.map((x) => x.name).join('\n')} />
        <button className="btn btn-ghost btn-sm" onClick={() => saveXlsx([{ name: r.kind === 'tf' ? 'Transcription factors' : 'Kinases', rows: r.regulators.map((x) => ({
          Rank: x.rank, [r.kind === 'tf' ? 'Transcription factor' : 'Kinase']: x.name, 'Mean rank': x.score, 'Genes of your list': x.overlap.length,
          Genes: x.overlap.join(', '), 'Evidence libraries': x.libraries.map((l) => `${l.library} (${l.rank})`).join('; ') })) }], `flame_${r.tool.toLowerCase()}.xlsx`)}>Excel</button>
        <button className="btn btn-ghost btn-sm" onClick={() => saveText(toCsv(rows, cols), `flame_${r.tool.toLowerCase()}.csv`, 'text/csv')}>CSV</button>
      </div>
      <Plot colorKey="flame_analysis_top" height={Math.max(300, top.length * 20 + 80)} onSelect={(ids, add) => setSel((c) => (add ? [...new Set([...c, ...ids])] : ids))} data={[{
        type: 'bar', orientation: 'h', y: top.map((x) => x.name), x: top.map((x) => x.overlap.length), customdata: top.map((x) => x.name),
        marker: { color: top.map((x) => (selV.includes(x.name) ? '#e11d48' : x.score)), colorscale: 'Viridis', reversescale: true, showscale: !selV.length, colorbar: { title: { text: 'mean rank' } } },
        text: top.map((x) => `#${x.rank}`), textposition: 'outside',
        hovertemplate: `%{y}<br>%{x} genes of your list · mean rank %{marker.color}<extra></extra>`,
      } as never]} layout={{ yaxis: { autorange: 'reversed', automargin: true }, xaxis: { title: { text: 'genes of your list with evidence' } }, margin: { l: 90, r: 40, t: 10, b: 50 } }} />
      {sel.length > 0 && (
        <div className="chart-card stack-sm">
          <div className="row wrap between">
            <strong>{sel.join(', ')}</strong>
            <span className="row"><CopyButton label="Copy names" text={sel.join('\n')} /><CopyButton label="Copy target genes" text={genes.join('\n')} /><button className="btn btn-ghost btn-sm" onClick={() => setSel([])}>Clear</button></span>
          </div>
          <p className="small">{plural(genes.length, 'target gene')} in your list (click for GeneCards):</p>
          <GeneChips genes={genes} max={80} />
          <SaveDerived ids={genes} defaultName={`Targets of ${sel.slice(0, 3).join(', ')}${sel.length > 3 ? '…' : ''}`} taxid={9606} idFormat="USERINPUT" meta={{ method: r.tool, regulators: sel }} onDone={onDone} />
        </div>
      )}
      <div className="table-scroll"><table className="table compact">
        <thead><tr><th>Rank</th><th>{r.kind === 'tf' ? 'Transcription factor' : 'Kinase'}</th><th>Mean rank</th><th>Evidence libraries (rank)</th><th>Genes of your list</th></tr></thead>
        <tbody>{top.map((x) => (
          <tr key={x.name} className={`clickable${sel.includes(x.name) ? ' selected' : ''}`} onClick={(e) => setSel((c) => (e.shiftKey ? [...new Set([...c, x.name])] : [x.name]))}>
            <td>{x.rank}</td><td><strong><GeneCardsLink gene={x.name} /></strong></td><td>{x.score}</td>
            <td><span className="row wrap">{x.libraries.map((l) => <span key={l.library} className="src-chip">{l.library} {l.rank}</span>)}</span></td>
            <td className="small">{x.overlap.length} <CopyButton compact title={`Copy the genes of ${x.name}`} text={x.overlap.join('\n')} /></td>
          </tr>
        ))}</tbody>
      </table></div>
      <p className="muted small">{r.citation}</p>
    </div>
  )
}

function DrugsView({ r }: { r: DrugResult }) {
  const [dir, setDir] = useState<'reverse' | 'mimic'>(r.reverse.length ? 'reverse' : 'mimic')
  const [sel, setSel] = useState<string | null>(null)
  const selV = usePlainSelection(sel, null) // none while an image is exported without highlighting
  const rows = (dir === 'reverse' ? r.reverse : r.mimic).slice(0, 30)
  const picked = rows.find((x) => x.agent === sel)
  // Results saved before the server fix can hold a single gene as a bare string: always work with an array.
  const genesIn = (x: DrugRow, d: 'mimic' | 'reverse') => { const g: unknown = d === 'reverse' ? x.genesReverse : x.genesMimic; return Array.isArray(g) ? (g as string[]) : g ? [String(g)] : [] }
  const genesOf = (x: DrugRow) => genesIn(x, dir === 'reverse' ? 'reverse' : 'mimic')
  const exp = rows.map((x) => ({ agent: x.agent, score: x[dir], up_genes_p: dir === 'reverse' ? x.upInDown : x.upInUp, down_genes_p: dir === 'reverse' ? x.downInUp : x.downInDown, genes: genesOf(x).join(', ') }))
  return (
    <div className="stack">
      <div className="toolbar">
        <Segmented label="Direction" value={dir} onChange={(v) => { setDir(v); setSel(null) }} options={[{ value: 'reverse', label: `Reverse (${r.reverse.length})` }, { value: 'mimic', label: `Mimic (${r.mimic.length})` }]} />
        <span className="muted small">{r.library} · {r.upCount} up / {r.downCount} down genes</span>
        <span className="toolbar-spacer" />
        <CopyButton label="Copy names" text={rows.map((x) => x.agent).join('\n')} />
        <button className="btn btn-ghost btn-sm" title="Excel workbook: one sheet for compounds that reverse the signature, one for those that mimic it" onClick={() => saveXlsx(
          (['reverse', 'mimic'] as const).map((d) => ({ name: d === 'reverse' ? 'Reverse' : 'Mimic', rows: r[d].map((x) => ({
            [r.kind === 'crispr' ? 'Knock-out' : 'Compound']: x.agent, Score: x[d],
            'Up genes, adj. p': d === 'reverse' ? x.upInDown ?? '' : x.upInUp ?? '', 'Down genes, adj. p': d === 'reverse' ? x.downInUp ?? '' : x.downInDown ?? '',
            Genes: genesIn(x, d === 'reverse' ? 'reverse' : 'mimic').join(', ') })) })), 'flame_lincs_signatures.xlsx')}>Excel</button>
        <button className="btn btn-ghost btn-sm" onClick={() => saveText(toCsv(exp, Object.keys(exp[0] ?? {}).map((k) => ({ key: k, label: k }))), `flame_lincs_${dir}.csv`, 'text/csv')}>CSV</button>
      </div>
      {rows.length === 0 ? <Note kind="info">No signature {dir === 'reverse' ? 'reverses' : 'mimics'} this response significantly.</Note> : (
        <Plot colorKey="flame_analysis_rows" height={Math.max(300, rows.length * 20 + 80)} onSelect={(ids) => setSel(ids[0] ?? null)} data={[{
          type: 'bar', orientation: 'h', y: rows.map((x) => x.agent), x: rows.map((x) => x[dir]), customdata: rows.map((x) => x.agent),
          marker: { color: rows.map((x) => (x.agent === selV ? '#f97316' : dir === 'reverse' ? '#2563eb' : '#e11d48')) },
          hovertemplate: '%{y}<br>score %{x}<extra></extra>',
        } as never]} layout={{ yaxis: { autorange: 'reversed', automargin: true }, xaxis: { title: { text: `${dir} score (−log10 adj. p, up + down)` } }, margin: { l: 160, r: 20, t: 10, b: 50 } }} />
      )}
      {picked && (
        <div className="chart-card stack-sm">
          <div className="row between wrap"><strong>{picked.agent}</strong><CopyButton label="Copy genes" text={genesOf(picked).join('\n')} /></div>
          <p className="small">
            Up genes: {formatP(dir === 'reverse' ? picked.upInDown : picked.upInUp)} in its {dir === 'reverse' ? 'down' : 'up'} signature ·
            Down genes: {formatP(dir === 'reverse' ? picked.downInUp : picked.downInDown)} in its {dir === 'reverse' ? 'up' : 'down'} signature (adjusted p)
          </p>
          <GeneChips genes={genesOf(picked)} />
        </div>
      )}
      <p className="muted small">{r.note}</p>
      <p className="muted small">{r.citation} These are hypotheses from cell-line signatures, not treatment advice.</p>
    </div>
  )
}

export function ModuleList({ r, colorOf, onPick, picked, taxid, onDone, details = true }: {
  r: ModulesResult; colorOf: (m: number) => string; onPick: (m: number | null) => void; picked: number | null; taxid: number; onDone: () => void; details?: boolean
}) {
  const m = r.modules.find((x) => x.id === picked)
  return (
    <div className="stack-sm">
      <div className="row between wrap">
        <p className="small">{plural(r.modules.length, 'module')} · modularity {r.modularity} · {r.nodes} proteins, {r.edges} interactions. Click a module to select it.</p>
        <button className="btn btn-ghost btn-sm" title="Excel workbook: modules with their proteins, then the enriched terms of every module (all and per source)" onClick={() => saveXlsx([
          { name: 'Modules', rows: r.modules.map((m) => ({ Module: m.id, Label: m.label, Proteins: m.size, Density: m.density, Genes: m.genes.join(', ') })) },
          ...sheetsByCategory(r.modules.flatMap((m) => m.terms.map((t) => ({ Module: m.id, Source: t.source, 'Term ID': t.id, Term: t.name, 'Adjusted p': t.p, 'Genes found': t.size, Genes: t.genes ?? '' }))), 'Source', 'Module terms'),
        ], 'flame_network_modules.xlsx')}>Excel</button>
      </div>
      <div className="module-grid">
        {r.modules.map((x) => (
          <button key={x.id} type="button" className={`module-card${picked === x.id ? ' on' : ''}`} style={{ borderLeftColor: colorOf(x.id) }} onClick={() => onPick(picked === x.id ? null : x.id)}>
            <strong>Module {x.id}: {x.label}</strong>
            <span className="muted small">{x.size} proteins · density {x.density}</span>
            {x.terms.slice(0, 3).map((t) => <span key={t.id} className="small">{t.source} · {t.name} <span className="muted">({formatP(t.p)})</span></span>)}
          </button>
        ))}
      </div>
      {m && m.terms.length > 0 && (
        <div className="chart-card stack-sm">
          <strong>Module {m.id}: enriched terms <span className="count">{m.terms.length}</span></strong>
          <div className="table-scroll module-terms"><table className="table compact">
            <thead><tr><th>Source</th><th>Term</th><th className="num">Adjusted p</th><th className="num">Genes</th></tr></thead>
            <tbody>{m.terms.map((t) => (
              <tr key={`${t.source}:${t.id}`}><td className="small">{t.source}</td><td>{t.name} <span className="muted small">{t.id}</span></td><td className="num">{formatP(t.p)}</td><td className="num">{t.size}</td></tr>
            ))}</tbody>
          </table></div>
        </div>
      )}
      {m && m.terms.length === 0 && <p className="muted small">Module {m.id} has no significantly enriched terms.</p>}
      {m && details && (
        <div className="chart-card stack-sm">
          <div className="row between wrap"><strong>Module {m.id}: {m.genes.length} proteins</strong><CopyButton label="Copy genes" text={m.genes.join('\n')} /></div>
          <GeneChips genes={m.genes} taxid={taxid} />
          <SaveDerived ids={m.genes} defaultName={`Module ${m.id} – ${m.label.slice(0, 40)}`} taxid={taxid} idFormat="USERINPUT" meta={{ method: 'string-module', module: m.id }} onDone={onDone} />
        </div>
      )}
      <p className="muted small">{r.method}</p>
    </div>
  )
}

function ModulesView({ r, taxid, onDone }: { r: ModulesResult; taxid: number; onDone: () => void }) {
  const pm = usePaletteMode()
  const [picked, setPicked] = useState<number | null>(null)
  const pal = categorical(pm)
  const colorOf = (k: number) => pal[(k - 1) % pal.length]
  const graph = useMemo(() => {
    const ids = [...new Set((r.links ?? []).flatMap((e) => [e.from, e.to]))]
    return {
      nodes: ids.map((id) => {
        const k = r.membership[id]
        const inMod = r.modules.some((m) => m.id === k)
        return { id, label: id, color: inMod ? colorOf(k) : '#a1a1aa', shape: 'dot', size: picked && k === picked ? 16 : 10, title: inMod ? `${id} · module ${k}` : `${id} · small cluster` }
      }),
      edges: (r.links ?? []).map((e, i) => ({ id: i, from: e.from, to: e.to, value: e.score, width: 0.5 + 2.5 * e.score, color: r.membership[e.from] === r.membership[e.to] ? colorOf(r.membership[e.from]) : '#d4d4d8' })),
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [r, picked, pm])
  const sel = picked ? r.modules.find((m) => m.id === picked)?.genes ?? [] : []
  return (
    <div className="stack">
      {graph.nodes.length > 0 && <NetworkGraph nodes={graph.nodes} edges={graph.edges} height={440} selected={sel}
        onSelect={(ids) => { const k = r.membership[ids[0]]; setPicked(r.modules.some((m) => m.id === k) ? k : null) }} />}
      <ModuleList r={r} colorOf={colorOf} picked={picked} onPick={setPicked} taxid={taxid} onDone={onDone} />
    </div>
  )
}

function RobustnessView({ r }: { r: RobustnessResult }) {
  const [sel, setSel] = useState<string[]>([])
  const selV = usePlainSelection(sel, NO_IDS) // none while an image is exported without highlighting
  const [only, setOnly] = useState<'all' | 'stable' | 'fragile'>('all')
  const shown = r.terms.filter((t) => (only === 'all' ? true : only === 'stable' ? t.stability >= 0.8 : t.stability < 0.5))
  const exp = r.terms.map((t) => ({ term_id: t.id, term: t.name, source: t.source, padj: t.padj, stability: t.stability, random_rate: t.randomRate, genes: t.genes ?? '' }))
  return (
    <div className="stack">
      <div className="stat-tiles">
        <div className="stat-tile"><strong>{r.significant}</strong><span className="small">significant terms (FDR &lt; 0.05)</span></div>
        <div className="stat-tile"><strong>{r.stable}</strong><span className="small">stable in ≥ 80% of {r.nBoot} subsamples</span></div>
        <div className="stat-tile"><strong>{r.medianRandom}</strong><span className="small">median significant terms in {r.nRandom} random lists</span></div>
        <div className="stat-tile"><strong>{r.query}</strong><span className="small">of your genes in {r.sets.toLocaleString()} gene sets</span></div>
      </div>
      <Note kind={r.significant > 0 && r.medianRandom < r.significant / 10 ? 'success' : 'warning'}>
        {r.significant === 0 ? 'No term is significant for this list with these collections.'
          : r.medianRandom < r.significant / 10 ? `Random lists of the same size give about ${r.medianRandom} significant terms, far fewer than your ${r.significant}: the enrichment is unlikely to be chance.`
          : `Random lists of the same size give about ${r.medianRandom} significant terms — not far from your ${r.significant}. Treat the enrichment with caution (very generic sets, or a list that is close to random).`}
      </Note>
      <div className="two-col-tight">
        <div className="chart-card">
          <h3>Stability of each term</h3>
          <Plot colorKey="flame_stability_terms" height={360} dragmode="lasso" onSelect={(ids, add) => setSel((c) => (add ? [...new Set([...c, ...ids])] : ids))} data={[{
            type: 'scatter', mode: 'markers', x: r.terms.map((t) => t.stability), y: r.terms.map((t) => -Math.log10(Math.max(t.padj, 1e-300))),
            customdata: r.terms.map((t) => t.id), text: r.terms.map((t) => `${t.name} (${t.source})`),
            marker: { size: r.terms.map((t) => (selV.includes(t.id) ? 11 : 7)), color: r.terms.map((t) => t.randomRate), colorscale: [[0, '#16a34a'], [0.2, '#f59e0b'], [1, '#e11d48']], cmin: 0, cmax: 0.5, colorbar: { title: { text: 'random rate' } }, line: { width: r.terms.map((t) => (selV.includes(t.id) ? 1.5 : 0)), color: '#111827' } },
            hovertemplate: '%{text}<br>stability %{x:.0%} · −log10 adj. p %{y:.1f}<extra></extra>',
          } as never]} layout={{ xaxis: { title: { text: 'stability (share of subsamples)' }, range: [-0.05, 1.05], tickformat: '.0%' }, yaxis: { title: { text: '−log10 adjusted p' } },
            shapes: [{ type: 'line', x0: 0.8, x1: 0.8, yref: 'paper', y0: 0, y1: 1, line: { dash: 'dot', color: '#16a34a' } }] }} />
        </div>
        <div className="chart-card">
          <h3>Your list vs random lists</h3>
          <Plot colorKey="flame_stability_random" height={360} data={[{ type: 'histogram', x: r.randomSignificant, name: 'random lists', marker: { color: '#a1a1aa' }, hovertemplate: '%{x} terms: %{y} lists<extra></extra>' } as never]}
            layout={{ xaxis: { title: { text: 'significant terms' }, range: [0, Math.max(r.significant, ...r.randomSignificant) * 1.1 + 1] }, yaxis: { title: { text: 'random lists' } }, showlegend: false,
              shapes: [{ type: 'line', x0: r.significant, x1: r.significant, yref: 'paper', y0: 0, y1: 1, line: { color: '#e11d48', width: 3 } }],
              annotations: [{ x: r.significant, yref: 'paper', y: 1, text: 'your list', showarrow: false, font: { color: '#e11d48' }, yanchor: 'bottom' }] }} />
          <table className="table compact"><thead><tr><th>FDR threshold</th>{r.thresholds.map((t) => <th key={t.fdr}>{t.fdr}</th>)}</tr></thead>
            <tbody><tr><td>Significant terms</td>{r.thresholds.map((t) => <td key={t.fdr}>{t.terms}</td>)}</tr></tbody></table>
        </div>
      </div>
      <div className="toolbar">
        <label>Show <select className="input input-sm" value={only} onChange={(e) => setOnly(e.target.value as typeof only)}><option value="all">all significant terms</option><option value="stable">stable (≥ 80%)</option><option value="fragile">fragile (&lt; 50%)</option></select></label>
        {sel.length > 0 && <><span className="small">{sel.length} selected</span><button className="btn btn-ghost btn-sm" onClick={() => setSel([])}>Clear</button></>}
        <span className="toolbar-spacer" />
        <CopyButton label="Copy names" text={(sel.length ? r.terms.filter((t) => sel.includes(t.id)) : shown).map((t) => t.name).join('\n')} />
        <button className="btn btn-ghost btn-sm" title="Excel workbook: all terms, one sheet per source, and the FDR thresholds" onClick={() => saveXlsx([
          ...sheetsByCategory(r.terms.map((t) => ({ Source: t.source, 'Term ID': t.id, Term: t.name, 'Adjusted p': t.padj, Stability: t.stability, 'Random rate': t.randomRate, Genes: t.genes ?? '' })), 'Source', 'Terms'),
          { name: 'FDR thresholds', rows: r.thresholds.map((t) => ({ 'FDR threshold': t.fdr, 'Significant terms': t.terms })) },
        ], 'flame_robustness.xlsx')}>Excel</button>
        <button className="btn btn-ghost btn-sm" onClick={() => saveText(toCsv(exp, Object.keys(exp[0] ?? {}).map((k) => ({ key: k, label: k }))), 'flame_robustness.csv', 'text/csv')}>CSV</button>
      </div>
      <div className="table-scroll"><table className="table compact">
        <thead><tr><th>Term</th><th>Source</th><th>Adjusted p</th><th>Stability</th><th>Random rate</th><th>Genes</th></tr></thead>
        <tbody>{(sel.length ? r.terms.filter((t) => sel.includes(t.id)) : shown).slice(0, 200).map((t) => (
          <tr key={t.id}><td>{t.name}</td><td>{t.source}</td><td>{formatP(t.padj)}</td>
            <td><span className="meter"><i style={{ width: `${t.stability * 100}%`, background: t.stability >= 0.8 ? '#16a34a' : t.stability >= 0.5 ? '#f59e0b' : '#e11d48' }} /></span> {Math.round(t.stability * 100)}%</td>
            <td>{Math.round(t.randomRate * 100)}%</td>
            <td className="genes-cell"><GeneList genes={t.genes ?? ''} /></td></tr>
        ))}</tbody>
      </table></div>
      <p className="muted small">{r.method}</p>
    </div>
  )
}
