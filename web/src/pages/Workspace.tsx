import { useQuery } from '@tanstack/react-query'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { Link, useNavigate, useSearchParams } from 'react-router-dom'
import { api, downloadFromApi, getWorkspaceId } from '../api/client'
import { useConfig, useDeleteList, useDeleteRun, useInvalidateWorkspace, useRenameList, useWorkspace } from '../api/hooks'
import type { AnalyteList, Run } from '../api/types'
import { CopyButton, EmptyState, ErrorNote, Modal, Note, Spinner } from '../components/ui'
import { useUndoToast } from '../components/Undo'
import { MethodPicker, type InputKind } from '../features/inputs/MethodPicker'
import { InputModal } from '../features/inputs/InputModal'
import { NewRunPanel } from '../features/NewRunPanel'
import { QuickActionModal, type QuickAction } from '../features/utilities/QuickActions'
import { formatNum, plural } from '../lib/format'
import { JobsWindow, ProgressBar, type JobProgress } from '../components/Progress'
import { ToolLogo } from '../components/ToolLogo'
import { ConceptIcon, IconLabel } from '../components/Icons'
import { AnalysesPanel } from '../features/analyses/Analyses'
import { startTask } from '../features/analyses/tasks'
import { saveXlsx } from '../lib/xlsx'
import { SessionModal, SessionStatusPill, useSaveShortcut, useSessionStatus, type Tab as SessionTab } from '../features/session/Session'

type Dialog =
  | { type: 'picker' }
  | { type: 'input'; kind: InputKind; example?: boolean; text?: string; fileName?: string; view?: string }
  | { type: 'quick'; action: QuickAction; listId?: string }
  | { type: 'newRun'; listId?: string; preset?: 'celltypes' | 'literature' }
  | { type: 'gmt' }
  | { type: 'session'; tab?: SessionTab }
  | null

export default function WorkspacePage() {
  const ws = getWorkspaceId()!
  const { data, error, isLoading } = useWorkspace(ws)
  const { data: config } = useConfig()
  const [dialog, setDialog] = useState<Dialog>(null)
  const sessionStatus = useSessionStatus(ws, data)
  useSaveShortcut(ws, useCallback(() => { sessionStatus.refetch() }, [sessionStatus]))
  const [params, setParams] = useSearchParams()
  const [selected, setSelected] = useState<string[]>([])
  const [toolFilter, setToolFilter] = useState<string>('all')
  const [listFilter, setListFilter] = useState<string>('all')
  const navigate = useNavigate()
  const invalidate = useInvalidateWorkspace()
  const exampleStarted = useRef(false)
  const [hiddenRuns, setHiddenRuns] = useState<Set<string>>(new Set())
  const undoToast = useUndoToast()
  const delRun = useDeleteRun()
  const removeRun = (r: Run) => {
    setHiddenRuns((h) => new Set(h).add(r.id))
    setSelected((s) => s.filter((x) => x !== r.id))
    undoToast(`Removed ${r.title}.`,
      () => setHiddenRuns((h) => { const n = new Set(h); n.delete(r.id); return n }),
      () => delRun.mutate(r.id))
  }
  const [exampleNote, setExampleNote] = useState<string | null>(null)
  const [openTask, setOpenTask] = useState<string | null>(null)

  // Preloaded examples, one per analysis option. Started from the Home tab
  // (?example=<kind>) or the Examples menu in the rail.
  const runExample = useCallback(async (kind: string) => {
    setExampleNote(null)
    const SOURCES = ['GO:BP', 'GO:MF', 'GO:CC', 'KEGG', 'REAC']
    const exampleList = async (name = 'Example genes') => {
      const ex = await api<{ text: string }>('/inputs/examples/upload')
      return api<AnalyteList>('/lists', { body: { name, text: ex.text, organismTaxId: 9606, uniqueName: true, meta: { source: 'example' } } })
    }
    const rankedList = async () => {
      const ex = await api<{ text: string }>('/inputs/examples/ranked')
      return api<AnalyteList>('/lists', { body: { name: 'Example ranked genes (DE)', text: ex.text, ranked: true, skipFirstRow: true, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'volcano-ranked' } } })
    }
    const deLists = async () => {
      const d = await api<{ up: string[]; down: string[] }>('/inputs/examples/de_lists')
      const up = await api<AnalyteList>('/lists', { body: { name: 'Example up-regulated', ids: d.up, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'volcano' } } })
      const down = await api<AnalyteList>('/lists', { body: { name: 'Example down-regulated', ids: d.down, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'volcano' } } })
      return { up, down, both: [...d.up, ...d.down] }
    }
    const toRegulators = () => setParams((p) => { const n = new URLSearchParams(p); n.set('tab', 'regulators'); n.delete('example'); return n }, { replace: true })
    const runTask = async (body: Record<string, unknown>, note: string) => {
      const t = await startTask(body)
      toRegulators(); setOpenTask(t.id); setExampleNote(note)
    }
    const startRuns = (body: Record<string, unknown>) => api<{ created: string[] }>('/runs', { body: { threshold: 0.05, ...body } })
    const preselect = (ids: string[]) => setSelected(ids)
    switch (kind) {
      case 'demo': {
        const list = await exampleList()
        await startRuns({ listId: list.id, tools: ['gprofiler', 'string'], datasources: SOURCES })
        setExampleNote('Example loaded: 200 human genes are being analysed with g:Profiler and STRING. Open a run when it finishes, or select both and press Compare.')
        break
      }
      case 'upset': {
        // Up- and down-regulated genes and the 200 most significant ones (dexamethasone, airway):
        // the UpSet plot shows how the top-200 list splits into up and down.
        await deLists()
        await exampleList('Example top 200 dexamethasone genes')
        invalidate()
        setDialog({ type: 'input', kind: 'upset' })
        return
      }
      case 'gsea': {
        const list = await rankedList()
        await startRuns({ listId: list.id, paradigm: 'GSEA', tools: ['clusterprofiler'], datasources: ['HALLMARK', 'KEGG', 'REAC', 'GO:BP'] })
        setExampleNote('GSEA example: about 6,000 genes from a differential-expression table, ranked by sign(log2FC) × −log10 p, are tested against MSigDB Hallmark, KEGG, Reactome and GO with clusterProfiler/fgsea. Positive NES = up-regulated sets, negative = down-regulated.')
        break
      }
      case 'compare-tools': {
        const list = await exampleList()
        const r = await startRuns({ listId: list.id, tools: ['gprofiler', 'string', 'enrichr', 'panther', 'clusterprofiler', 'reactome'], datasources: ['GO:BP', 'KEGG', 'REAC'] })
        preselect(r.created)
        setExampleNote(`Compare tools: the same 200 genes run through ${r.created.length} tools on GO:BP, KEGG and Reactome. The runs are preselected — press Compare when they finish, then open Statistics to see how far the tools agree.`)
        break
      }
      case 'compare-strategies': {
        const ranked = await rankedList()
        const { up, down, both } = await deLists()
        const sig = await api<AnalyteList>('/lists', { body: { name: 'Example significant (up + down)', ids: both, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'volcano' } } })
        const g = await startRuns({ listId: ranked.id, paradigm: 'GSEA', tools: ['clusterprofiler'], datasources: ['HALLMARK', 'KEGG', 'REAC'] })
        const o = await startRuns({ listId: sig.id, tools: ['clusterprofiler'], datasources: ['HALLMARK', 'KEGG', 'REAC'] })
        preselect([...g.created, ...o.created])
        void up; void down
        setExampleNote('Compare strategies: the same experiment analysed two ways with clusterProfiler on Hallmark, KEGG and Reactome — GSEA on all genes ranked by their change, and over-representation (ORA) on the significant genes only (|log2FC| ≥ 1, p ≤ 0.05). Press Compare when both finish.')
        break
      }
      case 'compare-lists': {
        const { up, down } = await deLists()
        const a = await startRuns({ listId: up.id, tools: ['gprofiler'], datasources: SOURCES })
        const b = await startRuns({ listId: down.id, tools: ['gprofiler'], datasources: SOURCES })
        preselect([...a.created, ...b.created])
        setExampleNote('Compare lists: up- and down-regulated genes of the example experiment, each analysed with g:Profiler. Press Compare when both finish to see which functions go up, which go down and which are shared.')
        break
      }
      case 'reactome': {
        const list = await exampleList()
        const r = await startRuns({ listId: list.id, tools: ['reactome', 'gprofiler'], datasources: ['REAC'] })
        preselect(r.created)
        setExampleNote('Reactome example: the 200 dexamethasone genes are analysed by Reactome\'s own Analysis Service and, for comparison, by g:Profiler on its copy of Reactome. Open the Reactome run and click a pathway: “Pathway links” opens the diagram with your genes highlighted. Press Compare to see where the two analyses agree.')
        break
      }
      case 'gmt': {
        const gmt = await api<{ text: string; name: string }>('/inputs/examples/gmt')
        const src = await api<{ id: string }>('/sources', { body: { name: gmt.name, text: gmt.text } })
        const list = await exampleList()
        await startRuns({ listId: list.id, tools: [], sourceIds: [src.id], datasources: [] })
        setExampleNote('Custom gene sets: the MSigDB Hallmark collection was uploaded as a GMT file and tested on 200 example genes (hypergeometric test, Benjamini–Hochberg). Upload your own GMT file under Custom sources the same way.')
        break
      }
      case 'ppi': {
        const list = await exampleList()
        invalidate()
        setDialog({ type: 'quick', action: 'ppi', listId: list.id })
        break
      }
      case 'tf':
      case 'kinase': {
        const { up } = await deLists()
        await runTask({ type: kind, listId: up.id }, kind === 'tf'
          ? 'Transcription factors example: ChEA3 ranks the factors that may drive the 200 genes raised by dexamethasone in airway cells (Himes et al. 2014). Glucocorticoid-response factors such as FOXO1, KLF15 and NR4A1 should rank high.'
          : 'Kinases example: KEA3 ranks kinases whose substrates and partners are over-represented among the genes raised by dexamethasone (Himes et al. 2014); SGK1-related and metabolic kinases are expected.')
        break
      }
      case 'drugs': {
        const { up, down } = await deLists()
        await runTask({ type: 'drugs', upListId: up.id, downListId: down.id, kind: 'chemical' },
          'Drug signatures example: the up- and down-regulated genes of the dexamethasone experiment are compared with LINCS L1000 compound signatures. Glucocorticoids (dexamethasone, hydrocortisone) should appear under “mimic”, and the glucocorticoid-receptor antagonist mifepristone under “reverse” — a built-in sanity check.')
        break
      }
      case 'celltypes': {
        const ex = await api<{ text: string }>('/inputs/examples/reduction')
        const rows = ex.text.split('\n').map((l) => l.split('\t'))
        const ci = rows[0].indexOf('cell_type')
        const nk = rows.slice(1).filter((r) => r[ci] === 'NK cells').map((r) => r[0])
        const list = await api<AnalyteList>('/lists', { body: { name: 'Example NK-cell genes (PBMC 3k)', ids: nk, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'reduction' } } })
        const cfg = await api<{ datasourceCategories: { category: string; sources: { id: string }[] }[] }>('/config', { workspace: null })
        const ct = cfg.datasourceCategories.find((c) => c.category === 'Cell types')?.sources.map((x) => x.id) ?? []
        await startRuns({ listId: list.id, tools: ['enrichr', 'clusterprofiler'], datasources: ct })
        setExampleNote(`Cell types example: the ${nk.length} genes of the NK-cell island of the PBMC 3k UMAP (10x Genomics; Zheng et al. 2017) are tested against PanglaoDB, CellMarker, Azimuth, Tabula Sapiens and MSigDB C8 marker sets. NK cells should come out on top.`)
        break
      }
      case 'literature': {
        const list = await exampleList()
        const cfg = await api<{ datasourceCategories: { category: string; sources: { id: string }[] }[] }>('/config', { workspace: null })
        const lit = cfg.datasourceCategories.find((c) => c.category === 'Literature')?.sources.map((x) => x.id) ?? []
        await startRuns({ listId: list.id, tools: ['string', 'enrichr'], datasources: lit })
        setExampleNote('Literature example: the 200 dexamethasone genes are tested against PubMed (STRING), GeneSigDB published signatures and Rummagene gene sets from PubMed Central papers. Glucocorticoid and asthma studies should rank high; each term links to its article.')
        break
      }
      case 'metabolites': {
        const ex = await api<{ text: string }>('/inputs/examples/metabolites')
        await runTask({ type: 'metabolites', text: ex.text, name: 'Prostate cancer metabolites (Sreekumar 2009)', organismTaxId: 9606 },
          'Metabolite pathways example: six metabolites raised in metastatic prostate cancer (sarcosine, uracil, kynurenine, glycerol 3-phosphate, leucine, proline; Sreekumar et al. 2009, Nature) are mapped to KEGG and tested against KEGG pathways. The result opens as a normal run.')
        break
      }
      case 'modules': {
        const list = await exampleList()
        await runTask({ type: 'modules', listId: list.id, score: 700 },
          'Network modules example: the STRING network of the 200 dexamethasone genes is split into modules (Louvain) and each module is enriched with g:Profiler — a long list becomes a few functional units.')
        break
      }
      case 'robustness': {
        const list = await exampleList()
        await runTask({ type: 'robustness', listId: list.id, sources: ['GO:BP', 'KEGG', 'REAC', 'HALLMARK'] },
          'Robustness example: are the terms enriched in the 200 dexamethasone genes stable? 50 subsamples of 80% of the genes and 50 random lists of the same size are tested against GO:BP, KEGG, Reactome and Hallmark.')
        break
      }
      case 'activity':
        setDialog({ type: 'input', kind: 'expression', example: true, view: 'activity' })
        return
      case 'variants':
      case 'gwas':
        navigate(`/w/${ws}/variants?example=${kind}`)
        return
      default:
        if (['upload', 'snps', 'textmining', 'volcano', 'reduction', 'expression', 'mirna', 'disease', 'pubmed', 'geo', 'dbsnp'].includes(kind)) setDialog({ type: 'input', kind: kind as InputKind, example: true })
        return
    }
    invalidate()
  }, [invalidate, navigate, ws, setParams])

  useEffect(() => {
    const kind = params.get('example')
    if (!kind || !data || exampleStarted.current) return
    exampleStarted.current = true
    setParams({}, { replace: true })
    runExample(kind).catch((e) => setExampleNote(`The example could not be loaded: ${e.message}`))
  }, [params, data, setParams, runExample])

  const runs = (data?.runs ?? []).filter((r) => !hiddenRuns.has(r.id))
  const lists = data?.lists ?? []
  const shownRuns = runs.filter((r) => (toolFilter === 'all' || r.toolKey === toolFilter) && (listFilter === 'all' || r.listName === listFilter))
  const selectableRuns = selected.filter((id) => runs.find((r) => r.id === id && r.status === 'complete' && (r.resultCount ?? 0) > 0))
  const pending = runs.filter((r) => r.status === 'queued' || r.status === 'running').length

  const step = lists.length === 0 ? 1 : runs.length === 0 ? 2 : 3

  if (isLoading) return <div className="page-center"><Spinner label="Opening workspace" /></div>
  if (error) {
    return (
      <div className="page-center stack">
        <ErrorNote error={error} />
        <Link className="btn btn-primary" to="/start?new=1">
          Start a new workspace
        </Link>
      </div>
    )
  }

  const section = (params.get('tab') as WorkspaceSection) || 'enrich'
  const setSection = (t: WorkspaceSection) => setParams((p) => { const n = new URLSearchParams(p); n.set('tab', t); return n }, { replace: true })
  const loadExample = (k: string) => runExample(k).catch((e) => setExampleNote(`The example could not be loaded: ${e.message}`))

  return (
    <div className="workspace-page">
      <div className="ws-topbar">
        <ShareBanner ws={ws} expires={data?.expires} />
        <SessionStatusPill status={sessionStatus.data} onOpen={(tab) => setDialog({ type: 'session', tab })} />
        <ExamplesMenu onPick={loadExample} />
      </div>
      {exampleNote && <Note kind="success">{exampleNote}</Note>}

      <div className="workspace">
        <aside className="rail sec-data" aria-label="Your data">
          <div className="sec-label"><ConceptIcon name="upload" size={18} /> Your data</div>
          <ListsSection lists={lists} max={config?.limits.maxLists ?? 10} onAdd={() => setDialog({ type: 'picker' })}
            onRun={(id) => setDialog({ type: 'newRun', listId: id })} onQuick={(action, id) => setDialog({ type: 'quick', action, listId: id })} />
          <section className="rail-section">
            <div className="rail-head">
              <h2>Custom gene sets</h2>
              <button className="btn btn-ghost btn-sm" onClick={() => setDialog({ type: 'gmt' })} title="Upload gene sets as a GMT file"><IconLabel icon="gmt">+ GMT</IconLabel></button>
            </div>
            {(data?.sources ?? []).length === 0 ? (
              <p className="muted small">Upload your own gene sets (GMT) to test them like any other database.</p>
            ) : (
              <ul className="plain-list">
                {data!.sources.map((s) => (
                  <li key={s.id} className="rail-item">
                    <span>{s.name}</span>
                    <span className="muted small">{s.termCount ?? s.term_count} sets</span>
                    <button className="icon-btn" aria-label={`Delete ${s.name}`} onClick={() => api(`/sources/${s.id}`, { method: 'DELETE' }).then(invalidate)}>×</button>
                  </li>
                ))}
              </ul>
            )}
          </section>
        </aside>

        <div className="workspace-main">
          <nav className="sec-tabs" role="tablist" aria-label="Analyses">
            {SECTIONS.map((t) => (
              <button key={t.id} role="tab" aria-selected={section === t.id} className={`sec-tab sec-${t.id}${section === t.id ? ' active' : ''}`} onClick={() => setSection(t.id)}>
                <ConceptIcon name={t.icon} size={22} />
                <span><strong>{t.label}</strong><span className="small">{t.hint}</span></span>
                {t.id === 'enrich' && runs.length > 0 && <span className="sec-count">{runs.length}{pending ? ` · ${pending} running` : ''}</span>}
              </button>
            ))}
          </nav>

          {section === 'enrich' && (
            <section className="sec-panel sec-enrich" aria-label="Functional enrichment">
              {runs.length === 0 && <ol className="progress-steps" aria-label="Progress">
                <li className={step >= 1 ? (step > 1 ? 'done' : 'current') : ''}><span>1</span> Add a list</li>
                <li className={step >= 2 ? (step > 2 ? 'done' : 'current') : ''}><span>2</span> Run enrichment</li>
                <li className={step >= 3 ? 'current' : ''}><span>3</span> Explore and compare</li>
              </ol>}
              <div className="page-header">
                <div>
                  <h1>Enrichment runs</h1>
                  <p className="muted">
                    {runs.length ? `${plural(runs.length, 'run')}${pending ? ` · ${pending} in progress` : ''}` : 'Results of each tool appear here as cards.'}
                  </p>
                </div>
                <div className="row">
                  <button className="btn btn-secondary" disabled={selectableRuns.length < 2}
                    title={selectableRuns.length < 2 ? 'Select two or more finished runs to compare them' : ''}
                    onClick={() => navigate(`/w/${ws}/compare?runs=${selectableRuns.join(',')}`)}>
                    <IconLabel icon="compare">Compare{selectableRuns.length ? ` (${selectableRuns.length})` : ''}</IconLabel>
                  </button>
                  <button className="btn btn-primary" disabled={!lists.length} onClick={() => setDialog({ type: 'newRun' })}>
                    <IconLabel icon="run">New run</IconLabel>
                  </button>
                </div>
              </div>
              {lists.length === 0 ? (
                <EmptyState title="Start by adding a gene list">
                  <p>Paste IDs, upload a file, or pick genes from a plot. Not sure? Load an example and see how it works.</p>
                  <div className="row center">
                    <button className="btn btn-primary" onClick={() => setDialog({ type: 'picker' })}>Add a list</button>
                    <button className="btn btn-secondary" onClick={() => setDialog({ type: 'input', kind: 'upload', example: true })}>Use example genes</button>
                  </div>
                </EmptyState>
              ) : runs.length === 0 ? (
                <EmptyState title="Your list is ready — now run an enrichment">
                  <p>Choose one or more tools. The defaults (GO and KEGG, FDR &lt; 0.05) are a good start.</p>
                  <button className="btn btn-primary" onClick={() => setDialog({ type: 'newRun' })}>Run enrichment</button>
                </EmptyState>
              ) : (
                <>
                  <div className="filters-row" role="group" aria-label="Filter runs">
                    <label>Tool{' '}
                      <select className="input input-sm" value={toolFilter} onChange={(e) => setToolFilter(e.target.value)}>
                        <option value="all">All tools</option>
                        {[...new Set(runs.map((r) => r.toolKey))].map((k) => <option key={k} value={k}>{runs.find((r) => r.toolKey === k)?.tool}</option>)}
                      </select>
                    </label>
                    <label>List{' '}
                      <select className="input input-sm" value={listFilter} onChange={(e) => setListFilter(e.target.value)}>
                        <option value="all">All lists</option>
                        {[...new Set(runs.map((r) => r.listName))].map((n) => <option key={n} value={n}>{n}</option>)}
                      </select>
                    </label>
                    {selected.length > 0 && <button className="btn btn-ghost btn-sm" onClick={() => setSelected([])}>Clear selection</button>}
                    <span className="muted small">Tick two or more runs to compare them.</span>
                  </div>
                  <div className="run-grid">
                    {shownRuns.map((r) => (
                      <RunCard key={r.id} run={r} onRemove={() => removeRun(r)} selected={selected.includes(r.id)}
                        onToggle={() => setSelected((s) => (s.includes(r.id) ? s.filter((x) => x !== r.id) : [...s, r.id]))}
                        onOpen={() => navigate(`/w/${ws}/runs/${r.id}`)} />
                    ))}
                  </div>
                </>
              )}
            </section>
          )}

          {section === 'variants' && <VariantsPanel ws={ws} onExample={loadExample} />}

          {section === 'regulators' && (
            <AnalysesPanel lists={lists} onAddList={() => setDialog({ type: 'picker' })} open={openTask} setOpen={setOpenTask}
              onExample={(k) => runExample(k).catch((e) => setExampleNote(`The example could not be loaded: ${e.message}`))}
              onCellTypes={() => setDialog({ type: 'newRun', preset: 'celltypes' })} onLiterature={() => setDialog({ type: 'newRun', preset: 'literature' })} />
          )}

          {section === 'tools' && (
            <section className="sec-panel sec-tools" aria-label="Gene and protein tools">
              <div className="page-header"><div><h1>Gene and protein tools</h1><p className="muted">Work on one of your lists; results can be saved as new lists.</p></div></div>
              <div className="tool-grid-ws">
                {TOOLS.map((t) => (
                  <button key={t.action} type="button" className="tool-tile" disabled={!lists.length} onClick={() => setDialog({ type: 'quick', action: t.action })}>
                    <ConceptIcon name={t.icon} size={36} />
                    <strong>{t.title}</strong>
                    <span className="muted small">{t.text}</span>
                  </button>
                ))}
              </div>
              {!lists.length && <p className="muted small">Add a list first, or <button className="link-btn" onClick={() => loadExample('ppi')}>load an example list</button>.</p>}
            </section>
          )}
        </div>
      </div>

      {dialog?.type === 'picker' && (
        <MethodPicker lists={lists} onClose={() => setDialog(null)} onPick={(kind) => setDialog({ type: 'input', kind })} />
      )}
      {dialog?.type === 'input' && (
        <InputModal key={`${dialog.kind}-${dialog.fileName ?? ''}`} kind={dialog.kind} example={dialog.example} lists={lists} onClose={() => setDialog(null)}
          initialText={dialog.text} initialFileName={dialog.fileName} initialView={dialog.view}
          onOpenVolcano={(text, fileName) => setDialog({ type: 'input', kind: 'volcano', text, fileName })}
          onCreated={(list) => setDialog({ type: 'newRun', listId: list.id })} />
      )}
      {dialog?.type === 'quick' && (
        <QuickActionModal action={dialog.action} lists={lists} initialListId={dialog.listId} onClose={() => setDialog(null)} />
      )}
      {dialog?.type === 'newRun' && (
        <NewRunPanel lists={lists} sources={data?.sources ?? []} initialListId={dialog.listId} preset={dialog.preset} onClose={() => setDialog(null)} />
      )}
      {dialog?.type === 'gmt' && <GmtModal onClose={() => setDialog(null)} />}
      <JobsWindow runs={runs} ws={ws} />
      {dialog?.type === 'session' && <SessionModal ws={ws} summary={data} status={sessionStatus.data} initialTab={dialog.tab} onClose={() => { setDialog(null); sessionStatus.refetch() }} />}
    </div>
  )
}

function ShareBanner({ ws, expires }: { ws: string; expires?: number }) {
  const url = `${window.location.origin}/w/${ws}`
  const days = expires ? Math.max(1, Math.round((expires * 1000 - Date.now()) / 86_400_000)) : null
  return (
    <div className="share-banner">
      <span>
        <strong>Bookmark this link</strong> to come back to your lists and results{days ? ` (kept for ${days} more days)` : ''}. Anyone with the link can view this workspace.
      </span>
      <code className="share-url">{url}</code>
      <CopyButton text={url} label="Copy link" />
    </div>
  )
}

function ListsSection({ lists, max, onAdd, onRun, onQuick }: {
  lists: AnalyteList[]; max: number; onAdd: () => void; onRun: (id: string) => void; onQuick: (a: QuickAction, id: string) => void
}) {
  const rename = useRenameList()
  const del = useDeleteList()
  const undoToast = useUndoToast()
  const invalidate = useInvalidateWorkspace()
  const deleteWithUndo = async (l: AnalyteList) => {
    const full = await api<AnalyteList & { scores?: number[] }>(`/lists/${l.id}`)
    await del.mutateAsync(l.id)
    undoToast(`Deleted list "${l.name}".`, () =>
      api('/lists', { body: { name: full.name, ids: full.ids, scores: full.scores, organismTaxId: full.organismTaxId,
        analyteType: full.analyteType, idFormat: full.idFormat, meta: full.meta, uniqueName: true } }).then(invalidate))
  }
  const [editing, setEditing] = useState<string | null>(null)
  const [name, setName] = useState('')
  const [menu, setMenu] = useState<string | null>(null)
  return (
    <section className="rail-section">
      <div className="rail-head">
        <h2>Lists <span className="count">{lists.length}/{max}</span></h2>
      </div>
      <button className="btn btn-primary btn-block" onClick={onAdd} disabled={lists.length >= max}>+ Add list</button>
      {lists.length >= max && <p className="muted small">List limit reached. Delete a list to add another.</p>}
      <ErrorNote error={rename.error ?? del.error} />
      <ul className="plain-list">
        {lists.map((l) => (
          <li key={l.id} className="rail-item list-item">
            {editing === l.id ? (
              <form className="row" onSubmit={(e) => {
                e.preventDefault()
                const old = l.name
                rename.mutate({ id: l.id, name }, { onSuccess: (nl) => {
                  setEditing(null)
                  undoToast(`Renamed to "${nl.name}".`, () => rename.mutateAsync({ id: l.id, name: old }))
                } })
              }}>
                <input className="input input-sm" autoFocus value={name} onChange={(e) => setName(e.target.value)}
                  onKeyDown={(e) => e.key === 'Escape' && setEditing(null)} aria-label="New list name" maxLength={80} />
                <button className="btn btn-sm btn-primary">Save</button>
              </form>
            ) : (
              <>
                <div className="list-text">
                  <span className="list-name" title={l.name}>{l.name}</span>
                  <span className="muted small">{formatNum(l.size)} IDs · {l.organismName.replace(/ \[NCBI.*$/, '')}{l.ranked && <> · <span className="tag-ranked" title="IDs with scores: can be used for GSEA">ranked</span></>}</span>
                </div>
                <div className="menu-wrap">
                  <button className="icon-btn" aria-label={`Actions for ${l.name}`} aria-expanded={menu === l.id} onClick={() => setMenu(menu === l.id ? null : l.id)}>⋯</button>
                  {menu === l.id && (
                    <div className="menu" onMouseLeave={() => setMenu(null)}>
                      <button onClick={() => { setMenu(null); onRun(l.id) }}>Run enrichment</button>
                      <button onClick={() => { setMenu(null); setEditing(l.id); setName(l.name) }}>Rename</button>
                      <button onClick={() => { setMenu(null); api<{ ids: string[]; scores?: number[] }>(`/lists/${l.id}`).then((d) => saveXlsx([{ name: l.name,
                        rows: d.ids.map((id, i) => ({ ID: id, ...(d.scores ? { Score: d.scores[i] } : {}) })) }], `${l.name}.xlsx`)) }}>Download Excel</button>
                      <button onClick={() => { setMenu(null); downloadFromApi(`/lists/${l.id}/csv`, `${l.name}.csv`) }}>Download CSV</button>
                      <button onClick={() => { setMenu(null); onQuick('convert', l.id) }}>Convert IDs</button>
                      <button onClick={() => { setMenu(null); onQuick('orthologs', l.id) }}>Find orthologs</button>
                      <button onClick={() => { setMenu(null); onQuick('ppi', l.id) }}>Protein network</button>
                      <button className="danger" onClick={() => { setMenu(null); deleteWithUndo(l) }}>Delete</button>
                    </div>
                  )}
                </div>
              </>
            )}
          </li>
        ))}
      </ul>
    </section>
  )
}

function RunCard({ run, selected, onToggle, onOpen, onRemove }: { run: Run; selected: boolean; onToggle: () => void; onOpen: () => void; onRemove: () => void }) {
  const busy = run.status === 'queued' || run.status === 'running'
  const empty = run.status === 'complete' && !run.resultCount
  const canSelect = run.status === 'complete' && !!run.resultCount
  const sources = Array.isArray(run.params?.datasources) ? run.params.datasources : [run.params?.datasources].filter(Boolean)
  return (
    <article className={`run-card${selected ? ' selected' : ''}${run.status === 'error' ? ' error' : ''}`} style={{ borderLeftColor: `var(--color-${run.toolKey}, var(--accent-color))` }}>
      <header className="run-card-head">
        <label className="run-check" title={canSelect ? 'Select to compare' : 'Only finished runs with results can be compared'}>
          <input type="checkbox" checked={selected} disabled={!canSelect} onChange={onToggle} aria-label={`Select ${run.title} for comparison`} />
        </label>
        <h3 className="run-title"><ToolLogo tool={run.toolKey} size={20} /> {run.title}</h3>
        <button className="icon-btn" aria-label={`Remove ${run.title}`} title="Remove (you can undo)" onClick={onRemove}>×</button>
      </header>
      <p className="muted small">{run.listName} · {run.organismName.replace(/ \[NCBI.*$/, '')}</p>
      <p className="small">{sources.slice(0, 4).join(', ')}{sources.length > 4 ? ` +${sources.length - 4}` : ''} · {run.params?.metric} &lt; {run.params?.threshold}</p>
      <div className="run-card-foot">
        {busy && <ProgressBar progress={run.progress} status={run.status} />}
        {run.status === 'error' && <span className="text-danger small">{run.error ?? 'The tool did not respond.'}</span>}
        {empty && <span className="muted small">No significant terms{run.messages?.length ? ` — ${run.messages[0]}` : ''}</span>}
        {canSelect && (
          <button className="btn btn-primary btn-sm" onClick={onOpen}>
            View {run.resultCount} terms →
          </button>
        )}
      </div>
    </article>
  )
}

function GmtModal({ onClose }: { onClose: () => void }) {
  const [name, setName] = useState('My gene sets')
  const [text, setText] = useState('')
  const [err, setErr] = useState<unknown>(null)
  const invalidate = useInvalidateWorkspace()
  const sets = useMemo(() => text.split(/\r?\n/).filter((l) => l.split('\t').length >= 3).length, [text])
  return (
    <Modal title="Upload custom gene sets (GMT)" onClose={onClose}
      subtitle="Each line: set ID ⇥ description ⇥ gene ⇥ gene … (tab-separated). Gene IDs must match your list's IDs."
      footer={<>
        <button className="btn btn-ghost" onClick={onClose}>Cancel</button>
        <button className="btn btn-primary" disabled={!sets} onClick={() =>
          api('/sources', { body: { name, text } }).then(() => { invalidate(); onClose() }).catch(setErr)}>Add source ({sets} sets)</button>
      </>}>
      <label className="field"><span className="field-label">Name</span>
        <input className="input" value={name} maxLength={80} onChange={(e) => setName(e.target.value)} /></label>
      <label className="field"><span className="field-label">GMT file</span>
        <input type="file" accept=".gmt,.txt,.tsv" onChange={(e) => e.target.files?.[0]?.text().then(setText)} /></label>
      <textarea className="input mono" rows={8} value={text} onChange={(e) => setText(e.target.value)} placeholder={'HALLMARK_APOPTOSIS\tApoptosis\tCASP3\tCASP8\tBAX\t…'} aria-label="GMT content" />
      <ErrorNote error={err} />
    </Modal>
  )
}


export const WORKSPACE_EXAMPLES: { kind: string; title: string; text: string }[] = [
  { kind: 'demo', title: 'Gene list → two tools', text: '200 genes through g:Profiler and STRING' },
  { kind: 'gsea', title: 'Ranked list (GSEA)', text: 'All genes of an experiment, ranked by change' },
  { kind: 'compare-tools', title: 'Compare tools', text: 'One list, six enrichment tools' },
  { kind: 'reactome', title: 'Reactome pathways', text: 'Reactome Analysis Service vs g:Profiler' },
  { kind: 'compare-strategies', title: 'Compare strategies', text: 'GSEA on all genes vs ORA on significant genes' },
  { kind: 'compare-lists', title: 'Compare lists', text: 'Up- vs down-regulated genes' },
  { kind: 'gmt', title: 'Custom gene sets', text: 'MSigDB Hallmark uploaded as GMT' },
  { kind: 'ppi', title: 'Protein network', text: 'STRING interactions of an example list' },
  { kind: 'tf', title: 'Transcription factors', text: 'ChEA3 on genes raised by dexamethasone' },
  { kind: 'kinase', title: 'Kinases', text: 'KEA3 on the same genes' },
  { kind: 'drugs', title: 'Drug signatures', text: 'LINCS: which compounds mimic or reverse dexamethasone' },
  { kind: 'celltypes', title: 'Cell types', text: 'Marker genes of NK cells (PBMC 3k)' },
  { kind: 'literature', title: 'Literature', text: 'Papers reporting gene sets like yours' },
  { kind: 'metabolites', title: 'Metabolite pathways', text: 'Prostate cancer metabolites → KEGG' },
  { kind: 'modules', title: 'Network modules', text: 'STRING modules, each enriched' },
  { kind: 'robustness', title: 'Robustness check', text: 'Bootstrap and random-list controls' },
  { kind: 'activity', title: 'Pathway activity', text: 'ssGSEA / PROGENy scores per sample' },
  { kind: 'volcano', title: 'Volcano plot', text: 'Select up- and down-regulated genes' },
  { kind: 'reduction', title: 'UMAP / 2D plot', text: 'Lasso a cluster on a UMAP of single cells' },
  { kind: 'expression', title: 'Expression matrix', text: 'Clusters, gene map, two-group comparison' },
  { kind: 'mirna', title: 'microRNAs', text: 'From miRNAs to their target genes' },
  { kind: 'disease', title: 'Disease genes', text: 'Asthma: Open Targets, DisGeNET, OMIM, GWAS' },
  { kind: 'pubmed', title: 'PubMed genes', text: 'Genes of articles on glucocorticoids in airway muscle' },
  { kind: 'geo', title: 'GEO signature', text: 'GSE52778: dexamethasone vs untreated airway cells' },
  { kind: 'dbsnp', title: 'dbSNP variants', text: 'Pathogenic APOE variants and their genes' },
  { kind: 'variants', title: 'Variants', text: 'rsIDs and VCF lines → genes and traits' },
  { kind: 'gwas', title: 'GWAS results', text: 'Summary statistics → loci, genes, traits' },
]

function ExamplesMenu({ onPick }: { onPick: (kind: string) => Promise<void> | void }) {
  const [busy, setBusy] = useState<string | null>(null)
  return (
    <details className="examples-menu">
      <summary className="btn btn-secondary btn-sm"><IconLabel icon="examples">Examples ▾</IconLabel></summary>
      <div className="examples-panel" role="menu">
        <p className="muted small">Load ready-made data from published studies for any analysis.</p>
        <div className="example-list">
          {WORKSPACE_EXAMPLES.map((e) => (
            <button key={e.kind} type="button" role="menuitem" className="example-mini" disabled={!!busy}
              onClick={async (ev) => { setBusy(e.kind); try { await onPick(e.kind) } finally { setBusy(null); (ev.currentTarget.closest('details') as HTMLDetailsElement | null)?.removeAttribute('open') } }}>
              <ConceptIcon name={e.kind} />
              <span><strong>{busy === e.kind ? 'Loading…' : e.title}</strong><span className="muted small">{e.text}</span></span>
            </button>
          ))}
        </div>
      </div>
    </details>
  )
}

type WorkspaceSection = 'enrich' | 'regulators' | 'variants' | 'tools'
const SECTIONS: { id: WorkspaceSection; label: string; hint: string; icon: string }[] = [
  { id: 'enrich', label: 'Enrichment', hint: 'ORA and GSEA of your lists, comparisons', icon: 'run' },
  { id: 'regulators', label: 'Regulators & drugs', hint: 'TFs, kinases, drugs, cell types, robustness', icon: 'regulators' },
  { id: 'variants', label: 'Variants & GWAS', hint: 'From variants to genes and traits', icon: 'variants' },
  { id: 'tools', label: 'Gene tools', hint: 'ID conversion, orthologs, STRING network', icon: 'ppi' },
]
const TOOLS: { action: QuickAction; title: string; text: string; icon: string }[] = [
  { action: 'convert', title: 'Convert IDs', text: 'Translate a list to Ensembl, Entrez, UniProt and other ID types (g:Convert).', icon: 'convert' },
  { action: 'orthologs', title: 'Find orthologs', text: 'Map a list to the matching genes of another organism (g:Orth).', icon: 'orthologs' },
  { action: 'ppi', title: 'Protein network', text: 'Known and predicted interactions among your proteins (STRING); select and save sub-networks.', icon: 'ppi' },
]

/** Variant analyses of this workspace, with live progress. */
function VariantsPanel({ ws, onExample }: { ws: string; onExample: (k: string) => void }) {
  const q = useQuery({
    queryKey: ['variant-analyses', ws],
    queryFn: () => api<{ id: string; name: string; status: string; inputCount: number; created: number; error?: string; progress?: JobProgress | null }[]>('/variants'),
    refetchInterval: (qq) => ((qq.state.data ?? []).some((a) => a.status === 'queued' || a.status === 'running') ? 1500 : false),
  })
  const items = q.data ?? []
  return (
    <section className="sec-panel sec-variants" aria-label="Variants and GWAS">
      <div className="page-header">
        <div><h1>Variants and GWAS</h1><p className="muted">Annotate SNPs, VCF variants or GWAS summary statistics: consequences, genes, frequencies, ClinVar and trait associations.</p></div>
        <div className="row">
          <Link className="btn btn-primary" to={`/w/${ws}/variants`}><IconLabel icon="variants">New variant analysis</IconLabel></Link>
        </div>
      </div>
      {items.length === 0 ? (
        <EmptyState title="No variant analyses yet">
          <p>Start one, or open a published example.</p>
          <div className="row center">
            <button className="btn btn-secondary" onClick={() => onExample('variants')}>Clinical variants example</button>
            <button className="btn btn-secondary" onClick={() => onExample('gwas')}>Alzheimer GWAS example</button>
          </div>
        </EmptyState>
      ) : (
        <div className="run-grid">
          {items.map((a) => (
            <article key={a.id} className={`run-card sec-card${a.status === 'error' ? ' error' : ''}`}>
              <header className="run-card-head"><h3 className="run-title"><ConceptIcon name="variants" size={20} /> {a.name}</h3></header>
              <p className="muted small">{plural(a.inputCount, 'variant')} · {new Date(a.created * 1000).toLocaleString()}</p>
              <div className="run-card-foot">
                {(a.status === 'queued' || a.status === 'running') && <ProgressBar progress={a.progress} status={a.status} />}
                {a.status === 'error' && <span className="text-danger small">{a.error}</span>}
                {a.status === 'complete' && <Link className="btn btn-primary btn-sm" to={`/w/${ws}/variants/${a.id}`}>Open report →</Link>}
              </div>
            </article>
          ))}
        </div>
      )}
    </section>
  )
}
