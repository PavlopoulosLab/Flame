import { TableTextArea } from '../components/TableTextArea'
import { useQuery } from '@tanstack/react-query'
import { useEffect, useMemo, useRef, useState } from 'react'
import { API_BASE, api } from '../api/client'
import { useConfig, useInvalidateWorkspace, useStartRuns } from '../api/hooks'
import type { AnalyteList, CustomSource } from '../api/types'
import { BusyBar, ErrorNote, Field, Modal, Note, Segmented } from '../components/ui'
import { ToolLogo } from '../components/ToolLogo'
import { SplitUpDown } from './UpDownRunPanel'

const METRIC_HELP: Record<string, string> = {
  gSCS: 'g:SCS — g:Profiler’s own correction, accounts for the GO structure',
  fdr: 'FDR — Benjamini–Hochberg false discovery rate',
  bonferroni: 'Bonferroni — most conservative',
  BH: 'FDR (Benjamini–Hochberg)',
  BY: 'FDR (Benjamini–Yekutieli)',
  holm: 'Holm', hochberg: 'Hochberg', hommel: 'Hommel',
  top: 'Top 100 terms (no threshold)',
  FDR: 'FDR', NONE: 'Uncorrected p-value', BONFERRONI: 'Bonferroni',
  p_value: 'Uncorrected p-value',
  adjusted_pvalue: 'Adjusted p-value (Benjamini–Hochberg)',
  none: 'Uncorrected p-value',
}

interface ToolSettings { correction?: string; threshold?: number; backgroundListId?: string }

export function NewRunPanel({ lists, sources, initialListId, initialListIds, onClose, preset, onUpDown }: { lists: AnalyteList[]; sources: CustomSource[]; initialListId?: string; /** Several lists ticked in the lists panel: the same analysis is started on each. */ initialListIds?: string[]; onClose: () => void; preset?: 'celltypes' | 'literature'; /** Open the up/down GO step for a volcano pair. */ onUpDown?: (pairId: string) => void }) {
  const { data: config } = useConfig()
  const cellTypeSources = config?.datasourceCategories.find((c) => c.category === 'Cell types')?.sources.map((s) => s.id) ?? []
  const literatureSources = config?.datasourceCategories.find((c) => c.category === 'Literature')?.sources.map((s) => s.id) ?? []
  const start = useStartRuns()
  const invalidateWs = useInvalidateWorkspace()
  const [loadingExample, setLoadingExample] = useState(false)
  // Up/down lists saved together (from a volcano table, or split from a ranked list) are analysed together: ticking one ticks its partner.
  const withPartners = (ids: string[]) => {
    const pairs = new Set(lists.filter((l) => ids.includes(l.id) && l.meta?.direction && l.meta?.pairId).map((l) => String(l.meta.pairId)))
    return [...new Set([...ids, ...lists.filter((l) => l.meta?.direction && pairs.has(String(l.meta.pairId))).map((l) => l.id)])]
  }
  const startIds = withPartners((initialListIds?.length ? initialListIds : initialListId ? [initialListId] : []).filter((id) => lists.some((l) => l.id === id)))
  const initial = lists.find((l) => l.id === startIds[0])
  const [paradigm, setParadigm] = useState<'ORA' | 'GSEA'>(initial?.ranked ? 'GSEA' : 'ORA')
  const usableLists = paradigm === 'GSEA' ? lists.filter((l) => l.ranked) : lists
  // One or more lists; the same tools and settings run on each of them.
  const [listIds, setListIds] = useState<string[]>(startIds.length ? startIds : lists[0] ? [lists[0].id] : [])
  const picked = usableLists.filter((l) => listIds.includes(l.id))
  const list = picked[0]   // drives organism-specific options (tool support, background lists)
  const mixedOrganisms = new Set(picked.map((l) => l.organismTaxId)).size > 1
  const [progress, setProgress] = useState<string | null>(null)
  const [showGmt, setShowGmt] = useState(false)
  const [showSplit, setShowSplit] = useState(false)
  const [showSignature, setShowSignature] = useState(false)
  const [tools, setTools] = useState<string[]>(preset === 'celltypes' ? ['enrichr', 'clusterprofiler'] : preset === 'literature' ? ['string', 'enrichr'] : initial?.ranked ? ['clusterprofiler'] : ['gprofiler'])
  const shownParadigm = useRef(paradigm)
  const toolOffers = (key: string) => (config?.tools.find((t) => t.key === key)?.paradigms ?? ['ORA']).includes(paradigm)
  useEffect(() => {
    setTools((ts) => {
      const kept = ts.filter(toolOffers)
      return kept.length ? kept : paradigm === 'GSEA' ? ['clusterprofiler'] : ['gprofiler']
    })
    // GSEA picks from MSigDB; switching paradigm starts from that paradigm's usual gene sets.
    if (config && shownParadigm.current !== paradigm) setDatasources(paradigm === 'GSEA' ? config.gseaDefaultDatasources : config.defaultDatasources)
    if (config) shownParadigm.current = paradigm
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [paradigm, config])
  const [customIds, setCustomIds] = useState<string[]>([])
  const [datasources, setDatasources] = useState<string[]>([])
  const [threshold, setThreshold] = useState(0.05)
  const [perTool, setPerTool] = useState<Record<string, ToolSettings>>({})
  const [showAdvanced, setShowAdvanced] = useState(false)
  const [result, setResult] = useState<{ created: string[]; reused: string[]; skipped: { tool: string; reason: string }[] } | null>(null)

  useEffect(() => {
    if (config && !datasources.length) setDatasources(preset === 'celltypes' ? cellTypeSources : preset === 'literature' ? literatureSources : paradigm === 'GSEA' ? config.gseaDefaultDatasources : config.defaultDatasources)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [config, datasources.length])

  const support = useQuery({
    queryKey: ['tool-support', list?.organismTaxId],
    queryFn: () => api<{ tools: { key: string; supported: boolean }[] }>(`/organisms/${list!.organismTaxId}/tools`, { workspace: null }),
    enabled: !!list,
  })
  const supported = (key: string) => support.data?.tools.find((t) => t.key === key)?.supported ?? true
  useEffect(() => {
    // Drop tools that do not support the list's organism.
    if (support.data) setTools((ts) => ts.filter((t) => supported(t)))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [support.data])

  const toolCfg = (key: string) => config?.tools.find((t) => t.key === key)
  const supportedSources = useMemo(() => new Set(tools.flatMap((k) => toolCfg(k)?.datasources ?? [])),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [tools, config])
  const chosenSources = datasources.filter((d) => supportedSources.has(d))
  const backgroundOptions = lists.filter((l) => !listIds.includes(l.id) && l.organismTaxId === list?.organismTaxId)
  const canSubmit = !!list && (tools.length > 0 || customIds.length > 0) && (customIds.length > 0 || chosenSources.length > 0)
    && (paradigm === 'ORA' || picked.every((l) => l.ranked))

  const submit = async () => {
    if (!picked.length) return
    let failed = false
    const total = { created: [] as string[], reused: [] as string[], skipped: [] as { tool: string; reason: string }[] }
    try {
      for (const [i, l] of picked.entries()) {
        if (picked.length > 1) setProgress(`Starting runs for ${l.name} (${i + 1} of ${picked.length})`)
        const r = await start.mutateAsync({
          listId: l.id, paradigm, tools, sourceIds: customIds, datasources: chosenSources, threshold,
          perTool: Object.fromEntries(Object.entries(perTool).map(([k, v]) => [k, { ...v, threshold: v.threshold ?? threshold }])),
        })
        total.created.push(...r.created); total.reused.push(...r.reused)
        total.skipped.push(...r.skipped.map((x) => ({ tool: picked.length > 1 ? `${l.name} · ${x.tool}` : x.tool, reason: x.reason })))
      }
    } catch { failed = true /* shown through start.error; runs already started are kept */ }
    setProgress(null)
    if (total.created.length || total.reused.length || total.skipped.length) setResult(total)
    if (!failed && !total.skipped.length && (total.created.length || total.reused.length)) setTimeout(onClose, 900)
  }

  const setTool = (key: string, patch: ToolSettings) => setPerTool((p) => ({ ...p, [key]: { ...p[key], ...patch } }))

  return (
    <Modal title="New enrichment run" size="xl" onClose={onClose} palette={false}
      subtitle="Each selected tool runs separately; you can compare them afterwards."
      footer={<>
        <span className="muted small">{picked.length > 1 ? `${picked.length} lists × ` : ''}{tools.length + customIds.length} run{tools.length + customIds.length === 1 ? '' : 's'}{picked.length > 1 ? ` = ${picked.length * (tools.length + customIds.length)} runs` : ''} · {chosenSources.length} sources</span>
        <button className="btn btn-ghost" onClick={onClose}>Cancel</button>
        <button className="btn btn-primary" disabled={!canSubmit || start.isPending} onClick={submit}>{start.isPending ? 'Starting…' : picked.length > 1 ? `Run enrichment on ${picked.length} lists` : 'Run enrichment'}</button>
      </>}>
      <div className="run-form">
        <section>
          <h3>1 · What to analyse</h3>
          <Field label="Analysis type" hint={paradigm === 'ORA'
            ? 'Over-representation: are some functions more common in your list than expected by chance?'
            : 'Gene set enrichment (GSEA): do the genes of a function sit at the top or bottom of your ranked list? Needs a list with scores.'}>
            <Segmented label="Analysis type" value={paradigm} onChange={setParadigm}
              options={[{ value: 'ORA', label: 'Over-representation (ORA)' }, { value: 'GSEA', label: 'Ranked list (GSEA)' }]} />
          </Field>
          {paradigm === 'GSEA' && !usableLists.length ? (
            <div className="stack-sm">
              <Note kind="warning">You have no ranked lists yet. Create one from a volcano table (“Save whole table as ranked list”) or paste IDs with scores (“IDs with scores” in Paste or upload).</Note>
              <button type="button" className="btn btn-secondary" disabled={loadingExample} onClick={async () => {
                setLoadingExample(true)
                try {
                  const ex = await api<{ text: string }>('/inputs/examples/ranked')
                  const l = await api<{ id: string }>('/lists', { body: { name: 'Example ranked genes (DE)', text: ex.text, ranked: true, skipFirstRow: true, organismTaxId: 9606, uniqueName: true, meta: { source: 'example', method: 'volcano-ranked' } } })
                  await invalidateWs()
                  setListIds([l.id])
                } finally { setLoadingExample(false) }
              }}>{loadingExample ? 'Loading…' : 'Load an example ranked list (all 12,426 genes of a differential-expression experiment)'}</button>
            </div>
          ) : (
            <Field label={picked.length > 1 ? `Lists (${picked.length} selected — the same analysis runs on each)` : 'Lists (tick several to run the same analysis on each)'}>
              <div className="run-lists">
                {usableLists.map((l) => (
                  <label key={l.id} className="check">
                    <input type="checkbox" checked={listIds.includes(l.id)} onChange={() => setListIds((cur) => (cur.includes(l.id) ? cur.filter((x) => x !== l.id) : withPartners([...cur, l.id])))} />
                    <span>{l.name} {l.meta?.direction === 'up' ? <span className="dir-badge dir-up" title="Up-regulated genes (paired with a down list)">▲ up</span> : l.meta?.direction === 'down' ? <span className="dir-badge dir-down" title="Down-regulated genes (paired with an up list)">▼ down</span> : null} <span className="muted small">— {l.size} IDs{l.ranked ? ', ranked' : ''}, {l.organismName.replace(/ \[NCBI.*$/, '')}</span></span>
                  </label>
                ))}
              </div>
              {usableLists.length > 1 && (
                <span className="row">
                  <button type="button" className="btn btn-ghost btn-sm" onClick={() => setListIds(usableLists.map((l) => l.id))}>Select all</button>
                  <button type="button" className="btn btn-ghost btn-sm" onClick={() => setListIds([])}>None</button>
                </span>
              )}
              {picked.length === 0 && <span className="muted small">Tick at least one list.</span>}
              {(() => {
                const half = picked.find((l) => l.meta?.direction && l.meta?.pairId)
                const pairId = half?.meta.pairId
                return pairId && onUpDown && paradigm === 'ORA' ? (
                  <Note kind="info">These are the <strong>up- and down-regulated</strong> genes {half?.meta.method === 'ranked-split' ? 'of one list with scores' : 'of one volcano plot'}.{' '}
                    <button type="button" className="btn btn-secondary btn-sm" onClick={() => onUpDown(String(pairId))}>Open the up &amp; down GO enrichment step →</button>
                    {' '}runs your chosen tools on both and opens their results together.</Note>
                ) : null
              })()}
              {picked.length === 1 && picked[0].ranked && !picked[0].meta?.direction && onUpDown && (
                <div className="note note-info">
                  <div className="row between wrap">
                    <span><strong>Up &amp; down:</strong> this list has scores. Enrich its up-regulated (positive score) and down-regulated (negative score) genes separately and see both in one figure.</span>
                    {!showSplit && <button type="button" className="btn btn-secondary btn-sm" onClick={() => setShowSplit(true)}>Up &amp; down enrichment…</button>}
                  </div>
                  {showSplit && <SplitUpDown key={picked[0].id} list={picked[0]} lists={lists} onPair={onUpDown} />}
                </div>
              )}
              {mixedOrganisms && <Note kind="warning">The selected lists are from different organisms. Tool availability below is shown for {list?.organismName.replace(/ \[NCBI.*$/, '')}; a tool that does not support another list's organism is skipped for that list and reported.</Note>}
            </Field>
          )}
        </section>

        <section>
          <div className="row between">
            <h3>2 · Which tools</h3>
            <span className="row">
              <button className="btn btn-ghost btn-sm" title="Select every tool that offers this analysis type and supports the list's organism"
                onClick={() => setTools((config?.tools ?? []).filter((t) => toolOffers(t.key) && supported(t.key)).map((t) => t.key))}>Select all</button>
              <button className="btn btn-ghost btn-sm" onClick={() => setTools([])}>Clear</button>
            </span>
          </div>
          <div className="chip-grid">
            {(config?.tools ?? []).map((t) => {
              if (!toolOffers(t.key)) return null
              const ok = supported(t.key)
              const on = tools.includes(t.key)
              return (
                <button key={t.key} type="button" className={`tool-chip${on ? ' on' : ''}`} disabled={!ok} aria-pressed={on}
                  title={ok ? `${t.name}: ${t.organismCount.toLocaleString()} organisms` : `${t.name} does not support this organism`}
                  style={{ ['--chip-color' as string]: t.color }}
                  onClick={() => setTools((s) => (on ? s.filter((x) => x !== t.key) : [...s, t.key]))}>
                  <ToolLogo tool={t.key} size={18} />{t.name}{!ok && <span className="small"> (not for this organism)</span>}
                </button>
              )
            })}
          </div>
          {(
            <div className="own-sets">
              <div className="row between wrap">
                <span className="field-label">{paradigm === 'GSEA' ? 'Your own signatures' : 'Your own pathway sets'} <span className="muted small">optional</span></span>
                <span className="row">
                  <button type="button" className="btn btn-ghost btn-sm" onClick={() => setShowSignature(true)} title="Paste the genes of one signature (e.g. from a paper or your own experiment)">+ Paste a signature</button>
                  <button type="button" className="btn btn-ghost btn-sm" onClick={() => setShowGmt(true)} title="Upload a collection of gene sets as a GMT file">+ Upload GMT</button>
                </span>
              </div>
              {sources.length === 0
                ? <p className="muted small">{paradigm === 'GSEA'
                  ? 'Test your ranking against your own signature: paste its genes (from a paper, another experiment or a cell type) or upload several as a GMT file. Flame runs GSEA on them as a separate run, always reports them (significant or not) and draws their enrichment plot.'
                  : 'Besides the tools above, you can test your list against your own gene sets (pathways or signatures): paste one signature or upload a collection as a GMT file. Flame tests them itself, as a separate run.'}</p>
                : (
                  <div className="chip-grid">
                    {sources.map((s) => {
                      const on = customIds.includes(s.id)
                      return (
                        <span key={s.id} className="own-set">
                          <button type="button" className={`tool-chip${on ? ' on' : ''}`} aria-pressed={on}
                            onClick={() => setCustomIds((c) => (on ? c.filter((x) => x !== s.id) : [...c, s.id]))}>
                            <i aria-hidden="true" />{s.name} <span className="muted small">{s.termCount ?? s.term_count} sets</span>
                          </button>
                          <button type="button" className="icon-btn" aria-label={`Delete ${s.name}`} title={`Delete the collection "${s.name}" from this workspace`}
                            onClick={() => { setCustomIds((c) => c.filter((x) => x !== s.id)); api(`/sources/${s.id}`, { method: 'DELETE' }).then(invalidateWs) }}>×</button>
                        </span>
                      )
                    })}
                  </div>
                )}
            </div>
          )}
        </section>

        <section>
          <div className="row between">
            <h3>3 · Where to look</h3>
            <span className="row">
              <button className="btn btn-ghost btn-sm" onClick={() => setDatasources([...supportedSources])}>Select all</button>
              <button className="btn btn-ghost btn-sm" onClick={() => setDatasources((paradigm === 'GSEA' ? config?.gseaDefaultDatasources : config?.defaultDatasources) ?? [])}>Defaults</button>
              {cellTypeSources.length > 0 && <button className="btn btn-ghost btn-sm" title="PanglaoDB, CellMarker, Azimuth, Tabula Sapiens and MSigDB C8 marker sets (Enrichr and clusterProfiler)"
                onClick={() => { setParadigm('ORA'); setTools((t) => [...new Set([...t, 'enrichr', 'clusterprofiler'])]); setDatasources(cellTypeSources) }}>Cell types</button>}
              {literatureSources.length > 0 && <button className="btn btn-ghost btn-sm" title="PubMed (STRING), GeneSigDB and Rummagene (Enrichr): published gene sets"
                onClick={() => { setParadigm('ORA'); setTools((t) => [...new Set([...t, 'string', 'enrichr'])]); setDatasources(literatureSources) }}>Literature</button>}
              <button className="btn btn-ghost btn-sm" onClick={() => setDatasources([])}>Clear</button>
            </span>
          </div>
          {paradigm === 'GSEA' ? (
            <div className="source-groups">
              <p className="muted small">Gene sets from the Molecular Signatures Database (MSigDB), grouped by its collections. Hallmark is the usual first choice; large collections (CGP, GO, ImmuneSigDB) take longer and need stronger evidence after multiple-testing correction.</p>
              {(config?.msigdbCollections ?? []).map((cl) => {
                const n = cl.sources.filter((s) => chosenSources.includes(s.id)).length
                return (
                  <details key={cl.code} open={n > 0 || cl.code === 'H'}>
                    <summary><strong>{cl.code}</strong> {cl.name} <span className="muted small">({n}/{cl.sources.length}) · {cl.text}</span></summary>
                    <div className="source-list">
                      {cl.sources.map((s) => (
                        <label key={s.id} className={`check${supportedSources.has(s.id) ? '' : ' muted'}`}>
                          <input type="checkbox" disabled={!supportedSources.has(s.id)} checked={chosenSources.includes(s.id)}
                            onChange={() => setDatasources((d) => (d.includes(s.id) ? d.filter((x) => x !== s.id) : [...d, s.id]))} />
                          {s.label}{s.sets != null && <span className="muted small"> · {s.sets.toLocaleString()} sets</span>}
                        </label>
                      ))}
                    </div>
                  </details>
                )
              })}
            </div>
          ) : (
          <div className="source-groups">
            {(config?.datasourceCategories ?? []).map((cat) => {
              const avail = cat.sources.filter((s) => supportedSources.has(s.id))
              return (
                <details key={cat.category} open={cat.sources.some((s) => chosenSources.includes(s.id)) || cat.category.startsWith('Gene Ontology')}>
                  <summary>{cat.category} <span className="muted small">({cat.sources.filter((s) => chosenSources.includes(s.id)).length}/{avail.length})</span></summary>
                  <div className="source-list">
                    {cat.sources.map((s) => {
                      const usable = supportedSources.has(s.id)
                      const by = tools.filter((k) => toolCfg(k)?.datasources.includes(s.id))
                      return (
                        <label key={s.id} className={`check${usable ? '' : ' muted'}`} title={usable ? `Supported by ${by.map((k) => toolCfg(k)?.name).join(', ')}` : 'None of the selected tools offer this source'}>
                          <input type="checkbox" disabled={!usable} checked={chosenSources.includes(s.id)}
                            onChange={() => setDatasources((d) => (d.includes(s.id) ? d.filter((x) => x !== s.id) : [...d, s.id]))} />
                          {s.label}
                          <span className="dots" aria-hidden="true">{by.map((k) => <i key={k} style={{ background: toolCfg(k)?.color }} />)}</span>
                        </label>
                      )
                    })}
                  </div>
                </details>
              )
            })}
          </div>
          )}
        </section>

        <section>
          <div className="row between">
            <h3>4 · Significance</h3>
            <button className="btn btn-ghost btn-sm" onClick={() => setShowAdvanced((s) => !s)} aria-expanded={showAdvanced}>
              {showAdvanced ? 'Hide' : 'Show'} per-tool settings
            </button>
          </div>
          <Field label="Keep terms with corrected p-value below" hint="0.05 is the usual choice. Each tool applies its default multiple-testing correction.">
            <input className="input input-narrow" type="number" min="0.0001" max="1" step="0.01" value={threshold} onChange={(e) => setThreshold(Number(e.target.value))} />
          </Field>
          {showAdvanced && (
            <div className="per-tool">
              {[...tools, ...(customIds.length ? ['custom'] : [])].map((k) => {
                const t = k === 'custom' ? { key: 'custom', name: 'Custom GMT', metrics: config?.customSourceTool.metrics ?? [], supportsBackground: true, defaultMetricGenome: 'BH' } : toolCfg(k)!
                const s = perTool[k] ?? {}
                return (
                  <fieldset key={k} className="tool-settings">
                    <legend><ToolLogo tool={t.key} size={16} /> {t.name}</legend>
                    <Field label="Correction method">
                      <select className="input" value={s.correction ?? t.defaultMetricGenome} onChange={(e) => setTool(k, { correction: e.target.value })}>
                        {t.metrics.map((m) => <option key={m.value} value={m.value}>{METRIC_HELP[m.value] ?? m.label}</option>)}
                      </select>
                    </Field>
                    <Field label="Threshold">
                      <input className="input input-narrow" type="number" min="0.0001" max="1" step="0.01" value={s.threshold ?? threshold} onChange={(e) => setTool(k, { threshold: Number(e.target.value) })} />
                    </Field>
                    {paradigm === 'ORA' && <Field label="Background (statistical universe)" hint={t.supportsBackground ? 'By default, all annotated genes of the organism. Use a list when your experiment measured only some genes.' : 'This tool always uses its own background.'}>
                      <select className="input" disabled={!t.supportsBackground} value={s.backgroundListId ?? ''} onChange={(e) => setTool(k, { backgroundListId: e.target.value || undefined })}>
                        <option value="">Tool default (whole genome)</option>
                        {backgroundOptions.map((l) => <option key={l.id} value={l.id}>{l.name} ({l.size} IDs)</option>)}
                      </select>
                    </Field>}
                  </fieldset>
                )
              })}
            </div>
          )}
        </section>
        <ErrorNote error={start.error} />
        {progress && <BusyBar label={progress} expect={6} />}
        {showGmt && <GmtModal onClose={() => setShowGmt(false)} onCreated={(id) => setCustomIds((c) => [...c, id])} />}
        {showSignature && <SignatureModal onClose={() => setShowSignature(false)} onCreated={(id) => setCustomIds((c) => [...c, id])} />}
        {result && (
          <Note kind={result.skipped.length ? 'warning' : 'success'}>
            {result.created.length > 0 && `Started ${result.created.length} run(s). `}
            {result.reused.length > 0 && `${result.reused.length} identical run(s) already exist and were reused. `}
            {result.skipped.map((s) => `${s.tool}: ${s.reason}`).join(' ')}
          </Note>
        )}
      </div>
    </Modal>
  )
}

/** Upload a collection of gene sets (GMT) to test lists against; opened from the New run panel. */
/** One signature: a name and its genes, pasted or from a file. Saved as a custom source with one gene set. */
function SignatureModal({ onClose, onCreated }: { onClose: () => void; onCreated: (sourceId: string) => void }) {
  const [name, setName] = useState('My signature')
  const [genes, setGenes] = useState('')
  const [err, setErr] = useState<unknown>(null)
  const invalidate = useInvalidateWorkspace()
  const n = useMemo(() => new Set(genes.split(/[\s,;]+/).map((g) => g.trim().toUpperCase()).filter(Boolean)).size, [genes])
  return (
    <Modal title="Your own signature" onClose={onClose} palette={false}
      subtitle="A set of genes you want to test, e.g. a published signature, the up-regulated genes of another experiment or the markers of a cell type."
      footer={<>
        <button className="btn btn-ghost" onClick={onClose}>Cancel</button>
        <button className="btn btn-primary" disabled={n < 5 || !name.trim()} onClick={() =>
          api<{ id: string }>('/sources', { body: { name: name.trim(), genes } }).then((s) => { invalidate(); onCreated(s.id); onClose() }).catch(setErr)}>
          Add signature ({n} gene{n === 1 ? '' : 's'})</button>
      </>}>
      <label className="field"><span className="field-label">Name</span>
        <input className="input" value={name} maxLength={80} onChange={(e) => setName(e.target.value)} /></label>
      <label className="field"><span className="field-label">Genes <span className="muted small">one per line, or separated by commas or spaces; at least 5</span></span>
        <textarea className="input mono" rows={8} value={genes} onChange={(e) => setGenes(e.target.value)} placeholder={'FKBP5\nTSC22D3\nPER1\nKLF15\nZBTB16'} aria-label="Signature genes" /></label>
      <label className="field"><span className="field-label">…or a file</span>
        <input type="file" accept=".txt,.csv,.tsv" onChange={(e) => e.target.files?.[0]?.text().then(setGenes)} /></label>
      <p className="muted small">Use the same kind of IDs as your lists (gene symbols work for most). With a ranked list, GSEA asks whether these genes sit at the top or the bottom of your ranking; open the run's <em>GSEA enrichment plot</em> to see it. With a plain list, Flame tests the overlap (ORA).</p>
      <ErrorNote error={err} />
    </Modal>
  )
}

function GmtModal({ onClose, onCreated }: { onClose: () => void; onCreated?: (sourceId: string) => void }) {
  const [name, setName] = useState('My gene sets')
  const [text, setText] = useState('')
  const [err, setErr] = useState<unknown>(null)
  const invalidate = useInvalidateWorkspace()
  const sets = useMemo(() => text.split(/\r?\n/).filter((l) => l.split('\t').length >= 3).length, [text])
  return (
    <Modal title="Upload your own pathway sets (GMT)" onClose={onClose} palette={false}
      subtitle="A GMT file holds many named gene sets, one per line. Enrichment tests your list against each of them, as it does for GO terms or KEGG pathways."
      footer={<>
        <button className="btn btn-ghost" onClick={onClose}>Cancel</button>
        <button className="btn btn-primary" disabled={!sets} onClick={() =>
          api<{ id: string }>('/sources', { body: { name, text } }).then((s) => { invalidate(); onCreated?.(s.id); onClose() }).catch(setErr)}>Add collection ({sets} gene sets)</button>
      </>}>
      <Note kind="info">
        <strong>Use this for</strong> your own pathways, published signatures or a collection Flame does not offer (e.g. MSigDB Hallmark). <strong>Not for a background:</strong> to restrict the statistical universe, add those genes as a normal list and choose it under <em>Background</em> in a run's per-tool settings. <strong>Works with</strong> over-representation (ORA) and, for ranked lists, GSEA runs.
      </Note>
      <p className="small">Format: each line is <code>set ID ⇥ description ⇥ gene ⇥ gene …</code> (tab-separated, no header). The gene IDs must be of the same type as the IDs in your lists. <a href={`${API_BASE}/inputs/examples/gmt/file`}>Download an example file</a> (MSigDB Hallmark).</p>
      <label className="field"><span className="field-label">Name of the collection</span>
        <input className="input" value={name} maxLength={80} onChange={(e) => setName(e.target.value)} /></label>
      <div className="row end">
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => api<{ text: string }>('/inputs/examples/gmt').then((ex) => { setText(ex.text); setName('MSigDB Hallmark (example)') }).catch(setErr)}>
          Load example (MSigDB Hallmark, 50 sets)</button>
      </div>
      <label className="field"><span className="field-label">GMT file</span>
        <input type="file" accept=".gmt,.txt,.tsv" onChange={(e) => e.target.files?.[0]?.text().then(setText)} /></label>
      <TableTextArea rows={9} value={text} onChange={setText} maxCols={250} header={false} placeholder={'HALLMARK_APOPTOSIS\tapoptosis\tCASP3\tCASP8\tBAX\tBCL2L11\tFAS\t…\nHALLMARK_HYPOXIA\thypoxia\tVEGFA\tSLC2A1\tPGK1\tLDHA\tADM\t…\nHALLMARK_GLYCOLYSIS\tglycolysis\tHK2\tPFKP\tALDOA\tENO1\tPKM\tLDHA\t…\nHALLMARK_P53_PATHWAY\tp53 pathway\tCDKN1A\tMDM2\tBAX\tGADD45A\tSESN1\t…\nHALLMARK_TNFA_SIGNALING_VIA_NFKB\tTNF-alpha signaling via NF-kB\tNFKBIA\tTNFAIP3\tJUN\tFOS\tIL6\t…\nHALLMARK_INTERFERON_GAMMA_RESPONSE\tinterferon gamma response\tSTAT1\tIRF1\tCXCL10\tIDO1\t…\nHALLMARK_MYC_TARGETS_V1\tMYC targets v1\tNPM1\tCDK4\tLDHA\tHSPD1\tEIF4A1\t…'} ariaLabel="GMT content" />
      <ErrorNote error={err} />
    </Modal>
  )
}
