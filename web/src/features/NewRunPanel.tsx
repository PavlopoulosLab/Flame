import { useQuery } from '@tanstack/react-query'
import { useEffect, useMemo, useState } from 'react'
import { API_BASE, api } from '../api/client'
import { useConfig, useInvalidateWorkspace, useStartRuns } from '../api/hooks'
import type { AnalyteList, CustomSource } from '../api/types'
import { BusyBar, ErrorNote, Field, Modal, Note, Segmented } from '../components/ui'
import { ToolLogo } from '../components/ToolLogo'

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

export function NewRunPanel({ lists, sources, initialListId, initialListIds, onClose, preset }: { lists: AnalyteList[]; sources: CustomSource[]; initialListId?: string; /** Several lists ticked in the lists panel: the same analysis is started on each. */ initialListIds?: string[]; onClose: () => void; preset?: 'celltypes' | 'literature' }) {
  const { data: config } = useConfig()
  const cellTypeSources = config?.datasourceCategories.find((c) => c.category === 'Cell types')?.sources.map((s) => s.id) ?? []
  const literatureSources = config?.datasourceCategories.find((c) => c.category === 'Literature')?.sources.map((s) => s.id) ?? []
  const start = useStartRuns()
  const invalidateWs = useInvalidateWorkspace()
  const [loadingExample, setLoadingExample] = useState(false)
  const startIds = (initialListIds?.length ? initialListIds : initialListId ? [initialListId] : []).filter((id) => lists.some((l) => l.id === id))
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
  const [tools, setTools] = useState<string[]>(preset === 'celltypes' ? ['enrichr', 'clusterprofiler'] : preset === 'literature' ? ['string', 'enrichr'] : initial?.ranked ? ['clusterprofiler'] : ['gprofiler'])
  const toolOffers = (key: string) => (config?.tools.find((t) => t.key === key)?.paradigms ?? ['ORA']).includes(paradigm)
  useEffect(() => {
    setTools((ts) => {
      const kept = ts.filter(toolOffers)
      return kept.length ? kept : paradigm === 'GSEA' ? ['clusterprofiler'] : ['gprofiler']
    })
    if (paradigm === 'GSEA') setCustomIds([])
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [paradigm, config])
  const [customIds, setCustomIds] = useState<string[]>([])
  const [datasources, setDatasources] = useState<string[]>([])
  const [threshold, setThreshold] = useState(0.05)
  const [perTool, setPerTool] = useState<Record<string, ToolSettings>>({})
  const [showAdvanced, setShowAdvanced] = useState(false)
  const [result, setResult] = useState<{ created: string[]; reused: string[]; skipped: { tool: string; reason: string }[] } | null>(null)

  useEffect(() => {
    if (config && !datasources.length) setDatasources(preset === 'celltypes' ? cellTypeSources : preset === 'literature' ? literatureSources : config.defaultDatasources)
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
    <Modal title="New enrichment run" size="xl" onClose={onClose}
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
              }}>{loadingExample ? 'Loading…' : 'Load an example ranked list (6,000 genes from a differential-expression table)'}</button>
            </div>
          ) : (
            <Field label={picked.length > 1 ? `Lists (${picked.length} selected — the same analysis runs on each)` : 'Lists (tick several to run the same analysis on each)'}>
              <div className="run-lists">
                {usableLists.map((l) => (
                  <label key={l.id} className="check">
                    <input type="checkbox" checked={listIds.includes(l.id)} onChange={() => setListIds((cur) => (cur.includes(l.id) ? cur.filter((x) => x !== l.id) : [...cur, l.id]))} />
                    <span>{l.name} <span className="muted small">— {l.size} IDs{l.ranked ? ', ranked' : ''}, {l.organismName.replace(/ \[NCBI.*$/, '')}</span></span>
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
          {paradigm === 'ORA' && (
            <div className="own-sets">
              <div className="row between wrap">
                <span className="field-label">Your own pathway sets <span className="muted small">optional</span></span>
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => setShowGmt(true)} title="Upload a collection of gene sets as a GMT file">+ Upload GMT</button>
              </div>
              {sources.length === 0
                ? <p className="muted small">Besides the tools above, you can test your list against your own collection of gene sets (pathways or signatures) uploaded as a GMT file. Flame tests them itself, as a separate run.</p>
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
              <button className="btn btn-ghost btn-sm" onClick={() => setDatasources(config?.defaultDatasources ?? [])}>Defaults</button>
              {cellTypeSources.length > 0 && <button className="btn btn-ghost btn-sm" title="PanglaoDB, CellMarker, Azimuth, Tabula Sapiens and MSigDB C8 marker sets (Enrichr and clusterProfiler)"
                onClick={() => { setParadigm('ORA'); setTools((t) => [...new Set([...t, 'enrichr', 'clusterprofiler'])]); setDatasources(cellTypeSources) }}>Cell types</button>}
              {literatureSources.length > 0 && <button className="btn btn-ghost btn-sm" title="PubMed (STRING), GeneSigDB and Rummagene (Enrichr): published gene sets"
                onClick={() => { setParadigm('ORA'); setTools((t) => [...new Set([...t, 'string', 'enrichr'])]); setDatasources(literatureSources) }}>Literature</button>}
              <button className="btn btn-ghost btn-sm" onClick={() => setDatasources([])}>Clear</button>
            </span>
          </div>
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
        {showGmt && <GmtModal onClose={() => setShowGmt(false)} />}
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
function GmtModal({ onClose }: { onClose: () => void }) {
  const [name, setName] = useState('My gene sets')
  const [text, setText] = useState('')
  const [err, setErr] = useState<unknown>(null)
  const invalidate = useInvalidateWorkspace()
  const sets = useMemo(() => text.split(/\r?\n/).filter((l) => l.split('\t').length >= 3).length, [text])
  return (
    <Modal title="Upload your own pathway sets (GMT)" onClose={onClose}
      subtitle="A GMT file holds many named gene sets, one per line. Enrichment tests your list against each of them, as it does for GO terms or KEGG pathways."
      footer={<>
        <button className="btn btn-ghost" onClick={onClose}>Cancel</button>
        <button className="btn btn-primary" disabled={!sets} onClick={() =>
          api('/sources', { body: { name, text } }).then(() => { invalidate(); onClose() }).catch(setErr)}>Add collection ({sets} gene sets)</button>
      </>}>
      <Note kind="info">
        <strong>Use this for</strong> your own pathways, published signatures or a collection Flame does not offer (e.g. MSigDB Hallmark). <strong>Not for a background:</strong> to restrict the statistical universe, add those genes as a normal list and choose it under <em>Background</em> in a run's per-tool settings. <strong>Works with</strong> over-representation (ORA) runs; ranked-list GSEA runs use the built-in MSigDB collections and cannot use an uploaded file.
      </Note>
      <p className="small">Format: each line is <code>set ID ⇥ description ⇥ gene ⇥ gene …</code> (tab-separated, no header). The gene IDs must be of the same type as the IDs in your lists. <a href={`${API_BASE}/inputs/examples/gmt/file`}>Download an example file</a> (MSigDB Hallmark).</p>
      <label className="field"><span className="field-label">Name of the collection</span>
        <input className="input" value={name} maxLength={80} onChange={(e) => setName(e.target.value)} /></label>
      <label className="field"><span className="field-label">GMT file</span>
        <input type="file" accept=".gmt,.txt,.tsv" onChange={(e) => e.target.files?.[0]?.text().then(setText)} /></label>
      <textarea className="input mono" rows={8} value={text} onChange={(e) => setText(e.target.value)} placeholder={'HALLMARK_APOPTOSIS\tApoptosis\tCASP3\tCASP8\tBAX\t…'} aria-label="GMT content" />
      <ErrorNote error={err} />
    </Modal>
  )
}
