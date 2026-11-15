import { useQuery } from '@tanstack/react-query'
import { useEffect, useMemo, useState } from 'react'
import { api } from '../api/client'
import { useConfig, useInvalidateWorkspace, useStartRuns } from '../api/hooks'
import type { AnalyteList, CustomSource } from '../api/types'
import { ErrorNote, Field, Modal, Note, Segmented } from '../components/ui'

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

export function NewRunPanel({ lists, sources, initialListId, onClose }: { lists: AnalyteList[]; sources: CustomSource[]; initialListId?: string; onClose: () => void }) {
  const { data: config } = useConfig()
  const start = useStartRuns()
  const invalidateWs = useInvalidateWorkspace()
  const [loadingExample, setLoadingExample] = useState(false)
  const initial = lists.find((l) => l.id === initialListId)
  const [paradigm, setParadigm] = useState<'ORA' | 'GSEA'>(initial?.ranked ? 'GSEA' : 'ORA')
  const usableLists = paradigm === 'GSEA' ? lists.filter((l) => l.ranked) : lists
  const [listId, setListId] = useState(initialListId ?? lists[0]?.id ?? '')
  const list = usableLists.find((l) => l.id === listId) ?? usableLists[0]
  const [tools, setTools] = useState<string[]>(initial?.ranked ? ['clusterprofiler'] : ['gprofiler'])
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
    if (config && !datasources.length) setDatasources(config.defaultDatasources)
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
  const backgroundOptions = lists.filter((l) => l.id !== listId && l.organismTaxId === list?.organismTaxId)
  const canSubmit = !!list && (tools.length > 0 || customIds.length > 0) && (customIds.length > 0 || chosenSources.length > 0)
    && (paradigm === 'ORA' || !!list?.ranked)

  const submit = () => {
    if (!list) return
    const body = {
      listId: list.id, paradigm, tools, sourceIds: customIds, datasources: chosenSources, threshold,
      perTool: Object.fromEntries(Object.entries(perTool).map(([k, v]) => [k, { ...v, threshold: v.threshold ?? threshold }])),
    }
    start.mutate(body, { onSuccess: (r) => { setResult(r); if (!r.skipped.length) setTimeout(onClose, 900) } })
  }

  const setTool = (key: string, patch: ToolSettings) => setPerTool((p) => ({ ...p, [key]: { ...p[key], ...patch } }))

  return (
    <Modal title="New enrichment run" size="xl" onClose={onClose}
      subtitle="Each selected tool runs separately; you can compare them afterwards."
      footer={<>
        <span className="muted small">{tools.length + customIds.length} run{tools.length + customIds.length === 1 ? '' : 's'} · {chosenSources.length} sources</span>
        <button className="btn btn-ghost" onClick={onClose}>Cancel</button>
        <button className="btn btn-primary" disabled={!canSubmit || start.isPending} onClick={submit}>{start.isPending ? 'Starting…' : 'Run enrichment'}</button>
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
                  setListId(l.id)
                } finally { setLoadingExample(false) }
              }}>{loadingExample ? 'Loading…' : 'Load an example ranked list (6,000 genes from a differential-expression table)'}</button>
            </div>
          ) : (
            <Field label="List">
              <select className="input" value={list?.id ?? ''} onChange={(e) => setListId(e.target.value)}>
                {usableLists.map((l) => <option key={l.id} value={l.id}>{l.name} — {l.size} IDs{l.ranked ? ', ranked' : ''}, {l.organismName.replace(/ \[NCBI.*$/, '')}</option>)}
              </select>
            </Field>
          )}
        </section>

        <section>
          <h3>2 · Which tools</h3>
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
                  <i aria-hidden="true" />{t.name}{!ok && <span className="small"> (not for this organism)</span>}
                </button>
              )
            })}
          </div>
          {paradigm === 'ORA' && sources.length > 0 && (
            <div className="chip-grid">
              {sources.map((s) => {
                const on = customIds.includes(s.id)
                return (
                  <button key={s.id} type="button" className={`tool-chip${on ? ' on' : ''}`} aria-pressed={on}
                    onClick={() => setCustomIds((c) => (on ? c.filter((x) => x !== s.id) : [...c, s.id]))}>
                    <i aria-hidden="true" />Custom: {s.name}
                  </button>
                )
              })}
            </div>
          )}
        </section>

        <section>
          <div className="row between">
            <h3>3 · Where to look</h3>
            <span className="row">
              <button className="btn btn-ghost btn-sm" onClick={() => setDatasources([...supportedSources])}>Select all</button>
              <button className="btn btn-ghost btn-sm" onClick={() => setDatasources(config?.defaultDatasources ?? [])}>Defaults</button>
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
                    <legend>{t.name}</legend>
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
