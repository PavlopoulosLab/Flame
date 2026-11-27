import { useQuery } from '@tanstack/react-query'
import { useMemo, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { api } from '../api/client'
import { useConfig, useCreateList, useStartRuns, useWorkspace } from '../api/hooks'
import type { AnalyteList, Run } from '../api/types'
import { ErrorNote, Field, Modal, Note } from '../components/ui'
import { ToolLogo } from '../components/ToolLogo'
import { MAX_LIST_IDS, splitByScore } from '../lib/updown'

const GO = [
  { id: 'GO:BP', label: 'Biological process (BP)' },
  { id: 'GO:MF', label: 'Molecular function (MF)' },
  { id: 'GO:CC', label: 'Cellular component (CC)' },
]
const PATHWAYS = [
  { id: 'KEGG', label: 'KEGG' },
  { id: 'REAC', label: 'Reactome' },
  { id: 'WP', label: 'WikiPathways' },
]
const DEFAULT_TOOLS = ['gprofiler', 'enrichr', 'clusterprofiler']

/** The up- and down-regulated lists of one pair (saved from a volcano plot, or split from a ranked list). */
export function pairLists(lists: AnalyteList[], pairId: string) {
  const of = (d: 'up' | 'down') => lists.find((l) => l.meta?.pairId === pairId && l.meta?.direction === d)
  return { up: of('up'), down: of('down') }
}

/**
 * GO enrichment of the up- and down-regulated genes of a volcano pair: the same tools and GO
 * aspects run on both lists, then each tool's two runs open as one combined figure.
 */
export function UpDownRunPanel({ ws, lists, pairId, onClose }: { ws: string; lists: AnalyteList[]; pairId: string; onClose: () => void }) {
  const { data: config } = useConfig()
  const { data: summary } = useWorkspace(ws)
  const navigate = useNavigate()
  const start = useStartRuns()
  const { up, down } = pairLists(lists, pairId)
  const [tools, setTools] = useState<string[]>(DEFAULT_TOOLS)
  const [sources, setSources] = useState<string[]>(['GO:BP', 'GO:MF', 'GO:CC'])
  const [threshold, setThreshold] = useState(0.05)
  // Started runs per list (ids from the server; reused ones count too).
  const [started, setStarted] = useState<{ up: string[]; down: string[] } | null>(null)
  const [skipped, setSkipped] = useState<{ tool: string; reason: string }[]>([])

  const support = useQuery({
    queryKey: ['tool-support', up?.organismTaxId],
    queryFn: () => api<{ tools: { key: string; supported: boolean }[] }>(`/organisms/${up!.organismTaxId}/tools`, { workspace: null }),
    enabled: !!up,
  })
  const supported = (key: string) => support.data?.tools.find((t) => t.key === key)?.supported ?? true
  // Tools that offer over-representation on at least one of the chosen annotation sources.
  const goTools = (config?.tools ?? []).filter((t) => t.paradigms.includes('ORA') && t.datasources.some((d) => GO.some((g) => g.id === d)))
  const picked = tools.filter((k) => goTools.some((t) => t.key === k) && supported(k))
  const cutoffs = (up?.meta?.cutoffs ?? {}) as { log2fc?: number; pvalue?: number; score?: number; top?: number }

  if (!up || !down) {
    return (
      <Modal title="GO enrichment of up- and down-regulated genes" onClose={onClose}>
        <ErrorNote error="The up or down list of this pair was deleted. Save both again from the volcano plot, or split the ranked list again under New run." />
      </Modal>
    )
  }

  const run = async () => {
    const body = (listId: string) => ({ listId, paradigm: 'ORA' as const, tools: picked, sourceIds: [], datasources: sources, threshold, perTool: {} })
    const a = await start.mutateAsync(body(up.id))
    const b = await start.mutateAsync(body(down.id))
    setStarted({ up: [...a.created, ...a.reused], down: [...b.created, ...b.reused] })
    setSkipped([...a.skipped, ...b.skipped].filter((s, i, all) => all.findIndex((x) => x.tool === s.tool) === i))
  }

  const runOf = (id: string) => summary?.runs.find((r) => r.id === id)
  const byTool = (ids: string[]) => new Map(ids.map(runOf).filter((r): r is Run => !!r).map((r) => [r.toolKey, r]))
  const status = (r?: Run) => !r ? '…' : r.status === 'complete' ? `${r.resultCount ?? 0} terms` : r.status === 'error' ? 'failed' : r.progress?.message ? `${r.status}: ${r.progress.message}` : r.status

  const listCard = (l: AnalyteList, d: 'up' | 'down') => (
    <div className={`updown-card updown-${d}`}>
      <span className={`dir-badge dir-${d}`}>{d === 'up' ? '▲ up' : '▼ down'}</span>
      <strong>{l.name}</strong>
      <span className="muted small">{l.size.toLocaleString()} genes{cutoffs.log2fc != null ? ` · log2FC ${d === 'up' ? '>' : '<'} ${d === 'up' ? '' : '−'}${cutoffs.log2fc}, p < ${cutoffs.pvalue}`
        : cutoffs.score != null ? ` · score ${d === 'up' ? '>' : '<'} ${d === 'up' ? '' : '−'}${cutoffs.score}` : ''}</span>
    </div>
  )

  if (started) {
    const upRuns = byTool(started.up), downRuns = byTool(started.down)
    const keys = [...new Set([...upRuns.keys(), ...downRuns.keys()])]
    return (
      <Modal title="GO enrichment of up- and down-regulated genes" subtitle={`${up.name} and ${down.name}`} onClose={onClose} size="lg"
        footer={<button className="btn btn-ghost" onClick={onClose}>Close (runs continue in the background)</button>}>
        <div className="stack">
          <p>Each tool analyses both lists. When both runs of a tool have finished, open its <strong>up &amp; down figure</strong>: the top terms of the up-regulated genes, then of the down-regulated genes, and terms enriched in both shown mirrored.</p>
          <table className="table">
            <thead><tr><th>Tool</th><th><span className="dir-badge dir-up">▲ up</span></th><th><span className="dir-badge dir-down">▼ down</span></th><th /></tr></thead>
            <tbody>
              {keys.map((k) => {
                const u = upRuns.get(k), d = downRuns.get(k)
                const done = u?.status === 'complete' && d?.status === 'complete'
                return (
                  <tr key={k}>
                    <td><ToolLogo tool={k} size={16} /> {u?.tool ?? d?.tool}</td>
                    <td>{status(u)}</td><td>{status(d)}</td>
                    <td className="num">
                      <button className="btn btn-primary btn-sm" disabled={!done} onClick={() => navigate(`/w/${ws}/runs/${encodeURIComponent(u!.id)}/barchart`)}>
                        {done ? 'Open up & down figure' : 'Waiting…'}
                      </button>
                    </td>
                  </tr>
                )
              })}
            </tbody>
          </table>
          {skipped.length > 0 && <Note kind="warning">Not run: {skipped.map((s) => `${s.tool} (${s.reason})`).join('; ')}</Note>}
        </div>
      </Modal>
    )
  }

  const toggle = (arr: string[], set: (v: string[]) => void, id: string) => set(arr.includes(id) ? arr.filter((x) => x !== id) : [...arr, id])
  return (
    <Modal title="GO enrichment of up- and down-regulated genes" subtitle="The same analysis runs on each list; the results are then drawn together." onClose={onClose} size="lg"
      footer={
        <div className="row between" style={{ width: '100%' }}>
          <span className="muted small">{picked.length} tool{picked.length === 1 ? '' : 's'} × 2 lists = {picked.length * 2} runs · {sources.length} annotation source{sources.length === 1 ? '' : 's'}</span>
          <span className="row">
            <button className="btn btn-ghost" onClick={onClose}>Cancel</button>
            <button className="btn btn-primary" disabled={!picked.length || !sources.length || start.isPending} onClick={() => run().catch(() => undefined)}>
              {start.isPending ? 'Starting…' : 'Run GO enrichment on both lists'}
            </button>
          </span>
        </div>
      }>
      <div className="stack">
        <section>
          <h3>1 · Your two lists</h3>
          <div className="updown-cards">{listCard(up, 'up')}{listCard(down, 'down')}</div>
        </section>
        <section>
          <h3>2 · Tools <span className="muted small">(tick several to compare them)</span></h3>
          <div className="chip-grid">
            {goTools.map((t) => {
              const ok = supported(t.key), on = tools.includes(t.key)
              return (
                <button key={t.key} type="button" className={`tool-chip${on ? ' on' : ''}`} disabled={!ok} aria-pressed={on}
                  style={{ ['--chip-color' as string]: t.color }} onClick={() => toggle(tools, setTools, t.key)}>
                  <ToolLogo tool={t.key} size={18} />{t.name}{!ok && <span className="small"> (not for this organism)</span>}
                </button>
              )
            })}
          </div>
        </section>
        <section>
          <h3>3 · What to test</h3>
          <div className="row wrap">
            <span className="field-label">Gene Ontology</span>
            {GO.map((g) => <label key={g.id} className="check"><input type="checkbox" checked={sources.includes(g.id)} onChange={() => toggle(sources, setSources, g.id)} /> {g.label}</label>)}
          </div>
          <div className="row wrap">
            <span className="field-label">Also pathways <span className="muted small">optional</span></span>
            {PATHWAYS.map((g) => <label key={g.id} className="check"><input type="checkbox" checked={sources.includes(g.id)} onChange={() => toggle(sources, setSources, g.id)} /> {g.label}</label>)}
          </div>
          <Field label="Significance threshold (adjusted p)">
            <input className="input input-sm input-narrow" type="number" min={0.0001} max={1} step={0.01} value={threshold} onChange={(e) => setThreshold(Number(e.target.value) || 0.05)} />
          </Field>
          <p className="muted small">Tools that do not offer one of these sources simply skip it.</p>
        </section>
        <ErrorNote error={start.error} />
      </div>
    </Modal>
  )
}

/**
 * Up & down from a ranked list (IDs with signed scores, such as log2 fold changes): positive
 * scores form the up list and negative ones the down list, saved as a linked pair like the
 * volcano's, so the up & down GO step and the combined figure work the same way.
 */
export function SplitUpDown({ list, lists, onPair }: { list: AnalyteList; lists: AnalyteList[]; onPair: (pairId: string) => void }) {
  const create = useCreateList()
  const full = useQuery({
    queryKey: ['list-scores', list.id],
    queryFn: () => api<AnalyteList & { ids: string[]; scores?: number[] }>(`/lists/${encodeURIComponent(list.id)}`),
  })
  const [minAbs, setMinAbs] = useState(0)
  const [maxPer, setMaxPer] = useState(MAX_LIST_IDS)
  const [busy, setBusy] = useState(false)
  const split = useMemo(() => (full.data?.scores ? splitByScore(full.data.ids, full.data.scores.map(Number), minAbs, Math.min(maxPer, MAX_LIST_IDS)) : null), [full.data, minAbs, maxPer])
  // Pairs made earlier: split from this list, or (a volcano's ranked list) saved with it.
  const existing = [...new Set(lists.filter((l) => l.meta?.direction && l.meta?.pairId &&
      (l.meta.sourceListId === list.id || (list.meta?.pairId && l.meta.pairId === list.meta.pairId))).map((l) => String(l.meta.pairId)))]
    .filter((pid) => ['up', 'down'].every((d) => lists.some((l) => l.meta?.pairId === pid && l.meta?.direction === d)))

  const save = async () => {
    if (!split) return
    setBusy(true)
    try {
      const pairId = `p${Date.now().toString(36)}${Math.random().toString(36).slice(2, 7)}`
      const cutoffs = { score: minAbs, top: Math.min(maxPer, MAX_LIST_IDS) }
      const mk = (dir: 'up' | 'down', ids: string[]) => create.mutateAsync({ name: `${list.name} ${dir}`, ids, organismTaxId: list.organismTaxId, idFormat: list.idFormat,
        uniqueName: true, meta: { method: 'ranked-split', direction: dir, pairId, cutoffs, sourceListId: list.id } })
      await mk('up', split.up)
      await mk('down', split.down)
      onPair(pairId)
    } catch { /* shown below */ } finally { setBusy(false) }
  }

  const capped = split && (split.upTotal > split.up.length || split.downTotal > split.down.length)
  const unsigned = split && (!split.upTotal || !split.downTotal) && minAbs === 0
  return (
    <div className="stack-sm">
      {existing.length > 0 && (
        <p className="row wrap">
          <span>Already split:</span>
          {existing.map((pid) => {
            const { up, down } = pairLists(lists, pid)
            return <button key={pid} type="button" className="btn btn-secondary btn-sm" onClick={() => onPair(pid)}>{up?.name} / {down?.name} →</button>
          })}
        </p>
      )}
      {full.isLoading && <p className="muted small">Reading the scores…</p>}
      <ErrorNote error={full.error ?? create.error} />
      {split && (
        <>
          <div className="row wrap">
            <label>Up if score &gt; <input className="input input-sm input-narrow" type="number" min={0} step={0.1} value={minAbs}
              onChange={(e) => setMinAbs(Math.max(0, Number(e.target.value) || 0))} aria-label="Minimum absolute score" />, down if &lt; −{minAbs}</label>
            <label>At most <input className="input input-sm input-narrow" type="number" min={1} max={MAX_LIST_IDS} step={100} value={maxPer}
              onChange={(e) => setMaxPer(Math.min(MAX_LIST_IDS, Math.max(1, Math.floor(Number(e.target.value) || MAX_LIST_IDS))))} aria-label="Most genes per direction" /> genes each (strongest first)</label>
          </div>
          <p>
            <span className="dir-badge dir-up">▲ up</span> <strong>{split.up.length.toLocaleString()}</strong>{split.upTotal > split.up.length ? ` of ${split.upTotal.toLocaleString()}` : ''} genes ·{' '}
            <span className="dir-badge dir-down">▼ down</span> <strong>{split.down.length.toLocaleString()}</strong>{split.downTotal > split.down.length ? ` of ${split.downTotal.toLocaleString()}` : ''} genes
          </p>
          {capped && <p className="muted small">A list holds at most {MAX_LIST_IDS.toLocaleString()} genes; only the strongest are kept. Raise the score cut-off to choose them yourself.</p>}
          {unsigned && <Note kind="warning">All scores have the same sign. Up &amp; down needs signed scores, such as log2 fold changes or sign(log2FC) × −log10 p.</Note>}
          <div className="row end">
            <button type="button" className="btn btn-primary btn-sm" disabled={busy || !split.up.length || !split.down.length} onClick={() => save()}>
              {busy ? 'Saving…' : 'Save as up & down lists and continue →'}
            </button>
          </div>
        </>
      )}
    </div>
  )
}
