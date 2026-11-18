import { keepPreviousData, useQuery } from '@tanstack/react-query'
import { useRef, useState } from 'react'
import { api, getWorkspaceId } from '../../api/client'
import { Plot } from '../../components/charts'
import { ToolLogo } from '../../components/ToolLogo'
import { CopyButton, ErrorNote, Spinner } from '../../components/ui'
import { usePlainSelection } from '../../lib/imageExport'
import { WordCloud, type CloudWord } from '../../components/WordCloud'
import { saveXlsx } from '../../lib/xlsx'
import { formatP } from '../../lib/format'

interface Theme {
  id: string; label: string; leadTerm: string; size: number; bestP: number; score: number; terms: string[]; genes: string[]
  sources: Record<string, number>; keywords: CloudWord[]; cohesion: number; runs?: Record<string, { terms: number }>
}
interface ThemesResult {
  themes: Theme[]; words: CloudWord[]; termCount: number; cutoff: number; method: string
  runs?: { id: string; label: string; tool: string; listName: string }[]; activeRunIds?: string[]
}

/**
 * Functional themes: overlapping terms grouped by shared genes, each named by
 * its most significant term and summarised by weighted keywords, plus a word
 * cloud of all terms. Works for one run or across compared runs.
 */
export function ThemesView({ path, body, terms, onSelect, colorOf, baseName }: {
  path: string; body: Record<string, unknown>; terms: { id: string; name: string }[]
  onSelect: (ids: string[], additive: boolean) => void; colorOf: (source: string) => string; baseName: string
}) {
  const [cutoff, setCutoff] = useState(0.35)
  const [word, setWord] = useState<string | null>(null)
  const shownWord = usePlainSelection(word, null)
  const [showAll, setShowAll] = useState(false)
  const cloudRef = useRef<SVGSVGElement | null>(null)
  const q = useQuery({
    queryKey: ['themes', getWorkspaceId(), path, JSON.stringify(body), cutoff],
    queryFn: () => api<ThemesResult>(path, { body: { ...body, cutoff } }),
    placeholderData: keepPreviousData,
  })
  const d = q.data
  const pickWord = (w: string) => {
    const next = word === w ? null : w
    setWord(next)
    if (next) onSelect(terms.filter((t) => t.name.toLowerCase().includes(next)).map((t) => t.id), false)
  }
  if (q.isLoading) return <Spinner label="Grouping terms into themes" />
  if (q.error) return <ErrorNote error={q.error} />
  if (!d) return null
  const runs = (d.runs ?? []).filter((r) => (d.activeRunIds ?? []).includes(r.id))
  const shown = showAll ? d.themes : d.themes.slice(0, 24)
  const maxScore = Math.max(1, ...d.themes.map((t) => t.score))
  return (
    <div className="stack themes-view">
      <div className="themes-top">
        <div className="chart-card">
          <div className="row between wrap">
            <h3>Word cloud of all {d.termCount.toLocaleString()} terms</h3>
            <span className="row">
              {word && <button className="chip on" onClick={() => { setWord(null); onSelect([], false) }}>{word} ×</button>}
              <button className="btn btn-ghost btn-sm" title="Excel workbook: themes with their genes, the terms of each theme, and the word weights" onClick={() => saveXlsx([
                { name: 'Themes', rows: d.themes.map((t) => ({ Theme: t.label, Terms: t.size, Genes: t.genes.join(', '), 'Gene count': t.genes.length, 'Best p': t.bestP, Score: t.score,
                  Overlap: t.cohesion, Keywords: t.keywords.map((k) => k.word).join(', '), Sources: Object.entries(t.sources).map(([k, n]) => `${k} ${n}`).join('; ') })) },
                { name: 'Terms by theme', rows: d.themes.flatMap((t) => t.terms.map((id) => ({ Theme: t.label, 'Term ID': id, Term: terms.find((x) => x.id === id)?.name ?? '' }))) },
                { name: 'Word cloud', rows: d.words.map((w) => ({ Word: w.word, Weight: w.weight })) },
              ], `${baseName}_themes.xlsx`)}>Excel</button>
            </span>
          </div>
          <WordCloud words={d.words} onWord={pickWord} active={shownWord} svgRef={cloudRef} width={620} height={280} exportName={`${baseName}_wordcloud`} />
          <p className="muted small">Word size = summed −log10 p of the terms using the word (common words such as “regulation” or “process” are left out). Click a word to select its terms.</p>
        </div>
        <div className="chart-card themes-summary">
          <h3>{d.themes.length} themes</h3>
          <label className="small">Group terms that share at least <strong>{Math.round(cutoff * 100)}%</strong> of their genes
            <input type="range" min={0.1} max={0.8} step={0.05} value={cutoff} onChange={(e) => setCutoff(Number(e.target.value))} aria-label="Gene overlap needed to group terms" />
          </label>
          <p className="muted small">Lower = fewer, broader themes; higher = more, tighter themes. {d.method}</p>
          {runs.length > 0 && (
            <Plot exportName={`${baseName}_themes`} height={Math.max(220, 26 * Math.min(d.themes.length, 20) + 90)} data={[{
              type: 'heatmap', x: runs.map((r) => r.label), y: d.themes.slice(0, 20).map((t) => (t.label.length > 40 ? `${t.label.slice(0, 39)}…` : t.label)),
              z: d.themes.slice(0, 20).map((t) => runs.map((r) => t.runs?.[r.id]?.terms ?? 0)),
              customdata: d.themes.slice(0, 20).map((t) => runs.map(() => t.id)), colorscale: [[0, '#f5f3ff'], [1, '#6d28d9']],
              hovertemplate: '%{y}<br>%{x}: %{z} terms<extra></extra>', showscale: false,
            } as never]} layout={{ margin: { l: 230, r: 10, t: 10, b: 80 }, yaxis: { autorange: 'reversed' } }}
              onSelect={(keys) => { const t = d.themes.find((x) => x.id === keys[0]); if (t) onSelect(t.terms, false) }} />
          )}
        </div>
      </div>
      <div className="theme-grid">
        {shown.map((t) => (
          <article key={t.id} className="theme-card">
            <div className="theme-bar" style={{ width: `${(t.score / maxScore) * 100}%` }} />
            <h3 title={t.label}>{t.label}</h3>
            <p className="muted small">{t.size} term{t.size === 1 ? '' : 's'} · {t.genes.length} genes · best p = {formatP(t.bestP)}{t.size > 1 ? ` · overlap ${Math.round(t.cohesion * 100)}%` : ''}</p>
            <div className="keyword-cloud">
              {t.keywords.slice(0, 12).map((k, i) => {
                const mx = t.keywords[0]?.weight || 1
                return <button key={k.word} type="button" className={`kw${word === k.word ? ' on' : ''}`} style={{ fontSize: `${0.72 + 0.55 * Math.sqrt(k.weight / mx)}rem`, opacity: i < 3 ? 1 : 0.8 }}
                  onClick={() => pickWord(k.word)}>{k.word}</button>
              })}
            </div>
            <div className="row wrap theme-sources">
              {Object.entries(t.sources).map(([s, n]) => <span key={s} className="src-chip" style={{ borderColor: colorOf(s) }}><i style={{ background: colorOf(s) }} />{s} {n}</span>)}
            </div>
            {runs.length > 0 && (
              <div className="row wrap theme-runs">
                {runs.map((r) => {
                  const n = t.runs?.[r.id]?.terms ?? 0
                  return <span key={r.id} className={`found-chip${n ? ' on' : ''}`} title={`${r.label}: ${n} term(s)`}><ToolLogo tool={r.tool} size={14} />{n > 0 && <span className="small">{n}</span>}</span>
                })}
              </div>
            )}
            <div className="row wrap">
              <button className="btn btn-secondary btn-sm" onClick={(e) => onSelect(t.terms, e.shiftKey)}>Select {t.size} terms</button>
              <CopyButton label="Copy genes" text={t.genes.join('\n')} />
              <CopyButton compact title="Copy term names" text={t.terms.map((id) => terms.find((x) => x.id === id)?.name ?? id).join('\n')} />
            </div>
          </article>
        ))}
      </div>
      {d.themes.length > 24 && !showAll && <button className="btn btn-ghost btn-sm" onClick={() => setShowAll(true)}>Show all {d.themes.length} themes</button>}
    </div>
  )
}
