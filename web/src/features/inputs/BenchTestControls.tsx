import { Field } from '../../components/ui'
import { POSTHOC, posthocsFor, resolvePosthoc, resolveTest, TESTS, type PosthocId, type StatOpts, type TestId } from './benchTests'

/**
 * The statistical test, its multiple-comparisons test, which comparisons, the correction across rows
 * and (one-sample tests) the hypothetical value, offered as in GraphPad Prism for the number of groups.
 */
export function BenchTestControls({ s, set, k, rows, canRatio, foldScale, autoHypo }: {
  s: StatOpts; set: (f: (s: StatOpts) => StatOpts) => void
  /** Number of groups being tested. */ k: number
  /** Number of rows tested (genes / readouts, without reference or normalising rows). */ rows: number
  /** Raw positive values (not a log-scale analysis): the ratio paired t test makes sense. */ canRatio: boolean
  /** Fold changes / log scale: the hypothetical value defaults to 1. */ foldScale: boolean
  /** The value used when none is typed (from the last analysis, e.g. the control's 100). */ autoHypo?: number | null
}) {
  const up = <K extends keyof StatOpts>(key: K, v: StatOpts[K]) => set((x) => ({ ...x, [key]: v }))
  const r = resolveTest(s.test, k)
  const offered = TESTS.filter((t) => (t.family === 'two' ? k === 2 : t.family === 'many' ? k >= 3 : t.family === 'rows' ? rows >= 2 : true) && (t.id !== 't_ratio' || canRatio))
  const fam = (f: string) => offered.filter((t) => t.family === f)
  const posts = posthocsFor(s.test, k)
  const post = resolvePosthoc(s, k)
  const info = TESTS.find((t) => t.id === r)
  const onlyCompare = post ? POSTHOC[post].only : undefined
  const isValue = r === 'one_sample_t' || r === 'one_sample_wilcoxon'
  const valid = s.test === 'auto' || offered.some((t) => t.id === s.test)
  return (
    <div className="stack-sm">
      <div className="row wrap">
        <Field label="Statistical test">
          <select className="input input-sm" aria-label="Statistical test" value={valid ? s.test : 'auto'} onChange={(e) => up('test', e.target.value as TestId)} style={{ maxWidth: 360 }}>
            <option value="auto">Auto: {k === 2 ? "unpaired t test with Welch's correction" : 'ordinary one-way ANOVA'}</option>
            {fam('two').length > 0 && <optgroup label="Two groups (t tests)">{fam('two').map((t) => <option key={t.id} value={t.id}>{t.label}</option>)}</optgroup>}
            {fam('many').length > 0 && <optgroup label="Three or more groups (one-way)">{fam('many').map((t) => <option key={t.id} value={t.id}>{t.label}</option>)}</optgroup>}
            {fam('rows').length > 0 && <optgroup label="Rows × groups">{fam('rows').map((t) => <option key={t.id} value={t.id}>{t.label}</option>)}</optgroup>}
            <optgroup label="Each group vs a value">{fam('value').map((t) => <option key={t.id} value={t.id}>{t.label}</option>)}</optgroup>
          </select>
        </Field>
        {posts.length > 0 && (
          <Field label="Multiple comparisons">
            <select className="input input-sm" aria-label="Multiple comparisons" value={post ?? ''} onChange={(e) => up('posthoc', e.target.value as PosthocId)} disabled={posts.length === 1}>
              {posts.map((p) => <option key={p} value={p}>{POSTHOC[p].label}</option>)}
            </select>
          </Field>
        )}
        {posts.length > 0 && (
          <Field label="Compare">
            <select className="input input-sm" aria-label="Which comparisons" value={s.posthoc === 'auto' ? s.compare : onlyCompare ?? s.compare} disabled={!!onlyCompare && s.posthoc !== 'auto'}
              onChange={(e) => { const c = e.target.value as StatOpts['compare']; set((x) => ({ ...x, compare: c, posthoc: onlyCompare && onlyCompare !== c ? 'auto' : x.posthoc })) }}>
              <option value="control">Each group vs the control</option>
              <option value="all">All pairs of groups</option>
            </select>
          </Field>
        )}
        {isValue && (
          <Field label="Hypothetical value">
            <input className="input input-sm input-narrow" type="number" step="any" value={s.hypo} placeholder={autoHypo != null ? `auto: ${autoHypo}` : foldScale ? '1' : 'auto'} style={{ width: 110 }}
              title="Empty: 1 for fold changes; otherwise the control's value when all its replicates have the same one (e.g. 100 for % of control), else 0" onChange={(e) => up('hypo', e.target.value)} />
          </Field>
        )}
        {rows > 1 && r !== 'twoway' && (
          <Field label="Correct across rows">
            <select className="input input-sm" aria-label="Correction across rows" value={s.across} onChange={(e) => up('across', e.target.value as StatOpts['across'])}>
              <option value="holm">Holm</option>
              <option value="bh">Benjamini–Hochberg (FDR)</option>
              <option value="none">None</option>
            </select>
          </Field>
        )}
      </div>
      <p className="muted small">
        {info?.note}{post ? ` Then ${POSTHOC[post].label}: ${POSTHOC[post].note}` : ''}
        {info?.paired ? ' Paired by replicate: the first sample of each group goes with the first of the others, and so on.' : ''}
        {isValue ? ` The control group is not tested; ${foldScale ? 'fold changes are compared on a log2 scale.' : 'values are compared as given.'}` : ''}
      </p>
    </div>
  )
}
