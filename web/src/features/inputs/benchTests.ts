// The statistical tests of Bench Data (GraphPad Prism's set); the R side is core-bench-stats.R.

export type TestId = 'auto' | 't_unpaired' | 't_welch' | 't_paired' | 't_ratio' | 'mann_whitney' | 'ks' | 'wilcoxon'
  | 'anova' | 'welch_anova' | 'bf_anova' | 'rm_anova' | 'kruskal' | 'friedman' | 'twoway' | 'one_sample_t' | 'one_sample_wilcoxon'
export type PosthocId = 'auto' | 'dunnett' | 'tukey' | 'sidak' | 'bonferroni' | 'holm_sidak' | 'fisher' | 'games_howell' | 'dunnett_t3' | 'dunn'
export interface StatOpts {
  test: TestId
  posthoc: PosthocId
  compare: 'control' | 'all'
  /** Several rows (genes / readouts): correct their main p-values for multiple testing. */
  across: 'holm' | 'bh' | 'none'
  /** One-sample tests: the hypothetical value ('' = 1 for fold changes, 0 otherwise). */
  hypo: string
}
export const DEFAULT_STAT: StatOpts = { test: 'auto', posthoc: 'auto', compare: 'control', across: 'holm', hypo: '' }

export interface TestInfo { id: TestId; label: string; family: 'two' | 'many' | 'rows' | 'value'; paired?: boolean; note: string }
export const TESTS: TestInfo[] = [
  { id: 't_unpaired', family: 'two', label: 'Unpaired t test', note: 'Two independent groups; assumes Gaussian data with equal SDs.' },
  { id: 't_welch', family: 'two', label: "Unpaired t test with Welch's correction", note: 'Two independent groups; Gaussian data, does not assume equal SDs.' },
  { id: 't_paired', family: 'two', label: 'Paired t test', paired: true, note: 'The same experiment, donor or blot in both groups; tests the mean of the differences.' },
  { id: 't_ratio', family: 'two', label: 'Ratio paired t test', paired: true, note: 'Paired, when the treatment changes values by a ratio: a paired t test on log values. Needs positive values.' },
  { id: 'mann_whitney', family: 'two', label: 'Mann–Whitney test', note: 'Two independent groups, non-parametric (ranks).' },
  { id: 'ks', family: 'two', label: 'Kolmogorov–Smirnov test', note: 'Two independent groups, non-parametric: compares the whole distributions.' },
  { id: 'wilcoxon', family: 'two', label: 'Wilcoxon matched-pairs signed-rank test', paired: true, note: 'Paired, non-parametric.' },
  { id: 'anova', family: 'many', label: 'Ordinary one-way ANOVA', note: 'Three or more independent groups; assumes Gaussian data with equal SDs.' },
  { id: 'welch_anova', family: 'many', label: 'Welch ANOVA', note: 'Three or more groups; Gaussian data, does not assume equal SDs.' },
  { id: 'bf_anova', family: 'many', label: 'Brown–Forsythe ANOVA', note: 'Three or more groups; Gaussian data, does not assume equal SDs.' },
  { id: 'rm_anova', family: 'many', label: 'Repeated-measures one-way ANOVA', paired: true, note: 'The same experiment or donor in every group; Geisser–Greenhouse correction (no sphericity assumed).' },
  { id: 'kruskal', family: 'many', label: 'Kruskal–Wallis test', note: 'Three or more independent groups, non-parametric.' },
  { id: 'friedman', family: 'many', label: 'Friedman test', paired: true, note: 'Matched (repeated) groups, non-parametric.' },
  { id: 'twoway', family: 'rows', label: 'Two-way ANOVA (rows × groups)', note: 'Every gene or readout together: tests the treatment, the row and their interaction; comparisons within each row.' },
  { id: 'one_sample_t', family: 'value', label: 'One-sample t test (each group vs a value)', note: 'Each group against a hypothetical value, e.g. fold change vs 1 when the controls were set to exactly 1.' },
  { id: 'one_sample_wilcoxon', family: 'value', label: 'Wilcoxon signed-rank test (each group vs a value)', note: 'Non-parametric one-sample test against a hypothetical value.' },
]
export const POSTHOC: Record<Exclude<PosthocId, 'auto'>, { label: string; only?: 'control' | 'all'; note: string }> = {
  dunnett: { label: "Dunnett's test", only: 'control', note: 'Each group vs the control (recommended for that question).' },
  tukey: { label: "Tukey's test", only: 'all', note: 'Every pair of groups (recommended for that question).' },
  sidak: { label: 'Šídák', note: 'A chosen set of comparisons; a little more power than Bonferroni.' },
  bonferroni: { label: 'Bonferroni', note: 'A chosen set of comparisons; conservative.' },
  holm_sidak: { label: 'Holm–Šídák', note: 'Step-down: more power than Šídák or Bonferroni; no confidence intervals.' },
  fisher: { label: "Fisher's LSD (no correction)", note: 'Each comparison on its own, without correcting for multiple comparisons.' },
  games_howell: { label: 'Games–Howell', only: 'all', note: 'Every pair, without assuming equal SDs (after Welch / Brown–Forsythe ANOVA).' },
  dunnett_t3: { label: "Dunnett's T3", note: 'Without assuming equal SDs (after Welch / Brown–Forsythe ANOVA); best for small samples.' },
  dunn: { label: "Dunn's test", note: 'After Kruskal–Wallis or Friedman.' },
}

/** The test "Auto" stands for: Welch's t test for two groups, ordinary one-way ANOVA for more. */
export const resolveTest = (t: TestId, k: number): TestId => (t !== 'auto' ? t : k === 2 ? 't_welch' : 'anova')
/** Post hoc tests offered after a test (none for two groups or one-sample tests). */
export function posthocsFor(t: TestId, k: number): Exclude<PosthocId, 'auto'>[] {
  const r = resolveTest(t, k)
  if (r === 'anova' || r === 'rm_anova' || r === 'twoway') return k > 2 || r === 'twoway' ? ['dunnett', 'tukey', 'sidak', 'bonferroni', 'holm_sidak', 'fisher'] : []
  if (r === 'welch_anova' || r === 'bf_anova') return ['dunnett_t3', 'games_howell']
  if (r === 'kruskal' || r === 'friedman') return ['dunn']
  return []
}
/** The post hoc test used: the chosen one if allowed, else the usual one for the question. */
export function resolvePosthoc(s: StatOpts, k: number): Exclude<PosthocId, 'auto'> | null {
  const ok = posthocsFor(s.test, k)
  if (!ok.length) return null
  if (s.posthoc !== 'auto' && ok.includes(s.posthoc)) return s.posthoc
  if (ok.includes('dunnett')) return s.compare === 'all' ? 'tukey' : 'dunnett'
  if (ok.includes('dunnett_t3')) return s.compare === 'all' ? 'games_howell' : 'dunnett_t3'
  return ok[0]
}
