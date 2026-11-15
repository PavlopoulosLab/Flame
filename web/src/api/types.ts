// Shapes returned by the Flame REST API (api/routes/*.R)

export interface ToolConfig {
  key: string
  id: string
  label: string
  name: string
  paradigms: string[]
  datasources: string[]
  metrics: { label: string; value: string }[]
  defaultMetricGenome: string
  defaultMetricBackground: string
  supportsBackground: boolean
  organismCount: number
  color: string
}

export interface OrganismOption {
  value: string
  label: string
  searchText?: string
  shortName?: string
}

export interface IdFormat {
  value: string
  label: string
  example?: string
  group?: string
  common?: boolean
}

export interface ResultTabConfig {
  tabId: string
  label: string
  icon: string
  defaultVisible: boolean
  hasArena3D: boolean
  tabType: string
}

export interface AppConfig {
  version: string
  tools: ToolConfig[]
  datasourceCategories: { category: string; sources: { label: string; id: string }[] }[]
  defaultDatasources: string[]
  datasourceColors: Record<string, string>
  linkProviders: Record<string, string>
  geneNodeColor: string
  idFormats: IdFormat[]
  commonOrganisms: OrganismOption[]
  analytes: { analyteType: string; key: string; label: string; uploadEnabled: boolean }[]
  resultTabs: ResultTabConfig[]
  networkLayouts: string[]
  customSourceTool: { key: string; label: string; metrics: { label: string; value: string }[] }
  limits: { maxLists: number; maxIdsPerList: number; maxTextBytes: number; retentionDays: number; stringMaxIds: number }
  emailNotifications: boolean
}

export interface AnalyteList {
  id: string
  name: string
  analyteType: string
  organismTaxId: number
  organismName: string
  idFormat: string
  ranked?: boolean
  size: number
  created: number
  meta: Record<string, unknown>
  ids?: string[]
}

export type RunStatus = 'queued' | 'running' | 'complete' | 'error'

export interface RunParams {
  geneListName: string
  datasources: string[] | string
  threshold: number
  namespace: string
  metric: string
  backgroundMode: 'genome' | 'custom'
  backgroundList?: string | null
}

export interface Run {
  id: string
  tool: string
  toolKey: string
  displayNumber: number
  paradigm: string
  title: string
  listName: string
  backgroundList: string | null
  organismTaxId: number
  organismName: string
  params: RunParams
  status: RunStatus
  error: string | null
  resultCount: number | null
  messages: string[]
  created: number
  finished: number | null
  /** Latest progress report of a queued or running job. */
  progress?: { fraction: number; message: string; elapsed: number } | null
}

export interface CustomSource {
  id: string
  name: string
  termCount?: number
  term_count?: number
  created: number
}

export interface WorkspaceSummary {
  id: string
  created: number
  expires: number
  hasEmail: boolean
  lists: AnalyteList[]
  runs: Run[]
  sources: CustomSource[]
}

export interface Term {
  id: string
  source: string
  name: string
  pvalue: number
  log10p: number
  termSize: number
  querySize: number
  intersection: number
  geneRatio: number
  coverage: number
  foldEnrichment: number | null
  genes: string
  link: string | null
  nes?: number | null
}

export interface NetworkNode {
  id: string
  label: string
  kind: 'term' | 'gene'
  group: string
  name: string
  x: number
  y: number
}

export interface NetworkEdge {
  id: number
  from: string
  to: string
  weight: number
  width: number
  common?: number
  total?: number
}

export interface ViewData {
  view: string
  totalCount: number
  filteredCount: number
  shownCount: number
  availableSources: string[]
  terms: Term[]
  cells?: Record<string, string | number>[]
  network?: { nodes: NetworkNode[]; edges: NetworkEdge[]; layout: string }
}

export interface ResultFilters {
  sources?: string[]
  search?: string
  pvalueMax?: number | null
  termSizeMin?: number | null
  termSizeMax?: number | null
  intersectionMin?: number | null
  intersectionMax?: number | null
  termCoverageMin?: number | null
  termCoverageMax?: number | null
}

export interface RunDetails extends Run {
  details: Record<string, unknown> | null
  backgroundSize?: number | null
  sourcesWithHits?: string[]
  inputSize?: number
  mappedCount?: number
  unmapped?: string[]
  noHitAnalytes?: string[]
  significantTerms?: number
}

export interface ComparisonTerm {
  id: string
  runIds: string[]
  source: string
  name: string
  tools: string
  rank: number
  combPvalue: number
  chisq: number
  termSize: number
  intersectionHits: string
  unionHits: string
  intersectionCount: number
  unionCount: number
  foldEnrichment?: number
  agreement?: number
  activeCombPvalue?: number
  negLog10p?: number
}

export interface ComparisonRunMeta {
  id: string
  tool: string
  displayNumber: number
  label: string
  paradigm: string
  organism: string
  listName: string
  datasources: string[]
  threshold: number
  metric: string
}

export interface OverviewChart {
  runs: {
    runId: string
    tool: string
    paradigm: string
    configGroup: number
    inputList: string
    runLabel: string
    opacity: number
    termsFound: number
    exclusiveTerms: number
    consensusBlocker: number
  }[]
  pins: { uniqueTerms: number; consensus: number; singleRun: number }
  metricLabels: Record<string, string>
  metricDescriptions: Record<string, string>
  pinLabels: Record<string, string>
}

export interface ComparisonData {
  view: 'overview' | 'bubble' | 'heatmap'
  totalCount: number
  filteredCount: number
  shownCount?: number
  availableSources: string[]
  runs: ComparisonRunMeta[]
  activeRunIds: string[]
  terms: ComparisonTerm[]
  chart?: OverviewChart
  cells?: { term: string; run: string; value: number | null }[]
}

export interface ComparisonFilters {
  runs?: string[]
  runsMode?: 'any' | 'all'
  consensusMin?: number | null
  consensusMax?: number | null
  sources?: string[]
  search?: string
  pvalueMax?: number | null
  termSizeMin?: number | null
  termSizeMax?: number | null
}
