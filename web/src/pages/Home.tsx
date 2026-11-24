import type { CSSProperties } from 'react'
import { Link, useSearchParams } from 'react-router-dom'
import { API_BASE } from '../api/client'
import { useConfig } from '../api/hooks'
import { Logo } from '../components/Shell'
import { ConceptIcon } from '../components/Icons'
import { ToolLogo } from '../components/ToolLogo'
import { ErrorNote } from '../components/ui'

/** Each example's group on the Home page (examples are listed per group, not as one wall of cards). */
const EXAMPLE_GROUPS: { title: string; kinds: string[] }[] = [
  { title: 'Start from a gene list', kinds: ['demo', 'upload', 'gsea', 'upset', 'gmt', 'reactome'] },
  { title: 'Start from data', kinds: ['volcano', 'reduction', 'expression', 'activity', 'mirna', 'textmining'] },
  { title: 'Start from public resources', kinds: ['disease', 'pubmed', 'geo', 'clinvar', 'snps'] },
  { title: 'Compare', kinds: ['compare-tools', 'compare-strategies', 'compare-lists'] },
  { title: 'Regulators, drugs and networks', kinds: ['tf', 'kinase', 'drugs', 'celltypes', 'literature', 'ppi', 'robustness'] },
  { title: 'Variants and GWAS', kinds: ['variants', 'gwas'] },
]

const EXAMPLES = [
  {
    kind: 'demo',
    title: 'Full worked example',
    text: 'A 200-gene human list run through g:Profiler and STRING, ready to explore and compare.',
  },
  { kind: 'upload', title: 'Gene list', text: '200 genes, a new random draw each time, from the 500 most changed by dexamethasone in airway cells (Himes et al. 2014).' },
  { kind: 'volcano', title: 'Volcano plot', text: 'Dexamethasone vs untreated airway cells: pick up- and down-regulated genes (Himes et al. 2014).' },
  { kind: 'reduction', title: 'UMAP / 2D plot', text: 'A real UMAP of genes from 2,638 blood cells (10x PBMC 3k): lasso the B-cell or monocyte island.' },
  { kind: 'expression', title: 'Expression matrix', text: 'Airway RNA-seq counts (8 samples): co-expression clusters, a gene map, or dex vs untreated.' },
  { kind: 'variants', title: 'Variant analysis', text: 'Annotate SNPs and VCF variants: consequence, impact, gnomAD frequency, ClinVar and traits.' },
  { kind: 'gwas', title: 'GWAS results', text: 'Alzheimer’s disease loci (Bellenguez et al. 2022): Manhattan plot, genes, traits and GSEA.' },
  { kind: 'mirna', title: 'microRNAs', text: 'The solid-cancer microRNA signature of Volinia et al. 2006 → target genes → enrichment.' },
  { kind: 'disease', title: 'Genes of a disease', text: 'Asthma genes from Open Targets (scored), DisGeNET, OMIM, Orphanet, ClinVar and the GWAS Catalog.' },
  { kind: 'pubmed', title: 'Genes from PubMed', text: 'Genes linked to the articles of a PubMed search, ranked by how many articles mention them.' },
  { kind: 'geo', title: 'Genes from GEO', text: 'Up- or down-regulated genes of GEO series GSE52778 (Himes et al. 2014), via RummaGEO.' },
  { kind: 'clinvar', title: 'ClinVar variants', text: 'Pathogenic cystic fibrosis variants from ClinVar: classification, conditions and genes.' },
  { kind: 'snps', title: 'SNPs to genes', text: 'Map dbSNP rsIDs to the genes they affect.' },
  { kind: 'textmining', title: 'Text mining', text: 'Find gene and protein names in an abstract or any free text.' },
  { kind: 'upset', title: 'Set operations', text: 'Build intersections and unions of several lists with an UpSet plot.' },
  { kind: 'gsea', title: 'Ranked list (GSEA)', text: 'All genes of the airway dexamethasone experiment ranked by change, tested with GSEA.' },
  { kind: 'compare-tools', title: 'Compare tools', text: 'One list through six tools (including Reactome), with agreement statistics, consensus ranks and plots.' },
  { kind: 'reactome', title: 'Reactome pathways', text: 'Reactome\'s own Analysis Service, with each pathway diagram showing your genes; compared with g:Profiler.' },
  { kind: 'compare-strategies', title: 'Compare strategies', text: 'GSEA on all genes vs. over-representation on the significant ones.' },
  { kind: 'compare-lists', title: 'Up vs. down', text: 'Up- and down-regulated genes analysed and compared side by side.' },
  { kind: 'gmt', title: 'Custom gene sets', text: 'Upload gene sets as a GMT file (here: MSigDB Hallmark) and test them.' },
  { kind: 'ppi', title: 'Protein network and modules', text: 'STRING interactions among your proteins; find modules and what each does; save sub-networks.' },
  { kind: 'tf', title: 'Transcription factors', text: 'Which factors drive the dexamethasone response? ChEA3 on the airway up-regulated genes.' },
  { kind: 'kinase', title: 'Kinases', text: 'KEA3: kinases whose substrates are over-represented among the dexamethasone-induced genes.' },
  { kind: 'literature', title: 'Literature enrichment', text: 'Papers reporting gene sets like yours: PubMed (STRING), GeneSigDB and Rummagene.' },
  { kind: 'drugs', title: 'Drug signatures', text: 'LINCS L1000: glucocorticoids mimic the dexamethasone signature, mifepristone reverses it.' },
  { kind: 'celltypes', title: 'Cell types', text: 'Genes of the NK-cell island of the PBMC 3k UMAP against PanglaoDB, CellMarker and Azimuth.' },
  { kind: 'robustness', title: 'Robustness check', text: 'Bootstrap subsamples and random lists: how stable is the enrichment?' },
  { kind: 'activity', title: 'Pathway activity', text: 'Score Hallmark pathways or PROGENy signalling in every airway sample (ssGSEA).' },
]

/** The few examples shown up front; the rest are one click away. */
const FEATURED = ['demo', 'volcano', 'disease', 'compare-tools', 'gwas', 'drugs']

/** What Flame does, one card each (icons are the app's own concept icons). */
const FEATURES: { icon: string; tone: string; title: string; text: string }[] = [
  { icon: 'upload', tone: 'violet', title: 'Many ways in',
    text: 'Paste a gene list, pick genes from a volcano or UMAP plot, or fetch them from a disease, a PubMed search, a GEO series, SNPs or free text.' },
  { icon: 'run', tone: 'orange', title: 'Eight tools, one run',
    text: 'g:Profiler, STRING, Enrichr, Reactome, clusterProfiler and more, on the annotation sources you choose — or just keep the defaults.' },
  { icon: 'compare', tone: 'crimson', title: 'Compare everything',
    text: 'Tools, lists and strategies side by side, with agreement statistics, consensus ranks and UpSet plots.' },
  { icon: 'ppi', tone: 'teal', title: 'Views that talk to each other',
    text: 'Networks, heatmaps, Sankey and scatter plots: select terms in one view and they stay selected in every other.' },
  { icon: 'regulators', tone: 'indigo', title: 'Beyond enrichment',
    text: 'Transcription factors, kinases, drug signatures, cell types, literature, network modules, variants and GWAS.' },
  { icon: 'save', tone: 'amber', title: 'Share and reproduce',
    text: 'Private workspace links, figure export, a REST API and a Python client. No login, no tracking cookies.' },
]

/** One accent per featured example card. */
const TONES = ['violet', 'orange', 'crimson', 'teal', 'indigo', 'amber']

/** Rising sparks around the hero logo: [left %, size px, duration s, delay s, sideways drift px]. */
const EMBERS: [number, number, number, number, number][] = [
  [18, 7, 6.5, 0, 18], [30, 5, 5.2, 1.4, -14], [42, 8, 7.4, 2.6, 10], [55, 5, 5.8, 0.7, -20], [66, 7, 6.9, 3.3, 16],
  [78, 6, 5.5, 1.9, -10], [24, 4, 4.8, 3.9, 12], [48, 6, 6.2, 4.6, -16], [72, 4, 5.0, 2.9, 20], [60, 8, 7.8, 5.4, -8],
]

/** Decorative hero picture: the Flame logo burning — it wavers like fire, with flame tongues, a glow and rising sparks. */
function HeroArt() {
  return (
    <div className="hm-flame">
      <span className="hm-flame-glow" />
      <span className="hm-flame-ring" />
      <span className="hm-flame-ring hm-flame-ring-2" />
      {/* Fire motion: noise scrolls upward and bends the logo, the way heat bends a flame. */}
      <svg className="hm-fire-defs" width="0" height="0" aria-hidden="true" focusable="false">
        <defs>
          <filter id="hm-fire" x="-15%" y="-15%" width="130%" height="230%" colorInterpolationFilters="sRGB">
            <feTurbulence type="fractalNoise" baseFrequency="0.016 0.026" numOctaves="2" seed="4" stitchTiles="stitch" x="0" y="0" width="400" height="260" result="noise" />
            <feTile in="noise" result="tiled" />
            <feOffset in="tiled" dx="0" dy="0" result="rising">
              <animate attributeName="dy" from="0" to="-260" dur="4.2s" repeatCount="indefinite" />
            </feOffset>
            <feDisplacementMap in="SourceGraphic" in2="rising" scale="6" xChannelSelector="R" yChannelSelector="G" />
          </filter>
          <filter id="hm-fire-aura" x="-30%" y="-40%" width="160%" height="260%" colorInterpolationFilters="sRGB">
            <feTurbulence type="fractalNoise" baseFrequency="0.011 0.02" numOctaves="2" seed="11" stitchTiles="stitch" x="0" y="0" width="400" height="300" result="noise" />
            <feTile in="noise" result="tiled" />
            <feOffset in="tiled" dx="0" dy="0" result="rising">
              <animate attributeName="dy" from="0" to="-300" dur="3s" repeatCount="indefinite" />
            </feOffset>
            <feDisplacementMap in="SourceGraphic" in2="rising" scale="30" xChannelSelector="R" yChannelSelector="G" result="licks" />
            <feGaussianBlur in="licks" stdDeviation="6" />
          </filter>
        </defs>
      </svg>
      <span className="hm-flame-logo">
        <span className="hm-fire-aura"><Logo size={400} /></span>
        <span className="hm-fire-body"><Logo size={400} /></span>
      </span>
      {EMBERS.map(([left, size, dur, delay, drift], i) => (
        <span key={i} className="hm-ember" style={{ left: `${left}%`, width: size, height: size, animationDuration: `${dur}s`, animationDelay: `${delay}s`, '--drift': `${drift}px` } as CSSProperties} />
      ))}
    </div>
  )
}

export default function HomePage() {
  const { data: config } = useConfig()
  const [params] = useSearchParams()
  const importError = params.get('import_error')
  let lastWs: string | null = null
  try {
    lastWs = sessionStorage.getItem('flame-last-workspace')
  } catch {
    /* storage unavailable */
  }
  const byKind = (k: string) => EXAMPLES.find((e) => e.kind === k)
  const nTools = config?.tools.length ?? 8
  const stats: [string, string][] = [
    [String(nTools), 'enrichment tools in one run'],
    [String(config?.idFormats.length ?? 19), 'gene and protein ID formats'],
    [String(EXAMPLES.length), 'ready-made examples'],
    ['12,000+', 'organisms'],
    ['0', 'logins or tracking cookies'],
  ]

  return (
    <div className="home home-modern">
      {importError && <ErrorNote error={`The gene lists in the link could not be imported: ${importError}`} />}

      <section className="hm-hero">
        <div className="hm-hero-text">
          <div className="hm-lockup"><Logo size={44} /><span>Flame <span className="hm-version">3.0</span></span></div>
          <h1 className="hm-title" aria-label="Flame 3.0: The bioscientist’s everyday toolkit">
            The bioscientist’s
            <span className="hm-title-grad">everyday toolkit.</span>
          </h1>
          <p className="hm-lead">
            Bring a gene list, a volcano plot, a PDF, GWAS hits or a TCGA cohort. Flame runs {nTools} enrichment tools
            side by side, uncovers the pathways, regulators and drugs behind your genes, and turns it all into
            interactive figures ready to explore, compare and publish. No coding needed.
          </p>
          <div className="hm-actions">
            <Link className="hm-btn hm-btn-primary" to="/start?new=1&example=demo">
              Explore an example
            </Link>
            <Link className="hm-btn hm-btn-ghost" to="/start?new=1">
              Start an analysis
            </Link>
            {lastWs && (
              <Link className="hm-link" to={`/w/${lastWs}`}>
                Back to my workspace →
              </Link>
            )}
          </div>
          <div className="hm-tools" aria-label="Enrichment tools">
            <span>Powered by</span>
            {(config?.tools ?? []).map((t) => (
              <span key={t.key} className="hm-tool" title={t.name}><ToolLogo tool={t.key} size={20} /></span>
            ))}
          </div>
        </div>
        <div className="hm-hero-art" aria-hidden="true">
          <HeroArt />
        </div>
      </section>

      <dl className="hm-stats">
        {stats.map(([n, label]) => (
          <div key={label}><dt>{n}</dt><dd>{label}</dd></div>
        ))}
      </dl>

      <section className="hm-section">
        <h2>Everything an enrichment analysis needs</h2>
        <p className="hm-sub">From the first gene list to the published figure, in one place.</p>
        <div className="hm-features">
          {FEATURES.map((f) => (
            <article key={f.title} className={`hm-card hm-tone-${f.tone}`}>
              <span className="hm-icon"><ConceptIcon name={f.icon} size={22} /></span>
              <h3>{f.title}</h3>
              <p>{f.text}</p>
            </article>
          ))}
        </div>
      </section>

      <section className="hm-section">
        <h2>From genes to insight in three steps</h2>
        <ol className="hm-steps">
          <li>
            <span className="hm-step-num">1</span>
            <div>
              <h3>Add your lists</h3>
              <p>Paste genes, pick them from a plot, or fetch them from a disease, PubMed or GEO.</p>
            </div>
          </li>
          <li>
            <span className="hm-step-num">2</span>
            <div>
              <h3>Run enrichment</h3>
              <p>Choose tools and sources, or keep the defaults.</p>
            </div>
          </li>
          <li>
            <span className="hm-step-num">3</span>
            <div>
              <h3>Explore and compare</h3>
              <p>Select terms across views, compare runs, export figures.</p>
            </div>
          </li>
        </ol>
      </section>

      <section className="hm-section">
        <h2>Start from an example</h2>
        <p className="hm-sub">One click opens a ready-made analysis on published data.</p>
        <div className="hm-examples">
          {FEATURED.map(byKind).filter((e): e is (typeof EXAMPLES)[number] => !!e).map((e, i) => (
            <Link key={e.kind} to={`/start?new=1&example=${e.kind}`} className={`hm-example hm-tone-${TONES[i % TONES.length]}`}>
              <span className="hm-icon"><ConceptIcon name={e.kind} size={22} /></span>
              <strong>{e.title}</strong>
              <span className="hm-example-text">{e.text}</span>
              <span className="hm-open">Open →</span>
            </Link>
          ))}
        </div>
        <details className="all-examples">
          <summary>All {EXAMPLES.length} examples</summary>
          <div className="example-groups">
            {EXAMPLE_GROUPS.map((g) => (
              <section key={g.title} className="example-group">
                <h3>{g.title}</h3>
                <ul>
                  {g.kinds.map(byKind).filter((e): e is (typeof EXAMPLES)[number] => !!e).map((e) => (
                    <li key={e.kind}>
                      <Link to={`/start?new=1&example=${e.kind}`} className="example-row">
                        <ConceptIcon name={e.kind} size={20} />
                        <span><strong>{e.title}</strong><span className="muted small">{e.text}</span></span>
                      </Link>
                    </li>
                  ))}
                </ul>
              </section>
            ))}
          </div>
          <p className="muted small">
            Sample input files:{' '}
            <a href={`${API_BASE}/inputs/examples/upload/file`} download>gene list</a> ·{' '}
            <a href={`${API_BASE}/inputs/examples/volcano/file`} download>volcano table</a> ·{' '}
            <a href={`${API_BASE}/inputs/examples/reduction/file`} download>2D coordinates</a> ·{' '}
            <a href={`${API_BASE}/inputs/examples/snps/file`} download>SNP list</a> ·{' '}
            <a href={`${API_BASE}/inputs/examples/textmining/file`} download>text</a>
          </p>
        </details>
      </section>

      <footer className="hm-footer">
        <p>
          Private workspace links, deleted after {config?.limits.retentionDays ?? 30} days of inactivity · no tracking cookies
        </p>
        <nav aria-label="More">
          <Link to="/help">Help and tutorial →</Link>
          <Link to="/api-docs">REST API and Python client →</Link>
          <Link to="/cite">How to cite →</Link>
        </nav>
      </footer>
    </div>
  )
}
