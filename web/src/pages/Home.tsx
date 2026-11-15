import { Link, useSearchParams } from 'react-router-dom'
import { useConfig } from '../api/hooks'
import { BrandLockup, Wordmark } from '../components/Shell'
import { ConceptIcon } from '../components/Icons'
import { ToolLogo } from '../components/ToolLogo'
import { ErrorNote } from '../components/ui'
import { PUBLICATIONS } from './publications'

/** Each example's group on the Home page (examples are listed per group, not as one wall of cards). */
const EXAMPLE_GROUPS: { title: string; kinds: string[] }[] = [
  { title: 'Start from a gene list', kinds: ['demo', 'upload', 'gsea', 'upset', 'gmt', 'reactome'] },
  { title: 'Start from data', kinds: ['volcano', 'reduction', 'expression', 'activity', 'mirna', 'textmining'] },
  { title: 'Start from public resources', kinds: ['disease', 'pubmed', 'geo', 'dbsnp', 'snps'] },
  { title: 'Compare', kinds: ['compare-tools', 'compare-strategies', 'compare-lists'] },
  { title: 'Regulators, drugs and networks', kinds: ['tf', 'kinase', 'drugs', 'celltypes', 'literature', 'metabolites', 'modules', 'robustness', 'ppi'] },
  { title: 'Variants and GWAS', kinds: ['variants', 'gwas'] },
]

const EXAMPLES = [
  {
    kind: 'demo',
    title: 'Full worked example',
    text: 'A 200-gene human list run through g:Profiler and STRING, ready to explore and compare.',
  },
  { kind: 'upload', title: 'Gene list', text: '200 genes changed by dexamethasone in airway cells (Himes et al. 2014).' },
  { kind: 'volcano', title: 'Volcano plot', text: 'Dexamethasone vs untreated airway cells: pick up- and down-regulated genes (Himes et al. 2014).' },
  { kind: 'reduction', title: 'UMAP / 2D plot', text: 'A real UMAP of genes from 2,638 blood cells (10x PBMC 3k): lasso the B-cell or monocyte island.' },
  { kind: 'expression', title: 'Expression matrix', text: 'Airway RNA-seq counts (8 samples): co-expression clusters, a gene map, or dex vs untreated.' },
  { kind: 'variants', title: 'Variant analysis', text: 'Annotate SNPs and VCF variants: consequence, impact, gnomAD frequency, ClinVar and traits.' },
  { kind: 'gwas', title: 'GWAS results', text: 'Alzheimer’s disease loci (Bellenguez et al. 2022): Manhattan plot, genes, traits and GSEA.' },
  { kind: 'mirna', title: 'microRNAs', text: 'The solid-cancer microRNA signature of Volinia et al. 2006 → target genes → enrichment.' },
  { kind: 'disease', title: 'Genes of a disease', text: 'Asthma genes from Open Targets (scored), DisGeNET, OMIM, Orphanet, ClinVar and the GWAS Catalog.' },
  { kind: 'pubmed', title: 'Genes from PubMed', text: 'Genes linked to the articles of a PubMed search, ranked by how many articles mention them.' },
  { kind: 'geo', title: 'Genes from GEO', text: 'Up- or down-regulated genes of GEO series GSE52778 (Himes et al. 2014), via RummaGEO.' },
  { kind: 'dbsnp', title: 'dbSNP variants', text: 'Pathogenic APOE variants from dbSNP: locations, ClinVar, frequencies and genes.' },
  { kind: 'snps', title: 'SNPs to genes', text: 'Map dbSNP rsIDs to the genes they affect.' },
  { kind: 'textmining', title: 'Text mining', text: 'Find gene and protein names in an abstract or any free text.' },
  { kind: 'upset', title: 'Set operations', text: 'Build intersections and unions of several lists with an UpSet plot.' },
  { kind: 'gsea', title: 'Ranked list (GSEA)', text: 'All genes of the airway dexamethasone experiment ranked by change, tested with GSEA.' },
  { kind: 'compare-tools', title: 'Compare tools', text: 'One list through six tools (including Reactome), with agreement statistics, consensus ranks and plots.' },
  { kind: 'reactome', title: 'Reactome pathways', text: 'Reactome\'s own Analysis Service, with each pathway diagram showing your genes; compared with g:Profiler.' },
  { kind: 'compare-strategies', title: 'Compare strategies', text: 'GSEA on all genes vs. over-representation on the significant ones.' },
  { kind: 'compare-lists', title: 'Up vs. down', text: 'Up- and down-regulated genes analysed and compared side by side.' },
  { kind: 'gmt', title: 'Custom gene sets', text: 'Upload gene sets as a GMT file (here: MSigDB Hallmark) and test them.' },
  { kind: 'ppi', title: 'Protein network', text: 'STRING interactions among your proteins; select and save sub-networks.' },
  { kind: 'tf', title: 'Transcription factors', text: 'Which factors drive the dexamethasone response? ChEA3 on the airway up-regulated genes.' },
  { kind: 'kinase', title: 'Kinases', text: 'KEA3: kinases whose substrates are over-represented among the dexamethasone-induced genes.' },
  { kind: 'literature', title: 'Literature enrichment', text: 'Papers reporting gene sets like yours: PubMed (STRING), GeneSigDB and Rummagene.' },
  { kind: 'drugs', title: 'Drug signatures', text: 'LINCS L1000: glucocorticoids mimic the dexamethasone signature, mifepristone reverses it.' },
  { kind: 'celltypes', title: 'Cell types', text: 'Genes of the NK-cell island of the PBMC 3k UMAP against PanglaoDB, CellMarker and Azimuth.' },
  { kind: 'metabolites', title: 'Metabolite pathways', text: 'Metabolites of metastatic prostate cancer (Sreekumar et al. 2009) mapped to KEGG pathways.' },
  { kind: 'modules', title: 'Network modules', text: 'Split the STRING network of a list into modules and enrich each one.' },
  { kind: 'robustness', title: 'Robustness check', text: 'Bootstrap subsamples and random lists: how stable is the enrichment?' },
  { kind: 'activity', title: 'Pathway activity', text: 'Score Hallmark pathways or PROGENy signalling in every airway sample (ssGSEA).' },
]

const VIEWS = [
  ['Dot plot', 'Gene ratio (or NES for GSEA), significance and term size at a glance'],
  ['Bar chart', 'Top terms by significance or coverage'],
  ['Scatter plot', 'Significance against term coverage'],
  ['Heatmaps', 'Terms × genes, terms × terms, genes × genes'],
  ['Networks', 'Term–gene, term–term and gene–gene networks'],
  ['Sankey, chord and hive', 'Gene → term → source flows, term overlap and multi-axis views'],
  ['Comparison', 'Overview, UpSet, chord, bubble and heatmap across runs, with combined p-values'],
  ['Variant reports', 'Manhattan plot, impact and ClinVar summaries, variants → genes → traits'],
  ['3D and external viewers', 'Open any network in Arena3D (3D layers) or NORMA'],
  ['Table', 'Searchable, filterable, exportable results'],
]

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

  return (
    <div className="home">
      {importError && <ErrorNote error={`The gene lists in the link could not be imported: ${importError}`} />}

      <section className="hero">
        <div className="hero-text">
          <h1 className="hero-title" aria-label="Flame 3.0: Functional Analysis and Integration made easy">
            <Wordmark className="hero-brand" />
            <span className="hero-tagline">Functional Analysis and Integration <em>made easy</em></span>
          </h1>
          <p className="lead">
            Run functional and literature enrichment on one or many gene lists with eight established tools, combine their
            results, and explore them with interactive plots and networks. Free and open to everyone.
          </p>
          <div className="hero-actions">
            <Link className="btn btn-primary btn-lg" to="/start?new=1">
              Start an analysis
            </Link>
            <Link className="btn btn-secondary btn-lg" to="/start?new=1&example=demo">
              Try a worked example
            </Link>
            {lastWs && (
              <Link className="btn btn-ghost btn-lg" to={`/w/${lastWs}`}>
                Back to my workspace
              </Link>
            )}
          </div>
          <ul className="hero-facts">
            <li>
              <strong>14,000+</strong> organisms
            </li>
            <li>
              <strong>8</strong> enrichment tools, ORA and GSEA, plus your own gene sets
            </li>
            <li>
              <strong>13</strong> interactive result views
            </li>
          </ul>
        </div>
        <div className="hero-art" aria-hidden="true">
          <BrandLockup size={230} stacked />
        </div>
      </section>

      <section className="home-section">
        <h2>How it works</h2>
        <ol className="steps">
          <li>
            <span className="step-num">1</span>
            <h3>Add your lists</h3>
            <p>Paste genes, upload a file, pick genes from a volcano or UMAP plot, map SNPs, mine text, or combine lists.</p>
          </li>
          <li>
            <span className="step-num">2</span>
            <h3>Run enrichment</h3>
            <p>Choose tools and annotation sources (GO, KEGG, Reactome, diseases, drugs, literature…). Defaults work out of the box.</p>
          </li>
          <li>
            <span className="step-num">3</span>
            <h3>Explore and compare</h3>
            <p>Filter, search and select terms across plots, compare tools side by side, and export figures and tables.</p>
          </li>
        </ol>
      </section>

      <section className="home-section">
        <h2>Examples</h2>
        <p className="muted">Each example opens a new workspace with published data already loaded.</p>
        <div className="example-groups">
          {EXAMPLE_GROUPS.map((g) => (
            <section key={g.title} className="example-group">
              <h3>{g.title}</h3>
              <ul>
                {g.kinds.map((k) => EXAMPLES.find((e) => e.kind === k)).filter((e): e is (typeof EXAMPLES)[number] => !!e).map((e) => (
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
          Download sample input files to see the expected formats:{' '}
          <a href="/api/inputs/examples/upload/file" download>gene list</a> ·{' '}
          <a href="/api/inputs/examples/volcano/file" download>volcano table</a> ·{' '}
          <a href="/api/inputs/examples/reduction/file" download>2D coordinates</a> ·{' '}
          <a href="/api/inputs/examples/snps/file" download>SNP list</a> ·{' '}
          <a href="/api/inputs/examples/textmining/file" download>text</a>
        </p>
      </section>

      <section className="home-section">
        <h2>Enrichment tools</h2>
        <div className="tool-grid">
          {(config?.tools ?? []).map((t) => (
            <div key={t.key} className="tool-card" style={{ borderTopColor: t.color }}>
              <div className="tool-card-logo"><ToolLogo tool={t.key} size={34} variant="full" /></div>
              <h3>{t.name}</h3>
              <p className="muted">{t.organismCount.toLocaleString()} organisms</p>
              <p className="small">{t.datasources.slice(0, 6).join(', ')}{t.datasources.length > 6 ? '…' : ''}</p>
            </div>
          ))}
          <div className="tool-card" style={{ borderTopColor: 'var(--accent-color)' }}>
            <h3>Your gene sets</h3>
            <p className="muted">Any organism</p>
            <p className="small">Upload a GMT file and test it like any other source.</p>
          </div>
        </div>
      </section>

      <section className="home-section">
        <h2>Visualizations</h2>
        <div className="view-grid">
          {VIEWS.map(([name, text]) => (
            <div key={name} className="view-card">
              <h3>{name}</h3>
              <p className="muted">{text}</p>
            </div>
          ))}
        </div>
      </section>

      <section className="home-section two-col">
        <div>
          <h2>Open by design</h2>
          <ul className="checklist">
            <li>Your data stays private: only people with your workspace link can see it.</li>
            <li>Workspaces are deleted after {config?.limits.retentionDays ?? 30} days of inactivity, or whenever you choose.</li>
            <li>No tracking or advertising cookies.</li>
            <li>
              A REST <Link to="/api-docs">API</Link> with a ready-to-use Python script.
            </li>
          </ul>
          <p>
            <Link to="/privacy">Privacy details</Link> · <Link to="/help">Help and tutorial</Link>
          </p>
        </div>
        <div>
          <h2>How to cite</h2>
          <p className="muted">If Flame helps your work, please cite:</p>
          <ul className="pubs">
            {PUBLICATIONS.map((p) => (
              <li key={p.doi}>
                {p.authors} ({p.year}) <strong>{p.title}</strong>. <em>{p.journal}</em>.{' '}
                <a href={`https://doi.org/${p.doi}`} target="_blank" rel="noopener noreferrer">
                  doi:{p.doi}
                </a>
              </li>
            ))}
          </ul>
          <p>
            <Link to="/cite">All citation formats</Link>
          </p>
        </div>
      </section>
    </div>
  )
}
