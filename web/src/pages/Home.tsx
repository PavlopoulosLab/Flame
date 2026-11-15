import { Link, useSearchParams } from 'react-router-dom'
import { useConfig } from '../api/hooks'
import { Logo } from '../components/Shell'
import { ErrorNote } from '../components/ui'
import { PUBLICATIONS } from './publications'

const EXAMPLES = [
  {
    kind: 'demo',
    title: 'Full worked example',
    text: 'A 200-gene human list run through g:Profiler and STRING, ready to explore and compare.',
    tag: 'Recommended',
  },
  { kind: 'upload', title: 'Gene list', text: 'Paste or upload gene/protein IDs in any of 19 formats.' },
  { kind: 'volcano', title: 'Volcano plot', text: 'Select up- and down-regulated genes from a differential expression table.' },
  { kind: 'reduction', title: '2D reduction plot', text: 'Lasso clusters on a UMAP, t-SNE or PCA scatter.' },
  { kind: 'snps', title: 'SNPs to genes', text: 'Map dbSNP rsIDs to the genes they affect.' },
  { kind: 'textmining', title: 'Text mining', text: 'Find gene and protein names in an abstract or any free text.' },
  { kind: 'upset', title: 'Set operations', text: 'Build intersections and unions of several lists with an UpSet plot.' },
]

const VIEWS = [
  ['Dot plot', 'Gene ratio, significance and term size at a glance'],
  ['Bar chart', 'Top terms by significance or coverage'],
  ['Scatter plot', 'Significance against term coverage'],
  ['Heatmaps', 'Terms × genes, terms × terms, genes × genes'],
  ['Networks', 'Term–gene, term–term and gene–gene networks, plus 3D in Arena3Dweb'],
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
          <h1>
            Flame 3.0: Functional Enrichment Integration made easy
          </h1>
          <p className="lead">
            Run functional and literature enrichment on one or many gene lists with six established tools, combine their
            results, and explore them with interactive plots and networks. Free and open to everyone — no login, no
            registration.
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
              <strong>6</strong> enrichment tools + your own GMT gene sets
            </li>
            <li>
              <strong>10</strong> interactive result views
            </li>
          </ul>
        </div>
        <div className="hero-art" aria-hidden="true">
          <Logo size={220} />
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
        <p className="muted">Each example opens a new workspace with sample data already loaded, so you can see exactly what Flame expects and produces.</p>
        <div className="example-grid">
          {EXAMPLES.map((e) => (
            <Link key={e.kind} to={`/start?new=1&example=${e.kind}`} className={`example-card${e.tag ? ' featured' : ''}`}>
              {e.tag && <span className="badge">{e.tag}</span>}
              <h3>{e.title}</h3>
              <p>{e.text}</p>
              <span className="example-cta">Open example →</span>
            </Link>
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
            <li>No login, registration or email needed.</li>
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
