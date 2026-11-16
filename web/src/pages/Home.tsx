import { Link, useSearchParams } from 'react-router-dom'
import { useConfig } from '../api/hooks'
import { BrandLockup, Wordmark } from '../components/Shell'
import { ConceptIcon } from '../components/Icons'
import { ToolLogo } from '../components/ToolLogo'
import { ErrorNote } from '../components/ui'

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

/** The few examples shown up front; the rest are one click away. */
const FEATURED = ['demo', 'volcano', 'disease', 'compare-tools', 'gwas', 'drugs']

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

  return (
    <div className="home home-light">
      {importError && <ErrorNote error={`The gene lists in the link could not be imported: ${importError}`} />}

      <section className="hero">
        <div className="hero-text">
          <h1 className="hero-title" aria-label="Flame 3.0: Functional Analysis and Integration made easy">
            <Wordmark className="hero-brand" />
            <span className="hero-tagline">Functional Analysis and Integration <em>made easy</em></span>
          </h1>
          <p className="lead">
            Enrichment analysis of one or many gene lists with eight established tools, combined and explored in interactive views.
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
          <div className="tool-strip" aria-label="Enrichment tools">
            <span className="muted small">{config?.tools.length ?? 8} tools</span>
            {(config?.tools ?? []).map((t) => (
              <span key={t.key} title={t.name}><ToolLogo tool={t.key} size={22} /></span>
            ))}
          </div>
        </div>
        <div className="hero-art" aria-hidden="true">
          <BrandLockup size={200} stacked />
        </div>
      </section>

      <section className="home-section">
        <ol className="steps steps-compact">
          <li>
            <span className="step-num">1</span>
            <h3>Add your lists</h3>
            <p>Paste genes, pick them from a plot, or fetch them from a disease, PubMed or GEO.</p>
          </li>
          <li>
            <span className="step-num">2</span>
            <h3>Run enrichment</h3>
            <p>Choose tools and sources, or keep the defaults.</p>
          </li>
          <li>
            <span className="step-num">3</span>
            <h3>Explore and compare</h3>
            <p>Select terms across views, compare runs, export figures.</p>
          </li>
        </ol>
      </section>

      <section className="home-section">
        <h2>Examples</h2>
        <div className="featured-examples">
          {FEATURED.map(byKind).filter((e): e is (typeof EXAMPLES)[number] => !!e).map((e) => (
            <Link key={e.kind} to={`/start?new=1&example=${e.kind}`} className="featured-example">
              <ConceptIcon name={e.kind} size={24} />
              <strong>{e.title}</strong>
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
            <a href="/api/inputs/examples/upload/file" download>gene list</a> ·{' '}
            <a href="/api/inputs/examples/volcano/file" download>volcano table</a> ·{' '}
            <a href="/api/inputs/examples/reduction/file" download>2D coordinates</a> ·{' '}
            <a href="/api/inputs/examples/snps/file" download>SNP list</a> ·{' '}
            <a href="/api/inputs/examples/textmining/file" download>text</a>
          </p>
        </details>
      </section>

      <p className="home-footer muted small">
        Private workspace links, deleted after {config?.limits.retentionDays ?? 30} days of inactivity · no tracking cookies ·{' '}
        <Link to="/help">Help and tutorial</Link> · <Link to="/api-docs">REST API and Python client</Link>
      </p>
    </div>
  )
}
