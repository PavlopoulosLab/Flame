import { Link } from 'react-router-dom'
import { useConfig } from '../api/hooks'
import { CITATION_GROUPS } from '../data/citations'
import { CopyButton } from '../components/ui'
import { PUBLICATIONS } from './publications'

const bibtex = (p: (typeof PUBLICATIONS)[number]) => `@article{${p.bibtexKey},
  author  = {${p.authors.replace(/\.,/g, '. and').replace(/ ([A-Z])\./g, ', $1.')}},
  title   = {${p.title}},
  journal = {${p.journal}},
  year    = {${p.year}},
  volume  = {${p.details.split('(')[0]}},
  doi     = {${p.doi}},
  pmid    = {${p.pmid}}
}`

function Cite() {
  return (
    <article className="doc">
      <h1>How to cite</h1>
      <p>If you use Flame in your work, please cite the publications below.</p>
      <h2>Flame 3.0</h2>
      <p className="muted">The Flame 3.0 article is in preparation; this page will list it once published.</p>
      <h2>Previous publications</h2>
      {PUBLICATIONS.map((p) => (
        <section key={p.doi} className="cite-card">
          <p>
            {p.authors} ({p.year}) <strong>{p.title}</strong>. <em>{p.journal}</em> {p.details}.
          </p>
          <p className="small">
            DOI: <a href={`https://doi.org/${p.doi}`} target="_blank" rel="noopener noreferrer">{p.doi}</a> · PubMed:{' '}
            <a href={`https://pubmed.ncbi.nlm.nih.gov/${p.pmid}/`} target="_blank" rel="noopener noreferrer">{p.pmid}</a> · Full text:{' '}
            <a href={`https://www.ncbi.nlm.nih.gov/pmc/articles/${p.pmcid}/`} target="_blank" rel="noopener noreferrer">{p.pmcid}</a>
          </p>
          <details>
            <summary className="small">BibTeX</summary>
            <pre className="code">{bibtex(p)}</pre>
            <CopyButton text={bibtex(p)} label="Copy BibTeX" />
          </details>
        </section>
      ))}
      <h2>Tools and resources used</h2>
      <p className="small">Please also cite the tools and databases whose results you report. The latest publication of each is listed below; most results also name their source.</p>
      <p className="row wrap"><CopyButton label="Copy all references" text={CITATION_GROUPS.flatMap((g) => g.items.map((c) => `${c.ref}. doi:${c.doi}`)).join('\n')} />
        <span className="muted small">Paste into a reference manager (Zotero, EndNote…) via “Add item by identifier” using the DOIs.</span></p>
      {CITATION_GROUPS.map((g) => (
        <section key={g.title} className="cite-group">
          <h3>{g.title}</h3>
          <ul className="cite-list">
            {g.items.map((c) => (
              <li key={c.key}><strong>{c.resource}</strong> — {c.ref}. <a href={`https://doi.org/${c.doi}`} target="_blank" rel="noopener noreferrer">doi:{c.doi}</a></li>
            ))}
          </ul>
        </section>
      ))}
    </article>
  )
}

interface Related { name: string; url: string; text: string; logo?: string }
const R = (logo: string) => `/logos/related/${logo}.png`
const RELATED: { title: string; items: Related[] }[] = [
  { title: 'Enrichment tools in Flame', items: [
    { name: 'g:Profiler', url: 'https://biit.cs.ut.ee/gprofiler/', logo: '/logos/gprofiler-icon.png', text: 'Functional profiling of gene lists; g:Convert, g:Orth and g:SNPense.' },
    { name: 'STRING', url: 'https://string-db.org/', logo: '/logos/string-icon.png', text: 'Protein networks and enrichment, including PubMed.' },
    { name: 'Enrichr', url: 'https://maayanlab.cloud/Enrichr/', logo: '/logos/enrichr.png', text: 'Hundreds of gene-set libraries.' },
    { name: 'WebGestalt', url: 'https://www.webgestalt.org/', logo: '/logos/webgestalt.svg', text: 'Disease and drug databases.' },
    { name: 'PANTHER', url: 'https://pantherdb.org/', text: 'Pathways and protein classes.' },
    { name: 'GeneCodis', url: 'https://genecodis.genyo.es/', logo: '/logos/genecodis-icon.png', text: 'Regulatory and co-annotation analysis.' },
    { name: 'Reactome', url: 'https://reactome.org/', logo: '/logos/reactome-icon.png', text: 'Pathway knowledgebase and Analysis Service.' },
    { name: 'clusterProfiler', url: 'https://bioconductor.org/packages/clusterProfiler/', logo: '/logos/clusterprofiler.png', text: 'ORA and GSEA on MSigDB, run locally.' },
  ] },
  { title: 'Pathways and ontologies', items: [
    { name: 'Gene Ontology', url: 'https://geneontology.org/', logo: R('geneontology'), text: 'Biological processes, functions and components.' },
    { name: 'KEGG', url: 'https://www.kegg.jp/', logo: R('kegg'), text: 'Pathway maps, with your genes coloured.' },
    { name: 'WikiPathways', url: 'https://www.wikipathways.org/', logo: R('wikipathways'), text: 'Community-curated pathways.' },
    { name: 'MSigDB', url: 'https://www.gsea-msigdb.org/gsea/msigdb/', text: 'Hallmark, curated, regulatory and cell-type gene sets.' },
  ] },
  { title: 'Regulators, drugs and cell types', items: [
    { name: 'ChEA3 and KEA3', url: 'https://maayanlab.cloud/chea3/', logo: R('maayanlab'), text: 'Transcription factor and kinase enrichment.' },
    { name: 'LINCS L1000', url: 'https://lincsproject.org/', logo: R('maayanlab'), text: 'Drug and CRISPR perturbation signatures.' },
    { name: 'PanglaoDB', url: 'https://panglaodb.se/', logo: R('panglaodb'), text: 'Single-cell marker genes.' },
    { name: 'CellMarker', url: 'http://bio-bigdata.hrbmu.edu.cn/CellMarker/', logo: R('cellmarker'), text: 'Curated cell-type markers.' },
    { name: 'PROGENy', url: 'https://bioconductor.org/packages/progeny/', logo: R('bioconductor'), text: 'Signalling pathway activity from expression.' },
    { name: 'miRDB', url: 'https://mirdb.org/', text: 'microRNA target predictions.' },
  ] },
  { title: 'Diseases and variants', items: [
    { name: 'Open Targets', url: 'https://platform.opentargets.org/', logo: R('opentargets'), text: 'Scored disease–gene associations.' },
    { name: 'DisGeNET', url: 'https://www.disgenet.com/', logo: R('disgenet'), text: 'Gene–disease associations.' },
    { name: 'OMIM', url: 'https://www.omim.org/', text: 'Mendelian disorders and their genes.' },
    { name: 'Orphanet', url: 'https://www.orpha.net/', text: 'Rare diseases.' },
    { name: 'GWAS Catalog', url: 'https://www.ebi.ac.uk/gwas/', logo: R('gwascatalog'), text: 'Published trait associations.' },
    { name: 'ClinVar and dbSNP', url: 'https://www.ncbi.nlm.nih.gov/snp/', logo: R('ncbi'), text: 'Variants and their clinical significance.' },
    { name: 'Ensembl VEP', url: 'https://rest.ensembl.org/', text: 'Variant consequences.' },
    { name: 'gnomAD', url: 'https://gnomad.broadinstitute.org/', logo: R('gnomad'), text: 'Population allele frequencies.' },
  ] },
  { title: 'Literature and expression data', items: [
    { name: 'PubMed and NCBI Gene', url: 'https://pubmed.ncbi.nlm.nih.gov/', logo: R('ncbi'), text: 'Articles and the genes linked to them.' },
    { name: 'GEO', url: 'https://www.ncbi.nlm.nih.gov/geo/', logo: R('ncbi'), text: 'Gene expression datasets.' },
    { name: 'RummaGEO', url: 'https://rummageo.com/', logo: R('rummageo'), text: 'Signatures computed from GEO series.' },
    { name: 'Rummagene', url: 'https://rummagene.com/', logo: R('rummagene'), text: 'Gene sets from PubMed Central papers.' },
    { name: 'GeneSigDB', url: 'https://maayanlab.cloud/Enrichr/', logo: R('genesigdb'), text: 'Published gene signatures (via Enrichr).' },
    { name: 'EXTRACT', url: 'https://extract.jensenlab.org/', text: 'Finds gene names in text.' },
  ] },
  { title: 'Network viewers from our lab', items: [
    { name: 'Arena3D', url: 'https://arena3d.org/', logo: R('arena3d'), text: 'Multilayer networks in 3D.' },
    { name: 'NORMA', url: 'https://normaviz.org/', logo: R('norma'), text: 'Networks with annotated groups.' },
  ] },
]

function RelatedTools() {
  return (
    <div className="related">
      {RELATED.map((c) => (
        <section key={c.title}>
          <h3>{c.title}</h3>
          <div className="related-grid">
            {c.items.map((t) => (
              <a key={t.name} className="related-card" href={t.url} target="_blank" rel="noopener noreferrer">
                {t.logo ? <img src={t.logo} alt="" width={32} height={32} loading="lazy" />
                  : <span className="related-badge" aria-hidden="true">{t.name.replace(/[^A-Za-z0-9]/g, '').slice(0, 2).toUpperCase()}</span>}
                <span><strong>{t.name}</strong><span className="muted small">{t.text}</span></span>
              </a>
            ))}
          </div>
        </section>
      ))}
    </div>
  )
}

function About() {
  return (
    <article className="doc">
      <h1>About Flame</h1>
      <p>Flame integrates functional and literature enrichment from several established services, lets you build and combine gene lists from many kinds of input, and helps you interpret and compare the results with interactive visualizations.</p>
      <h2>Team</h2>
      <p>Developed by the Bioinformatics and Integrative Biology Lab (Pavlopoulos Lab), Biomedical Sciences Research Center "Alexander Fleming", Greece.</p>
      <h2>Licence</h2>
      <p>The web service is free for everyone. The source code is available under the <a href="https://www.gnu.org/licenses/gpl-3.0.html" target="_blank" rel="noopener noreferrer">GNU General Public License v3.0</a> at <a href="https://github.com/PavlopoulosLab/Flame" target="_blank" rel="noopener noreferrer">github.com/PavlopoulosLab/Flame</a>.</p>
      <h2>Related tools and resources</h2>
      <p className="muted small">The services Flame queries, the databases behind them, and our network viewers. Please cite the ones whose results you report.</p>
      <RelatedTools />
      <p><Link to="/cite">How to cite</Link> · <Link to="/privacy">Privacy</Link></p>
    </article>
  )
}

function Privacy() {
  const { data: config } = useConfig()
  const days = config?.limits.retentionDays ?? 30
  return (
    <article className="doc">
      <h1>Privacy</h1>
      <h2>Personal details</h2>
      <p>Flame does not ask for personal details. An email address is optional and used only, if you give one, to tell you that your runs have finished.</p>
      <h2>What is stored</h2>
      <p>The lists you add, your custom gene sets and the results of your runs are stored on the server in your workspace, so that you can come back to them. Uploaded tables and text are processed to extract IDs and are not kept beyond what you save as a list.</p>
      <h2>Who can see it</h2>
      <p>A workspace is identified by a long random link. Only people who have the link can open it; workspaces cannot be listed or searched. Share the link only with people you want to see your data.</p>
      <h2>How long</h2>
      <p>Workspaces are deleted automatically after {days} days without use. You can clear or permanently delete your workspace at any time from <em>Save, load or clear session</em> in the workspace.</p>
      <h2>Third-party services</h2>
      <p>To run analyses, Flame sends the IDs (and, for text mining, the text) you submit to the corresponding services: g:Profiler, STRING, Enrichr, WebGestalt, PANTHER, GeneCodis, Reactome, ChEA3 and KEA3, KEGG, Ensembl VEP, the EXTRACT tagger; searches typed in the disease, PubMed, GEO and dbSNP inputs go to Open Targets, NCBI E-utilities, RummaGEO and the GWAS Catalog; and — only if you press <em>3D view</em> — Arena3Dweb. Nothing else is shared.</p>
      <h2>Cookies and tracking</h2>
      <p>Flame sets no cookies, uses no analytics or tracking, and loads no third-party scripts, fonts or images. Your colour theme and open views are remembered only for the current browser session.</p>
    </article>
  )
}

const PY_SNIPPET = `# pip install requests
import time, requests

BASE = "https://YOUR-FLAME-SERVER/api/v1"
ws = requests.post(f"{BASE}/workspace").json()["id"]
H = {"X-Flame-Workspace": ws}

lst = requests.post(f"{BASE}/lists", headers=H, json={
    "name": "My genes", "ids": ["TP53", "BRCA1", "EGFR", "KRAS", "MYC"], "organismTaxId": 9606}).json()
requests.post(f"{BASE}/runs", headers=H, json={
    "listId": lst["id"], "tools": ["gprofiler", "string"], "datasources": ["GO:BP", "KEGG"]})

while any(r["status"] in ("queued", "running") for r in requests.get(f"{BASE}/runs", headers=H).json()):
    time.sleep(3)

for run in requests.get(f"{BASE}/runs", headers=H).json():
    terms = requests.post(f"{BASE}/runs/{run['id']}/view", headers=H,
                          json={"view": "table", "top": "all"}).json()["terms"]
    print(run["title"], len(terms), "terms; top:", terms[0]["name"] if terms else "-")
print("Open in the browser:", BASE.replace("/api", f"/w/{ws}"))`

const ENDPOINT_GROUPS: { title: string; rows: [string, string, string][] }[] = [
  { title: 'Workspace and sessions', rows: [
    ['POST', '/api/v1/workspace', 'Create a workspace; returns its id (send it as the X-Flame-Workspace header).'],
    ['GET', '/api/v1/workspace', 'Lists, runs, custom sources and session status of the workspace.'],
    ['GET', '/api/v1/workspace/status', 'Session status: saved / changed / unsaved, counts, running jobs, expiry, stored data.'],
    ['GET', '/api/v1/workspace/export?parts=&runIds=', 'Session file (JSON). parts = lists,sources,runs,variants (default all); runIds limits the runs.'],
    ['POST', '/api/v1/workspace/import?mode=add|replace&parts=', 'Load a session file (body = the JSON); up to 25 MB.'],
    ['PUT', '/api/v1/workspace/email', 'Optional address for a "runs finished" email (if the server sends email).'],
    ['DELETE', '/api/v1/workspace · /api/v1/workspace/data', 'Delete the workspace, or only its data.'],
  ] },
  { title: 'Reference data', rows: [
    ['GET', '/api/v1/config', 'Tools, paradigms (ORA/GSEA), metrics, annotation sources, ID formats, limits.'],
    ['GET', '/api/v1/organisms?q=', 'Search organisms; /api/organisms/{taxid}/tools shows tool support.'],
  ] },
  { title: 'Lists and inputs', rows: [
    ['GET / POST', '/api/v1/lists', 'List or create lists from ids or text; ranked lists: scores[] or ranked=true with "ID<TAB>score" text.'],
    ['GET / PATCH / DELETE', '/api/v1/lists/{id}', 'Read, rename or delete a list; /csv downloads it (with scores).'],
    ['POST', '/api/v1/inputs/upload/parse', 'Parse pasted IDs, or IDs with scores (ranked=true).'],
    ['POST', '/api/v1/inputs/snps/map', 'Map rsIDs to genes (g:SNPense).'],
    ['POST', '/api/v1/inputs/textmining/extract', 'Find gene names in text (EXTRACT).'],
    ['POST', '/api/v1/inputs/table/prepare · volcano/points · reduction/points', 'Volcano (differential expression) and 2D (UMAP / t-SNE / PCA) inputs.'],
    ['POST', '/api/v1/inputs/expression/overview', 'Expression matrix: co-expression clusters (heatmap) and gene map (PCA).'],
    ['POST', '/api/v1/inputs/expression/de', 'Expression matrix: two-group Welch t-test; returns a table for the volcano input.'],
    ['POST', '/api/v1/inputs/expression/activity', 'Expression matrix: pathway activity per sample. method = ssgsea | zscore | progeny; sources = HALLMARK, KEGG, REAC, WP, GO:BP.'],
    ['POST', '/api/v1/inputs/mirna/targets', 'microRNAs → predicted target genes (miRDB via MSigDB); minMirnas keeps genes hit by several.'],
    ['POST', '/api/v1/inputs/disease/search', 'Search diseases (Open Targets: EFO / MONDO ids) and disease gene sets (DisGeNET, OMIM, Orphanet, ClinVar, Jensen DISEASES, GWAS Catalog). Body: query.'],
    ['POST', '/api/v1/inputs/disease/genes', 'Genes of a disease. source = opentargets (id; scored, ranked) · gwas (id; GWAS Catalog variants, p ≤ 5e-8, and mapped genes) · DisGeNET | OMIM | Orphanet | ClinVar | Jensen | GWAS (term).'],
    ['POST', '/api/v1/inputs/pubmed/genes', 'Genes linked to PubMed articles (NCBI gene2pubmed). Body: query = PMIDs or a PubMed search, organismTaxId, max (articles, ≤ 500).'],
    ['POST', '/api/v1/inputs/geo/search · geo/genes', 'GEO differential-expression signatures (RummaGEO): search by GSE accession or keywords; genes of one signature by id.'],
    ['POST', '/api/v1/inputs/dbsnp/lookup', 'dbSNP: rsIDs or a dbSNP search (e.g. APOE[gene] AND pathogenic[clinical_significance]) → variants, genes, consequences, ClinVar, frequencies.'],
    ['POST', '/api/v1/inputs/upset', 'Set combinations of lists (intersection, distinct, union).'],
    ['GET', '/api/v1/inputs/examples/{kind}[/file]', 'Example inputs: upload, ranked, de_lists, volcano, reduction, expression, mirna, metabolites, snps, textmining, gmt.'],
    ['GET / POST / DELETE', '/api/v1/sources', 'Custom gene sets (GMT).'],
  ] },
  { title: 'Enrichment runs', rows: [
    ['GET / POST', '/api/v1/runs', 'List runs (with status) or start runs. Body: listId, tools[], datasources[], sourceIds[], threshold, paradigm = ORA | GSEA, perTool{}.'],
    ['GET / DELETE', '/api/v1/runs/{id}', 'One run; /details gives parameters, ID mapping and unmapped IDs.'],
    ['POST', '/api/v1/runs/{id}/view', 'Result data for any view: table, dotplot, barchart, scatter, heatmap-*, network-* (filters, top, sortBy, settings).'],
    ['POST', '/api/v1/runs/{id}/pathway-links', 'For one term: KEGG map with the genes coloured, Reactome with genes flagged, ontology pages, searches.'],
    ['POST', '/api/v1/runs/{id}/themes', 'Functional themes: terms grouped by shared genes (Jaccard ≥ cutoff, Louvain) with keywords, plus word-cloud weights.'],
    ['POST', '/api/v1/runs/{id}/arena3d', 'Open a network view in Arena3D (3D layers); returns a link valid for one hour.'],
    ['POST', '/api/v1/runs/{id}/norma', 'Open a network view in NORMA (terms as annotated groups).'],
  ] },
  { title: 'Comparing runs', rows: [
    ['POST', '/api/v1/comparisons', 'Compare 2–10 runs (overview, bubble, heatmap) with Fisher\'s combined p-value.'],
    ['POST', '/api/v1/comparisons/statistics', 'Overlap (Jaccard, overlap coefficient), rank agreement (Spearman, Kendall), paired Wilcoxon, McNemar / Cochran\'s Q, Kruskal–Wallis, concordance at the top, MDS.'],
    ['POST', '/api/v1/comparisons/themes', 'Themes across the compared runs, with the runs that support each theme.'],
    ['POST', '/api/v1/comparisons/overview-terms', 'Term ids behind an overview bar or card.'],
    ['POST', '/api/v1/comparisons/arena3d', 'Open the comparison in Arena3D (one layer per run, a genes layer).'],
  ] },
  { title: 'Regulators, drugs and other analyses', rows: [
    ['POST', '/api/v1/analyses', 'Start a background analysis; returns its id. type = tf | kinase (listId; ChEA3 / KEA3, human) · drugs (upListId, downListId, kind = chemical | crispr; LINCS L1000) · modules (listId, score; STRING + Louvain + g:Profiler) · robustness (listId, sources[]) · metabolites (text, name, organismTaxId; KEGG).'],
    ['GET', '/api/v1/analyses', 'Analyses of the workspace with status and progress (stored with the workspace and its session files).'],
    ['DELETE', '/api/v1/analyses/{id}', 'Remove an analysis (stops it if still running).'],
    ['GET', '/api/v1/analyses/{id}', 'Status, progress and result. A finished metabolite analysis becomes an ordinary run (runId).'],
  ] },
  { title: 'Variants and GWAS', rows: [
    ['GET', '/api/v1/variants/species', 'Organisms with variant annotation.'],
    ['GET / POST', '/api/v1/variants', 'List or start analyses (rsIDs, VCF lines, chr:pos:ref:alt, HGVS or GWAS summary statistics).'],
    ['GET / DELETE', '/api/v1/variants/{id}', 'Status and report: consequences, impact, gnomAD, ClinVar, genes, traits.'],
    ['POST', '/api/v1/variants/{id}/arena3d', 'Variants → genes → traits in Arena3D.'],
    ['GET', '/api/v1/variants/examples/{variants|gwas}[/file]', 'Example inputs.'],
  ] },
  { title: 'Utilities and legacy', rows: [
    ['POST', '/api/v1/utilities/convert · orthologs · ppi', 'Convert IDs (g:Convert), find orthologs (g:Orth), STRING network.'],
    ['POST', '/api/v1/utilities/ppi/arena3d', 'Open a STRING network in Arena3D, evidence types as edge channels.'],
    ['GET / POST', '/api/v1/legacy', 'Flame v2-style import: ?url_genes=A,B;C,D or JSON gene lists.'],
    ['GET', '/api/v1/health', 'Server status.'],
  ] },
]

const CURL_SNIPPET = `BASE=${typeof window === 'undefined' ? 'https://your-flame-server' : window.location.origin}/api/v1
# 1. A workspace (keep the id)
WS=$(curl -s -X POST $BASE/workspace | python3 -c "import json,sys; print(json.load(sys.stdin)['id'])")
# 2. A gene list
LIST=$(curl -s -X POST $BASE/lists -H "X-Flame-Workspace: $WS" -H "Content-Type: application/json" \\
  -d '{"name":"p53 targets","ids":["TP53","MDM2","CDKN1A","BAX","GADD45A","FAS","BBC3","SESN1"],"organismTaxId":9606}' \\
  | python3 -c "import json,sys; print(json.load(sys.stdin)['id'])")
# 3. Runs with two tools (HTTP 202 + run ids)
curl -s -X POST $BASE/runs -H "X-Flame-Workspace: $WS" -H "Content-Type: application/json" \\
  -d "{\\"listId\\":\\"$LIST\\",\\"tools\\":[\\"gprofiler\\",\\"reactome\\"],\\"datasources\\":[\\"GO:BP\\",\\"REAC\\"]}"
# 4. Poll until "status": "complete", then read the results
curl -s $BASE/runs/reactome_2 -H "X-Flame-Workspace: $WS"
curl -s -X POST $BASE/runs/reactome_2/view -H "X-Flame-Workspace: $WS" -H "Content-Type: application/json" -d '{"view":"table","top":"all"}'`

const CLI_EXAMPLES = `# Enrichment of one or more gene lists with several tools, CSV per run, comparison statistics
python flame_client.py enrich genes_a.txt genes_b.txt --tools gprofiler,string,enrichr --stats

# Add KEGG maps with your genes highlighted for the top pathway terms
python flame_client.py enrich genes.txt --pathways 5

# GSEA on a ranked list (two columns: gene<TAB>score)
python flame_client.py gsea ranked.tsv --sources HALLMARK,KEGG,REAC

# Variants or GWAS summary statistics: genes, traits, and enrichment of the affected genes
python flame_client.py variants gwas.tsv --enrich

# Expression matrix: two-group comparison, then enrichment of the significant genes
python flame_client.py expression matrix.tsv --group-a treated_1,treated_2,treated_3 --group-b control_1,control_2,control_3

# microRNAs: targets hit by at least 2 of your miRNAs, then enrichment
python flame_client.py mirna mirnas.txt --min-mirnas 2

# Save the whole workspace as a session file (load it later in the web app)
python flame_client.py enrich genes.txt --save-session my_session.json`

function ApiDocs() {
  return (
    <article className="doc">
      <h1>API</h1>
      <p>Everything the web app does is available as a versioned REST API returning JSON, at <code>{window.location.origin}/api/v1</code>. Create a workspace and send its id in the <code>X-Flame-Workspace</code> header on every other call.</p>
      <p>
        <a className="btn btn-primary" href="/flame_client.py" download>Download the Python example script</a>{' '}
        <a className="btn btn-secondary" href="/__docs__/" target="_blank" rel="noopener noreferrer">Interactive API reference (Swagger)</a>{' '}
        <a className="btn btn-ghost" href="/openapi.json" target="_blank" rel="noopener noreferrer">OpenAPI 3 specification (JSON)</a>
      </p>
      <ul className="small">
        <li><strong>Try it in the browser:</strong> in the interactive reference, run <code>POST /api/v1/workspace</code>, press <em>Authorize</em> and paste the id; every endpoint then has <em>Try it out</em> with a ready-made example body.</li>
        <li><strong>Generate a client:</strong> the OpenAPI file works with generators such as openapi-generator (Python, R, JavaScript, Java…).</li>
        <li><strong>Responses:</strong> JSON; <code>202</code> when a job is queued (poll its id), <code>400</code> invalid input, <code>401</code> missing workspace, <code>404</code> not found, <code>413</code> too large, <code>429</code> too many requests or jobs. Errors carry <code>{'{ "error": "…" }'}</code>.</li>
        <li><strong>Stability:</strong> <code>/api/v1</code> keeps its request and response formats; new fields may be added. The unversioned <code>/api/…</code> paths serve the web app and may change.</li>
      </ul>
      <h2>Quick start (curl)</h2>
      <pre className="code">{CURL_SNIPPET}</pre>
      <CopyButton text={CURL_SNIPPET} label="Copy" />
      <h2>Quick start (Python)</h2>
      <pre className="code">{PY_SNIPPET}</pre>
      <CopyButton text={PY_SNIPPET} label="Copy" />
      <h2>Open lists directly in the browser</h2>
      <p>Link to Flame with gene lists in the URL (lists separated by <code>;</code>, genes by <code>,</code>):</p>
      <pre className="code">{`${window.location.origin}/?url_genes=TP53,BRCA1,EGFR;KRAS,MYC&organism=9606`}</pre>
      <h2>Command-line client</h2>
      <p>The Python script covers the common tasks; run <code>python flame_client.py -h</code> for all options.</p>
      <pre className="code">{CLI_EXAMPLES}</pre>
      <h2>Endpoints</h2>
      {ENDPOINT_GROUPS.map((g) => (
        <section key={g.title}>
          <h3>{g.title}</h3>
          <table className="table">
            <thead><tr><th>Method</th><th>Path</th><th>Purpose</th></tr></thead>
            <tbody>{g.rows.map(([m, p, d]) => <tr key={p}><td><code>{m}</code></td><td><code>{p}</code></td><td>{d}</td></tr>)}</tbody>
          </table>
        </section>
      ))}
      <p className="small muted">Limits: requests up to 2 MB (session files up to 25 MB); 600 requests per minute per address; up to 6 jobs in progress per workspace; Arena3D links last one hour.</p>
    </article>
  )
}

export default function InfoPages({ page }: { page: 'cite' | 'about' | 'privacy' | 'api' }) {
  return <div className="doc-single">{page === 'cite' ? <Cite /> : page === 'about' ? <About /> : page === 'privacy' ? <Privacy /> : <ApiDocs />}</div>
}
