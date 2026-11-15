import { Link } from 'react-router-dom'
import { useConfig } from '../api/hooks'
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
      <p className="small">Results come from g:Profiler, STRING, Enrichr, WebGestalt, PANTHER and GeneCodis, with g:SNPense, g:Convert, g:Orth, the EXTRACT tagger and Arena3Dweb. Please also cite the tools whose results you report.</p>
    </article>
  )
}

function About() {
  return (
    <article className="doc">
      <h1>About Flame</h1>
      <p>Flame integrates functional and literature enrichment from several established services, lets you build and combine gene lists from many kinds of input, and helps you interpret and compare the results with interactive visualizations.</p>
      <h2>Team</h2>
      <p>Developed by the Bioinformatics and Integrative Biology Lab (Pavlopoulos Lab), Biomedical Sciences Research Center "Alexander Fleming", Greece: Evangelos Karatzas, Fotis A. Baltoumas, Georgios A. Pavlopoulos and colleagues.</p>
      <h2>Licence</h2>
      <p>The web service is free for everyone, with no login. The source code is available under the <a href="https://www.gnu.org/licenses/gpl-3.0.html" target="_blank" rel="noopener noreferrer">GNU General Public License v3.0</a> at <a href="https://github.com/PavlopoulosLab/Flame" target="_blank" rel="noopener noreferrer">github.com/PavlopoulosLab/Flame</a>.</p>
      <h2>Related tools</h2>
      <ul>
        <li><a href="https://biit.cs.ut.ee/gprofiler/" target="_blank" rel="noopener noreferrer">g:Profiler</a></li>
        <li><a href="https://string-db.org/" target="_blank" rel="noopener noreferrer">STRING</a></li>
        <li><a href="https://maayanlab.cloud/Enrichr/" target="_blank" rel="noopener noreferrer">Enrichr</a></li>
        <li><a href="https://www.webgestalt.org/" target="_blank" rel="noopener noreferrer">WebGestalt</a></li>
        <li><a href="https://pantherdb.org/" target="_blank" rel="noopener noreferrer">PANTHER</a></li>
        <li><a href="https://genecodis.genyo.es/" target="_blank" rel="noopener noreferrer">GeneCodis</a></li>
        <li><a href="https://www.arena3d.org/" target="_blank" rel="noopener noreferrer">Arena3D</a> and <a href="https://normaviz.org/" target="_blank" rel="noopener noreferrer">NORMA</a> (network viewers)</li>
        <li><a href="https://rest.ensembl.org/" target="_blank" rel="noopener noreferrer">Ensembl VEP</a> (variant annotation)</li>
      </ul>
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
      <h2>No accounts</h2>
      <p>Flame does not ask for a login, registration or personal details. An email address is optional and used only, if you give one, to tell you that your runs have finished.</p>
      <h2>What is stored</h2>
      <p>The lists you add, your custom gene sets and the results of your runs are stored on the server in your workspace, so that you can come back to them. Uploaded tables and text are processed to extract IDs and are not kept beyond what you save as a list.</p>
      <h2>Who can see it</h2>
      <p>A workspace is identified by a long random link. Only people who have the link can open it; workspaces cannot be listed or searched. Share the link only with people you want to see your data.</p>
      <h2>How long</h2>
      <p>Workspaces are deleted automatically after {days} days without use. You can clear or permanently delete your workspace at any time from <em>Save, load or clear session</em> in the workspace.</p>
      <h2>Third-party services</h2>
      <p>To run analyses, Flame sends the IDs (and, for text mining, the text) you submit to the corresponding services: g:Profiler, STRING, Enrichr, WebGestalt, PANTHER, GeneCodis, the EXTRACT tagger, and — only if you press <em>3D view</em> — Arena3Dweb. Nothing else is shared.</p>
      <h2>Cookies and tracking</h2>
      <p>Flame sets no cookies, uses no analytics or tracking, and loads no third-party scripts, fonts or images. Your colour theme and open views are remembered only for the current browser session.</p>
    </article>
  )
}

const PY_SNIPPET = `# pip install requests
import time, requests

BASE = "https://YOUR-FLAME-SERVER/api"
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
    ['POST', '/api/workspace', 'Create a workspace; returns its id (send it as the X-Flame-Workspace header).'],
    ['GET', '/api/workspace', 'Lists, runs, custom sources and session status of the workspace.'],
    ['GET', '/api/workspace/status', 'Session status: saved / changed / unsaved, counts, running jobs, expiry, stored data.'],
    ['GET', '/api/workspace/export?parts=&runIds=', 'Session file (JSON). parts = lists,sources,runs,variants (default all); runIds limits the runs.'],
    ['POST', '/api/workspace/import?mode=add|replace&parts=', 'Load a session file (body = the JSON); up to 25 MB.'],
    ['PUT', '/api/workspace/email', 'Optional address for a "runs finished" email (if the server sends email).'],
    ['DELETE', '/api/workspace · /api/workspace/data', 'Delete the workspace, or only its data.'],
  ] },
  { title: 'Reference data', rows: [
    ['GET', '/api/config', 'Tools, paradigms (ORA/GSEA), metrics, annotation sources, ID formats, limits.'],
    ['GET', '/api/organisms?q=', 'Search organisms; /api/organisms/{taxid}/tools shows tool support.'],
  ] },
  { title: 'Lists and inputs', rows: [
    ['GET / POST', '/api/lists', 'List or create lists from ids or text; ranked lists: scores[] or ranked=true with "ID<TAB>score" text.'],
    ['GET / PATCH / DELETE', '/api/lists/{id}', 'Read, rename or delete a list; /csv downloads it (with scores).'],
    ['POST', '/api/inputs/upload/parse', 'Parse pasted IDs, or IDs with scores (ranked=true).'],
    ['POST', '/api/inputs/snps/map', 'Map rsIDs to genes (g:SNPense).'],
    ['POST', '/api/inputs/textmining/extract', 'Find gene names in text (EXTRACT).'],
    ['POST', '/api/inputs/table/prepare · volcano/points · reduction/points', 'Volcano (differential expression) and 2D (UMAP / t-SNE / PCA) inputs.'],
    ['POST', '/api/inputs/expression/overview', 'Expression matrix: co-expression clusters (heatmap) and gene map (PCA).'],
    ['POST', '/api/inputs/expression/de', 'Expression matrix: two-group Welch t-test; returns a table for the volcano input.'],
    ['POST', '/api/inputs/mirna/targets', 'microRNAs → predicted target genes (miRDB via MSigDB); minMirnas keeps genes hit by several.'],
    ['POST', '/api/inputs/upset', 'Set combinations of lists (intersection, distinct, union).'],
    ['GET', '/api/inputs/examples/{kind}[/file]', 'Example inputs: upload, ranked, de_lists, volcano, reduction, expression, mirna, snps, textmining, gmt.'],
    ['GET / POST / DELETE', '/api/sources', 'Custom gene sets (GMT).'],
  ] },
  { title: 'Enrichment runs', rows: [
    ['GET / POST', '/api/runs', 'List runs (with status) or start runs. Body: listId, tools[], datasources[], sourceIds[], threshold, paradigm = ORA | GSEA, perTool{}.'],
    ['GET / DELETE', '/api/runs/{id}', 'One run; /details gives parameters, ID mapping and unmapped IDs.'],
    ['POST', '/api/runs/{id}/view', 'Result data for any view: table, dotplot, barchart, scatter, heatmap-*, network-* (filters, top, sortBy, settings).'],
    ['POST', '/api/runs/{id}/pathway-links', 'For one term: KEGG map with the genes coloured, Reactome with genes flagged, ontology pages, searches.'],
    ['POST', '/api/runs/{id}/arena3d', 'Open a network view in Arena3D (3D layers); returns a link valid for one hour.'],
    ['POST', '/api/runs/{id}/norma', 'Open a network view in NORMA (terms as annotated groups).'],
  ] },
  { title: 'Comparing runs', rows: [
    ['POST', '/api/comparisons', 'Compare 2–10 runs (overview, bubble, heatmap) with Fisher\'s combined p-value.'],
    ['POST', '/api/comparisons/statistics', 'Overlap (Jaccard, overlap coefficient), rank agreement (Spearman, Kendall), paired Wilcoxon, McNemar / Cochran\'s Q, Kruskal–Wallis, concordance at the top, MDS.'],
    ['POST', '/api/comparisons/overview-terms', 'Term ids behind an overview bar or card.'],
    ['POST', '/api/comparisons/arena3d', 'Open the comparison in Arena3D (one layer per run, a genes layer).'],
  ] },
  { title: 'Variants and GWAS', rows: [
    ['GET', '/api/variants/species', 'Organisms with variant annotation.'],
    ['GET / POST', '/api/variants', 'List or start analyses (rsIDs, VCF lines, chr:pos:ref:alt, HGVS or GWAS summary statistics).'],
    ['GET / DELETE', '/api/variants/{id}', 'Status and report: consequences, impact, gnomAD, ClinVar, genes, traits.'],
    ['POST', '/api/variants/{id}/arena3d', 'Variants → genes → traits in Arena3D.'],
    ['GET', '/api/variants/examples/{variants|gwas}[/file]', 'Example inputs.'],
  ] },
  { title: 'Utilities and legacy', rows: [
    ['POST', '/api/utilities/convert · orthologs · ppi', 'Convert IDs (g:Convert), find orthologs (g:Orth), STRING network.'],
    ['POST', '/api/utilities/ppi/arena3d', 'Open a STRING network in Arena3D, evidence types as edge channels.'],
    ['GET / POST', '/api/legacy', 'Flame v2-style import: ?url_genes=A,B;C,D or JSON gene lists.'],
    ['GET', '/api/health', 'Server status.'],
  ] },
]

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
      <p>Everything the web app does is available as a REST API returning JSON. No key is needed: create a workspace and send its id in the <code>X-Flame-Workspace</code> header.</p>
      <p>
        <a className="btn btn-primary" href="/flame_client.py" download>Download the Python example script</a>{' '}
        <a className="btn btn-secondary" href="/__docs__/" target="_blank" rel="noopener noreferrer">Interactive API reference (OpenAPI)</a>
      </p>
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
