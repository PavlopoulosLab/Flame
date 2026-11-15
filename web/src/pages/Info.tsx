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
        <li><a href="https://arena3d.pavlopouloslab.info/" target="_blank" rel="noopener noreferrer">Arena3Dweb</a></li>
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

const ENDPOINTS: [string, string, string][] = [
  ['POST', '/api/workspace', 'Create a workspace; returns its id (send it as the X-Flame-Workspace header).'],
  ['GET', '/api/workspace', 'Lists, runs and custom sources of the workspace.'],
  ['GET / POST', '/api/workspace/export · /import', 'Save or load a whole session (JSON).'],
  ['DELETE', '/api/workspace · /api/workspace/data', 'Delete the workspace, or only its data.'],
  ['GET', '/api/config', 'Tools, metrics, annotation sources, ID formats, limits.'],
  ['GET', '/api/organisms?q=', 'Search organisms; /api/organisms/{taxid}/tools shows tool support.'],
  ['GET / POST', '/api/lists', 'List or create lists (from ids or pasted text).'],
  ['GET / PATCH / DELETE', '/api/lists/{id}', 'Read, rename or delete a list; /csv downloads it.'],
  ['POST', '/api/inputs/upload/parse', 'Parse pasted IDs (counts duplicates and invalid entries).'],
  ['POST', '/api/inputs/snps/map', 'Map rsIDs to genes (g:SNPense).'],
  ['POST', '/api/inputs/textmining/extract', 'Find gene names in text (EXTRACT).'],
  ['POST', '/api/inputs/table/prepare · volcano/points · reduction/points', 'Volcano and 2D-reduction inputs.'],
  ['POST', '/api/inputs/upset', 'Set combinations of lists.'],
  ['GET', '/api/inputs/examples/{kind}[/file]', 'Example inputs (upload, snps, textmining, volcano, reduction).'],
  ['POST', '/api/utilities/convert · orthologs · ppi', 'Convert IDs, find orthologs, STRING network.'],
  ['GET / POST', '/api/sources', 'Custom GMT gene sets.'],
  ['GET / POST', '/api/runs', 'List runs (with status) or start runs.'],
  ['GET / DELETE', '/api/runs/{id}', 'One run; /details gives parameters and ID mapping.'],
  ['POST', '/api/runs/{id}/view', 'Result data for any view (filters, top, sortBy, settings).'],
  ['POST', '/api/runs/{id}/arena3d', 'Send a network view to Arena3Dweb.'],
  ['POST', '/api/comparisons', 'Compare runs (overview, bubble, heatmap); /overview-terms resolves clicks.'],
  ['GET / POST', '/api/legacy', 'Flame v2-style import: ?url_genes=A,B;C,D or JSON gene lists.'],
]

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
      <h2>Endpoints</h2>
      <table className="table">
        <thead><tr><th>Method</th><th>Path</th><th>Purpose</th></tr></thead>
        <tbody>{ENDPOINTS.map(([m, p, d]) => <tr key={p}><td><code>{m}</code></td><td><code>{p}</code></td><td>{d}</td></tr>)}</tbody>
      </table>
      <p className="small muted">Limits: requests up to 2 MB; 600 requests per minute per address; up to 6 runs in progress per workspace.</p>
    </article>
  )
}

export default function InfoPages({ page }: { page: 'cite' | 'about' | 'privacy' | 'api' }) {
  return <div className="doc-single">{page === 'cite' ? <Cite /> : page === 'about' ? <About /> : page === 'privacy' ? <Privacy /> : <ApiDocs />}</div>
}
